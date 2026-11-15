# ---------------------------------------------------------------------------
# QualiR - importação, propriedades, exclusão, equipe e exportação
# ---------------------------------------------------------------------------

server_acoes <- function(input, output, session, S) {
  con <- S$con

  opcoes_transcricao <- function(prefixo) tagList(
    checkboxInput(paste0(prefixo, "_detect"), "Detectar transcrições (linhas “Falante: fala”)", TRUE),
    checkboxInput(paste0(prefixo, "_auto"), "Criar um caso para cada falante e codificar suas falas automaticamente", TRUE),
    textInput(paste0(prefixo, "_ign"), "Não criar casos para estes falantes (separados por vírgula)", "",
              placeholder = "ex.: Entrevistador, E, Moderadora", width = "100%"))

  # ---------------- importar arquivos -------------------------------------
  observeEvent(input$rb_importar_l, shinyjs::click("rb_importar"))
  observeEvent(input$rb_importar, {
    if (!S$pode_editar()) return()
    showModal(modalDialog(title = tagList(icon("file-import"), " Importar arquivos"), size = "m",
      fileInput("imp_files", "Arquivos (.txt, .docx, .pdf, .vtt, .srt, .md)", multiple = TRUE, width = "100%",
                accept = c(".txt", ".docx", ".pdf", ".vtt", ".srt", ".md"), buttonLabel = "Escolher…",
                placeholder = "nenhum arquivo"),
      opcoes_transcricao("imp"),
      footer = tagList(modalButton("Cancelar"), actionButton("imp_go", "Importar", class = "btn-primary"))))
  })
  observeEvent(input$imp_go, {
    f <- input$imp_files
    if (is.null(f)) { showNotification("Escolha ao menos um arquivo.", type = "warning"); return() }
    ign <- strsplit(input$imp_ign %||% "", ",")[[1]]
    res <- character()
    withProgress(message = "Importando…", value = 0, {
      for (i in seq_len(nrow(f))) {
        incProgress(1 / nrow(f), detail = f$name[i])
        r <- tryCatch({
          x <- ler_arquivo(f$datapath[i], f$name[i])
          if (!nzchar(x$texto)) stop("arquivo sem texto")
          importar_texto(con, tools::file_path_sans_ext(f$name[i]), x$texto, x$tipo, S$me(),
                         isTRUE(input$imp_detect), isTRUE(input$imp_auto), ign)
        }, error = function(e) e)
        if (inherits(r, "error")) res <- c(res, sprintf("❌ %s: %s", f$name[i], conditionMessage(r)))
        else {
          S$log("importou arquivo", r$nome)
          res <- c(res, sprintf("✅ %s%s%s", r$nome, if (r$transcricao) " — transcrição" else "",
                                if (r$novos_casos) sprintf(", %d caso(s) novo(s)", r$novos_casos) else ""))
        }
      }
    })
    removeModal()
    showNotification(HTML(paste(esc(res), collapse = "<br>")), duration = 8)
    S$ir_secao("docs")
  })

  # ---------------- colar transcrição ------------------------------------
  observeEvent(input$rb_colar, {
    if (!S$pode_editar()) return()
    showModal(modalDialog(title = tagList(icon("paste"), " Colar texto ou transcrição"), size = "l",
      textInput("pst_name", "Nome do arquivo", "Entrevista", width = "100%"),
      textAreaInput("pst_text", NULL, "", width = "100%", rows = 14,
                    placeholder = "Entrevistador: Como foi sua experiência?\nMaria: Foi muito boa, porque...\n\n(também aceita [00:01:23] Maria: ...)"),
      opcoes_transcricao("pst"),
      footer = tagList(modalButton("Cancelar"), actionButton("pst_go", "Importar", class = "btn-primary"))))
  })
  observeEvent(input$pst_go, {
    tx <- normalizar_texto(input$pst_text %||% "")
    if (!nzchar(tx)) { showNotification("Cole algum texto.", type = "warning"); return() }
    r <- importar_texto(con, trimws(input$pst_name %||% "Texto"), tx, "txt", S$me(),
                        isTRUE(input$pst_detect), isTRUE(input$pst_auto), strsplit(input$pst_ign %||% "", ",")[[1]])
    S$log("importou arquivo", r$nome); removeModal()
    showNotification(sprintf("“%s” importado%s.", r$nome, if (r$transcricao) " como transcrição" else ""))
    S$ir_secao("docs")
    S$detail(list(type = "doc", id = r$doc_id))
  })

  # ---------------- propriedades / exclusão --------------------------------
  S$modal_doc_prop <- function(id) {
    if (!S$pode_editar()) return()
    d <- isolate(S$docs()); d <- d[d$id == as.integer(id), ]
    S$doc_prop <- d$id
    showModal(modalDialog(title = "Propriedades do arquivo",
      textInput("dp_name", "Nome", d$name, width = "100%"),
      textAreaInput("dp_desc", "Descrição", d$description %||% "", width = "100%", rows = 3),
      tags$small(sprintf("Tipo: %s \u00B7 %s caracteres \u00B7 importado em %s", d$type, d$n_chars, fmt_data(d$created))),
      footer = tagList(modalButton("Cancelar"), actionButton("dp_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$rb_propriedades, {
    it <- S$sel_item()
    if (is.null(it)) { showNotification("Selecione um item na lista.", type = "warning"); return() }
    switch(it$type,
      node = , case = S$modal_node("edit", id = as.integer(it$id)),
      doc = S$modal_doc_prop(it$id),
      user = S$modal_usuario(as.integer(it$id)),
      rel = S$modal_rel(as.integer(it$id)),
      reltype = S$modal_reltipo(as.integer(it$id)),
      set = S$modal_conjunto(as.integer(it$id)),
      annot = S$modal_anotacao_edit(as.integer(it$id)),
      S$abrir(it$type, it$id))
  })
  observeEvent(input$dp_ok, {
    req(S$doc_prop); nm <- trimws(input$dp_name); req(nzchar(nm))
    db_atualizar_doc(con, S$doc_prop, nm, input$dp_desc %||% ""); S$log("editou arquivo", nm); removeModal()
  })

  confirmar <- function(tipo, id, nome, msg) {
    S$excluir_ctx <- list(type = tipo, id = as.integer(id), name = nome)
    showModal(modalDialog(title = "Confirmar exclusão", msg,
      footer = tagList(modalButton("Cancelar"), actionButton("del_ok", "Excluir", class = "btn-danger"))))
  }
  observeEvent(input$rb_excluir, {
    it <- S$sel_item()
    if (is.null(it)) { showNotification("Selecione um item na lista.", type = "warning"); return() }
    if (!S$pode_editar()) return()
    id <- suppressWarnings(as.integer(it$id))
    switch(it$type,
      node = , case = S$modal_node("edit", id = id),
      doc = {
        d <- S$docs(); d <- d[d$id == id, ]
        if (!(S$eh_admin() || identical(d$created_by, S$me()))) {
          showNotification("Somente o administrador ou quem importou pode excluir o arquivo.", type = "error"); return() }
        confirmar("doc", id, d$name, sprintf("Excluir \u201c%s\u201d e suas %d codificações? Esta ação não pode ser desfeita.",
                                             d$name, sum(S$cods()$doc_id == id)))
      },
      squery = { sq <- S$squeries(); confirmar("squery", id, sq$name[sq$id == id], "Excluir este critério de consulta salvo?") },
      memo = confirmar("memo", id, "memorando", "Excluir este memorando?"),
      rel = confirmar("rel", id, "relacionamento", "Excluir este relacionamento?"),
      reltype = confirmar("reltype", id, "tipo de relacionamento", "Excluir este tipo? Os relacionamentos dele ficarão sem tipo."),
      set = confirmar("set", id, "conjunto", "Excluir este conjunto? Os itens em si não são apagados."),
      matrix = confirmar("matrix", id, "matriz", "Excluir esta matriz salva?"),
      annot = confirmar("annot", id, "anotação", "Excluir esta anotação?"),
      showNotification("Este item não pode ser excluído por aqui.", type = "warning"))
  })
  observeEvent(input$del_ok, {
    x <- S$excluir_ctx; req(x)
    switch(x$type, doc = db_excluir_doc(con, x$id), squery = db_excluir_consulta(con, x$id),
           memo = db_excluir_memo(con, x$id), rel = db_excluir_relacionamento(con, x$id),
           reltype = db_excluir_rel_tipo(con, x$id), set = db_excluir_conjunto(con, x$id),
           matrix = db_excluir_matriz(con, x$id), annot = db_excluir_anotacao(con, x$id))
    S$log("excluiu", x$name); removeModal()
    if (identical(S$detail()$id, x$id) && identical(S$detail()$type, x$type)) S$detail(list(type = "none"))
    S$sel_item(NULL)
  })

  observeEvent(input$rb_projeto, {
    if (!S$eh_admin()) { showNotification("Somente o administrador pode renomear o projeto.", type = "error"); return() }
    showModal(modalDialog(title = "Nome do projeto", size = "s",
      textInput("proj_name", NULL, db_meta(con, "project_name"), width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("proj_ok", "Salvar", class = "btn-primary"))))
  })
  observeEvent(input$proj_ok, {
    db_set_meta(con, "project_name", trimws(input$proj_name)); S$projeto_nome <- trimws(input$proj_name); removeModal()
    shinyjs::html("tb-proj", esc(trimws(input$proj_name)))
  })

  # ---------------- notas --------------------------------------------------
  observeEvent(input$rb_nova_nota_l, shinyjs::click("rb_nova_nota"))
  observeEvent(input$rb_nova_nota, {
    if (!S$pode_editar()) return()
    d <- S$detail(); dd <- S$docs(); nd <- S$nodes()
    op <- c("Projeto (geral)" = "projeto")
    if (identical(d$type, "doc")) op <- c(op, setNames("doc", paste("Arquivo aberto:", dd$name[dd$id == d$id])))
    if (!is.null(S$cur_node()) && S$cur_node() %in% nd$id) op <- c(op, setNames("node", paste("Nó selecionado:", nd$name[nd$id == S$cur_node()])))
    showModal(modalDialog(title = "Nova nota", size = "m",
      textInput("nn_title", "Título", "Nota analítica", width = "100%"),
      radioButtons("nn_link", "Vincular a", op),
      footer = tagList(modalButton("Cancelar"), actionButton("nn_ok", "Criar", class = "btn-primary"))))
  })
  observeEvent(input$nn_ok, {
    lk <- input$nn_link %||% "projeto"
    lid <- switch(lk, doc = S$detail()$id, node = S$cur_node(), NULL)
    mid <- db_criar_memo(con, trimws(input$nn_title), "", lk, lid, S$me())
    S$log("criou nota", input$nn_title); removeModal()
    S$detail(list(type = "memo", id = mid))
  })

  # ---------------- equipe ------------------------------------------------
  irpara <- function(sec) S$ir_secao(sec)
  observeEvent(input$rb_atividade, irpara("activity"))
  observeEvent(input$rb_usuarios, irpara("users"))

  perfis <- c("Administrador" = "admin", "Pesquisador(a) — codifica e edita" = "pesquisador", "Leitor(a) — apenas visualiza" = "leitor")
  observeEvent(input$user_new, {
    if (!S$eh_admin()) return()
    showModal(modalDialog(title = "Novo membro da equipe",
      textInput("un_user", "Usuário (login)", width = "100%"),
      textInput("un_name", "Nome", width = "100%"),
      passwordInput("un_pass", "Senha inicial", width = "100%"),
      selectInput("un_role", "Perfil", perfis, selected = "pesquisador", width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("un_ok", "Criar", class = "btn-primary"))))
  })
  observeEvent(input$un_ok, {
    u <- trimws(input$un_user); p <- input$un_pass
    if (!nzchar(u) || nchar(p) < 4) { showNotification("Informe usuário e senha (mínimo 4 caracteres).", type = "error"); return() }
    if (tolower(u) %in% S$users()$username) { showNotification("Usuário já existe.", type = "error"); return() }
    nm <- trimws(input$un_name %||% ""); db_criar_usuario(con, u, if (nzchar(nm)) nm else u, p, input$un_role)
    S$log("adicionou membro", u); removeModal()
  })
  S$modal_usuario <- function(id) {
    if (!S$eh_admin()) return()
    us <- isolate(S$users()); u <- us[us$id == id, ]
    S$edit_user <- id
    showModal(modalDialog(title = paste("Membro:", u$username),
      textInput("ue_name", "Nome", u$name, width = "100%"),
      selectInput("ue_role", "Perfil", perfis, selected = u$role, width = "100%"),
      tags$label("Cor nas faixas de codificação"), div(tags$input(id = "ue_color", type = "color", class = "qr-color", value = u$color %||% "#2B579A")),
      checkboxInput("ue_active", "Ativo (pode entrar)", u$active == 1),
      passwordInput("ue_pass", "Redefinir senha (deixe em branco para manter)", width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("ue_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$ue_ok, {
    id <- S$edit_user; req(id)
    if (id == S$me() && input$ue_role != "admin") { showNotification("Você não pode remover seu próprio perfil de administrador.", type = "error"); return() }
    db_atualizar_usuario(con, id, input$ue_name, input$ue_role, input$ue_color, isTRUE(input$ue_active))
    if (nzchar(input$ue_pass %||% "")) db_trocar_senha(con, id, input$ue_pass)
    S$log("editou membro", input$ue_name); removeModal()
  })
  observeEvent(input$rb_senha, shinyjs::click("btn_senha"))
  observeEvent(input$btn_senha, {
    showModal(modalDialog(title = "Trocar minha senha", size = "s",
      passwordInput("pw_old", "Senha atual", width = "100%"),
      passwordInput("pw_new", "Nova senha", width = "100%"),
      passwordInput("pw_new2", "Repita a nova senha", width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("pw_ok", "Salvar", class = "btn-primary"))))
  })
  observeEvent(input$pw_ok, {
    u <- S$user()
    if (is.null(db_verificar_login(con, u$username, input$pw_old))) { showNotification("Senha atual incorreta.", type = "error"); return() }
    if (nchar(input$pw_new) < 4 || input$pw_new != input$pw_new2) { showNotification("As senhas não conferem (mínimo 4 caracteres).", type = "error"); return() }
    db_trocar_senha(con, u$id, input$pw_new); removeModal(); showNotification("Senha alterada.")
  })

  # ---------------- exportação --------------------------------------------
  output$dl_excel <- downloadHandler(
    filename = function() paste0("codificacao_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      nd <- S$nodes(); cc <- S$cods(); dd <- S$docs(); us <- S$users(); pt <- S$paths()
      refs <- tabela_referencias(con, nd, cc, dd, us)
      cont <- contagens_nodes(nd, cc)
      livro <- data.frame(`Nó` = unname(pt[as.character(nd$id)]), Tipo = ifelse(nd$kind == "case", "Caso", "Nó"),
                          `Descrição` = nd$description %||% "", Cor = nd$color, Arquivos = cont$n_docs,
                          `Referências` = cont$n_refs, check.names = FALSE)
      livro <- livro[order(livro$Tipo != "Nó", livro$`Nó`), ]
      wb <- openxlsx::createWorkbook()
      for (nm in c("Referências", "Livro de nós", "Casos", "Arquivos")) openxlsx::addWorksheet(wb, nm)
      hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F4")
      openxlsx::writeData(wb, "Referências", if (nrow(refs)) refs else data.frame(Aviso = "Sem codificações"), headerStyle = hs)
      openxlsx::writeData(wb, "Livro de nós", livro, headerStyle = hs)
      openxlsx::writeData(wb, "Casos", S$classif_df()$df, headerStyle = hs)
      openxlsx::writeData(wb, "Arquivos", data.frame(Nome = dd$name, Tipo = dd$type, `Transcrição` = dd$is_transcript == 1,
                          Caracteres = dd$n_chars, `Importado em` = dd$created, check.names = FALSE), headerStyle = hs)
      openxlsx::setColWidths(wb, "Referências", cols = 7, widths = 90)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })

  output$dl_codebook <- downloadHandler(
    filename = function() paste0("livro_de_nos_", format(Sys.Date(), "%Y%m%d"), ".docx"),
    content = function(file) {
      nd <- S$nodes(); cc <- S$cods()
      cods <- ordenar_arvore(nd[nd$kind == "code", ])
      cont <- contagens_nodes(nd, cc, agregar = FALSE)
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, paste("Livro de nós —", db_meta(con, "project_name")), style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Gerado em", format(Sys.time(), "%d/%m/%Y %H:%M")), style = "Normal")
      for (i in seq_len(if (is.null(cods)) 0 else nrow(cods))) {
        n <- cods[i, ]; k <- cont[cont$node_id == n$id, ]
        estilo <- if (n$depth == 0) "heading 2" else "heading 3"
        doc <- officer::body_add_par(doc, paste0(strrep(" ", max(0, n$depth - 1)), n$name), style = estilo)
        doc <- officer::body_add_par(doc, if (nzchar(n$description %||% "")) n$description else "(sem definição)", style = "Normal")
        doc <- officer::body_add_par(doc, sprintf("%d referência(s) em %d arquivo(s)", k$n_refs, k$n_docs), style = "Normal")
      }
      print(doc, target = file)
    })

  backup_handler <- function() downloadHandler(
    filename = function() paste0(slug_projeto(db_meta(con, "project_name")), "_backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".sqlite"),
    content = function(file) {
      if (!S$eh_admin()) stop("Somente administradores")
      tmp <- tempfile(fileext = ".sqlite"); db_backup(con, tmp); file.copy(tmp, file, overwrite = TRUE)
      S$log("fez cópia de segurança", "")
    })
  output$dl_backup2 <- backup_handler()

  output$dl_codebook_xlsx <- downloadHandler(
    filename = function() paste0("livro_de_nos_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      nd <- S$nodes(); cods <- ordenar_arvore(nd[nd$kind == "code", ]); pt <- S$paths()
      cont <- contagens_nodes(nd, S$cods())
      df <- if (is.null(cods)) data.frame(`Nó` = character()) else
        data.frame(`Nó` = unname(pt[as.character(cods$id)]), Nome = cods$name, `Nível` = cods$depth + 1,
                   `Descrição` = cods$description %||% "", Cor = cods$color,
                   Arquivos = cont$n_docs[match(cods$id, cont$node_id)], `Referências` = cont$n_refs[match(cods$id, cont$node_id)],
                   check.names = FALSE)
      wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Livro de nós")
      openxlsx::writeData(wb, 1, df, headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#E8F0FA"))
      openxlsx::setColWidths(wb, 1, cols = 1:7, widths = c(40, 25, 7, 60, 10, 10, 12))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })

  output$dl_doc_cod <- downloadHandler(
    filename = function() {
      d <- isolate(S$detail()); dd <- isolate(S$docs())
      nm <- if (identical(d$type, "doc")) dd$name[dd$id == d$id] else "arquivo"
      paste0(gsub("[^[:alnum:]_ -]", "_", nm), "_codificado.docx")
    },
    content = function(file) {
      d <- S$detail()
      doc <- officer::read_docx()
      if (!identical(d$type, "doc")) {
        doc <- officer::body_add_par(doc, "Abra um arquivo antes de exportar.", style = "Normal"); print(doc, target = file); return()
      }
      dd <- S$docs(); nome <- dd$name[dd$id == d$id]; tx <- db_doc_texto(con, d$id)
      cc <- S$cods(); cc <- cc[cc$doc_id == d$id, ]; nd <- S$nodes(); pt <- S$paths(); us <- S$users()
      doc <- officer::body_add_par(doc, nome, style = "heading 1")
      for (par in strsplit(tx, "\n", fixed = TRUE)[[1]]) doc <- officer::body_add_par(doc, par, style = "Normal")
      doc <- officer::body_add_break(doc)
      doc <- officer::body_add_par(doc, "Codificação", style = "heading 1")
      for (nid in unique(cc$node_id[order(pt[as.character(cc$node_id)])])) {
        g <- cc[cc$node_id == nid, ]; g <- g[order(g$pos_start), ]
        doc <- officer::body_add_par(doc, sprintf("%s (%d referência(s))", pt[as.character(nid)], nrow(g)), style = "heading 2")
        for (i in seq_len(nrow(g)))
          doc <- officer::body_add_par(doc, sprintf("[%s] %s", us$name[match(g$user_id[i], us$id)],
                                                    trecho(tx, g$pos_start[i], g$pos_end[i])), style = "Normal")
      }
      a <- S$anns(); a <- a[a$doc_id == d$id, ]
      if (nrow(a)) {
        doc <- officer::body_add_par(doc, "Anotações", style = "heading 1")
        for (i in seq_len(nrow(a))) doc <- officer::body_add_par(doc, sprintf("%d. \u201c%s\u201d \u2014 %s", i,
                                                                 trunca(trecho(tx, a$pos_start[i], a$pos_end[i]), 120), a$body[i]), style = "Normal")
      }
      print(doc, target = file)
    })

  # importar livro de nós (Excel ou CSV)
  observeEvent(input$rb_imp_codebook, {
    if (!S$pode_editar()) return()
    showModal(modalDialog(title = tagList(icon("book"), " Importar livro de nós"), size = "m",
      p("Planilha (.xlsx ou .csv) com uma coluna ", tags$b("Nó"), " e, opcionalmente, ", tags$b("Descrição"), " e ", tags$b("Cor"), ". ",
        "Para hierarquia, escreva o caminho com barra invertida, ex.: ", tags$code("Família \\ Apoio da mãe"),
        ", ou use as colunas ", tags$b("Pai"), " e ", tags$b("Nó"), "."),
      fileInput("cb_file", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Escolher\u2026", width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("cb_ok", "Importar", class = "btn-primary"))))
  })
  observeEvent(input$cb_ok, {
    f <- input$cb_file; req(f)
    df <- tryCatch(if (grepl("\\.csv$", f$name, ignore.case = TRUE)) utils::read.csv(f$datapath, encoding = "UTF-8", check.names = FALSE)
                   else openxlsx::read.xlsx(f$datapath, check.names = FALSE, sep.names = " "), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) { showNotification("Não foi possível ler a planilha.", type = "error"); return() }
    names(df) <- decodificar_entidades(names(df)); df[] <- lapply(df, decodificar_entidades)
    nms <- tolower(trimws(names(df)))
    col <- function(...) { k <- which(nms %in% c(...)); if (length(k)) as.character(df[[k[1]]]) else rep(NA_character_, nrow(df)) }
    cod <- col("nó", "no", "nós", "código", "codigo", "code", "node", "nome", "name"); desc <- col("descrição", "descricao", "description", "definição")
    cor <- col("cor", "color"); pai <- col("pai", "parent")
    n <- 0
    achar_ou_criar <- function(caminho, d = "", cr = NA) {
      partes <- trimws(strsplit(caminho, "\\s*[\\\\>/]\\s*", perl = TRUE)[[1]]); partes <- partes[nzchar(partes)]
      pid <- NA
      for (k in seq_along(partes)) {
        ex <- DBI::dbGetQuery(con, "SELECT id FROM nodes WHERE kind='code' AND lower(name)=lower(?) AND parent_id IS ?",
                              params = list(partes[k], if (is.na(pid)) NA_integer_ else as.integer(pid)))
        if (nrow(ex)) { pid <- ex$id[1]; if (k == length(partes) && nzchar(d %||% "")) DBI::dbExecute(con, "UPDATE nodes SET description=? WHERE id=?", params = list(d, pid)) }
        else { pid <- db_criar_node(con, partes[k], "code", pid, if (k == length(partes) && grepl("^#[0-9A-Fa-f]{6}$", cr %||% "")) cr else NULL,
                                    if (k == length(partes)) d %||% "" else "", S$me()); n <<- n + 1 }
      }
    }
    for (i in seq_along(cod)) {
      if (is.na(cod[i]) || !nzchar(trimws(cod[i]))) next
      caminho <- if (!is.na(pai[i]) && nzchar(pai[i])) paste(pai[i], "\\", cod[i]) else cod[i]
      achar_ou_criar(caminho, if (is.na(desc[i])) "" else desc[i], cor[i])
    }
    S$log("importou livro de nós", sprintf("%d nós", n)); removeModal()
    showNotification(sprintf("%d nó(s) criado(s).", n)); S$ir_secao("codes")
  })

  output$dl_backup <- downloadHandler(
    filename = function() paste0(slug_projeto(db_meta(con, "project_name")), "_backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".sqlite"),
    content = function(file) {
      if (!S$eh_admin()) stop("Somente administradores")
      tmp <- tempfile(fileext = ".sqlite"); db_backup(con, tmp); file.copy(tmp, file, overwrite = TRUE)
      S$log("fez cópia de segurança", "")
    })
}
