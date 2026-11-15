# ---------------------------------------------------------------------------
# QualiR - codificação e gestão de nós/casos
# ---------------------------------------------------------------------------

server_codificacao <- function(input, output, session, S) {
  con <- S$con

  observeEvent(input$doc_sel, {
    s <- input$doc_sel
    if (is.null(s$start) || s$start < 0) S$doc_sel(NULL) else S$doc_sel(c(as.integer(s$start), as.integer(s$end)))
  })

  doc_aberto <- function() {
    d <- isolate(S$detail())
    if (!identical(d$type, "doc")) { showNotification("Abra um arquivo para codificar.", type = "warning"); return(NULL) }
    d$id
  }

  # aplica a codificação de [s,e) no nó
  codificar <- function(node_id, s, e) {
    did <- doc_aberto(); if (is.null(did)) return(FALSE)
    if (!S$pode_editar()) return(FALSE)
    tx <- isolate(S$doc_texto())
    se <- aparar_selecao(tx, s, e)
    if (is.null(se)) { showNotification("Selecione um trecho do texto.", type = "warning"); return(FALSE) }
    n <- DBI::dbGetQuery(con, "SELECT * FROM nodes WHERE id=?", params = list(as.integer(node_id)))
    if (!nrow(n)) return(FALSE)
    db_codificar(con, did, node_id, se[1], se[2], S$me())
    dd <- isolate(S$docs())
    S$log("codificou", sprintf("“%s” em %s (%s)", trunca(trecho(tx, se[1], se[2]), 50), n$name,
                               dd$name[dd$id == did]))
    S$cur_node(as.integer(node_id))
    TRUE
  }
  S$codificar <- codificar

  observeEvent(input$code_apply, {
    a <- input$code_apply
    codificar(as.integer(a$node), a$start, a$end)
  })

  observeEvent(input$qc_go, {
    sel <- S$doc_sel()
    if (is.null(sel)) { showNotification("Selecione um trecho do texto.", type = "warning"); return() }
    v <- input$qc_node %||% ""
    if (!nzchar(v)) { showNotification("Escolha um nó na barra “Codificar em”.", type = "warning"); return() }
    if (grepl("^\\d+$", v)) {
      codificar(as.integer(v), sel[1], sel[2])
    } else {               # nome digitado: cria nó novo na raiz
      if (!S$pode_editar()) return()
      nid <- db_criar_node(con, trimws(v), "code", NA, cor_aleatoria(nrow(S$nodes()) + 1), "", S$me())
      S$log("criou nó", v)
      codificar(nid, sel[1], sel[2])
      updateSelectizeInput(session, "qc_node", choices = S$escolhas_nodes(), selected = nid)
    }
  })

  observeEvent(input$dt_codsel, {
    sel <- S$doc_sel(); v <- input$qc_node %||% ""
    if (is.null(sel)) { showNotification("Selecione um trecho do texto.", type = "warning"); return() }
    if (nzchar(v)) { shinyjs::click("qc_go"); return() }
    session$sendCustomMessage("qr_ctx_open", list())
  })
  observeEvent(input$dt_newcode, novo_com_selecao(S$doc_sel()))
  observeEvent(input$dt_uncode, modal_descodificar(S$doc_sel()))
  observeEvent(input$rb_codificar, {
    sel <- S$doc_sel(); n <- S$cur_node()
    if (is.null(sel) || is.null(n)) {
      showNotification("Selecione um trecho no arquivo aberto e um nó (na lista, na árvore ou na barra “Codificar em”).",
                       type = "warning", duration = 6); return()
    }
    codificar(n, sel[1], sel[2])
  })

  # novo nó a partir da seleção
  novo_com_selecao <- function(sel) {
    if (is.null(doc_aberto())) return()
    if (is.null(sel)) { showNotification("Selecione um trecho do texto.", type = "warning"); return() }
    S$modal_node("new", kind = "code", code_sel = sel)
  }
  observeEvent(input$code_new_sel, novo_com_selecao(c(input$code_new_sel$start, input$code_new_sel$end)))
  observeEvent(input$qc_new, novo_com_selecao(S$doc_sel()))
  observeEvent(input$rb_cod_novo, novo_com_selecao(S$doc_sel()))

  # ---------------- descodificar -----------------------------------------
  modal_descodificar <- function(sel) {
    did <- doc_aberto(); if (is.null(did)) return()
    if (is.null(sel)) { showNotification("Selecione o trecho a descodificar.", type = "warning"); return() }
    cc <- S$cods(); cc <- cc[cc$doc_id == did & cc$pos_start < sel[2] & cc$pos_end > sel[1], ]
    if (!S$eh_admin()) cc <- cc[cc$user_id == S$me(), ]
    if (!nrow(cc)) { showNotification("Não há codificação sua neste trecho.", type = "warning"); return() }
    pt <- S$paths(); ids <- unique(cc$node_id)
    S$uncode_ctx <- list(doc = did, sel = sel)
    showModal(modalDialog(title = "Descodificar seleção", size = "m",
      p("Remover a codificação do trecho selecionado nos nós marcados:"),
      checkboxGroupInput("unc_nodes", NULL, choices = setNames(as.character(ids), pt[as.character(ids)]),
                         selected = as.character(intersect(ids, S$cur_node() %||% ids))),
      if (S$eh_admin()) tags$small("Como administrador, você remove a codificação de todos os membros neste trecho."),
      footer = tagList(modalButton("Cancelar"), actionButton("unc_ok", "Descodificar", class = "btn-primary"))))
  }
  observeEvent(input$uncode_sel_menu, modal_descodificar(c(input$uncode_sel_menu$start, input$uncode_sel_menu$end)))
  observeEvent(input$rb_descodificar, modal_descodificar(S$doc_sel()))
  observeEvent(input$qc_uncode, {
    v <- input$qc_node %||% ""; sel <- S$doc_sel()
    if (nzchar(v) && grepl("^\\d+$", v) && !is.null(sel)) {
      did <- doc_aberto(); if (is.null(did) || !S$pode_editar()) return()
      cc <- S$cods(); cc <- cc[cc$doc_id == did & cc$node_id == as.integer(v) & cc$pos_start < sel[2] & cc$pos_end > sel[1], ]
      if (!S$eh_admin()) cc <- cc[cc$user_id == S$me(), ]
      if (!nrow(cc)) { showNotification("Este trecho não tem sua codificação nesse nó.", type = "warning"); return() }
      db_descodificar_intervalo(con, cc, sel[1], sel[2])
      S$log("descodificou", S$paths()[v])
    } else modal_descodificar(sel)
  })
  observeEvent(input$unc_ok, {
    ctx <- S$uncode_ctx; req(ctx); if (!S$pode_editar()) return()
    cc <- S$cods(); cc <- cc[cc$doc_id == ctx$doc & cc$node_id %in% as.integer(input$unc_nodes) &
                             cc$pos_start < ctx$sel[2] & cc$pos_end > ctx$sel[1], ]
    if (!S$eh_admin()) cc <- cc[cc$user_id == S$me(), ]
    db_descodificar_intervalo(con, cc, ctx$sel[1], ctx$sel[2])
    S$log("descodificou", paste(S$paths()[input$unc_nodes], collapse = ", "))
    removeModal()
  })

  # clicar em trecho codificado: mostra as codificações naquele ponto
  observeEvent(input$seg_click, {
    d <- S$detail(); req(identical(d$type, "doc"))
    pos <- input$seg_click$pos
    cc <- S$doc_cc(); cc <- cc[cc$pos_start <= pos & cc$pos_end > pos, ]
    if (!nrow(cc)) return()
    tx <- S$doc_texto(); pt <- S$paths()
    showModal(modalDialog(title = tagList(icon("highlighter"), " Codificação neste ponto"), size = "l", easyClose = TRUE,
      lapply(seq_len(nrow(cc)), function(i) {
        r <- cc[i, ]
        pode <- S$eh_admin() || r$user_id == S$me()
        div(class = "ref", style = sprintf("border-left-color:%s", r$color),
          div(class = "rmeta",
              tags$b(pt[as.character(r$node_id)]), tags$span(icon("user"), r$uname), tags$span(fmt_data(r$created)),
              tags$a(href = "#", onclick = sprintf("QualiR.send('open_node_from_modal',{id:%d});return false;", r$node_id),
                     "abrir nó"),
              if (pode) tags$a(href = "#", style = "color:#C0392B",
                               onclick = sprintf("QualiR.send('rm_coding',{id:%d});return false;", r$id),
                               icon("trash-can"), "remover esta codificação")),
          div(class = "rtext", trunca(trecho(tx, r$pos_start, r$pos_end), 600)))
      }),
      footer = modalButton("Fechar")))
  })
  observeEvent(input$rm_coding, {
    if (!S$pode_editar()) return()
    id <- as.integer(input$rm_coding$id)
    r <- S$cods(); r <- r[r$id == id, ]; req(nrow(r))
    if (!(S$eh_admin() || r$user_id == S$me())) { showNotification("Você só pode remover sua própria codificação.", type = "error"); return() }
    db_descodificar(con, id); S$log("removeu codificação", S$paths()[as.character(r$node_id)])
    removeModal()
  })
  observeEvent(input$open_node_from_modal, { removeModal(); S$detail(list(type = "node", id = as.integer(input$open_node_from_modal$id))) })

  # ---------------- criar / editar nós e casos --------------------------
  paleta_ui <- function() div(class = "palette", lapply(QR_PALETA, function(c) tags$span(style = sprintf("background:%s", c), `data-c` = c)))

  S$modal_node <- function(modo, kind = "code", id = NULL, parent = NA, code_sel = NULL) {
    if (!S$pode_editar()) return()
    nd <- isolate(S$nodes()); pt <- isolate(S$paths())
    n <- if (!is.null(id)) nd[nd$id == id, ] else NULL
    if (!is.null(n)) kind <- n$kind
    excl <- if (!is.null(n)) descendentes(nd, n$id) else integer()
    cand <- nd[nd$kind == kind & !(nd$id %in% excl), ]
    cand <- cand[order(tolower(pt[as.character(cand$id)])), ]
    pais <- c("(nenhum — nível superior)" = "", setNames(as.character(cand$id), pt[as.character(cand$id)]))
    rot <- if (kind == "case") "caso" else "nó"
    S$modal(list(modo = modo, kind = kind, id = id, code_sel = code_sel, doc = isolate(S$detail())$id))
    cor <- if (!is.null(n)) n$color else cor_aleatoria(nrow(nd) + 1)
    showModal(modalDialog(
      title = if (modo == "new") paste("Novo", rot) else paste("Propriedades do", rot),
      textInput("nm_name", "Nome", value = if (!is.null(n)) n$name else "", width = "100%"),
      if (kind == "code") selectInput("nm_parent", "Nó pai (hierarquia)", pais, width = "100%",
                                      selected = if (!is.null(n)) as.character(n$parent_id %||% "") else as.character(parent %||% "")),
      textAreaInput("nm_desc", "Descrição / definição (livro de nós)", value = if (!is.null(n)) n$description %||% "" else "",
                    width = "100%", rows = 3),
      tags$label("Cor"), div(tags$input(id = "nm_color", type = "color", class = "qr-color", value = cor), paleta_ui()),
      if (!is.null(code_sel)) div(class = "alert alert-info py-1 mt-2", icon("highlighter"), " O trecho selecionado será codificado neste nó."),
      if (modo == "edit") tagList(tags$hr(),
        div(style = "display:flex;gap:8px;align-items:flex-end",
            selectInput("nm_merge_into", paste("Mesclar este", rot, "em:"), pais[-1], width = "70%"),
            actionButton("nm_merge", "Mesclar", icon = icon("code-merge"), class = "btn-outline-primary mb-3"))),
      footer = tagList(
        if (modo == "edit") actionButton("nm_delete", "Excluir", icon = icon("trash-can"), class = "btn-outline-danger me-auto"),
        modalButton("Cancelar"), actionButton("nm_save", "Salvar", class = "btn-primary")),
      easyClose = FALSE))
  }

  observeEvent(input$rb_novo_codigo, S$modal_node("new", "code"))
  observeEvent(input$rb_novo_codigo_l, S$modal_node("new", "code"))
  observeEvent(input$rb_novo_caso, S$modal_node("new", "case"))
  observeEvent(input$rb_novo_caso_l, S$modal_node("new", "case"))
  observeEvent(input$rb_novo_subcodigo, {
    n <- S$cur_node(); nd <- S$nodes()
    if (is.null(n) || !(n %in% nd$id[nd$kind == "code"])) {
      showNotification("Selecione primeiro o nó pai na lista de nós.", type = "warning"); return() }
    S$modal_node("new", "code", parent = n)
  })
  observeEvent(input$rb_editar_codigo, {
    n <- S$cur_node()
    if (is.null(n) || !(n %in% S$nodes()$id)) { showNotification("Selecione um nó.", type = "warning"); return() }
    S$modal_node("edit", id = n)
  })
  observeEvent(input$rb_mesclar, {
    n <- S$cur_node()
    if (is.null(n) || !(n %in% S$nodes()$id)) { showNotification("Selecione o nó a ser mesclado.", type = "warning"); return() }
    S$modal_node("edit", id = n)
  })

  observeEvent(input$nm_save, {
    m <- S$modal(); req(m)
    nome <- trimws(input$nm_name %||% "")
    if (!nzchar(nome)) { showNotification("Informe um nome.", type = "error"); return() }
    pai <- if (m$kind == "code" && nzchar(input$nm_parent %||% "")) as.integer(input$nm_parent) else NA
    nd <- S$nodes()
    irmaos <- nd[nd$kind == m$kind & ((is.na(pai) & is.na(nd$parent_id)) | (!is.na(nd$parent_id) & nd$parent_id %in% pai)), ]
    if (tolower(nome) %in% tolower(irmaos$name[irmaos$id != (m$id %||% -1)])) {
      showNotification("Já existe um item com esse nome neste nível.", type = "error"); return() }
    cor <- input$nm_color %||% cor_aleatoria()
    if (m$modo == "new") {
      nid <- db_criar_node(con, nome, m$kind, pai, cor, input$nm_desc %||% "", S$me())
      S$log(if (m$kind == "case") "criou caso" else "criou nó", nome)
      removeModal()
      if (!is.null(m$code_sel)) {
        if (identical(S$detail()$type, "doc") && identical(S$detail()$id, m$doc)) codificar(nid, m$code_sel[1], m$code_sel[2])
      }
      S$cur_node(nid)
    } else {
      db_atualizar_node(con, m$id, nome, pai, cor, input$nm_desc %||% "", S$me())
      S$log("editou", nome); removeModal()
    }
  })

  observeEvent(input$nm_delete, {
    m <- S$modal(); req(m$id)
    nd <- S$nodes(); ids <- descendentes(nd, m$id); cc <- S$cods()
    showModal(modalDialog(title = "Confirmar exclusão",
      sprintf("Excluir “%s”%s e %d referência(s) codificada(s)? Esta ação não pode ser desfeita.",
              nd$name[nd$id == m$id], if (length(ids) > 1) sprintf(", %d nó filho(s)", length(ids) - 1) else "",
              sum(cc$node_id %in% ids)),
      footer = tagList(modalButton("Cancelar"), actionButton("nm_delete_ok", "Excluir", class = "btn-danger"))))
  })
  observeEvent(input$nm_delete_ok, {
    m <- S$modal(); req(m$id); if (!S$pode_editar()) return()
    nome <- S$nodes()$name[S$nodes()$id == m$id]
    db_excluir_node(con, m$id); S$log("excluiu", nome); removeModal()
    if (identical(S$detail()$type, "node") && S$detail()$id %in% m$id) S$detail(list(type = "none"))
    S$cur_node(NULL)
  })
  observeEvent(input$nm_merge, {
    m <- S$modal(); req(m$id, nzchar(input$nm_merge_into %||% ""))
    dest <- as.integer(input$nm_merge_into); nd <- S$nodes()
    if (dest %in% descendentes(nd, m$id)) { showNotification("Não é possível mesclar em um nó filho.", type = "error"); return() }
    db_mesclar_nodes(con, m$id, dest)
    S$log("mesclou", sprintf("%s em %s", nd$name[nd$id == m$id], nd$name[nd$id == dest]))
    removeModal()
    if (identical(S$detail()$type, "node") && identical(S$detail()$id, m$id)) S$detail(list(type = "node", id = dest))
    S$cur_node(dest)
  })

  # arrastar um nó para outro (muda hierarquia)
  observeEvent(input$node_move, {
    if (!S$pode_editar()) return()
    mv <- input$node_move; nd <- S$nodes()
    id <- as.integer(mv$node); pai <- if (is.null(mv$parent)) NA else as.integer(mv$parent)
    n <- nd[nd$id == id, ]; req(nrow(n))
    if (!is.na(pai)) {
      p <- nd[nd$id == pai, ]
      if (!nrow(p) || p$kind != n$kind || n$kind != "code") return()
      if (pai %in% descendentes(nd, id)) { showNotification("Não é possível mover um nó para dentro de si mesmo.", type = "error"); return() }
    }
    if (identical(n$parent_id, pai) || (is.na(n$parent_id) && is.na(pai))) return()
    db_mover_node(con, id, pai)
    S$log("moveu nó", sprintf("%s → %s", n$name, if (is.na(pai)) "(raiz)" else nd$name[nd$id == pai]))
  })
}


# ---------------------------------------------------------------------------
# Autocodificação e nó de intervalo
# ---------------------------------------------------------------------------
server_autocod <- function(input, output, session, S) {
  con <- S$con
  observeEvent(input$rb_autocod, {
    if (!S$pode_editar()) return()
    dd <- S$docs(); d <- S$detail()
    showModal(modalDialog(title = tagList(icon("wand-magic-sparkles"), " Autocodificar"), size = "m",
      selectizeInput("ac_docs", "Arquivos", setNames(as.character(dd$id), dd$name), multiple = TRUE, width = "100%",
                     selected = if (identical(d$type, "doc")) as.character(d$id) else NULL,
                     options = list(placeholder = "Escolha os arquivos", plugins = list("remove_button"))),
      radioButtons("ac_modo", "Método", c("Falantes \u2192 casos (um caso por falante)" = "spk_case",
                                         "Falantes \u2192 nós (um nó por falante)" = "spk_code",
                                         "Parágrafos que contêm um termo \u2192 nó" = "term")),
      conditionalPanel("input.ac_modo != 'term'",
        textInput("ac_ign", "Ignorar estes falantes (separados por vírgula)", "", width = "100%")),
      conditionalPanel("input.ac_modo == 'term'",
        textInput("ac_term", "Termo (aceita * e OU)", "", width = "100%"),
        textInput("ac_code", "Nome do nó", "", width = "100%")),
      footer = tagList(modalButton("Cancelar"), actionButton("ac_ok", "Autocodificar", class = "btn-primary"))))
  })
  observeEvent(input$ac_ok, {
    ids <- as.integer(input$ac_docs)
    if (!length(ids)) { showNotification("Escolha ao menos um arquivo.", type = "warning"); return() }
    td <- DBI::dbGetQuery(con, sprintf("SELECT id, name, content FROM documents WHERE id IN (%s)", paste(ids, collapse = ",")))
    nd <- S$nodes(); n <- 0
    if (input$ac_modo %in% c("spk_case", "spk_code")) {
      kind <- if (input$ac_modo == "spk_case") "case" else "code"
      ign <- tolower(trimws(strsplit(input$ac_ign %||% "", ",")[[1]]))
      pai <- NA
      if (kind == "code") {
        ex <- nd$id[nd$kind == "code" & nd$name == "Falantes" & is.na(nd$parent_id)]
        pai <- if (length(ex)) ex[1] else db_criar_node(con, "Falantes", "code", NA, "#7F8C8D", "Criado por autocodificação", S$me())
      }
      for (i in seq_len(nrow(td))) {
        t <- turnos_de_fala(td$content[i]); t <- t[!(tolower(t$falante) %in% ign), , drop = FALSE]
        for (f in unique(t$falante)) {
          atual <- DBI::dbGetQuery(con, "SELECT id FROM nodes WHERE kind=? AND lower(name)=lower(?) AND (parent_id IS ? )",
                                   params = list(kind, f, if (is.na(pai)) NA_integer_ else as.integer(pai)))
          nid <- if (nrow(atual)) atual$id[1] else db_criar_node(con, f, kind, pai, NULL, "", S$me())
          tt <- t[t$falante == f, ]
          for (k in seq_len(nrow(tt))) { db_codificar(con, td$id[i], nid, tt$fala_start[k], tt$fala_end[k], S$me(), tocar = FALSE); n <- n + 1 }
        }
      }
    } else {
      termo <- trimws(input$ac_term %||% ""); nome <- trimws(input$ac_code %||% "")
      if (!nzchar(termo) || !nzchar(nome)) { showNotification("Informe o termo e o nome do nó.", type = "warning"); return() }
      nid <- db_criar_node(con, nome, "code", NA, NULL, sprintf("Autocodificado: parágrafos com \u201c%s\u201d", termo), S$me())
      r <- busca_texto(td, termo, 1)
      for (i in seq_len(nrow(r))) {
        lp <- limites_paragrafo(td$content[td$id == r$doc_id[i]], r$pos_start[i], r$pos_end[i])
        db_codificar(con, r$doc_id[i], nid, lp[1], lp[2], S$me(), tocar = FALSE); n <- n + 1
      }
    }
    db_tocar(con); S$log("autocodificou", sprintf("%d trechos", n)); removeModal()
    showNotification(sprintf("Autocodificação concluída: %d trecho(s).", n))
  })

  observeEvent(input$rb_intervalo, {
    d <- S$detail()
    if (!identical(d$type, "doc")) { showNotification("Abra um arquivo para usar o nó de intervalo.", type = "warning"); return() }
    if (!S$pode_editar()) return()
    np <- length(strsplit(S$doc_texto(), "\n", fixed = TRUE)[[1]])
    showModal(modalDialog(title = tagList(icon("arrows-up-down"), " Codificar intervalo"), size = "s",
      p(sprintf("Codifica os parágrafos de um intervalo (o arquivo tem %d parágrafos; os números aparecem à esquerda do texto).", np)),
      numericInput("ri_de", "Do parágrafo", 1, 1, np), numericInput("ri_ate", "Até o parágrafo", min(np, 5), 1, np),
      selectizeInput("ri_node", "Nó ou caso", S$escolhas_nodes(), options = SELECTIZE_CRIAR("Escolha ou digite um novo nó")),
      footer = tagList(modalButton("Cancelar"), actionButton("ri_ok", "Codificar", class = "btn-primary"))))
  })
  observeEvent(input$ri_ok, {
    tx <- S$doc_texto(); paras <- strsplit(tx, "\n", fixed = TRUE)[[1]]
    lens <- nchar(paras); ini <- c(0L, cumsum(lens + 1L))[seq_along(paras)]
    a <- max(1, min(input$ri_de, input$ri_ate)); b <- min(length(paras), max(input$ri_de, input$ri_ate))
    v <- input$ri_node %||% ""
    if (!nzchar(v)) { showNotification("Escolha o nó.", type = "warning"); return() }
    nid <- if (grepl("^\\d+$", v)) as.integer(v) else db_criar_node(con, trimws(v), "code", NA, NULL, "", S$me())
    removeModal()
    S$codificar(nid, ini[a], ini[b] + lens[b])
  })
}
