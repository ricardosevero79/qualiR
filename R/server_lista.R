# ---------------------------------------------------------------------------
# QualiR - painel de lista 
# ---------------------------------------------------------------------------

MAPAS <- c(hier = "Gráfico hierárquico de nós", proj = "Mapa do projeto", rel = "Mapa de relacionamentos",
           cooc = "Rede de coocorrência", docchart = "Gráfico de codificação por arquivo")
MAPAS_DESC <- c(hier = "Retângulos proporcionais às referências de cada nó e nó filho",
                proj = "Arquivos, nós e casos ligados pela codificação",
                rel = "Nós e casos ligados pelos relacionamentos definidos",
                cooc = "Nós que aparecem juntos nos mesmos trechos",
                docchart = "Cobertura dos nós em cada arquivo")
RELATORIOS <- c(codigos = "Resumo dos nós", arquivos = "Resumo dos arquivos", casos = "Resumo dos casos e atributos",
                cod_arquivo = "Codificação por arquivo", trechos = "Trechos codificados por nó",
                anotacoes = "Anotações e memorandos", equipe = "Codificação por membro da equipe")

server_lista <- function(input, output, session, S) {
  con <- S$con

  titulos <- list(quick = c("Itens recentes", "clock-rotate-left"), docs = c("Arquivos", "file-lines"),
                  classif_docs = c("Classificações de arquivo", "table-columns"), codes = c("Nós", "circle"),
                  rels = c("Relacionamentos", "arrows-left-right"), reltypes = c("Tipos de relacionamento", "tags"),
                  cases = c("Casos", "briefcase"), classif = c("Classificações de caso", "table-list"),
                  memos = c("Memorandos", "note-sticky"), annots = c("Anotações", "comment-dots"),
                  sets = c("Conjuntos", "circle-half-stroke"), queries = c("Consultas", "magnifying-glass"),
                  results = c("Resultados da consulta", "square-poll-horizontal"), matrices = c("Matrizes de codificação", "table-cells"),
                  maps = c("Mapas e gráficos", "diagram-project"), reports = c("Relatórios", "file-lines"),
                  activity = c("Atividade da equipe", "clock-rotate-left"), users = c("Usuários", "users-gear"))
  output$list_title <- renderUI({
    sec <- S$sec(); t <- titulos[[sec]] %||% c("", "file")
    acao <- switch(sec,
      users = actionLink("user_new", tagList(icon("user-plus"), " Novo usuário")),
      classif = actionLink("attr_new", tagList(icon("plus"), " Novo atributo")),
      classif_docs = actionLink("attr_new_doc", tagList(icon("plus"), " Novo atributo")),
      codes = actionLink("rb_novo_codigo_l", tagList(icon("plus"), " Novo nó")),
      cases = actionLink("rb_novo_caso_l", tagList(icon("plus"), " Novo caso")),
      rels = actionLink("rb_novo_rel_l", tagList(icon("plus"), " Novo relacionamento")),
      reltypes = actionLink("rb_novo_reltipo_l", tagList(icon("plus"), " Novo tipo")),
      sets = actionLink("rb_novo_conjunto_l", tagList(icon("plus"), " Novo conjunto")),
      memos = actionLink("rb_nova_nota_l", tagList(icon("plus"), " Novo memorando")),
      docs = actionLink("rb_importar_l", tagList(icon("file-import"), " Importar")),
      NULL)
    tags$span(class = "ptitle", t[1], acao)
  })

  output$list_view <- renderUI({
    req(S$user())
    sec <- S$sec(); nd <- S$nodes(); cc <- S$cods(); us <- S$users()
    nome_u <- function(id) { x <- us$name[match(id, us$id)]; x[is.na(x)] <- ""; x }
    n <- 0
    lista_nos <- function(kind, tipo) {
      n2 <- ordenar_arvore(nd[nd$kind == kind, ])
      if (is.null(n2) || !nrow(n2)) return(list(html = render_lista(data.frame(), integer(), tipo), n = 0))
      cont <- contagens_nodes(n2, cc)
      df <- data.frame(Nome = n2$name, Arquivos = cont$n_docs, `Referências` = cont$n_refs,
                       `Criado em` = fmt_data(n2$created), `Criado por` = nome_u(n2$created_by),
                       `Modificado em` = fmt_data(n2$modified), `Modificado por` = nome_u(n2$modified_by), check.names = FALSE)
      list(html = render_lista(df, n2$id, tipo, depth = n2$depth, parents = n2$parent_id, cores = n2$color,
                               arrastavel = TRUE, circulo = TRUE), n = nrow(n2))
    }
    html <- switch(sec,
      quick = {
        r <- db_recentes(con, S$me())
        dd <- S$docs(); mm <- S$memos(); st <- S$sets(); mx <- S$matrices()
        if (!nrow(r)) "<div class='list-empty'>Os itens que você abrir aparecerão aqui para acesso rápido.</div>" else {
          nome <- vapply(seq_len(nrow(r)), function(i) {
            id <- r$item_id[i]
            x <- switch(r$item_type[i], doc = dd$name[dd$id == id], node = nd$name[nd$id == id],
                        memo = mm$title[mm$id == id], query = QUERY_TIPOS[id], set = st$name[st$id == id],
                        matrix = mx$name[mx$id == id], map = MAPAS[id], report = RELATORIOS[id], NA)
            if (!length(x) || is.na(x[1])) NA_character_ else unname(x[1])
          }, "")
          ok <- !is.na(nome); r <- r[ok, ]; nome <- nome[ok]
          tipo_nm <- c(doc = "Arquivo", node = "Nó/Caso", memo = "Memorando", query = "Consulta", set = "Conjunto",
                       matrix = "Matriz", map = "Visualização", report = "Relatório")
          ic <- c(doc = "file-lines", node = "circle", memo = "note-sticky", query = "magnifying-glass", set = "circle-half-stroke",
                  matrix = "table-cells", map = "diagram-project", report = "file-lines")
          n <- nrow(r)
          render_lista(data.frame(Nome = nome, Tipo = unname(tipo_nm[r$item_type]), `Aberto em` = fmt_data(r$ts), check.names = FALSE),
                       paste(r$item_type, r$item_id, sep = "|"), "recent", icone = unname(ic[r$item_type]))
        }
      },
      docs = {
        dd <- S$docs(); n <- nrow(dd)
        c2 <- cc[cc$node_id %in% nd$id[nd$kind == "code"], ]
        df <- data.frame(Nome = dd$name,
          `Nós` = vapply(dd$id, function(i) length(unique(c2$node_id[c2$doc_id == i])), 0L),
          `Referências` = vapply(dd$id, function(i) sum(c2$doc_id == i), 0L),
          Tipo = ifelse(dd$is_transcript == 1, paste0("Transcrição (", dd$type, ")"), dd$type),
          `Criado em` = fmt_data(dd$created), `Criado por` = nome_u(dd$created_by),
          `Modificado em` = fmt_data(dd$modified), `Modificado por` = nome_u(dd$modified_by), check.names = FALSE)
        render_lista(df, dd$id, "doc", icone = "file-lines")
      },
      codes = {
        x <- lista_nos("code", "node"); n <- x$n
        paste0(x$html, '<div class="drop-new">Arraste a seleção aqui para codificar para novo nó</div>')
      },
      cases = { x <- lista_nos("case", "case"); n <- x$n; x$html },
      results = {
        x <- lista_nos("result", "node"); n <- x$n
        if (n == 0) "<div class='list-empty'>Resultados salvos de consultas aparecem aqui (use “Salvar resultado” na Pesquisa de texto ou na Consulta de codificação).</div>" else x$html
      },
      rels = {
        r <- S$rels(); rt <- S$reltypes(); pt <- S$paths(); n <- nrow(r)
        seta <- c(associativa = "—", unidirecional = "→", simetrica = "↔")
        tipo <- rt$name[match(r$type_id, rt$id)]; dir <- rt$direction[match(r$type_id, rt$id)]
        df <- data.frame(Nome = sprintf("%s %s %s", pt[as.character(r$from_id)], seta[dir] %||% "—", pt[as.character(r$to_id)]),
                         De = unname(pt[as.character(r$from_id)]), Tipo = tipo, Para = unname(pt[as.character(r$to_id)]),
                         `Descrição` = trunca(r$description %||% "", 60), `Criado por` = nome_u(r$created_by),
                         `Criado em` = fmt_data(r$created), check.names = FALSE)
        render_lista(df, r$id, "rel", icone = "arrows-left-right")
      },
      reltypes = {
        rt <- S$reltypes(); r <- S$rels(); n <- nrow(rt)
        dirn <- c(associativa = "Associativa (—)", unidirecional = "Unidirecional (→)", simetrica = "Simétrica (↔)")
        df <- data.frame(Nome = rt$name, `Direção` = unname(dirn[rt$direction]),
                         Relacionamentos = vapply(rt$id, function(i) sum(r$type_id %in% i), 0L), check.names = FALSE)
        render_lista(df, rt$id, "reltype", icone = "tags")
      },
      memos = {
        mm <- S$memos(); dd <- S$docs(); pt <- S$paths(); n <- nrow(mm)
        vinc <- ifelse(is.na(mm$link_type) | mm$link_type == "projeto", "Projeto",
                 ifelse(mm$link_type == "doc", paste("Arquivo:", dd$name[match(mm$link_id, dd$id)]),
                        paste("Nó:", pt[as.character(mm$link_id)])))
        df <- data.frame(Nome = mm$title, `Vinculado a` = vinc, `Criado por` = nome_u(mm$user_id),
                         `Criado em` = fmt_data(mm$created), `Modificado em` = fmt_data(mm$modified), check.names = FALSE)
        render_lista(df, mm$id, "memo", icone = "note-sticky")
      },
      annots = {
        a <- S$anns(); dd <- S$docs(); n <- nrow(a)
        if (!n) "<div class='list-empty'>Nenhuma anotação. Selecione um trecho em um arquivo e use “Nova anotação”.</div>" else {
          tx <- DBI::dbGetQuery(con, sprintf("SELECT id, content FROM documents WHERE id IN (%s)", paste(unique(a$doc_id), collapse = ",")))
          df <- data.frame(Arquivo = dd$name[match(a$doc_id, dd$id)],
                           `Trecho anotado` = trunca(substr(tx$content[match(a$doc_id, tx$id)], a$pos_start + 1, a$pos_end), 60),
                           `Anotação` = trunca(a$body, 80), `Criado por` = nome_u(a$user_id), `Criado em` = fmt_data(a$created),
                           check.names = FALSE)
          render_lista(df, a$id, "annot", icone = "comment-dots")
        }
      },
      sets = {
        st <- S$sets(); si <- S$set_items(); n <- nrow(st)
        df <- data.frame(Nome = st$name, Membros = vapply(st$id, function(i) sum(si$set_id == i), 0L),
                         `Descrição` = trunca(st$description %||% "", 60), `Criado por` = nome_u(st$created_by),
                         `Criado em` = fmt_data(st$created), check.names = FALSE)
        render_lista(df, st$id, "set", icone = "circle-half-stroke")
      },
      queries = {
        sq <- S$squeries(); n <- length(QUERY_TIPOS) + nrow(sq)
        df <- data.frame(Nome = unname(QUERY_TIPOS), `Descrição` = unname(QUERY_DESC), check.names = FALSE)
        h <- render_lista(df, names(QUERY_TIPOS), "query", icone = unname(QUERY_ICONES))
        if (nrow(sq)) {
          df2 <- data.frame(Nome = sq$name, Tipo = unname(QUERY_TIPOS[sq$qtype]), `Criada por` = nome_u(sq$user_id),
                            `Criada em` = fmt_data(sq$created), check.names = FALSE)
          h <- paste0(h, '<div class="list-sub">CONSULTAS SALVAS</div>', render_lista(df2, sq$id, "squery", icone = "floppy-disk"))
        }
        h
      },
      matrices = {
        mx <- S$matrices(); n <- nrow(mx)
        if (!n) "<div class='list-empty'>Execute uma Matriz de codificação e clique em “Salvar matriz” para guardá-la aqui.</div>" else
          render_lista(data.frame(Nome = mx$name, `Criada por` = nome_u(mx$user_id), `Criada em` = fmt_data(mx$created), check.names = FALSE),
                       mx$id, "matrix", icone = "table-cells")
      },
      maps = {
        n <- length(MAPAS)
        render_lista(data.frame(Nome = unname(MAPAS), `Descrição` = unname(MAPAS_DESC), check.names = FALSE),
                     names(MAPAS), "map", icone = "diagram-project")
      },
      reports = {
        n <- length(RELATORIOS)
        render_lista(data.frame(Nome = unname(RELATORIOS), check.names = FALSE), names(RELATORIOS), "report", icone = "file-lines")
      },
      activity = {
        a <- db_atividade(con); n <- nrow(a)
        df <- data.frame(Quando = fmt_data(a$ts), `Usuário` = a$user_name, `Ação` = a$action, Detalhe = a$detail, check.names = FALSE)
        render_lista(df, a$id, "act", icone = "clock")
      },
      users = {
        n <- nrow(us)
        df <- data.frame(Nome = us$name, `Usuário` = us$username, Perfil = us$role, Ativo = ifelse(us$active == 1, "sim", "não"),
                         `Codificações` = vapply(us$id, function(i) sum(cc$user_id == i), 0L),
                         `Criado em` = fmt_data(us$created), check.names = FALSE)
        render_lista(df, us$id, "user", cores = us$color)
      },
      NULL)
    dica <- tags$small(style = "color:#6B7280;padding:4px 10px;display:block",
                       "Dê duplo clique em uma célula para editar e tecle Enter para salvar.")
    if (sec == "classif") { isolate(S$n_itens(sum(nd$kind == "case"))); return(tagList(dica, DT::DTOutput("classif_dt"))) }
    if (sec == "classif_docs") { isolate(S$n_itens(nrow(S$docs()))); return(tagList(dica, DT::DTOutput("classif_docs_dt"))) }
    isolate(S$n_itens(n))
    HTML(html %||% "")
  })

  # ---- classificação de casos (planilha editável de atributos)
  S$classif_df <- reactive({
    nd <- S$nodes(); at <- S$attrs()
    casos <- nd[nd$kind == "case", ]
    casos <- casos[order(tolower(casos$name)), ]
    nomes <- sort(unique(at$attr))
    df <- data.frame(Caso = casos$name, stringsAsFactors = FALSE)
    for (a in nomes) df[[a]] <- at$value[match(paste(casos$id, a), paste(at$node_id, at$attr))] %||% ""
    df[is.na(df)] <- ""
    list(df = df, ids = casos$id)
  })
  output$classif_dt <- DT::renderDT({
    x <- S$classif_df()
    DT::datatable(x$df, rownames = FALSE, selection = "none",
                  editable = list(target = "cell", disable = list(columns = 0)),
                  options = list(pageLength = 500, dom = "t", scrollY = "60vh", scrollCollapse = TRUE,
                                 language = list(emptyTable = "Nenhum caso. Importe transcrições ou crie casos.")))
  })
  observeEvent(input$classif_dt_cell_edit, {
    if (!S$pode_editar()) return()
    e <- input$classif_dt_cell_edit; x <- isolate(S$classif_df())
    db_set_atributo(con, x$ids[e$row], names(x$df)[e$col + 1], as.character(e$value))
  })
  novo_attr_caso <- function() showModal(modalDialog(title = "Novo atributo de caso", size = "s",
      textInput("attr_name", "Nome do atributo (ex.: Idade, Gênero, Escolaridade)"),
      footer = tagList(modalButton("Cancelar"), actionButton("attr_ok", "Criar", class = "btn-primary"))))
  observeEvent(input$attr_new, novo_attr_caso())
  observeEvent(input$rb_novo_attr_caso, novo_attr_caso())
  observeEvent(input$attr_ok, {
    nm <- trimws(input$attr_name %||% ""); req(nzchar(nm))
    if (!S$pode_editar()) return()
    ids <- S$nodes()$id[S$nodes()$kind == "case"]
    if (!length(ids)) { showNotification("Crie ou importe casos primeiro.", type = "warning"); return() }
    for (i in ids) db_set_atributo(con, i, nm, "")
    removeModal(); S$log("criou atributo", nm); S$ir_secao("classif")
  })

  # ---- classificação de arquivos
  S$classif_docs_df <- reactive({
    dd <- S$docs(); at <- S$dattrs()
    nomes <- sort(unique(at$attr))
    df <- data.frame(Arquivo = dd$name, stringsAsFactors = FALSE)
    for (a in nomes) df[[a]] <- at$value[match(paste(dd$id, a), paste(at$doc_id, at$attr))] %||% ""
    df[is.na(df)] <- ""
    list(df = df, ids = dd$id)
  })
  output$classif_docs_dt <- DT::renderDT({
    x <- S$classif_docs_df()
    DT::datatable(x$df, rownames = FALSE, selection = "none",
                  editable = list(target = "cell", disable = list(columns = 0)),
                  options = list(pageLength = 500, dom = "t", scrollY = "60vh", scrollCollapse = TRUE,
                                 language = list(emptyTable = "Nenhum arquivo importado.")))
  })
  observeEvent(input$classif_docs_dt_cell_edit, {
    if (!S$pode_editar()) return()
    e <- input$classif_docs_dt_cell_edit; x <- isolate(S$classif_docs_df())
    db_set_doc_atributo(con, x$ids[e$row], names(x$df)[e$col + 1], as.character(e$value))
  })
  novo_attr_doc <- function() showModal(modalDialog(title = "Novo atributo de arquivo", size = "s",
      textInput("dattr_name", "Nome do atributo (ex.: Escola, Região, Data da entrevista, Grupo)"),
      footer = tagList(modalButton("Cancelar"), actionButton("dattr_ok", "Criar", class = "btn-primary"))))
  observeEvent(input$attr_new_doc, novo_attr_doc())
  observeEvent(input$rb_novo_attr_arq, novo_attr_doc())
  observeEvent(input$dattr_ok, {
    nm <- trimws(input$dattr_name %||% ""); req(nzchar(nm))
    if (!S$pode_editar()) return()
    ids <- S$docs()$id
    if (!length(ids)) { showNotification("Importe arquivos primeiro.", type = "warning"); return() }
    for (i in ids) db_set_doc_atributo(con, i, nm, "")
    removeModal(); S$log("criou atributo de arquivo", nm); S$ir_secao("classif_docs")
  })
  observeEvent(input$rb_classif_caso, S$ir_secao("classif"))
  observeEvent(input$rb_classif_arq, S$ir_secao("classif_docs"))
}
