# ---------------------------------------------------------------------------
# QualiR - visualização de detalhes (documento, nó/caso, nota)
# ---------------------------------------------------------------------------

server_detalhe <- function(input, output, session, S) {
  con <- S$con

  output$detail_view <- renderUI({
    d <- S$detail()
    isolate(switch(d$type %||% "none",
      doc = ui_doc(d$id), node = ui_node(d$id), memo = ui_memo(d$id),
      query = ui_consulta(d$id, d, S), set = S$ui_set(d$id), matrix = S$ui_matrix(d$id),
      map = S$ui_map(d$id), report = S$ui_report(d$id),
      div(class = "detail-empty", icon("hand-pointer"),
          "Dê um duplo clique em um item da lista para abri-lo aqui.",
          tags$small("Os itens abertos ficam em abas."))))
  })

  cabecalho <- function(ic, titulo, ..., extra = NULL) {
    div(class = "dhead", div(class = "dtitle", icon(ic), tags$span(titulo), ...), extra,
        actionLink("detail_close", icon("xmark"), title = "Fechar"))
  }

  escolhas_nodes <- function(kind = NULL) {
    nd <- S$nodes(); pt <- S$paths()
    cod <- nd[nd$kind == "code", ]; cas <- nd[nd$kind == "case", ]
    o1 <- order(tolower(pt[as.character(cod$id)])); o2 <- order(tolower(cas$name))
    l <- list(`Nós` = setNames(as.character(cod$id[o1]), pt[as.character(cod$id[o1])]),
              Casos = setNames(as.character(cas$id[o2]), cas$name[o2]))
    if (identical(kind, "code")) l$Casos <- NULL
    if (identical(kind, "case")) l$`Nós` <- NULL
    l
  }
  S$escolhas_nodes <- escolhas_nodes

  # ===================== DOCUMENTO =========================================
  tbtn <- function(id, rot, ic, title = rot) tags$button(id = id, type = "button", class = "tb action-button", title = title, icon(ic), rot)
  ui_doc <- function(id) {
    dd <- S$docs(); doc <- dd[dd$id == id, ]
    if (!nrow(doc)) return(div(class = "detail-empty", "Arquivo não encontrado."))
    us <- S$users()
    kind_qc <- isolate(input$qc_in) %||% "code"
    tagList(
      div(class = "doctools",
          tbtn("dt_codsel", "Codificar seleção", "circle-plus"),
          tbtn("dt_newcode", "Novo nó", "circle", "Codificar a seleção em um novo nó"),
          tbtn("dt_uncode", "Descodificar", "circle-minus"),
          div(class = "sep"),
          tbtn("dt_annot", "Nova anotação", "comment-dots"),
          tbtn("doc_memo", "Memorando", "note-sticky", "Memorando vinculado a este arquivo"),
          div(class = "sep"),
          tbtn("dt_chart", "Gráfico", "chart-simple", "Gráfico de codificação deste arquivo"),
          tbtn("dt_query", "Consulta", "magnifying-glass", "Pesquisar texto neste arquivo"),
          tbtn("dt_side", "Painel", "table-columns", "Mostrar/ocultar painel de nós ao lado do texto"),
          tbtn("dt_props", "Propriedades", "sliders"),
          div(class = "opts",
              selectInput("doc_hl", "Destacar", c("Todos os nós" = "all", "Nó selecionado" = "sel", "Nenhum" = "none"), selectize = FALSE),
              selectInput("doc_coder", "Codificação de", c("Toda a equipe" = "all", setNames(as.character(us$id), us$name)), selectize = FALSE),
              selectInput("doc_stripes", "Faixas", c("Por nó" = "codes", "Por codificador" = "users", "Ocultar" = "none"), selectize = FALSE),
              checkboxInput("doc_cases", "Casos", FALSE))),
      div(class = "dbody",
          div(class = "doc-scroll", uiOutput("doc_html")),
          uiOutput("doc_side")),
      uiOutput("doc_annlist"),
      div(class = "quickcode",
          tags$span("Lista"),
          div(class = "qc-in", selectInput("qc_in", NULL, c("Nós" = "code", "Casos" = "case"), kind_qc, selectize = FALSE)),
          tags$span("Codificar em"),
          div(class = "qc-node", selectizeInput("qc_node", NULL, choices = escolhas_nodes(kind_qc),
                         selected = S$cur_node() %||% character(0),
                         options = SELECTIZE_CRIAR("Digite o nome do nó (Ctrl+Q)"))),
          tags$button(id = "qc_go", type = "button", class = "qcb action-button", title = "Codificar", icon("circle-plus")),
          tags$button(id = "qc_uncode", type = "button", class = "qcb action-button", title = "Descodificar", icon("circle-minus")),
          tags$button(id = "qc_new", type = "button", class = "qcb action-button", title = "Novo nó com a seleção", icon("plus")),
          tags$span(id = "sel-info", class = "selinfo", textOutput("doc_stats", inline = TRUE)))
    )
  }

  S$side_on <- reactiveVal(FALSE)
  observeEvent(input$dt_side, S$side_on(!S$side_on()))
  output$doc_side <- renderUI({
    req(identical(S$detail()$type, "doc"), S$side_on())
    div(class = "doc-side",
        tags$h6("NÓS"),
        div(class = "hint", "Arraste a seleção até um nó ou dê duplo clique nele."),
        uiOutput("doc_tree_codes"),
        div(class = "root-drop", `data-kind` = "code", "solte um nó aqui para movê-lo à raiz"),
        tags$h6("CASOS"), uiOutput("doc_tree_cases"))
  })

  # anotações do arquivo aberto
  S$doc_anns <- reactive({
    d <- S$detail(); req(identical(d$type, "doc"))
    a <- S$anns(); a <- a[a$doc_id == d$id, ]
    a <- a[order(a$pos_start, a$id), ]
    a$num <- seq_len(nrow(a))
    a
  })
  output$doc_annlist <- renderUI({
    a <- S$doc_anns(); if (!nrow(a)) return(NULL)
    us <- S$users(); tx <- S$doc_texto()
    div(class = "annlist", div(class = "ah", sprintf("ANOTAÇÕES (%d)", nrow(a))),
        lapply(seq_len(nrow(a)), function(i) div(class = "ai",
          tags$b(a$num[i]),
          div(style = "flex:1", a$body[i],
              div(class = "meta", sprintf("\u201c%s\u201d \u2014 %s, %s", trunca(trecho(tx, a$pos_start[i], a$pos_end[i]), 60),
                                          us$name[match(a$user_id[i], us$id)], fmt_data(a$created[i])))),
          tags$a(`data-goto` = "1", `data-doc` = a$doc_id[i], `data-start` = a$pos_start[i], `data-end` = a$pos_end[i], "ir"),
          tags$a(onclick = sprintf("QualiR.send('ann_edit',{id:%d})", a$id[i]), "editar"),
          tags$a(onclick = sprintf("QualiR.send('ann_del',{id:%d})", a$id[i]), style = "color:#C0392B", "excluir"))))
  })

  # gráfico de codificação do arquivo
  observeEvent(input$dt_chart, {
    showModal(modalDialog(title = "Gráfico de codificação do arquivo", size = "l", easyClose = TRUE,
                          plotOutput("doc_chart", height = "420px"), footer = modalButton("Fechar")))
  })
  output$doc_chart <- renderPlot({
    cc <- S$doc_cc(); tx <- S$doc_texto(); req(!is.null(cc))
    L <- max(nchar(tx), 1)
    cc <- cc[cc$node_id %in% S$nodes()$id[S$nodes()$kind == "code"], ]
    if (!nrow(cc)) { plot.new(); text(0.5, 0.5, "Este arquivo ainda não tem codificação.", col = "#6B7280"); return() }
    cob <- vapply(split(cc, cc$node_id), function(g) {
      v <- logical(L); for (i in seq_len(nrow(g))) v[(g$pos_start[i] + 1):g$pos_end[i]] <- TRUE; 100 * sum(v) / L }, 0)
    nd <- S$nodes(); nm <- nd$name[match(as.integer(names(cob)), nd$id)]; cor <- nd$color[match(as.integer(names(cob)), nd$id)]
    o <- order(cob); op <- par(mar = c(4, 12, 1, 2)); on.exit(par(op))
    b <- barplot(cob[o], names.arg = trunca(nm[o], 28), horiz = TRUE, las = 1, col = cor[o], border = NA,
                 xlab = "Cobertura do arquivo (%)", cex.names = 0.85, xlim = c(0, max(cob) * 1.15))
    text(cob[o], b, sprintf(" %.1f%%", cob[o]), adj = 0, cex = 0.8)
  })
  observeEvent(input$dt_query, {
    d <- S$detail(); req(identical(d$type, "doc"))
    S$abrir_q("busca", list(docs = as.character(d$id)))
  })
  observeEvent(input$dt_props, { d <- S$detail(); req(identical(d$type, "doc")); S$modal_doc_prop(d$id) })

  S$doc_texto <- reactive({
    d <- S$detail(); req(identical(d$type, "doc"))
    db_doc_texto(con, d$id) %||% ""
  })
  S$doc_meta <- reactive({
    d <- S$detail(); req(identical(d$type, "doc")); dd <- S$docs(); dd[dd$id == d$id, ]
  })

  # codificações do documento aberto (valor estável: só invalida se mudar)
  S$doc_cc <- reactiveVal(NULL)
  observe({
    d <- S$detail()
    if (!identical(d$type, "doc")) { S$doc_cc(NULL); return() }
    cc <- S$cods(); nd <- S$nodes(); us <- S$users(); pt <- S$paths()
    cc <- cc[cc$doc_id == d$id, ]
    if (!isTRUE(input$doc_cases)) cc <- cc[cc$node_id %in% nd$id[nd$kind == "code"], ]
    cod <- input$doc_coder %||% "all"
    if (cod != "all") cc <- cc[cc$user_id == as.integer(cod), ]
    cc <- cc[order(cc$pos_start, cc$id), ]
    cc$color <- nd$color[match(cc$node_id, nd$id)]
    cc$color[is.na(cc$color)] <- "#999999"
    cc$ucolor <- us$color[match(cc$user_id, us$id)]
    cc$uname <- us$name[match(cc$user_id, us$id)]
    cc$label <- if (nrow(cc)) paste0(unname(pt[as.character(cc$node_id)]), " — ", cc$uname) else character()
    S$doc_cc(cc)
  })

  output$doc_html <- renderUI({
    tx <- S$doc_texto(); meta <- S$doc_meta(); cc <- S$doc_cc(); req(!is.null(cc))
    hl <- input$doc_hl %||% "all"
    dest <- switch(hl, none = FALSE, all = TRUE, sel = cc$id[cc$node_id %in% (S$cur_node() %||% -1)])
    modo <- input$doc_stripes %||% "codes"
    faixas <- data.frame(key = integer(), label = character(), color = character())
    col <- "node_id"
    if (modo == "codes" && nrow(cc)) {
      tb <- sort(table(cc$node_id), decreasing = TRUE)
      keys <- as.integer(names(tb))[seq_len(min(20, length(tb)))]
      keys <- keys[order(match(keys, cc$node_id))]
      faixas <- data.frame(key = keys, label = S$nodes()$name[match(keys, S$nodes()$id)],
                           color = cc$color[match(keys, cc$node_id)])
    } else if (modo == "users" && nrow(cc)) {
      keys <- unique(cc$user_id); col <- "user_id"
      faixas <- data.frame(key = keys, label = cc$uname[match(keys, cc$user_id)], color = cc$ucolor[match(keys, cc$user_id)])
    }
    HTML(render_documento(tx, cc, faixas, faixa_col = col, destacar = dest,
                          transcricao = isTRUE(meta$is_transcript == 1), anotacoes = S$doc_anns()))
  })

  output$doc_stats <- renderText({
    cc <- S$doc_cc(); tx <- S$doc_texto(); req(!is.null(cc))
    sprintf("%s palavras · %d referências · %d nós", format(n_palavras(tx), big.mark = ".", decimal.mark = ","),
            nrow(cc), length(unique(cc$node_id)))
  })

  output$doc_tree_codes <- renderUI({
    req(identical(S$detail()$type, "doc"))
    HTML(render_arvore(S$nodes(), contagens_nodes(S$nodes(), S$cods()), "code"))
  })
  output$doc_tree_cases <- renderUI({
    req(identical(S$detail()$type, "doc"))
    HTML(render_arvore(S$nodes(), contagens_nodes(S$nodes(), S$cods()), "case"))
  })

  # atualiza opções da barra de codificação quando os nós mudam
  observe({
    ch <- escolhas_nodes(input$qc_in %||% "code")
    if (identical(isolate(S$detail()$type), "doc"))
      updateSelectizeInput(session, "qc_node", choices = ch, selected = isolate(input$qc_node))
  })

  # escolher um nó na barra também o torna o "nó selecionado"
  observeEvent(input$qc_node, {
    v <- input$qc_node
    if (length(v) == 1 && grepl("^\\d+$", v)) S$cur_node(as.integer(v))
  }, ignoreInit = TRUE)

  # clicar em faixa: destaca apenas aquele nó
  observeEvent(input$stripe_click, {
    if ((input$doc_stripes %||% "codes") != "codes") return()
    S$set_node(as.integer(input$stripe_click$key))
    updateSelectInput(session, "doc_hl", selected = "sel")
  })

  # ir para uma referência no documento
  observeEvent(input$goto_ref, {
    g <- input$goto_ref
    S$detail(list(type = "doc", id = as.integer(g$doc)))
    session$onFlushed(function() session$sendCustomMessage("qr_scroll", list(start = g$start, end = g$end)), once = TRUE)
  })

  # ===================== NÓ / CASO ======================================
  ui_node <- function(id) {
    nd <- S$nodes(); n <- nd[nd$id == id, ]
    if (!nrow(n)) return(div(class = "detail-empty", "Nó não encontrado."))
    us <- S$users()
    tagList(
      cabecalho(if (n$kind == "case") "briefcase" else "circle", uiOutput("node_title", inline = TRUE),
                extra = tagList(
                  actionLink("node_edit", tagList(icon("pen"), " Propriedades"), style = "font-size:12px"),
                  actionLink("node_memo", tagList(icon("note-sticky"), " Nota"), style = "font-size:12px"))),
      div(class = "dtoolbar",
          checkboxInput("node_agg", "Incluir nós filhos (agregar)", FALSE),
          selectInput("node_coder", "Codificação de", c("Toda a equipe" = "all", setNames(as.character(us$id), us$name)), selectize = FALSE),
          tags$span(style = "margin-left:auto;color:#6B7280", textOutput("node_stats", inline = TRUE))),
      div(class = "dscroll", uiOutput("node_desc"), uiOutput("node_refs"))
    )
  }

  S$node_cur <- reactive({
    d <- S$detail(); req(identical(d$type, "node"))
    nd <- S$nodes(); n <- nd[nd$id == d$id, ]; req(nrow(n)); n
  })
  output$node_title <- renderUI({
    n <- S$node_cur()
    tagList(tags$span(class = "dot", style = sprintf("background:%s", n$color %||% "#999")),
            tags$span(S$paths()[as.character(n$id)]))
  })
  output$node_desc <- renderUI({
    n <- S$node_cur(); at <- S$attrs(); at <- at[at$node_id == n$id, ]
    tagList(
      if (nzchar(n$description %||% "")) div(style = "color:#555;margin-bottom:10px;font-style:italic", n$description),
      if (nrow(at)) div(style = "margin-bottom:12px",
        lapply(seq_len(nrow(at)), function(i) tags$span(class = "badge bg-light text-dark border", style = "margin-right:6px",
                                                         paste0(at$attr[i], ": ", at$value[i])))))
  })
  S$node_refs_df <- reactive({
    n <- S$node_cur(); nd <- S$nodes(); cc <- S$cods()
    ids <- if (isTRUE(input$node_agg)) descendentes(nd, n$id) else n$id
    cc <- cc[cc$node_id %in% ids, ]
    cod <- input$node_coder %||% "all"
    if (cod != "all") cc <- cc[cc$user_id == as.integer(cod), ]
    cc[order(cc$doc_id, cc$pos_start), ]
  })
  output$node_stats <- renderText({
    cc <- S$node_refs_df()
    sprintf("%d referências em %d arquivos", nrow(cc), length(unique(cc$doc_id)))
  })
  output$node_refs <- renderUI({
    cc <- S$node_refs_df(); dd <- S$docs(); us <- S$users(); nd <- S$nodes(); pt <- S$paths()
    if (!nrow(cc)) return(div(class = "detail-empty", icon("highlighter"),
                              "Nenhuma referência codificada ainda.",
                              tags$small("Abra um arquivo, selecione um trecho e arraste-o até este nó.")))
    textos <- DBI::dbGetQuery(con, sprintf("SELECT id, content FROM documents WHERE id IN (%s)",
                                           paste(unique(cc$doc_id), collapse = ",")))
    lapply(split(cc, cc$doc_id), function(g) {
      did <- g$doc_id[1]; tx <- textos$content[textos$id == did]; L <- nchar(tx)
      cob <- logical(L); for (i in seq_len(nrow(g))) cob[(g$pos_start[i] + 1):g$pos_end[i]] <- TRUE
      div(class = "refgroup",
        div(class = "rg-head", `data-goto` = "1", `data-doc` = did, `data-start` = g$pos_start[1], `data-end` = g$pos_end[1],
            icon("file-lines"), sprintf(" Arquivos\\%s", dd$name[dd$id == did]),
            tags$small(sprintf(" — %d referência(s) codificada(s) [%.2f%% de cobertura]", nrow(g), 100 * sum(cob) / max(L, 1)))),
        lapply(seq_len(nrow(g)), function(i) {
          r <- g[i, ]
          div(class = "ref", style = sprintf("border-left-color:%s", nd$color[nd$id == r$node_id] %||% "#ccc"),
            div(class = "rmeta",
                tags$span(sprintf("Referência %d — %.2f%%", i, 100 * (r$pos_end - r$pos_start) / max(L, 1))),
                if (isTRUE(input$node_agg) && r$node_id != S$node_cur()$id) tags$span(pt[as.character(r$node_id)]),
                tags$span(icon("user"), us$name[us$id == r$user_id]),
                tags$span(fmt_data(r$created)),
                tags$a(`data-goto` = "1", `data-doc` = did, `data-start` = r$pos_start, `data-end` = r$pos_end,
                       icon("arrow-up-right-from-square"), "Abrir no arquivo")),
            div(class = "rtext", trecho(tx, r$pos_start, r$pos_end)))
        }))
    })
  })

  observeEvent(input$node_edit, S$modal_node("edit", id = S$node_cur()$id))
  observeEvent(input$node_memo, S$abrir_memo_vinculada("node", S$node_cur()$id, S$node_cur()$name))
  observeEvent(input$doc_memo, {
    d <- S$doc_meta(); S$abrir_memo_vinculada("doc", d$id, d$name)
  })

  # ===================== NOTAS ==============================================
  S$abrir_memo_vinculada <- function(tipo, id, nome) {
    mm <- isolate(S$memos())
    ex <- mm[!is.na(mm$link_type) & mm$link_type == tipo & !is.na(mm$link_id) & mm$link_id == id, ]
    if (nrow(ex)) { S$detail(list(type = "memo", id = ex$id[1])); return() }
    if (!S$pode_editar()) return()
    mid <- db_criar_memo(con, paste("Nota —", nome), "", tipo, id, S$me())
    S$log("criou nota", nome)
    S$detail(list(type = "memo", id = mid))
  }

  S$memo_aberta <- reactiveVal(NULL)
  ui_memo <- function(id) {
    m <- db_memos(con); m <- m[m$id == id, ]
    if (!nrow(m)) return(div(class = "detail-empty", "Nota não encontrada."))
    S$memo_aberta(list(id = id, modified = m$modified))
    us <- S$users()
    tagList(
      cabecalho("note-sticky", m$title,
                extra = tags$small(style = "color:#6B7280", sprintf("por %s · modificada %s",
                                   us$name[match(m$user_id, us$id)], fmt_data(m$modified)))),
      div(class = "dscroll memo-edit",
          textInput("memo_title", "Título", m$title, width = "100%"),
          textAreaInput("memo_body", NULL, m$body %||% "", width = "100%", rows = 18,
                        placeholder = "Escreva aqui suas reflexões analíticas, decisões de codificação, ideias…"),
          actionButton("memo_save", "Salvar nota", icon = icon("floppy-disk"), class = "btn-primary btn-sm"),
          actionButton("memo_del", "Excluir", icon = icon("trash-can"), class = "btn-outline-danger btn-sm"))
    )
  }
  salvar_memo <- function(forcar = FALSE) {
    ma <- S$memo_aberta(); req(ma)
    if (!S$pode_editar()) return()
    atual <- DBI::dbGetQuery(con, "SELECT m.modified, u.name FROM memos m LEFT JOIN users u ON u.id = m.user_id WHERE m.id=?",
                             params = list(ma$id))
    if (!forcar && nrow(atual) && !identical(atual$modified, ma$modified)) {
      showModal(modalDialog(title = "Nota alterada por outra pessoa",
        "Esta nota foi modificada por outro membro da equipe depois que você a abriu. Deseja sobrescrever?",
        footer = tagList(modalButton("Cancelar"), actionButton("memo_force", "Sobrescrever", class = "btn-danger"))))
      return()
    }
    db_salvar_memo(con, ma$id, input$memo_title, input$memo_body)
    novo <- DBI::dbGetQuery(con, "SELECT modified FROM memos WHERE id=?", params = list(ma$id))$modified
    S$memo_aberta(list(id = ma$id, modified = novo))
    S$log("editou nota", input$memo_title)
    showNotification("Nota salva.", duration = 2)
  }
  observeEvent(input$memo_save, salvar_memo(FALSE))
  observeEvent(input$memo_force, { removeModal(); salvar_memo(TRUE) })
  observeEvent(input$memo_del, {
    showModal(modalDialog(title = "Excluir nota", "Excluir esta nota permanentemente?",
      footer = tagList(modalButton("Cancelar"), actionButton("memo_del_ok", "Excluir", class = "btn-danger"))))
  })
  observeEvent(input$memo_del_ok, {
    ma <- S$memo_aberta(); req(ma); if (!S$pode_editar()) return()
    db_excluir_memo(con, ma$id); S$log("excluiu nota", ""); removeModal(); S$detail(list(type = "none"))
  })
}
