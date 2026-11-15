# ---------------------------------------------------------------------------
# QualiR - anotações, relacionamentos, conjuntos, matrizes salvas, mapas e relatórios
# ---------------------------------------------------------------------------

server_extras <- function(input, output, session, S) {
  con <- S$con
  cab <- function(ic, titulo, ...) div(class = "dhead", div(class = "dtitle", icon(ic), tags$span(titulo)), ...,
                                      actionLink("detail_close", icon("xmark"), title = "Fechar"))

  # ===================== ANOTAÇÕES ==========================================
  modal_anotacao <- function(sel) {
    d <- isolate(S$detail())
    if (!identical(d$type, "doc")) { showNotification("Abra um arquivo para anotar.", type = "warning"); return() }
    if (is.null(sel)) { showNotification("Selecione o trecho a ser anotado.", type = "warning"); return() }
    if (!S$pode_editar()) return()
    se <- aparar_selecao(isolate(S$doc_texto()), sel[1], sel[2]); req(se)
    S$ann_ctx <- list(doc = d$id, s = se[1], e = se[2], id = NULL)
    showModal(modalDialog(title = tagList(icon("comment-dots"), " Nova anotação"), size = "m",
      div(class = "ref", div(class = "rtext", trunca(trecho(isolate(S$doc_texto()), se[1], se[2]), 400))),
      textAreaInput("ann_body", NULL, "", rows = 5, width = "100%", placeholder = "Escreva a anotação sobre este trecho…"),
      footer = tagList(modalButton("Cancelar"), actionButton("ann_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$dt_annot, modal_anotacao(S$doc_sel()))
  observeEvent(input$rb_nova_anotacao, modal_anotacao(S$doc_sel()))
  observeEvent(input$annot_sel, modal_anotacao(c(input$annot_sel$start, input$annot_sel$end)))
  S$modal_anotacao_edit <- function(id) {
    a <- isolate(S$anns()); a <- a[a$id == id, ]; req(nrow(a))
    if (!S$pode_editar()) return()
    S$ann_ctx <- list(id = id)
    showModal(modalDialog(title = "Editar anotação", size = "m",
      textAreaInput("ann_body", NULL, a$body, rows = 5, width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("ann_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$ann_edit, S$modal_anotacao_edit(as.integer(input$ann_edit$id)))
  observeEvent(input$ann_ok, {
    x <- S$ann_ctx; b <- trimws(input$ann_body %||% "")
    if (!nzchar(b)) { showNotification("Escreva a anotação.", type = "warning"); return() }
    if (is.null(x$id)) { db_criar_anotacao(con, x$doc, x$s, x$e, b, S$me()); S$log("criou anotação", trunca(b, 50)) }
    else { db_editar_anotacao(con, x$id, b); S$log("editou anotação", trunca(b, 50)) }
    removeModal()
  })
  observeEvent(input$ann_del, {
    if (!S$pode_editar()) return()
    a <- S$anns(); a <- a[a$id == as.integer(input$ann_del$id), ]; req(nrow(a))
    if (!(S$eh_admin() || a$user_id == S$me())) { showNotification("Você só pode excluir suas anotações.", type = "error"); return() }
    db_excluir_anotacao(con, a$id); S$log("excluiu anotação", "")
  })

  # ===================== RELACIONAMENTOS =====================================
  dirs <- c("Associativa (—)" = "associativa", "Unidirecional (→)" = "unidirecional", "Simétrica (↔)" = "simetrica")
  S$modal_reltipo <- function(id = NULL) {
    if (!S$pode_editar()) return()
    rt <- isolate(S$reltypes()); r <- if (!is.null(id)) rt[rt$id == id, ] else NULL
    S$reltipo_ctx <- id
    showModal(modalDialog(title = if (is.null(id)) "Novo tipo de relacionamento" else "Tipo de relacionamento", size = "s",
      textInput("rt_name", "Nome", if (!is.null(r)) r$name else "", width = "100%", placeholder = "ex.: Influencia, Contradiz, Faz parte de"),
      radioButtons("rt_dir", "Direção", dirs, if (!is.null(r)) r$direction else "associativa"),
      footer = tagList(modalButton("Cancelar"), actionButton("rt_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$rb_novo_reltipo, S$modal_reltipo())
  observeEvent(input$rb_novo_reltipo_l, S$modal_reltipo())
  observeEvent(input$rt_ok, {
    nm <- trimws(input$rt_name %||% ""); req(nzchar(nm))
    if (is.null(S$reltipo_ctx)) db_criar_rel_tipo(con, nm, input$rt_dir, S$me()) else db_editar_rel_tipo(con, S$reltipo_ctx, nm, input$rt_dir)
    S$log("salvou tipo de relacionamento", nm); removeModal(); S$ir_secao("reltypes")
  })
  S$modal_rel <- function(id = NULL) {
    if (!S$pode_editar()) return()
    r <- if (!is.null(id)) { x <- isolate(S$rels()); x[x$id == id, ] } else NULL
    ch <- isolate(S$escolhas_nodes()); rt <- isolate(S$reltypes())
    S$rel_ctx <- id
    cur <- isolate(S$cur_node())
    showModal(modalDialog(title = if (is.null(id)) "Novo relacionamento" else "Relacionamento", size = "m",
      selectizeInput("rl_from", "De", ch, selected = if (!is.null(r)) r$from_id else cur, width = "100%"),
      selectInput("rl_type", "Tipo", setNames(rt$id, sprintf("%s (%s)", rt$name, rt$direction)),
                  selected = if (!is.null(r)) r$type_id else NULL, width = "100%", selectize = FALSE),
      selectizeInput("rl_to", "Para", ch, selected = if (!is.null(r)) r$to_id else character(0), width = "100%"),
      textAreaInput("rl_desc", "Descrição / evidência", if (!is.null(r)) r$description %||% "" else "", rows = 3, width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("rl_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$rb_novo_rel, S$modal_rel())
  observeEvent(input$rb_novo_rel_l, S$modal_rel())
  observeEvent(input$rl_ok, {
    a <- as.integer(input$rl_from); b <- as.integer(input$rl_to)
    if (!length(a) || !length(b) || is.na(a) || is.na(b) || a == b) { showNotification("Escolha dois itens diferentes.", type = "warning"); return() }
    if (is.null(S$rel_ctx)) db_criar_relacionamento(con, a, b, as.integer(input$rl_type), input$rl_desc %||% "", S$me())
    else db_editar_relacionamento(con, S$rel_ctx, a, b, as.integer(input$rl_type), input$rl_desc %||% "")
    S$log("salvou relacionamento", ""); removeModal(); S$ir_secao("rels")
  })

  # ===================== CONJUNTOS ===========================================
  S$modal_conjunto <- function(id = NULL) {
    if (!S$pode_editar()) return()
    st <- isolate(S$sets()); x <- if (!is.null(id)) st[st$id == id, ] else NULL
    S$set_ctx <- id
    showModal(modalDialog(title = if (is.null(id)) "Novo conjunto" else "Propriedades do conjunto", size = "s",
      textInput("st_name", "Nome", if (!is.null(x)) x$name else "", width = "100%", placeholder = "ex.: Entrevistas de 2025"),
      textAreaInput("st_desc", "Descrição", if (!is.null(x)) x$description %||% "" else "", rows = 2, width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("st_ok", "Salvar", class = "btn-primary"))))
  }
  observeEvent(input$rb_novo_conjunto, S$modal_conjunto())
  observeEvent(input$rb_novo_conjunto_l, S$modal_conjunto())
  observeEvent(input$st_ok, {
    nm <- trimws(input$st_name %||% ""); req(nzchar(nm))
    if (is.null(S$set_ctx)) db_criar_conjunto(con, nm, input$st_desc %||% "", S$me()) else db_editar_conjunto(con, S$set_ctx, nm, input$st_desc %||% "")
    S$log("salvou conjunto", nm); removeModal(); S$ir_secao("sets")
  })
  observeEvent(input$rb_add_conjunto, {
    it <- S$sel_item(); d <- S$detail()
    alvo <- if (!is.null(it) && it$type %in% c("doc", "node", "case")) list(type = if (it$type == "doc") "doc" else "node", id = as.integer(it$id))
            else if (identical(d$type, "doc") || identical(d$type, "node")) list(type = d$type, id = as.integer(d$id)) else NULL
    if (is.null(alvo)) { showNotification("Selecione um arquivo, nó ou caso na lista.", type = "warning"); return() }
    if (!S$pode_editar()) return()
    S$set_add <- alvo
    st <- S$sets()
    showModal(modalDialog(title = "Adicionar ao conjunto", size = "s",
      selectizeInput("sa_set", "Conjunto", setNames(as.character(st$id), st$name), selected = character(0), width = "100%",
                     options = list(create = TRUE, placeholder = "Escolha ou digite um novo conjunto")),
      footer = tagList(modalButton("Cancelar"), actionButton("sa_ok", "Adicionar", class = "btn-primary"))))
  })
  observeEvent(input$sa_ok, {
    v <- input$sa_set %||% ""; req(nzchar(v))
    sid <- if (grepl("^\\d+$", v)) as.integer(v) else db_criar_conjunto(con, trimws(v), "", S$me())
    db_add_conjunto(con, sid, S$set_add$type, S$set_add$id); removeModal()
    showNotification("Item adicionado ao conjunto.")
  })
  S$ui_set <- function(id) {
    st <- S$sets(); x <- st[st$id == id, ]
    if (!nrow(x)) return(div(class = "detail-empty", "Conjunto não encontrado."))
    tagList(cab("circle-half-stroke", x$name, actionLink("set_props", tagList(icon("sliders"), " Propriedades"), style = "font-size:12px")),
            div(class = "dscroll", if (nzchar(x$description %||% "")) p(style = "color:#555;font-style:italic", x$description),
                uiOutput("set_members")))
  }
  observeEvent(input$set_props, S$modal_conjunto(as.integer(S$detail()$id)))
  output$set_members <- renderUI({
    d <- S$detail(); req(identical(d$type, "set"))
    si <- S$set_items(); si <- si[si$set_id == d$id, ]
    if (!nrow(si)) return(div(class = "detail-empty", icon("circle-half-stroke"), "Conjunto vazio.",
                              tags$small("Selecione um arquivo, nó ou caso na lista e use Início > Adicionar ao conjunto.")))
    dd <- S$docs(); pt <- S$paths()
    nome <- ifelse(si$item_type == "doc", dd$name[match(si$item_id, dd$id)], pt[as.character(si$item_id)])
    tags$table(class = "qlist", tags$thead(tags$tr(tags$th("Membro"), tags$th("Tipo"), tags$th(""))),
      tags$tbody(lapply(seq_len(nrow(si)), function(i) tags$tr(class = "qrow", `data-type` = si$item_type[i], `data-id` = si$item_id[i],
        tags$td(div(class = "namecell", icon(if (si$item_type[i] == "doc") "file-lines" else "circle"), nome[i])),
        tags$td(if (si$item_type[i] == "doc") "Arquivo" else "Nó/caso"),
        tags$td(tags$a(onclick = sprintf("QualiR.send('set_rm',{t:'%s',id:%d})", si$item_type[i], si$item_id[i]),
                       style = "cursor:pointer;color:#C0392B", "remover"))))))
  })
  observeEvent(input$set_rm, {
    if (!S$pode_editar()) return()
    db_rm_conjunto(con, S$detail()$id, input$set_rm$t, input$set_rm$id)
  })

  # ===================== MATRIZES SALVAS =====================================
  S$ui_matrix <- function(id) {
    m <- db_matriz(con, id)
    if (!nrow(m)) return(div(class = "detail-empty", "Matriz não encontrada."))
    tagList(cab("table-cells", m$name,
                actionLink("mxs_rerun", tagList(icon("play"), " Executar novamente"), style = "font-size:12px"),
                downloadLink("mxs_export", tagList(icon("file-excel"), " Exportar (Excel)"), style = "font-size:12px")),
            div(class = "dscroll", tags$small(style = "color:#6B7280", sprintf("Salva em %s.", fmt_data(m$created))),
                DT::DTOutput("mxs_dt")))
  }
  mxs_df <- reactive({
    d <- S$detail(); req(identical(d$type, "matrix"))
    m <- db_matriz(con, d$id); req(nrow(m))
    as.data.frame(jsonlite::fromJSON(m$data), check.names = FALSE)
  })
  output$mxs_dt <- DT::renderDT({
    df <- mxs_df(); num <- names(df)[-1]
    mx <- max(c(1, unlist(df[num])), na.rm = TRUE)
    brks <- unique(round(seq(0, mx, length.out = 6)[-1] - 0.5, 1))
    cores <- colorRampPalette(c("#FFFFFF", "#B8D0EC", "#1F5FA8"))(length(brks) + 1)
    dt <- DT::datatable(df, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 500, ordering = FALSE, scrollX = TRUE))
    if (length(num)) dt <- DT::formatStyle(dt, num, backgroundColor = DT::styleInterval(brks, cores),
                                           color = DT::styleInterval(mx * 0.6, c("#222", "#fff")), textAlign = "center")
    dt
  })
  output$mxs_export <- downloadHandler(filename = function() "matriz.xlsx", content = function(file) {
    wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Matriz"); openxlsx::writeData(wb, 1, mxs_df())
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  })
  observeEvent(input$mxs_rerun, {
    m <- db_matriz(con, S$detail()$id); req(nrow(m))
    S$abrir_q("matriz", jsonlite::fromJSON(m$params %||% "{}"), TRUE, m$name)
  })

  # ===================== MAPAS E GRÁFICOS ====================================
  S$ui_map <- function(id) {
    opts <- switch(id,
      hier = tagList(radioButtons("mp_val", NULL, c("Referências" = "refs", "Arquivos" = "docs"), inline = TRUE)),
      proj = tagList(checkboxInput("mp_cases", "Incluir casos", TRUE), checkboxInput("mp_docs", "Incluir arquivos", TRUE)),
      cooc = numericInput("mp_min", "Mínimo de trechos em comum", 1, 1, 50, width = "160px"),
      NULL)
    tagList(cab("diagram-project", MAPAS[[id]], downloadLink("mp_png", tagList(icon("image"), " Salvar imagem (PNG)"), style = "font-size:12px")),
            div(class = "dtoolbar", opts, tags$small(style = "color:#6B7280", MAPAS_DESC[[id]])),
            div(class = "dscroll", plotOutput("mp_plot", height = "auto")))
  }
  desenhar_mapa <- function(id) {
    nd <- S$nodes(); cc <- S$cods(); dd <- S$docs()
    switch(id,
      hier = {
        cont <- contagens_nodes(nd, cc, agregar = TRUE)
        v <- setNames(if ((input$mp_val %||% "refs") == "docs") cont$n_docs else cont$n_refs, cont$node_id)
        desenhar_treemap(nd, v)
      },
      cooc = {
        ids <- nd$id[nd$kind == "code"]; m <- coocorrencia(cc, ids); m[m < (input$mp_min %||% 1)] <- 0
        rede(m, nd, "Nenhuma coocorrência encontrada")
      },
      rel = {
        r <- S$rels(); rt <- S$reltypes()
        if (!nrow(r)) { plot.new(); text(0.5, 0.5, "Nenhum relacionamento. Use Criar > Relacionamento.", col = "#6B7280"); return() }
        vs <- unique(c(r$from_id, r$to_id))
        g <- igraph::graph_from_data_frame(data.frame(from = as.character(r$from_id), to = as.character(r$to_id)),
                                           directed = TRUE, vertices = data.frame(name = as.character(vs)))
        dir <- rt$direction[match(r$type_id, rt$id)]
        lab <- rt$name[match(r$type_id, rt$id)]
        par(mar = c(0, 0, 0, 0)); set.seed(7)
        plot(g, layout = igraph::layout_with_fr(g), vertex.label = trunca(nd$name[match(vs, nd$id)], 26),
             vertex.color = nd$color[match(vs, nd$id)], vertex.size = 18, vertex.label.color = "#1E1E1E",
             vertex.label.family = "sans", vertex.frame.color = "white", vertex.shape = ifelse(nd$kind[match(vs, nd$id)] == "case", "square", "circle"),
             edge.arrow.mode = ifelse(dir %in% "unidirecional", 2, ifelse(dir %in% "simetrica", 3, 0)), edge.arrow.size = 0.6,
             edge.label = lab, edge.label.cex = 0.85, edge.label.color = "#1F5FA8", edge.color = "#7C9CC4", edge.width = 2,
             vertex.label.dist = 2.2, margin = 0.1)
      },
      proj = {
        cod <- nd[nd$kind == "code", ]; cas <- nd[nd$kind == "case", ]
        vert <- data.frame(name = paste0("n", cod$id), lab = cod$name, cor = cod$color, shape = "circle", size = 14)
        arest <- data.frame(from = character(), to = character(), w = numeric())
        if (isTRUE(input$mp_docs %||% TRUE) && nrow(dd)) {
          vert <- rbind(vert, data.frame(name = paste0("d", dd$id), lab = dd$name, cor = "#BFC5CC", shape = "square", size = 12))
          x <- cc[cc$node_id %in% cod$id, ]
          if (nrow(x)) { a <- aggregate(id ~ doc_id + node_id, x, length)
            arest <- rbind(arest, data.frame(from = paste0("d", a$doc_id), to = paste0("n", a$node_id), w = a$id)) }
        }
        if (isTRUE(input$mp_cases %||% TRUE) && nrow(cas)) {
          vert <- rbind(vert, data.frame(name = paste0("n", cas$id), lab = cas$name, cor = "#F3C5A8", shape = "square", size = 12))
          ov <- sobreposicoes(cc[cc$node_id %in% cod$id, ], cc[cc$node_id %in% cas$id, ])
          if (nrow(ov)) { a <- aggregate(id.a ~ node_id.a + node_id.b, ov, function(z) length(unique(z)))
            arest <- rbind(arest, data.frame(from = paste0("n", a$node_id.b), to = paste0("n", a$node_id.a), w = a$id.a)) }
        }
        if (!nrow(vert)) { plot.new(); text(0.5, 0.5, "Projeto vazio", col = "#6B7280"); return() }
        g <- igraph::graph_from_data_frame(arest, directed = FALSE, vertices = vert)
        par(mar = c(0, 0, 0, 0)); set.seed(3)
        w <- igraph::E(g)$w
        plot(g, layout = igraph::layout_with_fr(g), vertex.label = trunca(igraph::V(g)$lab, 22), vertex.color = igraph::V(g)$cor,
             vertex.shape = igraph::V(g)$shape, vertex.size = igraph::V(g)$size, vertex.label.color = "#1E1E1E",
             vertex.label.family = "sans", vertex.label.cex = 0.8, vertex.frame.color = "white", vertex.label.dist = 1.8,
             edge.width = if (length(w)) 0.5 + 3 * w / max(w) else 1, edge.color = adjustcolor("#1F5FA8", 0.35), margin = 0.05)
        legend("bottomleft", c("Nó", "Arquivo", "Caso"), pch = c(21, 22, 22), pt.bg = c("#1F5FA8", "#BFC5CC", "#F3C5A8"),
               bty = "n", cex = 0.85)
      },
      docchart = {
        cod <- nd[nd$kind == "code", ]
        if (!nrow(dd) || !nrow(cod)) { plot.new(); text(0.5, 0.5, "Sem dados", col = "#6B7280"); return() }
        x <- cc[cc$node_id %in% cod$id, ]
        m <- t(vapply(dd$id, function(d) vapply(cod$id, function(n) sum(x$doc_id == d & x$node_id == n), 0), numeric(nrow(cod))))
        if (nrow(cod) == 1) m <- matrix(m, ncol = 1)
        par(mar = c(4, 12, 1, 10), xpd = TRUE)
        barplot(t(m), names.arg = trunca(dd$name, 26), horiz = TRUE, las = 1, col = cod$color, border = NA,
                xlab = "Referências", cex.names = 0.85)
        legend("topright", inset = c(-0.28, 0), legend = trunca(cod$name, 22), fill = cod$color, border = NA, bty = "n", cex = 0.8)
      })
  }
  rede <- function(m, nd, vazio) {
    g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE, diag = FALSE)
    vid <- as.integer(igraph::V(g)$name); cont <- contagens_nodes(nd, S$cods())
    refs <- cont$n_refs[match(vid, cont$node_id)]; w <- igraph::E(g)$weight
    par(mar = c(0, 0, 0, 0))
    if (!length(w)) { plot.new(); text(0.5, 0.5, vazio, col = "#6B7280"); return() }
    set.seed(42)
    plot(g, layout = igraph::layout_with_fr(g), vertex.label = trunca(nd$name[match(vid, nd$id)], 24),
         vertex.color = nd$color[match(vid, nd$id)], vertex.size = 8 + 22 * sqrt(refs / max(refs, 1)),
         vertex.label.color = "#1E1E1E", vertex.label.family = "sans", vertex.frame.color = "white", vertex.label.dist = 1.6,
         edge.width = 1 + 5 * w / max(w), edge.color = adjustcolor("#1F5FA8", 0.45), edge.label = w, edge.label.cex = 0.8,
         edge.label.color = "#1F5FA8", margin = 0.12)
  }
  altura_mapa <- function() {
    d <- S$detail(); if (!identical(d$type, "map")) return(400)
    if (d$id == "docchart") max(380, 60 + 34 * nrow(S$docs())) else 560
  }
  output$mp_plot <- renderPlot({ d <- S$detail(); req(identical(d$type, "map")); desenhar_mapa(d$id) }, height = altura_mapa)
  output$mp_png <- downloadHandler(filename = function() paste0("qualir_", S$detail()$id, ".png"), content = function(file) {
    png(file, width = 1600, height = 1100, res = 150); on.exit(dev.off()); desenhar_mapa(S$detail()$id)
  })
  abrir_mapa <- function(id) S$detail(list(type = "map", id = id))
  observeEvent(input$rb_v_hier, abrir_mapa("hier"))
  observeEvent(input$rb_v_mapa, abrir_mapa("proj"))
  observeEvent(input$rb_visualizar, abrir_mapa("proj"))
  observeEvent(input$rb_v_rel, abrir_mapa("rel"))
  observeEvent(input$rb_consulta, S$abrir_q("busca"))
  observeEvent(input$rb_q_comp2, S$abrir_q("comp"))
  observeEvent(input$rb_relatorios, S$ir_secao("reports"))

  # ===================== RELATÓRIOS ==========================================
  dados_relatorio <- function(id) {
    nd <- S$nodes(); cc <- S$cods(); dd <- S$docs(); us <- S$users(); pt <- S$paths()
    nome_u <- function(x) us$name[match(x, us$id)]
    codn <- nd[nd$kind == "code", ]
    switch(id,
      codigos = {
        o <- ordenar_arvore(codn); cont <- contagens_nodes(nd, cc, agregar = FALSE); cont2 <- contagens_nodes(nd, cc, agregar = TRUE)
        list(`Nós` = if (is.null(o)) data.frame() else data.frame(`Nó` = unname(pt[as.character(o$id)]),
             `Descrição` = o$description %||% "", Arquivos = cont$n_docs[match(o$id, cont$node_id)],
             `Referências` = cont$n_refs[match(o$id, cont$node_id)], `Referências (com nós filhos)` = cont2$n_refs[match(o$id, cont2$node_id)],
             Codificadores = vapply(o$id, function(i) length(unique(cc$user_id[cc$node_id == i])), 0L),
             `Criado por` = nome_u(o$created_by), `Criado em` = fmt_data(o$created), check.names = FALSE))
      },
      arquivos = {
        da <- S$dattrs(); x <- cc[cc$node_id %in% codn$id, ]
        cob <- vapply(dd$id, function(d) { g <- x[x$doc_id == d, ]; L <- dd$n_chars[dd$id == d]; if (!nrow(g) || !L) return(0)
          v <- logical(L); for (i in seq_len(nrow(g))) v[(g$pos_start[i] + 1):g$pos_end[i]] <- TRUE; round(100 * sum(v) / L, 1) }, 0)
        df <- data.frame(Arquivo = dd$name, Tipo = ifelse(dd$is_transcript == 1, "Transcrição", dd$type), Caracteres = dd$n_chars,
                         `Nós` = vapply(dd$id, function(d) length(unique(x$node_id[x$doc_id == d])), 0L),
                         `Referências` = vapply(dd$id, function(d) sum(x$doc_id == d), 0L), `Cobertura codificada (%)` = cob,
                         check.names = FALSE)
        for (a in sort(unique(da$attr))) df[[a]] <- da$value[match(paste(dd$id, a), paste(da$doc_id, da$attr))] %||% ""
        list(Arquivos = df)
      },
      casos = {
        x <- S$classif_df()$df; cas <- nd[nd$kind == "case", ]; cas <- cas[order(tolower(cas$name)), ]
        ov <- sobreposicoes(cc[cc$node_id %in% codn$id, ], cc[cc$node_id %in% cas$id, ])
        x$Arquivos <- vapply(cas$id, function(i) length(unique(cc$doc_id[cc$node_id == i])), 0L)
        x$`Falas codificadas` <- vapply(cas$id, function(i) sum(cc$node_id == i), 0L)
        x$`Referências de nós` <- vapply(cas$id, function(i) if (nrow(ov)) length(unique(ov$id.a[ov$node_id.b == i])) else 0L, 0L)
        list(Casos = x)
      },
      cod_arquivo = {
        x <- cc[cc$node_id %in% codn$id, ]
        if (!nrow(x)) return(list(`Codificação` = data.frame(Aviso = "Sem codificação")))
        a <- aggregate(id ~ doc_id + node_id, x, length)
        list(`Codificação` = data.frame(Arquivo = dd$name[match(a$doc_id, dd$id)], `Nó` = unname(pt[as.character(a$node_id)]),
                                        `Referências` = a$id, check.names = FALSE)[order(dd$name[match(a$doc_id, dd$id)], pt[as.character(a$node_id)]), ])
      },
      trechos = {
        t <- tabela_referencias(con, nd, cc[cc$node_id %in% codn$id, ], dd, us)
        list(Trechos = if (nrow(t)) t[order(t$`Nó`, t$Documento, t$`Início`), c("Nó", "Documento", "Codificador", "Trecho")] else data.frame())
      },
      anotacoes = {
        a <- S$anns(); mm <- S$memos()
        tx <- if (nrow(a)) DBI::dbGetQuery(con, "SELECT id, content FROM documents") else NULL
        list(`Anotações` = if (nrow(a)) data.frame(Arquivo = dd$name[match(a$doc_id, dd$id)],
               Trecho = trunca(substr(tx$content[match(a$doc_id, tx$id)], a$pos_start + 1, a$pos_end), 200),
               `Anotação` = a$body, Autor = nome_u(a$user_id), Data = fmt_data(a$created), check.names = FALSE) else data.frame(),
             Memorandos = if (nrow(mm)) data.frame(`Título` = mm$title, Texto = mm$body %||% "", Autor = nome_u(mm$user_id),
               Modificado = fmt_data(mm$modified), check.names = FALSE) else data.frame())
      },
      equipe = {
        x <- cc[cc$node_id %in% codn$id, ]
        if (!nrow(x)) return(list(Equipe = data.frame(Aviso = "Sem codificação")))
        a <- aggregate(id ~ user_id + node_id, x, length)
        list(`Por membro` = data.frame(Membro = nome_u(us$id), `Referências` = vapply(us$id, function(u) sum(x$user_id == u), 0L),
                                       Arquivos = vapply(us$id, function(u) length(unique(x$doc_id[x$user_id == u])), 0L),
                                       `Nós usados` = vapply(us$id, function(u) length(unique(x$node_id[x$user_id == u])), 0L), check.names = FALSE),
             `Membro × nó` = data.frame(Membro = nome_u(a$user_id), `Nó` = unname(pt[as.character(a$node_id)]), `Referências` = a$id, check.names = FALSE))
      })
  }
  S$ui_report <- function(id) {
    tagList(cab("file-lines", RELATORIOS[[id]],
                downloadLink("rp_docx", tagList(icon("file-word"), " Word"), style = "font-size:12px"),
                downloadLink("rp_xlsx", tagList(icon("file-excel"), " Excel"), style = "font-size:12px")),
            div(class = "dscroll report", uiOutput("rp_body")))
  }
  output$rp_body <- renderUI({
    d <- S$detail(); req(identical(d$type, "report"))
    r <- dados_relatorio(d$id)
    tagList(tags$small(style = "color:#6B7280", sprintf("%s — gerado em %s", db_meta(con, "project_name"), format(Sys.time(), "%d/%m/%Y %H:%M"))),
      lapply(names(r), function(nm) {
        df <- r[[nm]]
        tagList(tags$h4(nm), if (!nrow(df)) p("(vazio)") else
          tags$table(tags$thead(tags$tr(lapply(names(df), tags$th))),
                     tags$tbody(lapply(seq_len(min(nrow(df), 1000)), function(i) tags$tr(lapply(df[i, ], function(v) tags$td(as.character(v))))))))
      }))
  })
  output$rp_xlsx <- downloadHandler(filename = function() paste0("relatorio_", S$detail()$id, ".xlsx"), content = function(file) {
    r <- dados_relatorio(S$detail()$id); wb <- openxlsx::createWorkbook()
    for (nm in names(r)) { sh <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "", nm), 1, 31); openxlsx::addWorksheet(wb, sh)
      openxlsx::writeData(wb, sh, if (nrow(r[[nm]])) r[[nm]] else data.frame(Aviso = "vazio")) }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  })
  output$rp_docx <- downloadHandler(filename = function() paste0("relatorio_", S$detail()$id, ".docx"), content = function(file) {
    id <- S$detail()$id; r <- dados_relatorio(id)
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, RELATORIOS[[id]], style = "heading 1")
    doc <- officer::body_add_par(doc, sprintf("%s — %s", db_meta(con, "project_name"), format(Sys.time(), "%d/%m/%Y %H:%M")), style = "Normal")
    for (nm in names(r)) {
      doc <- officer::body_add_par(doc, nm, style = "heading 2")
      df <- r[[nm]]
      if (nrow(df)) { df[] <- lapply(df, as.character); doc <- officer::body_add_table(doc, df, style = "table_template") }
      else doc <- officer::body_add_par(doc, "(vazio)", style = "Normal")
    }
    print(doc, target = file)
  })
}
