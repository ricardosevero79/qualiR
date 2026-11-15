# ---------------------------------------------------------------------------
# QualiR - consultas (Explorar): parâmetros, execução, exportação e consultas salvas
# ---------------------------------------------------------------------------

dt_pt <- list(search = "Filtrar:", lengthMenu = "Mostrar _MENU_", info = "_START_ a _END_ de _TOTAL_",
              infoEmpty = "0 resultados", emptyTable = "Sem resultados — clique em Executar",
              zeroRecords = "Nada encontrado", paginate = list(previous = "Anterior", `next` = "Próxima"))

QUERY_TIPOS <- c(freq = "Frequência de palavras", busca = "Pesquisa de texto", arvore = "Árvore de palavras",
                 codq = "Consulta de codificação", matriz = "Matriz de codificação",
                 cooc = "Coocorrência de nós", cluster = "Análise de agrupamento",
                 comp = "Comparação de codificação")
QUERY_DESC <- c(freq = "Palavras mais frequentes, nuvem e gráfico",
                busca = "Localiza termos com contexto e permite codificar os resultados",
                arvore = "Mostra em ramos as palavras que vêm antes e depois de um termo",
                codq = "Combina nós (E, OU, NÃO) e filtra por casos ou atributos",
                matriz = "Cruza nós com arquivos, casos, atributos ou codificadores",
                cooc = "Nós que se sobrepõem nos mesmos trechos (tabela e rede)",
                cluster = "Agrupa nós ou arquivos semelhantes (dendrograma)",
                comp = "Concordância e Kappa de Cohen entre membros da equipe")
QUERY_ICONES <- c(freq = "chart-column", busca = "magnifying-glass", arvore = "code-branch", codq = "filter",
                  matriz = "table-cells", cooc = "share-nodes", cluster = "sitemap", comp = "people-arrows")

# converte parâmetros vindos do JSON (listas vazias, etc.)
norm_params <- function(p) lapply(p, function(x) if (is.list(x)) unlist(x) else x)

ui_consulta <- function(qid, d, S) {
  p <- norm_params(d$params %||% list())
  pv <- function(k, def) if (!is.null(p[[k]])) p[[k]] else def
  dd <- S$docs(); nd <- S$nodes(); us <- S$users(); pt <- S$paths(); at <- S$attrs(); da <- S$dattrs()
  docs_ch <- setNames(as.character(dd$id), dd$name)
  st <- S$sets()
  if (nrow(st)) docs_ch <- list(Arquivos = docs_ch, Conjuntos = setNames(paste0("s:", st$id), paste("Conjunto:", st$name)))
  cod <- nd[nd$kind == "code", ]; cod <- cod[order(tolower(pt[as.character(cod$id)])), ]
  cod_ch <- setNames(as.character(cod$id), pt[as.character(cod$id)])
  all_ch <- S$escolhas_nodes()
  us_ch <- setNames(as.character(us$id), us$name)
  attr_ch <- sort(unique(at$attr)); dattr_ch <- sort(unique(da$attr))
  casos <- nd[nd$kind == "case", ]
  filtro_ch <- list()
  if (nrow(casos)) filtro_ch[["Casos"]] <- setNames(paste0("c:", casos$id), casos$name)
  atv <- unique(at[nzchar(at$value), c("attr", "value")])
  if (nrow(atv)) filtro_ch[["Atributos dos casos"]] <- setNames(paste0("a:", atv$attr, "\u001f", atv$value),
                                                                paste0(atv$attr, " = ", atv$value))

  titulo <- QUERY_TIPOS[[qid]]
  if (!is.null(d$saved_name)) titulo <- paste0(titulo, " — ", d$saved_name)
  head <- div(class = "dhead", div(class = "dtitle", icon(QUERY_ICONES[[qid]]), tags$span(titulo)),
              actionLink("q_save", tagList(icon("floppy-disk"), " Salvar consulta"), style = "font-size:12px"),
              downloadLink("q_export", tagList(icon("file-excel"), " Exportar resultados (Excel)"), style = "font-size:12px"),
              actionLink("detail_close", icon("xmark")))
  multi <- function(id, lab, ch, sel = NULL, ph = "Todos") selectizeInput(id, lab, ch, selected = sel, multiple = TRUE,
                                                                          options = list(placeholder = ph, plugins = list("remove_button")))
  executar <- function(id) actionButton(id, "Executar consulta", icon = icon("play"), class = "btn-primary w-100")

  corpo <- switch(qid,
    freq = div(class = "qpanel",
      div(class = "qparams",
          radioButtons("fq_scope", "Pesquisar em", c("Todos os arquivos" = "all", "Arquivos selecionados" = "docs",
                                                    "Trechos codificados em" = "node"), pv("scope", "all")),
          conditionalPanel("input.fq_scope == 'docs'", multi("fq_docs", NULL, docs_ch, pv("docs", NULL), ph = "Escolha os arquivos")),
          conditionalPanel("input.fq_scope == 'node'", selectizeInput("fq_node", NULL, all_ch, pv("node", NULL)),
                           checkboxInput("fq_agg", "Incluir nós filhos", pv("agg", TRUE))),
          numericInput("fq_top", "Exibir as N palavras mais frequentes", pv("top", 100), 10, 1000, 10),
          numericInput("fq_min", "Comprimento mínimo", pv("min", 3), 1, 20),
          checkboxInput("fq_sw", "Ignorar palavras vazias (stopwords em português)", pv("sw", TRUE)),
          checkboxInput("fq_spk", "Ignorar nomes de falantes e marcações de tempo", pv("spk", TRUE)),
          textAreaInput("fq_stop", "Palavras a ignorar (separadas por vírgula)", pv("stop", ""), rows = 2),
          executar("fq_run")),
      div(class = "qresults", tabsetPanel(
        tabPanel("Resumo", DT::DTOutput("fq_dt"),
                 tags$small("Clique em uma palavra para ver a árvore de palavras; use os botões abaixo para pesquisá-la."),
                 div(style = "margin-top:6px", actionButton("fq_to_busca", "Pesquisar palavra selecionada", class = "btn-sm btn-outline-primary"),
                     actionButton("fq_to_arvore", "Árvore da palavra selecionada", class = "btn-sm btn-outline-primary"))),
        tabPanel("Nuvem de palavras", plotOutput("fq_cloud", height = "400px")),
        tabPanel("Gráfico", plotOutput("fq_bar", height = "420px"))))),

    busca = div(class = "qpanel",
      div(class = "qparams",
          textInput("bs_term", "Pesquisar por", pv("term", ""), placeholder = "ex.: escola*  |  \"saúde mental\"  |  medo OU receio"),
          tags$small(style = "color:#6B7280", "Use * como coringa (prefeit* = prefeito, prefeitura…), aspas para frase exata e OU para alternativas."),
          multi("bs_docs", "Arquivos", docs_ch, pv("docs", NULL), ph = "Todos os arquivos"),
          numericInput("bs_ctx", "Palavras de contexto", pv("ctx", 8), 1, 50),
          executar("bs_run"),
          tags$hr(),
          tags$label("Codificar os resultados em"),
          selectizeInput("bs_node", NULL, all_ch, selected = character(0), options = SELECTIZE_CRIAR("Nó existente ou novo nome")),
          radioButtons("bs_spread", "Ampliar codificação", c("Apenas o termo encontrado" = "none", "Parágrafo inteiro" = "para"), "para"),
          actionButton("bs_code", "Codificar todos os resultados", icon = icon("highlighter"), class = "btn-outline-primary w-100"),
          actionButton("bs_result", "Salvar em Resultados da consulta", icon = icon("square-poll-horizontal"), class = "btn-outline-secondary w-100 mt-2")),
      div(class = "qresults", uiOutput("bs_info"), DT::DTOutput("bs_dt"))),

    arvore = div(class = "qpanel",
      div(class = "qparams",
          textInput("wt_term", "Palavra ou expressão", pv("term", ""), placeholder = "ex.: escola  |  medo*"),
          multi("wt_docs", "Arquivos", docs_ch, pv("docs", NULL), ph = "Todos os arquivos"),
          numericInput("wt_n", "Palavras em cada ramo", pv("n", 6), 2, 15),
          executar("wt_run"),
          tags$hr(),
          tags$small(style = "color:#6B7280", "À esquerda, o que vem antes do termo; à direita, o que vem depois. ",
                     "O tamanho da fonte indica a frequência. Os ramos param no fim da frase ou da fala.")),
      div(class = "qresults", tabsetPanel(
        tabPanel("Árvore", uiOutput("wt_tree")),
        tabPanel("Ocorrências", DT::DTOutput("wt_dt"))))),

    codq = div(class = "qpanel",
      div(class = "qparams",
          selectizeInput("cq_a", "Trechos codificados em", all_ch, pv("a", NULL)),
          radioButtons("cq_op", "Combinar com", c("Nada (apenas este nó)" = "so", "E (codificado também em)" = "e",
                                                 "OU (ou codificado em)" = "ou", "NÃO (mas não codificado em)" = "nao"), pv("op", "so")),
          conditionalPanel("input.cq_op != 'so'", selectizeInput("cq_b", NULL, all_ch, pv("b", NULL))),
          checkboxInput("cq_agg", "Incluir nós filhos", pv("agg", TRUE)),
          multi("cq_filter", "Somente nas falas de", filtro_ch, pv("filter", NULL), ph = "Todos (sem restrição)"),
          multi("cq_docs", "Arquivos", docs_ch, pv("docs", NULL), ph = "Todos os arquivos"),
          multi("cq_coders", "Codificação de", us_ch, pv("coders", NULL), ph = "Toda a equipe"),
          executar("cq_run"),
          tags$hr(),
          textInput("cq_newname", "Salvar resultado como nó", "", placeholder = "Nome do novo nó"),
          actionButton("cq_tonode", "Criar nó com os resultados", icon = icon("circle-nodes"), class = "btn-outline-primary w-100"),
          actionButton("cq_result", "Salvar em Resultados da consulta", icon = icon("square-poll-horizontal"), class = "btn-outline-secondary w-100 mt-2")),
      div(class = "qresults", uiOutput("cq_out"))),

    matriz = div(class = "qpanel",
      div(class = "qparams",
          multi("mx_rows", "Linhas (nós)", cod_ch, pv("rows", NULL), ph = "Todos os nós de nível superior"),
          radioButtons("mx_cols", "Colunas", c("Arquivos" = "docs", "Casos" = "cases", "Atributo dos casos" = "attr",
                                              "Atributo dos arquivos" = "dattr", "Codificadores (equipe)" = "users"), pv("cols", "docs")),
          conditionalPanel("input.mx_cols == 'attr'", selectInput("mx_attr", "Atributo dos casos", attr_ch, pv("attr", NULL))),
          conditionalPanel("input.mx_cols == 'dattr'", selectInput("mx_dattr", "Atributo dos arquivos", dattr_ch, pv("dattr", NULL))),
          radioButtons("mx_val", "Valor das células", c("Número de referências" = "refs", "Número de arquivos" = "docs"), pv("val", "refs")),
          checkboxInput("mx_agg", "Agregar nós filhos", pv("agg", TRUE)),
          executar("mx_run")),
      div(class = "qresults", div(style = "margin-bottom:6px", actionButton("mx_save", "Salvar matriz", icon = icon("floppy-disk"), class = "btn-sm btn-outline-secondary")),
          DT::DTOutput("mx_dt"),
          tags$small("Clique em uma célula para ver os trechos correspondentes."), uiOutput("mx_refs"))),

    cooc = div(class = "qpanel",
      div(class = "qparams",
          multi("cc_nodes", "Nós", cod_ch, pv("nodes", NULL), ph = "Todos os nós"),
          numericInput("cc_min", "Mínimo de coocorrências para a rede", pv("min", 1), 1, 100),
          executar("cc_run")),
      div(class = "qresults", tabsetPanel(
        tabPanel("Rede", plotOutput("cc_net", height = "400px")),
        tabPanel("Matriz", DT::DTOutput("cc_dt")),
        tabPanel("Pares", DT::DTOutput("cc_pairs"))))),

    cluster = div(class = "qpanel",
      div(class = "qparams",
          radioButtons("cl_itens", "Agrupar", c("Nós" = "codes", "Arquivos" = "docs"), pv("itens", "codes")),
          radioButtons("cl_base", "Semelhança por", c("Palavras (vocabulário)" = "palavras",
                                                     "Codificação" = "codificacao"), pv("base", "codificacao")),
          conditionalPanel("input.cl_itens == 'codes'", multi("cl_codes", "Nós", cod_ch, pv("codes", NULL), ph = "Todos os nós")),
          conditionalPanel("input.cl_itens == 'docs'", multi("cl_docs", "Arquivos", docs_ch, pv("docs", NULL), ph = "Todos os arquivos")),
          numericInput("cl_k", "Número de grupos (cores)", pv("k", 3), 1, 20),
          executar("cl_run"),
          tags$hr(),
          tags$small(style = "color:#6B7280",
            "Nós por codificação: Jaccard sobre os parágrafos em que cada nó aparece. ",
            "Arquivos por codificação: cosseno sobre o número de referências por nó. ",
            "Por palavras: cosseno sobre o vocabulário (sem stopwords).")),
      div(class = "qresults", tabsetPanel(
        tabPanel("Dendrograma", plotOutput("cl_plot", height = "auto")),
        tabPanel("Similaridade", DT::DTOutput("cl_dt"))))),

    comp = div(class = "qpanel",
      div(class = "qparams",
          multi("cp_a", "Usuário(s) A", us_ch, pv("a", us_ch[1]), ph = "Escolha"),
          multi("cp_b", "Usuário(s) B", us_ch, pv("b", if (length(us_ch) > 1) us_ch[2] else NULL), ph = "Escolha"),
          multi("cp_nodes", "Nós", cod_ch, pv("nodes", NULL), ph = "Todos os nós"),
          multi("cp_docs", "Arquivos", docs_ch, pv("docs", NULL), ph = "Todos os arquivos"),
          executar("cp_run"),
          tags$hr(),
          tags$small(style = "color:#6B7280",
            "Concordância e Kappa de Cohen calculados caractere a caractere. ",
            "Referência usual: Kappa < 0,40 baixa; 0,40–0,75 razoável a boa; > 0,75 excelente.")),
      div(class = "qresults", tabsetPanel(
        tabPanel("Resumo por nó", DT::DTOutput("cp_sum")),
        tabPanel("Por nó e arquivo", DT::DTOutput("cp_dt")))))
  )
  tagList(head, corpo)
}

server_consultas <- function(input, output, session, S) {
  con <- S$con
  abrir_q <- function(q, params = NULL, autorun = FALSE, saved_name = NULL)
    S$detail(list(type = "query", id = q, params = params, autorun = autorun, saved_name = saved_name, n = runif(1)))
  S$abrir_q <- abrir_q
  for (q in names(QUERY_TIPOS)) local({
    qq <- q
    observeEvent(input[[paste0("rb_q_", qq)]], abrir_q(qq))
  })

  textos_docs <- function(ids = NULL) {
    q <- "SELECT id, name, content, is_transcript FROM documents"
    if (length(ids)) q <- paste0(q, " WHERE id IN (", paste(as.integer(ids), collapse = ","), ")")
    DBI::dbGetQuery(con, q)
  }
  num <- function(x, def) { x <- suppressWarnings(as.numeric(x)); if (!length(x) || is.na(x[1])) def else x[1] }
  ids_int <- function(x) as.integer(unlist(x) %||% integer())
  # arquivos escolhidos, expandindo conjuntos ("s:ID")
  docs_de <- function(x) {
    x <- as.character(unlist(x) %||% character())
    conj <- as.integer(sub("^s:", "", x[startsWith(x, "s:")]))
    ids <- suppressWarnings(as.integer(x[!startsWith(x, "s:")]))
    if (length(conj)) { si <- S$set_items(); ids <- c(ids, si$item_id[si$set_id %in% conj & si$item_type == "doc"]) }
    if (length(conj) && !length(ids)) return(-1L)
    unique(ids[!is.na(ids)])
  }
  aviso <- function(msg) { showNotification(msg, type = "warning"); NULL }

  # ===================== parâmetros atuais de cada consulta =================
  params_atuais <- function(q) switch(q,
    freq = list(scope = input$fq_scope, docs = input$fq_docs, node = input$fq_node, agg = isTRUE(input$fq_agg),
                top = input$fq_top, min = input$fq_min, sw = isTRUE(input$fq_sw), spk = isTRUE(input$fq_spk), stop = input$fq_stop),
    busca = list(term = input$bs_term, docs = input$bs_docs, ctx = input$bs_ctx),
    arvore = list(term = input$wt_term, docs = input$wt_docs, n = input$wt_n),
    codq = list(a = input$cq_a, op = input$cq_op, b = input$cq_b, agg = isTRUE(input$cq_agg), filter = input$cq_filter,
                docs = input$cq_docs, coders = input$cq_coders),
    matriz = list(rows = input$mx_rows, cols = input$mx_cols, attr = input$mx_attr, dattr = input$mx_dattr,
                  val = input$mx_val, agg = isTRUE(input$mx_agg)),
    cooc = list(nodes = input$cc_nodes, min = input$cc_min),
    cluster = list(itens = input$cl_itens, base = input$cl_base, codes = input$cl_codes, docs = input$cl_docs, k = input$cl_k),
    comp = list(a = input$cp_a, b = input$cp_b, nodes = input$cp_nodes, docs = input$cp_docs))

  R <- reactiveValues()   # resultados por tipo de consulta

  # ===================== execução ==========================================
  run <- list()
  run$freq <- function(p) {
    sc <- p$scope %||% "all"
    if (sc == "node") {
      if (!nzchar(p$node %||% "")) return(aviso("Escolha um nó."))
      nd <- S$nodes(); ids <- if (isTRUE(p$agg)) descendentes(nd, as.integer(p$node)) else as.integer(p$node)
      cc <- S$cods(); cc <- cc[cc$node_id %in% ids, ]
      td <- textos_docs(unique(cc$doc_id))
      tx <- substr(td$content[match(cc$doc_id, td$id)], cc$pos_start + 1, cc$pos_end)
    } else {
      td <- textos_docs(if (sc == "docs") docs_de(p$docs) else NULL)
      tx <- td$content
      if (isTRUE(p$spk)) tx <- ifelse(td$is_transcript == 1, vapply(tx, remover_prefixos_turno, ""), tx)
    }
    if (isTRUE(p$spk)) tx <- gsub("\\[?\\d{1,2}:\\d{2}(:\\d{2})?([.,]\\d+)?\\]?", " ", tx)
    extra <- trimws(strsplit(p$stop %||% "", "[,;\n]")[[1]])
    R$freq <- frequencia_palavras(tx, num(p$min, 3), extra, num(p$top, 100), !isFALSE(p$sw))
  }
  run$busca <- function(p) {
    if (!nzchar(trimws(p$term %||% ""))) return(aviso("Digite o termo a pesquisar."))
    R$busca <- busca_texto(textos_docs(docs_de(p$docs)), p$term, num(p$ctx, 8))
    R$busca_term <- p$term
  }
  run$arvore <- function(p) {
    if (!nzchar(trimws(p$term %||% ""))) return(aviso("Digite a palavra."))
    td <- textos_docs(docs_de(p$docs))
    R$arvore <- list(dados = arvore_dados(td, p$term, num(p$n, 6)), kwic = busca_texto(td, p$term, 10))
  }
  run$codq <- function(p) {
    if (!nzchar(p$a %||% "")) return(aviso("Escolha o nó."))
    nd <- S$nodes(); cc <- S$cods(); at <- S$attrs()
    casos <- NULL
    if (length(p$filter)) {
      f <- unlist(p$filter)
      casos <- as.integer(sub("^c:", "", f[startsWith(f, "c:")]))
      for (a in f[startsWith(f, "a:")]) {
        kv <- strsplit(sub("^a:", "", a), "\u001f", fixed = TRUE)[[1]]
        casos <- c(casos, at$node_id[at$attr == kv[1] & at$value == kv[2]])
      }
      casos <- unique(casos)
    }
    b <- if ((p$op %||% "so") != "so" && nzchar(p$b %||% "")) as.integer(p$b) else NULL
    if ((p$op %||% "so") != "so" && is.null(b)) return(aviso("Escolha o segundo nó."))
    res <- consulta_codificacao(nd, cc, as.integer(p$a), p$op %||% "so", b, isTRUE(p$agg),
                                ids_int(p$coders), casos, docs_de(p$docs))
    td <- textos_docs(unique(res$doc_id)); dd <- S$docs()
    res$Documento <- dd$name[match(res$doc_id, dd$id)]
    res$Trecho <- if (nrow(res)) substr(td$content[match(res$doc_id, td$id)], res$s + 1, res$e) else character()
    R$codq <- res
  }
  run$matriz <- function(p) {
    nd <- S$nodes()
    rows <- ids_int(p$rows)
    if (!length(rows)) rows <- nd$id[nd$kind == "code" & is.na(nd$parent_id)]
    if (!length(rows)) return(aviso("Não há nós."))
    cols <- p$cols %||% "docs"
    if (cols == "attr" && !nzchar(p$attr %||% "")) return(aviso("Crie atributos em Classificação de casos."))
    if (cols == "dattr" && !nzchar(p$dattr %||% "")) return(aviso("Crie atributos em Classificação de arquivos."))
    r <- matriz_codificacao(nd, S$cods(), S$docs(), S$users(), S$attrs(), rows, cols,
                            if (cols == "dattr") p$dattr else p$attr, !isFALSE(p$agg), p$val %||% "refs", dattrs = S$dattrs())
    r$rows <- rows
    R$matriz <- r
  }
  run$cooc <- function(p) {
    nd <- S$nodes(); ids <- ids_int(p$nodes)
    if (!length(ids)) ids <- nd$id[nd$kind == "code"]
    if (length(ids) < 2) return(aviso("São necessários ao menos 2 nós."))
    R$cooc <- list(mat = coocorrencia(S$cods(), ids), ids = ids, min = num(p$min, 1))
  }
  run$cluster <- function(p) {
    nd <- S$nodes(); dd <- S$docs()
    itens <- p$itens %||% "codes"
    ids <- if (itens == "codes") ids_int(p$codes) else docs_de(p$docs)
    if (!length(ids)) ids <- if (itens == "codes") nd$id[nd$kind == "code"] else dd$id
    if (length(ids) < 3) return(aviso("São necessários ao menos 3 itens para agrupar."))
    withProgress(message = "Calculando semelhanças…", {
      r <- similaridade(con, nd, S$cods(), dd, itens, p$base %||% "codificacao", ids)
    })
    r$k <- max(1, min(num(p$k, 3), length(ids)))
    R$cluster <- r
  }
  run$comp <- function(p) {
    a <- ids_int(p$a); b <- ids_int(p$b)
    if (!length(a) || !length(b)) return(aviso("Escolha os usuários A e B."))
    if (length(intersect(a, b))) return(aviso("Um mesmo usuário não pode estar em A e B."))
    nd <- S$nodes(); dd <- S$docs()
    nodes <- ids_int(p$nodes); if (!length(nodes)) nodes <- nd$id[nd$kind == "code"]
    docs <- docs_de(p$docs); if (!length(docs)) docs <- dd$id
    withProgress(message = "Comparando codificação…", {
      R$comp <- comparar_codificadores(S$cods(), dd, nd, a, b, nodes, docs)
    })
  }

  botoes <- c(freq = "fq_run", busca = "bs_run", arvore = "wt_run", codq = "cq_run", matriz = "mx_run",
              cooc = "cc_run", cluster = "cl_run", comp = "cp_run")
  for (q in names(botoes)) local({
    qq <- q
    observeEvent(input[[botoes[[qq]]]], run[[qq]](params_atuais(qq)))
  })
  # execução automática (consultas salvas e atalhos)
  observeEvent(S$detail(), {
    d <- S$detail()
    if (identical(d$type, "query") && isTRUE(d$autorun) && !is.null(d$params))
      run[[d$id]](norm_params(d$params))
  })

  # ===================== salvar consulta / exportar =========================
  observeEvent(input$q_save, {
    d <- S$detail(); req(identical(d$type, "query"))
    if (!S$pode_editar()) return()
    showModal(modalDialog(title = "Salvar consulta", size = "s",
      textInput("qs_name", "Nome", d$saved_name %||% paste(QUERY_TIPOS[[d$id]], format(Sys.Date(), "%d/%m")), width = "100%"),
      tags$small("A consulta fica disponível para toda a equipe em Consultas, com os mesmos parâmetros."),
      footer = tagList(modalButton("Cancelar"), actionButton("qs_ok", "Salvar", class = "btn-primary"))))
  })
  observeEvent(input$qs_ok, {
    d <- S$detail(); nm <- trimws(input$qs_name %||% ""); req(nzchar(nm))
    db_salvar_consulta(con, nm, d$id, params_atuais(d$id), S$me())
    S$log("salvou consulta", nm); removeModal()
    showNotification(sprintf("Consulta “%s” salva em Consultas.", nm))
  })

  output$q_export <- downloadHandler(
    filename = function() paste0("consulta_", isolate(S$detail()$id), "_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content = function(file) {
      q <- S$detail()$id; pt <- S$paths(); folhas <- list()
      if (q == "freq" && !is.null(R$freq)) folhas$`Frequência` <- R$freq
      if (q %in% c("busca") && !is.null(R$busca)) folhas$Resultados <- R$busca[, c("Documento", "Esquerda", "Termo", "Direita", "pos_start", "pos_end")]
      if (q == "arvore" && !is.null(R$arvore)) folhas$`Ocorrências` <- R$arvore$kwic[, c("Documento", "Esquerda", "Termo", "Direita")]
      if (q == "codq" && !is.null(R$codq)) folhas$Trechos <- data.frame(Documento = R$codq$Documento, `Início` = R$codq$s,
                                                                        Fim = R$codq$e, Trecho = R$codq$Trecho, check.names = FALSE)
      if (q == "matriz" && !is.null(R$matriz)) {
        r <- R$matriz
        folhas$Matriz <- data.frame(`Nó` = unname(pt[rownames(r$mat)]), r$mat, check.names = FALSE)
        det <- r$detalhes[!duplicated(paste(r$detalhes$id, r$detalhes$col_key)), ]
        if (nrow(det)) {
          td <- textos_docs(unique(det$doc_id)); dd <- S$docs()
          folhas$Trechos <- data.frame(`Nó` = unname(pt[as.character(det$row_id)]), Coluna = as.character(det$col_key),
                                       Documento = dd$name[match(det$doc_id, dd$id)],
                                       Trecho = substr(td$content[match(det$doc_id, td$id)], det$pos_start + 1, det$pos_end),
                                       check.names = FALSE)
          mapa <- setNames(names(r$cols), as.character(unlist(r$cols)))
          folhas$Trechos$Coluna <- unname(mapa[folhas$Trechos$Coluna])
        }
      }
      if (q == "cooc" && !is.null(R$cooc)) {
        m <- R$cooc$mat; nms <- unname(pt[rownames(m)])
        folhas$Matriz <- data.frame(`Nó` = nms, setNames(as.data.frame(m), nms), check.names = FALSE)
      }
      if (q == "cluster" && !is.null(R$cluster)) {
        s <- R$cluster$sim
        h <- stats::hclust(stats::as.dist(1 - s), "average")
        folhas$Grupos <- data.frame(Item = rownames(s), Grupo = stats::cutree(h, R$cluster$k), check.names = FALSE)
        folhas$Similaridade <- data.frame(Item = rownames(s), round(s, 3), check.names = FALSE)
      }
      if (q == "comp" && !is.null(R$comp) && nrow(R$comp)) {
        folhas$Resumo <- resumo_kappa(R$comp)
        folhas$Detalhes <- R$comp[, setdiff(names(R$comp), c("node_id", "doc_id", "n_chars"))]
      }
      if (!length(folhas)) folhas$Aviso <- data.frame(Aviso = "Execute a consulta antes de exportar.")
      wb <- openxlsx::createWorkbook()
      hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F4")
      for (nm in names(folhas)) {
        openxlsx::addWorksheet(wb, nm)
        openxlsx::writeData(wb, nm, folhas[[nm]], headerStyle = hs)
        openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(folhas[[nm]])), widths = "auto")
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })

  # ===================== FREQUÊNCIA ==========================================
  output$fq_dt <- DT::renderDT({
    r <- R$freq; req(r)
    DT::datatable(r, rownames = FALSE, selection = "single",
                  options = list(pageLength = 25, language = dt_pt, dom = "ftip")) |>
      DT::formatStyle("Contagem", background = DT::styleColorBar(range(c(0, r$Contagem)), "#BFD3EE"),
                      backgroundSize = "98% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })
  palavra_sel <- function() {
    r <- R$freq; i <- input$fq_dt_rows_selected
    if (is.null(r) || !length(i)) { showNotification("Selecione uma palavra na tabela.", type = "warning"); return(NULL) }
    r$Palavra[i]
  }
  observeEvent(input$fq_to_busca, { w <- palavra_sel(); req(w); abrir_q("busca", list(term = w), TRUE) })
  observeEvent(input$fq_to_arvore, { w <- palavra_sel(); req(w); abrir_q("arvore", list(term = w), TRUE) })
  output$fq_cloud <- renderPlot({
    r <- R$freq; req(r, nrow(r) > 0)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    pal <- c("#9DB7DE", "#6A93CF", "#2B579A", "#1E3F73", "#E8505B")
    wordcloud::wordcloud(r$Palavra, r$Contagem, min.freq = 1, max.words = nrow(r), random.order = FALSE,
                         rot.per = 0.15, colors = pal, scale = c(4.5, 0.7))
  })
  output$fq_bar <- renderPlot({
    r <- R$freq; req(r, nrow(r) > 0); r <- head(r, 30)
    op <- par(mar = c(4, 9, 1, 1)); on.exit(par(op))
    barplot(rev(r$Contagem), names.arg = rev(r$Palavra), horiz = TRUE, las = 1, col = "#2B579A", border = NA,
            xlab = "Contagem", cex.names = 0.85)
  })

  # ===================== PESQUISA DE TEXTO ===================================
  ir_para <- function(doc, s, e) {
    S$detail(list(type = "doc", id = as.integer(doc)))
    session$onFlushed(function() session$sendCustomMessage("qr_scroll", list(start = s, end = e)), once = TRUE)
  }
  output$bs_info <- renderUI({
    r <- R$busca; req(r)
    div(style = "margin-bottom:6px", tags$b(nrow(r)), " ocorrência(s) em ", tags$b(length(unique(r$doc_id))), " arquivo(s).",
        tags$small(style = "color:#6B7280", " Clique em uma linha para abrir o trecho no arquivo."))
  })
  kwic_dt <- function(r) DT::datatable(r[, c("Documento", "Esquerda", "Termo", "Direita")], rownames = FALSE, selection = "single",
                                       options = list(pageLength = 25, language = dt_pt, dom = "ftip",
                                                      columnDefs = list(list(className = "dt-right", targets = 1)))) |>
    DT::formatStyle("Termo", fontWeight = "bold", backgroundColor = "#FFF3B0")
  output$bs_dt <- DT::renderDT({ r <- R$busca; req(r); kwic_dt(r) })
  observeEvent(input$bs_dt_rows_selected, { x <- R$busca[input$bs_dt_rows_selected, ]; ir_para(x$doc_id, x$pos_start, x$pos_end) })
  observeEvent(input$bs_code, {
    r <- R$busca
    if (is.null(r) || !nrow(r)) { showNotification("Execute a pesquisa primeiro.", type = "warning"); return() }
    if (!S$pode_editar()) return()
    v <- input$bs_node %||% ""
    if (!nzchar(v)) { showNotification("Escolha ou digite um nó.", type = "warning"); return() }
    nid <- if (grepl("^\\d+$", v)) as.integer(v) else {
      x <- db_criar_node(con, trimws(v), "code", NA, cor_aleatoria(nrow(S$nodes()) + 1), "Criado a partir de pesquisa de texto", S$me())
      S$log("criou nó", v); x }
    td <- textos_docs(unique(r$doc_id)); n <- 0
    DBI::dbWithTransaction(con, {
      for (i in seq_len(nrow(r))) {
        s <- r$pos_start[i]; e <- r$pos_end[i]
        if (input$bs_spread == "para") { lp <- limites_paragrafo(td$content[td$id == r$doc_id[i]], s, e); s <- lp[1]; e <- lp[2] }
        db_codificar(con, r$doc_id[i], nid, s, e, S$me(), tocar = FALSE); n <- n + 1
      }
    })
    db_tocar(con)
    S$log("codificou resultados de pesquisa", sprintf("%d trechos (“%s”)", n, R$busca_term %||% ""))
    showNotification(sprintf("%d trecho(s) codificado(s).", n))
  })

  # ===================== ÁRVORE DE PALAVRAS ==================================
  output$wt_tree <- renderUI({
    r <- R$arvore; req(r)
    HTML(render_arvore_palavras(r$dados))
  })
  output$wt_dt <- DT::renderDT({ r <- R$arvore; req(r, nrow(r$kwic) > 0); kwic_dt(r$kwic) })
  observeEvent(input$wt_dt_rows_selected, { x <- R$arvore$kwic[input$wt_dt_rows_selected, ]; ir_para(x$doc_id, x$pos_start, x$pos_end) })

  # ===================== CONSULTA DE CODIFICAÇÃO =============================
  output$cq_out <- renderUI({
    r <- R$codq; req(r)
    if (!nrow(r)) return(div(class = "detail-empty", icon("filter"), "Nenhum trecho atende aos critérios."))
    dd <- S$docs()
    tagList(
      div(style = "margin-bottom:8px", tags$b(nrow(r)), " trecho(s) em ", tags$b(length(unique(r$doc_id))), " arquivo(s)."),
      lapply(split(r, r$doc_id), function(g) div(class = "refgroup",
        div(class = "rg-head", icon("file-lines"), sprintf(" Arquivos\\%s", g$Documento[1]), tags$small(sprintf(" — %d trecho(s)", nrow(g)))),
        lapply(seq_len(nrow(g)), function(i) div(class = "ref",
          div(class = "rmeta", tags$span(sprintf("Trecho %d", i)),
              tags$a(`data-goto` = "1", `data-doc` = g$doc_id[i], `data-start` = g$s[i], `data-end` = g$e[i],
                     icon("arrow-up-right-from-square"), "Abrir no arquivo")),
          div(class = "rtext", trunca(g$Trecho[i], 1500)))))))
  })
  observeEvent(input$cq_tonode, {
    r <- R$codq
    if (is.null(r) || !nrow(r)) { showNotification("Execute a consulta primeiro.", type = "warning"); return() }
    nm <- trimws(input$cq_newname %||% "")
    if (!nzchar(nm)) { showNotification("Digite o nome do novo nó.", type = "warning"); return() }
    if (!S$pode_editar()) return()
    nid <- db_criar_node(con, nm, "code", NA, cor_aleatoria(nrow(S$nodes()) + 1), "Criado a partir de consulta de codificação", S$me())
    DBI::dbWithTransaction(con, for (i in seq_len(nrow(r))) db_codificar(con, r$doc_id[i], nid, r$s[i], r$e[i], S$me(), tocar = FALSE))
    db_tocar(con); S$log("criou nó a partir de consulta", nm)
    showNotification(sprintf("Nó “%s” criado com %d trecho(s).", nm, nrow(r)))
  })

  # ===================== MATRIZ ==============================================
  output$mx_dt <- DT::renderDT({
    r <- R$matriz; req(r)
    pt <- S$paths()
    df <- data.frame(`Nó` = unname(pt[rownames(r$mat)]), r$mat, check.names = FALSE)
    mx <- max(r$mat, 1)
    brks <- unique(round(seq(0, mx, length.out = 6)[-1] - 0.5, 1))
    cores <- colorRampPalette(c("#FFFFFF", "#9DB7DE", "#2B579A"))(length(brks) + 1)
    dt <- DT::datatable(df, rownames = FALSE, selection = list(mode = "single", target = "cell"),
                        options = list(pageLength = 50, dom = "t", scrollX = TRUE, ordering = FALSE, language = dt_pt))
    if (ncol(r$mat)) dt <- DT::formatStyle(dt, colnames(r$mat), backgroundColor = DT::styleInterval(brks, cores),
                                            color = DT::styleInterval(mx * 0.6, c("#222", "#fff")), textAlign = "center")
    dt
  })
  output$mx_refs <- renderUI({
    r <- R$matriz; req(r); cs <- input$mx_dt_cells_selected
    req(!is.null(cs), length(cs) == 2, cs[2] >= 1)
    i <- cs[1]; j <- cs[2]
    key <- r$cols[[j]]; row <- r$rows[i]
    det <- r$detalhes[r$detalhes$row_id == row & r$detalhes$col_key == key, ]
    det <- det[!duplicated(det$id), ]
    if (!nrow(det)) return(div(style = "margin-top:10px;color:#6B7280", "Nenhum trecho nesta célula."))
    td <- textos_docs(unique(det$doc_id)); dd <- S$docs(); us <- S$users()
    tagList(tags$h6(style = "margin-top:14px", sprintf("%s × %s — %d trecho(s)", S$paths()[as.character(row)],
                                                       names(r$cols)[j], nrow(det))),
      lapply(seq_len(min(nrow(det), 200)), function(k) {
        x <- det[k, ]
        div(class = "ref", div(class = "rmeta", tags$b(dd$name[dd$id == x$doc_id]), tags$span(us$name[us$id == x$user_id]),
                               tags$a(`data-goto` = "1", `data-doc` = x$doc_id, `data-start` = x$pos_start, `data-end` = x$pos_end,
                                      "Abrir no arquivo")),
            div(class = "rtext", trunca(substr(td$content[td$id == x$doc_id], x$pos_start + 1, x$pos_end), 700)))
      }))
  })

  # ===================== COOCORRÊNCIA ========================================
  output$cc_dt <- DT::renderDT({
    r <- R$cooc; req(r); pt <- S$paths()
    m <- r$mat; colnames(m) <- trunca(unname(pt[colnames(m)]), 25)
    df <- data.frame(`Nó` = unname(pt[rownames(r$mat)]), m, check.names = FALSE)
    DT::datatable(df, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 100, scrollX = TRUE, ordering = FALSE))
  })
  output$cc_pairs <- DT::renderDT({
    r <- R$cooc; req(r); pt <- S$paths(); m <- r$mat
    idx <- which(upper.tri(m) & m > 0, arr.ind = TRUE)
    df <- data.frame(`Nó A` = unname(pt[rownames(m)[idx[, 1]]]), `Nó B` = unname(pt[colnames(m)[idx[, 2]]]),
                     `Trechos em comum` = m[idx], check.names = FALSE)
    df <- df[order(-df$`Trechos em comum`), ]
    DT::datatable(df, rownames = FALSE, selection = "none", options = list(pageLength = 25, language = dt_pt))
  })
  output$cc_net <- renderPlot({
    r <- R$cooc; req(r); nd <- S$nodes(); m <- r$mat
    m[m < r$min] <- 0
    g <- igraph::graph_from_adjacency_matrix(m, mode = "undirected", weighted = TRUE, diag = FALSE)
    cont <- contagens_nodes(nd, S$cods())
    vid <- as.integer(igraph::V(g)$name)
    refs <- cont$n_refs[match(vid, cont$node_id)]
    igraph::V(g)$label <- trunca(nd$name[match(vid, nd$id)], 24)
    igraph::V(g)$color <- nd$color[match(vid, nd$id)]
    igraph::V(g)$size <- 8 + 22 * sqrt(refs / max(refs, 1))
    w <- igraph::E(g)$weight
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    if (!length(w)) { plot.new(); text(0.5, 0.5, "Nenhuma coocorrência encontrada", col = "#6B7280"); return() }
    set.seed(42)
    plot(g, layout = igraph::layout_with_fr(g), vertex.label.color = "#1E1E1E", vertex.label.cex = 0.9,
         vertex.label.family = "sans", vertex.frame.color = "white", vertex.label.dist = 1.6,
         edge.width = 1 + 5 * w / max(w), edge.color = adjustcolor("#2B579A", 0.45), edge.label = w,
         edge.label.cex = 0.8, edge.label.color = "#2B579A", margin = 0.15)
  })

  # ===================== AGRUPAMENTO =========================================
  output$cl_plot <- renderPlot({
    r <- R$cluster; req(r)
    s <- r$sim; h <- stats::hclust(stats::as.dist(1 - s), "average")
    grupos <- stats::cutree(h, r$k)
    pal <- c("#2B579A", "#E8505B", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C", "#795548", "#E91E63", "#7F8C8D")
    dend <- stats::as.dendrogram(h)
    dend <- stats::dendrapply(dend, function(n) {
      if (is.leaf(n)) attr(n, "nodePar") <- list(pch = 19, cex = 1.1, col = pal[(grupos[attr(n, "label")] - 1) %% length(pal) + 1],
                                                lab.col = pal[(grupos[attr(n, "label")] - 1) %% length(pal) + 1], lab.cex = 0.9)
      n
    })
    mx <- max(nchar(rownames(s)))
    op <- par(mar = c(4, 1, 2, min(25, 1 + mx * 0.45))); on.exit(par(op))
    plot(dend, horiz = TRUE, xlab = "Distância (1 − similaridade)", main = paste("Semelhança por", r$metodo),
         cex.main = 0.9, font.main = 1)
  }, height = function() { r <- R$cluster; if (is.null(r)) 300 else max(320, 90 + 34 * nrow(r$sim)) })
  output$cl_dt <- DT::renderDT({
    r <- R$cluster; req(r); s <- r$sim
    idx <- which(upper.tri(s), arr.ind = TRUE)
    df <- data.frame(`Item A` = rownames(s)[idx[, 1]], `Item B` = colnames(s)[idx[, 2]],
                     Similaridade = round(s[idx], 3), check.names = FALSE)
    df <- df[order(-df$Similaridade), ]
    DT::datatable(df, rownames = FALSE, selection = "none", options = list(pageLength = 25, language = dt_pt)) |>
      DT::formatStyle("Similaridade", background = DT::styleColorBar(c(0, 1), "#BFD3EE"),
                      backgroundSize = "98% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  # ===================== COMPARAÇÃO =========================================
  kappa_style <- function(dt, col) DT::formatStyle(dt, col, backgroundColor = DT::styleInterval(c(0.4, 0.75), c("#F8D7DA", "#FFF3CD", "#D1E7DD")), fontWeight = "bold")
  output$cp_sum <- DT::renderDT({
    r <- R$comp; req(r)
    if (!nrow(r)) return(DT::datatable(data.frame(Aviso = "Nenhuma codificação de A ou B nos itens escolhidos."), rownames = FALSE))
    kappa_style(DT::datatable(resumo_kappa(r), rownames = FALSE, selection = "none",
                              options = list(pageLength = 50, language = dt_pt)), "Kappa médio (ponderado)")
  })
  output$cp_dt <- DT::renderDT({
    r <- R$comp; req(r, nrow(r) > 0)
    kappa_style(DT::datatable(r[, setdiff(names(r), c("node_id", "doc_id", "n_chars"))], rownames = FALSE, selection = "none",
                              options = list(pageLength = 50, language = dt_pt, scrollX = TRUE)), "Kappa")
  })

  # ===================== salvar matriz e resultados ==========================
  observeEvent(input$mx_save, {
    r <- R$matriz
    if (is.null(r)) { showNotification("Execute a matriz primeiro.", type = "warning"); return() }
    showModal(modalDialog(title = "Salvar matriz de codificação", size = "s",
      textInput("mxsv_name", "Nome", paste("Matriz", format(Sys.time(), "%d/%m %H:%M")), width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("mxsv_ok", "Salvar", class = "btn-primary"))))
  })
  observeEvent(input$mxsv_ok, {
    r <- R$matriz; pt <- S$paths(); nm <- trimws(input$mxsv_name %||% ""); req(r, nzchar(nm))
    df <- data.frame(`Nó` = unname(pt[rownames(r$mat)]), r$mat, check.names = FALSE)
    db_salvar_matriz(con, nm, df, params_atuais("matriz"), S$me())
    S$log("salvou matriz", nm); removeModal(); showNotification("Matriz salva em Consultas > Matrizes de codificação.")
  })
  salvar_resultado <- function(nome, df) {   # df: doc_id, s, e
    if (!S$pode_editar()) return()
    nid <- db_criar_node(con, nome, "result", NA, "#9B59B6", "Resultado salvo de consulta", S$me())
    DBI::dbWithTransaction(con, for (i in seq_len(nrow(df))) db_codificar(con, df$doc_id[i], nid, df$s[i], df$e[i], S$me(), tocar = FALSE))
    db_tocar(con); S$log("salvou resultado de consulta", nome)
    showNotification(sprintf("Resultado \u201c%s\u201d salvo (%d trechos) em Consultas > Resultados da consulta.", nome, nrow(df)))
  }
  observeEvent(input$bs_result, {
    r <- R$busca; if (is.null(r) || !nrow(r)) { showNotification("Execute a pesquisa primeiro.", type = "warning"); return() }
    salvar_resultado(paste("Pesquisa de texto:", R$busca_term), data.frame(doc_id = r$doc_id, s = r$pos_start, e = r$pos_end))
  })
  observeEvent(input$cq_result, {
    r <- R$codq; if (is.null(r) || !nrow(r)) { showNotification("Execute a consulta primeiro.", type = "warning"); return() }
    salvar_resultado(paste("Consulta de codificação", format(Sys.time(), "%d/%m %H:%M")), r[, c("doc_id", "s", "e")])
  })
}
