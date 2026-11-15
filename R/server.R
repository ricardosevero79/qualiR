# ---------------------------------------------------------------------------
# QualiR - lógica do servidor (núcleo: projetos, login, dados ao vivo, abas, estado)
# ---------------------------------------------------------------------------

app_server <- function(reg, online) {
  function(input, output, session) {

    S <- new.env()   # estado compartilhado entre os blocos do servidor
    S$reg <- reg
    S$user <- reactiveVal(NULL)
    S$refresh <- reactiveVal(0)
    S$proj_ver <- reactiveVal(0)
    S$proj_sel <- reactiveVal(NULL)
    S$last_act <- reactiveVal(0)
    S$iniciado <- FALSE

    # ======================= TELA DE ENTRADA ================================
    output$page <- renderUI({
      u <- S$user()
      if (is.null(u)) {
        S$proj_ver()
        q <- parseQueryString(isolate(session$clientData$url_search) %||% "")
        ui_login(listar_projetos(), S$proj_sel() %||% q$projeto)
      } else isolate(ui_principal(u, S$projeto_nome))
    })
    server_projetos(input, output, session, S, reg)

    observeEvent(input$login_go, {
      pr <- listar_projetos(); p <- pr[pr$id == (input$login_proj %||% ""), ]
      if (!nrow(p)) { output$login_msg <- renderText("Escolha ou crie um projeto."); return() }
      con <- reg$get(p$path)
      u <- db_verificar_login(con, input$login_user %||% "", input$login_pass %||% "")
      if (is.null(u)) {
        output$login_msg <- renderText("Usuário ou senha inválidos para este projeto.")
        return()
      }
      S$con <- con; S$projeto <- p$path; S$projeto_nome <- db_meta(con, "project_name")
      if (!S$iniciado) { S$iniciado <- TRUE; iniciar_sessao(input, output, session, S, online) }
      db_log(con, u$id, "entrou no projeto")
      S$last_act(DBI::dbGetQuery(con, "SELECT COALESCE(MAX(id),0) m FROM activity")$m)
      lst <- online(); lst[[session$token]] <- list(id = u$id, name = u$name, color = u$color, proj = p$path)
      online(lst)
      S$user(u)
      session$sendCustomMessage("qr_url", list(projeto = p$id))
      if (identical(input$login_pass, "admin") && u$username == "admin")
        showNotification("Você está usando a senha padrão do administrador. Troque-a em Arquivo > Trocar senha.",
                         type = "warning", duration = 12)
    })

    session$onSessionEnded(function() {
      lst <- isolate(online()); lst[[session$token]] <- NULL; isolate(online(lst))
    })
  }
}

# Tudo o que depende do projeto aberto (executado uma vez, após o login)
iniciar_sessao <- function(input, output, session, S, online) {
    con <- S$con
    sair <- function() { lst <- online(); lst[[session$token]] <- NULL; online(lst); session$reload() }
    observeEvent(input$btn_sair, sair())
    observeEvent(input$rb_sair, sair())

    output$online_users <- renderUI({
      u <- S$user(); req(u)
      lst <- Filter(function(x) identical(x$proj, S$projeto), online())
      outros <- Filter(function(x) x$id != u$id, lst)
      outros <- outros[!duplicated(vapply(outros, `[[`, 0, "id"))]
      if (!length(outros)) return(tags$div(class = "online", tags$span(class = "lbl", "Somente você online")))
      tags$div(class = "online", tags$span(class = "lbl", "Online:"),
        lapply(outros, function(x) tags$span(class = "av", style = sprintf("background:%s", x$color %||% "#888"),
                                              title = x$name, toupper(substr(x$name, 1, 1)))))
    })

    # ======================= DADOS (atualização ao vivo) ====================
    S$ver <- reactivePoll(700, session, function() db_versao(con), function() db_versao(con))
    lerdb <- function(f) reactive({ S$ver(); S$refresh(); f(con) })
    S$nodes <- lerdb(db_nodes); S$docs <- lerdb(db_docs); S$cods <- lerdb(db_codings)
    S$users <- lerdb(db_usuarios); S$memos <- lerdb(db_memos); S$attrs <- lerdb(db_atributos)
    S$dattrs <- lerdb(db_doc_atributos); S$squeries <- lerdb(db_consultas_salvas)
    S$anns <- lerdb(db_anotacoes); S$rels <- lerdb(db_relacionamentos); S$reltypes <- lerdb(db_rel_tipos)
    S$sets <- lerdb(db_conjuntos); S$set_items <- lerdb(db_conjunto_itens); S$matrices <- lerdb(db_matrizes)
    S$paths <- reactive(caminhos_nodes(S$nodes()))
    S$me <- function() isolate(S$user())$id
    S$pode_editar <- function() {
      u <- isolate(S$user())
      if (is.null(u) || identical(u$role, "leitor")) {
        showNotification("Seu perfil é somente leitura.", type = "error"); return(FALSE)
      }
      TRUE
    }
    S$eh_admin <- function() identical(isolate(S$user())$role, "admin")
    S$log <- function(acao, detalhe = "") db_log(con, S$me(), acao, detalhe)

    # notificações de atividade de outros membros
    observe({
      S$ver(); u <- S$user(); req(u)
      novos <- DBI::dbGetQuery(con, "SELECT a.*, u.name AS uname FROM activity a LEFT JOIN users u ON u.id=a.user_id
                                     WHERE a.id > ? ORDER BY a.id", params = list(as.integer(isolate(S$last_act()))))
      if (!nrow(novos)) return()
      S$last_act(max(novos$id))
      outros <- novos[novos$user_id != u$id & novos$action != "entrou no projeto", ]
      for (i in utils::tail(seq_len(nrow(outros)), 3))
        showNotification(tags$span(icon("user-pen"), tags$b(outros$uname[i]), outros$action[i],
                                   tags$small(outros$detail[i])), duration = 5)
    })

    # lista de nós para o menu de contexto (JS)
    observe({
      u <- S$user(); req(u)
      nd <- S$nodes(); pt <- S$paths()
      nd <- nd[nd$kind %in% c("code", "case"), ]
      nd <- nd[order(nd$kind != "code", tolower(pt[as.character(nd$id)])), ]
      session$sendCustomMessage("qr_codes", lapply(seq_len(nrow(nd)), function(i)
        list(id = nd$id[i], name = nd$name[i], path = unname(pt[as.character(nd$id[i])]),
             color = nd$color[i] %||% "#999", kind = nd$kind[i])))
    })

    # ======================= ESTADO DA INTERFACE ============================
    S$sec <- reactiveVal("docs")
    S$sel_item <- reactiveVal(NULL)
    S$detail <- reactiveVal(list(type = "none"))
    S$doc_sel <- reactiveVal(NULL)
    S$cur_node <- reactiveVal(NULL)
    S$modal <- reactiveVal(NULL)
    S$n_itens <- reactiveVal(0)

    S$ir_secao <- function(sec) {
      S$sec(sec); S$sel_item(NULL)
      session$sendCustomMessage("qr_nav", list(sec = sec))
    }
    observeEvent(input$nav, { S$sec(input$nav$sec); S$sel_item(NULL) })
    observeEvent(S$user(), session$onFlushed(function() session$sendCustomMessage("qr_nav", list(sec = "docs")), once = TRUE),
                 ignoreNULL = TRUE)

    observeEvent(input$list_sel, {
      it <- input$list_sel
      S$sel_item(list(type = it$type, id = it$id))
      if (it$type %in% c("node", "case")) S$set_node(as.integer(it$id))
    })
    observeEvent(input$tree_sel, S$set_node(as.integer(input$tree_sel$id)))
    S$set_node <- function(id) {
      S$cur_node(id)
      updateSelectizeInput(session, "qc_node", selected = id)
    }

    # ======================= ABAS DO PAINEL DE DETALHE ======================
    S$tabs <- reactiveVal(list())
    S$active_key <- reactiveVal(NULL)
    item_key <- function(d) paste0(d$type, ":", d$id)

    observeEvent(S$detail(), {
      d <- S$detail()
      if (identical(d$type, "none")) {
        tb <- S$tabs(); ak <- S$active_key()
        if (!is.null(ak)) tb[[ak]] <- NULL
        S$tabs(tb)
        if (length(tb)) { nd <- tb[[length(tb)]]; nd$autorun <- FALSE; S$detail(nd) } else S$active_key(NULL)
        return()
      }
      k <- item_key(d); tb <- S$tabs(); tb[[k]] <- d; S$tabs(tb); S$active_key(k)
      if (!is.null(S$user())) try(db_registrar_recente(con, S$me(), d$type, d$id), silent = TRUE)
    }, priority = 10)
    observeEvent(input$tab_sel, {
      d <- S$tabs()[[input$tab_sel$key]]
      if (!is.null(d)) { d$autorun <- FALSE; S$detail(d) }
    })
    observeEvent(input$tab_close, {
      k <- input$tab_close$key
      if (identical(k, S$active_key())) S$detail(list(type = "none"))
      else { tb <- S$tabs(); tb[[k]] <- NULL; S$tabs(tb) }
    })
    observeEvent(input$detail_close, S$detail(list(type = "none")))

    S$titulo_item <- function(d) {
      nm <- switch(d$type,
        doc = S$docs()$name[S$docs()$id == d$id],
        node = S$nodes()$name[S$nodes()$id == d$id],
        memo = S$memos()$title[S$memos()$id == d$id],
        query = paste0(QUERY_TIPOS[[d$id]], if (!is.null(d$saved_name)) paste0(" — ", d$saved_name) else ""),
        set = S$sets()$name[S$sets()$id == d$id],
        matrix = S$matrices()$name[S$matrices()$id == d$id],
        map = MAPAS[[d$id]] %||% "Mapa",
        report = RELATORIOS[[d$id]] %||% "Relatório", "")
      if (!length(nm) || is.na(nm[1])) "(excluído)" else nm[1]
    }
    icone_item <- c(doc = "file-lines", node = "circle", memo = "note-sticky", query = "magnifying-glass",
                    set = "circle-half-stroke", matrix = "table-cells", map = "diagram-project", report = "file-lines")
    output$detail_tabs <- renderUI({
      tb <- S$tabs(); ak <- S$active_key()
      if (!length(tb)) return(NULL)
      div(class = "dtabs",
          lapply(names(tb), function(k) {
            d <- tb[[k]]
            div(class = paste("dtab", if (identical(k, ak)) "active"), `data-key` = k, title = S$titulo_item(d),
                icon(icone_item[[d$type]] %||% "file"), tags$span(S$titulo_item(d)), tags$span(class = "x", "×"))
          }),
          tags$span(class = "dt-max", icon("up-right-and-down-left-from-center"), " Maximizar"))
    })

    S$abrir <- function(type, id) {
      switch(type,
        doc = S$detail(list(type = "doc", id = as.integer(id))),
        node = , case = , result = S$detail(list(type = "node", id = as.integer(id))),
        memo = S$detail(list(type = "memo", id = as.integer(id))),
        query = S$abrir_q(id),
        squery = {
          sq <- S$squeries(); sq <- sq[sq$id == as.integer(id), ]
          if (nrow(sq)) S$abrir_q(sq$qtype, jsonlite::fromJSON(sq$params), TRUE, sq$name)
        },
        set = S$detail(list(type = "set", id = as.integer(id))),
        matrix = S$detail(list(type = "matrix", id = as.integer(id))),
        map = S$detail(list(type = "map", id = id)),
        report = S$detail(list(type = "report", id = id)),
        user = S$modal_usuario(as.integer(id)),
        rel = S$modal_rel(as.integer(id)),
        reltype = S$modal_reltipo(as.integer(id)),
        annot = {
          a <- S$anns(); a <- a[a$id == as.integer(id), ]
          if (nrow(a)) S$ir_para(a$doc_id, a$pos_start, a$pos_end)
        },
        recent = {
          p <- strsplit(id, "|", fixed = TRUE)[[1]]
          if (p[1] == "query") S$abrir_q(p[2]) else S$abrir(p[1], p[2])
        },
        NULL)
    }
    S$ir_para <- function(doc, s, e) {
      S$detail(list(type = "doc", id = as.integer(doc)))
      session$onFlushed(function() session$sendCustomMessage("qr_scroll", list(start = s, end = e)), once = TRUE)
    }
    observeEvent(input$list_open, S$abrir(input$list_open$type, input$list_open$id))
    observeEvent(input$rb_abrir, {
      it <- S$sel_item()
      if (is.null(it)) showNotification("Selecione um item na lista.", type = "warning") else S$abrir(it$type, it$id)
    })
    atualizar <- function() S$refresh(S$refresh() + 1)
    observeEvent(input$rb_atualizar, atualizar())
    observeEvent(input$rb_atualizar2, atualizar())

    # ======================= BARRA DE STATUS ===============================
    output$status_bar <- renderUI({
      u <- S$user(); req(u)
      nd <- S$nodes(); cc <- S$cods()
      tagList(
        tags$span(class = "sb-item", icon("user"), " ", u$name),
        tags$span(class = "sb-item", S$n_itens(), " itens"),
        tags$span(class = "sb-item", nrow(S$docs()), " arquivos"),
        tags$span(class = "sb-item", sum(nd$kind == "code"), " nós"),
        tags$span(class = "sb-item", sum(nd$kind == "case"), " casos"),
        tags$span(class = "sb-item", sum(cc$node_id %in% nd$id[nd$kind == "code"]), " referências"),
        tags$span(class = "sb-item", icon("database"), " ", basename(S$projeto)))
    })

    # demais blocos
    server_lista(input, output, session, S)
    server_detalhe(input, output, session, S)
    server_codificacao(input, output, session, S)
    server_acoes(input, output, session, S)
    server_consultas(input, output, session, S)
    server_extras(input, output, session, S)
    server_autocod(input, output, session, S)
    outputOptions(output, "detail_tabs", suspendWhenHidden = FALSE)
    outputOptions(output, "detail_view", suspendWhenHidden = FALSE)
    # botões de download ficam em abas da faixa que começam ocultas
    for (dl in c("dl_backup", "dl_backup2", "dl_excel", "dl_codebook", "dl_codebook_xlsx", "dl_doc_cod"))
      try(outputOptions(output, dl, suspendWhenHidden = FALSE), silent = TRUE)
}
