# ---------------------------------------------------------------------------
# QualiR - consultas e análises
# ---------------------------------------------------------------------------

# Hierarquia --------------------------------------------------------------
descendentes <- function(nodes, id) {
  out <- id; fila <- id
  while (length(fila)) {
    filhos <- nodes$id[!is.na(nodes$parent_id) & nodes$parent_id %in% fila]
    filhos <- setdiff(filhos, out)
    out <- c(out, filhos); fila <- filhos
  }
  out
}

caminho_node <- function(nodes, id, sep = " \\ ") {
  partes <- character(); atual <- id; guard <- 0
  while (!is.na(atual) && guard < 50) {
    r <- nodes[nodes$id == atual, ]
    if (!nrow(r)) break
    partes <- c(r$name[1], partes); atual <- r$parent_id[1]; guard <- guard + 1
  }
  paste(partes, collapse = sep)
}

caminhos_nodes <- function(nodes) {
  if (!nrow(nodes)) return(character())
  setNames(vapply(nodes$id, function(i) caminho_node(nodes, i), ""), nodes$id)
}

# ordena nós em profundidade (pai, depois filhos) e devolve com coluna depth
ordenar_arvore <- function(nodes) {
  if (!nrow(nodes)) return(cbind(nodes, depth = integer()))
  out <- list()
  visita <- function(pid, d) {
    filhos <- if (is.na(pid)) nodes[is.na(nodes$parent_id) | !(nodes$parent_id %in% nodes$id), ]
              else nodes[!is.na(nodes$parent_id) & nodes$parent_id == pid, ]
    filhos <- filhos[order(tolower(filhos$name)), , drop = FALSE]
    for (i in seq_len(nrow(filhos))) {
      out[[length(out) + 1]] <<- cbind(filhos[i, ], depth = d)
      if (d < 30) visita(filhos$id[i], d + 1)
    }
  }
  visita(NA, 0)
  do.call(rbind, out)
}

contagens_nodes <- function(nodes, codings, agregar = FALSE, user_ids = NULL) {
  if (!is.null(user_ids)) codings <- codings[codings$user_id %in% user_ids, ]
  res <- data.frame(node_id = nodes$id, n_refs = 0L, n_docs = 0L)
  for (i in seq_len(nrow(nodes))) {
    ids <- if (agregar) descendentes(nodes, nodes$id[i]) else nodes$id[i]
    cc <- codings[codings$node_id %in% ids, ]
    res$n_refs[i] <- nrow(cc)
    res$n_docs[i] <- length(unique(cc$doc_id))
  }
  res
}

# Texto --------------------------------------------------------------------
trecho <- function(texto, s, e) substr(texto, s + 1, e)

remover_prefixos_turno <- function(texto) {
  linhas <- strsplit(texto, "\n", fixed = TRUE)[[1]]
  sub(RX_TURNO, "\\4", linhas, perl = TRUE) |> paste(collapse = "\n")
}

tokenizar <- function(textos) {
  w <- unlist(strsplit(tolower(textos), "[^\\p{L}\\p{N}'-]+", perl = TRUE), use.names = FALSE)
  w <- gsub("^['-]+|['-]+$", "", w)
  w[nzchar(w) & !grepl("^\\d+$", w)]
}

frequencia_palavras <- function(textos, min_len = 3, stop_extra = character(), top = 100,
                                usar_stopwords = TRUE) {
  w <- tokenizar(textos)
  w <- w[nchar(w) >= min_len]
  sw <- tolower(trimws(stop_extra))
  if (usar_stopwords) sw <- c(sw, STOPWORDS_PT)
  w <- w[!(w %in% sw)]
  if (!length(w)) return(data.frame(Palavra = character(), Contagem = integer(), `%` = numeric(), check.names = FALSE))
  tb <- sort(table(w), decreasing = TRUE)
  tb <- head(tb, top)
  data.frame(Palavra = names(tb), Contagem = as.integer(tb),
             `%` = round(100 * as.integer(tb) / length(w), 2), check.names = FALSE,
             stringsAsFactors = FALSE)
}

# Converte termo de busca em expressão regular
# - "frase exata" entre aspas; * como coringa; termos separados por " OU "
termo_para_regex <- function(termo) {
  partes <- trimws(strsplit(termo, "\\s+(OU|OR|ou)\\s+|\\s*\\|\\s*", perl = TRUE)[[1]])
  partes <- partes[nzchar(partes)]
  rx <- vapply(partes, function(p) {
    p <- gsub('^"|"$', "", p)
    p <- gsub("([.\\\\+?^${}()\\[\\]|])", "\\\\\\1", p, perl = TRUE)
    p <- gsub("\\*", "[\\\\p{L}\\\\p{N}]*", p)
    p <- gsub("\\s+", "\\\\s+", p)
    p
  }, "")
  paste0("(?i)(?<![\\p{L}\\p{N}])(", paste(rx, collapse = "|"), ")(?![\\p{L}\\p{N}])")
}

busca_texto <- function(docs, termo, n_contexto = 8) {
  vazio <- data.frame(doc_id = integer(), Documento = character(), pos_start = integer(),
                      pos_end = integer(), Esquerda = character(), Termo = character(),
                      Direita = character(), stringsAsFactors = FALSE)
  if (!nzchar(trimws(termo)) || !nrow(docs)) return(vazio)
  rx <- termo_para_regex(termo)
  res <- list()
  for (i in seq_len(nrow(docs))) {
    tx <- docs$content[i]
    m <- gregexpr(rx, tx, perl = TRUE)[[1]]
    if (m[1] == -1) next
    len <- attr(m, "match.length")
    for (k in seq_along(m)) {
      s <- m[k]; e <- s + len[k] - 1
      esq <- substr(tx, max(1, s - 250), s - 1)
      dir <- substr(tx, e + 1, e + 250)
      we <- strsplit(gsub("\n", " ¶ ", esq), "\\s+")[[1]]
      wd <- strsplit(gsub("\n", " ¶ ", dir), "\\s+")[[1]]
      res[[length(res) + 1]] <- data.frame(
        doc_id = docs$id[i], Documento = docs$name[i], pos_start = s - 1L, pos_end = e,
        Esquerda = paste(tail(we, n_contexto), collapse = " "),
        Termo = substr(tx, s, e),
        Direita = paste(head(wd[nzchar(wd)], n_contexto), collapse = " "),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(res)) vazio else do.call(rbind, res)
}

# limites do parágrafo que contém uma posição
limites_paragrafo <- function(texto, s, e) {
  pos_nl <- as.integer(gregexpr("\n", texto, fixed = TRUE)[[1]]) - 1L  # 0-based
  pos_nl <- pos_nl[pos_nl >= 0]
  ini <- max(c(-1L, pos_nl[pos_nl < s])) + 1L
  fim_c <- pos_nl[pos_nl >= e]
  fim <- if (length(fim_c)) min(fim_c) else nchar(texto)
  c(ini, fim)
}

# Ajusta seleção removendo espaços nas pontas
aparar_selecao <- function(texto, s, e) {
  s <- max(0L, as.integer(s)); e <- min(nchar(texto), as.integer(e))
  if (e <= s) return(NULL)
  sel <- substr(texto, s + 1, e)
  esq <- nchar(sel) - nchar(sub("^\\s+", "", sel))
  dir <- nchar(sel) - nchar(sub("\\s+$", "", sel))
  s2 <- s + esq; e2 <- e - dir
  if (e2 <= s2) NULL else c(s2, e2)
}

# Sobreposição: codificações (a) x codificações (b), mesmo documento
sobreposicoes <- function(a, b) {
  if (!nrow(a) || !nrow(b)) return(data.frame())
  m <- merge(a, b, by = "doc_id", suffixes = c(".a", ".b"))
  m[m$pos_start.a < m$pos_end.b & m$pos_end.a > m$pos_start.b, , drop = FALSE]
}

# Matriz de codificação --------------------------------------------------
# linhas: nós; colunas: documentos | casos | codificadores | valores de atributo
matriz_codificacao <- function(nodes, codings, docs, users, attrs, row_ids,
                               col_tipo = "docs", atributo = NULL, agregar = TRUE,
                               valor = "refs", dattrs = NULL) {
  linhas <- lapply(row_ids, function(r) {
    ids <- if (agregar) descendentes(nodes, r) else r
    cc <- codings[codings$node_id %in% ids, ]
    cc$row_id <- rep(r, nrow(cc))
    cc
  })
  cc <- do.call(rbind, linhas)
  if (is.null(cc) || !nrow(cc)) cc <- data.frame(id = integer(), doc_id = integer(), node_id = integer(),
                                                   pos_start = integer(), pos_end = integer(),
                                                   user_id = integer(), created = character(), row_id = integer())
  # colunas
  if (col_tipo == "docs") {
    cols <- setNames(docs$id, docs$name)
    cc$col_key <- cc$doc_id
  } else if (col_tipo == "dattr") {
    at <- dattrs[dattrs$attr == atributo, ]
    vals <- sort(unique(at$value[nzchar(at$value)]))
    cols <- setNames(vals, vals)
    cc$col_key <- at$value[match(cc$doc_id, at$doc_id)]
    cc <- cc[!is.na(cc$col_key), ]
  } else if (col_tipo == "users") {
    cols <- setNames(users$id, users$name)
    cc$col_key <- cc$user_id
  } else {
    casos <- nodes[nodes$kind == "case", ]
    cod_casos <- codings[codings$node_id %in% casos$id, c("doc_id", "node_id", "pos_start", "pos_end")]
    names(cod_casos)[2] <- "case_id"
    ov <- sobreposicoes(cc, cod_casos)
    if (col_tipo == "cases") {
      cols <- setNames(casos$id, casos$name)
      cc <- if (nrow(ov)) data.frame(id = ov$id, doc_id = ov$doc_id, row_id = ov$row_id,
                                     col_key = ov$case_id, pos_start = ov$pos_start.a,
                                     pos_end = ov$pos_end.a, user_id = ov$user_id) else cc[0, ]
      if (!nrow(cc)) cc$col_key <- integer()
    } else { # atributo
      at <- attrs[attrs$attr == atributo, ]
      vals <- sort(unique(at$value[nzchar(at$value)]))
      cols <- setNames(vals, vals)
      if (nrow(ov)) {
        ov$col_key <- at$value[match(ov$case_id, at$node_id)]
        cc <- data.frame(id = ov$id, doc_id = ov$doc_id, row_id = ov$row_id, col_key = ov$col_key,
                         pos_start = ov$pos_start.a, pos_end = ov$pos_end.a, user_id = ov$user_id)
        cc <- cc[!is.na(cc$col_key), ]
      } else { cc <- cc[0, ]; cc$col_key <- character() }
    }
  }
  mat <- matrix(0L, nrow = length(row_ids), ncol = length(cols),
                dimnames = list(as.character(row_ids), names(cols)))
  for (i in seq_along(row_ids)) for (j in seq_along(cols)) {
    sub <- cc[cc$row_id == row_ids[i] & cc$col_key == cols[[j]], , drop = FALSE]
    mat[i, j] <- if (valor == "docs") length(unique(sub$doc_id)) else length(unique(sub$id))
  }
  list(mat = mat, cols = cols, detalhes = cc)
}

# Coocorrência ------------------------------------------------------------
coocorrencia <- function(codings, node_ids) {
  cc <- codings[codings$node_id %in% node_ids, c("id", "doc_id", "node_id", "pos_start", "pos_end")]
  k <- length(node_ids)
  mat <- matrix(0L, k, k, dimnames = list(as.character(node_ids), as.character(node_ids)))
  if (nrow(cc) < 2) return(mat)
  ov <- sobreposicoes(cc, cc)
  ov <- ov[ov$node_id.a != ov$node_id.b, ]
  if (!nrow(ov)) return(mat)
  # conta trechos (pares únicos de codificações)
  ov$par <- paste(pmin(ov$id.a, ov$id.b), pmax(ov$id.a, ov$id.b))
  ov <- ov[!duplicated(ov$par), ]
  for (r in seq_len(nrow(ov))) {
    a <- as.character(ov$node_id.a[r]); b <- as.character(ov$node_id.b[r])
    mat[a, b] <- mat[a, b] + 1L; mat[b, a] <- mat[b, a] + 1L
  }
  mat
}

# Comparação de codificação (concordância e Kappa de Cohen, por caractere) --
comparar_codificadores <- function(codings, docs, nodes, user_a, user_b, node_ids, doc_ids) {
  out <- list()
  for (d in doc_ids) {
    L <- docs$n_chars[docs$id == d]
    if (!length(L) || L == 0) next
    for (n in node_ids) {
      ca <- codings[codings$doc_id == d & codings$node_id == n & codings$user_id %in% user_a, ]
      cb <- codings[codings$doc_id == d & codings$node_id == n & codings$user_id %in% user_b, ]
      if (!nrow(ca) && !nrow(cb)) next
      va <- logical(L); vb <- logical(L)
      for (i in seq_len(nrow(ca))) va[(ca$pos_start[i] + 1):ca$pos_end[i]] <- TRUE
      for (i in seq_len(nrow(cb))) vb[(cb$pos_start[i] + 1):cb$pos_end[i]] <- TRUE
      ab <- sum(va & vb); so_a <- sum(va & !vb); so_b <- sum(!va & vb); nenhum <- L - ab - so_a - so_b
      po <- (ab + nenhum) / L
      pa <- sum(va) / L; pb <- sum(vb) / L
      pe <- pa * pb + (1 - pa) * (1 - pb)
      kappa <- if (abs(1 - pe) < 1e-12) 1 else (po - pe) / (1 - pe)
      out[[length(out) + 1]] <- data.frame(
        node_id = n, doc_id = d,
        `Nó` = caminho_node(nodes, n), Documento = docs$name[docs$id == d],
        Kappa = round(kappa, 3), `Concordância (%)` = round(100 * po, 2),
        `A e B (%)` = round(100 * ab / L, 2), `Nem A nem B (%)` = round(100 * nenhum / L, 2),
        `Só A (%)` = round(100 * so_a / L, 2), `Só B (%)` = round(100 * so_b / L, 2),
        n_chars = L, check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

resumo_kappa <- function(cmp) {
  if (!nrow(cmp)) return(data.frame())
  sp <- split(cmp, cmp$`Nó`)
  do.call(rbind, lapply(names(sp), function(nm) {
    x <- sp[[nm]]
    data.frame(`Nó` = nm, Documentos = nrow(x),
               `Kappa médio (ponderado)` = round(weighted.mean(x$Kappa, x$n_chars), 3),
               `Concordância média (%)` = round(weighted.mean(x$`Concordância (%)`, x$n_chars), 2),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

# Exportação ---------------------------------------------------------------
tabela_referencias <- function(con, nodes, codings, docs, users) {
  if (!nrow(codings)) return(data.frame())
  textos <- DBI::dbGetQuery(con, "SELECT id, content FROM documents")
  caminhos <- caminhos_nodes(nodes)
  tx <- textos$content[match(codings$doc_id, textos$id)]
  data.frame(
    Documento = docs$name[match(codings$doc_id, docs$id)],
    Tipo = ifelse(nodes$kind[match(codings$node_id, nodes$id)] == "case", "Caso", "Nó"),
    `Nó` = unname(caminhos[as.character(codings$node_id)]),
    Codificador = users$name[match(codings$user_id, users$id)],
    `Início` = codings$pos_start, Fim = codings$pos_end,
    Trecho = substr(tx, codings$pos_start + 1, codings$pos_end),
    `Data` = codings$created, check.names = FALSE, stringsAsFactors = FALSE)
}

# ===========================================================================
# Operações com intervalos [s, e)
# ===========================================================================
unir_intervalos <- function(df) {
  if (is.null(df) || !nrow(df)) return(data.frame(doc_id = integer(), s = integer(), e = integer()))
  df <- df[order(df$doc_id, df$s, df$e), c("doc_id", "s", "e")]
  out <- list()
  for (d in unique(df$doc_id)) {
    x <- df[df$doc_id == d, ]
    cs <- x$s[1]; ce <- x$e[1]
    if (nrow(x) > 1) for (i in 2:nrow(x)) {
      if (x$s[i] <= ce) ce <- max(ce, x$e[i])
      else { out[[length(out) + 1]] <- c(d, cs, ce); cs <- x$s[i]; ce <- x$e[i] }
    }
    out[[length(out) + 1]] <- c(d, cs, ce)
  }
  m <- do.call(rbind, out)
  data.frame(doc_id = as.integer(m[, 1]), s = as.integer(m[, 2]), e = as.integer(m[, 3]))
}

intersecao_intervalos <- function(a, b) {
  a <- unir_intervalos(a); b <- unir_intervalos(b)
  if (!nrow(a) || !nrow(b)) return(a[0, ])
  m <- merge(a, b, by = "doc_id", suffixes = c(".a", ".b"))
  m$s <- pmax(m$s.a, m$s.b); m$e <- pmin(m$e.a, m$e.b)
  unir_intervalos(m[m$e > m$s, c("doc_id", "s", "e")])
}

diferenca_intervalos <- function(a, b) {
  a <- unir_intervalos(a); b <- unir_intervalos(b)
  if (!nrow(a) || !nrow(b)) return(a)
  out <- list()
  for (i in seq_len(nrow(a))) {
    pecas <- data.frame(s = a$s[i], e = a$e[i])
    bb <- b[b$doc_id == a$doc_id[i] & b$s < a$e[i] & b$e > a$s[i], ]
    for (j in seq_len(nrow(bb))) {
      nov <- list()
      for (k in seq_len(nrow(pecas))) {
        ps <- pecas$s[k]; pe <- pecas$e[k]
        if (bb$e[j] <= ps || bb$s[j] >= pe) { nov[[length(nov) + 1]] <- c(ps, pe); next }
        if (bb$s[j] > ps) nov[[length(nov) + 1]] <- c(ps, bb$s[j])
        if (bb$e[j] < pe) nov[[length(nov) + 1]] <- c(bb$e[j], pe)
      }
      pecas <- if (length(nov)) setNames(as.data.frame(do.call(rbind, nov)), c("s", "e")) else data.frame(s = integer(), e = integer())
    }
    if (nrow(pecas)) out[[length(out) + 1]] <- data.frame(doc_id = a$doc_id[i], s = pecas$s, e = pecas$e)
  }
  if (!length(out)) a[0, ] else unir_intervalos(do.call(rbind, out))
}

intervalos_de <- function(codings, ids, user_ids = NULL) {
  cc <- codings[codings$node_id %in% ids, ]
  if (length(user_ids)) cc <- cc[cc$user_id %in% user_ids, ]
  unir_intervalos(data.frame(doc_id = cc$doc_id, s = cc$pos_start, e = cc$pos_end))
}

# Consulta de codificação combinada --------------------------------------
# op: "so" (apenas A), "e" (A e B), "ou" (A ou B), "nao" (A mas não B)
# casos_ids: restringe aos trechos de falas desses casos (ex.: casos com Gênero = Feminino)
consulta_codificacao <- function(nodes, codings, a, op = "so", b = NULL, agregar = TRUE,
                                 user_ids = NULL, casos_ids = NULL, doc_ids = NULL) {
  ids <- function(x) if (agregar) descendentes(nodes, x) else x
  A <- intervalos_de(codings, ids(a), user_ids)
  res <- if (op == "so" || is.null(b) || is.na(b)) A else {
    B <- intervalos_de(codings, ids(b), user_ids)
    switch(op, e = intersecao_intervalos(A, B), ou = unir_intervalos(rbind(A, B)),
           nao = diferenca_intervalos(A, B), A)
  }
  if (!is.null(casos_ids)) res <- intersecao_intervalos(res, intervalos_de(codings, casos_ids))
  if (length(doc_ids)) res <- res[res$doc_id %in% doc_ids, ]
  res
}

# ===========================================================================
# Árvore de palavras
# ===========================================================================
palavras_contexto <- function(txt, lado, n) {
  txt <- gsub("\n", " ¶ ", txt)
  w <- strsplit(trimws(txt), "\\s+")[[1]]
  w <- w[nzchar(w) & !grepl("^\\[?\\d{1,2}:\\d{2}(:\\d{2})?([.,]\\d+)?\\]?$", w)]
  if (lado == "esq") {
    w <- rev(w)
    corte <- which(w == "¶" | grepl("[.!?:]$", w))[1]
    if (!is.na(corte)) w <- w[seq_len(corte - 1)]
  } else {
    corte <- which(w == "¶")[1]
    if (!is.na(corte)) w <- w[seq_len(corte - 1)]
    fim <- which(grepl("[.!?]$", w))[1]
    if (!is.na(fim)) w <- w[seq_len(fim)]
  }
  head(w, n)
}

arvore_dados <- function(docs, termo, n = 6) {
  if (!nzchar(trimws(termo))) return(NULL)
  rx <- termo_para_regex(termo)
  esq <- dir <- list(); termos <- character()
  for (i in seq_len(nrow(docs))) {
    tx <- docs$content[i]
    m <- gregexpr(rx, tx, perl = TRUE)[[1]]
    if (m[1] == -1) next
    len <- attr(m, "match.length")
    for (k in seq_along(m)) {
      s <- m[k]; e <- s + len[k] - 1
      esq[[length(esq) + 1]] <- palavras_contexto(substr(tx, max(1, s - 400), s - 1), "esq", n)
      dir[[length(dir) + 1]] <- palavras_contexto(substr(tx, e + 1, e + 400), "dir", n)
      termos <- c(termos, substr(tx, s, e))
    }
  }
  if (!length(termos)) return(NULL)
  list(termo = names(sort(table(tolower(termos)), decreasing = TRUE))[1], esq = esq, dir = dir, n = length(termos))
}

chave_palavra <- function(w) tolower(gsub("^[[:punct:]“”«»]+|[[:punct:]“”«»]+$", "", w))

render_ramos <- function(seqs, total, lado, nivel = 1, max_filhos = 14) {
  seqs <- seqs[lengths(seqs) > 0]
  if (!length(seqs)) return("")
  chaves <- vapply(seqs, function(s) chave_palavra(s[1]), "")
  grupos <- split(seq_along(seqs), chaves)
  grupos <- grupos[order(-lengths(grupos))]
  extra <- 0
  if (length(grupos) > max_filhos) { extra <- sum(lengths(grupos[-(1:max_filhos)])); grupos <- grupos[1:max_filhos] }
  fs <- function(n) sprintf("font-size:%.1fpx", 12 + 14 * sqrt(n / total))
  itens <- vapply(grupos, function(g) {
    n <- length(g)
    if (n == 1) {
      frase <- seqs[[g]]
      if (lado == "esq") frase <- rev(frase)
      return(sprintf('<div class="wt-n"><span class="wt-w leaf" style="%s">%s</span></div>', fs(1),
                     esc(paste(frase, collapse = " "))))
    }
    palavra <- seqs[[g[1]]][1]
    resto <- lapply(seqs[g], function(s) s[-1])
    sub <- if (nivel < 8) render_ramos(resto, total, lado, nivel + 1, max_filhos) else ""
    sprintf('<div class="wt-n"><span class="wt-w" style="%s" title="%d ocorrências">%s</span>%s</div>',
            fs(n), n, esc(palavra), if (nzchar(sub)) sprintf('<div class="wt-c">%s</div>', sub) else "")
  }, "")
  if (extra) itens <- c(itens, sprintf('<div class="wt-n"><span class="wt-w more">+%d outras</span></div>', extra))
  paste(itens, collapse = "")
}

render_arvore_palavras <- function(dados) {
  if (is.null(dados)) return('<div class="list-empty">Termo não encontrado.</div>')
  sprintf('<div class="wtree"><div class="wt-side wt-left"><div class="wt-c">%s</div></div><div class="wt-root">%s<small>%d ocorrência(s)</small></div><div class="wt-side wt-right"><div class="wt-c">%s</div></div></div>',
          render_ramos(dados$esq, dados$n, "esq"), esc(dados$termo), dados$n, render_ramos(dados$dir, dados$n, "dir"))
}

# ===========================================================================
# Agrupamento (cluster)
# ===========================================================================
vetor_palavras <- function(textos, nomes, max_vocab = 600) {
  toks <- lapply(textos, function(t) { w <- tokenizar(t); w[nchar(w) >= 3 & !(w %in% STOPWORDS_PT)] })
  vocab <- head(names(sort(table(unlist(toks)), decreasing = TRUE)), max_vocab)
  m <- t(vapply(toks, function(w) as.numeric(table(factor(w, levels = vocab))), numeric(length(vocab))))
  if (length(vocab) == 1) m <- matrix(m, ncol = 1)
  rownames(m) <- nomes
  m
}

cosseno <- function(m) {
  nr <- sqrt(rowSums(m^2)); nr[nr == 0] <- 1
  mm <- m / nr
  s <- mm %*% t(mm)
  s[s > 1] <- 1
  s
}

jaccard <- function(m) {
  m <- (m > 0) * 1
  inter <- m %*% t(m)
  tot <- rowSums(m)
  uni <- outer(tot, tot, "+") - inter
  s <- ifelse(uni == 0, 0, inter / uni)
  diag(s) <- 1
  s
}

# itens: "codes" ou "docs"; base: "palavras" ou "codificacao"
similaridade <- function(con, nodes, codings, docs, itens, base, ids) {
  textos_all <- DBI::dbGetQuery(con, "SELECT id, content FROM documents")
  pt <- caminhos_nodes(nodes)
  if (itens == "docs") {
    nomes <- docs$name[match(ids, docs$id)]
    if (base == "palavras") {
      m <- vetor_palavras(textos_all$content[match(ids, textos_all$id)], nomes)
    } else {
      cods <- nodes$id[nodes$kind == "code"]
      m <- t(vapply(ids, function(d) vapply(cods, function(n) sum(codings$doc_id == d & codings$node_id == n), 0), numeric(length(cods))))
      if (length(cods) == 1) m <- matrix(m, ncol = 1)
      rownames(m) <- nomes
    }
    return(list(sim = cosseno(m), metodo = if (base == "palavras") "cosseno (vocabulário)" else "cosseno (referências por nó)"))
  }
  nomes <- unname(pt[as.character(ids)])
  if (base == "palavras") {
    tx <- vapply(ids, function(n) {
      cc <- codings[codings$node_id == n, ]
      paste(substr(textos_all$content[match(cc$doc_id, textos_all$id)], cc$pos_start + 1, cc$pos_end), collapse = " ")
    }, "")
    return(list(sim = cosseno(vetor_palavras(tx, nomes)), metodo = "cosseno (vocabulário dos trechos)"))
  }
  # codificação: presença do nó em cada parágrafo de cada arquivo -> Jaccard
  unid <- list()
  for (i in seq_len(nrow(textos_all))) {
    tx <- textos_all$content[i]
    nl <- as.integer(gregexpr("\n", tx, fixed = TRUE)[[1]]); nl <- nl[nl > 0] - 1L
    ini <- c(0L, nl + 1L); fim <- c(nl, nchar(tx))
    unid[[i]] <- data.frame(doc_id = textos_all$id[i], s = ini, e = fim)
  }
  unid <- do.call(rbind, unid); unid <- unid[unid$e > unid$s, ]
  m <- t(vapply(ids, function(n) {
    cc <- codings[codings$node_id == n, ]
    v <- numeric(nrow(unid))
    for (k in seq_len(nrow(cc))) v[unid$doc_id == cc$doc_id[k] & unid$s < cc$pos_end[k] & unid$e > cc$pos_start[k]] <- 1
    v
  }, numeric(nrow(unid))))
  rownames(m) <- nomes
  list(sim = jaccard(m), metodo = "Jaccard (parágrafos codificados em comum)")
}

# ===========================================================================
# Gráfico hierárquico (treemap) de nós
# ===========================================================================
desenhar_treemap <- function(nodes, contagem, titulo = "") {
  # contagem: named numeric (id -> valor agregado)
  cod <- nodes[nodes$kind == "code", ]
  val <- function(id) contagem[as.character(id)] %||% 0
  filhos <- function(pid) {
    f <- if (is.na(pid)) cod[is.na(cod$parent_id) | !(cod$parent_id %in% cod$id), ] else cod[cod$parent_id %in% pid, ]
    f <- f[vapply(f$id, val, 0) > 0, , drop = FALSE]
    f[order(-vapply(f$id, val, 0)), , drop = FALSE]
  }
  op <- par(mar = c(0, 0, if (nzchar(titulo)) 1.6 else 0, 0)); on.exit(par(op))
  plot.new(); plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  if (nzchar(titulo)) title(titulo, cex.main = 0.95, font.main = 1)
  raiz <- filhos(NA)
  if (!nrow(raiz)) { text(0.5, 0.5, "Nenhum nó com referências", col = "#6B7280"); return(invisible()) }
  desenha <- function(f, x0, y0, x1, y1, d) {
    vals <- vapply(f$id, val, 0)
    R <- squarify(vals, x0, y0, x1 - x0, y1 - y0)
    for (i in seq_len(nrow(f))) {
      rx <- R[i, ]; v <- vals[i]
      cor <- f$color[i] %||% "#999999"
      rect(rx[1], rx[2], rx[3], rx[4], col = adjustcolor(cor, alpha.f = max(0.35, 0.85 - 0.2 * d)), border = "white", lwd = 2)
      w <- rx[3] - rx[1]; h <- rx[4] - rx[2]
      sub <- filhos(f$id[i])
      rot <- sprintf("%s (%d)", f$name[i], as.integer(v))
      cx <- min(1.1, max(0.6, sqrt(w * h) * 3))
      if (nrow(sub) && h > 0.1 && w > 0.08) {
        text(rx[1] + 0.006, rx[4] - 0.012, rot, adj = c(0, 1), cex = cx * 0.85, font = 2, col = "#1E1E1E")
        desenha(sub, rx[1] + 0.006, rx[2] + 0.006, rx[3] - 0.006, rx[4] - 0.05, d + 1)
      } else if (w > 0.03 && h > 0.025) {
        lab <- if (strwidth(rot, cex = cx) < w * 0.95) rot else if (strwidth(f$name[i], cex = cx) < w * 0.95) f$name[i] else trunca(f$name[i], max(3, floor(w * 60)))
        text((rx[1] + rx[3]) / 2, (rx[2] + rx[4]) / 2, lab, cex = cx, col = "#1E1E1E")
      }
    }
  }
  desenha(raiz, 0, 0, 1, 1, 0)
}


# Algoritmo "squarified treemap" (Bruls et al.): devolve matriz x0, y0, x1, y1 na ordem dos valores
squarify <- function(vals, x, y, w, h) {
  n <- length(vals); out <- matrix(0, n, 4)
  if (!n || sum(vals) <= 0) return(out)
  ord <- order(-vals); areas <- vals[ord] / sum(vals) * w * h
  pior <- function(r, lado) { s <- sum(r); max(lado^2 * max(r) / s^2, s^2 / (lado^2 * min(r))) }
  i <- 1; res <- matrix(0, n, 4)
  while (i <= n) {
    lado <- min(w, h); fila <- areas[i]; j <- i + 1
    while (j <= n && pior(c(fila, areas[j]), lado) <= pior(fila, lado)) { fila <- c(fila, areas[j]); j <- j + 1 }
    s <- sum(fila); k <- i:(j - 1)
    if (w >= h) {
      rw <- s / h; yy <- y + h
      for (m in seq_along(fila)) { rh <- fila[m] / rw; res[k[m], ] <- c(x, yy - rh, x + rw, yy); yy <- yy - rh }
      x <- x + rw; w <- w - rw
    } else {
      rh <- s / w; xx <- x
      for (m in seq_along(fila)) { rw <- fila[m] / rh; res[k[m], ] <- c(xx, y + h - rh, xx + rw, y + h); xx <- xx + rw }
      h <- h - rh
    }
    i <- j
  }
  out[ord, ] <- res
  out
}
