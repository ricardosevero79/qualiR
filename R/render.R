# ---------------------------------------------------------------------------
# QualiR - montagem de HTML (documento com faixas de codificação, árvores, listas)
# ---------------------------------------------------------------------------

# Documento ----------------------------------------------------------------
# codings: data.frame com id, node_id, pos_start, pos_end, user_id, color, label
# faixas: data.frame(key, label, color) - colunas das faixas de codificação
render_documento <- function(texto, codings, faixas, faixa_col = "node_id",
                             destacar = TRUE, transcricao = FALSE, anotacoes = NULL) {
  if (is.null(anotacoes)) anotacoes <- data.frame(id = integer(), pos_start = integer(), pos_end = integer(), num = integer())
  paras <- strsplit(texto, "\n", fixed = TRUE)[[1]]
  if (!length(paras)) paras <- ""
  if (endsWith(texto, "\n")) paras <- c(paras, "")
  lens <- nchar(paras)
  starts <- c(0L, cumsum(lens + 1L))[seq_along(paras)]

  turnos <- if (transcricao) turnos_de_fala(texto) else data.frame()
  pref_fim <- rep(NA_integer_, length(paras))
  if (nrow(turnos)) pref_fim[turnos$linha] <- turnos$fala_start

  nf <- nrow(faixas)
  cs <- codings$pos_start; ce <- codings$pos_end
  # destacar: TRUE (todos), FALSE (nenhum) ou vetor de ids de codificações a destacar
  hl_ok <- if (isTRUE(destacar)) rep(TRUE, nrow(codings)) else if (isFALSE(destacar)) rep(FALSE, nrow(codings))
           else codings$id %in% destacar
  destacar <- any(hl_ok)
  linhas <- character(length(paras))

  for (i in seq_along(paras)) {
    ps <- starts[i]; pe <- ps + lens[i]; p <- paras[i]
    hit <- which(cs < max(pe, ps + 1L) & ce > ps)
    # --- texto com destaques
    hh <- hit[hl_ok[hit]]
    bnd <- c(ps, pe)
    if (destacar && length(hh)) bnd <- c(bnd, pmax(cs[hh], ps), pmin(ce[hh], pe))
    if (!is.na(pref_fim[i])) bnd <- c(bnd, pref_fim[i])
    ahit <- which(anotacoes$pos_start < pe & anotacoes$pos_end > ps)
    if (length(ahit)) bnd <- c(bnd, pmax(anotacoes$pos_start[ahit], ps), pmin(anotacoes$pos_end[ahit], pe))
    bnd <- sort(unique(bnd[bnd >= ps & bnd <= pe]))
    pecas <- character()
    if (length(bnd) >= 2) for (k in seq_len(length(bnd) - 1)) {
      a <- bnd[k]; b <- bnd[k + 1]
      if (b <= a) next
      tx <- esc(substr(p, a - ps + 1, b - ps))
      cls <- character(); st <- ""; tt <- ""
      if (!is.na(pref_fim[i]) && b <= pref_fim[i]) cls <- c(cls, "spk")
      an <- ahit[anotacoes$pos_start[ahit] <= a & anotacoes$pos_end[ahit] >= b]
      extra_attr <- ""
      if (length(an)) {
        cls <- c(cls, "ann")
        fim <- an[anotacoes$pos_end[an] == b]
        if (length(fim)) { cls <- c(cls, "annend"); extra_attr <- sprintf(' data-n="%s"', paste(anotacoes$num[fim], collapse = ",")) }
      }
      if (destacar && length(hh)) {
        cob <- hh[cs[hh] <= a & ce[hh] >= b]
        if (length(cob)) {
          cls <- c(cls, "cod", if (length(cob) > 1) "multi")
          st <- sprintf(' style="background:%s"', mistura_rgba(codings$color[cob], if (length(cob) > 1) 0.45 else 0.33))
          tt <- sprintf(' title="%s"', esc(paste(unique(codings$label[cob]), collapse = "\n")))
        }
      }
      pecas <- c(pecas, if (length(cls) || nzchar(st))
        sprintf('<span class="%s"%s%s%s>%s</span>', paste(cls, collapse = " "), st, tt, extra_attr, tx) else tx)
    }
    # --- faixas
    fx <- ""
    if (nf) {
      cel <- character(nf)
      for (j in seq_len(nf)) {
        on <- length(hit) && any(codings[[faixa_col]][hit] == faixas$key[j])
        cel[j] <- if (on) sprintf('<i class="sc on" style="background:%s" title="%s" data-key="%s"></i>',
                                  faixas$color[j], esc(faixas$label[j]), faixas$key[j])
                  else '<i class="sc"></i>'
      }
      fx <- paste0('<div class="stripes">', paste(cel, collapse = ""), '</div>')
    }
    linhas[i] <- sprintf(
      '<div class="prow"><div class="pnum">%d</div><div class="para%s" data-start="%d" data-len="%d">%s</div>%s</div>',
      i, if (lens[i] == 0) " empty" else "", ps, lens[i], paste(pecas, collapse = ""), fx)
  }

  cab <- ""
  if (nf) {
    cab <- paste0('<div class="stripe-head"><div class="sh-spacer">Faixas de codificação</div><div class="stripes">',
                  paste(sprintf('<i class="sh" title="%s"><span style="--c:%s">%s</span></i>',
                                esc(faixas$label), faixas$color, esc(trunca(faixas$label, 22))), collapse = ""),
                  '</div></div>')
  }
  paste0(cab, '<div id="doc-text" class="doc-text">', paste(linhas, collapse = ""), '</div>')
}

# Árvore de nós (painel lateral) -------------------------------------------
render_arvore <- function(nodes, contagens, kind = "code", classe = "qtree") {
  nn <- nodes[nodes$kind == kind, , drop = FALSE]
  if (!nrow(nn)) return(sprintf('<div class="%s empty-tree">Nenhum %s ainda.</div>', classe,
                                if (kind == "code") "nó" else "caso"))
  filhos <- function(pid) {
    f <- if (is.na(pid)) nn[is.na(nn$parent_id) | !(nn$parent_id %in% nn$id), ] else nn[nn$parent_id %in% pid, ]
    f[order(tolower(f$name)), , drop = FALSE]
  }
  build <- function(pid, d) {
    f <- filhos(pid)
    if (!nrow(f) || d > 30) return("")
    itens <- vapply(seq_len(nrow(f)), function(i) {
      sub <- build(f$id[i], d + 1)
      n <- contagens$n_refs[match(f$id[i], contagens$node_id)] %||% 0
      sprintf('<li data-id="%d"><div class="qnode" data-id="%d" draggable="true" title="%s"><span class="caret%s"></span><span class="dot" style="background:%s"></span><span class="nm">%s</span><span class="cnt">%s</span></div>%s</li>',
              f$id[i], f$id[i], esc(f$description[i] %||% ""), if (nzchar(sub)) "" else " leaf",
              f$color[i] %||% "#999", esc(f$name[i]), n, sub)
    }, "")
    paste0("<ul>", paste(itens, collapse = ""), "</ul>")
  }
  sprintf('<div class="%s" data-kind="%s">%s</div>', classe, kind, build(NA, 0))
}

# Tabela de lista -------------------------------------------
# df: colunas a exibir; ids; tipo; depth/has_children opcionais para hierarquia
render_lista <- function(df, ids, tipo, depth = NULL, parents = NULL, cores = NULL,
                         icone = "file-lines", selecionado = NULL, arrastavel = FALSE, circulo = FALSE) {
  if (circulo && !is.null(cores) && nrow(df)) df[["Cor"]] <- cores
  if (!nrow(df)) return('<div class="list-empty">Nenhum item. Use a faixa de opções acima para criar ou importar.</div>')
  cab <- paste0("<tr>", paste(sprintf('<th data-col="%d">%s</th>', seq_along(df) - 1,
                                      ifelse(names(df) == "Cor" & circulo, '<i class="fa-solid fa-palette"></i>', esc(names(df)))), collapse = ""), "</tr>")
  tem_filhos <- if (!is.null(parents)) ids %in% parents else rep(FALSE, length(ids))
  tp <- rep_len(tipo, nrow(df))
  icv <- rep_len(icone, nrow(df))
  linhas <- vapply(seq_len(nrow(df)), function(i) {
    cels <- vapply(seq_along(df), function(j) {
      v <- df[[j]][i]; v <- if (is.na(v)) "" else as.character(v)
      if (j == 1) {
        ind <- if (!is.null(depth)) depth[i] * 18 else 0
        car <- if (!is.null(depth)) sprintf('<span class="caret%s"></span>', if (tem_filhos[i]) "" else " leaf") else ""
        ico <- if (circulo) '<span class="ncirc"></span>'
               else if (!is.null(cores)) sprintf('<span class="dot" style="background:%s"></span>', cores[i])
               else sprintf('<i class="fa-solid fa-%s licon"></i>', icv[i])
        sprintf('<td><div class="namecell" style="padding-left:%dpx">%s%s<span>%s</span></div></td>', ind, car, ico, esc(v))
      } else if (circulo && names(df)[j] == "Cor") sprintf('<td class="ccol"><span class="cdot" style="background:%s"></span></td>', esc(v))
      else sprintf("<td>%s</td>", esc(v))
    }, "")
    sprintf('<tr class="qrow%s" data-type="%s" data-id="%s" data-depth="%s" data-parent="%s"%s>%s</tr>',
            if (!is.null(selecionado) && identical(as.character(selecionado), as.character(ids[i]))) " sel" else "",
            tp[i], ids[i], if (is.null(depth)) 0 else depth[i],
            if (is.null(parents)) "" else (parents[i] %||% ""),
            if (arrastavel) ' draggable="true"' else "", paste(cels, collapse = ""))
  }, "")
  sprintf('<table class="qlist" data-type="%s"><thead>%s</thead><tbody>%s</tbody></table>', tipo[1], cab,
          paste(linhas, collapse = ""))
}
