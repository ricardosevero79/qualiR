# ---------------------------------------------------------------------------
# QualiR - importação de arquivos e tratamento de transcrições
# Formatos: .txt .md .docx .pdf .odt(não) .vtt (Teams/Zoom/Whisper) .srt
# ---------------------------------------------------------------------------

ler_texto_bruto <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  # remove BOM UTF-8
  if (length(raw) >= 3 && all(raw[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
  raw <- raw[raw != as.raw(0)]
  s <- rawToChar(raw)
  Encoding(s) <- "UTF-8"
  if (!validUTF8(s)) s <- iconv(s, "latin1", "UTF-8")
  s
}

ler_docx <- function(path) {
  d <- officer::docx_summary(officer::read_docx(path))
  d <- d[d$content_type %in% c("paragraph", "table cell"), , drop = FALSE]
  if (!nrow(d)) return("")
  d$text[is.na(d$text)] <- ""
  par <- d[d$content_type == "paragraph", c("doc_index", "text")]
  par$ord <- 0
  tab <- d[d$content_type == "table cell", , drop = FALSE]
  if (nrow(tab)) {
    linhas <- aggregate(text ~ doc_index + row_id, data = tab,
                        FUN = function(x) paste(x[nzchar(x)], collapse = " | "))
    linhas$ord <- linhas$row_id
    par <- rbind(par, linhas[, c("doc_index", "text", "ord")])
  }
  par <- par[order(par$doc_index, par$ord), ]
  paste(par$text, collapse = "\n")
}

ler_pdf <- function(path) {
  pg <- pdftools::pdf_text(path)
  # junta linhas quebradas dentro de parágrafos
  pg <- gsub("[ \t]+\n", "\n", pg)
  paste(pg, collapse = "\n\n")
}

# hh:mm:ss.mmm -> [hh:mm:ss]
fmt_tempo <- function(x) {
  x <- sub("[.,]\\d+$", "", trimws(x))
  if (grepl("^\\d{1,2}:\\d{2}$", x)) x <- paste0("00:", x)
  x
}

# Junta falas consecutivas do mesmo falante
juntar_falas <- function(tempo, falante, texto) {
  if (!length(texto)) return("")
  out <- character(); i <- 1
  while (i <= length(texto)) {
    j <- i
    while (j < length(texto) && identical(falante[j + 1], falante[i]) && nzchar(falante[i])) j <- j + 1
    fala <- paste(texto[i:j], collapse = " ")
    pref <- paste0("[", tempo[i], "] ")
    out <- c(out, if (nzchar(falante[i])) paste0(pref, falante[i], ": ", fala) else paste0(pref, fala))
    i <- j + 1
  }
  paste(out, collapse = "\n")
}

parse_legendas <- function(txt) {
  linhas <- strsplit(gsub("\r", "", txt), "\n", fixed = TRUE)[[1]]
  idx <- grep("-->", linhas, fixed = TRUE)
  if (!length(idx)) return(txt)
  tempo <- falante <- texto <- character()
  for (k in seq_along(idx)) {
    ini <- idx[k] + 1
    fim <- if (k < length(idx)) idx[k + 1] - 1 else length(linhas)
    bloco <- linhas[ini:fim]
    bloco <- bloco[nzchar(trimws(bloco))]
    # descarta o número do próximo bloco (SRT) ou identificadores
    if (k < length(idx) && length(bloco) && grepl("^\\s*\\d+\\s*$|^[0-9a-f-]{8,}(/\\d+-\\d+)?$", bloco[length(bloco)]))
      bloco <- bloco[-length(bloco)]
    if (!length(bloco)) next
    t0 <- fmt_tempo(strsplit(linhas[idx[k]], "-->", fixed = TRUE)[[1]][1])
    b <- paste(bloco, collapse = " ")
    spk <- ""
    m <- regmatches(b, regexec("<v\\s+([^>]+)>", b))[[1]]
    if (length(m) == 2) spk <- trimws(m[2])
    b <- trimws(gsub("<[^>]+>", "", b))
    if (!nzchar(spk)) {
      m2 <- regmatches(b, regexec("^([^:]{1,40}):\\s+(.*)$", b))[[1]]
      if (length(m2) == 3) { spk <- trimws(m2[2]); b <- m2[3] }
    }
    tempo <- c(tempo, t0); falante <- c(falante, spk); texto <- c(texto, b)
  }
  juntar_falas(tempo, falante, texto)
}

normalizar_texto <- function(txt) {
  txt <- gsub("\r\n?", "\n", txt)
  txt <- gsub("[​‌‍﻿]", "", txt)
  txt <- gsub(" ", " ", txt)
  txt <- gsub("[ \t]+\n", "\n", txt)
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- sub("^\n+", "", txt)
  sub("\\s+$", "", txt)
}

ler_arquivo <- function(path, nome) {
  ext <- tolower(tools::file_ext(nome))
  txt <- switch(ext,
    txt = , md = , text = ler_texto_bruto(path),
    docx = ler_docx(path),
    pdf  = ler_pdf(path),
    vtt  = , srt = parse_legendas(ler_texto_bruto(path)),
    stop("Formato não suportado: .", ext)
  )
  list(texto = normalizar_texto(txt), tipo = ext)
}

# ---------------------------------------------------------------------------
# Turnos de fala. Reconhece linhas como:
#   Entrevistador: texto          [00:01:23] Maria: texto
#   00:01:23 P1: texto            E: texto      ENTREVISTADA (Ana): texto
# Devolve data.frame com posições absolutas (0-based) no texto completo.
# ---------------------------------------------------------------------------
RX_TURNO <- "^(\\s*(?:\\[?\\(?(\\d{1,2}:\\d{2}(?::\\d{2})?(?:[.,]\\d+)?)\\)?\\]?\\s*[-–]?\\s*)?([^:\\[\\]\n]{1,40}?)\\s*:\\s+)(\\S.*)$"

turnos_de_fala <- function(texto) {
  linhas <- strsplit(texto, "\n", fixed = TRUE)[[1]]
  if (!length(linhas)) return(data.frame())
  ini <- c(0, cumsum(nchar(linhas) + 1))[seq_along(linhas)]
  m <- regexec(RX_TURNO, linhas, perl = TRUE)
  mm <- regmatches(linhas, m)
  ok <- lengths(mm) == 5
  if (!any(ok)) return(data.frame())
  falante <- vapply(mm[ok], `[`, "", 4)
  prefixo <- vapply(mm[ok], `[`, "", 2)
  tempo <- vapply(mm[ok], `[`, "", 3)
  df <- data.frame(linha = which(ok), falante = trimws(falante), tempo = tempo,
                   pref_len = nchar(prefixo), stringsAsFactors = FALSE)
  df$line_start <- ini[df$linha]
  df$fala_start <- df$line_start + df$pref_len
  df$fala_end <- ini[df$linha] + nchar(linhas[df$linha])
  # falante plausível: até 5 palavras, aparece 2+ vezes
  df <- df[n_palavras(df$falante) <= 5 & nzchar(df$falante), , drop = FALSE]
  freq <- table(df$falante)
  df <- df[df$falante %in% names(freq)[freq >= 2], , drop = FALSE]
  df
}

eh_transcricao <- function(texto) {
  linhas <- strsplit(texto, "\n", fixed = TRUE)[[1]]
  linhas <- linhas[nzchar(trimws(linhas))]
  if (length(linhas) < 3) return(FALSE)
  t <- turnos_de_fala(texto)
  nrow(t) >= 3 && nrow(t) / length(linhas) >= 0.3 && length(unique(t$falante)) <= 25
}
