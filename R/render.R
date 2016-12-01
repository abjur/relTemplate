#' Renderiza doc da ABJ.
#'
#' Renderiza doc da ABJ usando \code{\link{pdf_document2}}.
#'
#' @param arq nome do arquivo.
#' @param toc table of contents.
#'
#' @export
render_abj_pdf <- function(arq, toc = FALSE) {
  if (missing(arq)) arq <- sort(dir('./', pattern = '\\.[rR]md$')[1])
  arq <- normalizePath(arq)
  s1 <- system.file('style.sty', package = 'relTemplate')
  s2 <- system.file('body.tex', package = 'relTemplate')
  s3 <- system.file('logo.png', package = 'relTemplate')
  file.copy(c(s1, s2, s3), dirname(arq))
  fmt <- bookdown::pdf_document2(
    number_sections = TRUE,
    toc = toc,
    includes = list(
      in_header = s1,
      before_body = system.file('body.tex', package = 'relTemplate')
    )
  )
  rmarkdown::render(arq, output_format = fmt, output_dir = dirname(arq),
                    quiet = TRUE)
  r <- file.remove(c('style.sty', 'body.tex', 'logo.png'))
  file.show(gsub('\\.[rR]md$', '.pdf', arq))
}

#' Cria novo relatório com base em template.
#'
#' Cria novo relatório com base em template da ABJ, com opcoes de tipo de doc.
#'
#' @param tipo por enquanto só pdf. Futuramente word e html?
#' @param nm nome do arquivo.
#' @param path caminho (pasta) do arquivo.
#'
#' @export
abj_new <- function(tipo = 'pdf', nm = 'relatorio.Rmd', path = './') {
  arq <- paste0(path, nm)
  template <- system.file('abj_pdf.Rmd', package = 'relTemplate')
  file.copy(template, arq)
  file.edit(arq)
}
