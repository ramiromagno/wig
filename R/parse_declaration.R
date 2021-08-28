#' @keywords internal
parse_declaration <- function(x) {

  format <- stringr::str_match(x, pattern = '^(variableStep|fixedStep)')[, 2]
  chrom <- stringr::str_match(x, pattern = 'chrom=(\\w+)')[, 2]
  start <- stringr::str_match(x, pattern = 'start=(\\w+)')[, 2]
  step <- stringr::str_match(x, pattern = 'step=(\\w+)')[, 2]
  span <- stringr::str_match(x, pattern = 'span=(\\w+)')[, 2]

  list(format = format, chrom = chrom, start = as.integer(start), step = as.integer(step), span = as.integer(span))

}
