#' @keywords internal
parse_declaration <- function(x) {

  format <- stringr::str_match(x, pattern = '^(variableStep|fixedStep)')[, 2]
  chrom <- stringr::str_match(x, pattern = 'chrom=(\\S+)')[, 2]
  start <- stringr::str_match(x, pattern = 'start=(\\S+)')[, 2]
  step <- stringr::str_match(x, pattern = 'step=(\\S+)')[, 2]
  span <- stringr::str_match(x, pattern = 'span=(\\S+)')[, 2]

  list(format = format, chrom = chrom, start = as.integer(start), step = as.integer(step), span = as.integer(span))

}
