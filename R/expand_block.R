#' @keywords internal
expand_block_variable_step <- function(chr_pos, value, chrom, span = 1) {

  n <- length(chr_pos)
  chr_pos2 <- sequence(nvec = rep(span, n), from = chr_pos)
  value2 <- rep(value, each = span)

  tibble::tibble(chr = chrom, pos = chr_pos2, val = value2)

}

#' @keywords internal
expand_block_fixed_step <- function(value, chrom, start, step, span = 1) {

  n <- length(value)
  chr_pos <- seq(start, by = step, length.out = n)
  expand_block_variable_step(chr_pos = chr_pos, value = value, chrom = chrom, span = span)

}

