#' Imports a WIG file
#'
#' `import_wig` reads a [WIG
#' (wiggle)](https://m.ensembl.org/info/website/upload/wig.html) file and
#' expands the data into long format, i.e., the each observation in the returned
#' tibble pertains the position of one single base.
#'
#' @param file_path A path to a WIG file.
#' @param n The (maximal) number of lines to read. Negative values indicate that
#'   one should read up to the end of input on the connection.
#'
#' @return A tibble of three variables: `chr`, chromosome; `pos`, genomic
#'   position; and `val`, value. Chromosome positions are 1-relative, i.e. the
#'   first base is 1, as specified in WIG files.
#'
#' @md
#'
#' @examples
#' # Import a WIG file
#' wig_file <- system.file(
#'               "extdata",
#'               file = 'hg19-pik3ca.wig',
#'               package = "wig",
#'               mustWork = TRUE)
#'
#' import_wig(wig_file)
#'
#' @export
import_wig <- function(file_path, n = -1L) {

  lines <- readLines(file_path, n = n)
  is_declaration <- grepl('Step', lines)
  blocks <- cumsum(is_declaration)

  lines_lst <- split(lines, blocks)
  lst <- vector(mode = 'list', length = length(lines_lst))

  for(i in seq_along(lst)) {

    declaration <- parse_declaration(lines_lst[[i]][1])
    if(identical(declaration$format, 'fixedStep')) {
      lst[[i]] <-
        expand_block_fixed_step(
          value = as.double(lines_lst[[i]][-1]),
          chrom = declaration$chrom,
          start = declaration$start,
          step = declaration$step,
          span = ifelse(is.na(declaration$span), 1, declaration$span)
        )
    } else { # 'variableStep'
      m <- stringr::str_split(lines_lst[[i]][-1], pattern = '\\s+', simplify = TRUE)
      lst[[i]] <-
        expand_block_variable_step(
          chr_pos = as.integer(m[, 1]),
          value = as.double(m[, 2]),
          chrom = declaration$chrom,
          span = ifelse(is.na(declaration$span), 1, declaration$span)
        )
    }
  }

  dplyr::bind_rows(lst)

}
