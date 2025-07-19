
# old name [format_named()]

#' @title Print Sideways
#' 
#' @param x \link[base]{character} \link[base]{vector}, 
#' or a \link[base]{list} of \link[base]{character} object.
#' Input `x` must be named
#' 
#' @param sep \link[base]{character} scalar, see \link[base]{paste}
#' 
#' @param colour \link[base]{list} of color functions from package \CRANpkg{cli}, default
#' `list(col_cyan, col_magenta)`
#' 
#' @param ... additional parameters for future use
#' 
#' @returns
#' Function [sideway()] does not have a return value.
#' 
#' @examples
#' c(a = 'a1', bc = '2\n3') |> sideway()
#' list(a = '1\n2', b = character(), cd = '3\n4', efg = '5\n6\n7') |> sideway()
#' c(a = '1\n2') |> sideway()
#' 
#' # data.frame
#' Formaldehyde |> sideway() # no rownames
#' USArrests |> head() |> sideway() # with rownames
#' @keywords internal
#' @name sideway
#' @importFrom cli col_cyan col_magenta style_bold
#' @export
sideway <- function(x, ...) UseMethod(generic = 'sideway')

#' @rdname sideway
#' @export sideway.default
#' @export
sideway.default <- function(
    x, 
    sep = ': ',
    colour = list(col_cyan, col_magenta), 
    ...
) {
  
  x0 <- x |>
    vapply(FUN = paste, collapse = ' ', FUN.VALUE = '') |> 
    trimws()
  
  x1 <- x0[nzchar(x0)]
  if (!length(nm <- names(x1))) stop('input must be named')
  if (!all(nzchar(nm))) stop('do not allow empty name!')
  
  x2 <- x1 |>
    strsplit(split = '\n')
  
  nx <- lengths(x2)
  cx <- nx |> 
    cumsum()
  
  if (!all(nx == 1L)) { # some element(s) contains '\n'
    x1 <- x2 |>
      unlist(use.names = FALSE) # overwrite `x1` !!!
    xnm. <- character(length = length(x1))
    xnm.[cx] <- nm # now has zchar in `xnm.`
  } else xnm. <- nm
  
  ret <- xnm. |>
    format.default(justify = 'right') |> # justify all elements!
    paste(x1, sep = sep)
  
  id <- mapply(
    FUN = `:`, 
    c(1L, cx[-length(cx)] + 1L) |> unname(),
    cx |> unname(), 
    SIMPLIFY = FALSE
  ) # `real` indices
  
  colour <- colour[seq_len(min(length(id), length(colour)))] # must!
  
  mapply(FUN = \(i, col) {
    ret[i] |>
      col() |>
      style_bold() |>
      lapply(FUN = cat, '\n')
  }, i = id, col = colour) |> 
    suppressWarnings() # warning in length recycling
  
  return(invisible())
  
}


#' @rdname sideway
#' @export sideway.data.frame
#' @export
sideway.data.frame <- function(x, ...) {
  
  rseq <- x |> 
    .row_names_info(type = 2L) |>
    seq_len()
  
  .mapply(FUN = \(x, rnm, rid) {
    sprintf(fmt = '%s Row %s', rid, sQuote(rnm)) |> 
      message()
    x |> 
      sideway.default(...)
  }, dots = list(
    x = split.data.frame(x, f = rseq), 
    rnm = row.names.data.frame(x),
    rid = rseq |> 
      sprintf(fmt = '[%d]') |> 
      format.default(justify = 'right')
  ), MoreArgs = NULL)
  
  return(invisible())
  
}








# https://gist.github.com/upsilun/4a85ab3bc7cf92e8acde720c6eb7ddea

