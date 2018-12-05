#' @useDynLib dvRutils, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' `%ni%`
#'
#' @param x , vector
#' @param y , vector
#'
#' @return
#' @export
#'
#' @examples
`%ni%` <- function(x,
                   y) {
  !(x %in% y)
}

#' remove_these
#'
#' @param x a vector
#' @param remove a vector of values to remove from \code{x}
#'
#' @return \code{x} with all those elements matching any in \code{remove} removed
#' @export
#'
#' @examples
remove_these <- function(x, remove) {
  x[x %ni% remove]
}

#' keep_these
#'
#' @param x a vector
#' @param keep a vector of values to keep, exclusively, from \code{x}
#'
#' @return \code{x} with only those elements matching any in \code{remove}
#' @export
#'
#' @examples
keep_these <- function(x, keep) {
  x[x %in% keep]
}

#' nlapply
#'
#' @param n first argument to lapply function, with names
#' @param ...
#'
#' @return \code{lapply} call with named list returned
#' @export
#'
#' @examples
nlapply <- function(n, ...) {
  o <- lapply(n, ...)
  names(o) <- n
  o
}

#' library_many
#'
#' @param x character vector of packages to load
#'
#' @return load packages in \code{x} into session/namespace
#' @export
#'
#' @examples
library_many <- function(x) {
  lapply(x, library, character.only = TRUE)
}

#' clear
#'
#' @return Remove all in environment and clear console (x4)
#' @export
#'
#' @examples
clear <- function() {
  rm(list = ls())
  cat("\f")
  rm(list = ls())
  cat("\f")
  rm(list = ls())
  cat("\f")
  rm(list = ls())
  cat("\f")
}

#' %%
#'
#' @param ...
#'
#' @return paste together with no space
#' @export
#'
#' @examples
`%%` <- function(...) {
  paste0(...)
}

#' % %
#'
#' @param ...
#'
#' @return paste together with space
#' @export
#'
#' @examples
`% %` <- function(x,
                  y) {
  paste(x, y)
}

#' most_common
#'
#' @param x vector
#'
#' @return return which value in \code{x} occurs most often. Only 1 returned in case of tie
#' @export
#'
#' @examples
most_common <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' most_commons
#'
#' @param x vector
#'
#' @return return which value in \code{x} occurs most often. All returned in case of tie
#' @export
#'
#' @examples
most_commons <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#' remove_last
#'
#' @param x vector
#' @param n number of tail to remove
#'
#' @return \code{x} with the last \code{n} values removed
#' @export
#'
#' @examples
remove_last <- function(x,
                        n) {
  length(x) <- length(x) - n
  x
}
