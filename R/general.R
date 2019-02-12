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
library_many <- function(...) {
  packages <- as.character(match.call(expand.dots = FALSE)[[2]])

  try <- unlist(lapply(packages, function(package) {
    suppressWarnings(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }))
  names(try) <- packages

  good <- c()
  bad <- c()

  if ( length(which(try)) > 0 ) good <- names(which(try))
  if ( length(which(!try))) bad <- names(which(!try))

  try_install <- unlist(lapply(bad, function(package) {
    message(paste0("Couldn't find ", package, ", trying to install"))
    suppressWarnings(install.packages(package,
                                      dependencies = TRUE,
                                      verbose = FALSE,
                                      quiet = TRUE))
    ok <- suppressWarnings(require(package,
                                   character.only = TRUE,
                                   quietly = TRUE))

    if ( !ok ) {

      if ( !require("BiocManager", quietly = TRUE, character.only = TRUE) ) {
        install.packages("BiocManager")
      }

      message("### Bioconductor will not try to install silently! ###")
      suppressWarnings(suppressWarnings(BiocManager::install(package,
                                                             update = FALSE,
                                                             ask = FALSE,
                                                             dependencies = TRUE,
                                                             quietly = TRUE,
                                                             verbose = FALSE)))
      message("### it is annoying ###")
    }

    suppressWarnings(require(package,
                             character.only = TRUE,
                             quietly = TRUE))
  }))

  if ( length(try_install) > 0 ) names(try_install) <- bad

  if ( length(good) > 0 )  message(paste("Loaded:", paste(good, collapse = ", ")))
  if ( length(which(try_install)) > 0) message(paste("Loaded (and installed):", paste(names(which(try_install)), collapse = ", ")))
  if ( length(which(!try_install)) > 0) message(paste("NOT Loaded (can't find on CRAN or BioConductor):", paste(names(which(!try_install)), collapse = ", ")))
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
                        n = 1) {
  length(x) <- length(x) - n
  x
}


#' %==%
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @return test for numeric equality, within error
#' @export
`%==%` <- function(x, y, epsilon = .Machine$double.eps) {
  all(abs(x - y) <= epsilon)
}

#' remove_intercept
#'
#' @param x matrix or data.frame
#'
#' @return If there is an intercept column (all 1s), it is removed
#' @export
remove_intercept <- function(x) {
  n <- x %>% nrow
  p <- x %>% ncol
  intercept <- 1 %>% rep(n)

  test <- lapply(1:p, function(i) {
    x[, i] %==% intercept
  }) %>% unlist() %>% which

  x[, -test]
}
