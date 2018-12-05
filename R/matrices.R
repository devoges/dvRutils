#' mtimes
#'
#' @param A a matrix
#' @param B a compatible matrix
#'
#' @return A \times B
#' @export
#'
#' @examples
mtimes <- function(A,
                   B) {
  eigenMapMatMult(A, B)
}

#' %mtimes%
#'
#' @param A a matrix
#' @param B a compatible matrix
#'
#' @return A \times B
#' @export
#'
#' @examples
`%mtimes%` <- function(A,
                       B) {
  mtimes(A, B)
}

#' minv
#'
#' @param A a matrix
#'
#' @return the inverse of A
#' @export
#'
#' @examples
minv <- function(A) {
  eigenMapMatInv(A)
}

#' meig
#'
#' @param A a matrix
#'
#' @return the eigenvalues of A
#' @export
#'
#' @examples
meig <- function(A) {
  rev(eigenMapMatEig(A))
}
