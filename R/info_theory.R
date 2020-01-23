#' Information measures
#'
#' @aliases entropy kl_div js_div cross_entropy
#'
#' @description Compute information-based estimates and distances.
#'
#' @usage
#' entropy(.data, .base = 2, .norm = F, .do.norm = NA, .laplace = 1e-12)
#'
#' kl_div(.alpha, .beta, .base = 2, .do.norm = NA, .laplace = 1e-12)
#'
#' js_div(.alpha, .beta, .base = 2, .do.norm = NA, .laplace = 1e-12, .norm.entropy = F)
#'
#' cross_entropy(.alpha, .beta, .base = 2, .do.norm = NA,
#'               .laplace = 1e-12, .norm.entropy = F)
#'
#' @param .data Numeric vector. Any distribution.
#' @param .alpha Numeric vector. A distribution of some random value.
#' @param .beta Numeric vector. A distribution of some random value.
#' @param .base Numeric. A base of logarithm.
#' @param .norm Logical. If TRUE then normalise the entropy by the maximal value of the entropy.
#' @param .do.norm If TRUE then normalise input distributions to make them sum up to 1.
#' @param .laplace Numeric. A value for the laplace correction.
#' @param .norm.entropy Logical. If TRUE then normalise the resultant value by the average entropy of input distributions.
#'
#' @return
#' A numeric value.
#'
#' @export entropy kl_div js_div cross_entropy
entropy <- function (.data, .base = 2, .norm = F, .do.norm = NA, .laplace = 1e-12) {
  .data <- check_distribution(.data, .do.norm, .laplace, .warn.zero = T)
  res <- - sum(.data * log(.data, base = .base))
  if (.norm) {
    res / log(length(.data), base = .base)
  } else {
    res
  }
}

kl_div <- function (.alpha, .beta, .base = 2, .do.norm = NA, .laplace = 1e-12) {
  .alpha <- check_distribution(.alpha, .do.norm, .laplace, .warn.zero = T)
  .beta <- check_distribution(.beta, .do.norm, .laplace, .warn.zero = T)
  sum(log(.alpha / .beta, base = .base) * .alpha)
}

js_div <- function (.alpha, .beta, .base = 2, .do.norm = NA, .laplace = 1e-12, .norm.entropy = F) {
  .alpha <- check_distribution(.alpha, .do.norm, .laplace, .warn.zero = T)
  .beta <- check_distribution(.beta, .do.norm, .laplace, .warn.zero = T)
  nrm = if (.norm.entropy) 0.5 * (entropy(.alpha, .base, F, .do.norm, .laplace) + entropy(.beta, .base, F, .do.norm, .laplace)) else 1
  M <- (.alpha + .beta) / 2
  0.5 * (kl_div(.alpha, M, .base, F) + kl_div(.beta, M, .base, F)) / nrm
}

cross_entropy <- function (.alpha, .beta, .base = 2, .do.norm = NA, .laplace = 1e-12, .norm.entropy = F) {
  .alpha <- check_distribution(.alpha, .do.norm, .laplace, .warn.zero = T)
  .beta <- check_distribution(.beta, .do.norm, .laplace, .warn.zero = T)
  -sum(log( .beta, base = .base) * .alpha)
}

