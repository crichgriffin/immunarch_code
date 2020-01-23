default_scale_fun <- function (x) {
  # if (sum(x < 1, na.rm = T) == 0) {
  #   -log(x)
  # } else {
  #   x
  # }

  x
}


#' Dimensionality reduction
#'
#' @importFrom stats prcomp
#'
#' @aliases immunr_pca immunr_mds immunr_tsne
#'
#' @description Collect a set of principal variables, reducing the number of not important variables
#' to analyse. Dimensionality reduction makes data analysis algorithms work faster and
#' sometimes more accurate, since it also reduces noise in the data. Currently available
#' methods are:
#'
#' - \code{immunr_pca} performs PCA (Principal Component Analysis) using \link{prcomp};
#'
#' - \code{immunr_mds} performs MDS (Multi-Dimensional Scaling) using \link[MASS]{isoMDS};
#'
#' - \code{immunr_tsne} performs tSNE (t-Distributed Stochastic Neighbour Embedding) using \link[Rtsne]{Rtsne}.
#'
#' @usage
#'
#' immunr_pca(.data, .scale = default_scale_fun, .raw = T, .orig = F, .dist = F)
#'
#' immunr_mds(.data, .scale = default_scale_fun, .raw = T, .orig = F, .dist = T)
#'
#' immunr_tsne(.data, .perp = 1, .dist = T, ...)
#'
#' @param .data A matrix or a data frame with features, distance matrix or output from \link{repOverlapAnalysis} or \link{geneUsageAnalysis} functions.
#'
#' @param .scale A function to apply to your data before passing it to any of
#' dimensionality reduction algorithms. There is no scaling by default.
#'
#' @param .perp The perplexity parameter for \link[Rtsne]{Rtsne}. Sepcifies the number
#' of neighbours each data point must have in the resulting plot.
#'
#' @param .raw If TRUE then return non-processed output from dimensionality reduction
#' algorithms. Pass FALSE if you want to visualise results.
#'
#' @param .orig If TRUE then return the original result from algorithms. Pass FALSE
#' if you want to visualise results.
#'
#' @param .dist If TRUE then assume ".data" is a distance matrix.
#'
#' @param ... Other parameters passed to \link[Rtsne]{Rtsne}.
#'
#' @return
#' \code{immunr_pca} - an output from \link{prcomp}.
#'
#' \code{immunr_mds} - an output from \link{isoMDS}.
#'
#' \code{immunr_tsne} - an output from \link{Rtsne}.
#'
#' @seealso \link{vis.immunr_pca} for visualisations.
#'
#' @export immunr_pca immunr_mds immunr_tsne
immunr_pca <- function (.data, .scale = default_scale_fun, .raw = T, .orig = F, .dist = F) {
  if (.dist) {
    res = cmdscale(as.dist(.scale(.data)), list. = T)

    if (!.orig) {
      res$scale = F
      res$center = c(0, 0, 0, 0)
      res$x = res$points
      res$rotation = res$points
      res$sdev = c(0, 0, 0, 0)
      colnames(res$x) = c("DimI", "DimII")
      res = add_class(res, "prcomp")
    }
  } else {
    res = prcomp(.scale(.data), scale. = T)

    if (!.raw) {
      res = data.frame(res$x)[c(1,2)]
      colnames(res) = c("V1", "V2")
      res$Sample = row.names(res)
    }
  }

  add_class(res, "immunr_pca")
}

immunr_mds <- function (.data, .scale = default_scale_fun, .raw = T, .orig = F, .dist = T) {
  if (.dist) {
    .data = as.dist(.scale(.data))
  } else {
    .data = .scale(.data)
  }
  res = MASS::isoMDS(.data, k = 2, trace=F)
  if (!.raw) {
    res = data.frame(res$points)
    colnames(res) = c("V1", "V2")
    res$Sample = row.names(res)
  } else {
    if (!.orig) {
      # Dirty hack to make factoextra work with MDS objects
      res$scale = F
      res$center = c(0, 0, 0, 0)
      res$x = res$points
      res$rotation = res$points
      res$sdev = c(0, 0, 0, 0)
      colnames(res$x) = c("DimI", "DimII")
      res = add_class(res, "prcomp")
    }
  }
  add_class(res, "immunr_mds")
}

immunr_tsne <- function (.data, .perp = 1, .dist = T, ...) {
  if (.dist) {
    data_proc = as.dist(.data)
  } else {
    data_proc = .data
  }
  res = Rtsne::Rtsne(data_proc, perplexity = .perp, is_distance = .dist, ...)$Y
  row.names(res) = row.names(.data)
  colnames(res) = c("DimI", "DimII")
  add_class(res, "immunr_tsne")
}
