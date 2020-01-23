#' Clustering of objects or distance matrices
#'
#' @aliases immunr_hclust immunr_kmeans immunr_dbscan
#'
#' @importFrom fpc dbscan
#' @importFrom factoextra hcut fviz_nbclust
#' @importFrom stats kmeans as.dist cmdscale dist
#'
#' @description Cluster the data with one of the following methods:
#'
#' - \code{immunr_hclust} clusters the data using the hierarchical clustering from \link[factoextra]{hcut};
#'
#' - \code{immunr_kmeans} clusters the data using the K-means algorithm from \link{kmeans};
#'
#' - \code{immunr_dbscan} clusters the data using the DBSCAN algorithm from \link[fpc]{dbscan}.
#'
#' @usage
#' immunr_hclust(.data, .k = 2, .k.max = nrow(.data) - 1, .method = "complete", .dist = T)
#'
#' immunr_kmeans(.data, .k = 2, .k.max = as.integer(sqrt(nrow(.data))) + 1,
#' .method = c("silhouette", "gap_stat"))
#'
#' immunr_dbscan(.data, .eps, .dist = T)
#'
#' @param .data Matrix or data frame with features, distance matrix or output from \link{repOverlapAnalysis} or \link{geneUsageAnalysis} functions.
#'
#' @param .k The number of clusters to create, passed as \code{k} to \link[factoextra]{hcut} or as \code{centers} to \link{kmeans}.
#'
#' @param .k.max Limits the maximum number of clusters. It is passed as \code{k.max} to \link{fviz_nbclust} for \code{immunr_hclust} and \code{immunr_kmeans}.
#'
#' @param .eps Local radius for expanding clusters, minimal distance between points to expand clusters. Passed as \code{eps} to \link[fpc]{dbscan}.
#'
#' @param .method Passed to \link[factoextra]{hcut} or as \link{fviz_nbclust}.
#'
#' In case of \link[factoextra]{hcut} the agglomeration method is going to be used (argument \code{hc_method}).
#'
#' In case of \link{fviz_nbclust} it is the method to be used for estimating the optimal number of clusters (argument \code{method}).
#'
#' @param .dist If TRUE then ".data" is expected to be a distance matrix. If FALSE then the euclidean distance is computed for the input objects.
#'
#' @return
#' \code{immunr_hclust} - list with two elements. First element is an output from \link{hcut}.
#' Second element is an output from \link{fviz_nbclust}
#'
#' \code{immunr_kmeans} - list with three elements. First element is an output from \link{kmeans}.
#' Second element is an output from \link{fviz_nbclust}.
#' Third element is the input dataset \code{.data}.
#'
#' \code{immunr_dbscan} - list with two elements. First element is an output from \link{dbscan}.
#' Second element is the input dataset \code{.data}.
#'
#' @export immunr_hclust immunr_kmeans immunr_dbscan
immunr_hclust <- function (.data, .k = 2, .k.max = nrow(.data) - 1, .method = "complete", .dist = T) {
  if (.dist) {
    dist_mat = as.dist(.data)
  } else {
    dist_mat = dist(.data)
  }
  res = list(hcut = add_class(hcut(dist_mat, k = .k, hc_method = .method), "immunr_hcut"),
             nbclust = add_class(fviz_nbclust(.data, hcut, k.max = .k.max), "immunr_nbclust"))
  add_class(res, "immunr_hclust")
}

immunr_kmeans <- function (.data, .k = 2, .k.max = as.integer(sqrt(nrow(.data))) + 1, .method = c("silhouette", "gap_stat")) {
  # res = list(kmeans = add_class(kmeans(as.dist(.data), .k), "immunr_kmeans"),
  res = list(kmeans = add_class(kmeans(.data, .k), "immunr_kmeans"),
             nbclust = add_class(fviz_nbclust(.data, kmeans, k.max = .k.max, .method[1]), "immunr_nbclust"),
             data = .data)
  add_class(res, "immunr_kmeans")
}

immunr_dbscan <- function (.data, .eps, .dist = T) {
  if (.dist) {
    .data = as.dist(.data)
    method = "dist"
  } else {
    method = "hybrid"
  }
  res = list(dbscan = fpc::dbscan(.data, eps = .eps, method = method), data = .data)
  add_class(res, "immunr_dbscan")
}
