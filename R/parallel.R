# make_immunr_cluster = function () {
#   require(doParallel)
#   require(parallel)
#   IMMUNR_CLUSTER = makeCluster(parallel::detectCores(), type='PSOCK')
#   registerDoParallel(IMMUNR_CLUSTER)
# }
