# mutationNetwork <- function (.data, .method = c("hamm", "lev"), .err = 2) {
#   require(igraph)
#   UseMethod("mutationNetwork")
# }
#
# mutationNetwork.character <- function (.data, .method = c("hamm", "lev"), .err = 2) {
#   add_class(res, "immunr_mutation_network")
# }
#
# mutationNetwork.immunr_shared_repertoire <- function (.data, .method = c("hamm", "lev"), .err = 2) {
#   add_class(res, "immunr_mutation_network")
# }
#
# mutationNetwork.tbl <- function (.data, .col, .method = c("hamm", "lev"), .err = 2) {
#   select_(.data, .dots = .col)
#   add_class(res, "immunr_mutation_network")
# }
#
# mut.net = mutationNetwork
