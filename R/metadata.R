# read_metadata <- function (.obj) {
#
# }
#
#
# write_metadata <- function (.obj) {
#
# }
#
#
# check_metadata <- function (.data, .meta) {
#   .meta = collect(.meta)
#
#   (length(.data) == length(unique(names(.data)))) &
#     (length(.meta$Sample) == length(unique(.meta$Sample))) &
#     (sum(!(names(.data) %in% .meta$Sample)) == 0)
# }
