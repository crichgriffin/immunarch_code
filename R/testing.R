# load_test_data <- function () {
#   # data(ms_small, package = "immunarch")
#   data(immdata)
#   if (requireNamespace("MonetDBLite", quietly = TRUE)) {
#     immdata = test_make_db(immdata$data, immdata$meta)
#   }
#   immdata
# }
#
# test_make_db <- function (.data, .meta = NA) {
#   require("MonetDBLite")
#
#   assertthat::assert_that(has_class(.data, "list"))
#
#   dbdir = tempdir()
#   con = DBI::dbConnect(MonetDBLite::MonetDBLite(), embedded = dbdir)
#
#   for (i in 1:length(.data)) {
#     DBI::dbWriteTable(con, names(.data)[i], .data[[i]], overwrite=TRUE)
#   }
#
#   ms = MonetDBLite::src_monetdblite(dbdir = dbdir)
#   res_db = list()
#   for (i in 1:length(.data)) {
#     res_db[[names(.data)[i]]] = dplyr::tbl(ms, names(.data)[i])
#   }
#
#   if (is.na(.meta)) {
#     res_db
#   } else {
#     list(data = res_db, meta = .meta)
#   }
# }
