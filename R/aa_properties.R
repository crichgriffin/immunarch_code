#######
# WIP #
#######

# cdrProp <- function (.data, .prop = c("hydro", "polarity+turn"), .region = c("vhj", "h", "v", "j")) {
#   proplist = lapply(.prop, function(.prop) { chooseprop(.prop) })
#
#   if (!has_class(.data, "list")) {
#     .data = list(Data = .data)
#   }
#
#   data("aaproperties")
#
#   res = .data[IMMCOL$cdr3aa]
#
#   for (property in proplist){
#   new = .data %>%
#     select(IMMCOL$cdr3aa) %>%
#     mutate(hydro = aapropeval(IMMCOL$cdr3aa, property)) %>%
#     collect()
#
#   colnames(new) <- c("IMMCOL$cdr3aa", property)
#   res <- dplyr::bind_cols(res, new[property])
#   }
#
#   add_class(res, "immunr_cdr_prop")
# }


# chooseprop <- function(prop) {
#   switch(prop,
#          alpha = "alpha",
#          beta = "beta",
#          charge = "charge",
#          core = "core",
#          hydro = "hydropathy",
#          ph = "pH",
#          polar = "polarity",
#          rim = "rim",
#          surf = "surface",
#          turn = "turn",
#          vol = "volume",
#          str = "strength",
#          dis = "disorder",
#          high = "high_contact",
#          stop("Unknown property name"))
#   }
#
# aapropeval <- function(seq, col){
#   aaproperty <- AA_PROP[,c("amino.acid", col)]
#   seq <- strsplit(x = seq, split = "")
#   aaseqpropvalue <- lapply(seq, function(seq) {
#     sum(aaproperty[seq, ][[col]], na.rm = TRUE) / length(seq) })
#   return(aaseqpropvalue)
# }
#
# cdrPropAnalysis <- function (.data, .method = c("t.test")) {
#
# }
