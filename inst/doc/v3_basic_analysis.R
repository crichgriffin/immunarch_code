## ----setup, include=FALSE, echo=FALSE-----------------------------------------
# knitr::knit_hooks$set(optipng = knitr::hook_optipng)
# knitr::opts_chunk$set(optipng = '-o7')

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.width = 12)
knitr::opts_chunk$set(fig.height = 6)

library(immunarch)
# source("../R/testing.R")
# immdata = load_test_data()
data(immdata)

## ----rename-list, eval=F------------------------------------------------------
#  your_data  # Your list with repertoires without names
#  
#  names(your_data)
#  # Output: NULL
#  
#  names(your_data) = sapply(1:length(your_data), function (i) paste0("Sample", i))
#  names(your_data)
#  # Output: Sample1 Sample2 ... Sample10

## ----eda-by-meta, warning=F, fig.width=10, fig.height=4.5---------------------
exp_vol = repExplore(immdata$data, .method = "volume")
p1 = vis(exp_vol, .by = c("Status"), .meta = immdata$meta)
p2 = vis(exp_vol, .by = c("Status", "Sex"), .meta = immdata$meta)
grid.arrange(p1, p2, ncol=2)

## ----eda-by-only, warning=F, fig.width=6, fig.height=4.5----------------------
exp_vol = repExplore(immdata$data, .method = "volume")
by_vec = c('C', 'C', 'C', 'C', 'C', 'C', 'MS', 'MS', 'MS', 'MS', 'MS', 'MS')
p = vis(exp_vol, .by = by_vec)
p

## ----eda-fixvis, eval=F-------------------------------------------------------
#  # 1. Analyse
#  exp_len = repExplore(immdata$data, .method = "len", .col = "aa")
#  
#  # 2. Visualise
#  p1 = vis(exp_len)
#  
#  # 3. Fix and make publication-ready results
#  fixVis(p1)

## ----eda-1, warning=F, fig.width=12, fig.height=4-----------------------------
exp_len = repExplore(immdata$data, .method = "len", .col = "aa")
exp_cnt = repExplore(immdata$data, .method = "count")
exp_vol = repExplore(immdata$data, .method = "volume")

p1 = vis(exp_len)
p2 = vis(exp_cnt)
p3 = vis(exp_vol)

p1

## ----eda-2, warning=F, fig.width=14, fig.height=4-----------------------------
grid.arrange(p2, p3, ncol = 2)

## ----eda-3, warning=F, fig.width=10, fig.height=4-----------------------------
# You can group samples by their metainformation
p4 = vis(exp_len, .by="Status", .meta=immdata$meta)
p5 = vis(exp_cnt, .by="Sex", .meta=immdata$meta)
p6 = vis(exp_vol, .by=c("Status", "Sex"), .meta=immdata$meta)

p4

## ----eda-4, warning=F, fig.width=10, fig.height=5-----------------------------
grid.arrange(p5, p6, ncol = 2)

## ----clonality-pr-------------------------------------------------------------
imm_pr = repClonality(immdata$data, .method = "clonal.prop")
imm_pr

## ----clonality-top------------------------------------------------------------
imm_top = repClonality(immdata$data, .method = "top", .head = c(10, 100, 1000, 3000, 10000))
imm_top

## ----clonality-rare-----------------------------------------------------------
imm_rare = repClonality(immdata$data, .method = "rare")
imm_rare

## ----clonality-hom-vis-1, message=F, warning=F, fig.width=11, fig.height=4.5----
imm_hom = repClonality(immdata$data, .method = "homeo", 
                       .clone.types = c(Small = .0001, Medium = .001, Large = .01, Hyperexpanded = 1))
imm_hom

grid.arrange(vis(imm_top), vis(imm_top, .by="Status", .meta=immdata$meta), ncol = 2)

grid.arrange(vis(imm_rare), vis(imm_rare, .by="Status", .meta=immdata$meta), ncol = 2)

grid.arrange(vis(imm_hom), vis(imm_hom, .by=c("Status", "Sex"), .meta=immdata$meta), ncol = 2)

