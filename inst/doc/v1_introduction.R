## ----eval=FALSE---------------------------------------------------------------
#  # 1.1) Load the package into R:
#  library(immunarch)
#  
#  # 1.2a) To quickly test immunarch, load the test dataset:
#  data(immdata)
#  
#  # 1.2b) To try immunarch on your own data, use the `repLoad` function on your data folder:
#  immdata = repLoad("path/to/your/folder/with/repertoires")

## ----eval=FALSE---------------------------------------------------------------
#  # 2.1) Find the number of shared clonotypes and visualise it:
#  ov = repOverlap(immdata$data)
#  vis(ov)
#  
#  # 2.2) Cluster samples by their similarity:
#  ov.kmeans = repOverlapAnalysis(ov, .method = "mds+kmeans")
#  vis(ov.kmeans)

## ----eval=FALSE---------------------------------------------------------------
#  # 3.1) Compute V gene usage and and highlight gene differences in groups with different clinical status:
#  gu = geneUsage(immdata$data)
#  vis(gu, .by="Status", .meta=immdata$meta)
#  
#  # 3.2) Cluster samples by their V gene usage similarity:
#  gu.clust = geneUsageAnalysis(gu, .method = "js+hclust")
#  vis(gu.clust)

## ----eval=F-------------------------------------------------------------------
#  # 4.1) Compare diversity of repertoires and visualise samples, grouped by both clinical status and sequencing Lane:
#  div = repDiversity(immdata$data, .method = "chao1")
#  vis(div, .by=c("Status", "Lane"), .meta=immdata$meta)

## ----eval=FALSE---------------------------------------------------------------
#  # 5.1) Manipulate the visualisation of diversity estimates to make the plot publication-ready:
#  div = repDiversity(immdata$data, .method = "chao1")
#  div.plot = vis(div, .by=c("Status", "Lane"), .meta=immdata$meta)
#  fixVis(div.plot)

