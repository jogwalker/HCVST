# test CEdecisiontree package

# library(devtools)
# install_github("Health-Economics-in-R/CEdecisiontree")
library(CEdecisiontree)

data("cost")
data("probs")

CEdecisiontree:::trans_binarytree(depth = 3)
