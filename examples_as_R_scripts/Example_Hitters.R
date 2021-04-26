source("random_forests_with_missing_values.R")
source("plot_regtree_and_forest.R")

# This example appears in Introduction to Statistical Learning with R,
# chap 8 'Three-Based Methods', section 8.1

# library with the Hitter data
library(ISLR)

# Data
XX1 <- Hitters$Years
XX2 <- Hitters$Hits
XX <- cbind(XX1,XX2)
XX <- as.data.frame(XX)
colnames(XX) <- c('Years', 'Hits')
yy <- Hitters$Salary
yy_name <- 'Salary'

NA_places <- is.na(yy)

# Left out the NA's
XX <- XX[!NA_places,]
yy <- yy[!NA_places]

# The tree presented in the mentioned book
hitter_tree <- miss_regTree_RF(XX, yy, random=FALSE, nodesize = 100)

x11()
miss_tree.plot(hitter_tree, eps_x=0.5, eps_y=5, add=FALSE)

x11()
miss_tree.plot(hitter_tree, eps_x=0.5, eps_y=5)

# Plot the tree in 3D
miss_tree.plot(hitter_tree, in2D = FALSE, add=FALSE, y_name=yy_name)

# Plot the tree in 3D and add the observations
miss_tree.plot(hitter_tree, in2D = FALSE, y_name=yy_name, rad=20)

## About prediction and conection in the tree
miss_tree.pred(c(2,100), hitter_tree)
miss_tree.pred(c(15,50), hitter_tree)
miss_tree.pred(c(15,150), hitter_tree)
miss_tree.pred(c(2,400), hitter_tree)
miss_tree.pred(c(1,12,150), hitter_tree)

miss_tree.conect(c(NA,150), tree = hitter_tree)
miss_tree.pred(c(NA,150), tree = hitter_tree)

miss_tree.conect(c(15,NA), tree = hitter_tree)
miss_tree.pred(c(15,NA), tree = hitter_tree)

miss_tree.conect(c(2,NA), tree = hitter_tree)
miss_tree.pred(c(2,NA), tree = hitter_tree)


## A deeper tree
hitter_tree_2 <- miss_regTree_RF(XX, yy, random=FALSE, nodesize = 10)

x11()
miss_tree.plot(hitter_tree_2, eps_x=0.5, eps_y=5, add=FALSE,
             y_name = yy_name, cex_plot=0.7)

x11()
miss_tree.plot(hitter_tree_2, eps_x=0.5, eps_y=5,
             y_name = yy_name, pred=FALSE)

miss_tree.plot(hitter_tree_2, in2D = FALSE, add=FALSE,
             y_name=yy_name)

miss_tree.plot(hitter_tree_2, in2D = FALSE,
             y_name=yy_name, rad=20)


## An even deeper tree
hitter_tree_3 <- miss_regTree_RF(XX, yy, random=FALSE, nodesize = 1)

x11()
miss_tree.plot(hitter_tree_3, eps_x=0.5, eps_y=5, add=FALSE,
             y_name = yy_name, cex_plot=0.7)

x11()
miss_tree.plot(hitter_tree_3, eps_x=0.5, eps_y=5,
             y_name = yy_name, pred=FALSE)

miss_tree.plot(hitter_tree_3, in2D = FALSE, add=FALSE,
             y_name=yy_name)

miss_tree.plot(hitter_tree_3, in2D = FALSE,
             y_name=yy_name, rad=20)


# Forests
numCores <- detectCores()
cl <- parallel::makeCluster(numCores, setup_strategy = "sequential")
registerDoParallel(cl)

set.seed(111)
hitter_RF <- miss_regRF(XX, yy, nodesize = 50)

# Let's see the OOB error
hitter_RF$OOB.err <- miss_oob.err(hitter_RF)

x11()
plot(round(hitter_RF$OOB.err,3), type = 'l', pch = 21,
     ylab = 'OOB.err', xlab = 'ntree')


miss_RF.plot(hitter_RF, y_name = yy_name, add = FALSE)x

miss_RF.plot(hitter_RF, y_name = yy_name, rad = 20)

stopCluster(cl)
