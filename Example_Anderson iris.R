source("random_forests_with_missing_values.R")
source("plot_regtree_and_forest.R")

# Load data 
XX1 <- iris$Petal.Length
XX2 <- iris$Sepal.Length

# Introducing missing values in the data
XX1 <- c(XX1, 1.5, 6, NA)
XX2 <- c(XX2, NA, NA, 7)

XX <- cbind(XX1,XX2)
XX <- as.data.frame(XX)
colnames(XX) <- c('Petal Length', 'Sepal Length')
yy <- iris$Petal.Width
yy <- c(yy, 0.25, 3, 1.53)
yy_name <- 'Petal Width'

# The usual tree
miss_iris_tree <- miss_regTree_RF(XX, yy, random = FALSE, nodesize = 50)

# Structure of the tree
miss_iris_tree$Structure

# Plot the tree
x11()
miss_tree.plot(miss_iris_tree)

# Plot the tree in 3D
miss_tree.plot(tree = miss_iris_tree, y_name = yy_name, in2D = FALSE, add = FALSE)

# Plot the tree in 3D and add the observations
miss_tree.plot(tree = miss_iris_tree, y_name = yy_name, in2D = FALSE, rad = 0.2)

# Calculate and plot the connectivity matrix
connect_matrix <- miss_tree.conect(tree = miss_iris_tree)
x11()
matrix.plot(connect_matrix)

miss_tree.conect(x = c(2,6), tree = miss_iris_tree)

# Prediction with the tree
miss_tree.pred(x = c(2,4.5),  tree = miss_iris_tree)$y_pred
miss_tree.pred(x = c(6,NA),   tree = miss_iris_tree)$y_pred
miss_tree.pred(x = c(NA,4.5), tree = miss_iris_tree)$y_pred
miss_tree.pred(x = c(2,NA),   tree = miss_iris_tree)$y_pred

# A random tree
set.seed(111)
miss_iris_random_tree <- miss_regTree_RF(XX, yy, nodesize = 50)
x11()
miss_tree.plot(miss_iris_random_tree)

# Forest
numCores <- detectCores()
cl <- parallel::makeCluster(numCores, setup_strategy = "sequential")
registerDoParallel(cl)

miss_iris_random_forest <- miss_regRF(XX, yy, nodesize = 50)

# The OOB error
miss_iris_random_forest$OOB.err <- miss_oob.err(miss_iris_random_forest)

x11()
plot(round(miss_iris_random_forest$OOB.err,3), type = 'l',
     ylab = 'OOB.err', xlab = 'ntree')

# Add some trees to the forest
inn <- increase(miss_iris_random_forest, 50)
miss_iris_random_forest$OOBmatrix <- inn$new.OOB
for (k in 1:50){
  miss_iris_random_forest$trees <- list.append(
    miss_iris_random_forest$trees, inn$new.trees[[k]]
    )
}
miss_iris_random_forest$OOB.err <- miss_oob.err(miss_iris_random_forest)

x11()
plot(round(miss_iris_random_forest$OOB.err,3), type = 'l',
     ylab = 'OOB.err', xlab = 'ntree')

# Remove some trees to the forest
decc <- decrease(miss_iris_random_forest, rem = 101:150)
miss_iris_random_forest$trees <- decc$new.trees
miss_iris_random_forest$OOBmatrix <- decc$new.OOB

miss_iris_random_forest$OOB.err <- miss_oob.err(miss_iris_random_forest)

x11()
plot(round(miss_iris_random_forest$OOB.err,3), type = 'l',
     ylab = 'OOB.err', xlab = 'ntree')

# Plot the forest
miss_RF.plot(miss_iris_random_forest, y_name = yy_name, add = FALSE)

# Plot the forest and add the observations
miss_RF.plot(miss_iris_random_forest, y_name = yy_name, rad = 0.2)

# Prediction with the forest
miss_RF.pred(x = c(2,4.5),  forest = miss_iris_random_forest)
miss_RF.pred(x = c(6,NA),   forest = miss_iris_random_forest)
miss_RF.pred(x = c(NA,4.5), forest = miss_iris_random_forest)
miss_RF.pred(x = c(2,NA),   forest = miss_iris_random_forest)

# Proximity matrix 
prox_matrix <- miss_RF.proximity(forest = miss_iris_random_forest)
x11()
matrix.plot(prox_matrix)

stopCluster(cl)