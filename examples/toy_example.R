source("random_forests_with_missing_values.R")
source("plot_regtree_and_forest.R")

# Data
# The last two digits are out of the [0,1], they are going to be banned.
set.seed(111)
XX <- c(runif(3,0,0.4), runif(3,0.4,0.75), runif(3,0.75,1), runif(2,1,1.5))
XX <- as.data.frame(XX)
colnames(XX) <- c('X1')
X_space <- c(0,1) %>% matrix(nrow = 2)
colnames(X_space) <- colnames(XX)

yy <- c(rnorm(3,1,0.1), rnorm(3,2,0.1), rnorm(3,2.5,0.1), c(3,3))

# the toy tree
toy.tree <- miss_regTree_RF(XX, yy, random=FALSE, X_space = X_space)

x11()
miss_tree.plot(toy.tree)

# Using a palette from the colorspace
#pal <- sequential_hcl(10, h=280, l=c(30,80)) # original palette
pal <- sequential_hcl(10, h=180, l=c(30,80))
pal <- rev(pal)

x11()
miss_tree.plot(toy.tree, fill_palette=pal, point_palette=pal)

# Playing with the parameters of the plot
x11()
miss_tree.plot(toy.tree, point_color = FALSE)

x11()
miss_tree.plot(toy.tree, point_color = FALSE, fill_palette='purple')

x11()
miss_tree.plot(toy.tree, point_color = FALSE, fill_color=FALSE)

x11()
miss_tree.plot(toy.tree, add = FALSE, fill_color=FALSE)