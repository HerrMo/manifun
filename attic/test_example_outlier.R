devtools::install_github("https://github.com/mlr-org/mlr.git", "d609f17889")
library("mlr")
# remove.packages("mlr")

# example data
load("data/distance_matrices1.RData")

# data set with outlier
#   setting:
#     - 1df amplitude variation and some outliers with 1d phase variation
#     - outlier ratio 1 %

n_out <- 0.025
smpl <- sample(1:1000, 1000 * n_out)
lbls <- c(rep(1, 1000), rep(2, 1000 * n_out))

# data with outliers
dat_o <- rbind(dist_list1$l2_df1_a$data$dat$funs, dist_list1$l2_df1_p$data$dat$funs[smpl, ])

plot_funs(dat_o, color = lbls)

dists_l2 <- as.matrix(dist(dat_o))
imap <- embed(dists_l2, method = "isomap", k = 10, ndim = 3)
umap <- embed(dists_l2, method = "umap", n_components = 3)
plot_emb(imap, color = lbls) + ggtitle("Isomap embedding of example data")
plot_emb(umap, color = lbls) + ggtitle("UMAP embedding of example data")

# data without outliers
dat_wo <- dist_list1$l2_df1_a$data$dat$funs
imap_wo <- embed(as.matrix(dist(dat_wo)), method = "isomap", k = 10)
plot_emb(imap_wo, color = rep(1, 1000)) + ggtitle("Isomap embedding of example data without outliers")

# AMV example from Minh

# between 0.8 and 0.99 with 50 steps.
temp_dat <- imap$points
AMV = makeAMVMeasure(id = "AMV", minimize = TRUE, alphas = c(0.5, 0.75), n.alpha = 50, n.sim = 10e3, best = 0, worst = NULL)

inds.split = BBmisc::chunk(seq_len(nrow(temp_dat)), shuffle = TRUE, props = c(0.6, 0.4))
train.inds = inds.split[[1]]
test.inds = inds.split[[2]]

lrn = makeLearner("oneclass.svm", predict.type = "prob")
mod = train(lrn, oneclass2d.task, subset = train.inds)
pred = predict(mod, oneclass2d.task, subset = test.inds)

performance(pred = pred, measures = list(AMV, auc), model = mod, task = oneclass2d.task)

