show_image <- function(intensities)
{
   image(matrix(as.numeric(intensities), ncol=28, nrow=28), 
	       col=gray.colors(256))
}

display_random_images <- function(im, nb_images=50)
{
  index_range <- 1:nb_images
  for (i in index_range){
    index <- sample(index_range, 1)
    show_image(im[index, ])
    Sys.sleep(0.1)
  }
}

dt <- read.csv("train.csv", colClasses=rep("integer", 785))
print(dim(dt))
# remove pixels without variation
does_vary <- apply(dt, 2, function(x){length(unique(x)) != 1})
dt <- dt[, does_vary]
print(dim(dt))
images <- as.matrix(dt[, -1])
labels <- dt[, 1]

# the dataset is well-balanced
print(table(labels))
# visual check
# display_random_images(images)
## Find a good thresholding to binarize the picture
summary(as.numeric(images))
hist(images, breaks=256)
barplot(table(images[images > 200]))

# threshold at 250
dt_th <- dt
dt_th[dt_th < 250] <- 0 
dt_th[dt_th >= 250] <- 1
# restore the labels
dt_th[, 1] <- dt[, 1]
does_vary_th <- apply(dt_th, 2, function(x){length(unique(x)) != 1})
dt_th <- dt_th[, does_vary_th]
# display_random_images(images_th, 200)
print(dim(dt_th))

# See what we get with pca
im_pca <- prcomp(images)
barplot(im_pca$sdev[1:50])
pairs(im_pca$x[, 1:3], pch=20, col=labels)

im_th_pca <- prcomp(images_th)
barplot(im_th_pca$sdev[1:50])
pairs(im_th_pca$x[, 1:3], pch=20, col=labels)

# 
library(gbm)

im_gbm <- gbm(label ~ ., data=dt, distribution="multinomial", cv.folds=10, n.cores=3)
print(im_gbm$cv.error)

im_gbm_th <- gbm(label ~ ., data=dt_th, distribution="multinomial", cv.folds=10, n.cores=3)
print(im_gbm_th$cv.error)



