knn.impl <- function(df, x, y, k){
# Implememtation of k-nearest neighbours algorithm.
#
# Args:
#  df: data frame containing x and y coordinates plus the corresponding
#      binary class label (0, 1)
#   x: x coordinate of the record to be predicted.
#   y: y coordinate of the record to be predicted
#   k: number of neighbours.
#
# Returns:
#   Class label. 0 if majority of k neighbours are 0, 1 otherwise.

  df$dist <- sqrt((df$x - x)^2 + (df$y - y)^2)
  vote <- sum((as.numeric(df[order(df$dist), 3]))[1:k])/k
  ifelse(vote >= 0.5, 1, 0)

}