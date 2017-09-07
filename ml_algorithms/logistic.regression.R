# Simple implemenation of logistic regression using gradient descent.
# Functions for: activation (sigmoid), forward propagation, weight optimisation
# and class label prediction.



sigmoid <- function(x){
# The activation function.

  1 / (1 + exp(-x))

}




propagate <- function(w, b, X, Y){
# Given the weights, bias, inputs and outputs, this function
# computes and returns the cost and gradients.
#
# Args:
#  w: weight vector.
#  b: bias term.
#  X: input matrix.
#  Y: output matrix. 
#
# Returns:
#  A list with the following elements:
#     cost: the computed cost
#     dw: derivative of the weights.
#     db: derivative of the bias term.


 m <- nrow(X)
 A <- sigmoid((t(w) %*% X) + b)
 cost <- sum( Y %*% t(log(A)) + (1 - Y) %*% t(log(1 - A)) ) / (-m)
 dw <- (X %*% t(A - Y)) / m
 db <- sum( A - Y) / m

 list(cost = cost, dw = dw, db = db)

}


optimise.weights <- function(w, b, X, Y, alpha, num.iterations){
# Optimises logistic regression coefficients for some
# given input and output matrices. 
#
# Args:
#  w: input weights to be optimised.
#  b: bias term.
#  X: input matrix.
#  Y: output matrix.
#  alpha: learning rate.
#  num.iterations: limit to how many iterations for optimisation.
#
#  Return:
#  A list with the optimised coefficients and final gradient values.

   for( i in 1:num.iterations){

       cost.and.grads <- propagate(w, b, X, Y)
              dw <- cost.and.grads$dw
	             db <- cost.and.grads$db


       w <- w - (alpha * dw)
              b <- b - (alpha * db)

   }

   list( w = w, b = b, dw = dw, db = db)

}

predict.labels <- function(w, b, X){
# Computes the class probabilities for some
# input, using the optimised coefficients
# and converts them to binary class labels on the basis of p > 0.5 = 1
#
# Args:
#   w: optimised weight coefficients.
#   b: optimised bias term.
#   X: input matrix.
#
# Returns:
#  Vector of predicted class labels for every input in X.
#
  A <- sigmoid((t(w) %*% X) + b)
  ifelse( A > 0.5, 1, 0)
}
    