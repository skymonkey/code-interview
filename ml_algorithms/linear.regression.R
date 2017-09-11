yhat <- function(w, b, X){
# Computes yhat as linear combination
# of some inputs.
#
# Args:
#   w: regression coefficient.
#   b: bias constant.
#   X: input values.
#
# Returns:
#   yhat <- w * X + b

  w * X + b
}


loss <- function(w, b, X, Y){
# Computes mean squared error
# given some linear combination of inputs vs
# the known outputs.
#
# Args:
#   w: regression coefficient.
#   b: bias term.
#   X: input values.
#   Y: output values.
#
#  Returns:
#   Returns mean squared error between yhat and y

    sum((yhat(w, b, X) - Y)^2) / (length(X))

}


loss.diff <- function(w, b, new.w, new.b, X, Y){
# Computes the loss difference between an existing
# pair of coefficients and a new set. Calculating
# the loss difference can be used to determine if
# the gradient descent algorithm has converged.
#
# Args:
#  w: regression coefficient.
#  b: bias term.
#  new.w: newly computed regression coefficient.
#  new.b: newly computed bias term.
#  X: input values.
#  Y: output values.
#
# Return:
#  The root squared difference between the loss
#  function when calculated separately on (w, b) and
#  (new.w, new.b)

   loss1 <- loss(w, b, X, Y)
   loss2 <- loss(new.w, new.b, X, Y)

   sqrt((loss1 - loss2)^2)

}


grad.desc <- function(w, b, X, Y, alpha, threshold){
# Performs gradient descent.
#
# Args:
#   w: initial regression coefficient.
#   b: initial bias term.
#   X: input values.
#   Y: output values.
#   alpha: learning rate.
#   threshold: the threshold for convergence.
#
# Returns:
#  Optimised values for (b, w)

   converged <- 0

   while(!converged){

      m <- length(X)
      new.b <- b - alpha * sum( (b + w*X - Y) ) * (1/m)
      new.w <- w - alpha * sum( (b + w*X - Y)*X) * (1/m)



      improvement <- loss.diff(w, b, new.w, new.b, X, Y)

      if( improvement > threshold ){

         b <- new.b
         w <- new.w


       }else{

         converged <- 1

       }

    }
    c(b, w)
}
    