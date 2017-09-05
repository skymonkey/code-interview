left.shift <- function(x, d){
# A left rotation operation on an array of size n shifts
# each of the array's elements unit to the left.
#
# Args:
#  x: the array to be shifted.
#  d: the number of units to shift to the left.
#
# Returns:
#  The same array, but with the elements shifted d
#  units to the left.

   n <- length(x)

   if( d > n ) {


      if( d %% n != 0 ) {

         x <- c(x[((d %% n) + 1):n],x[1:(d %% n)])

      }

    } else {

       x <- c(x[(d+1):n],x[1:d])

    }

   x

}