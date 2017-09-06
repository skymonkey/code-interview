sum.multiples <- function(x, y, limit = 1000){
# Calculate the sum of all multiples of two numbers
# below a certain limit.
#
# Args:
#  x, y:  the two numbers.
#  limit: the upper multiple limit.
#
# Returns:
#  The sum of all multiples of x and y which are < limit.

  z <- seq(1:limit)
  z <- union(z[z %% x == 0], z[z %% y == 0])
  sum( z[z < limit])

}
