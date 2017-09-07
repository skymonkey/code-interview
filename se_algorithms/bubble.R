bubble <- function(x){
# Implementation of the bubble sort.
#
# Args:
#  x: vector to be sorted
#
# Returns:
#  Sorted x.


   swapped <- 1

   while(swapped){

      swapped <- 0

      for(i in 2:length(x)){

        if(x[i - 1] > x[i]){

           tmp <- x[i - 1]
           x[i - 1] <- x[i]
           x[i] <- tmp
           swapped <- 1
        }

      }
   }
   
   x
}
	  