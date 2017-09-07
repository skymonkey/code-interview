even.fibb <- function(x){
# By considering the terms in the Fibonacci sequence whose values
# do not exceed a certain limit, find the sum of the even-valued terms.
#
# Args:
#   x: upper limit to sequence
#
# Returns:
#   The sum of all even numbers in the sequence.

    fibb <- c(1, 2)

    while(fibb[length(fibb)] < x){

       fibb <-c(fibb, sum(fibb[length(fibb)],fibb[(length(fibb)-1)]))

    }

    sum(fibb[fibb %% 2 == 0])

}


