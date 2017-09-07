deletions.needed <- function(a, b) {

# Given two strings,a  and b, that may or may not
# be of the same length, determine the minimum number of character
# deletions required to make and anagrams.
# Any characters can be deleted from either of the strings.
#
# Args:
#  a, b: the character strings to be compared.
#
# Return:
#  An integer corresponding to the number of deletions necessary
#  to make a and b anagrams, or NA if this is not possible.

c <- intersect(a, b)

ifelse (length(c) == 0, NA, length(a) - length(intersect(c, a)) + length(b) - length(intersect(c, b)))
 
}