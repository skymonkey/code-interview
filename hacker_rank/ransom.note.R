contains.token <- function(x, y, token){
# Function that checks whether a given named list
# element has the same or greater value in a second list.
#
# Args:
#  x: the first list. The value of a named element must be
#     greater or equal to the same named element in a second list.
#
#  y: the second list.
#
#  token: the name of the element in both lists.
#
# Returns:
#  A logical vector. True if the named element in x is >=
#  the same named element in y, false if not. If the same
#  element is not present in both, will return an empty logical
#  vector of length zero. 

    x[[token]] >= y[[token]]

}

ransom.note <- function(x, y){
# A kidnapper wrote a ransom note but is worried it will be traced back to him.
# He found a magazine and wants to know if he can cut out whole words from it
# and use them to create an untraceable replica of his ransom note.
# The words in his note are case-sensitive and he must use whole words available
# in the magazine, meaning he cannot use substrings or concatenation to create the words he needs.
# Given the words in the magazine and the words in the ransom note, print Yes if he
# can replicate his ransom note exactly using whole words from the magazine; otherwise, print No.
#
# Args:
#   x: string vector representing the magazine words.
#   y: string vector representing the note.
#
# Return:
#  "YES" if y can be constructed from x, otherwise "NO".

  m <- as.list(table(unlist(strsplit(x, " "))))
  n <- as.list(table(unlist(strsplit(y, " "))))

  result <- unlist(lapply(names(n), function(z) contains.token(m, n, z)))

  ifelse(length(result) == length(n) && all(result), "YES", "NO")

}

    