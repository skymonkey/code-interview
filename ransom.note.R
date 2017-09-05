contains.token <- function(x, y, token){

    x[[token]] >= y[[token]]

}

ransom.note <- function(x, y){


  m <- as.list(table(unlist(strsplit(x, " "))))
  n <- as.list(table(unlist(strsplit(y, " "))))

  result <- unlist(lapply(names(n), function(z) contains.token(m, n, z)))

  ifelse(length(result) == length(n) && all(result), "YES", "NO")

}

    