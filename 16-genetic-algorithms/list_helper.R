# Find() returns first or last match

contains <- function(list, agent) {
  return (find_element(list, agent) > 0)
}

find_element <- function(list, agent) {
  # http://stackoverflow.com/questions/28254164/r-find-vector-in-list-of-vectors
  return (Position(function(x) identical(x$agent, agent), list, nomatch=0) )
}

sort_by_fitness <- function(list) {
  #browser()
  # http://stackoverflow.com/questions/24203361/r-sorting-list-by-values-in-nested-list
  return(list[order(rapply(list, "numeric", f=c))])
}