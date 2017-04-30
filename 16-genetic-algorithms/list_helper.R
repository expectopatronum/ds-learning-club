# Find() returns first or last match

contains <- function(list, agent) {
  return (find_element(list, agent) > 0)
}

find_element <- function(list, agent) {
  return (Position(function(x) identical(x$agent, agent), list, nomatch=0) )
}

sort_by_fitness <- function(list) {
  #browser()
  return(list[order(rapply(list, "numeric", f=c))])
}