library(dplyr)

waypoint_distances <- function(data_store, waypoint1, waypoint2) {
  return(data_store_lookup(data_store, waypoint1, waypoint2, "distance_m"))
}


waypoint_durations <- function(data_store, waypoint1, waypoint2) {
  return(data_store_lookup(data_store, waypoint1, waypoint2, "duration_s"))  
}

data_store_lookup <- function(data_store, waypoint1, waypoint2, feature) {
  return(data_store[(data_store$waypoint1 == waypoint1 & data_store$waypoint2 == waypoint2) | (data_store$waypoint1 == waypoint2 & data_store$waypoint2 == waypoint1) , feature])
}

build_distance_matrix <- function(data_store) {
  all_waypoints <- union(data_store$waypoint1, data_store$waypoint2)
  n <- length(all_waypoints)
  dist_m <- matrix(rep(0, n*n), nrow=n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        dist_m[i, j] <- 0
      } else if (i < j) {
        dist_ij <- unlist(waypoint_distances(data_store, all_waypoints[i], all_waypoints[j]))
        dist_m[i, j] <- dist_ij
        dist_m[j, i] <- dist_ij
      }
    }
  }
  colnames(dist_m) <- rownames(dist_m) <- all_waypoints
  return(dist_m)
}
