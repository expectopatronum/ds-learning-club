waypoint_distances <- function(data_store, waypoint1, waypoint2) {
  return(data_store_lookup(data_store, waypoint1, waypoint2, "distance_m"))
}


waypoint_durations <- function(data_store, waypoint1, waypoint2) {
  return(data_store_lookup(data_store, waypoint1, waypoint2, "duration_s"))  
}

data_store_lookup <- function(data_store, waypoint1, waypoint2, feature) {
  return(data_store[(data_store$waypoint1 == waypoint1 & data_store$waypoint2 == waypoint2) | (data_store$waypoint1 == waypoint2 & data_store$waypoint2 == waypoint1) , feature])
}