library(readr)

data_store_us <- read_tsv("my-waypoints-dist-dur.tsv" )
all_waypoints_us <- union(data_store_us$waypoint1, data_store_us$waypoint2)

dist_matrix <- build_distance_matrix(data_store_us)

results <- list()
for (i in 1:10) {
  system.time(results[[i]] <- run_genetic_algorithm(dist_matrix, all_waypoints_us, generations=5000, population_size=100)) 
}
