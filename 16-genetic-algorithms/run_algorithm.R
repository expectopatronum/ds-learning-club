library(readr)

data_store_us <- read_tsv("my-waypoints-dist-dur.tsv" )
all_waypoints_us <- union(data_store_us$waypoint1, data_store_us$waypoint2)

system.time(run_genetic_algorithm(data_store_us, all_waypoints_us, generations=5000, population_size=100))
