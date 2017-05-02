### packages and other sources

library(ggplot2)
library(microbenchmark)
source("genetic_algorithm.R")
source("data_store_helper.R")
source("list_helper.R")

### prep data

data_store_us <- read_tsv("my-waypoints-dist-dur.tsv" )
all_waypoints_us <- union(data_store_us$waypoint1, data_store_us$waypoint2)
population <- generate_random_population(all_waypoints_us, 100)
dist_matrix <- build_distance_matrix(data_store_us)

### run on data frame

system.time({
population_fitness <- list()
for (agent_genome in population) {
  if (!contains(population_fitness, agent_genome)) {
    fit <- compute_fitness(data_store_us, agent_genome)
    agent_fitness <- list("agent"=agent_genome, "fitness"=unname(fit))
    population_fitness[[length(population_fitness)+1]] <- agent_fitness
  }
}})

#   user  system elapsed 
#5.619   0.038   5.691 

### run on distance matrix

system.time({
  population_fitness <- list()
  for (agent_genome in population) {
    if (!contains(population_fitness, agent_genome)) {
      fit <- compute_fitness_fast(dist_matrix, agent_genome)
      agent_fitness <- list("agent"=agent_genome, "fitness"=unname(fit))
      population_fitness[[length(population_fitness)+1]] <- agent_fitness
    }
  }})

#    user  system elapsed 
# 0.042   0.000   0.044 

### microbenchmark

res <- microbenchmark(matrix=({
  population_fitness <- list()
  for (agent_genome in population) {
    if (!contains(population_fitness, agent_genome)) {
      fit <- compute_fitness_fast(dist_matrix, agent_genome)
      agent_fitness <- list("agent"=agent_genome, "fitness"=unname(fit))
      population_fitness[[length(population_fitness)+1]] <- agent_fitness
    }
  }}),
  df=({
    population_fitness <- list()
    for (agent_genome in population) {
      if (!contains(population_fitness, agent_genome)) {
        fit <- compute_fitness(data_store_us, agent_genome)
        agent_fitness <- list("agent"=agent_genome, "fitness"=unname(fit))
        population_fitness[[length(population_fitness)+1]] <- agent_fitness
      }
    }}),
  times = 100)

autoplot(res)