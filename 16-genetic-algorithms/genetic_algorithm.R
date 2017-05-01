compute_fitness <- function(data_store, solution) {
  solution_fitness <- 0.0
  for (index in 2:length(solution)) {
    waypoint1 <- solution[index - 1]
    waypoint2 <- solution[index]
    solution_fitness <- solution_fitness + waypoint_distances(data_store, waypoint1, waypoint2)
  }
  return(solution_fitness)
}

generate_random_agent <- function(all_waypoints) {
  
  new_random_agent <- sample(all_waypoints, length(all_waypoints), replace=FALSE)
  return (new_random_agent)
}

mutate_agent <- function(agent_genome, max_mutations=3) {
  num_mutations <- sample(1:max_mutations, 1)
  for (mutation in 1:num_mutations) {
    swap_index1 <- sample(1:length(agent_genome), 1)

    swap_index2 <- sample((1:length(agent_genome))[-swap_index1], 1)
    
    temp_agent_genome <- agent_genome[swap_index1]
    agent_genome[swap_index1] <- agent_genome[swap_index2]
    agent_genome[swap_index2] <- temp_agent_genome
  }
  
  return (agent_genome)
}

shuffle_mutation <- function(agent_genome) { # TODO: check indeces

  start_index <- sample(1:length(agent_genome), 1)
  leng <- sample(1:min(length(agent_genome)-start_index + 1,20), 1)
  
  genome_subset <- agent_genome[start_index:(start_index + leng - 1)]
  first_part <- 0:(start_index-1)
  second_part <- c()
  if (start_index + leng <= length(agent_genome)) {
    second_part <- (start_index + leng):length(agent_genome)
  }
  agent_genome <- c(agent_genome[first_part], agent_genome[second_part])
  
  insert_index <- sample(0:(length(agent_genome)), 1)
  first_insert <- 0:insert_index
  last_insert <- c()
  if (insert_index < length(agent_genome)) {
    last_insert <- (insert_index+1):length(agent_genome)
  }
  agent_genome <- c(agent_genome[first_insert], genome_subset, agent_genome[last_insert])
  agent_genome <- agent_genome[!is.na(agent_genome)]
  
  return(agent_genome)
}

generate_random_population <- function(all_waypoints, pop_size) {
  
  random_population <- list()
  for (agent in 1:pop_size) {
    random_population[[agent]] <- generate_random_agent(all_waypoints)
  }
  return(random_population)
}

run_genetic_algorithm <- function(data_store, all_waypoints, generations=5000, population_size=100) {
  
  population_subset_size <- floor(population_size / 10)
  generations_10pct <- floor(generations / 10)
  
  population <- generate_random_population(all_waypoints, population_size)
  
  for (generation in 1:generations) {
    population_fitness <- list()
    for (agent_genome in population) {
      if (!contains(population_fitness, agent_genome)) {
        fit <- compute_fitness(data_store, agent_genome)
        agent_fitness <- list("agent"=agent_genome, "fitness"=unname(fit))
        population_fitness[[length(population_fitness)+1]] <- agent_fitness
      }
    }

    new_population <- list()
    
    sorted_population_fitness <- sort_by_fitness(population_fitness)
    for (i in 1:population_subset_size) {
      agent_genome <- sorted_population_fitness[[i]]$agent
      rank <- i
      if ((generation %% generations_10pct == 0 ||  generation == generations - 1) && rank == 1) {
        print(paste("Generation", generation, "best:", sorted_population_fitness[[i]]$fitness, "| Unique genomes:", length(population_fitness)))
      }      

      # Create 1 exact copy of each of the top road trips
      new_population[[length(new_population) + 1]] <- agent_genome
      # Create 2 offspring with 1-3 point mutations
      for (offspring in seq_len(2)) {
        new_population[[length(new_population) + 1]] <- mutate_agent(agent_genome, 3)
      }
      # Create 7 offspring with a single shuffle mutation
      for (offspring in seq_len(7)) {
        new_population[[length(new_population) + 1]] <- shuffle_mutation(agent_genome)
      }
    }
    population <-  new_population
  }
}