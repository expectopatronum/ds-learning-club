get_index <- function(states, s) {
  return(which(states == s))
}

compute_prob <- function(hidden, obs) {
  
  if (length(hidden) != length(obs)) {
    stop("hidden and obs need to be the same length")
  }
  
  hidden.states <- sort(unique(strsplit(paste(hidden, collapse=""), "")[[1]]))
  emiss.states <- sort(unique(strsplit(paste(obs, collapse=""), "")[[1]]))
  
  n <- length(hidden.states)
  m <- length(emiss.states)
  
  trans <- matrix(rep(0, n*n), nrow=n) # matrix A / P
  #init <- list() # vector pi
  emiss <- matrix(rep(0, m*n), nrow=n) # matrix O / E
  start_symbols <- matrix(rep(0, n), nrow=n)
  for (i in 1:length(hidden)) {
    # pi
    s1 <- substr(hidden[[i]], 1, 1)
    start_symbols[get_index(hidden.states, s1), 1] <- start_symbols[get_index(hidden.states, s1), 1] + 1
    # A
    for (j in 1:nchar(hidden[[i]])) {
      if (j < nchar(hidden[[i]])) {
        s1 <- substr(hidden[[i]], j, j)
        s2 <- substr(hidden[[i]], j+1, j+1)
        trans[get_index(hidden.states, s1), get_index(hidden.states, s2)] <- trans[get_index(hidden.states, s1), get_index(hidden.states, s2)] + 1
      }
    }
    # O
    for (j in 1:nchar(hidden[[i]])) {
      s1 <- substr(hidden[[i]], j, j)
      e1 <- substr(obs[[i]], j, j)
      emiss[get_index(hidden.states, s1), get_index(emiss.states, e1)] <- emiss[get_index(hidden.states, s1), get_index(emiss.states, e1)] + 1
    }
  }
  trans <- apply(trans, 2, function(x) { x / sum(x) } )
  emiss <- apply(emiss, 1, function(x) { x / sum(x) })
  #pi <- table(start_symbols)/length(hidden)
  start_symbols <- start_symbols + 1
  pi <- as.vector(start_symbols / sum(start_symbols))
  colnames(emiss) <- rownames(trans) <- colnames(trans) <- hidden.states
  rownames(emiss) <- emiss.states
  return (list("pi"=pi, "A"=trans, "E"=emiss, "state_names"=hidden.states, "obs_names"=emiss.states))
}

#params <- compute_prob(seq.data$hid, seq.data$obs)
