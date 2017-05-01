library(seqHMM)

## "training"
params <- compute_prob(train.list$hid, train.list$obs)

#obs_list <- seqdef(train.df)
#hmm <- build_hmm(obs_list, transition_probs = t(params$A), initial_probs = rep(1/20,20), state_names = params$state_names, emission_probs = t(params$E))

## applying parameters to test data

obs_list_test <- seqdef(test.df, alphabet = params$obs_names)
hmm <- build_hmm(obs_list_test, transition_probs = t(params$A), initial_probs = params$pi, emission_probs = t(params$E), 
                 state_names = params$state_names)

hidden_paths(hmm)
test.list$hid

hmm1 <- build_hmm(obs_list_test, transition_probs = t(params$A), initial_probs = rep(1/3, 3), emission_probs = t(params$E), 
                  state_names = params$state_names)

hidden_paths(hmm1)

##### visualisations
ssplot(hmm)
plot(obs_list_test)

# heatmap
image(params$A, axes=FALSE)
#axis(1, labels = params$state_names, at = (1:3)/3)
#axis(2, labels = params$state_names, at = (1:3)/3)


