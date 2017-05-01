
# typical problem in biology
# predict from a sequence of amino acids A, C, D, P, ...
# the secondary structure: helix (h), sheet (e), loop (-)

readSequences <- function(filename) {
  content <- readLines(filename)
  starts <- which(content == "<>")
  poss_ends <- which(content == "end" | content == "<end>" | content == "<>")
  observations <- list()
  hidden <- list()
  sequences <- list()
  for (i in 1:length(starts)) {
    s <- starts[i]
    e <- min(poss_ends[poss_ends > s])
    #print(s)
    #print(e)
    line <- strsplit(content[(s+1):(e-1)], " ")
    print (line)
    obs <- paste0(sapply(line, "[[", 1), collapse="")
    hid <- paste0(sapply(line, "[[", 2), collapse="")
    observations[[i]] <- obs
    hidden[[i]] <- hid
    #sequences[[i]] <- data.frame("hid"=hid, "obs"=obs)
  }
  return(list("obs"=observations, "hid"=hidden))
}

seq.data <- readSequences("../pss/protein-secondary-structure.train.txt")


## take sequences with 50 symbols or more and cut them

seq.demo <- list("obs"=list(), "hid"=list())
for (i in 1:length(seq.data$obs)) {
  if (nchar(seq.data$obs[[i]]) > 50) {
    j <- length(seq.demo$obs) + 1
    seq.demo$obs[[j]] <- substr(seq.data$obs[[i]], 1, 50)
    seq.demo$hid[[j]] <- substr(seq.data$hid[[i]], 1, 50)
  }  
}

demo.df <- as.data.frame(setNames(replicate(50,numeric(0), simplify = F), paste0("S", 1:50)))

for (i in 1:length(seq.demo$obs)) {
  o <- strsplit(seq.demo$obs[[i]], "")[[1]]
  h <- strsplit(seq.demo$hid[[i]], "")[[1]]
  demo.df[i,] <- as.character(o)
}

train.df <- demo.df[1:45,]
test.df <- demo.df[46:50,]

train.list <- seq.demo
train.list$obs <- train.list$obs[1:45]
train.list$hid <- train.list$hid[1:45]

test.list <- seq.demo
test.list$obs <- test.list$obs[46:50]
test.list$hid <- test.list$hid[46:50]

