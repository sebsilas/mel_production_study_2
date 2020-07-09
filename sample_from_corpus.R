library(dplyr)
library(sjmisc)

options(scipen = 999)
berkowitz.item.bank <- readRDS("Berkowitz_Item_Bank.RDS")
berkowitz.item.bank[, c("Freq", "N", "rel.freq", "log.freq")] <- mutate_all(berkowitz.item.bank[, c("Freq", "N", "rel.freq", "log.freq")], function(x) as.numeric(as.character(x)))


item.sampler <- function(item_bank, samples_per_iteration) {
  
  max.N <- max(item_bank$N)
  
  sample <- item_bank[1, ]

  for (i in 1:max.N) {

    N.subset <- item_bank[item_bank[, "N"] == i, ]
    rand.samp.i <- sample(1:nrow(N.subset), samples_per_iteration, replace = FALSE)
    rand.samp <- N.subset[rand.samp.i, ]
    sample <- rbind(sample, rand.samp)
      
  }
  
  sample[2:nrow(sample),]
  
}

user.sample <- item.sampler(berkowitz.item.bank, 2)


berkowitz.stimuli <- readRDS("Berkowitz_Relative.RDS")

rhythmic.sampler <- function(corpus, no_trials) {
  
  corpus_length <- length(corpus)
  
  samp <- sample(1:corpus_length, no_trials, replace = FALSE)
  
  sample <- corpus[samp]
  
  # randomly assign a N to cap the trial at
  
  #sample$length -> 1:nrow(sample)
  
  sample
    
}

samp <- rhythmic.sampler(berkowitz.stimuli, 20)

samp2 <- data.frame(matrix(unlist(samp), nrow=length(samp), byrow=T),stringsAsFactors=FALSE)

