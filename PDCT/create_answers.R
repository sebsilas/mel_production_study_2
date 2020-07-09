
items <- read.csv("PDCT_IRT_ItemBank.csv")

# transform data frame to make a possible position for each tone

random.item.bank <- items
random.item.bank[c("tone.1", "tone.2", "tone.3")] <- 330
random.item.bank$answer <- NA

n <- 3
random.item.bank <- do.call("rbind", replicate(3, random.item.bank, simplify = FALSE))


# create main data frame
data.out <- data.frame(matrix(vector(),ncol=ncol(random.item.bank)))
names(data.out) <- names(random.item.bank)

for (i in 1:nrow(items)) {
  
  rows <- data.frame(random.item.bank[random.item.bank["item.no"]==i, ])

  rows[1,"answer"] <- 1
  rows[2,"answer"] <- 2
  rows[3,"answer"] <- 3
  rows[1,"tone.1"] <- random.item.bank[i,"level"]
  rows[2,"tone.2"] <- random.item.bank[i,"level"]
  rows[3,"tone.3"] <- random.item.bank[i,"level"]
  
  data.out <- rbind(data.out, rows)
  
}

write.csv(data.out, "ItemBank.csv", row.names = FALSE)
