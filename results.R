library(dplyr)
library(writexl)
library(ggplot2)

dir <- "/Users/sebsilas/Downloads/results_2/"

rds_list <- list.files(dir)


res.df <- bind_rows(lapply(rds_list, function (x) as.data.frame(t(unlist( readRDS(paste0(dir, x)) ))) ))

# remove spaces from column names

names(res.df) <- make.names(names(res.df),unique = TRUE)

# only grab participants who have a musical training score (i.e completed test)
res.df.complete <- res.df[complete.cases(res.df$GMS.Musical.Training), ]

# remove columns where all nas
res.df.complete <- res.df.complete  %>% select_if(~any(!is.na(.)))

# convert factor columns to numeric
cols.num <- c("demographics.age","GMS.Musical.Training", "GMS.Singing.Abilities", 
              names(res.df.complete)[grepl( "playback.count" , names( res.df.complete ) )] # any column containing "playback.count"
              )

res.df.complete[cols.num] <- sapply(res.df.complete[cols.num], function(x) as.numeric(as.character(x)))

# write data to excel file
write_xlsx(res.df.complete, "data.xlsx")


## sum scores

# sum of playbacks for each participant
pbs <- res.df.complete[ , grepl( "playback.count" , names( res.df.complete ) ) ]

pbs_sums <- rowSums(res.df.complete[ , grepl( "playback.count" , names( res.df.complete ) ) ])

res.df.complete$playbacks <- pbs_sums


## histograms

res.df.complete %>%
  ggplot( aes(x=demographics.age)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9)


res.df.complete %>%
  ggplot( aes(x=GMS.Musical.Training)) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9)


res.df.complete %>%
  ggplot( aes(x=GMS.Singing.Abilities)) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9)


res.df.complete %>%
  ggplot( aes(x=playbacks) ) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9)

# sanity checks / correlations

cor.test(res.df.complete$demographics.age, res.df.complete$GMS.Musical.Training)

cor.test(res.df.complete$GMS.Musical.Training, res.df.complete$GMS.Singing.Abilities)

cor.test(res.df.complete$demographics.age, res.df.complete$GMS.Singing.Abilities)

cor.test(res.df.complete$playbacks, res.df.complete$GMS.Musical.Training)

cor.test(res.df.complete$playbacks, res.df.complete$GMS.Singing.Abilities)

cor.test(res.df.complete$playbacks, res.df.complete$demographics.age)



