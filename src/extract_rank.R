rm(list = ls())

library(stringi)

master <- read.csv(file = "pers_phil_master.csv")
ch.df <- read.csv(file = "Ch_10_raw.csv")





outtie <- master %>%
  filter(master$lemma %in% ch.df$Latin) %>%
  mutate(odds = (10000 / freq) %>%
           round(0)
           )

colnames(outtie)[1] <- "rank"
colnames(outtie)[5] <- "odds ( 1 in x)"
colnames(outtie)[4] <- "Frequency per 10000"

write.csv(outtie, file = "ch_10_ranked.csv", quote = FALSE, row.names = FALSE)

master[which(str_detect(master$lemma, "salv") == TRUE), ]


