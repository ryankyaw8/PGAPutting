
library(tidyverse)

putt_baseline <- read.csv("pga_tour_baselines.csv")
putt_baseline$Distance <- putt_baseline$Distance/12

putt_baseline <- putt_baseline %>%
  select(c("Distance", "Putt"))

shot_2020 <- readRDS("shot_df_2020_best.rds")

# PUtting Make Probabilities

shot_2020_putting <- shot_2020  %>%
  filter(Strokes.Gained.Category == "Putting")

shot_2020_putting$Distance.to.Pin.ft <- round(shot_2020_putting$Distance.to.Pin/12, 0)

# 1 -> Make
# 0 -> Miss

shot_2020_putting$Make[shot_2020_putting$Distance.to.Hole.after.the.Shot == 0] <- 1
shot_2020_putting$Make[shot_2020_putting$Distance.to.Hole.after.the.Shot != 0] <- 0

total_putts <- aggregate(shot_2020_putting$Year, 
                         list(shot_2020_putting$Distance.to.Pin.ft),
                         length)
colnames(total_putts) <- c("Distance", "TotalPutts")

made_putts <- aggregate(shot_2020_putting$Make,
                        list(shot_2020_putting$Distance.to.Pin.ft),
                        sum)
colnames(made_putts) <- c("Distance", "MadePutts")

putt_probs <- made_putts %>%
  left_join(total_putts, by = "Distance") %>%
  left_join(putt_baseline, by = "Distance") %>%
  filter(Distance <= 60)

putt_probs$MakeProb <- round(putt_probs$MadePutts/putt_probs$TotalPutts, 3)

putt_probs$Class <- "All"

plot(putt_probs$Distance, putt_probs$MakeProb)

# Finding best and worst putters 

shot_2020_putting$Player.Name <- paste(shot_2020_putting$Player.First.Name, shot_2020_putting$Player.Last.Name)

total_putts_2 <- aggregate(shot_2020_putting$Year, 
                         list(shot_2020_putting$Player.Name),
                         length)
colnames(total_putts_2) <- c("Player.Name", "Number.of.Putts")

sg_rankings <- aggregate(shot_2020_putting$SG_best_norm,
                         list(shot_2020_putting$Player.Name),
                         mean)
colnames(sg_rankings) <- c("Player.Name", "SGP")

sg_rankings_filter <- sg_rankings %>%
  left_join(total_putts_2, by = "Player.Name") %>%
  filter(Number.of.Putts >= 1000)

sg_rankings_filter_order <- sg_rankings_filter[order(-sg_rankings_filter$SGP), ]

top25 <- sg_rankings_filter_order$Player.Name[1:25]
bottom25 <- sg_rankings_filter_order$Player.Name[(nrow(sg_rankings_filter_order) - 24):
                                                   nrow(sg_rankings_filter_order)]


# Putting Make Probs (Best)

shot_2020$Player.Name <- paste(shot_2020$Player.First.Name, shot_2020$Player.Last.Name)

shot_2020_putting_filter <- shot_2020  %>%
  filter(Strokes.Gained.Category == "Putting") %>%
  filter(Player.Name %in% top25 | Player.Name %in% bottom25)

shot_2020_putting_filter$Distance.to.Pin.ft <- round(shot_2020_putting_filter$Distance.to.Pin/12, 0)

# 1 -> Make
# 0 -> Miss

shot_2020_putting_filter$Make[shot_2020_putting_filter$Distance.to.Hole.after.the.Shot == 0] <- 1
shot_2020_putting_filter$Make[shot_2020_putting_filter$Distance.to.Hole.after.the.Shot != 0] <- 0

shot_2020_putting_filter$Class[shot_2020_putting_filter$Player.Name %in% top25] <- "Best"
shot_2020_putting_filter$Class[shot_2020_putting_filter$Player.Name %in% bottom25] <- "Worst"

total_putts_filter <- aggregate(shot_2020_putting_filter$Year, 
                              list(shot_2020_putting_filter$Distance.to.Pin.ft,
                                   shot_2020_putting_filter$Class),
                              length)
colnames(total_putts_filter) <- c("Distance", "Class", "TotalPutts")

total_putts_filter2 <- total_putts_filter %>%
  filter(Distance <= 60)

made_putts_filter <- aggregate(shot_2020_putting_filter$Make,
                             list(shot_2020_putting_filter$Distance.to.Pin.ft,
                                  shot_2020_putting_filter$Class),
                             sum)
colnames(made_putts_filter) <- c("Distance", "Class", "MadePutts")

made_putts_filter2 <- made_putts_filter %>%
  filter(Distance <= 60)

putt_probs_filter <- made_putts_filter2 %>%
  left_join(total_putts_filter2, by = c("Distance", "Class"))

putt_probs_filter$MakeProb <- putt_probs_filter$MadePutts/putt_probs_filter$TotalPutts

# Data Combination

putt_probs <- putt_probs %>%
  select(-c("Putt"))

all <- rbind(putt_probs, putt_probs_filter)

ggplot(data = subset(all, all$Distance <= 40)) +
  geom_smooth(aes(x = Distance, y = MakeProb, color = Class))







                      