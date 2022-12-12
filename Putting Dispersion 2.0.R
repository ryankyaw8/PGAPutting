
library(tidyverse)
library(ggforce)

# Shotlink PGA Tour Data from 2020 season

shot_all <- readRDS("shot_df_2020_best.rds")

#### Converting XYZ coords to numeric var type #######

# Removing all the NA values from the xyz coordinates
shot_all = shot_all[!is.na(shot_all$X.Coordinate),]
shot_all = shot_all[!is.na(shot_all$Y.Coordinate),]
shot_all = shot_all[!is.na(shot_all$Z.Coordinate),]

# Finding the indices in shot_df_all with neg coords
neg_rows_x = grep("-",shot_all$X.Coordinate)
neg_rows_y = grep("-",shot_all$Y.Coordinate)
neg_rows_z = grep("-",shot_all$Z.Coordinate)

# Cleaning the xyz coordinate data 
# Removing "," and "-" to be able to convert to numeric var type 
shot_all$X.Coordinate = gsub(",", "", trimws(as.character(shot_all$X.Coordinate)))
shot_all$X.Coordinate = gsub("-", "", trimws(as.character(shot_all$X.Coordinate)))
shot_all$Y.Coordinate = gsub(",", "", trimws(as.character(shot_all$Y.Coordinate)))
shot_all$Y.Coordinate = gsub("-", "", trimws(as.character(shot_all$Y.Coordinate)))
shot_all$Z.Coordinate = gsub(",", "", trimws(as.character(shot_all$Z.Coordinate)))
shot_all$Z.Coordinate = gsub("-", "", trimws(as.character(shot_all$Z.Coordinate)))

# Making sure all the neg x coords are neg numeric var type
if(length(neg_rows_x) > 0) {
  shot_all$X.Coordinate[neg_rows_x] = -1*as.numeric(shot_all$X.Coordinate[neg_rows_x])
} 
shot_all$X.Coordinate = as.numeric(shot_all$X.Coordinate)

if(length(neg_rows_y) > 0) {
  shot_all$Y.Coordinate[neg_rows_y] = -1*as.numeric(shot_all$Y.Coordinate[neg_rows_y])
} 
shot_all$Y.Coordinate = as.numeric(shot_all$Y.Coordinate)

if(length(neg_rows_z) > 0) {
  shot_all$Z.Coordinate[neg_rows_z] = -1*as.numeric(shot_all$Z.Coordinate[neg_rows_z])
} 
shot_all$Z.Coordinate = as.numeric(shot_all$Z.Coordinate)

############################################################

# Get Shotink tracked putts 

shot_putt <- shot_all %>%
  filter(Strokes.Gained.Category == "Putting")

##### Get Hole Locations ######

shot_putt_filter <- shot_putt %>%
  filter(shot_putt$Hole.Score - shot_putt$Shot != 0 & 
           Distance.to.Hole.after.the.Shot < 12)

hole_loc_estimate = aggregate(cbind(shot_putt_filter$X.Coordinate,
                                    shot_putt_filter$Y.Coordinate,
                                    shot_putt_filter$Z.Coordinate),
                              list(shot_putt_filter$Course.Name,
                                   shot_putt_filter$Hole,
                                   shot_putt_filter$Round,
                                   shot_putt_filter$Year), 
                              median)
colnames(hole_loc_estimate) = c("Course","Hole","Round","Year","HoleX","HoleY","HoleZ")

hole_loc_estimate$CourseHoleRoundYear <- paste(hole_loc_estimate$Course,
                                               hole_loc_estimate$Hole,
                                               hole_loc_estimate$Round,
                                               hole_loc_estimate$Year)

hole_loc_estimate_new <- hole_loc_estimate %>%
  select(c("CourseHoleRoundYear", "HoleX", "HoleY", "HoleZ"))

###################################################

####### Figure out best and worst putters in 2020 ###########

shot_putt$Player.Name <- paste(shot_putt$Player.First.Name, shot_putt$Player.Last.Name)

total_putts <- aggregate(shot_putt$Year, 
                         list(shot_putt$Player.Name),
                         length)
colnames(total_putts) <- c("Player.Name", "Number.of.Putts")

sg_rankings <- aggregate(shot_putt$SG_best_norm,
                         list(shot_putt$Player.Name),
                         mean)
colnames(sg_rankings) <- c("Player.Name", "SGP")

sg_rankings_filter <- sg_rankings %>%
  left_join(total_putts, by = "Player.Name") %>%
  filter(Number.of.Putts >= 1000)

sg_rankings_filter_order <- sg_rankings_filter[order(-sg_rankings_filter$SGP), ]

top25 <- sg_rankings_filter_order$Player.Name[1:25]
bottom25 <- sg_rankings_filter_order$Player.Name[(nrow(sg_rankings_filter_order) - 24):
                                                 nrow(sg_rankings_filter_order)]

##################################

######## Get data from top 25 and worst 25 putters in 2020 ################

shot_all$Player.Name <- paste(shot_all$Player.First.Name, shot_all$Player.Last.Name)


shot_filter <- shot_all %>%
  filter(Player.Name %in% top25 | Player.Name %in% bottom25)

length(unique(shot_filter$Player.Name))

shot_filter$class[shot_filter$Player.Name %in% top25] <- "Top 25"
shot_filter$class[shot_filter$Player.Name %in% bottom25] <- "Bottom 25"

indices_to_edit <- which(shot_filter$Shot != 1)

shot_filter$X.Coordinate_prev <- NA
shot_filter$Y.Coordinate_prev <- NA

shot_filter$X.Coordinate_prev[indices_to_edit] <- shot_filter$X.Coordinate[indices_to_edit - 1]
shot_filter$Y.Coordinate_prev[indices_to_edit] <- shot_filter$Y.Coordinate[indices_to_edit - 1]

shot_putt <- shot_filter %>%
  filter(Strokes.Gained.Category == "Putting")

shot_putt$CourseHoleRoundYear <- paste(shot_putt$Course.Name,
                                             shot_putt$Hole,
                                             shot_putt$Round,
                                             shot_putt$Year)

shot_putt_holes <- shot_putt %>%
  left_join(hole_loc_estimate_new, by = "CourseHoleRoundYear")

shot_putt_holes <- shot_putt_holes %>%
  filter(HoleX != 0 | HoleY != 0 | HoleZ != 0)

shot_putt_holes$Xbegin <- shot_putt_holes$HoleX - shot_putt_holes$X.Coordinate_prev
shot_putt_holes$Ybegin <- shot_putt_holes$HoleY - shot_putt_holes$Y.Coordinate_prev

shot_putt_holes$Xafter <- shot_putt_holes$HoleX - shot_putt_holes$X.Coordinate
shot_putt_holes$Yafter <- shot_putt_holes$HoleY - shot_putt_holes$Y.Coordinate

shot_putt_holes$xcomp <- abs(-shot_putt_holes$Xbegin*shot_putt_holes$Xafter + shot_putt_holes$Ybegin*shot_putt_holes$Yafter)/
  (sqrt(shot_putt_holes$Xbegin^2 + shot_putt_holes$Ybegin^2))

shot_putt_holes$straightline <- sqrt((shot_putt_holes$Distance/12)^2 - shot_putt_holes$xcomp^2)

shot_putt_holes$ycomp <- shot_putt_holes$straightline - (shot_putt_holes$Distance.to.Pin/12)

shot_putt_holes$direction_coeff <- -shot_putt_holes$Xbegin*shot_putt_holes$Xafter +
  shot_putt_holes$Ybegin*shot_putt_holes$Yafter

# Q1
shot_putt_holes$direction[shot_putt_holes$direction_coeff < 0 & shot_putt_holes$Xbegin > 0 &
                            shot_putt_holes$Ybegin > 0] <- "left"
shot_putt_holes$direction[shot_putt_holes$direction_coeff > 0 & shot_putt_holes$Xbegin > 0 &
                            shot_putt_holes$Ybegin > 0] <- "right"

# Q2
shot_putt_holes$direction[shot_putt_holes$direction_coeff < 0 & shot_putt_holes$Xbegin < 0 &
                            shot_putt_holes$Ybegin > 0] <- "right"
shot_putt_holes$direction[shot_putt_holes$direction_coeff > 0 & shot_putt_holes$Xbegin < 0 &
                            shot_putt_holes$Ybegin > 0] <- "left"

# Q3
shot_putt_holes$direction[shot_putt_holes$direction_coeff < 0 & shot_putt_holes$Xbegin < 0 &
                            shot_putt_holes$Ybegin < 0] <- "right"
shot_putt_holes$direction[shot_putt_holes$direction_coeff > 0 & shot_putt_holes$Xbegin < 0 &
                            shot_putt_holes$Ybegin < 0] <- "left"

# Q4
shot_putt_holes$direction[shot_putt_holes$direction_coeff < 0 & shot_putt_holes$Xbegin > 0 &
                            shot_putt_holes$Ybegin < 0] <- "left"
shot_putt_holes$direction[shot_putt_holes$direction_coeff > 0 & shot_putt_holes$Xbegin > 0 &
                            shot_putt_holes$Ybegin < 0] <- "right"

shot_putt_holes$xcomp[shot_putt_holes$direction == "left"] <- 
  -1 * shot_putt_holes$xcomp[shot_putt_holes$direction == "left"]

# 4 to 6 feet

short_putts <- shot_putt_holes %>%
  filter(Distance.to.Pin > 48 & Distance.to.Pin < 72) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) 

ggplot(short_putts) +
  geom_point(aes(x = xcomp, y = ycomp, color = class)) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "darkolivegreen1",
                                        color = "darkolivegreen1")) +
  labs(title = "PGA Tour 4-6 Feet Missed Putt Dispersion",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")

# 10-15 feet

short_medium_putts <- shot_putt_holes %>%
  filter(Distance.to.Pin > 10*12 & Distance.to.Pin < 15*12) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) 

ggplot(short_medium_putts) +
  geom_point(aes(x = xcomp, y = ycomp, color = class)) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "darkolivegreen1",
                                        color = "darkolivegreen1")) +
  labs(title = "PGA Tour 10-15 Feet Missed Putt Dispersion",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")

# 20-25 feet

medium_putts <- shot_putt_holes %>%
  filter(Distance.to.Pin > 20*12 & Distance.to.Pin < 25*12) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) 

ggplot(medium_putts) +
  geom_point(aes(x = xcomp, y = ycomp, color = class)) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "darkolivegreen1",
                                        color = "darkolivegreen1")) +
  labs(title = "PGA Tour 20-25 Feet Missed Putt Dispersion",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")

# 35 plus feet

long_putts <- shot_putt_holes %>%
  filter(Distance.to.Pin > 35*12) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) 

ggplot(long_putts) +
  geom_point(aes(x = xcomp, y = ycomp, color = class)) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "darkolivegreen1",
                                        color = "darkolivegreen1")) +
  labs(title = "PGA Tour 35 Feet+ Missed Putt Dispersion",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")
