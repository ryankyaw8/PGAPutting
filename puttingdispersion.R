
library(tidyverse)
library(ggforce)

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


#########################################################

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

#############################

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

##################################

######## Denny McCarthy #############

shot_all$Player.Name <- paste(shot_all$Player.First.Name, shot_all$Player.Last.Name)

shot_Denny <- shot_all %>%
  filter(Player.Name == "Denny McCarthy")

indices_to_edit <- which(shot_Denny$Shot != 1)

shot_Denny$X.Coordinate_prev <- NA
shot_Denny$Y.Coordinate_prev <- NA

shot_Denny$X.Coordinate_prev[indices_to_edit] <- shot_Denny$X.Coordinate[indices_to_edit - 1]
shot_Denny$Y.Coordinate_prev[indices_to_edit] <- shot_Denny$Y.Coordinate[indices_to_edit - 1]

shot_Denny_putt <- shot_Denny %>%
  filter(Strokes.Gained.Category == "Putting")

shot_Denny_putt$CourseHoleRoundYear <- paste(shot_Denny_putt$Course.Name,
                                             shot_Denny_putt$Hole,
                                             shot_Denny_putt$Round,
                                             shot_Denny_putt$Year)

shot_Denny_putt <- shot_Denny_putt %>%
  left_join(hole_loc_estimate_new, by = "CourseHoleRoundYear")

shot_Denny_putt <- shot_Denny_putt %>%
  filter(HoleX != 0 | HoleY != 0 | HoleZ != 0)

shot_Denny_putt$Xbegin <- shot_Denny_putt$HoleX - shot_Denny_putt$X.Coordinate_prev
shot_Denny_putt$Ybegin <- shot_Denny_putt$HoleY - shot_Denny_putt$Y.Coordinate_prev

shot_Denny_putt$Xafter <- shot_Denny_putt$HoleX - shot_Denny_putt$X.Coordinate
shot_Denny_putt$Yafter <- shot_Denny_putt$HoleY - shot_Denny_putt$Y.Coordinate

# 8 - 12 feet 

shot_Denny_short <- shot_Denny_putt %>%
  filter(Distance.to.Pin > 96 & Distance.to.Pin < 144) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) %>%
  filter(abs(Xafter) < 10 | abs(Yafter) < 10)

shot_Denny_short$xcomp <- abs(-shot_Denny_short$Xbegin*shot_Denny_short$Xafter + shot_Denny_short$Ybegin*shot_Denny_short$Yafter)/
  (sqrt(shot_Denny_short$Xbegin^2 + shot_Denny_short$Ybegin^2))

shot_Denny_short$straightline <- sqrt((shot_Denny_short$Distance/12)^2 - shot_Denny_short$xcomp^2)

shot_Denny_short$ycomp <- shot_Denny_short$straightline - (shot_Denny_short$Distance.to.Pin/12)

shot_Denny_short <- shot_Denny_short %>%
  filter(abs(xcomp)< 10| abs(ycomp) < 10)

shot_Denny_short$direction_coeff <- -shot_Denny_short$Xbegin*shot_Denny_short$Xafter +
  shot_Denny_short$Ybegin*shot_Denny_short$Yafter

# Q1
shot_Denny_short$direction[shot_Denny_short$direction_coeff < 0 & shot_Denny_short$Xbegin > 0 &
                             shot_Denny_short$Ybegin > 0] <- "left"
shot_Denny_short$direction[shot_Denny_short$direction_coeff > 0 & shot_Denny_short$Xbegin > 0 &
                             shot_Denny_short$Ybegin > 0] <- "right"

# Q2
shot_Denny_short$direction[shot_Denny_short$direction_coeff < 0 & shot_Denny_short$Xbegin < 0 &
                             shot_Denny_short$Ybegin > 0] <- "right"
shot_Denny_short$direction[shot_Denny_short$direction_coeff > 0 & shot_Denny_short$Xbegin < 0 &
                             shot_Denny_short$Ybegin > 0] <- "left"

# Q3
shot_Denny_short$direction[shot_Denny_short$direction_coeff < 0 & shot_Denny_short$Xbegin < 0 &
                             shot_Denny_short$Ybegin < 0] <- "right"
shot_Denny_short$direction[shot_Denny_short$direction_coeff > 0 & shot_Denny_short$Xbegin < 0 &
                             shot_Denny_short$Ybegin < 0] <- "left"

# Q4
shot_Denny_short$direction[shot_Denny_short$direction_coeff < 0 & shot_Denny_short$Xbegin > 0 &
                             shot_Denny_short$Ybegin < 0] <- "left"
shot_Denny_short$direction[shot_Denny_short$direction_coeff > 0 & shot_Denny_short$Xbegin > 0 &
                             shot_Denny_short$Ybegin < 0] <- "right"

shot_Denny_short$xcomp[shot_Denny_short$direction == "left"] <- 
  -1 * shot_Denny_short$xcomp[shot_Denny_short$direction == "left"]

plot(shot_Denny_short$xcomp, shot_Denny_short$ycomp)

ggplot(shot_Denny_short) +
  geom_point(aes(x = xcomp, y = ycomp), color = "blue") +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "Denny McCarthy 8-12 ft Putting Dispersion on Misses",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")

# 18 - 22 feet 

shot_Denny_med <- shot_Denny_putt %>%
  filter(Distance.to.Pin > 18*12 & Distance.to.Pin < 22*12) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) %>%
  filter(abs(Xafter) < 10 | abs(Yafter) < 10)

shot_Denny_med$xcomp <- abs(-shot_Denny_med$Xbegin*shot_Denny_med$Xafter + shot_Denny_med$Ybegin*shot_Denny_med$Yafter)/
  (sqrt(shot_Denny_med$Xbegin^2 + shot_Denny_med$Ybegin^2))

shot_Denny_med$straightline <- sqrt((shot_Denny_med$Distance/12)^2 - shot_Denny_med$xcomp^2)

shot_Denny_med$ycomp <- shot_Denny_med$straightline - (shot_Denny_med$Distance.to.Pin/12)

shot_Denny_med <- shot_Denny_med %>%
  filter(abs(xcomp)< 10| abs(ycomp) < 10)

shot_Denny_med$direction_coeff <- -shot_Denny_med$Xbegin*shot_Denny_med$Xafter +
  shot_Denny_med$Ybegin*shot_Denny_med$Yafter

# Q1
shot_Denny_med$direction[shot_Denny_med$direction_coeff < 0 & shot_Denny_med$Xbegin > 0 &
                           shot_Denny_med$Ybegin > 0] <- "left"
shot_Denny_med$direction[shot_Denny_med$direction_coeff > 0 & shot_Denny_med$Xbegin > 0 &
                           shot_Denny_med$Ybegin > 0] <- "right"

# Q2
shot_Denny_med$direction[shot_Denny_med$direction_coeff < 0 & shot_Denny_med$Xbegin < 0 &
                           shot_Denny_med$Ybegin > 0] <- "right"
shot_Denny_med$direction[shot_Denny_med$direction_coeff > 0 & shot_Denny_med$Xbegin < 0 &
                           shot_Denny_med$Ybegin > 0] <- "left"

# Q3
shot_Denny_med$direction[shot_Denny_med$direction_coeff < 0 & shot_Denny_med$Xbegin < 0 &
                           shot_Denny_med$Ybegin < 0] <- "right"
shot_Denny_med$direction[shot_Denny_med$direction_coeff > 0 & shot_Denny_med$Xbegin < 0 &
                           shot_Denny_med$Ybegin < 0] <- "left"

# Q4
shot_Denny_med$direction[shot_Denny_med$direction_coeff < 0 & shot_Denny_med$Xbegin > 0 &
                           shot_Denny_med$Ybegin < 0] <- "left"
shot_Denny_med$direction[shot_Denny_med$direction_coeff > 0 & shot_Denny_med$Xbegin > 0 &
                           shot_Denny_med$Ybegin < 0] <- "right"

shot_Denny_med$xcomp[shot_Denny_med$direction == "left"] <- 
  -1 * shot_Denny_med$xcomp[shot_Denny_med$direction == "left"]

plot(shot_Denny_med$xcomp, shot_Denny_med$ycomp)

ggplot(shot_Denny_med) +
  geom_point(aes(x = xcomp, y = ycomp), color = "blue") +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "Denny McCarthy 18-22 ft Putting Dispersion on Misses",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")


# 30 plus feet 

shot_Denny_long <- shot_Denny_putt %>%
  filter(Distance.to.Pin > 30*12) %>%
  filter(Distance.to.Hole.after.the.Shot != 0) %>%
  filter(abs(Xafter) < 10 | abs(Yafter) < 10)

shot_Denny_long$xcomp <- abs(-shot_Denny_long$Xbegin*shot_Denny_long$Xafter + shot_Denny_long$Ybegin*shot_Denny_long$Yafter)/
  (sqrt(shot_Denny_long$Xbegin^2 + shot_Denny_long$Ybegin^2))

shot_Denny_long$straightline <- sqrt((shot_Denny_long$Distance/12)^2 - shot_Denny_long$xcomp^2)

shot_Denny_long$ycomp <- shot_Denny_long$straightline - (shot_Denny_long$Distance.to.Pin/12)

shot_Denny_long <- shot_Denny_long %>%
  filter(abs(xcomp)< 10| abs(ycomp) < 10)

shot_Denny_long$direction_coeff <- -shot_Denny_long$Xbegin*shot_Denny_long$Xafter +
  shot_Denny_long$Ybegin*shot_Denny_long$Yafter

# Q1
shot_Denny_long$direction[shot_Denny_long$direction_coeff < 0 & shot_Denny_long$Xbegin > 0 &
                            shot_Denny_long$Ybegin > 0] <- "left"
shot_Denny_long$direction[shot_Denny_long$direction_coeff > 0 & shot_Denny_long$Xbegin > 0 &
                            shot_Denny_long$Ybegin > 0] <- "right"

# Q2
shot_Denny_long$direction[shot_Denny_long$direction_coeff < 0 & shot_Denny_long$Xbegin < 0 &
                            shot_Denny_long$Ybegin > 0] <- "right"
shot_Denny_long$direction[shot_Denny_long$direction_coeff > 0 & shot_Denny_long$Xbegin < 0 &
                            shot_Denny_long$Ybegin > 0] <- "left"

# Q3
shot_Denny_long$direction[shot_Denny_long$direction_coeff < 0 & shot_Denny_long$Xbegin < 0 &
                            shot_Denny_long$Ybegin < 0] <- "right"
shot_Denny_long$direction[shot_Denny_long$direction_coeff > 0 & shot_Denny_long$Xbegin < 0 &
                            shot_Denny_long$Ybegin < 0] <- "left"

# Q4
shot_Denny_long$direction[shot_Denny_long$direction_coeff < 0 & shot_Denny_long$Xbegin > 0 &
                            shot_Denny_long$Ybegin < 0] <- "left"
shot_Denny_long$direction[shot_Denny_long$direction_coeff > 0 & shot_Denny_long$Xbegin > 0 &
                            shot_Denny_long$Ybegin < 0] <- "right"

shot_Denny_long$xcomp[shot_Denny_long$direction == "left"] <- 
  -1 * shot_Denny_long$xcomp[shot_Denny_long$direction == "left"]

plot(shot_Denny_long$xcomp, shot_Denny_long$ycomp)

ggplot(shot_Denny_long) +
  geom_point(aes(x = xcomp, y = ycomp), color = "blue") +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 2)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 3)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 4)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
  lims(x = c(-5, 5), y = c(-5, 5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "Denny McCarthy 30+ ft Putting Dispersion on Misses",
       subtitle = "Origin is Hole | Rings signify distance from hole in 1ft increments",
       x = "Feet Left/Right",
       y = "Feet Short/Long")






