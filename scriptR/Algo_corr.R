#Hauteur = 19%
# Largeur = 12%

coord_split <- read_csv("study/split_data.csv")

num_test <- rep(0, 0)
tester_id <- rep(0, 0)
item_id <- rep (0, 0)
bloc <- data.frame(num_test, tester_id, item_id)

j =1
for (i in 1:nrow(coord_split)){
  if (!is.na(coord_split[i,"tester_quality_grade"])){
    bloc[j,"num_test"] <- i
    bloc[j,"tester_id"] <- coord_split[i,"tester_id"]
    bloc[j,"item_id"] <- coord_split[i,"item_id"]
    j = j+1
  }
  print(i)
}

for (i in 1:nrow(bloc)){
  if (bloc$tester_id[i] == "9577727a-fa45-4185-9778-4a56a41cf0f6" & bloc$item_id[i] == "5445f170-12c1-48fc-9563-3709dd2993b0"){
    pos1 <<- bloc$num_test[i]
    pos2 <<- bloc$num_test[i+1]
  }
}

filtre <- data.frame(coord_split[c(pos1:(pos2-1)),])

x_min_pct <- 20
x_max_pct <- 74
y_min_pct <- 10
y_max_pct <- 81

filtre <- filtre[-1,15:16]
img_Greece <- readJPEG("Plateaux_monde/4_Greece.jpg")
img <- img_Greece
imgR <- rasterGrob(img, interpolate=TRUE, height = 1, width = 1, x = 0.5)

heatmap <- ggplot2::ggplot(filtre, aes(x=gaze_x_percents, y=gaze_y_percents))+
  annotation_custom(imgR) +
  stat_density_2d(aes(fill = ..density..*10e03, alpha = ..density..*10e3), 
                  geom = "raster", 
                  contour = FALSE) +
  coord_fixed(xlim = c(0,100),ylim = c(100,0))+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0.00)+
  scale_alpha_continuous(range = c(0, 1), limits = c(0, 2),
                         guide = guide_none())


heatmap


plateau <- c(x_min_pct, x_max_pct, y_min_pct, y_max_pct)

corr_x <- rep(0, nrow(filtre))
corr_y <- rep(0, nrow(filtre))

i <- 1

for (i in 1:nrow(filtre)){
  if (filtre$gaze_x_percents[i] + 6 < x_min_pct){
    corr_x[i] <- y_min_pct - (filtre$gaze_y_percents[i] - 6)
  }
  else if (filtre$gaze_x_percents[i] - 6 > x_max_pct){
    corr_x[i] <- x_max_pct - (filtre$gaze_x_percents[i] + 6)
  } else {
    corr_x[i] <- 0
  }
}

for (i in 1:nrow(filtre)){
  if (filtre$gaze_y_percents[i] + 9 < y_min_pct){
    corr_y[i] <- y_min_pct - (filtre$gaze_y_percents[i] - 9)
  }
  else if (filtre$gaze_y_percents[i] - 9 > y_max_pct){
    corr_y[i] <- y_max_pct - (filtre$gaze_y_percents[i] + 9)
  } else {
    corr_y[i] <- 0
  }
}

corr <- filtre

corr$gaze_x_percents <- filtre$gaze_x_percents + max(corr_x)
corr$gaze_y_percents <- filtre$gaze_y_percents + min(corr_y)


heatmap_corr <- ggplot2::ggplot(corr, aes(x=gaze_x_percents, y=gaze_y_percents))+
  annotation_custom(imgR) +
  stat_density_2d(aes(fill = ..density..*10e03, alpha = ..density..*10e3), 
                  geom = "raster", 
                  contour = FALSE) +
  coord_fixed(xlim = c(0,100),ylim = c(100,0))+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0.00)+
  scale_alpha_continuous(range = c(0, 1), limits = c(0, 2),
                         guide = guide_none())


heatmap_corr
