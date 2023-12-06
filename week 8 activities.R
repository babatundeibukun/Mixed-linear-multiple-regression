install.packages("raster")
library(raster)
install.packages("scatterplot3d")
library(scatterplot3d)


clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)
head(clim)


# Convert 'altitude' from character to numeric
clim$altitude <- as.numeric(gsub(",","",clim$altitude))

# Convert 'p_mean' from character to integer
clim$p_mean <- as.numeric(gsub(",","",clim$p_mean))

str(clim)

#to draw the map of france 
G1 <- raster::getData(country = "France", level = 1)
G1


library(ggplot2)

ggplot() +
  
  geom_polygon(
    
    data = G1,
    
    aes(x = long, y = lat, group = group),
    
    colour = "grey10", fill = "#fff7bc"
    
  ) +
  
  geom_point(
    
    data = clim,
    
    aes(x = lon, y = lat),
    
    alpha = .5,
    
    size = 2
    
  ) +
  
  theme_bw() +
  
  xlab("Longitude") +
  
  ylab("Latitude") +
  
  coord_map()

#we have to exclude the two high mountain extremes
climfrar <- clim[1:34,]

#Linear model
model <- lm(t_mean~altitude+ lat + lon, climfrar )
model

summary(model)
# 
# Call:
#   lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)
# 
# Coefficients:
#   (Intercept)     altitude          lat          lon  
# 37.265036    -0.006414    -0.533960     0.032101 
# 


# Call:
#   lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.76492 -0.32755  0.04337  0.24787  2.58927 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#   altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#   lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#   lon          0.0321010  0.0395728   0.811    0.424    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7308 on 30 degrees of freedom
# Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
# F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12




Intercept (37.2650364):
  
#intercept : This is the expected value of t_mean when all predictors (altitude, lat, lon) are 0.
#altitude (-0.0064139):
#This suggests that for each unit increase in altitude, 
#t_mean decreases by approximately 0.00586 units,
#holding other variables constant.
#lat (-0.5339603)
#For each unit increase in latitude, t_mean decreases by about 0.53378 units, other things being equal.
#lon (0.0321010):
#The effect of longitude on t_mean is very small and not statistically 
#significant (p = 0.988). This means changes in longitude do not have a significant impact on t_mean, based on this model.


#exercise 2


# Linear model excluding 'lon'
model_2 <- lm(t_mean ~ altitude + lat, data = clim)
summary(model_2)
model_2


# Call:
#   lm(formula = t_mean ~ altitude + lat, data = clim)
# 
# Coefficients:
#   (Intercept)     altitude          lat  
# 37.202611    -0.005861    -0.533492 



#summary of the model
# Predicting for Mont-Ventoux and Pic-du-midi

#get the values for each mount.

# For Mont-Ventoux
altitude_mont_ventoux <- clim[clim$station == "Mont-Ventoux", "altitude"]
lat_mont_ventoux <- clim[clim$station == "Mont-Ventoux", "lat"]
# For Pic-du-midi
altitude_pic_du_midi <- clim[clim$station == "Pic-du-midi", "altitude"]
lat_pic_du_midi<- clim[clim$station == "Mont-Ventoux", "altitude"]

#Mean
pred_t_mean <- predict(model_2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)




#Exercise 3
scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(model_2)