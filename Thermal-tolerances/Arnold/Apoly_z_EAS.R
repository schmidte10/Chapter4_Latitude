
dat <- read.csv("./Thermal-tolerances/Arnold/Apoly_dat.csv")
head(dat)

library(ggplot2)

plot(dat$Rate, dat$CTmax)
plot(1/dat$Rate, dat$CTmax)

ggplot(dat, (aes(x =1/Rate, y=CTmax))) + 
  geom_point() + 
  #geom_smooth(method ="lm", color ="red") + 
  geom_smooth(method ="lm", 
              color ="black", 
              formula = y ~ log(x)) + 
  theme_classic() +
  scale_y_continuous(limits =c(35, 40), 
                     breaks =seq(35, 40, 1)) + 
  annotate(geom ="text", x =0.7, y=37.5, 
           label ="y = -0.89ln(x) + 36.62")

plot(log10(1/dat$Rate), dat$CTmax) 

ggplot(dat, (aes(x =log10(1/Rate), y=CTmax))) + 
  geom_point() + 
  geom_smooth(method ="lm", 
              color ="dodgerblue4", 
              formula = y ~ (x)) + 
  theme_classic()

# calculate z as in Rezende et al 2014,2020,etc (temp ~ time)
# because ramp rate is a rate, take 1/rate to put in the same framework
# of increasing exposure duration = larger values.
# z = slope

coef(lm(dat$CTmax ~ log10(1/dat$Rate)))[[2]]

# -2.06

model1 <- lm(dat$CTmax ~ log(1/dat$Rate))
coef(lm(dat$CTmax ~ log(1/dat$Rate)))[[2]]

# -0.89



# calculate z as in Orsted et al 2022 (time ~ temp)
# because ramp rate is a rate, take 1/rate to put in the same framework 
# of increasing exposure duration.
# z = -1/slope
-1/(coef(lm(log10(1/dat$Rate) ~ dat$CTmax))[[2]])

# 2.347

-1/(coef(lm(log(1/dat$Rate) ~ dat$CTmax))[[2]]) 

# 1.02

# For the TDT_from_Dynamic Orsted et al 2022 R script to work properly, z needs to be a POSITIVE value. 


