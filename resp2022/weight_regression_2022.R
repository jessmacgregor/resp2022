setwd("C:/Users/macgr/Documents/Stony_Brook_Masters/Scallop_project/resp2022/resp2022")
data <- read.csv("scallop_pp_2022_fix.csv")

library(ggplot2)
ggplot(data, aes(x=scallop_wet_weight_g, y=dry_weight_total_g)) + geom_point() #pretty linear until the end
ggplot(data, aes(x=height_mm, y=dry_weight_total_g)) + geom_point() #even more linear wahoo


lmweight = lm(dry_weight_total_g~scallop_wet_weight_g, data = data) 
summary(lmweight) # 0.715 adjusted R^2 value, not terrible, p value very small, extremely significant
plot(lmweight$residuals, pch = 16, col = "red") #nice and random
qqnorm(lmweight$residuals) #ok not amazing at extremes
abline(qqline(lmweight$residuals))
plot(data$dry_weight_g, lmweight$residuals)
abline(h=0) #pretty good


lmheight = lm(dry_weight_total_g ~ height_mm, data = data)
summary(lmheight) #R^2 0.5163 not great, p value small, 
plot(lmheight$residuals, pch = 16, col = "red") #nice and random
#lower residuals but R^2 and p values worse

lmweightandheight = lm(dry_weight_total_g~scallop_wet_weight_g+ height_mm, data = data) 
summary(lmweightandheight) #adding didn't really improve R^2
plot(lmweightandheight$residuals, pch = 16, col = "red") #fine, pretty random
