#exploratory plots to look at data 

#all trials box plot (excluding first round and other rogue ones)
#but with time codes labeled

all_slopes_label <- all_slopes_fix %>%
  mutate(label = ifelse(time_code == "0_10", "1", 
                          ifelse(time_code == "10_20", "2", 
                                 ifelse(time_code == "20_30", "3",
                                        ifelse(time_code == "30_40", "4", 
                                               ifelse(time_code == "40_50", "5", "6"))))))
July <- all_slopes_label %>%
  filter(month == "earlyJuly")
August <- all_slopes_label %>%
  filter(month == "August")
library(ggrepel) 
Julyplot <- ggplot(July, aes(factor(trial), 
                                        O2_consumption_mg_per_g_per_hr,label = label, 
                                        color = chamber, fill = chamber))+
  geom_boxplot(width=1, fill="white", outlier.shape=NA, 
               position = position_dodge2(1, preserve = "single")) + 
  geom_point(position=position_jitterdodge(0.5)) +geom_text_repel(fontface = "bold",position=position_jitter(width = 0, height = 0))
dev.new(width = 10, height = 10)
Julyplot + theme_classic() + facet_grid(~site) +
  ggtitle("Summer 2022 July respirometry data") +
  xlab("animal") + ylab("O2 consumption rate (mg/g/hr)")+dev.new(width=10,height=10)

Augplot <- ggplot(August, aes(factor(trial), 
                             O2_consumption_mg_per_g_per_hr,label = label, 
                             color = chamber, fill = chamber))+
  geom_boxplot(width=1, fill="white", outlier.shape=NA, 
               position = position_dodge2(1, preserve = "single")) + 
  geom_point(position=position_jitterdodge(0.5)) +geom_text_repel(fontface = "bold",position=position_jitter(width = 0, height = 0))
Augplot + theme_classic() + facet_grid(~site) +
  ggtitle("Summer 2022 August respirometry data") +
  xlab("animal") + ylab("O2 consumption rate (mg/g/hr)")+dev.new(width=10,height=10)

# make graph of just showing all points together separated by time codes

#this is all of them without adjusting for control or removing crazy slopes
G <- slopes2022 %>%
  ggplot(aes(x=time_code,y=slope,color = site)) + 
  geom_boxplot() + geom_point(position=position_jitterdodge(0.1))
G + theme_classic()

#here it is with the adjusted data
A <- all_slopes_label %>%
  ggplot(aes(x=label, y=O2_consumption_mg_per_g_per_hr))+
  geom_violin() + geom_jitter(height = 0, width = 0.1, aes(color = factor(site)))+
  scale_color_manual(name = "site", values = c("pink","red","orange","darkgreen","darkblue"))
A + theme_classic() + labs(title = "Variability in O  consumption by time period") +
  labs(x="Time period (grouped by 10 min)", y = bquote('Oxygen consumption '(mg~g^-1~hr^-1))) 

#lets run an ANOVA to see if there is sig diff bw trial times
#this is after crazy outliers have been removed from malfunctioning equipment 
#and values have been adjusted for controls AND time point 1 has been removed

hist(all_slopes_label$O2_consumption_mg_per_g_per_hr) #right skew but not terrible
# Build the linear model
LM  <- lm(O2_consumption_mg_per_g_per_hr ~ time_code, data = all_slopes_label)
# Create a QQ plot of residuals
library(ggpubr)
library(rstatix)
ggqqplot(residuals(LM)) #not amazing low but not terrible
#outliers - only 5, no extreme outliers
outliers <- all_slopes_label%>% 
  group_by(time_code) %>%
  identify_outliers(O2_consumption_mg_per_g_per_hr)
#regular old ANOVA - they aint different DONT USE THIS
oneway.test(O2_consumption_mg_per_g_per_hr ~ time_code,
            data = all_slopes_av2022,
            var.equal = TRUE # assuming equal variances
)

#should actually be a repeated measures ANOVA 
#need to omit any incomplete data or it won't work
#(https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/)
all_slopes_label$IDkey <- as.factor(all_slopes_label$IDkey)
all_slopes_label$time_code <- as.factor(all_slopes_label$time_code)

forANOVA <- all_slopes_label %>% 
  select(site, IDkey, time_code, O2_consumption_mg_per_g_per_hr) %>%
  na.omit()
forANOVA%>%
  group_by(time_code) %>%
  shapiro_test(O2_consumption_mg_per_g_per_hr)
##qqplot preferred because S-W test becomes very sensitive to minor deviations
#from normality at high sample sizes

ggqqplot(forANOVA, "O2_consumption_mg_per_g_per_hr", facet.by = "time_code")
#look good

#now need to remove NAs from wide from matrix bc this is nec
#need complete sets to run repeated measures ANOVA
forSKI <- pivot_wider(forANOVA, names_from = time_code, values_from = O2_consumption_mg_per_g_per_hr, -IDkey) %>%
  ungroup() %>%
  select(-chamber, -site, -trial, -month) %>%
  relocate('30_40') %>% relocate('20_30') %>% relocate('10_20')
test <- forSKI %>%
  na.omit()
test <- test %>%
  mutate(ID = row_number()) %>%
  gather(key = "time", value = "slope", '10_20', '20_30','30_40','40_50','50_60',) %>%
  convert_as_factor(time, ID)
head(test, 3)
str(test)

res.aov <- anova_test(data = test, dv = slope, wid = ID, within = time)
get_anova_table(res.aov)

#great! p value is .933 wahooooo






#quick little mean + sd finding so can put over other plot

#mean + sd of total dry weight
mean(all_slopes_av2022$dry_weight_total_g)
sd(all_slopes_av2022$dry_weight_total_g)

#mean + sd of O2 consumed 
summary <- all_slopes_av2022 %>%
  drop_na(O2_consumption_mg_per_g_per_hr) %>%
  ungroup() %>%
  summarize(mean = mean(O2_consumption_mg_per_g_per_hr), 
            sd = sd(O2_consumption_mg_per_g_per_hr))

#in micromoles
(summary$mean)*(1000/32)
