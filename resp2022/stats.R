#Modelling - first ANOVA then LMM
library(car)
#first lets look at all data
hist(all_slopes_av2022$O2_consumption_mg_per_g_per_hr)
hist(log(all_slopes_av2022$O2_consumption_mg_per_g_per_hr))
#fairly normal, let's fit the ANOVA and then check qqplots etc,

#cant use OP
noOP <- all_slopes_av2022 %>%
  filter(site.x != "OrientPoint")

noOP %>% ungroup() %>% count(site.x, month.x, sort = TRUE)
#this shows me that my design is unbalanced. have to use either type 1, 2 or 3 SS
#ANOVA ignoring OP - but need to specify type 1 2 or 3 SS

res.aov2 <- aov(O2_consumption_mg_per_g_per_hr ~ site.x + month.x, 
                data = noOP)
summary(res.aov2)

#type2 SS - sig intxn so shouldnt use this
Anova(lm(O2_consumption_mg_per_g_per_hr ~ site.x * month.x, data=noOP, type=2))

#lets try type3
# https://rpubs.com/WhataBurger/Anovatype3 

#need to actually be using averages for each individual to avoid pseudorep
avnoOP <- allav %>% 
  filter(site != "OrientPoint") 
fit.type3 <- Anova(lm(average_O2_consump_mg_g_hr~site*month,
                      contrasts=list(site='contr.sum', month ='contr.sum'), data = avnoOP),
                   type='III')
summary(fit.type3)
fit.type3
#post-hoc to make pairwise comparisons - can't use tukeys bc unequal sample sizes
#so need to use tukey kramer
type3lm <- lm(average_O2_consump_mg_g_hr~site*month,
              contrasts=list(site='contr.sum', month ='contr.sum'), data = avnoOP)
TukeyHSD(aov(type3lm))

#then test all model assumptions

#normality - not using this bc sample size too high!! qqplots are fine
shapiro.test(residuals(type3lm))
#independence = not sig
library(car)
durbinWatsonTest(type3lm, alternative="two.sided",data=avnoOP)
#equal variance - not significant. we are good here!
leveneTest(average_O2_consump_mg_g_hr~site*month, data=avnoOP)
#residuals - seem to diverge from normality at higher values but not terrible overall
plot(type3lm)
#plots all look good

#https://www.r-bloggers.com/2022/05/two-way-anova-example-in-r-quick-guide/
#finish doing all of that 

#Type 1 2 and 3 SS explained
#https://www.r-bloggers.com/2011/03/anova-%E2%80%93-type-iiiiii-ss-explained/


#ok moving away from ANOVAS to try LMMs
#following steps from here (https://ourcodingclub.github.io/tutorials/mixed-models/#what) 

#first lets make a big ole df. **Including OP here bc allowed to I think*

#making a df with all variables of interest and resp rates, (temp, DO, site)
justDO <- slopesDO %>%
  select(IDkey, DO24, DO48, DO2weeks)
all <- cbind(slopestemp, justDO)

#check to make sure order is correct
ifelse(all$IDkey==all$IDkey,"Yes","No")
#add GI
all1 <- all %>%
  select(unique(colnames(.))) %>%
  mutate(GI = (dry_weight_gonad_g/dry_weight_total_g)*100) 
#average individuals and just keep unique (so each is an average)
allav <- all1 %>%
  distinct(IDkey, .keep_all = TRUE)
allav <- allav %>%
  filter(!is.na(average_O2_consump_mg_g_hr))

#1 - look at data
hist(allav$average_O2_consump_mg_g_hr) #resp rates
hist(log(allav$average_O2_consump_mg_g_hr))
#seems fairly close to normal but a bit of a right skew, longer tail
hist(allav$CI)
hist(allav$GI)

#3scale explanatory variables - all of them?
allav$temp48scale <- scale(allav$temp48, center = TRUE, scale = TRUE)
allav$DO48scale <- scale(allav$DO48, center = TRUE, scale = TRUE)
allav$CIscale <- scale(allav$CI, center = TRUE, scale = TRUE)
allav$GIscale <- scale(allav$GI, center = TRUE, scale = TRUE)

#lms for predictor variables

#temps 48 hours before 
temp48lm <- lm(average_O2_consump_mg_g_hr ~ temp48scale, data = allav)
summary(temp48lm) #R squared pretty much nonexistant here. 0.03952
 ggplot(allav, aes(x = temp48scale, y = average_O2_consump_mg_g_hr)) +
    geom_point() +
    geom_smooth(method = "lm") #from graph actually see it goes down a bit with higher temps
 plot(temp48lm, which = 1) # residuals weird bc nested by date
 plot(temp48lm, which = 2)  # qqplot a bit off at the extremes, but that's often the case; again doesn't look too bad
 
#DO
DO48lm <- lm(average_O2_consump_mg_g_hr ~ DO48scale, data = allav)
summary(DO48lm) #even lower adj R^2 -0.003478
 ggplot(allav, aes(x = DO48scale, y = average_O2_consump_mg_g_hr)) +
    geom_point() +
    geom_smooth(method = "lm") #from graph actually see it goes up a bit with higher temps
 plot(DO48lm, which = 1) #residuals, weird bc nested by date but pretty flat
 plot(DO48lm, which = 2) #qqplot, better than temp, good
 
#CI
CIlm <- lm(average_O2_consump_mg_g_hr ~ CIscale, data = allav)
summary(CIlm) #R^2 0.0134
ggplot(allav, aes(x = CI, y = average_O2_consump_mg_g_hr)) +
  geom_point() +
  geom_smooth(method = "lm") #from graph see it goes down with increasing CI
plot(CIlm, which = 1) #residuals good and flat 
plot(CIlm, which = 2) #qq plot good

#GI
GIlm <- lm(average_O2_consump_mg_g_hr ~ GIscale, data = allav)
summary(GIlm) #highest yet - 0.1227
ggplot(allav, aes(x = GI, y = average_O2_consump_mg_g_hr)) +
  geom_point() +
  geom_smooth(method = "lm") #from graph see it goes up with increasing CI
plot(GIlm, which = 1) #good and flat, some clumping at front
plot(GIlm, which = 2) #qq plot good, a little off at high extremes 

#independence testing
boxplot(average_O2_consump_mg_g_hr ~ site, data = allav) #site
boxplot(average_O2_consump_mg_g_hr ~ month, data = allav) #month
boxplot(average_O2_consump_mg_g_hr ~ Chamber_no, data = allav) #chamber

#modify model - what if we added chamber as fixed effect
testlm <- lm(average_O2_consump_mg_g_hr ~ temp48scale + Chamber_no, data = allav)
summary(testlm)
#so chamber has an effect (particularly chamber 2). what if we control for chamber?

#here we go - Mixed effects model! 
library(lme4)

#first just temp controlling for site variation
testmixed.lmer <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (1|site), data = allav)
summary(testmixed.lmer)
#how much variance does differences in site explain
0.004382/(0.004382+0.882469) #0.5%
plot(testmixed.lmer) #no crazy patterns
qqnorm(resid(testmixed.lmer))
qqline(resid(testmixed.lmer)) #totally fine

#FULL MODEL - including site, month, temp, DO, CI, GI as fixed
#random effects - chamber
mixed1 <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
              (CIscale) + (GIscale) + (site) + (month) + (1|Chamber_no), data = allav, REML = TRUE)  
summary(mixed1) # I truly do not know how to interpret this. why is it not showing
#me cowyard as a site?

Anova(mixed1) #from ANOVA, only GI significant. but is this legit bc
#unequal sample sizes?? This ANOVA is a Wald test - tells us how confident we are on the
#effect of different fixed effects on resp rate. 

#how to show this (source: https://lmudge13.github.io/sample_code/mixed_effects.html)
library(sjPlot)
sjPlot::plot_model(mixed1, 
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Site, enviro factors, and CI/GI status on scallop respiration rate") #coool

sjPlot::tab_model(mixed1)

#Now time for model selection. 
install.packages("AICcmodavg")
library(AICcmodavg)

#First things first- sorting out random effects. are individual and chamber both important?
#Let's compare with and without chamber using restricted maximum likelihood (assumes structure is correct)
#need to use REML bc ML variance estimates are biased

chamber <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
                  (CIscale) + (GIscale) + (site) + (month) + (1|Chamber_no), data = allav)  

nochamber <- lm(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
                 (CIscale) + (GIscale) + (site) + (month), data = allav)  
anova(chamber, nochamber, REML = TRUE) #sig diff
AIC(chamber, nochamber) #with chamber is lower by 10

#is it the same when I use a likelihood ratio test 
#(https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#when-can-i-include-a-predictor-as-both-fixed-and-random)

m2 <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
             (CIscale) + (GIscale) + (site) + (month) + (1|Chamber_no) + (0+temp48scale|Chamber_no), data = allav,REML=FALSE)
m1 <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
             (CIscale) + (GIscale) + (site) + (month) + (1|Chamber_no), data = allav,REML=FALSE)
m0 <- lm(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
           (CIscale) + (GIscale) + (site) + (month), data = allav) 
anova(m2, m1, m0)
#we should deffo include chamber. 


#next compare fixed effects - use likelihood ration test via AICc
#use AICc bc small sample size
install.packages("MuMIn")
require(MuMIn)
library(lme4)
globalmodel <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
                      (CIscale) + (GIscale) + (site) + (month) + (1|Chamber_no),
                      data = allav, na.action = na.fail)

combinations <- dredge(globalmodel, trace = TRUE, rank = "AICc", REML = FALSE)
print(combinations) #with AICc DO and GI in best model (over 2 AIC diff)


fmList <- get.models(combinations, 1:4)
summary(model.avg(fmList)) #averaging top 4 model weights, gives estimate and
#p values.

#plot model estimates with data also good table here
#(https://lmudge13.github.io/sample_code/mixed_effects.html)
install.packages("effects")
library(effects)
#refit model with REML true for parameter estimates
DOGI <- lmer(average_O2_consump_mg_g_hr ~ DO48scale + (GIscale) + (1|Chamber_no),
             data = allav, na.action = na.fail, REML = TRUE)
#model equation
install.packages("equatiomatic")
library(equatiomatic)
equatiomatic::extract_eq(DOGI)

#effect sizes
effects_DO <- effects::effect(term= "DO48scale", mod= DOGI)
summary(effects_DO)
DOX <- as.data.frame(effects_DO)
library(ggplot2)
DO_plot <- ggplot() + 
  geom_point(data=allav, aes(DO48scale, average_O2_consump_mg_g_hr)) + 
  geom_point(data=DOX, aes(x=DO48scale, y=fit), color="blue") +
  geom_line(data=DOX, aes(x=DO48scale, y=fit), color="blue") +
  geom_ribbon(data= DOX, aes(x=DO48scale, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="DO (centered & scaled)", y="Scallop respiration")
DO_plot

effects_GI <- effects::effect(term= "GIscale", mod= DOGI)
summary(effects_GI)
GIX <- as.data.frame(effects_GI)
GI_plot <- ggplot() + 
  geom_point(data=allav, aes(GIscale, average_O2_consump_mg_g_hr)) + 
  geom_point(data=GIX, aes(x=GIscale, y=fit), color="blue") +
  geom_line(data=GIX, aes(x=GIscale, y=fit), color="blue") +
  geom_ribbon(data= GIX, aes(x=GIscale, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="GI (centered & scaled)", y="Scallop respiration")
GI_plot

#ok cool have effect size estimates and graphed etc. re go through all of these tomorrow 

#BIC for top 3
DOGI <- lmer(average_O2_consump_mg_g_hr ~ DO48scale + (GIscale) + (1|Chamber_no),
                    data = allav, na.action = na.fail)
GI <- lmer(average_O2_consump_mg_g_hr ~ GIscale + (1|Chamber_no),
             data = allav, na.action = na.fail)

month <- lmer(average_O2_consump_mg_g_hr ~ month + (1|Chamber_no),
             data = allav, na.action = na.fail)

BIC(DOGI, GI, month) #even more similar, what to do.
#heather says convert AICs into model weights and do model averaging
#finding R squared for best model
install.packages("rsq")
library(rsq)
rsq.lmm(DOGI,adj=TRUE)
#random effects explain more of the model than fixed big yikes
#plotting effect sizes #WHAT DO THEY MEAN
sjPlot::plot_model(DOGI,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect GI and DO on respiration rate")

sjPlot::tab_model(mixed1)
sjPlot::tab_model(DOGI)

#for brad - redoing without orient with chla. 
#take all av, get rid of OP, add chla based on conditions and scale it
allavnoop <- allav %>%
  filter(site.y != "OrientPoint") %>%
  mutate(chla48 = ifelse(sitemonth == "Cowyard_earlyJuly", cmeanJ_CY48,
                ifelse(sitemonth == "Mattituck_earlyJuly", cmeanJ_MT48, 
                ifelse(sitemonth == "Bullhead_earlyJuly", cmeanJ_BH48,
                ifelse(sitemonth == "NWHarbor_earlyJuly", cmeanJ_NW48,
               ifelse(sitemonth == "Cowyard_August", cmeanA_CY48,
                ifelse(sitemonth == "Mattituck_August", cmeanA_MT48, 
                ifelse(sitemonth == "Bullhead_August", cmeanA_BH48,cmeanA_NW48))))))))
#scale and center
allavnoop$chla48scale <- scale(allavnoop$chla48, center = TRUE, scale = TRUE)
allavnoop$chla48scale

mixed2 <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
                 (CIscale) + (GIscale) + (chla48scale) + (site) + (month) + (1|Chamber_no), data = allavnoop, REML = TRUE)  
summary(mixed2) # I truly do not know how to interpret this. why is it not showing
#me cowyard as a site?
Anova(mixed2) #from ANOVA, only GI significant. but is this legit bc
#unequal sample sizes?? This ANOVA is a Wald test - tells us how confident we are on the
#effect of different fixed effects on resp rate. 

#how to show this (source: https://lmudge13.github.io/sample_code/mixed_effects.html)
library(sjPlot)
sjPlot::plot_model(mixed2, 
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Site, enviro factors, and CI/GI status on scallop respiration rate") #coool

sjPlot::tab_model(mixed2)

#next compare fixed effects - use likelihood ration test via AICc
#use AICc bc small sample size
require(MuMIn)
library(lme4)
globalmodel2 <- lmer(average_O2_consump_mg_g_hr ~ temp48scale + (DO48scale) +
                      (CIscale) + (GIscale) + (chla48scale) + (site) + (month) + (1|Chamber_no),
                    data = allavnoop, na.action = na.fail)

combinations2 <- dredge(globalmodel2, trace = TRUE, rank = "AICc", REML = FALSE)
print(combinations2) #with AICc GI, month, site in best model (over 2 AIC diff)
#different when we remove Orient Point!

fmList2 <- get.models(combinations2, 1:4)
summary(model.avg(fmList)) #averaging top 4 model weights, gives estimate and
#p values.

#plot model estimates with data also good table here
#(https://lmudge13.github.io/sample_code/mixed_effects.html)
library(effects)
#refit model with REML true for parameter estimates
GIsitemonth <- lmer(average_O2_consump_mg_g_hr ~ GIscale + site + month + (1|Chamber_no),
             data = allavnoop, na.action = na.fail, REML = TRUE)
effects_site <- effects::effect(term= "site", mod= GIsitemonth)
summary(effects_site)
SOX <- as.data.frame(effects_site)
site_plot <- ggplot() + 
  geom_point(data=allavnoop, aes(site, average_O2_consump_mg_g_hr)) + 
  geom_point(data=SOX, aes(x=site, y=fit), color="blue") +
  geom_line(data=SOX, aes(x=site, y=fit), color="blue") +
  geom_ribbon(data= SOX, aes(x=site, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="site", y="Scallop respiration")
site_plot #need some error bars or something here!!

effects_month <- effects::effect(term= "month", mod= GIsitemonth)
summary(effects_month)
MOX <- as.data.frame(effects_month)
month_plot <- ggplot() + 
  geom_point(data=allavnoop, aes(month, average_O2_consump_mg_g_hr)) + 
  geom_point(data=MOX, aes(x=month, y=fit), color="blue") +
  geom_line(data=MOX, aes(x=month, y=fit), color="blue") +
  geom_ribbon(data= MOX, aes(x=month, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="month", y="Scallop respiration")
month_plot

effects_GI <- effects::effect(term= "GIscale", mod= GIsitemonth)
summary(effects_GI)
GIX <- as.data.frame(effects_GI)
GI_plot <- ggplot() + 
  geom_point(data=allavnoop, aes(GIscale, average_O2_consump_mg_g_hr)) + 
  geom_point(data=GIX, aes(x=GIscale, y=fit), color="blue") +
  geom_line(data=GIX, aes(x=GIscale, y=fit), color="blue") +
  geom_ribbon(data= GIX, aes(x=GIscale, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="GI (centered & scaled)", y="Scallop respiration")
GI_plot

#ok cool have effect size estimates and graphed etc. re go through all of these tomorrow 

rsq.lmm(GIsitemonth,adj=FALSE)
#random effects explain more of the model than fixed big yikes
#plotting effect sizes #WHAT DO THEY MEAN
sjPlot::plot_model(GIsitemonth,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect GI, site, month on respiration rate")
sjPlot::tab_model(GIsitemonth)
#this is basically already shown in the ANOVA I dont think it needs to be included. 