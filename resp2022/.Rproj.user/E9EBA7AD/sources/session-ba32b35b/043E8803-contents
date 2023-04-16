#Temp and Do graphs - 24 hrs, 48 hrs, and 2 week averages at each site
#against O2 consumption rates AND condition index

#lets make a df with just temp and datetime and site
tempmost <- all_sondes1 %>%
  select(site, DateTime, temp_C)
tempOP <- OP %>%
  select(site, DateTime, temp_C)
tempall <- rbind(tempmost,tempOP)

#adding CI to slopes df (methods Rheault and Rice 1996 - slightly adjusted to match
#how cornell does theirs)
all_slopes_fix <- read.csv("all_slopes_fix.csv")
all_slopes_fix <- all_slopes_fix %>%
  mutate(CI = (dry_weight_total_g/height_mm)*100)

#ok now have them all combined need to make 24 hr, 48 h4, 2 week pre resp vectors with average temp

#CY July - 6/24/2022 10:45 AM
J_CY24 <- tempall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-23 10:45:00") & 
           DateTime < as.POSIXct("2022-06-24 10:45:00"))
J_CY48 <- tempall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-22 10:45:00") & 
           DateTime < as.POSIXct("2022-06-24 10:45:00"))
J_CY2weeks <- tempall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-10 10:45:00") & 
                                      DateTime < as.POSIXct("2022-06-24 10:45:00"))
meanJ_CY24 <- mean(J_CY24$temp_C)
meanJ_CY48 <- mean(J_CY48$temp_C)
meanJ_CY2weeks <- mean(J_CY2weeks$temp_C)
#MT July - 6/28/2022 11:00 AM
J_MT24 <- tempall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-27 11:00:00") & 
                                      DateTime < as.POSIXct("2022-06-28 11:00:00"))
J_MT48 <- tempall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-26 11:00:00") & 
                                      DateTime < as.POSIXct("2022-06-28 11:00:00"))
J_MT2weeks <- tempall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-14 11:00:00") & 
                                      DateTime < as.POSIXct("2022-06-28 11:00:00"))
meanJ_MT24 <- mean(J_MT24$temp_C)
meanJ_MT48 <- mean(J_MT48$temp_C)
meanJ_MT2weeks <- mean(J_MT2weeks$temp_C)
#Bullhead July - 6/29/2022 10:00 AM
J_BH24 <- tempall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-28 10:00:00") & 
                                        DateTime < as.POSIXct("2022-06-29 10:00:00"))
J_BH48 <- tempall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-27 10:00:00") & 
                                        DateTime < as.POSIXct("2022-06-29 10:00:00"))
J_BH2weeks <- tempall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-15 10:00:00") & 
                                        DateTime < as.POSIXct("2022-06-29 10:00:00"))
meanJ_BH24 <- mean(J_BH24$temp_C)
meanJ_BH48 <- mean(J_BH48$temp_C)
meanJ_BH2weeks <- mean(J_BH2weeks$temp_C)
#NW Harbor July - 6/23/2022 10:00 AM
J_NW24 <- tempall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-29 10:00:00") & 
                                       DateTime < as.POSIXct("2022-06-30 10:00:00"))
J_NW48 <- tempall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-28 10:00:00") & 
                                       DateTime < as.POSIXct("2022-06-30 10:00:00"))
J_NW2weeks <- tempall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-16 10:00:00") & 
                                          DateTime < as.POSIXct("2022-06-30 10:00:00"))
meanJ_NW24 <- mean(J_NW24$temp_C)
meanJ_NW48 <- mean(J_NW48$temp_C)
meanJ_NW2weeks <- mean(J_NW2weeks$temp_C)
#Orient Point July - 7/8/2022 10:00 AM
J_OP24 <- tempall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-07-07 10:00:00") & 
                                           DateTime < as.POSIXct("2022-07-08 10:00:00"))
J_OP48 <- tempall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-07-06 10:00:00") & 
                                           DateTime < as.POSIXct("2022-07-08 10:00:00"))
J_OP2weeks <- tempall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-24 10:00:00") & 
                                       DateTime < as.POSIXct("2022-07-08 10:00:00"))
meanJ_OP24 <- mean(J_OP24$temp_C)
meanJ_OP48 <- mean(J_OP48$temp_C)
meanJ_OP2weeks <- mean(J_OP2weeks$temp_C)
#CY August - 8/18/2022 10:00 AM
A_CY24 <- tempall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-17 10:00:00") & 
                                      DateTime < as.POSIXct("2022-08-18 10:00:00"))
A_CY48 <- tempall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-16 10:00:00") & 
                                      DateTime < as.POSIXct("2022-08-18 10:00:00"))
A_CY2weeks <- tempall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-04 10:00:00") & 
                                      DateTime < as.POSIXct("2022-08-18 10:00:00"))
meanA_CY24 <- mean(A_CY24$temp_C)
meanA_CY48 <- mean(A_CY48$temp_C)
meanA_CY2weeks <- mean(A_CY2weeks$temp_C)
#MT August - 8/24/2022 9:45 AM
A_MT24 <- tempall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-23 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
A_MT48 <- tempall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-22 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
A_MT2weeks <- tempall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-10 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
meanA_MT24 <- mean(A_MT24$temp_C)
meanA_MT48 <- mean(A_MT48$temp_C)
meanA_MT2weeks <- mean(A_MT2weeks$temp_C)
#Bullhead August - 8/10/2022 9:30 AM
A_BH24 <- tempall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-08-09 09:30:00") & 
                                       DateTime < as.POSIXct("2022-08-10 09:30:00"))
A_BH48 <- tempall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-08-08 09:30:00") & 
                                       DateTime < as.POSIXct("2022-08-10 09:30:00"))
A_BH2weeks <- tempall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-07-27 09:30:00") & 
                                          DateTime < as.POSIXct("2022-08-10 09:30:00"))
meanA_BH24 <- mean(A_BH24$temp_C)
meanA_BH48 <- mean(A_BH48$temp_C)
meanA_BH2weeks <- mean(A_BH2weeks$temp_C)
#NW Harbor August - 8/25/2022 10:00 AM
A_NW24 <- tempall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-24 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
A_NW48 <- tempall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-23 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
A_NW2weeks <- tempall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-11 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
meanA_NW24 <- mean(A_NW24$temp_C)
meanA_NW48 <- mean(A_NW48$temp_C)
meanA_NW2weeks <- mean(A_NW2weeks$temp_C)

# add new column for temp then add in using if else statements
slopestemp <- all_slopes_fix %>%
  unite("sitemonth", site:month, remove = FALSE) %>%
  mutate(temp24 = ifelse(sitemonth == "Cowyard_earlyJuly", meanJ_CY24,
                  ifelse(sitemonth == "Mattituck_earlyJuly", meanJ_MT24, 
                  ifelse(sitemonth == "Bullhead_earlyJuly", meanJ_BH24,
                  ifelse(sitemonth == "NWHarbor_earlyJuly", meanJ_NW24,
                  ifelse(sitemonth == "Orient_earlyJuly", meanJ_OP24,
                  ifelse(sitemonth == "Cowyard_August", meanA_CY24,
                  ifelse(sitemonth == "Mattituck_August", meanA_MT24, 
                  ifelse(sitemonth == "Bullhead_August", meanA_BH24,meanA_NW24
                         ))))))))) %>%
  mutate(temp48 = ifelse(sitemonth == "Cowyard_earlyJuly", meanJ_CY48,
                  ifelse(sitemonth == "Mattituck_earlyJuly", meanJ_MT48, 
                  ifelse(sitemonth == "Bullhead_earlyJuly", meanJ_BH48,
                  ifelse(sitemonth == "NWHarbor_earlyJuly", meanJ_NW48,
                  ifelse(sitemonth == "Orient_earlyJuly", meanJ_OP48,
                  ifelse(sitemonth == "Cowyard_August", meanA_CY48,
                  ifelse(sitemonth == "Mattituck_August", meanA_MT48, 
                  ifelse(sitemonth == "Bullhead_August", meanA_BH48,meanA_NW48)))))))))%>%
  mutate(temp2weeks = ifelse(sitemonth == "Cowyard_earlyJuly", meanJ_CY2weeks,
                  ifelse(sitemonth == "Mattituck_earlyJuly", meanJ_MT2weeks, 
                  ifelse(sitemonth == "Bullhead_earlyJuly", meanJ_BH2weeks,
                  ifelse(sitemonth == "NWHarbor_earlyJuly", meanJ_NW2weeks,
                  ifelse(sitemonth == "Orient_earlyJuly", meanJ_OP2weeks,
                  ifelse(sitemonth == "Cowyard_August", meanA_CY2weeks,
                  ifelse(sitemonth == "Mattituck_August", meanA_MT2weeks, 
                  ifelse(sitemonth == "Bullhead_August", meanA_BH2weeks,meanA_NW2weeks)))))))))

#simple graph of temps against O2 consumption - 24 hours before
temp24plot <- slopestemp %>%
  ggplot(aes(x=temp24,y=average_O2_consump_mg_g_hr,color=site.y)) + geom_point() +
  theme_classic() + ggtitle("temp 24 hours pre trial")
temp24plot
#48 hours before
temp48plot <- slopestemp %>%
  ggplot(aes(temp48,average_O2_consump_mg_g_hr,color=site.y)) + geom_point() +
  theme_classic() + ggtitle("temp 48 hours pre trial")
temp48plot 

#2 weeks before
temp2weeksplot <- slopestemp %>%
  ggplot(aes(temp2weeks,average_O2_consump_mg_g_hr,color=site.y)) + geom_point() +
  theme_classic() + ggtitle("temp 2 weeks pre trial") + facet_grid(~month)
temp2weeksplot

#could do more specifically temp right when taking it - or focus more on site to site
#thinking about how to incoporate other dependent variables like condition index 

#now lets do DO - for now jsut doing averages but need to think 
#of more to do - variability etc. 

DOmost <- all_sondes1 %>%
  select(site, DateTime, ODO_mgL)
DOOP <- OP %>%
  select(site, DateTime, ODO_mgL)
DOall <- rbind(DOmost,DOOP)
str(DOall)
#CY July - 6/24/2022 10:45 AM
DOJ_CY24 <- DOall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-23 10:45:00") & 
                                      DateTime < as.POSIXct("2022-06-24 10:45:00"))
DOJ_CY48 <- DOall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-22 10:45:00") & 
                                      DateTime < as.POSIXct("2022-06-24 10:45:00"))
DOJ_CY2weeks <- DOall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-10 10:45:00") & 
                                      DateTime < as.POSIXct("2022-06-24 10:45:00"))
meanDOJ_CY24 <- mean(DOJ_CY24$ODO_mgL)
meanDOJ_CY48 <- mean(DOJ_CY48$ODO_mgL)
meanDOJ_CY2weeks <- mean(DOJ_CY2weeks$ODO_mgL)
#MT July - 6/28/2022 11:00 AM
DOJ_MT24 <- DOall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-27 11:00:00") & 
                                        DateTime < as.POSIXct("2022-06-28 11:00:00"))
DOJ_MT48 <- DOall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-26 11:00:00") & 
                                        DateTime < as.POSIXct("2022-06-28 11:00:00"))
DOJ_MT2weeks <- DOall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-14 11:00:00") & 
                                        DateTime < as.POSIXct("2022-06-28 11:00:00"))
meanDOJ_MT24 <- mean(DOJ_MT24$ODO_mgL)
meanDOJ_MT48 <- mean(DOJ_MT48$ODO_mgL)
meanDOJ_MT2weeks <- mean(DOJ_MT2weeks$ODO_mgL)
#Bullhead July - 6/29/2022 10:00 AM
DOJ_BH24 <- DOall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-28 10:00:00") & 
                                          DateTime < as.POSIXct("2022-06-29 10:00:00"))
DOJ_BH48 <- DOall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-27 10:00:00") & 
                                          DateTime < as.POSIXct("2022-06-29 10:00:00"))
DOJ_BH2weeks <- DOall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-15 10:00:00") & 
                                          DateTime < as.POSIXct("2022-06-29 10:00:00"))
meanDOJ_BH24 <- mean(DOJ_BH24$ODO_mgL)
meanDOJ_BH48 <- mean(DOJ_BH48$ODO_mgL)
meanDOJ_BH2weeks <- mean(DOJ_BH2weeks$ODO_mgL)
#NW Harbor July - 6/23/2022 10:00 AM
DOJ_NW24 <- DOall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-29 10:00:00") & 
                                       DateTime < as.POSIXct("2022-06-30 10:00:00"))
DOJ_NW48 <- DOall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-28 10:00:00") & 
                                       DateTime < as.POSIXct("2022-06-30 10:00:00"))
DOJ_NW2weeks <- DOall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-16 10:00:00") & 
                                       DateTime < as.POSIXct("2022-06-30 10:00:00"))
meanDOJ_NW24 <- mean(DOJ_NW24$ODO_mgL)
meanDOJ_NW48 <- mean(DOJ_NW48$ODO_mgL)
meanDOJ_NW2weeks <- mean(DOJ_NW2weeks$ODO_mgL)
#Orient Point July - 7/8/2022 10:00 AM
DOJ_OP24 <- DOall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-07-07 10:00:00") & 
                                           DateTime < as.POSIXct("2022-07-08 10:00:00"))
DOJ_OP48 <- DOall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-07-06 10:00:00") & 
                                           DateTime < as.POSIXct("2022-07-08 10:00:00"))
DOJ_OP2weeks <- DOall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-24 10:00:00") & 
                                           DateTime < as.POSIXct("2022-07-08 10:00:00"))
meanDOJ_OP24 <- mean(DOJ_OP24$ODO_mgL)
meanDOJ_OP48 <- mean(DOJ_OP48$ODO_mgL)
meanDOJ_OP2weeks <- mean(DOJ_OP2weeks$ODO_mgL)
#CY August - 8/18/2022 10:00 AM
DOA_CY24 <- DOall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-17 10:00:00") & 
                                      DateTime < as.POSIXct("2022-08-18 10:00:00"))
DOA_CY48 <- DOall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-16 10:00:00") & 
                                                                            DateTime < as.POSIXct("2022-08-18 10:00:00"))
#can't use bwloe code becuase CY DO data is wrong
#  DOA_CY2weeks <- DOall %>%
#  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-04 10:00:00") & 
                                      #DateTime <= as.POSIXct("2022-08-18 10:00:00"))
#meanDOA_CY24 <- mean(DOA_CY24$ODO_mgL)
#meanDOA_CY48 <- mean(DOA_CY48$ODO_mgL)
#meanDOA_CY2weeks <- mean(DOA_CY2weeks$ODO_mgL)

#MT August - 8/24/2022 9:45 AM
DOA_MT24 <- DOall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-23 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
DOA_MT48 <- DOall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-22 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
DOA_MT2weeks <- DOall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-10 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
meanDOA_MT24 <- mean(DOA_MT24$ODO_mgL)
meanDOA_MT48 <- mean(DOA_MT48$ODO_mgL)
meanDOA_MT2weeks <- mean(DOA_MT2weeks$ODO_mgL)
#Bullhead August - 8/10/2022 9:30 AM
DOA_BH24 <- DOall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-08-09 09:30:00") & 
                                          DateTime < as.POSIXct("2022-08-10 09:30:00"))
DOA_BH48 <- DOall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-08-08 09:30:00") & 
                                          DateTime < as.POSIXct("2022-08-10 09:30:00"))
DOA_BH2weeks <- DOall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-07-27 09:30:00") & 
                                          DateTime < as.POSIXct("2022-08-10 09:30:00"))
meanDOA_BH24 <- mean(DOA_BH24$ODO_mgL)
meanDOA_BH48 <- mean(DOA_BH48$ODO_mgL)
meanDOA_BH2weeks <- mean(DOA_BH2weeks$ODO_mgL)
#NW Harbor August - 8/25/2022 10:00 AM
DOA_NW24 <- DOall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-24 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
DOA_NW48 <- DOall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-23 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
DOA_NW2weeks <- DOall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-11 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
meanDOA_NW24 <- mean(DOA_NW24$ODO_mgL)
meanDOA_NW48 <- mean(DOA_NW48$ODO_mgL)
meanDOA_NW2weeks <- mean(DOA_NW2weeks$ODO_mgL)

# add new column for DO then add in using if else statements
slopesDO <- all_slopes_fix %>%
  unite("sitemonth", site:month) %>%
  mutate(DO24 = ifelse(sitemonth == "Cowyard_earlyJuly", meanDOJ_CY24,
                ifelse(sitemonth == "Mattituck_earlyJuly", meanDOJ_MT24, 
                ifelse(sitemonth == "Bullhead_earlyJuly", meanDOJ_BH24,
                 ifelse(sitemonth == "NWHarbor_earlyJuly", meanDOJ_NW24,
                 ifelse(sitemonth == "Orient_earlyJuly", meanDOJ_OP24,
                ifelse(sitemonth == "Cowyard_August", meanDOA_CY24,
                ifelse(sitemonth == "Mattituck_August", meanDOA_MT24, 
                ifelse(sitemonth == "Bullhead_August", meanDOA_BH24,meanDOA_NW24))))))))) %>%
mutate(DO48 = ifelse(sitemonth == "Cowyard_earlyJuly", meanDOJ_CY48,
                       ifelse(sitemonth == "Mattituck_earlyJuly", meanDOJ_MT48, 
                      ifelse(sitemonth == "Bullhead_earlyJuly", meanDOJ_BH48,
                       ifelse(sitemonth == "NWHarbor_earlyJuly", meanDOJ_NW48,
                      ifelse(sitemonth == "Orient_earlyJuly", meanDOJ_OP48,
                     ifelse(sitemonth == "Cowyard_August", meanDOA_CY48,
                     ifelse(sitemonth == "Mattituck_August", meanDOA_MT48, 
                    ifelse(sitemonth == "Bullhead_August", meanDOA_BH48,meanDOA_NW48)))))))))%>%
mutate(DO2weeks = ifelse(sitemonth == "Cowyard_earlyJuly", meanDOJ_CY2weeks,
                    ifelse(sitemonth == "Mattituck_earlyJuly", meanDOJ_MT2weeks, 
                     ifelse(sitemonth == "Bullhead_earlyJuly", meanDOJ_BH2weeks,
                    ifelse(sitemonth == "NWHarbor_earlyJuly", meanDOJ_NW2weeks,
                     ifelse(sitemonth == "Orient_earlyJuly", meanDOJ_OP2weeks,
                   ifelse(sitemonth == "Mattituck_August", meanDOA_MT2weeks, 
                  ifelse(sitemonth == "Bullhead_August", meanDOA_BH2weeks,meanDOA_NW2weeks))))))))

#simple graph of DO against O2 consumption - 24hrs, 48hrs, 2weeks
DO24plot <- slopesDO %>%
  ggplot(aes(DO24,average_O2_consump_mg_g_hr,color=site.y)) + geom_point() +
  theme_classic() + ggtitle("DO average 24 hours before trial")
DO24plot
#48 hours before
DO48plot <- slopesDO %>%
  ggplot(aes(DO48,average_O2_consump_mg_g_hr,color=site.y)) + geom_point() +
  theme_classic()
DO48plot + ggtitle("DO average 48 hours before trial")
#2 weeks before
DO2weeksplot <- slopesDO %>%
  ggplot(aes(DO2weeks,average_O2_consump_mg_g_hr,color=site.y)) + geom_point() +
  theme_classic()
DO2weeksplot + ggtitle("DO average 2 weeks before trial")

#simple graph of temp against Condition Index - 2weeks

CI2weeksplot <- slopestemp %>%
  ggplot(aes(temp2weeks,CI,color=site.y)) + geom_point() +
  theme_classic() 
CI2weeksplot + ggtitle("CI against avg temp 2 weeks before trial")

#DO against CI #2 weeks before
CIDO2weeksplot <- slopesDO %>%
  ggplot(aes(DO2weeks,CI,color=site.y)) + geom_point() +
  theme_classic()
CIDO2weeksplot + ggtitle("CI against avg DO 2 weeks before trial")

#salinity and chla 48 hours before for LMM

salchlamost <- all_sondes1 %>%
  select(site, DateTime, sal_psu, chla_ugL)
salOP <- OP %>%
  select(site, DateTime, salinity) %>%
  rename(sal_psu = salinity)
salchlaall <- rbind(salchlamost,salOP)
#Cowyard July
scJ_CY48 <- salchlaall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-06-22 10:45:00") & 
                                      DateTime < as.POSIXct("2022-06-24 10:45:00"))
smeanJ_CY48 <- mean(scJ_CY48$sal_psu)
cmeanJ_CY48 <- mean(scJ_CY48$chla_ugL)
#Mattituck July
scJ_MT48 <- salchlaall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-06-26 11:00:00") & 
                                        DateTime < as.POSIXct("2022-06-28 11:00:00"))
smeanJ_MT48 <- mean(scJ_MT48$sal_psu)
cmeanJ_MT48 <- mean(scJ_MT48$chla_ugL)
#Bullhead July - 6/29/2022 10:00 AM
scJ_BH48 <- salchlaall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-06-27 10:00:00") & 
                                          DateTime < as.POSIXct("2022-06-29 10:00:00"))
smeanJ_BH48 <- mean(scJ_BH48$sal_psu)
cmeanJ_BH48 <- mean(scJ_BH48$chla_ugL)
#NW Harbor July - 6/23/2022 10:00 AM
scJ_NW48 <- salchlaall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-06-28 10:00:00") & 
                                       DateTime < as.POSIXct("2022-06-30 10:00:00"))
smeanJ_NW48 <- mean(scJ_NW48$sal_psu)
cmeanJ_NW48 <- mean(scJ_NW48$chla_ugL)
#Orient Point July - 7/8/2022 10:00 AM
scJ_OP48 <- salchlaall %>%
  filter(site=="OrientHarbor")%>% filter(DateTime >= as.POSIXct("2022-07-06 10:00:00") & 
                                           DateTime < as.POSIXct("2022-07-08 10:00:00")) %>%
  filter(!is.na(sal_psu))

smeanJ_OP48 <- mean(scJ_OP48$sal_psu)
cmeanJ_OP48 <- mean(scJ_OP48$chla_ugL)
#CY August - 8/18/2022 10:00 AM
scA_CY48 <- salchlaall %>%
  filter(site=="Cowyard")%>% filter(DateTime >= as.POSIXct("2022-08-16 10:00:00") & 
                                      DateTime < as.POSIXct("2022-08-18 10:00:00"))
smeanA_CY48 <- mean(scA_CY48$sal_psu)
cmeanA_CY48 <- mean(scA_CY48$chla_ugL)
#MT August - 8/24/2022 9:45 AM
scA_MT48 <- salchlaall %>%
  filter(site=="Mattituck")%>% filter(DateTime >= as.POSIXct("2022-08-22 09:45:00") & 
                                        DateTime < as.POSIXct("2022-08-24 09:45:00"))
smeanA_MT48 <- mean(scA_MT48$sal_psu)
cmeanA_MT48 <- mean(scA_MT48$chla_ugL)
#Bullhead August - 8/10/2022 9:30 AM
scA_BH48 <- salchlaall %>%
  filter(site=="BullheadBay")%>% filter(DateTime >= as.POSIXct("2022-08-08 09:30:00") & 
                                          DateTime < as.POSIXct("2022-08-10 09:30:00"))
smeanA_BH48 <- mean(scA_BH48$sal_psu)
cmeanA_BH48 <- mean(scA_BH48$chla_ugL)
#NW Harbor August - 8/25/2022 10:00 AM
scA_NW48 <- salchlaall %>%
  filter(site=="NWHarbor")%>% filter(DateTime >= as.POSIXct("2022-08-23 10:00:00") & 
                                       DateTime < as.POSIXct("2022-08-25 10:00:00"))
smeanA_NW48 <- mean(scA_NW48$sal_psu)
cmeanA_NW48 <- mean(scA_NW48$chla_ugL)
# add new column for temp then add in using if else statements
slopessalchla <- all_slopes_fix %>%
  unite("sitemonth", site:month, remove = FALSE) %>%
  mutate(sal48 = ifelse(sitemonth == "Cowyard_earlyJuly", smeanJ_CY48,
                         ifelse(sitemonth == "Mattituck_earlyJuly", smeanJ_MT48, 
                                ifelse(sitemonth == "Bullhead_earlyJuly", smeanJ_BH48,
                                       ifelse(sitemonth == "NWHarbor_earlyJuly", smeanJ_NW48,
                                              ifelse(sitemonth == "Orient_earlyJuly", smeanJ_OP48,
                                                     ifelse(sitemonth == "Cowyard_August", smeanA_CY48,
                                                            ifelse(sitemonth == "Mattituck_August", smeanA_MT48, 
                                                                   ifelse(sitemonth == "Bullhead_August", smeanA_BH48,smeanA_NW48))))))))) %>%
  mutate(chla48 = ifelse(sitemonth == "Cowyard_earlyJuly", cmeanJ_CY48,
                        ifelse(sitemonth == "Mattituck_earlyJuly", cmeanJ_MT48, 
                               ifelse(sitemonth == "Bullhead_earlyJuly", cmeanJ_BH48,
                                      ifelse(sitemonth == "NWHarbor_earlyJuly", cmeanJ_NW48,
                                             ifelse(sitemonth == "OrientPoint_earlyJuly", cmeanJ_OP48,
                                                    ifelse(sitemonth == "Cowyard_August", cmeanA_CY48,
                                                           ifelse(sitemonth == "Mattituck_August", cmeanA_MT48, 
                                                                  ifelse(sitemonth == "Bullhead_August", cmeanA_BH48,cmeanA_NW48)))))))))

 