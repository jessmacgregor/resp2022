getRversion()
#plotting mean, max, min boxplots for temp and DO pre trials
#making some CI plots at the bottom as well

##Using temp subset times from "resp.plotting.R"
temps48J <- rbind(J_BH48,J_CY48,J_MT48,J_NW48,J_OP48) 
temps48J <- temps48J %>%  mutate(month = "earlyJuly")
temps48A <- rbind(A_BH48,A_CY48,A_MT48,A_NW48)
temps48A <- temps48A %>%  mutate(month = "August")
temps48 <- rbind(temps48J,temps48A)

#TEMP boxplot with min max mean sep by month and site
temps48$month <- factor(temps48$month, levels=c('earlyJuly', 'August'))
temps48$site <- factor(temps48$site, levels=c('Cowyard','BullheadBay','Mattituck','NWHarbor','OrientHarbor'))

tempbox <- ggplot(temps48, aes(site, temp_C))+
  geom_boxplot(width=0.75, fill="white", outlier.shape=NA, 
               position = position_dodge2(1, preserve = "single")) + geom_point()

tempbox + theme_classic() + facet_grid(~month) +
  ggtitle("Temp 48 hours pre trial") +
  xlab("site") + ylab("Temperature (C)")

#ok now lets make some hours over 25 plots!
temps48hours <- temps48 %>%
  mutate(hot = ifelse(temp_C >= DegC, 1, 0)) %>%
  group_by(site,month) %>%
  summarise(hot.hour = (sum(hot))/4)%>%
  mutate(hot.hour = as.numeric(hot.hour))
p48 <-ggplot(temps48hours, aes(x=site,y=hot.hour)) + geom_bar(stat='identity') 
p48 + facet_grid(~month) + theme_classic() +
  ggtitle("Hours over 25C, 48 hours pre-trial") + ylab("Exceedance (hours)") +
  geom_hline(yintercept=48, color="blue") +
  scale_y_continuous(breaks = seq(0,48,6))

p48 <- ggplot(temps48hours, aes(site,hot.hour)) + geom_bar(stat = 'identity',
  width = 0.75, fill = "white", position = position_dodge(1, preserve = "single"))
p48 + theme_classic() +
  ggtitle("Hours over 25C, 48 hours pre-trial") + ylab("Exceedance (hours)") +
  scale_y_continuous(breaks = seq(0,48,6))+
  scale_x_discrete(breaks = site, labels = hot.hour) +
  geom_hline(yintercept=48, color="blue") +
  facet_grid(~month)

##Using DO subset times from "resp.plotting.R"
#48 hours
DO48J <- rbind(DOJ_BH48,DOJ_CY48,DOJ_MT48,DOJ_NW48,DOJ_OP48) 
DO48J <- DO48J %>%  mutate(month = "earlyJuly")
DO48A <- rbind(DOA_BH48,DOA_CY48,DOA_MT48,DOA_NW48)
DO48A <- DO48A %>%  mutate(month = "August")
DO48 <- rbind(DO48J,DO48A)
#2 weeks
DO2weeksJ <- rbind(DOJ_BH2weeks,DOJ_CY2weeks,DOJ_MT2weeks,DOJ_NW2weeks,DOJ_OP2weeks) 
DO2weeksJ <- DO2weeksJ %>%  mutate(month = "earlyJuly")
DO2weeksA <- rbind(DOA_BH2weeks,DOA_MT2weeks,DOA_NW2weeks)
DO2weeksA <- DO2weeksA %>%  mutate(month = "August")
DO2weeks <- rbind(DO2weeksJ,DO2weeksA)

#DO boxplot with min max mean sep by month and site
DO48$month <- factor(DO48$month, levels=c('earlyJuly', 'August'))
DO48$site <- factor(DO48$site, levels=c('Cowyard','BullheadBay','Mattituck','NWHarbor','OrientHarbor'))
DObox <- ggplot(DO48, aes(site, ODO_mgL))+
  geom_boxplot(width=0.75, fill="white", outlier.shape=NA, 
               position = position_dodge2(1, preserve = "single")) + geom_point()
DObox + theme_classic() + facet_grid(~month) +
  ggtitle("DO 48 hours pre trial") +
  xlab("site") + ylab("Dissolved Oxygen (mg/L)")

#hours under 4mgL plots - **doing 2 weeks bc 48 hours showed next to nothing

hypoxia <- 4
DO2weeks$month <- factor(DO2weeks$month, levels=c('earlyJuly', 'August'))
DO2weeksp <- DO2weeks %>%
  mutate(bad = ifelse(ODO_mgL <= hypoxia, 1, 0)) %>%
  group_by(site,month) %>%
  summarise(bad.hour = (sum(bad))/4)%>%
  mutate(bad.hour = as.numeric(bad.hour))
d48 <-ggplot(DO2weeksp, aes(x=site,y=bad.hour)) + geom_bar(stat='identity') 
d48 + facet_grid(~month) + theme_classic() +
  ggtitle("Hours under 4 mg/L, 2 weeks pre-trial") + ylab("Exceedance (hours)") +
  geom_hline(yintercept=48, color="blue") +
  scale_y_continuous(breaks = seq(0,48,6))

#graphing bullheads day vs night DO levels
library(scales)
BHDO <- DO2weeks %>%
  filter(site == "BullheadBay") %>%
  mutate(y = 1)%>%
  mutate(DateTime = na_if(DateTime, DateTime > as.POSIXct("2022-06-29 09:45:00")& DateTime < as.POSIXct("2022-07-27 09:30:00")))
BHDO$month <- factor(BHDO$month, levels = c("earlyJuly", "August"))
BHDO$hour = as.numeric(format(BHDO$DateTime, "%H"))
BHDO$hour_shade =ifelse(BHDO$hour >= 19 | BHDO$hour <= 6, "gray50", "gray80")
system.time(BHDO$Date <- as.Date(BHDO$DateTime))
ggplot(BHDO)+
  geom_rect(aes(xmin=DateTime,xmax=lead(DateTime),ymin=-Inf,ymax=Inf,fill=hour_shade))+
  geom_line(aes(x=DateTime,y=ODO_mgL,)) + theme_classic() +
  scale_y_continuous(breaks = seq(0,16,1))+ facet_grid(~month,scales="free_x") +
  scale_fill_manual(values = c("gray50", "gray80"), labels=c("night","day"))+labs(fill="") +
  geom_hline(yintercept=4, color="blue") +
  scale_x_datetime(labels = date_format("%B%d"))
#can't fully make above code work without dumb chunk of dead
#space after trial for July facet but its fine

#now moving on to make mortality/gonad plots

#gotta read in and fix cornell CI/GI data (thanks chatGPT)
library(dplyr)
library(readxl)
library(lubridate)
getwd()
# Define the directory path containing the Excel files
excel_dir <- "C:/Users/macgr/Documents/Stony_Brook_Masters/Scallop_project/resp2022/resp2022/cornell"

# Get a list of all Excel files in the directory
excel_files <- list.files(excel_dir, pattern = "*.xlsx", full.names = TRUE)

# Create an empty data frame to hold the combined data
combined_data <- data.frame()

# Loop through each Excel file
for (excel_file in excel_files) {
  # Read in all sheets from the Excel file
  all_sheets <- readxl::excel_sheets(excel_file)
  
  # Loop through each sheet in the Excel file
  for (sheet_name in all_sheets) {
    # Read in the sheet data
    sheet_data <- readxl::read_excel(excel_file, sheet = sheet_name)
    
    # Add a column to indicate the original sheet name and file name
    sheet_data <- sheet_data %>% mutate(sheet_name = sheet_name, file_name = excel_file)
    
    # Combine the sheet data with the existing combined data
    combined_data <- rbind(combined_data, sheet_data)
  }
}

#clean it up
combined <- combined_data %>% mutate_all(~gsub("ADPI_|Causeway_|Causeay_|Cuaseway_|Bay_", "", .))
combinednew <- combined %>% 
  separate(sheet_name, into = c("site","day","month","year"), sep="_") %>%
  mutate_all(~gsub("2022)", "2022", .)) %>%
  mutate_all(~gsub("Sept","Sep",.))
str(combined)
combinednew <- combinednew %>%  #not working anymore whyyyyy but will troubleshoot later
  mutate(day = as.factor(day)) %>%
  mutate(day = recode(day, "(02" = "02","(13"="13","(28"="28","(11"="11",
  "(26"="26","(08"="08","(22"="22","(21"="21","(07"="07","(19"="19",
  "(01"="01","(30"="30","(27"="27","(09"="09","(24"="24","(06"="06",
  "(10"="10","(18"="18","(05"="05"))
combined$Date<-as.Date(with(combined,paste(year,month,day,sep="-")),"%Y-%B-%d")
combined <- as.data.frame(combined)
combined <- select(combined, -c(13, 14, 15, 16, 18, 19, 20))

#combine my and cornell into 1 df, date, site, ci, gi
head(scallop_pp2022)
#make GI column in MY data
scallop_pp2022 <- scallop_pp2022 %>%
  mutate(GI = (dry_weight_gonad_g/dry_weight_total_g)*100)
#just keep date, site, ci, gi, and rename them
comparemy <- scallop_pp2022 %>%
  rename(date=trial_date) %>%
  mutate(CI = (dry_weight_total_g/height_mm)*100) %>%
  select(date,site,CI,GI) %>%
  mutate(data_source="Stony")%>%
  mutate(date = as.Date(date, "%m/%d/%Y"))
#fix cornell so ours match
comparecornell <- combined %>%
  rename(date = Date, GI = `Gonad Index`, CI = `Condition Index`) %>%
  select(date,site,CI,GI) %>%
  mutate(site = str_replace(site, "Barcelona", "NWHarbor")) %>%
  mutate(site = str_replace(site, "OH", "OrientPoint")) %>%
  mutate(site = str_replace(site, "Bulhead", "Bullhead")) %>%
  mutate(data_source="Cornell") %>%
  mutate(CI = as.numeric(CI)) %>%
  mutate(GI = as.numeric(GI))
str(comparecornell)
str(comparemy)
#merge em
compare <- rbind(comparemy, comparecornell) 
compare$order <- ifelse(compare$data_source == "Cornell", 1, 2)
#make some graphs
CI <- ggplot(compare %>%
         arrange(data_source),
       aes(x = date, y = CI, color = data_source)) +
  geom_point()
CI + theme_classic() + ggtitle("Stony vs Cornell CI") + facet_grid(~site)


GI <- ggplot(compare %>%
               arrange(data_source),
             aes(x = date, y = GI, color = data_source)) +
  geom_point()
GI + theme_classic() + ggtitle("Stony vs Cornell GI") + facet_grid(~site)
#yay looks like they are both totally in line

#now to graph some mortality data - making that same box plot 2 weeks before
#mortality closest to resp date then up to that point
library(readxl)
library(tidyverse)
mortality <- read_excel("CCE2022.xlsx", sheet = "ADPImortality")
mortality <- mortality %>%
  rename(total_alive = 5, total_dead = 6, bag_no = 4) %>%
  mutate(total_alive = as.numeric(total_alive)) %>%
  mutate(total_dead = as.numeric(total_dead)) %>%
  mutate(first = 1-(total_dead/total_alive)) 

# Create new survival column using dplyr
mort <- mortality %>%
  filter(site != "Mattituck") %>%
  group_by(site, bag_no) %>%
  mutate(S_t = cumprod(first)) %>%
  ungroup() %>%
  group_by(site,time) %>%
  mutate(mean_survival = mean(S_t)) 

#trying to make some plots (from chat GPT We LOVE her)

plottime <- ggplot(mort, aes(x = date, y = mean_survival)) +
  geom_step() +
  labs(x = "Time", y = "Survival") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + facet_grid(~site) + 
  theme_classic() + ggtitle("2022 Survivorship by site") 
#adding vertical lines for where my resp occurred
vertical_dates <- comparemy %>% 
  distinct(date, site, .keep_all = FALSE) %>%
  filter(site != "Mattituck") %>%
  mutate(site = str_replace(site, "NWHarbor", "NW Harbor")) %>%
  mutate(site = str_replace(site, "OrientPoint", "Orient Harbor")) %>%
  mutate(site = str_replace(site, "Bullhead", "Bullhead Bay")) %>%
  mutate(site = str_replace(site, "Cowyard", "Cow Yard")) %>%
  mutate(date = as.POSIXct(date))

#putting em together
plottime +
  geom_vline(data = vertical_dates, aes(xintercept = date),
             linetype = "dashed", color = "gray", size = 0.5)

#plot GIs at sites with my sites over it
comparecornell <- comparecornell %>%
  filter(!is.na(GI)) %>%
  group_by(site, date) %>%
  mutate(meanGI = mean(GI))%>%
  mutate(date = as.POSIXct(date))

ggplot(data=comparecornell) + geom_line(aes(x=date, y=meanGI)) + 
  geom_vline(data = vertical_dates, aes(xintercept = date),
             linetype = "dashed", color = "gray", size = 0.5) + 
  facet_grid(~site) + theme_classic() + ggtitle("2022 GI by site")
  

  
  


  
