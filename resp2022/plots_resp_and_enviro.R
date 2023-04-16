#graphing enviro data over resp

#renaming sites
temps48$site <- recode_factor(temps48$site, BullheadBay = "Bullhead", 
                OrientHarbor = "OrientPoint")

#temp plot
ggplot() +
  geom_boxplot(data = all_slopes_fix, aes(site, average_O2_consump_mg_g_hr), fill = "red",
               position = position_dodge2(1, preserve = "single")) + geom_point() +
  geom_boxplot(data = temps48, aes(x = site, y = temp_C*0.1), color = "blue",
               ) +
  scale_y_continuous(
    name = "Respiration rate (mgO2g^-1hr^-1)",
    sec.axis = sec_axis(~.*10, name = "Temperature 48 hours before trial (°C)", 
                        breaks = pretty(temps48$temp_C))
  ) +
  theme_classic() + facet_grid(~month) 

#renaming sites
DO48$site <- recode_factor(temps48$site, BullheadBay = "Bullhead", 
                              OrientHarbor = "OrientPoint")

#DO plot
ggplot() +
  geom_boxplot(data = all_slopes_fix, aes(site, average_O2_consump_mg_g_hr), fill = "red",
               position = position_dodge2(1, preserve = "single")) + geom_point() +
  geom_boxplot(data = DO48, aes(x = site, y = ODO_mgL*0.5), color = "blue",
  ) +
  scale_y_continuous(
    name = "Respiration rate (mgO2g^-1hr^-1)",
    sec.axis = sec_axis(~.*2, name = "DO 48 hours before trial (mg/L)", 
                        breaks = pretty(DO48$ODO_mgL))
  ) +
  theme_classic() + facet_grid(~month) 