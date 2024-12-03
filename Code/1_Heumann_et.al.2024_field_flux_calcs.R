## load tidyverse
library(tidyverse)
library(readxl)
library(ggpubr)

## This script outlines the calculations from Heumann et al. 2024 (Ecology) 
## Titled "Rates and controls of nitrogen fixation in post-fire lodgepole pine forests

## This script uses ppm and dry mass data included in the data repository with this script, while 
## using the geometric means, minimums and maximums of 15N2 calibrated conversion ratios reported
## in Soper et al. 2021("Is 3 the magic ratio"). 

## Workflow outline: 
# 1. Import 'revised_fluxes_5_24_24' from Data folder
# 2. Create a list of the conversion ratios listed in Soper et al. 2021 
# 3. Using that list, write a function that applies the correct 3 conversion ratios to the
#     correct niche (i.e. geomean, min, max for moss for example) 
# 4. Scale up each mean, min, and max rate using spatial mass per area data included in Data folder. 

#   ####################################Scaling up to fluxes#####################################################
## Scale up sample N rates to plot level and per hectare level fluxes using percent cover,
## CWD and litter density data from the plots. 

## 1. Calculate mean n fix rates and standard deviations for each niche in each plot during each season. 
# Create new data frame for this table. "seasonal_plot_fluxes"
## 2. merge plot level percent cover and CWD data with that "seasonal_plot_fluxes" data frame. 
## 3.  
##  A. Moss/lichen % cover: calculate the moss % cover for each plot by 
##      the area of the plot (2500 m2 = 25,000,000 cm2).

##      This gives you the area of moss per plot. 
##      - Now multiply that area by the plot average rate (N per gram per hour) and this will 
##        give the plot level hourly flux of N. 
##      - Now calculate the amount of hours in the 2 months of that season:
##      Spring: May and June,  Summer: July and August,  Fall: September and October
##      - Multiply the hourly plot flux for moss and lichen by the amount of hours in that season and that is the seasonal
##      plot level flux of N from those niches

##  B. CWD: Convert the CWD rates per plot to grams of wood. ***Note only using decay class 4-5
##      - Multiply the per gram average rate of fixation in wood for each plot by that CWD biomass amount in grams
##      - Multiply that hourly plot-level rate by the number of hours in that season and this equals your seasonal
##      plot-level flux of N from CWD fixation. 

##. C: Litter: Convert the average litter mass per plot to grams and follow same steps as above using litter mass instead of CWD

##. D: Soil: calculate the mass of soil in each plot by multiply cm2 area (25000000) by 10cm depth = 250000000 cm3
##      - multiply the volume (250000000) by bulk density average of soil samples from each plot 
##      (dry mass/9.818cm3  (2cm sampling depth by area of tube))
##      - this results in the mass of soil per plot which is then multiplied by the plot average N fix rate to produce
##      the seasonal plot-level flux. 

# 1. Import 'revised_fluxes_5_24_24' from seasonal_cryptic_fixation_2/0_Data folder then 
##   filter out pilot lupine, endophyte and rhizosphere samples from rates dataframe
rates <- read_csv("Data/revised_2022_season_fluxes_05_24.csv") %>%
  filter(niche %in% c("Lichen", "Wood", "Moss", "Litter", "Soil"))
unique(rates$niche)

# 2. create table of conversion ratios using Soper et al. 2021 table 1 data.
niches <- c("Lichen", "Wood",  "Moss", "Litter", "Soil")
moss_ratio <- c(niche = "Moss", geo_mean = 3, min = 0.01, max = 5.4)
lichen_ratio <- c(niche = "Lichen", geo_mean = 4.5, min = 1.6, max = 10.8)
wood_ratio <- c(niche = "Wood", geo_mean = 4.1, min = 1.1, max = 10.2)
litter_ratio <- c(niche = "Litter", geo_mean = 3.5, min = 0.8, max = 13.3)
soil_ratio <- c(niche = "Soil",geo_mean = 3.1, min = 0.5, max = 22)

Rratio <- as.data.frame(rbind(moss_ratio, lichen_ratio, litter_ratio, soil_ratio, wood_ratio))

# 3. Using the above list, write a function that applies the correct 3 conversion ratios to the
#     correct niche (i.e. geomean, min, max for moss for example) 
# grams N2 fixed per hour = (ppm/1000000) * 0.05L <- volume of headspace in tubes * 0.99 atm 
#                 -------------------------------------------------------------------
#                       0.0825 * incubation temp in K * R ratio * N2 molar mass
#                 ------------------------------------------------------------------
#                       mass or area of sample / incubation time (hours) 
#

## need to list of average incubation times per sampling period and temperatures
unique(rates$season)
unique(rates$site)
# from "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/FINAL_Cumulative Flux Calcs/incubation-info_YNP_2022_seasonals.xlsx"
incubation_data <- as.data.frame(rbind(incubation_data1 <- c(season = 'spring', site = c("FirLopSo"), avg_temp = 3.9+273.15, time = 23.5),
                                       incubation_data21 <- c(season = 'spring', site = c("BiscBas"), avg_temp = 3.9+273.15, time = 23.5),
                                       incubation_data22 <- c(season = 'spring', site = c("FounEast"), avg_temp = 5.3+273.15, time = 15.75),
                                       incubation_data23 <- c(season = 'spring', site = c("GibFalls"), avg_temp = 5.3+273.15, time = 15.75),
                                       incubation_data2 <- c(season = 'spring', site = c("GravPit"), avg_temp = 5.3+273.15, time = 15.75),
                                       incubation_data3 <- c(season = 'summer', site = c("FounEast"), avg_temp = 18.06+273.15, time = 21.5),
                                       incubation_data4 <- c(season = 'summer', site = c("FirLopSo"), avg_temp = 18.06+273.15, time = 20.5),
                                       incubation_data5 <- c(season = 'summer', site = c("GibFalls"), avg_temp = 18.06+273.15, time = 21),
                                       incubation_data6 <- c(season = 'summer', site = c("BiscBas"), avg_temp = 18.06+273.15, time = 20.75),
                                       incubation_data7 <- c(season = 'summer', site = c("GravPit"), avg_temp = 18.06+273.15, time = 20),
                                       incubation_data8 <- c(season = 'summer_wet', site = c("FounEast"), avg_temp = 13.1+273.15, time = 23.75),
                                       incubation_data9 <- c(season = 'summer_wet', site = c("FirLopSo"), avg_temp = 13.1+273.15, time = 23.5),
                                       incubation_data10 <- c(season = 'summer_wet', site = c("GibFalls"), avg_temp = 13.1+273.15, time = 24),
                                       incubation_data11 <- c(season = 'summer_wet', site = c("BiscBas"), avg_temp = 13.1+273.15, time = 23),
                                       incubation_data12 <- c(season = 'summer_wet', site = c("GravPit"), avg_temp = 13.1+273.15, time = 23.75),
                                       incubation_data13 <- c(season = 'fall', site = c("FounEast"), avg_temp = 10.84+273.15, time = 19.5),
                                       incubation_data14 <- c(season = 'fall', site = c("FirLopSo"), avg_temp = 6.39+273.15, time = 22.25),
                                       incubation_data15 <- c(season = 'fall', site = c("GibFalls"), avg_temp = 10.84+273.15, time = 19.25),
                                       incubation_data16 <- c(season = 'fall', site = c("BiscBas"), avg_temp = 6.39+273.15, time = 22.5),
                                       incubation_data17 <- c(season = 'fall', site = c("GravPit"), avg_temp = 10.84+273.15, time = 19.5)))
## in the future I would just record this data better in a easily imported document to R, but for this I manually entered...

## merge this data with the rates data:
rates_full<- left_join(rates, incubation_data, by = c('season', 'site'))

#Filter out values less than 0, this is below or detection limit
rates_full$blank_corrected_ppm[rates_full$blank_corrected_ppm<0] <- 0

# now compute mol c2h4 then merge with R ratio data to compute g N2 fluxes
ethylene_fluxes <- rates_full %>% mutate(vol_analyte = blank_corrected_ppm/1000000 * 0.05) %>%
  mutate(mol_c2h4 = (vol_analyte*0.99)/(0.0825 * as.numeric(avg_temp)))
merged_fluxes<- merge(ethylene_fluxes, Rratio, by = "niche") 

summary(merged_fluxes)

## further calculations of hourly per area and per dry mass rates
merged_fluxes_full <- merged_fluxes %>%
  mutate(g_n2_mean = (mol_c2h4/as.numeric(geo_mean)) * 28.0134,
         g_n2_min = (mol_c2h4/as.numeric(min)) * 28.0134,
         g_n2_max = (mol_c2h4/as.numeric(max)) * 28.0134,
         mean_gNfixed_gdm_h = g_n2_mean/dry_mass/as.numeric(time), ## mean grams of N2 fixed per gram sample dry mass per hour
         mean_gNfixed_cm_h = g_n2_mean/sample_area_cm2/as.numeric(time),## mean grams of N2 fixed per cm2 sample area per hour (this is used for scaling moss and lichen)
         min_gNfixed_gdm_h = g_n2_min/dry_mass/as.numeric(time), ## minimum grams of N2 fixed per gram sample dry mass per hour
         min_gNfixed_cm_h = g_n2_min/sample_area_cm2/as.numeric(time), ## minimum grams of N2 fixed per cm2 sample area per hour
         max_gNfixed_gdm_h = g_n2_max/dry_mass/as.numeric(time), ## maximum grams of N2 fixed per gram sample dry mass per hour
         max_gNfixed_cm_h = g_n2_max/sample_area_cm2/as.numeric(time)) ## maximum grams of N2 fixed per cm2 sample area per hour


ggplot(merged_fluxes_full, aes(x = blank_corrected_ppm)) + 
  geom_histogram() + 
  facet_wrap(~niche, scales = 'free')

## Scale up sample N rates to plot level and per hectare level fluxes using percent cover, CWD and litter density data from the plots. 
## first, to make it easier to keep track of, separate merged_fluxes full into three datasets: 1 for using the geomean R ratio, 2
## using the min R ratio and 3 using the max R ratio. ** REMEMBER higher R ratio = lower flux (less efficient conversion rate) 
colnames(merged_fluxes_full)
mean_fluxes <- merged_fluxes_full %>%
  select(c("niche", "season", "site",  "quad", "dry_mass", 
         "moist", "mol_c2h4", "geo_mean", "g_n2_mean", "mean_gNfixed_gdm_h", 
         "mean_gNfixed_cm_h"))

high_end_fluxes <- merged_fluxes_full %>% ## high end fluxes are the minimum R ratios, smaller ratio = bigger flux
  select(c("niche", "season", "site",  "quad", "moist", "dry_mass", "mol_c2h4", "min", "g_n2_min", "min_gNfixed_gdm_h", "min_gNfixed_cm_h"))

low_end_fluxes <- merged_fluxes_full %>%
  select(c("niche", "season", "site",  "quad", "moist", "dry_mass", "mol_c2h4", "max", "g_n2_max", "max_gNfixed_gdm_h", "max_gNfixed_cm_h"))


###### MEAN FLUX CALCS ######

## 1. Calculate mean n fix rates and standard deviations for each niche in each plot during each season. 
# Create new data frame for this table. "seasonal_plot_fluxes"
#First remove one data point from BiscBas summer that has no dry mass

mean_fluxes <- mean_fluxes %>% 
  filter(dry_mass > 0)

# starting with the mean R ratio fluxes:
site_fluxes_seasonal <- mean_fluxes %>% group_by(season, niche, site) %>%
  summarize(mean_moist = mean(moist, na.rm=T), 
            variance_moist = var(moist, na.rm=T),
            mean_dry_mass = mean(dry_mass),
            mean_gN_cm2_hr = mean(mean_gNfixed_cm_h),
            st_dev_cm2 = sd(mean_gNfixed_cm_h),
            variance_cm2 = var(mean_gNfixed_cm_h),
            mean_rate_gN_g_h = mean(mean_gNfixed_gdm_h), 
            st_dev_g = sd(mean_gNfixed_gdm_h),
            variance_g = var(mean_gNfixed_gdm_h), 
            sample_n = length(mean_gNfixed_gdm_h))

# re-order x axis (spring - summer - fall)
season_order <- c('spring', 'summer', 'summer_wet', 'fall')

## check to see what those patterns look like
ggplot(site_fluxes_seasonal, aes(x = factor(season, level = season_order), y = mean_rate_gN_g_h, fill = niche)) + 
  geom_col(position = 'dodge') +
  facet_wrap(~site, scales = 'free')

## import moss, lichen, litter and wood cover/mass data
## Moss and Lichen: replace Functional.Group with niche and Site with site
ML_cover <- read_csv("Data/GYE22_UnderstoryMossLichen.csv")
ML_cover <- ML_cover %>% filter(Site %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit')) %>% 
  rename('site' = 'Site', 'niche' = 'Functional.Group') %>% 
  mutate(total_coverage_cm2 = (mean.cover.percent / 100) * (50 * 100)^2) #area of plot in cm2

mean_site_fluxes_seasonal1 <- left_join(site_fluxes_seasonal, ML_cover, by = c("site", "niche"))
##  A. Moss/lichen % cover: calculate the moss % cover for each plot by 
##      the area of the plot (2500 m2 = 25,000,000 cm2).
##      This gives you the area of moss per plot. 

## now merge with litter and wood site data
# Litter: 
litter_mass <- read_excel("Data/GYE 2022 Litter Mass.xls", 
           sheet = "Plots") %>% 
  filter(Plot %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit')) %>%
  rename('site' = 'Plot') %>% 
  mutate(niche = 'Litter') %>%
  group_by(site, niche) %>%
  summarize(plot_litter_grams = (mean(`Mass (g)`) / 900) * (50 * 100)^2) ## each square was 900 cm2

mean_site_fluxes_seasonal2 <- left_join(mean_site_fluxes_seasonal1, litter_mass, by = c("site", "niche"))

# Wood:  
wood_mass <- read_csv("Data/GYE22_CoarseWoodyDebrisBiomass.csv") %>% 
  filter(Site %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit')) %>%
  select(c('Site', 'bm_1000h_Class45_Mg_ha')) %>%
  rename('site' = 'Site') %>% ## this data is already in per ha mass so just have to convert to gramsthen to 0.25 ha
  mutate(plot_wood_grams = bm_1000h_Class45_Mg_ha * 1000000/4, niche = 'Wood') # converting MG wood to grams per *plot* and creating wood niche

mean_site_fluxes_seasonal3 <- left_join(mean_site_fluxes_seasonal2, wood_mass, by = c("site", "niche"))

## Soil mass (to a depth of 10cm)
soil_mass <- site_fluxes_seasonal %>% 
  filter(niche == 'Soil') %>%
  mutate(BD = mean_dry_mass / (4.909 * 2))

soil_mass  <- soil_mass %>%
    group_by(site) %>% 
    summarize(mean_BD = mean(BD), 
              plot_soil_grams = mean(BD) * 250000000) %>%
    mutate(niche = 'Soil')

mean_site_fluxes_seasonal_final <- left_join(mean_site_fluxes_seasonal3, soil_mass, by = c("site", "niche")) %>%
  select("season", "niche" , "site" , "mean_moist", "mean_gN_cm2_hr", "mean_rate_gN_g_h","sample_n","total_coverage_cm2", 
         "plot_litter_grams", "plot_wood_grams", "plot_soil_grams")

## combine litter, soil and wood mass into one column based on niche
plot_fluxes_masses <- mean_site_fluxes_seasonal_final %>%
  mutate(plot_substrate_mass = ifelse(niche == 'Litter', plot_litter_grams, ifelse(niche == 'Wood', plot_wood_grams, plot_soil_grams))) 

mean_flux_kg_ha <- plot_fluxes_masses %>%
  group_by(season, niche, site) %>%
  mutate(mean_N_flux_kg_ha = ifelse(niche %in% c('Moss','Lichen'), mean_gN_cm2_hr * total_coverage_cm2 / 1000 * 4, 
                                       mean_rate_gN_g_h * plot_substrate_mass / 1000 * 4))

##      - Now multiply that area by the plot average rate (N per gram per hour) and this will 
##        give the plot level hourly flux of N. 
##      - Now calculate the amount of hours in the 2 months of that season:
##      Spring: May and June,  Summer: July and August,  Fall: September and October
##      - Multiply the hourly plot flux for moss and lichen by the amount of hours in that season and that is the seasonal
##      plot level flux of N from those niches

geomean_seasonal_flux_calcs <- mean_flux_kg_ha %>%
  mutate(Nfix_kg_ha_season = mean_N_flux_kg_ha*1460)

geomean_seasonal_niche_flux_calcs <- geomean_seasonal_flux_calcs%>%
  group_by(season, niche) %>%
  summarize(mean_Nfix = mean(Nfix_kg_ha_season))

geomean_seasonal_niche_flux_calcs %>%
  group_by(niche) %>%
  summarize(total_Nfix = sum(mean_Nfix)) 

merged_fluxes_full %>% 
  filter(site == 'BiscBas', niche == 'Soil', season == 'spring') %>%
  select(c("quad",  , "blank_corrected_ppm", "dry_mass","vol_analyte",  "mol_c2h4", "mean_gNfixed_gdm_h")) %>% 
  summarize(mean_gNfixed_gdm_h = mean(mean_gNfixed_gdm_h),
            mean_dry_mass = mean(dry_mass))
  
############## HIGH END FLUX CALCS ################

calculate_fluxes <- function(high_end_fluxes, ML_cover, litter_mass, wood_mass, soil_mass) {
  high_end_flux <- high_end_fluxes %>% 
  filter(dry_mass > 0)

# starting with the mean R ratio fluxes:
  site_fluxes_seasonal <- high_end_flux %>% group_by(season, niche, site) %>%
    summarize(mean_moist = mean(moist, na.rm=T), 
            variance_moist = var(moist, na.rm=T),
            dry_mass = mean(dry_mass),
            mean_gN_cm2_hr = mean(min_gNfixed_cm_h),
            st_dev_cm2 = sd(min_gNfixed_cm_h),
            variance_cm2 = var(min_gNfixed_cm_h),
            mean_rate_gN_g_h = mean(min_gNfixed_gdm_h), 
            st_dev_g = sd(min_gNfixed_gdm_h),
            variance_g = var(min_gNfixed_gdm_h), 
            sample_n = length(min_gNfixed_gdm_h))

  mean_site_fluxes_seasonal1 <- left_join(site_fluxes_seasonal, ML_cover, by = c("site", "niche"))
  mean_site_fluxes_seasonal2 <- left_join(mean_site_fluxes_seasonal1, litter_mass, by = c("site", "niche"))
  mean_site_fluxes_seasonal3 <- left_join(mean_site_fluxes_seasonal2, wood_mass, by = c("site", "niche"))
  mean_site_fluxes_seasonal_final <- left_join(mean_site_fluxes_seasonal3, soil_mass, by = c("site", "niche")) %>%
    select("season", "niche" , "site" , "mean_moist", "mean_gN_cm2_hr", "mean_rate_gN_g_h","sample_n","total_coverage_cm2", 
          "plot_litter_grams", "plot_wood_grams", "plot_soil_grams")

  ## combine litter, soil and wood mass into one column based on niche
  plot_fluxes_masses <- mean_site_fluxes_seasonal_final %>%
    mutate(plot_substrate_mass = ifelse(niche == 'Litter', plot_litter_grams, ifelse(niche == 'Wood', plot_wood_grams, plot_soil_grams))) 

  mean_flux_kg_ha <- plot_fluxes_masses %>%
    group_by(season, niche, site) %>%
    mutate(mean_N_flux_kg_ha = ifelse(niche %in% c('Moss','Lichen'), mean_gN_cm2_hr * total_coverage_cm2 / 1000 * 4, 
                                    mean_rate_gN_g_h * plot_substrate_mass / 1000 * 4))

  seasonal_flux_calcs <- mean_flux_kg_ha %>%
    mutate(Nfix_kg_ha_season = mean_N_flux_kg_ha*1460)

return(seasonal_flux_calcs)
  
}

high_seasonal_fluxes <- calculate_fluxes(high_end_fluxes, ML_cover, litter_mass, wood_mass, soil_mass)
high_season_flux_sum <- high_seasonal_fluxes %>%
  group_by(season, niche) %>% 
  summarize(mean_fluxNkghaseason = mean(Nfix_kg_ha_season))

############## LOW END FLUX CALCS ################

calculate_fluxes <- function(low_end_fluxes, ML_cover, litter_mass, wood_mass, soil_mass) {
  low_end_flux <- low_end_fluxes %>% 
    filter(dry_mass > 0)
  
  # starting with the mean R ratio fluxes:
  site_fluxes_seasonal <- low_end_flux %>% group_by(season, niche, site) %>%
    summarize(mean_moist = mean(moist, na.rm=T), 
              variance_moist = var(moist, na.rm=T),
              dry_mass = mean(dry_mass),
              mean_gN_cm2_hr = mean(max_gNfixed_cm_h),
              st_dev_cm2 = sd(max_gNfixed_cm_h),
              variance_cm2 = var(max_gNfixed_cm_h),
              mean_rate_gN_g_h = mean(max_gNfixed_gdm_h), 
              st_dev_g = sd(max_gNfixed_gdm_h),
              variance_g = var(max_gNfixed_gdm_h), 
              sample_n = length(max_gNfixed_gdm_h))
  
  mean_site_fluxes_seasonal1 <- left_join(site_fluxes_seasonal, ML_cover, by = c("site", "niche"))
  mean_site_fluxes_seasonal2 <- left_join(mean_site_fluxes_seasonal1, litter_mass, by = c("site", "niche"))
  mean_site_fluxes_seasonal3 <- left_join(mean_site_fluxes_seasonal2, wood_mass, by = c("site", "niche"))
  mean_site_fluxes_seasonal_final <- left_join(mean_site_fluxes_seasonal3, soil_mass, by = c("site", "niche")) %>%
    select("season", "niche" , "site" , "mean_moist", "mean_gN_cm2_hr", "mean_rate_gN_g_h","sample_n","total_coverage_cm2", 
           "plot_litter_grams", "plot_wood_grams", "plot_soil_grams")
  
  ## combine litter, soil and wood mass into one column based on niche
  plot_fluxes_masses <- mean_site_fluxes_seasonal_final %>%
    mutate(plot_substrate_mass = ifelse(niche == 'Litter', plot_litter_grams, ifelse(niche == 'Wood', plot_wood_grams, plot_soil_grams))) 
  
  mean_flux_kg_ha <- plot_fluxes_masses %>%
    group_by(season, niche, site) %>%
    mutate(mean_N_flux_kg_ha = ifelse(niche %in% c('Moss','Lichen'), mean_gN_cm2_hr * total_coverage_cm2 / 1000 * 4, 
                                      mean_rate_gN_g_h * plot_substrate_mass / 1000 * 4))
  
  seasonal_flux_calcs <- mean_flux_kg_ha %>%
    mutate(Nfix_kg_ha_season = mean_N_flux_kg_ha*1460)
  
  return(seasonal_flux_calcs)
  
}

low_seasonal_fluxes <- calculate_fluxes(low_end_fluxes, ML_cover, litter_mass, wood_mass, soil_mass)

low_season_flux_sum <- low_seasonal_fluxes %>%
  group_by(season, niche) %>% 
  summarize(mean_fluxNkghaseason = mean(Nfix_kg_ha_season))

###### Seasonal Ethylene Rates ########
c2h4 <- merged_fluxes_full %>%
  select(c("niche", "site", "time", "season","dry_mass", "avg_temp", "mol_c2h4", "moist")) %>%
  mutate(nmolc2h4_g_h = (as.numeric(mol_c2h4)*1e+9)/dry_mass/as.numeric(time))



