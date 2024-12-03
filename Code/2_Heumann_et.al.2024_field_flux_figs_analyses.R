library(tidyverse)
library(readxl)
library(ggpubr)
library(rstatix)
library(multcompView)
library(FSA)
library(rcompanion)
library(scales)

## Below is the script for figure creation and statistical analyses from Heumann et al. 2024 (Ecology) 
## "Rates and controls of nitrogen fixation in post-fire lodgepole pine forests"

### input dataset downloads:
c2h4 <- read_csv("Data/c2h4_Rates_6_2024.csv") 
geomean_flux <- read_csv("Data/geomean_season_flux_calcs_6_2024.csv") 
highend_flux <- read_csv("Data/high_end_season_flux_calcs_6_2024.csv")
lowend_flux <- read_csv("Data/low_end_season_flux_calcs_6_2024.csv") 

# re-order x axis (spring - summer - fall) for future graphs
season_order <- c('spring', 'summer', 'summer_wet', 'fall')

## the samples in c2h4 that have negative moist should set to 0 - those are rounding errors
c2h4$moist[c2h4$moist < 0] <- 0

## c2h4 rates using individual data points
ggplot(c2h4, aes(x = factor(season, level = season_order), y = nmolc2h4_g_h, color = niche)) + 
  geom_boxplot(position = 'dodge') +
  facet_wrap(~niche, scales = 'free_y')  +
  geom_point(size=4) + 
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) +  
  theme_minimal() +
  theme(text = element_text(size=25, family = ),
        legend.position = "none",
        axis.text.x = element_blank()) 

kw_niche_model <- c2h4 %>%
  group_by(niche) %>% 
  group_modify(~ broom::glance(kruskal.test(nmolc2h4_g_h ~ season, data = .x)))

ggplot(c2h4, aes(x = moist, y = nmolc2h4_g_h, color = niche)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se=F) +
  facet_wrap(~niche, scales = 'free') +
  theme_bw()

## same as above just by moist not season
ggplot(c2h4, aes(x = moist, y = nmolc2h4_g_h, fill = niche, shape = season)) + 
  geom_point() +
  facet_wrap(~niche, scales = 'free')  +
  geom_point(size=2) + 
  geom_smooth(method = 'lm', se=F)

## change season values to actual month of sampling: 

c2h4_samplingperiod <- c2h4 %>%
  mutate(month = ifelse(season == 'spring', 'May', 
                        ifelse(season == 'summer', 'August', 
                               ifelse(season == 'summer_wet', 'August rain', 'October'))))
month_order <- c('May', 'August', 'August rain', 'October')

kruskal_results <- c2h4_samplingperiod %>%
  group_by(niche) %>%
  summarize(statistic = kruskal.test(nmolc2h4_g_h ~ month)$statistic,
            p.value = kruskal.test(nmolc2h4_g_h ~ month)$p.value,
            parameter = kruskal.test(nmolc2h4_g_h ~ month)$parameter) %>%
  mutate(label = paste('H  =', round(statistic, 2), '\n',
                       'p < 0.005', '\n',
                       'df =', parameter))
## by sampling period
dunn_results <- c2h4_samplingperiod %>%
  group_by(niche) %>% 
  dunn_test(nmolc2h4_g_h ~ month, data = .)
#write.csv(dunn_results, "3_Output/results/dunn_speriod_table1.csv")

dunn_moist_results <- c2h4_samplingperiod %>%
  group_by(niche) %>% 
  dunn_test(moist ~ month, data = .)
#write.csv(dunn_moist_results, "3_Output/results/dunn_s_moist_period_table1.csv")
## aov by moisture
lm_results_moist <- c2h4_samplingperiod %>%
  group_by(niche) %>% 
  lm(log(nmolc2h4_g_h+0.00001) ~ moist*month, data = .)
summary(lm_results_moist)

cnf_palette <- c('skyblue', 'orange', 'green4', 'brown4', 'white')

ARA_season_vplot <- ggplot(c2h4_samplingperiod, aes(x = factor(month, levels = month_order),
                                y = nmolc2h4_g_h+0.0000001, fill = niche)) + 
  geom_violin(position = 'dodge') +
  facet_wrap(~niche, scales = 'free_y')  +
  geom_jitter(aes(size= moist), width = 0.1) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  theme(text = element_text(size=18)) +
  scale_y_log10() +
  theme(legend.position = 'none',
        plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  geom_label(data = kruskal_results, aes(x = ifelse(niche %in% c('Soil'), 1.5,
                                                    ifelse(niche == 'Lichen', 4, ### aes x coordinate, y coordinate, 
                                                    ifelse(niche == 'Wood', 2, 3.8))),
                                         y = ifelse(niche == 'Moss', 5, 
                                                    ifelse(niche == 'Lichen', 10, 0.00001)),
                                         label = label), size = 5) + 
  ggpubr::fill_palette(palette = cnf_palette) 

niche_flux_p <- ggplot(geomean_flux, aes(x = niche,y = Nfix_kg_ha_season+0.0001, fill = niche)) + 
  geom_boxplot(position = 'dodge', outliers = F) +
  geom_jitter(width = 0.1, size = 5) +
  ylab(label = expression("N fixation (kg N ha"^-1*")")) +
  xlab(label = element_blank()) +
  theme(text = element_text(size=40, color = 'black'))  +
  theme(legend.position = 'none',
        plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  ggpubr::fill_palette(palette = cnf_palette) 

c2h4_p_letters <- c2h4_samplingperiod %>% 
  group_by(niche, month) %>%
  summarize(nmolc2h4_g_h = mean(nmolc2h4_g_h, na.rm = T)) %>%
  mutate(dunn_l = ifelse(niche %in% c("Soil", "Litter"),
                                                ifelse(month == "May", "a", "b"), 
                         ifelse(month %in% c('May', 'August rain'), "a", "b")))

vplot_l <- ggplot(c2h4_samplingperiod, 
       aes(x = factor(month, levels = month_order),
           y = nmolc2h4_g_h+0.0000001, fill = niche)) + 
  geom_violin(position = 'dodge') +
  facet_wrap(~niche, scales = 'free_y')  +
  geom_jitter(aes(size= moist), width = 0.1) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  labs(size = "Moisture content %", color = element_blank()) +
  theme(text = element_text(size=40), axis.text = element_text(color  = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) + 
    guides(
    fill = "none",  # Removes the 'niche' legend
    size = guide_legend(title = "Moisture (%)")) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = c(0.8, 0.25)) +
  ggpubr::fill_palette(palette = cnf_palette) +
  geom_text(data = c2h4_p_letters, aes(label = dunn_l), 
            position = position_dodge(width = 0.75),
            vjust = -1,
            hjust = -0.7,
            size = 13,                                # Adjust size as needed
            color = "black")   
  
c2h4_sum <- c2h4_samplingperiod %>%
  group_by(niche, month) %>%
  summarize(n = length(nmolc2h4_g_h),
            mean_AR = mean(nmolc2h4_g_h, na.rm =T),
            mean_moist = mean(moist, na.rm =T),
            se_AR = sd(nmolc2h4_g_h)/sqrt(length(nmolc2h4_g_h)),
            median_AR = median(nmolc2h4_g_h,na.rm =T),
            min_AR = min(nmolc2h4_g_h),
            max_AR = max(nmolc2h4_g_h))
#write.csv(c2h4_sum, "3_Output/results/c2h4_sum.csv")

cnf_palette2 <- c('skyblue', 'orange', 'green4', 'brown4', 'black')

#### Seasonal N fix Table ####
colnames(geomean_flux)
colnames(highend_flux)
highend_flux <- highend_flux %>%
  rename(high_Nfix_kg_ha_season = Nfix_kg_ha_season)
colnames(lowend_flux)
lowend_flux <- lowend_flux %>%
  rename(low_Nfix_kg_ha_season = Nfix_kg_ha_season)

tidy_geomean_flux <- dplyr::select(geomean_flux, c('season', 'niche', 'mean_moist', 'Nfix_kg_ha_season'))

tidy_highend_flux <- highend_flux %>%
  dplyr::select(c('season', 'niche', 'mean_moist', 'high_Nfix_kg_ha_season')) 

tidy_lowend_flux <- lowend_flux %>%
  dplyr::select(c('season', 'niche', 'mean_moist', 'low_Nfix_kg_ha_season')) 

merge1 <- merge(tidy_geomean_flux, tidy_highend_flux, by = c('season', 'niche', 'mean_moist'))
tidy_fluxes <- merge(merge1, tidy_lowend_flux, by = c('season', 'niche', 'mean_moist'))

tidy_fluxes %>%
  group_by(niche, season) %>%
  summarize(mean_flux = mean(Nfix_kg_ha_season),
            mean_high_flux = mean(high_Nfix_kg_ha_season),
            mean_low_flux = mean(low_Nfix_kg_ha_season)) %>%
  group_by(niche) %>%
  summarize(annual_meanflux = sum(mean_flux),
            annual_high = sum(mean_high_flux),
            annual_low = sum(mean_low_flux)) %>% 
  ungroup() %>%
  mutate(mean_total = sum(annual_meanflux),
         high_total = sum(annual_high),
         low_total = sum(annual_low))

tidy_fluxes %>%
  group_by(niche, season) %>%
  summarize(mean_flux = mean(Nfix_kg_ha_season),
            se_flux = sd(Nfix_kg_ha_season)/sqrt(length(Nfix_kg_ha_season))) %>%
  group_by(niche) %>%
  summarize(annual_meanflux = sum(mean_flux),
            annual_se = sum(se_flux)) %>%
  ungroup() %>%
  mutate(mean_total = sum(annual_meanflux),
         se_total = sum(annual_se))

table1 <- tidy_fluxes %>%
  group_by(niche, season) %>%
  summarize(mean_flux = mean(Nfix_kg_ha_season),
            se_flux = sd(Nfix_kg_ha_season)/sqrt(length(Nfix_kg_ha_season)), 
            median_flux = median(Nfix_kg_ha_season),
            mean_high_flux = mean(high_Nfix_kg_ha_season),
            mean_low_flux = mean(low_Nfix_kg_ha_season), 
            n_niche = length(Nfix_kg_ha_season))
#write.csv(table1, "3_Output/results/table1.csv")

#### seasonal N fix plots ####
tidy_fluxes_3seas <- tidy_fluxes
tidy_fluxes_3seas$season[tidy_fluxes_3seas$season == 'summer_wet'] <- 'summer' ## reduce down to three seasons and take average of summer

### Dunns test across season fluxes 
tukey_hsd(aov(Nfix_kg_ha_season ~ season, data = tidy_fluxes_3seas))

fluxes_3seas_sum <- tidy_fluxes_3seas %>%
group_by(niche, season) %>%
  summarize(mean_flux = mean(Nfix_kg_ha_season),
            se_flux = sd(Nfix_kg_ha_season)/sqrt(length(Nfix_kg_ha_season)))

annual_flux_niche <- fluxes_3seas_sum %>%
  group_by(niche) %>%
  summarize(season = 'annual', 
            mean_flux = sum(mean_flux),
            se_flux = sum(se_flux))
seas_flux_sum <- rbind(fluxes_3seas_sum, annual_flux_niche)
  
flux_no_niche <- tidy_fluxes_3seas %>%
  group_by(niche, season) %>%
  summarize(mean_flux = mean(Nfix_kg_ha_season),
            se_flux = sd(Nfix_kg_ha_season)/sqrt(length(Nfix_kg_ha_season))) %>%
  group_by(season) %>%
  summarize(annual_meanflux = sum(mean_flux),
            annual_se = sum(se_flux)) %>%
  ungroup() %>%
  mutate(mean_total = sum(annual_meanflux),
         se_total = sum(annual_se))

annual_flux_no_niche <- data.frame(
  season = 'annual',
  annual_meanflux = sum(flux_no_niche$annual_meanflux),
  annual_se = sum(flux_no_niche$annual_se),
  mean_total = 0.281,
  se_total = 0.112)

flux_no_niche <- rbind(flux_no_niche, annual_flux_no_niche)

season_order2 <- c('Spring', 'Summer', 'Fall', 'Annual')
flux_no_niche$season[flux_no_niche$season == 'summer'] <- 'Summer' ## reduce down to three seasons and take average of summer
flux_no_niche$season[flux_no_niche$season == 'fall'] <- 'Fall' ## reduce down to three seasons and take average of summer
flux_no_niche$season[flux_no_niche$season == 'spring'] <- 'Spring' ## reduce down to three seasons and take average of summer
flux_no_niche$season[flux_no_niche$season == 'annual'] <- 'Annual' ## reduce down to three seasons and take average of summer
seas_flux_sum$season[seas_flux_sum$season == 'summer'] <- 'Summer' ## reduce down to three seasons and take average of summer
seas_flux_sum$season[seas_flux_sum$season == 'fall'] <- 'Fall' ## reduce down to three seasons and take average of summer
seas_flux_sum$season[seas_flux_sum$season == 'spring'] <- 'Spring' ## reduce down to three seasons and take average of summer
seas_flux_sum$season[seas_flux_sum$season == 'annual'] <- 'Annual'

season_flux_plot <- ggplot(flux_no_niche, aes(x = factor(season, levels = season_order2), y = annual_meanflux)) + 
  geom_col(data = seas_flux_sum, aes(y = mean_flux, fill = niche)) + 
  geom_errorbar(aes(x = season, 
                    ymax = annual_meanflux + annual_se,
                    ymin = annual_meanflux-annual_se), width=0.2) +
  theme_minimal() +
  theme(legend.position = c(.6,.5)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) + 
  theme(text = element_text(size=40), axis.text = element_text(color  = "black")) +
  ggpubr::fill_palette(palette = cnf_palette2) +
  ylab(label = expression("N fixation rate (kg N ha"^-1*" season"^-1*")")) +
  xlab(label = element_blank()) +
  labs(fill = "Niche") 

season_rate_hist <- ggplot(tidy_fluxes_3seas, aes(x = Nfix_kg_ha_season, fill = niche)) + 
  geom_histogram() +
  theme_minimal()  + 
  ggpubr::fill_palette(palette = cnf_palette2) +
  facet_wrap(~season) +
  theme(text = element_text(size=40), axis.text = element_text(color  = "black")) +
  xlab(label = expression("N fixation (kg N ha"^-1*")")) 

