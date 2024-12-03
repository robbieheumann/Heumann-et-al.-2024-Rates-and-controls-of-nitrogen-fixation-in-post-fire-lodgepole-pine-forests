library(tidyverse)
library(readxl)
library(nlme)
library(mgcv)
library(lmtest)
library(multcomp)
library(scales)
#### Import "cumulative clim_exp_flux_calcs" from cumulative flux calcs folder in box
fluxes <- read_csv(".../Data/final_clim_exp_fluxes.csv")

#make negative fluxes 0
fluxes$flux_nmol_C2H4_g_hr[fluxes$flux_nmol_C2H4_g_hr < 0] <- 0

round1 <- fluxes %>% filter(round == "1")
round2 <- fluxes %>% filter(round == "2")

## take all samples from round 2 and just moss and lichen from round one and combine into one df

lm_flux <- round1 %>%
  filter(niche %in% c('Lichen' , 'Moss'))

full <- rbind(lm_flux, round2)

ggplot(full, aes(x = flux_nmol_C2H4_g_hr)) + 
  geom_histogram() + 
  facet_wrap(~niche, scales = 'free')

active <- full %>%
  filter(flux_nmol_C2H4_g_hr > 0,
         actual_moist_perc > -3)
temp_order <- c(5, 15, 25, 35)
cnf_palette <- c('skyblue', 'orange', 'green4', 'brown4', 'black')
### how many samples were active in each temperature treatment? and what were the averages of those temperatures
## remove the extreme negative moist values (< -3)
### How do these active fluxes change with moisture? first what are th ranges of moist in each temp group?
full$actual_moist_perc[full$actual_moist_perc < -3] <- 0
active_sum <- full %>%
  group_by(niche, temp) %>%
  summarize(no_active = sum(flux_nmol_C2H4_g_hr > 0),
            n_total = length(flux_nmol_C2H4_g_hr),
            perc_active = no_active/n_total*100,
            mean_active = mean(flux_nmol_C2H4_g_hr > 0),
            var_active = var(flux_nmol_C2H4_g_hr > 0),
            moist_1q = quantile(actual_moist_perc, 0.25),
            active_moist1q = sum(flux_nmol_C2H4_g_hr > 0 & actual_moist_perc > quantile(actual_moist_perc, 0.25)) / no_active * 100, 
            moist_median = median(actual_moist_perc),
            active_moist_med = sum(flux_nmol_C2H4_g_hr > 0 & actual_moist_perc > median(actual_moist_perc)) / no_active * 100,
            moist_3q = quantile(actual_moist_perc, 0.75),
            active_moist3q = sum(flux_nmol_C2H4_g_hr > 0 & actual_moist_perc > quantile(actual_moist_perc, 0.75)) / no_active * 100) 

perc_active_p <- ggplot(active_sum, aes(x = factor(temp, levels =temp_order), y = perc_active, fill = niche)) + 
  geom_col() + 
  facet_wrap(~niche) +
  ggpubr::fill_palette(palette = cnf_palette) + 
  theme_minimal(base_size = 20) +
  xlab(label = expression("Temperature ("*degree~C*")")) +
  ylab(label = "Active samples (% of total)") +
  theme(legend.position = 'none') + 
  theme(text = element_text(size=40), axis.text = element_text(color  = "black"))+
  geom_text(aes(label = round(perc_active, 1)), 
               position = position_nudge(y=5), # Adjust position
               size = 6, color = "black") 

## How weel are temperature and moisture predicting active vs inactive samples?
full %>%
  group_by(niche) %>%
  group_modify(~broom::glance(glm(I(flux_nmol_C2H4_g_hr > 0) ~ poly(temp, 2) * actual_moist_perc,
                                  family = binomial, data = .)))
## not super well, somewhat well for lichen and litter but otherwise not well, 
## thus the inactive samples are likely due to other random factor we are not aware of
## deviance reduction is less when moisture is excluded
full_log_glm_results <- full %>%
  group_by(niche) %>%
  group_modify(~broom::glance(glm(I(flux_nmol_C2H4_g_hr > 0) ~ poly(temp, 2) * actual_moist_perc,
                                  family = binomial, data = .)))


## how does percent active change over temp?
ggplot(active_sum, aes(x = temp, y = perc_active, color = niche)) + 
  geom_point(size=3) + 
  geom_smooth(method = 'loess', se = F)

active_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(perc_active ~ poly(temp,2), data = .)))
## non significant relationship, likely otehr factors at play which we did not measure (i.e. presence/absence of diazotrophs)

## of the active samples, how do ethylene fluxes change with temp and moist? 
# data is highly skewed, non-normal and sinusoidal indicating non-linearity, 
# plus residuals are not normally distributed suggesting heteroscedasticity

## for the graph, going to manually define and color moisture contents
full$moisture_range <- cut(full$actual_moist_perc,
                           breaks = c(-Inf, 50, 100, 150, 200, 300, Inf),
                           labels = c("0-50", "50-100", "100-150", "150-200", "200-300", "300+"),
                           right = FALSE)
active$moisture_range <- cut(active$actual_moist_perc,
                           breaks = c(-Inf, 50, 100, 150, 200, 300, Inf),
                           labels = c("0-50", "50-100", "100-150", "150-200", "200-300", "300+"),
                           right = FALSE)

moisture_colors <- c("0-50" = "skyblue1", 
                     "50-100" = "steelblue1", 
                     "100-150" = "royalblue1", 
                     "150-200" = "royalblue3", 
                     "200-300" = "blue", 
                     "300+" = "blue4")

## now the full dataset 
clim_color_p <- ggplot(full, aes(x = temp, y = flux_nmol_C2H4_g_hr+0.00001, 
                  color = moisture_range)) +
  #geom_boxplot(outliers = F, position = position_dodge(width=0.9)) +
  geom_point(size=8, position = position_dodge(width=0.9)) +
  geom_smooth(data = subset(active, niche %in% c('Moss', 'Wood')), method = 'lm', 
             formula = y ~ poly(x,2),
             se = T, level = 0.9,
             alpha = 0.2, linetype = 'dashed')+
  facet_wrap(~niche, scales = "free_y") +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = expression("Temperature ("*degree~C*")")) + 
  theme(text = element_text(size=40), axis.text = element_text(color  = "black")) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_manual(name = "Moisture (%)", values = moisture_colors) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  theme(legend.text = element_text(size=32),
        legend.title = element_text(size=40),
        legend.position = c(0.85, 0.25),
        axis.title.x = element_text(colour = 'black')) + 
  scale_x_continuous(breaks = c(5, 15, 25, 35), labels = c("5", "15", "25", "35")) +
  geom_text(label = expression(paste("T: F"["2,15"]* " = 5.57 | p = 0.015")),
            aes(x = 20, y = 0.0001), 
            size = 9, color = "black", 
           data = subset(full, niche == "Moss")) +
  geom_text(label = expression(paste("M: F"["1,15"]* " = 8.64 | p = 0.01")),
            aes(x = 20, y = 0.00019), 
            size = 9, color = "black", 
            data = subset(full, niche == "Moss")) + # For Moss panel
  geom_text(label = expression("T: F"["1,32"]* " = 8.34 | p = 0.001"),
           aes(x = 20, y = 0.0001),
           size = 9, color = "black", 
           data = subset(full, niche == "Wood"))  # For Wood panel
clim_color_p

ggplot(full, aes(x = factor(temp, levels = temp_order),
                                 y = flux_nmol_C2H4_g_hr+0.00001, 
                                 color = moisture_range)) +
  geom_boxplot(outliers = F, position = position_dodge(width=0.9)) +
  geom_point(size=3, position = position_dodge(width=0.9)) +
  facet_wrap(~niche, scales = "free_y") +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = expression("Temperature ("*degree~C*")")) + 
  theme(text = element_text(size=40), axis.text = element_text(color  = "black")) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_manual(name = "Moisture (%)", values = moisture_colors) +
  #scale_y_log10() +
  theme(legend.text = element_text(size=32),
        legend.title = element_text(size=40),
        legend.position = c(0.85, 0.25)) 

full_sum <- full %>%
  group_by(niche, temp, moisture_range) %>%
  summarize(mean_flux = mean(flux_nmol_C2H4_g_hr),
            se_flux = sd(flux_nmol_C2H4_g_hr)/sqrt(length(flux_nmol_C2H4_g_hr)),
            n = length(flux_nmol_C2H4_g_hr))
full %>%
  group_by(niche, temp) %>%
  summarize(mean_flux = mean(flux_nmol_C2H4_g_hr),
            se_flux = sd(flux_nmol_C2H4_g_hr)/sqrt(length(flux_nmol_C2H4_g_hr)),
            n_temp = length(flux_nmol_C2H4_g_hr))
full %>%
  group_by(niche, moisture_range) %>%
  summarize(mean_flux = mean(flux_nmol_C2H4_g_hr),
            se_flux = sd(flux_nmol_C2H4_g_hr)/sqrt(length(flux_nmol_C2H4_g_hr)),
            n_moist = length(flux_nmol_C2H4_g_hr))

clim_color_p_bars <- ggplot(full_sum, aes(x = factor(temp, levels = temp_order), y = mean_flux, 
                     fill = moisture_range)) + 
  geom_col(position = position_dodge(width = 0.9), alpha = 1) + 
  #geom_point(data = full, aes(x = factor(temp, levels = temp_order),
   #                           y = flux_nmol_C2H4_g_hr,
    #                          color = moisture_range), 
     #        position = position_dodge(width = 0.9),
      #       size=3, alpha = 1.1) +
  geom_errorbar(aes(factor(temp, levels = temp_order),
                    ymax = mean_flux + se_flux,
                    ymin = mean_flux-se_flux, colour = moisture_range), 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~niche, scales = 'free_y') +
  scale_color_manual(name = "Moisture (%)", values = moisture_colors) +
  scale_fill_manual(name = "Moisture (%)", values = moisture_colors) +
  theme_minimal(base_size = 24) + 
  theme(legend.text = element_text(size=18),
        legend.title = element_text(size=24),
        legend.position = c(0.85, 0.25),
        text = element_text(size = 24, colour = 'black'),
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.text.y = element_text(size = 18, colour = 'black')) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = expression("Temperature ("*degree~C*")"))

clim_color_p_bars

### now each individual niche
autos <- full %>% filter(niche %in% c("Lichen", "Moss"))
ggplot(autos, aes(x = factor(temp, levels = temp_order), y = flux_nmol_C2H4_g_hr + 0.0001, 
                   color = actual_moist_perc,
                   size = actual_moist_perc)) + 
  geom_boxplot() +
  geom_point(size = 6) + 
  facet_wrap(~niche, scales = "free_y") +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  theme(text = element_text(size=24)) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_binned(name = "Moisture (%)") +
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=24),
        legend.position = c(0.85, 0.6))  + 
  theme(legend.position = "right") 

heteros <- active %>% filter(niche %in% c("Wood", "Soil", "Litter"))
ggplot(heteros, aes(x = factor(temp, levels = temp_order), y = flux_nmol_C2H4_g_hr , 
                   color = actual_moist_perc,
                   size = actual_moist_perc)) + 
  geom_jitter(size = 6, width = 0.2) +
  facet_wrap(~niche) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  theme(text = element_text(size=24)) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_continuous(name = "Moisture (%)") +  
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=24),
        legend.position = c(0.85, 0.6))  + 
  theme(legend.position = "right") 

summary(aov(log(flux_nmol_C2H4_g_hr+0.0001) ~ temp*actual_moist_perc, data = heteros))

Litter<- full %>% filter(niche == "Litter")
ggplot(Litter, aes(x = factor(temp, levels = temp_order), y = flux_nmol_C2H4_g_hr , 
                    color = actual_moist_perc,
                    size = actual_moist_perc)) + 
  geom_jitter(size = 6, width = 0.2) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  theme(text = element_text(size=24)) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_continuous(name = "Moisture (%)") +  
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        legend.position = c(0.19, 0.7)) 


soil<- full %>% filter(niche == "Soil")
ggplot(soil, aes(x = factor(temp, levels = temp_order), y = flux_nmol_C2H4_g_hr +0.001 , 
                   color = actual_moist_perc,
                   size = actual_moist_perc)) + 
  geom_jitter(size = 6, width = 0.2) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  theme(text = element_text(size=24)) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_continuous(name = "Moisture (%)") +
  scale_y_log10() + 
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        legend.position = c(0.19, 0.75)) 

wood <- full %>% filter(niche == "Wood")
ggplot(wood, aes(x = factor(temp, levels = temp_order), y = flux_nmol_C2H4_g_hr +0.001 , 
                 color = actual_moist_perc,
                 size = actual_moist_perc)) + 
  geom_jitter(size = 6, width = 0.2) +
  ylab(label = expression("AR rate (nmol C"[2]*"H"[4]*" g"^-1*" h"^-1*")")) +
  xlab(label = element_blank()) + 
  theme(text = element_text(size=24)) + 
  theme(plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_binned(name = "Moisture (%)") +
  scale_y_log10() + 
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        legend.position = c(0.84, 0.25)) 

##### 2 WAY ANOVA M * T for each niche active and full dataset ####
active %>%
  group_by(niche, temp) %>%
  summarize(n = length(flux_nmol_C2H4_g_hr))

active_results <- active %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(aov(log10(flux_nmol_C2H4_g_hr) ~ poly(temp,2)*sqrt(actual_moist_perc), data = .)))
### when all samples are considered, temp and moist are insignificant effects, 
## full not just active

full %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(aov(log(flux_nmol_C2H4_g_hr+0.0001) ~ temp*actual_moist_perc, data = .)))
### when all samples are considered, temp and moist are insignificant effects, 
full_results <- full %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(aov(log10(flux_nmol_C2H4_g_hr+0.0001) ~ poly(temp,2)*actual_moist_perc, data = .)))
### when all samples are considered, temp and moist are insignificant effects, 
## however when just active samples are considered temp is significant for wood

plot(lm(log10(flux_nmol_C2H4_g_hr) ~ poly(temp,2)*actual_moist_perc, data = active))



