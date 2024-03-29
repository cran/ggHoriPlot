## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(tidyverse)
library(ggthemes)
library(ggHoriPlot)


## ----sports_leisure-----------------------------------------------------------
utils::data(sports_time)

sports_time %>% ggplot() +
  geom_horizon(aes(time/60, p), origin = 'min', horizonscale = 4) +
  facet_wrap(~activity, ncol = 1, strip.position = 'right') +
  scale_fill_hcl(palette = 'Peach', reverse = T) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(angle = 0),
    legend.position = 'none',
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
    ) +
  scale_x_continuous(
    name = 'Time',
    breaks=seq(from = 3, to = 27, by = 3),
    labels = function(x) {sprintf("%02d:00", as.integer(x %% 24))}) +
  ggtitle('Peak time of day for sports and leisure')


## ----covid_19-----------------------------------------------------------------
utils::data(COVID)


COVID %>%  
  ggplot() +
  geom_horizon(aes(date_mine, 
                   y), origin = 'min', horizonscale = 4) +
  scale_fill_hcl(palette = 'BluGrn', reverse = T) +
  facet_grid(countriesAndTerritories~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    legend.position = 'none',
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
    ) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b") +
  ggtitle('Cumulative number for 14 days of COVID-19 cases per 100,000', 
          'in Asia, 2020') +
  xlab('Date')


## ----repeats------------------------------------------------------------------
utils::data(rmsk)


cutpoint_tab <- rmsk %>% 
  ungroup() %>% 
      mutate(
        outlier = between(
          p_repeat, 
          quantile(p_repeat, 0.25, na.rm=T)-1.5*IQR(p_repeat, na.rm=T),
          quantile(p_repeat, 0.75, na.rm=T)+1.5*IQR(p_repeat, na.rm=T))) %>% 
  filter(outlier)
ori <- sum(range(cutpoint_tab$p_repeat, na.rm = T))/2
sca <- seq(range(cutpoint_tab$p_repeat)[1], 
           range(cutpoint_tab$p_repeat)[2],
           length.out = 6)
  
rmsk %>%  
  ggplot() +
  geom_horizon(aes(x = bin, xend=bin_2,  y = p_repeat, fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  facet_grid(genoName~., switch = 'y') +
  theme_few() +
    theme(
      panel.spacing.y=unit(0, "lines"),
      strip.text.y.left = element_text(size = 7, angle = 0, hjust = 1),
      legend.position = c(0.85, 0.4),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank()
      ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_hcl() +
  ggtitle('Simple repeat content along the human genome', 
          'in 100 kb windows') +
  xlab('Position') +
  guides(fill=guide_legend(title="% of repeats"))


## ----fig.height=15, fig.width=6-----------------------------------------------


utils::data(climate_US)


climate_US %>% 
  ggplot() +
  geom_horizon(aes(date_mine, 
                   AvgTemperature), rm.outliers = T) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(State+City~.) +
  theme_bw() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 4, angle = 0),
    legend.position = 'none',
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),panel.grid = element_blank()
    ) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b") +
  xlab('Date') +
  ggtitle('Average daily temperature of major US cities', 
          'for the year 2000')



## -----------------------------------------------------------------------------
utils::data(climate_CPH)

cutpoints <- climate_CPH  %>% 
  mutate(
    outlier = between(
      AvgTemperature, 
      quantile(AvgTemperature, 0.25, na.rm=T)-1.5*IQR(AvgTemperature, na.rm=T),
      quantile(AvgTemperature, 0.75, na.rm=T)+1.5*IQR(AvgTemperature, na.rm=T))) %>% 
  filter(outlier)

ori <- sum(range(cutpoints$AvgTemperature))/2
sca <- seq(range(cutpoints$AvgTemperature)[1], range(cutpoints$AvgTemperature)[2], length.out = 7)[-4]
  

climate_CPH %>% ggplot() +
  geom_horizon(aes(date_mine, 
                   AvgTemperature,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(Year~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
    ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('Average daily temperature in Copenhagen', 
          'from 1995 to 2019')


