## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(tidyverse)
library(patchwork)
library(ggthemes)


## ----data---------------------------------------------------------------------
x = 1:300
y = x * sin(0.1 * x)
dat_tab <- tibble(x = x,
       xend = x+0.9999,
       y = y)

x = 1:400
y = x * sin(0.2 * x) + 100
dat_tab_bis <- tibble(x = x,
       xend = x+0.9999,
       y = y)

tab_tot <- mutate(dat_tab, type = 'A') %>% 
  bind_rows(mutate(dat_tab_bis, type='B'))

tab_tot %>% 
  ggplot() +
  geom_line(aes(x, y)) +
  facet_wrap(~type, scales = 'free_y', ncol = 1) +
  theme_few()


## ----set----------------------------------------------------------------------
library(ggHoriPlot)


plotAllLayers <- function(dat, ori, cutpoints, colors){
  # Helper function to plot the origin and cutpoints 
  # of the horizon plot for comparison
  p <- ggplot()
  acc <- 1
  for (i in cutpoints[cutpoints<=ori]) { 
    colo <- colors[acc]
    p <- p + geom_ribbon(aes(x = x, y = y, ymin = y, ymax = ori),
                         fill = colo,
              data = mutate(dat, y = ifelse(between(y, i, ori), y, 
                                                ifelse(y<ori, i, ori))))
    acc <- acc+1
  }
  for (i in cutpoints[cutpoints>=ori]) { 
    colo <- colors[acc]
    p <- p + geom_ribbon(aes(x = x, y = y, ymin = ori, ymax = y),
                         fill = colo,
              data = mutate(dat, y = ifelse(between(y, ori, i), y, 
                                                ifelse(y>ori, i, ori))))
    acc <- acc+1
  }
  
  
  p+geom_line(aes(x, y), data=dat)+
    theme_few()
}


## ----midpoint_1, fig.height=1, fig.width=7------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(aes(x = x, y=y)) 

a


## ----midpoint_2, fig.height=1, fig.width=7------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(aes(x = x, y=y)) +
  theme_few()  +
  scale_fill_hcl()

a


## ----midpoint_3---------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y)
  ) +
  theme_few()  +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)


mid <- sum(range(dat_tab$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))



## ----midpoint_4_bis-----------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y, 
    fill=..Cutpoints..)
  ) +
  theme_few()  +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)


mid <- sum(range(dat_tab$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))



## ----median-------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..), 
    origin = 'median'
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(96.20134, 190.4594, 284.7174, -92.31478, -186.57283, -280.83089),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

me <- median(dat_tab$y, na.rm = T)

b <- plotAllLayers(dat_tab, me, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----mean---------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..), 
    origin = 'mean'
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(91.89319,  186.15125,  280.40931,  -96.62292, -190.88098, -285.13903),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(
    names = factor(names, rev(names)),
    y_max = ifelse(cuts == min(cuts),
              -Inf, 
              ifelse(
                cuts == max(cuts), 
                Inf,
                cuts))) %>% 
  arrange(names)


me <- mean(dat_tab$y, na.rm = T)

b <- plotAllLayers(dat_tab, me, cutpoints$cuts, cutpoints$color)


b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----integer------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..), 
    origin = 50
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(144.25806,  238.51611,  332.77417,  -44.25806, -138.51611, -232.77417),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)



b <- plotAllLayers(dat_tab, 50, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----quantiles----------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..), 
    origin = 'quantiles'
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(43.02642,  124.15063,  266.99833,  -45.43396, -119.31147, -298.55001 ),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

me <- median(dat_tab$y, na.rm = T)

b <- plotAllLayers(dat_tab, me, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----min----------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 6,
    origin = 'min'
  ) +
  theme_few() +
  scale_fill_hcl()



cutpoints_a  <- tibble(
  cuts = c(-15.78, 78.48,  172.74,  266.998, -110.034, -204.292, -298.55),
  color = c("#D7E2D4", "#36ABA9", "#324DA0", 'white', "#F6DE90", "#E78200", "#A51122")
) 

cutpoints_a  <- cutpoints_a %>% arrange(desc(cuts))


b <- plotAllLayers(dat_tab, -298.55, cutpoints_a$cuts, cutpoints_a$color)

(b/a) + plot_layout(guides = 'collect', heights = c(6, 1))


## ----min_2--------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 6,
    origin = 'min'
  ) +
  theme_few() +
  scale_fill_hcl(palette = 'Purple-Orange', reverse = T)



cutpoints_a  <- tibble(
  cuts = c(-15.78, 78.48,  172.74,  266.998, -110.034, -204.292, -298.55),
  color = c( "#B76AA8", "#8F4D9F","#5B3794", 'white', "#D78CB1", "#F1B1BE", "#F8DCD9")
) 

cutpoints_a  <- cutpoints_a %>% arrange(desc(cuts))


b <- plotAllLayers(dat_tab, -298.55, cutpoints_a$cuts, cutpoints_a$color)

(b/a) + plot_layout(guides = 'collect', heights = c(6, 1))


## ----midpoint_n5--------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 5,
    origin = 'midpoint'
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(97.33383,  210.44349, -128.88551, -241.99518, -355.10485),
  names = c('ypos1', 'ypos2',  'yneg1', 'yneg2', 'yneg3'), 
  color = c("#69BBAB", "#324DA0", "#FEFDBE", "#EB9C00", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)


mid <- sum(range(dat_tab$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(5, 1))


## ----midpoint_n10-------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 10,
    origin = 'midpoint'
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(40.77899 ,  97.33383 , 153.88866,  210.44349 , 266.99833 ,
           -72.33068, -128.88551, -185.44035, -241.99518, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'ypos4', 'ypos5', 'yneg1', 'yneg2', 'yneg3', 'yneg4', 'yneg5'), 
  color = c("#E5F0D6", "#ACD2BB" ,"#4EB2A9" ,"#0088A7", "#324DA0",
                "#FAEDA9","#F1C363","#E98E00", "#DC4A00", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

mid <- sum(range(dat_tab$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)


b/a + plot_layout(guides = 'collect', heights = c(10, 1))


## ----midpoint_4---------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y, 
    fill=..Cutpoints..),
    horizonscale = c(78.48221,  172.74027,  
                     266.99833, -110.03390, 
                     -204.29196, -298.55001),
    origin = -15.77584
  ) +
  theme_few()  +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)


mid <- sum(range(dat_tab$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))



## ----midpoint_with_xend-------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 6,
    origin = 'midpoint'
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

dt <- dat_tab %>% 
  pivot_longer(c(x, xend)) %>% 
  mutate(x = value)

b <- plotAllLayers(dt, mid, cutpoints$cuts, cutpoints$color)

b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----faceting, fig.height=2, fig.width=7--------------------------------------
tab_tot %>% 
  ggplot() +
  geom_horizon(aes(x = x, y=y)) +
  facet_wrap(~type, ncol = 1, scales = 'free_y') +
  theme_few() +
  scale_fill_hcl()


## ----faceting_cutpoints, fig.height=2, fig.width=7----------------------------
tab_tot %>% 
  ggplot() +
  geom_horizon(aes(x = x, y=y, fill = ..Cutpoints..)) +
  facet_wrap(~type, ncol = 1, scales = 'free_y') +
  theme_few() +
  scale_fill_hcl()


## ----faceting_calculating_cutpoints, fig.height=2, fig.width=7----------------
ori <- sum(range(tab_tot$y))/2
sca <- seq(range(tab_tot$y)[1], range(tab_tot$y)[2], length.out = 7)[-4]

tab_tot %>% 
  ggplot() +
  geom_horizon(aes(x = x, y=y, fill = ..Cutpoints..),
               origin = ori, horizonscale = sca) +
  facet_wrap(~type, ncol = 1, scales = 'free_y') +
  theme_few() +
  scale_fill_hcl()


## ----midpoint_outlier_1-------------------------------------------------------
x = 1:300
y = sin(0.1 * x) + 50*dnorm(x, 150, 2)
dat_tab_outlier <- tibble(x = x,
       xend = x+0.9999,
       y = y)

a1 <- dat_tab_outlier %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..)
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints_a1  <- tibble(
  cuts = c(6.7492332,  8.6865390, 10.6238449,  2.8746215,  0.9373156, -0.9999902),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

b1 <- plotAllLayers(dat_tab_outlier, 4.811927, cutpoints_a1$cuts, cutpoints_a1$color)

(b1/a1) + plot_annotation("origin = 'midpoint', rm.outliers = F")+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----midpoint_outlier_2-------------------------------------------------------
a3 <- dat_tab_outlier %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 6,
    origin = 'median',
    rm.outliers = F
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints_a3  <- tibble(
  cuts = c(2.008043,  3.945348,  5.882654, -1.866569, -3.803875, -5.741181),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

b3 <- plotAllLayers(dat_tab_outlier, 0.07073676, cutpoints_a3$cuts, cutpoints_a3$color)

(b3/a3) + plot_annotation("origin = 'median', rm.outliers = F")+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----midpoint_outlier_2_bis---------------------------------------------------
a2 <- dat_tab_outlier %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = 6,
    origin = 'median',
    rm.outliers = T
  ) +
  theme_few() +
  scale_fill_hcl()


cutpoints_a2  <- tibble(
  cuts = c(0.5743823,  1.1151416,  1.6559008, -0.5071362, -1.0478955, -1.5886547),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)



b2 <- plotAllLayers(dat_tab_outlier, 0.03362305, cutpoints_a2$cuts, cutpoints_a2$color)


(b2/a2) + plot_annotation("origin = 'median', rm.outliers = T")+ plot_layout(guides = 'collect', heights = c(6, 1))



## ----custom_horizonscale------------------------------------------------------
a4 <- dat_tab_outlier %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y,
    fill=..Cutpoints..), 
    horizonscale = c(seq(-1, 1, 1/3))[-4],
    origin = 0
  ) +
  theme_few() +
  scale_fill_hcl()


cutpoints_a4  <- tibble(
  cuts = c(1/3,  2/3,  1, -1/3, -2/3, -1),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

b4 <- plotAllLayers(dat_tab_outlier, 0, cutpoints_a4$cuts, cutpoints_a4$color)



(b4/a4) + plot_annotation("origin = 0, rm.outliers = F, custom horizonscale")+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----midpoint_NA--------------------------------------------------------------
x = 1:300
y = x * sin(0.1 * x)
dat_tab_na <- tibble(x = x,
       xend = x+0.9999,
       y = y) %>% 
  mutate(
    y = ifelse(between(x, 125, 175), NA, y)
  )


a <- dat_tab_na %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y,
    fill=..Cutpoints..), 
  ) +
  theme_few() +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)


mid <- sum(range(dat_tab_na$y, na.rm = T))/2

b <- plotAllLayers(dat_tab_na, mid, cutpoints$cuts, cutpoints$color)


b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----reverse------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    # xend = xend, 
    y=y, 
    fill=..Cutpoints..), 
    horizonscale = 6,
    origin = 'midpoint',
    reverse = T
  ) +
  theme_few()  +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

mid <- sum(range(dat_tab_na$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)


b/a+ plot_layout(guides = 'collect', heights = c(6, 1))


## ----mirror-------------------------------------------------------------------
a <- dat_tab %>% 
  ggplot() +
  geom_horizon(
    aes(x = x, 
    y=y, 
    fill=..Cutpoints..),
    mirror = T
  ) +
  theme_few()  +
  scale_fill_hcl()

cutpoints  <- tibble(
  cuts = c(78.48221,  172.74027,  266.99833, -110.03390, -204.29196, -298.55001),
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = c("#D7E2D4", "#36ABA9", "#324DA0", "#F6DE90", "#E78200", "#A51122")
) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

mid <- sum(range(dat_tab_na$y, na.rm = T))/2

b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)


b/a+ plot_layout(guides = 'collect', heights = c(6, 1))



