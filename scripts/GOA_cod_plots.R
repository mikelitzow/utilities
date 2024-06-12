# plot GOA Pcod time series

library(tidyverse)

theme_set(theme_bw())

dat <- read.csv("./data/GOApcod.csv")

head(dat)

dat1 <- dat %>% 
  select(Year, Catch, Total.biomass) %>%
  pivot_longer(cols = -Year) %>%
  mutate(name = reorder(name, desc(name)),
         value = value / 1000)


ggplot(dat1, aes(Year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, scale = "free_y", ncol = 1) +
  ylab("Thousands of mt")

ggsave("./figs/goa_cod_biomass_catch.png", width = 6, height = 6, units = 'in')

dat2 <- dat %>% 
  select(Year, Recruitment) %>%
  pivot_longer(cols = -Year)


ggplot(dat2, aes(Year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name, scale = "free_y", ncol = 1) +
  ylab("Age-0 recruits (billions)")

ggsave("./figs/goa_cod_recruitment.png", width = 6, height = 3.5, units = 'in')


# plot catch and ABC

dat3 <- dat %>% 
  select(Year, Catch, Acceptable_biological_catch) %>%
  mutate(perc.Catch = 100*(Catch/Acceptable_biological_catch)) %>%
  rename(`% of ABC caught (Federal and State fisheries)` = perc.Catch,
         `Acceptable biological catch (ABC)` = Acceptable_biological_catch) %>%
  pivot_longer(cols = -Year) %>%
  filter(Year >= 2010)


dat3$plot.order <- if_else(
    dat3$name == "Catch", 1, 
    if_else(name == `% of ABC caught (Federal and State fisheries)`, 2, 3)),
  name = reorder(name, plot.order)) 

ggplot(dat3, aes(as.factor(as.character(Year)), value)) +
  geom_col(fill = "grey", col = "black") +
  # geom_line() +
  facet_wrap(~name, scale = "free_y", ncol = 1) +
  xlab("Year")

ggsave("./figs/goa_cod_ABC_v_catch.png", width = 6, height = 7, units = 'in')

#### plot SSB from 2014-2018 SAFES

dat <- read.csv("./data/GOA.Pcod.assessments.csv")

head(dat)

# scale each relative to max (since author / model changed in 2016)

max_values <- NA

for(j in 1:5){
  
  max_values[j] <- max(dat[,(j+1)], na.rm = T)
}

scaled_dat <- dat

for(j in 2:6){
  
  scaled_dat[,j] <- dat[,j]/max_values[(j-1)]
  
  
}

names(scaled_dat)[2:6] <- 2014:2018

scaled_dat <- scaled_dat %>%
  pivot_longer(cols = -Year) %>%
  rename(Assessment = name)



ggplot(filter(scaled_dat, Year >= 2000), aes(Year, value, color = Assessment)) +
  geom_point() +
  geom_line() +
  ylab("   Spawning stock biomass
       (proportion of max since 1977)") +
  scale_color_viridis_d()

ggsave("./figs/goa_cod_SSB_2014-2018.png", width = 6, height = 4, units = 'in')
