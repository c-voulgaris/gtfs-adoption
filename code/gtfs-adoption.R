##########################################################
# Analysis of GTFS adoption
##########################################################

library(tidyverse)
library(here)
library(lubridate)

agency_data <- here("assembled-data",
     "agency-data.csv") %>%
  read_csv() 

######### visualize change in market penetration over time ####
adoption_rates <- tibble(Date = seq(ymd("2005-1-1"), 
                                    ymd("2022-6-1"), 
                                    by = "months"),
                         num_agencies = 0,
                         num_adopted = 0) 

for (i in 1:length(adoption_rates$Date)) {
  adoption_rates$num_agencies[i] = 
    sum(agency_data$gtfs_status < 2) +
    sum(agency_data$gtfs_status == 2 &
          agency_data$gtfs_date > adoption_rates$Date[i])
  
  adoption_rates$num_adopted[i] = 
    sum(agency_data$gtfs_status == 1 & 
          agency_data$gtfs_date < adoption_rates$Date[i])
}

adoption_rates <- adoption_rates %>%
  mutate(`Percent adoption of GTFS data standard` = 
           num_adopted / num_agencies)

ggplot(adoption_rates) +
  geom_point(aes(x = Date,
                 y = `Percent adoption of GTFS data standard`),
             size = 1) +
  annotate("segment", 
           x = ymd("2005-12-07"), 
           xend = ymd("2005-12-07"), 
           y = 0.002, 
           yend = 0.8,
           color = "gray") +
  annotate("text", 
         x = ymd("2005-12-07"), 
         y = 0.85,
         label = "Portland,\nOregon",
         size = 3.5) +
  annotate("segment", 
           x = ymd("2005-12-07"), 
           xend = ymd("2005-12-07"), 
           y = 0.002, 
           yend = 0.8,
           color = "gray") +
  annotate("text", 
           x = ymd("2005-12-07"), 
           y = 0.85,
           label = "Portland,\nOregon",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2008-04-01"), 
           xend = ymd("2008-04-01"), 
           y = 0.05, 
           yend = 0.65,
           color = "gray") +
  annotate("text", 
           x = ymd("2008-04-01"), 
           y = 0.7,
           label = "Boston,\nMassachussetts",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2012-08-25"), 
           xend = ymd("2012-08-25"), 
           y = 0.31, 
           yend = 0.77,
           color= "gray") +
  annotate("text", 
           x = ymd("2012-08-25"), 
           y = 0.82,
           label = "Atlanta,\nGeorgia",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2017-08-27"), 
           xend = ymd("2017-08-27"), 
           y = 0.61, 
           yend = 0.7,
           color = "gray") +
  annotate("text", 
           x = ymd("2017-08-27"), 
           y = 0.75,
           label = "Portland,\nMaine",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2020-11-19"), 
           xend = ymd("2020-11-19"), 
           y = 0.7, 
           yend = 0.8, 
           color = "gray") +
  annotate("text", 
           x = ymd("2020-11-19"), 
           y = 0.85,
           label = "Shreveport,\nLouisiana",
           size = 3.5) +
  scale_y_continuous(limits = c(0,1),
                     breaks = breaks <- seq(0, 1, by = 0.1),
                     labels = paste0(breaks * 100, "%")) +
  theme_classic() 

here("figures",
     "gtfs-observed.png") %>%
  ggsave(width = 6,
         height = 6,
         units = "in")

########### Regression analysis ############################

# change status variable to 0 for all censoring (either 
# not-yet adopted or agency was dissolved)

# Set agency type and institution type as factors

agency_data <- agency_data %>%
#  mutate(gtfs_status = ifelse(gtfs_status != 1, 0, 1)) %>%
  mutate(inst_type = as_factor(inst_type)) %>%
  mutate(agency_type = as_factor(agency_type)) %>%
  mutate(inst_type = relevel(inst_type, ref = "2")) %>%
  mutate(agency_type = relevel(agency_type, ref = "2"))
  
# Cox regression model

cox_model <- coxph(Surv(time_to_adopt_gtfs, gtfs_status) ~ 
        VOMS,
        #Population +
        #`Population Density` +
        #VOMS +
        #VRM +
        #fare_recovery +
       # overhead +
        #n_in_uza +
        #VRM_UZA_share +
        #pct_rented +
        #Region +
        #inst_type +
     #   agency_type, 
      data = agency_data)

linear_model <- lm(time_to_adopt_gtfs ~ 
                     ridership +
                     Population +
                     `Population Density` +
                     VOMS +
                     VRM +
                     fare_recovery +
                     overhead +
                     n_in_uza +
                     VRM_UZA_share +
                     pct_rented +
                     Region +
                     inst_type +
                     agency_type, 
                   data = agency_data[agency_data$gtfs_status != 2,])

summary(cox_model)

AIC(cox_model)
