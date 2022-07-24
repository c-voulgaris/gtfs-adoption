##########################################################
# Analysis of GTFS adoption
##########################################################

library(tidyverse)
library(here)
library(lubridate)
library(GGally)
library(lme4)
library(ggplot2)
library(jtools)
library(huxtable)
library(sandwich)

agency_data <- here("assembled-data",
                    "agency-data.csv") %>%
  read_csv() %>%
  filter(!is.na(status)) %>%
  #### The "date" column is read in as a character column, you may convert it to a date column to allow the following date comparison.
  mutate(gtfs_date = as.Date(date, "%d-%b-%y")) %>%
  rename(gtfs_status = status)

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
           yend = 0.1,
           color = "gray") +
  annotate("text", 
         x = ymd("2005-12-07"), 
         y = 0.15,
         label = "TriMet,\nPortland, OR",
         size = 3.5) +
  annotate("segment", 
           x = ymd("2006-09-15"), 
           xend = ymd("2006-09-15"), 
           y = 0.002, 
           yend = 0.2,
           color = "gray") +
  annotate("text", 
           x = ymd("2006-09-15"), 
           y = 0.25,
           label = "KC Metro,\nSeattle, WA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2007-01-01"), 
           xend = ymd("2007-01-01"), 
           y = 0.01, 
           yend = 0.3,
           color = "gray") +
  annotate("text", 
           x = ymd("2007-01-01"), 
           y = 0.35,
           label = "BART,\nSan Francisco, CA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2008-03-23"), 
           xend = ymd("2008-03-23"), 
           y = 0.04, 
           yend = 0.35,
           color = "gray") +
  annotate("text", 
           x = ymd("2008-03-23"), 
           y = 0.4,
           label = "SEPTA,\nPhiladelphia, PA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2008-04-01"), 
           xend = ymd("2008-04-01"), 
           y = 0.05, 
           yend = 0.45,
           color = "gray") +
  annotate("text", 
           x = ymd("2008-04-01"), 
           y = 0.5,
           label = "MBTA,\nBoston, MA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2008-6-1"), #date is assumed, only year is confirmed
           xend = ymd("2008-6-1"), 
           y = 0.06, 
           yend = 0.85,
           color = "red",
           linetype = "dashed") +
  annotate("text", 
           x = ymd("2008-6-1"), 
           y = 0.9,
           label = "Launched GTFS\nData Exchange",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2008-12-14"), 
           xend = ymd("2008-12-14"), 
           y = 0.09, 
           yend = 0.55,
           color = "gray") +
  annotate("text", 
           x = ymd("2008-12-14"), 
           y = 0.6,
           label = "LA Metro,\nLos Angeles, CA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2009-03-23"), 
           xend = ymd("2009-03-23"), 
           y = 0.11, 
           yend = 0.65,
           color = "gray") +
  annotate("text", 
           x = ymd("2009-03-23"), 
           y = 0.7,
           label = "WMATA,\nWashington, DC",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2009-10-20"), 
           xend = ymd("2009-10-20"), 
           y = 0.12, 
           yend = 0.75,
           color = "gray") +
  annotate("text", 
           x = ymd("2009-10-20"), 
           y = 0.8,
           label = "CTA,\nChicago, IL",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2010-1-03"), 
           xend = ymd("2010-1-03"), 
           y = 0.15, 
           yend = 0.85,
           color = "gray") +
  annotate("text", 
           x = ymd("2010-1-03"), 
           y = 0.9,
           label = "MTA,\nNew York City, NY",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2010-1-11"), 
           xend = ymd("2010-1-11"), 
           y = 0.15, 
           yend = 0.95,
           color = "red",
           linetype = "dashed") +
  annotate("text", 
           x = ymd("2010-1-11"), 
           y = 1,
           label = "Renamed General Transit\nFeed Specifications",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2010-06-27"), 
           xend = ymd("2010-06-27"), 
           y = 0.16, 
           yend = 0.45,
           color = "gray") +
  annotate("text", 
           x = ymd("2010-06-27"), 
           y = 0.5,
           label = "Miami-Dade Transit,\nMiami, FL",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2011-08-01"), 
           xend = ymd("2011-08-01"), 
           y = 0.24, 
           yend = 0.85,
           color = "red",
           linetype = "dashed") +
  annotate("text", 
           x = ymd("2011-08-01"), 
           y = 0.9,
           label = "Real-time GTFS\nlaunched",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2012-08-25"), 
           xend = ymd("2012-08-25"), 
           y = 0.31, 
           yend = 0.65,
           color= "gray") +
  annotate("text", 
           x = ymd("2012-08-25"), 
           y = 0.7,
           label = "MARTA,\nAtlanta, GA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2014-09-28"), 
           xend = ymd("2014-09-28"), 
           y = 0.42, 
           yend = 0.7,
           color= "gray") +
  annotate("text", 
           x = ymd("2014-09-28"), 
           y = 0.75,
           label = "KCATA,\nKansas City, MO",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2016-05-13"), 
           xend = ymd("2016-05-13"), 
           y = 0.55, 
           yend = 0.77,
           color= "gray") +
  annotate("text", 
           x = ymd("2016-05-13"), 
           y = 0.82,
           label = "MTS,\nSan Diego, CA",
           size = 3.5) +
  annotate("segment", 
           x = ymd("2017-08-27"), 
           xend = ymd("2017-08-27"), 
           y = 0.61, 
           yend = 0.8,
           color = "gray") +
  annotate("text", 
           x = ymd("2017-08-27"), 
           y = 0.85,
           label = "Greater Portland Metro,\nPortland, ME",
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
           label = "SporTran,\nShreveport, LA",
           size = 3.5) +
  scale_y_continuous(limits = c(0,1),
                     breaks = breaks <- seq(0, 1, by = 0.1),
                     labels = paste0(breaks * 100, "%")) +
  theme_classic() 

here("figures",
     "gtfs-observed.png") %>%
  ggsave(width = 12,
         height = 6,
         units = "in")

########### Regression analysis ############################

# change status variable to 0 for all censoring (either 
# not-yet adopted or agency was dissolved)

# Set agency type and institution type as factors

agency_data <- agency_data %>%
  mutate(gtfs_status = ifelse(gtfs_status != 1, 0, 1)) %>%
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

### Logistic regression

long_data <- here("assembled-data",
                  "final-data.csv") %>%
  read_csv() %>%
  rename(penetration = `Percent adoption of GTFS data standard`) %>%
  select(-X1) %>%
  mutate(Region = as_factor(Region),
         Division = as_factor(Division),
         Org_Type = as_factor(Org_Type)) %>%
  mutate(Region = relevel(Region, ref = "r4"),
         Division = relevel(Division, ref = "d9"),
         Org_Type = relevel(Org_Type, ref = "Area Agency on Aging"))

# Clustering by transit agency (ID, NTDID, Company_Nm)

# Agency-level variables
## State
## Agency Type
## Institution type
## Region
## Division

# Year-level variables
## ridership
## Population
## Population density
## VOMS
## VRM
## fare_recovery
## overhead
## n_in_uza
## VRM_UZA_share
## pct_rented
## penetration

# estimate a logistic regression model
## We may divide the variables into several categories, and it's better not to include variables from the same category to one model.
### Region, Division
### Org_Type
### Population, Population Density
### n_in_uza, VRM_UZA_share
### year, penetration, num_adopted
### VOMS, VRM, fare_rev, ridership, op_exp, gen_admin_salary
### pct_rented
### overhead
### fare_recovery
### Agency_Type_Desc, Org_Type, Institution_Type_Desc (Since there are too many NA cells in these columns, they are not included in the model.)

## Model 1 is the one with one variable from each category
model_1 <-glm(adopted_yet ~
                Division +
                Org_Type +
                scale(`Population Density`) +
                scale(VRM_UZA_share) +
                scale(num_adopted) +
                scale(VRM) +
                scale(pct_rented) +
                scale(overhead) +
                scale(fare_recovery),
              data = long_data,
              family = binomial)

## Model 2 is the best fit model. Even though year, penetration, num_adopted come from the same category; VOMS, ridership, op_exp come from the same category, including them all increase the model fit. 
model_2 <-glm(adopted_yet ~
                Division +
                Org_Type +
                scale(`Population Density`) +
                scale(VRM_UZA_share) +
                scale(year) +
                scale(penetration) +
                scale(num_adopted) +
                scale(VOMS) +
                scale(ridership) +
                scale(op_exp) +
                scale(pct_rented) +
                scale(overhead) +
                scale(fare_recovery),
              data = long_data,
              family = binomial)

## Model 3 is a very different model to serve as a comparison. 
model_3 <-glm(adopted_yet ~
                Region +
                Org_Type +
                scale(Population) +
                scale(n_in_uza) +
                scale(num_adopted) +
                scale(VOMS) +
                scale(pct_rented) +
                scale(fare_recovery),
              data = long_data,
              family = binomial)

# Display regression results with cluster-robust standard errors
export_summs(model_1, model_2, model_3,
             robust = "HC3", cluster = "ID")

# Show variation in probability of adoption by numeric variables (Using Model 2)
## Note: some variables are log-transformed.

## Population Density
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = `Population Density`)

## VRM_UZA_share
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = VRM_UZA_share)

## year
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = year)

## penetration
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = penetration)

## num_adopted
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = num_adopted)

## VOMS
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = VOMS) +
  scale_x_continuous(trans = "log",
                     breaks = c(5,10,100,1000))

## ridership
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = ridership) +
  scale_x_continuous(trans = "log",
                     breaks = breaks <- 10^seq(6, 9, by=1),
                     labels = formatC(breaks, 
                                      big.mark = ",", 
                                      format = "f",
                                      digits = 0))

## op_exp
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = op_exp) +
  scale_x_continuous(trans = "log",
                     breaks = breaks <- 10^seq(4, 8, by=1),
                     labels = formatC(breaks, 
                                      big.mark = ",", 
                                      format = "f",
                                      digits = 0))

## pct_rented
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = pct_rented)

## overhead
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = overhead)

## fare_recovery
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = fare_recovery)

# Show variation in probability of adoption by categorical variables
## division
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = Division)

## org_type
effect_plot(model_2, 
            robust = "HC3", 
            cluster = "ID",
            interval = TRUE,
            pred = Org_Type)
