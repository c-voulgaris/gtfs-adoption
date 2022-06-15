### The purpose of this file is to assemble the data to be
# used in a regression analysis predicting the time it takes
# for a transit agency to adopt GTFS.

# TODO: Get data for each year.


library(tidyverse)
library(readxl)
library(tidycensus)
library(here)

### Load NTD data
agencies <- tibble(ID = c(""),
                   Agency_Type_Desc = c(""),
                   Institution_Type_Desc = c(""),
                   Company_Nm = c(""),
                   Service_Area = c(""),
                   year = 0)

service <- tibble(ID = c(""),
                  trips = 0,
                  VRM = 0,
                  year = 0)

service_area <- tibble(UZA = c(""),
                       `Urbanized Area` = c(""),
                       `Population` = 0,
                       `Square Miles` = 0,
                       `Population Density` = 0,
                       `Transit Agency` = "",
                       ID = "",
                        year = 0)

salary <- tibble(ID = c(""),
                 gen_admin_salary = 0,
                 year = 0)

farebox <- tibble(ID = c(""),
                  State = c(""),
                  Name = c(""),
                  VOMS = 0,
                  fare_rev = 0,
                  op_exp = 0,
                  fare_recovery = 0,
                  year = 0)

###### Mengyao to updated everything in this loop so it works for all years
# after downloading the spreadsheets for all years

for (i in 2005:2008) {
  ### agency info
  these_agencies <- here("NTD_data",
                   paste0("y", i),
                   paste0(i, "_agency_info.xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    select(Trs_Id,
           Agency_Type_Desc,
           Institution_Type_Desc,
           Company_Nm,
           Service_Area) %>%
    rename(ID = Trs_Id) %>%
    mutate(year = i)
    
  agencies <- rbind(agencies, these_agencies)
  
  ### Service (service spreadsheet)
  these_service <- here("NTD_data",
                        paste0("y", i),
                        paste0(i, "_Service.xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    rename_all(tolower) %>%
    filter(time_period_desc == "Annual Total") %>%
    filter((passenger_car_sched_rev_miles > 0) | (vehicle_sched_miles > 0)) %>%
    group_by(trs_id) %>%
    summarise(trips = sum(unlinked_passenger_trips),
              VRM = sum(vehicle_or_train_rev_miles)) %>%
    rename(ID = trs_id) %>%
    mutate(year = i)
  
  service <- rbind(service, these_service)
  
  skip <- ifelse(i < 2007, 4, 1)
  
  ### Service ares (appendix D)
  these_service_area <- here("NTD_data",
                       paste0("y", i),
                       paste0(i, "_Appendix_D.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    select(UZA, 
           `Urbanized Area`,
           Population,
           `Square Miles`,
           `Population Density`,
           `Transit Agency`,
           ID) %>%
    mutate(year = i)
    
  if (i < 2007) { 
    these_service_area <- these_service_area %>%
      fill(UZA, 
           `Urbanized Area`,
           Population,
           `Square Miles`,
           `Population Density`)
  }
  service_area <- rbind(service_area, these_service_area)
  
  ### Salary (operating expenses)
  these_salary <- here("NTD_data",
                 paste0("y", i),
                 paste0(i, "_Operating_Expenses.xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    rename_all(tolower) %>%
    select(trs_id, 
           expense_category_desc,
           op_sal_wage_amt,
           other_sal_wage_amt,
           fringe_benefit_amt)  %>%
    rename(ID = trs_id) %>%
    filter(expense_category_desc == "General Administration") %>%
    group_by(ID) %>%
    mutate(salary_plus_fringe = op_sal_wage_amt +
             other_sal_wage_amt +
             fringe_benefit_amt) %>%
    summarise(gen_admin_salary = sum(salary_plus_fringe)) %>%
    mutate(year = i)
  
  salary <- rbind(salary, these_salary)
  
  ### farebox (Table 26)
  skip <- ifelse(i < 2007, 3, 1)
  
  these_farebox <- here("NTD_data",
                  paste0("y", i),
                  paste0(i, "_Table_26.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    fill(State, Name) %>%
    filter(!is.na(ID)) %>%
    select(-...9,
           -...11,
           -...13,
           -...15,
           -...17) 
  
  colnames(these_farebox) <- c("State",
                                      "Name",
                                      "ID",
                                      "Org Type",
                                      "Mode",
                                      "TOS",
                                      "VOMS",
                                      "Fare Revenues",
                                      "Total Operating Expenses",
                                      "trips",
                                      "avg_fare",
                                      "rec_rat")
  
  these_farebox <- these_farebox %>%
    group_by(ID) %>%
    summarise(State = first(State),
              Name = first(Name),
              VOMS = sum(VOMS),
              fare_rev = sum(`Fare Revenues`),
              op_exp = sum(`Total Operating Expenses`)) %>%
    mutate(fare_recovery = fare_rev / op_exp) %>%
    mutate(year = i)
  
  farebox <- rbind(farebox, these_farebox)
}

### agency info
agencies <- tibble(ID = c(""),
                   Agency_Type_Desc = c(""),
                   Institution_Type_Desc = c(""),
                   Company_Nm = c(""),
                   Service_Area = c(""),
                   year = 0)

for (i in 2005:2020) {
  sheet <- ifelse(i == 2010, 2, 1)
  
  these_agencies <- here("NTD_data",
                         paste0("y", i),
                         paste0(i, "_agency_info.xlsx")) %>%
    read_xlsx(sheet = sheet)
  
  if (i < 2010) {
    these_agencies <- these_agencies %>%
      select(Trs_Id,
             Agency_Type_Desc,
             Institution_Type_Desc,
             Company_Nm,
             Service_Area) %>%
      rename(ID = Trs_Id) %>%
      mutate(year = i)
  }
  
  else if (i == 2010 | i == 2011) { # why can't say i == 2010|2011 here?
    these_agencies <- these_agencies %>%
      select(Trs_Id,
             Agency_Type_Desc,
             Company_Nm,
             Service_Area) %>%
      rename(ID = Trs_Id) %>%
      mutate(Institution_Type_Desc = NA, year = i)
  }

  else if (i == 2012) {
    these_agencies <- these_agencies %>%
      select(Trs_Id,
             Agency_Type_Desc,
             Org_Type,
             Company_Nm,
             Service_Area) %>%
      rename(ID = Trs_Id,
             Institution_Type_Desc = Org_Type) %>%
      mutate(year = i)
  }
  
  else if (i == 2013) {
    these_agencies <- these_agencies %>%
      select(NTDID,
             'Agency Type',
             'Organization Type',
             Agency,
             'Service Area (SQ Mi)') %>%
      rename(ID = NTDID,
             Agency_Type_Desc = 'Agency Type',
             Institution_Type_Desc = 'Organization Type',
             Company_Nm = Agency,
             Service_Area = 'Service Area (SQ Mi)') %>%
      mutate(year = i)
  }
  
  else if (i == 2014) {
    these_agencies <- these_agencies %>%
      select("4 digit NTDID",
             'Organization Type',
             "Reporter Name",
             "Service Area Sq Mi") %>%
      rename(ID = "4 digit NTDID",
             Institution_Type_Desc = 'Organization Type',
             Company_Nm = "Reporter Name",
             Service_Area = "Service Area Sq Mi") %>%
      mutate(Agency_Type_Desc = NA, year = i)
  }
 
  else {
    these_agencies <- these_agencies %>%
      select("Legacy NTD ID",
             'Organization Type',
             "Agency Name",
             "Service Area Sq Miles") %>%
      rename(ID = "Legacy NTD ID",
             Institution_Type_Desc = 'Organization Type',
             Company_Nm = "Agency Name",
             Service_Area = "Service Area Sq Miles") %>%
      mutate(Agency_Type_Desc = NA, year = i)
  } 
  
  agencies <- rbind(agencies, these_agencies)
}

### Service
service <- tibble(ID = c(""),
                  trips = 0,
                  VRM = 0,
                  year = 0)

for (i in 2005:2020) {
  skip <- ifelse(i == 2013 | i == 2014, 1, 0)
  
  these_service <- here("NTD_data",
                        paste0("y", i),
                        paste0(i, "_Service.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    rename_all(tolower)
  
  if (i < 2012) {
    these_service <- these_service %>%
      filter(time_period_desc == "Annual Total") %>%
      filter((passenger_car_sched_rev_miles > 0) | (vehicle_sched_miles > 0)) %>%
      group_by(trs_id) %>%
      summarise(trips = sum(unlinked_passenger_trips),
                VRM = sum(vehicle_or_train_rev_miles)) %>%
      rename(ID = trs_id) %>%
      mutate(year = i)
  }
  
  else if (i == 2012) {
    these_service <- these_service %>%
      filter(time_period_desc == "Annual Total") %>%
      filter((pass_car_sched_rev_miles_num > 0) | (veh_miles_num > 0)) %>%
      group_by(trs_id) %>%
      summarise(trips = sum(unl_pass_trips_num),
                VRM = sum(veh_rev_miles_num)) %>%
      rename(ID = trs_id) %>%
      mutate(year = i)
  }
  
  else if (i == 2013) {
    these_service <- these_service %>%
      filter(`time period` == "Annual Total") %>%
      filter(`scheduled vehicle revenue miles` > 0) %>%
      group_by(ntdid) %>%
      summarise(trips = sum(`unlinked passenger trips`),
                VRM = sum(`vehicle revenue miles`)) %>%
      rename(ID = ntdid) %>%
      mutate(year = i)
  }
  
  else if (i == 2014) {
    these_service <- these_service %>%
      filter(`time period` == "Annual Total") %>%
      filter(`scheduled revenue miles` > 0) %>%
      group_by(`4 digit ntdid`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`total actual revenue miles...14`)) %>%
      rename(ID = "4 digit ntdid") %>%
      mutate(year = i)
  }
  
  else if (i == 2015) {
    these_service <- these_service %>%
      filter(`time period` == "Annual Total") %>%
      filter(`scheduled actual vehicles/ passenger car revenue miles` > 0) %>%
      group_by(`legacy ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicles/ passenger car  revenue miles`)) %>%
      rename(ID = "legacy ntd id") %>%
      mutate(year = i)
  }
  
  else if (i>2015 & i<2019) {
    these_service <- these_service %>%
      filter(`time period` == "Annual Total") %>%
      filter(`scheduled actual vehicle/passenger car revenue miles` > 0) %>%
      group_by(`legacy ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicles/passenger car revenue miles`)) %>%
      rename(ID = "legacy ntd id") %>%
      mutate(year = i)
  }
  
  else if (i == 2019) {
    these_service <- these_service %>%
      filter(`time period` == "Annual Total") %>%
      filter(`scheduled actual vehicle/passenger car revenue miles` > 0) %>%
      group_by(`ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicle/passenger car revenue miles`)) %>%
      rename(ID = "ntd id") %>%
      mutate(year = i)
  }  
  
  else {
    these_service <- these_service %>%
      filter(`time period` == "Annual Total") %>%
      filter(`scheduled actual vehicle/ passenger car revenue miles` > 0) %>%
      group_by(`ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicles/ passenger car revenue miles`)) %>%
      rename(ID = "ntd id") %>%
      mutate(year = i)
  }   
  
  service <- rbind(service, these_service)
  }

### Service ares (appendix D)
service_area <- tibble(UZA = c(""),
                       `Urbanized Area` = c(""),
                       `Population` = 0,
                       `Square Miles` = 0,
                       `Population Density` = 0,
                       `Transit Agency` = "",
                       ID = "",
                       year = 0)

for (i in 2005:2008) {
  skip <- ifelse(i < 2007, 4, 1)
  
  these_service_area <- here("NTD_data",
                             paste0("y", i),
                             paste0(i, "_Appendix_D.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    select(UZA, 
           `Urbanized Area`,
           Population,
           `Square Miles`,
           `Population Density`,
           `Transit Agency`,
           ID) %>%
    mutate(year = i)

  if (i < 2007) { 
    these_service_area <- these_service_area %>%
      fill(UZA,
           `Urbanized Area`,
           Population,
           `Square Miles`,
           `Population Density`)
  }
  
  service_area <- rbind(service_area, these_service_area)
}



###### End of the part Mengyao will update at once she's 
###### downloaded all the NTD files.
########################################################

NTD_data <- inner_join(service, agencies) %>%
  inner_join(farebox) %>%
  inner_join(salary) %>%
  inner_join(service_area) %>%
  filter(year > 0) %>%
  mutate(overhead = gen_admin_salary / op_exp) %>%
  select(ID, 
         Company_Nm, 
         State, 
         `Urbanized Area`,
         Population,
         `Population Density`,
         Agency_Type_Desc,
         Institution_Type_Desc,
         VOMS,
         VRM,
         fare_rev,
         op_exp,
         fare_recovery,
         gen_admin_salary,
         overhead,
         year) %>%
  group_by(`Urbanized Area`) %>%
  mutate(n_in_uza = n(),
         VRM_UZA_share = VRM / sum(VRM))

# Load census data
vars2000 <- c(
  total_homes = "H004001",
  rented_homes = "H004003"
)

vars2010 <- c(
  total_homes = "H004001",
  rented_homes = "H004004"
)

UZAs2000 <- get_decennial(geography = "urban area", 
                      variables = vars2000,
                      year = 2000,
                      output = "wide") %>%
  mutate(pct_rented = rented_homes / total_homes) %>%
  filter(str_detect(NAME, "Urbanized Area")) %>%
  mutate(`Urbanized Area` = str_replace(NAME, 
                                        pattern = " Urbanized Area",
                                        replacement = "")) %>%
  mutate(`Urbanized Area` = str_replace_all(`Urbanized Area`,
                                            pattern = "--",
                                            replacement = "-")) %>%
  select(GEOID, `Urbanized Area`, pct_rented)

all_data <- inner_join(NTD_data, UZAs)

## Load dates and census regions.
dates <- here("assembled-data", 
              "agencies-dates.csv") %>%
  read_csv() %>%
  # ignore blank lines
  filter(!is.na(Company_Nm)) %>%
  rename(gtfs_date = date,
         gtfs_status = status,
         ridership = trips) %>%
  select(-query)

regions <- here("assembled-data",
                "census-regions.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  rename(State = State_Desc)

final_data <- dates %>%
  inner_join(all_data) %>%
  inner_join(regions) %>%
  mutate(adopted_year = as.numeric(substr(gtfs_date, 8, 9)) + 2000) %>%
  mutate(adopted_yet = adopted_year <= year & 
                         gtfs_status == 1) %>%
  filter(gtfs_status != 2 | adopted_year >= year)