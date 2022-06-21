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
  
  ### Service area (appendix D)
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
                   NTDID = c(""),
                   Agency_Type_Desc = c(""),
                   Org_Type = c(""),
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
      mutate(NTDID = NA, Org_Type = NA, year = i)
  }
  
  else if (i == 2010 | i == 2011) {
    these_agencies <- these_agencies %>%
      select(Trs_Id,
             Agency_Type_Desc,
             Company_Nm,
             Service_Area) %>%
      rename(ID = Trs_Id) %>%
      mutate(NTDID = NA, Org_Type = NA, Institution_Type_Desc = NA, year = i)
  } 
  
  
  else if (i == 2012) {
    these_agencies <- these_agencies %>%
      select(Trs_Id,
             Agency_Type_Desc,
             Org_Type,
             Company_Nm,
             Service_Area) %>%
      rename(ID = Trs_Id) %>%
      mutate(NTDID = NA, Institution_Type_Desc = NA, year = i)
  }
  
  else if (i == 2013) {
    these_agencies <- these_agencies %>%
      select(NTDID,
             "Agency Type",
             "Organization Type",
             Agency,
             "Service Area (SQ Mi)") %>%
      rename(ID = NTDID,
             Agency_Type_Desc = "Agency Type",
             Org_Type = "Organization Type",
             Company_Nm = Agency,
             Service_Area = 'Service Area (SQ Mi)') %>%
      mutate(NTDID = NA, Institution_Type_Desc = NA, year = i)
  }
  
  else if (i == 2014) {
    these_agencies <- these_agencies %>%
      select("4 digit NTDID",
             "5 digit NTDID",
             'Organization Type',
             "Reporter Name",
             "Service Area Sq Mi") %>%
      rename(ID = "4 digit NTDID",
             NTDID = "5 digit NTDID",
             Org_Type = 'Organization Type',
             Company_Nm = "Reporter Name",
             Service_Area = "Service Area Sq Mi") %>%
      mutate(Institution_Type_Desc = NA, Agency_Type_Desc = NA, year = i)
  } 
  
  else {
    colnames(these_agencies)[1] <- "NTDID"
    colnames(these_agencies)[2] <- "ID"
    colnames(these_agencies)[3] <- "Company_Nm"
    
    these_agencies <- these_agencies %>%
      select(NTDID,
             ID,
             'Organization Type',
             Company_Nm,
             "Service Area Sq Miles") %>%
      rename(Org_Type = 'Organization Type',
             Service_Area = "Service Area Sq Miles") %>%
      mutate(NTDID = as.character(NTDID), Institution_Type_Desc = NA, Agency_Type_Desc = NA, year = i)
  } 
  
  agencies <- rbind(agencies, these_agencies)
}

### Service
service <- tibble(ID = c(""),
                  NTDID = c(""),
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
  
  if (i > 2012) {
    these_service <- these_service %>%
      rename(time_period_desc = "time period")
  }
  
  these_service <- these_service %>%
    filter(time_period_desc == "Annual Total")
  
  if (i < 2012) {
    these_service <- these_service %>%
      filter((passenger_car_sched_rev_miles > 0) | (vehicle_sched_miles > 0)) %>%
      group_by(trs_id) %>%
      summarise(trips = sum(unlinked_passenger_trips),
                VRM = sum(vehicle_or_train_rev_miles)) %>%
      rename(ID = trs_id) %>%
      mutate(NTDID = NA, year = i)
  }
  
  else if (i == 2012) {
    these_service <- these_service %>%
      filter((pass_car_sched_rev_miles_num > 0) | (veh_miles_num > 0)) %>%
      group_by(trs_id) %>%
      summarise(trips = sum(unl_pass_trips_num),
                VRM = sum(veh_rev_miles_num)) %>%
      rename(ID = trs_id) %>%
      mutate(NTDID = NA, year = i)
  }
  
  else if (i == 2013) {
    these_service <- these_service %>%
      filter(`scheduled vehicle revenue miles` > 0) %>%
      group_by(ntdid) %>%
      summarise(trips = sum(`unlinked passenger trips`),
                VRM = sum(`vehicle revenue miles`)) %>%
      rename(ID = ntdid) %>%
      mutate(NTDID = NA, year = i)
  }
  
  else if (i == 2014) {
    these_service <- these_service %>%
      filter(`scheduled revenue miles` > 0) %>%
      group_by(`4 digit ntdid`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`total actual revenue miles...14`),
                NTDID = first(`5 digit ntdid`)) %>%
      rename(ID = "4 digit ntdid") %>%
      mutate(year = i)
  }
  
  else if (i == 2015) {
    these_service <- these_service %>%
      filter(`scheduled actual vehicles/ passenger car revenue miles` > 0) %>%
      group_by(`legacy ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicles/ passenger car  revenue miles`),
                NTDID = first(`5 digit ntd id`)) %>%
      rename(ID = "legacy ntd id") %>%
      mutate(year = i)
  }
  
  else if (i>2015 & i<2019) {
    colnames(these_service)[1] <- "NTDID"
    
    these_service <- these_service %>%
      filter(`scheduled actual vehicle/passenger car revenue miles` > 0) %>%
      group_by(`legacy ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicles/passenger car revenue miles`),
                NTDID = first(NTDID)) %>%
      rename(ID = "legacy ntd id") %>%
      mutate(year = i)
  }
  
  else {
    colnames(these_service)[16] <- "actual vehicle/passenger car revenue miles"
    colnames(these_service)[18] <- "scheduled actual vehicle/passenger car revenue miles"
    
    these_service <- these_service %>%
      filter(`scheduled actual vehicle/passenger car revenue miles` > 0) %>%
      group_by(`ntd id`) %>%
      summarise(trips = sum(`unlinked passenger trips (upt)`),
                VRM = sum(`actual vehicle/passenger car revenue miles`)) %>%
      rename(NTDID = "ntd id") %>%
      mutate(ID = NA, year = i)
  } 
  
  service <- rbind(service, these_service)
}

### Service area (appendix D)
service_area <- tibble(UZA = c(""),
                       `Urbanized Area` = c(""),
                       `Population` = 0,
                       `Square Miles` = 0,
                       `Population Density` = 0,
                       `Transit Agency` = "",
                       ID = "",
                       NTDID = c(""),
                       year = 0)

for (i in 2005:2018) {
  if (i < 2007) {a = 4}
  else if (i>2006 & i<2014) {a = 1}
  else {a = 0}
  
  these_service_area <- here("NTD_data",
                             paste0("y", i),
                             paste0(i, "_Appendix_D.xlsx")) %>%
    read_xlsx(sheet = 1, skip = a)
  
  if (i<2007) {
    these_service_area <- these_service_area %>%
      select(UZA, 
             `Urbanized Area`,
             Population,
             `Square Miles`,
             `Population Density`,
             `Transit Agency`,
             ID) %>%
      mutate(NTDID = NA, year = i) %>%
      fill(UZA,
           `Urbanized Area`,
           Population,
           `Square Miles`,
           `Population Density`)
  }
  
  else if (i>=2007 & i<2013) {
    these_service_area <- these_service_area %>%
      select(UZA, 
             `Urbanized Area`,
             Population,
             `Square Miles`,
             `Population Density`,
             `Transit Agency`,
             ID) %>%
      mutate(NTDID = NA, year = i)
  }
  
  else if (i == 2013) {
    these_service_area <- these_service_area %>%
      select(UZA, 
             `Urbanized Area`,
             Population,
             `Square Miles`,
             `Population Density`,
             `Transit Agency`,
             `NTD ID`) %>%
      rename(ID = "NTD ID") %>%
      mutate(NTDID = NA, year = i)
  }
  
  else if (i == 2014) {
    these_service_area <- these_service_area %>%
      select(UZA, 
             `UZA Name`,
             Population,
             `Square Miles`,
             Density,
             `Reporter Name`,
             `4 digit NTDID`,
             `5 digit NTDID`) %>%
      rename("Urbanized Area" = "UZA Name",
             "Population Density" = Density,
             "Transit Agency" = "Reporter Name",
             ID = "4 digit NTDID",
             NTDID = "5 digit NTDID") %>%
      mutate(year = i)
  }
  
  else {
    colnames(these_service_area)[1] <- "NTDID"
    
    these_service_area <- these_service_area %>%
      select(`UZA ID`, 
             `UZA Name`,
             Population,
             `Square Miles`,
             Density,
             `Agency Name`,
             `Legacy NTD ID`,
             NTDID) %>%
      rename(UZA = "UZA ID",
             "Urbanized Area" = "UZA Name",
             "Population Density" = Density,
             "Transit Agency" = "Agency Name",
             ID = "Legacy NTD ID") %>%
      mutate(year = i)
  }
  
  service_area <- rbind(service_area, these_service_area)
}

for (i in 2019:2020) {
  these_service_area <- here("NTD_data",
                             paste0("y", i),
                             paste0(i, "_agency_info.xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    select(`Primary UZA`,
           `UZA Name`,
           Population,
           `Sq Miles`,
           Density,
           `Agency Name`,
           `Legacy NTD ID`,
           `NTD ID`) %>%
    rename(UZA = "Primary UZA",
           "Urbanized Area" = "UZA Name",
           "Population Density" = Density,
           "Square Miles" = "Sq Miles",
           "Transit Agency" = "Agency Name",
           ID = "Legacy NTD ID",
           NTDID = "NTD ID") %>%
    mutate(NTDID = as.character(NTDID), year = i)
  
  service_area <- rbind(service_area, these_service_area)
}

### Salary (operating expenses)
salary <- tibble(ID = c(""),
                 NTDID = c(""),
                 gen_admin_salary = 0,
                 year = 0)

for (i in 2005:2020) {
  if (i < 2012) {a = 4
  b = 15}
  else if (i == 2012) {a = 6
  b = 17}
  else if (i == 2013) {a = 6
  b = 16}
  else if (i == 2014) {a = 7
  b = 17}
  else if (i > 2014 & i < 2018) {a = 8
  b = 17}
  else if (i == 2018) {a = 8
  b = 20}
  else {a = 7
  b = 19}
  
  these_salary <- here("NTD_data",
                       paste0("y", i),
                       paste0(i, "_Operating_Expenses.xlsx")) %>%
    read_xlsx(sheet = 1,
              col_types = c(rep("text", a), rep("numeric", b))) %>%
    rename_all(tolower) %>%
    mutate_all(~replace(., is.na(.), 0))

  if (i < 2013) {
    these_salary <- these_salary %>%
      select(trs_id, 
             expense_category_desc,
             op_sal_wage_amt,
             other_sal_wage_amt,
             fringe_benefit_amt)  %>%
      rename(ID = trs_id) %>%
      filter(expense_category_desc == "General Administration") %>%
      group_by(ID) %>%
      mutate(salary_plus_fringe = op_sal_wage_amt + other_sal_wage_amt + fringe_benefit_amt) %>%
      summarise(gen_admin_salary = sum(salary_plus_fringe)) %>%
      mutate(NTDID = NA, year = i)
  }
  else if (i == 2013) {
    these_salary <- these_salary %>%
      select(ntdid, 
             `operating expense category`,
             `operators salaries and wages`,
             `other salaries and wages`,
             `fringe benefits`)  %>%
      rename(ID = ntdid) %>%
      filter(`operating expense category` == "General Administration") %>%
      group_by(ID) %>%
      mutate(salary_plus_fringe = `operators salaries and wages` + `other salaries and wages` + `fringe benefits`) %>%
      summarise(gen_admin_salary = sum(salary_plus_fringe)) %>%
      mutate(NTDID = NA, year = i)
  }
  else if (i == 2014) {
    these_salary <- these_salary %>%
      select(`4 digit ntdid`, 
             `5 digit ntdid`,
             `op exp type`,
             `operators’ salaries and wages`,
             `other salaries and wages`,
             `fringe benefits`)  %>%
      rename(ID = "4 digit ntdid") %>%
      filter(`op exp type` == "General Administration") %>%
      group_by(ID) %>%
      mutate(salary_plus_fringe = `operators’ salaries and wages` + `other salaries and wages` + `fringe benefits`) %>%
      summarise(gen_admin_salary = sum(salary_plus_fringe),
                NTDID = first(`5 digit ntdid`)) %>%
      mutate(year = i)
  }
  else if (i > 2014 & i < 2018) {
    colnames(these_salary)[1] <- "NTDID"
    colnames(these_salary)[2] <- "ID"
    
    these_salary <- these_salary %>%
      select(ID, 
             NTDID,
             `operating expense type`,
             `operators' salaries and wages`,
             `other salaries and wages`,
             `fringe benefits`)  %>%
      filter(`operating expense type` == "General Administration") %>%
      group_by(ID) %>%
      mutate(salary_plus_fringe = `operators' salaries and wages` + `other salaries and wages` + `fringe benefits`) %>%
      summarise(gen_admin_salary = sum(salary_plus_fringe),
                NTDID = first(NTDID)) %>%
      mutate(year = i)
  }
  else if (i == 2018) {
    these_salary <- these_salary %>%
      select(`legacy ntd id`, 
             `ntd id`,
             `operating expense type`,
             `operators' salaries and wages`,
             `operators' paid absences`,
             `other salaries and wages`,
             `other paid absences`,
             `fringe benefits`)  %>%
      rename(ID = "legacy ntd id") %>%
      filter(`operating expense type` == "General Administration") %>%
      group_by(ID) %>%
      mutate(salary_plus_fringe = `operators' salaries and wages` + `operators' paid absences` + `other salaries and wages` + `other paid absences` + `fringe benefits`) %>%
      summarise(gen_admin_salary = sum(salary_plus_fringe),
                NTDID = first(`ntd id`)) %>%
      mutate(year = i)
  }
  else {
    these_salary <- these_salary %>%
      select(`ntd id`,
             `operating expense type`,
             `operators' salaries and wages`,
             `operators' paid absences`,
             `other salaries and wages`,
             `other paid absences`,
             `fringe benefits`)  %>%
      rename(NTDID = "ntd id") %>%
      filter(`operating expense type` == "General Administration") %>%
      group_by(NTDID) %>%
      mutate(salary_plus_fringe = `operators' salaries and wages` + `operators' paid absences` + `other salaries and wages` + `other paid absences` + `fringe benefits`) %>%
      summarise(gen_admin_salary = sum(salary_plus_fringe)) %>%
      mutate(ID = NA, year = i)
  }
  
  salary <- rbind(salary, these_salary)
}

### farebox (Table 26)
farebox <- tibble(ID = c(""),
                  NTDID = c(""),
                  State = c(""),
                  Name = c(""),
                  VOMS = 0,
                  fare_rev = 0,
                  op_exp = 0,
                  fare_recovery = 0,
                  year = 0)

for (i in 2005:2014) {
  skip <- ifelse(i < 2007, 3, 1)
  
  these_farebox <- here("NTD_data",
                        paste0("y", i),
                        paste0(i, "_Table_26.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    fill(State, Name)
  
  if (i > 2005) {
    these_farebox <- these_farebox %>%
      rename("Fare Revenues" = "Fare Revenues Earned")
  }
  
  if (i < 2014) {
    these_farebox <- these_farebox %>%
      filter(!is.na(ID)) %>%
      group_by(ID) %>%
      select(State, 
             Name,
             VOMS,
             `Fare Revenues`,
             `Total Operating Expenses`) %>%
      summarise(State = first(State),
                Name = first(Name),
                VOMS = sum(VOMS),
                fare_rev = sum(`Fare Revenues`),
                op_exp = sum(`Total Operating Expenses`)) %>%
      mutate(fare_recovery = fare_rev / op_exp) %>%
      mutate(NTDID = NA, year = i) 
  }
  else {
    these_farebox <- these_farebox %>%
      rename(ID = "Legacy NTDID") %>%
      filter(!is.na(ID)) %>%
      group_by(ID) %>%
      select(NTDID,
             State, 
             Name,
             VOMS,
             `Fare Revenues`,
             `Total Operating Expenses`) %>%
      summarise(NTDID = first(NTDID),
                State = first(State),
                Name = first(Name),
                VOMS = sum(VOMS),
                fare_rev = sum(`Fare Revenues`),
                op_exp = sum(`Total Operating Expenses`)) %>%
      mutate(fare_recovery = fare_rev / op_exp) %>%
      mutate(year = i) 
  }
  
  farebox <- rbind(farebox, these_farebox)
}

for (i in 2015:2020) {
  if (i < 2018) {a = 8
  b = 17} 
  else if (i == 2018) {a = 8
  b = 20}
  else {a = 7
  b = 19}
  
  df_1 <- here("NTD_data",
               paste0("y", i),
               paste0(i, "_agency_info.xlsx")) %>%
    read_xlsx(sheet = 1)
  
  colnames(df_1)[1] <- "NTDID"
  
  df_1 <- df_1 %>%
    select(`Legacy NTD ID`,
           NTDID,
           State,
           `Agency Name`)

  df_2 <- here("NTD_data",
               paste0("y", i),
               paste0(i, "_Service.xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    filter(`Time Period` == "Annual Total")
  colnames(df_2)[1] <- "NTDID"
  
  if (i > 2015 & i < 2020) {
    df_2 <- df_2 %>%
      rename("Vehicles/ Passenger Cars Operated in Maximum Service" = "Vehicles/Passenger Cars Operated in Maximum Service")
  }
  
  df_2 <- df_2 %>%
    select(NTDID,
           `Vehicles/ Passenger Cars Operated in Maximum Service`) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(NTDID) %>%
    summarize(VOMS = sum(`Vehicles/ Passenger Cars Operated in Maximum Service`))

  df_3 <- here("NTD_data",
               paste0("y", i),
               paste0(i, "_Fare_Revenue.xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    mutate_all(~replace(., is.na(.), 0))
  colnames(df_3)[1] <- "NTDID"
  
  if (i > 2017) {
    df_3 <- df_3 %>%
      rename(Fares = "Total Fares")
  }
  
  df_3 <- df_3 %>%
    select(NTDID,
           `Fares`) %>%
    group_by(NTDID) %>%
    summarize(fare_rev = sum(`Fares`))

  df_4 <- here("NTD_data",
               paste0("y", i),
               paste0(i, "_Operating_Expenses.xlsx")) %>%
    read_xlsx(sheet = 1,
              col_types = c(rep("text", a), rep("numeric", b))) %>%
    filter(`Operating Expense Type` == "Total")
  
  colnames(df_4)[1] <- "NTDID"
  
  df_4 <- df_4 %>%
    select(NTDID,
           `Total Operating Expenses`) %>%
    group_by(NTDID) %>%
    summarize(op_exp = sum(`Total Operating Expenses`))

  df_list <- list(df_1, df_2, df_3, df_4)
  these_farebox <- Reduce(function(x, y) merge(x, y, all = TRUE), df_list) %>%
    rename(ID = "Legacy NTD ID",
           Name = "Agency Name") %>%
    mutate(fare_recovery = fare_rev / op_exp,
           year = i)

  farebox <- rbind(farebox, these_farebox)
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