### The purpose of this file is to assemble the data to be
# used in a regression analysis predicting the time it takes
# for a transit agency to adopt GTFS.

library(tidyverse)
library(readxl)
library(tidycensus)
library(here)
library(lubridate)

### Load NTD data

###### Mengyao to updated everything in this loop so it works for all years
# after downloading the spreadsheets for all years

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
  #### For year 2010, the original spreadsheet has the content we want on sheet 2. For other years, the content we want locates on sheet 1. 
  sheet <- ifelse(i == 2010, 2, 1)
  
  these_agencies <- here("NTD_data",
                         paste0("y", i),
                         paste0(i, "_agency_info.xlsx")) %>%
    read_xlsx(sheet = sheet)
  
  #### The agency's ID information has been stored in different columns for different years. Since 2014, a new NTDID has been applied. ID of the original format has been stored in the "Legacy NTD ID" Column and has been abandoned since 2019. 
  
  #### For year 2005 to 2009, the agency's ID information is stored in the "Trs_id" column. The agency type information is stored in the "Agency_Type_Desc" column. The institution type information is stored in the "Institution_Type_Desc" column. The company name information is stored in the "Company_Nm" column. The service area data is stored in the "Service_Area" column. NTDID and organization type data are missing from the spreadsheet.
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
  
  #### For year 2010 and 2011, the NTDID, Organization Type, and Institution type data are missing from the spreadsheet.
  else if (i == 2010 | i == 2011) {
    these_agencies <- these_agencies %>%
      select(Trs_Id,
             Agency_Type_Desc,
             Company_Nm,
             Service_Area) %>%
      rename(ID = Trs_Id) %>%
      mutate(NTDID = NA, Org_Type = NA, Institution_Type_Desc = NA, year = i)
  } 
  
  #### For year 2012, the NTDID and Institution type data are missing from the spreadsheet.
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
 
  #### For year 2013, the agency's ID information is stored in the "NTDID" column. The agency type information is stored in the "Agency Type" column. The organization type information is stored in the "Organization Type" column. The company name information is stored in the "Agency" column. The service area data is stored in the "Service Area (SQ Mi)" column. The NTDID and Institution type data are missing from the spreadsheet. 
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
  
  #### For year 2014, the agency's ID information is stored in the "4 digit NTDID" column. The NTDID information is stored in the "5 digit NTDID" column. The organization type information is stored in the "Organization Type" column. The company name information is stored in the "Reporter Name" column. The service area data is stored in the "Service Area Sq Mi" column. The institution type and agency type data are missing from the spreadsheet.
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
  
  #### For year 2015 to 2020, the agency's ID information is stored in the 2nd column. The NTDID information is stored in the 1st column and is read in as numeric data. The organization type information is stored in the "Organization Type" column. The company name information is stored in the 3rd column. The service area data is stored in the "Service Area Sq Miles" column. The institution type and agency type data are missing from the spreadsheet.
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
  #### For year 2013 and 2014, the 1st row of the spreadsheet contains unwanted information that needs to be skipped. For other years, the 1st row contains column names that we want. 
  skip <- ifelse(i == 2013 | i == 2014, 1, 0)
  
  #### As the column names for each year are a mixture of uppercase and lowercase, you may change all to lowercase for convenience.
  these_service <- here("NTD_data",
                        paste0("y", i),
                        paste0(i, "_Service.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    rename_all(tolower)
  
  #### For year 2005 to 2012, the time period information is stored in the "time_period_desc" column. For year 2013 to 2020, the time period information is stored in the "time period" column. You may rename the column name in the beginning to simplify codes. 
  if (i > 2012) {
    these_service <- these_service %>%
      rename(time_period_desc = "time period")
  }
  #### Only rows showing the annual total service are what we need.
  these_service <- these_service %>%
    filter(time_period_desc == "Annual Total")
  
  #### For year 2005 to 2012, the agency's ID information is stored in the "trs_id" column. The passenger car scheduled revenue miles information is stored in the "passenger_car_sched_rev_miles" column. The vehicle scheduled miles information is stored in the "vehicle_sched_miles" column. The unlinked passenger trips information is stored in the "unlinked_passenger_trips" column. The vehicle revenue miles information is stored in the "vehicle_or_train_rev_miles" column. Variable named "trips" is a summary of unlinked passenger trips by agency's ID. Actual Vehicle Revenue Miles (VRM) is a summary of vehicle revenue miles by agency's ID. The NTDID data is missing from the spreadsheet.
  if (i < 2012) {
    these_service <- these_service %>%
      filter((passenger_car_sched_rev_miles > 0) | (vehicle_sched_miles > 0)) %>%
      group_by(trs_id) %>%
      summarise(trips = sum(unlinked_passenger_trips),
                VRM = sum(vehicle_or_train_rev_miles)) %>%
      rename(ID = trs_id) %>%
      mutate(NTDID = NA, year = i)
  }
  
  #### For year 2012, the passenger car scheduled revenue miles information is stored in the "pass_car_sched_rev_miles_num" column. The vehicle scheduled miles information is stored in the "veh_miles_num" column. The unlinked passenger trips information is stored in the "unl_pass_trips_num" column. The vehicle revenue miles information is stored in the "veh_rev_miles_num" column.
  else if (i == 2012) {
    these_service <- these_service %>%
      filter((pass_car_sched_rev_miles_num > 0) | (veh_miles_num > 0)) %>%
      group_by(trs_id) %>%
      summarise(trips = sum(unl_pass_trips_num),
                VRM = sum(veh_rev_miles_num)) %>%
      rename(ID = trs_id) %>%
      mutate(NTDID = NA, year = i)
  }

  #### For year 2013, the agency's ID information is stored in the "ntdid" column. The passenger car scheduled revenue miles and vehicle scheduled miles information are combined in the "scheduled vehicle revenue miles" column.  The unlinked passenger trips information is stored in the "unlinked passenger trips" column. The vehicle revenue miles information is stored in the "vehicle revenue miles" column.  
  else if (i == 2013) {
    these_service <- these_service %>%
      filter(`scheduled vehicle revenue miles` > 0) %>%
      group_by(ntdid) %>%
      summarise(trips = sum(`unlinked passenger trips`),
                VRM = sum(`vehicle revenue miles`)) %>%
      rename(ID = ntdid) %>%
      mutate(NTDID = NA, year = i)
  }
  
  #### For year 2014, the agency's ID information is stored in the "4 digit ntdid" column. The NTDID information is stored in the "5 digit ntdid" column. The passenger car scheduled revenue miles and vehicle scheduled miles information are combined in the "scheduled revenue miles" column. The unlinked passenger trips information is stored in the "unlinked passenger trips (upt)" column. The vehicle revenue miles information is stored in the "total actual revenue miles...14" column.
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
  
  #### For year 2015, the agency's ID information is stored in the "legacy ntd id" column. The NTDID information is stored in the "5 digit ntd id" column. The passenger car scheduled revenue miles and vehicle scheduled miles information are combined in the "scheduled actual vehicles/ passenger car revenue miles" column. The unlinked passenger trips information is stored in the "unlinked passenger trips (upt)" column. The vehicle revenue miles information is stored in the "actual vehicles/ passenger car  revenue miles" column.
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
  
  #### For year 2016 to 2018, the agency's ID information is stored in the "legacy ntd id" column. The NTDID information is stored in the 1st column. The passenger car scheduled revenue miles and vehicle scheduled miles information are combined in the "scheduled actual vehicle/passenger car revenue miles" column. The unlinked passenger trips information is stored in the "unlinked passenger trips (upt)" column. The vehicle revenue miles information is stored in the "actual vehicles/passenger car revenue miles" column.
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
  
  #### For year 2019 and 2020, the NTDID information is stored in the "ntd id" column. The passenger car scheduled revenue miles and vehicle scheduled miles information are combined in the "scheduled actual vehicle/passenger car revenue miles" column. The unlinked passenger trips information is stored in the "unlinked passenger trips (upt)" column. The vehicle revenue miles information is stored in the "actual vehicle/passenger car revenue miles" column. The agency's ID information is missing from the spreadsheet.
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
  #### For year 2005 and 2006, the first 4 rows of the spreadsheet contain unwanted information that needs to be skipped. For year 2007 to 2013, the 1st row needs to be skipped. For year 2014 to 2020, the 1st row contains column names that we want. 
  if (i < 2007) {a = 4}
  else if (i>2006 & i<2014) {a = 1}
  else {a = 0}
  
  these_service_area <- here("NTD_data",
                             paste0("y", i),
                             paste0(i, "_Appendix_D.xlsx")) %>%
    read_xlsx(sheet = 1, skip = a)
  
  #### For year 2005 to 2012, the agency's ID information is stored in the "ID" column. The UZA number information is stored in the "UZA" column. The Urbanized Area name information is stored in the "Urbanized Area" column. The population information is stored in the "Population" column. The size of urbanized area information is stored in the "Square Miles" column. The population density information is stored in the "Population Density" column. The agency's name information is stored in the "Transit Agency" column. The NTDID data is missing from the spreadsheet. Since there are blank rows in the original spreadsheet, you should use the fill() function to fill them with the same information. 
  
  #### For year 2013, the agency's ID information is stored in the "NTD ID" column. Except for this, other variables are of the same name. Therefore, you may rename the column name of the 2013 spreadsheet first to simply codes.
  
  if (i == 2013) {
    these_service_area <- these_service_area %>%
      rename(ID = "NTD ID")
  }
  
  if (i<2014) {
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
  
  #### For year 2014, the agency's ID information is stored in the "4 digit NTDID" column. The NTDID information is stored in the "5 digit NTDID" column. The Urbanized Area name information is stored in the "UZA Name" column. The population density information is stored in the "Density" column. The agency's name information is stored in the "Reporter Name" column. 
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
  
  #### For year 2015 to 2018, the agency's ID information is stored in the "Legacy NTD ID" column. The NTDID information is stored in the 1st column. The UZA number information is stored in the "UZA ID" column. The Urbanized Area name information is stored in the "UZA Name" column. The population density information is stored in the "Density" column. The agency's name information is stored in the "Agency Name" column. 
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

#### For year 2015 to 2018, UZA related information is stored in the "_agency_info" spreadsheet. The agency's ID information is stored in the "Legacy NTD ID" column. The NTDID information is stored in the "NTD ID" column and is read in as numeric data. The UZA number information is stored in the "Primary UZA" column. The Urbanized Area name information is stored in the "UZA Name" column. The Urbanized Area name information is stored in the "Sq Miles" column. The population density information is stored in the "Density" column. The agency's name information is stored in the "Agency Name" column. 
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

#### R has problem with identifying the data type of several essential columns and tends to fill them with a random date. To solve this, you may specify the data type in the beginning. As different spreadsheets have different number of columns, you may need the if else statement to clarify it for each year. 
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
 
  #### As the column names for each year are a mixture of uppercase and lowercase, you may change all to lowercase for convenience.
  #### The original spreadsheet's empty space is read in as NA, which influences the summary calculation. You may change NA value to O. 
  these_salary <- here("NTD_data",
                       paste0("y", i),
                       paste0(i, "_Operating_Expenses.xlsx")) %>%
    read_xlsx(sheet = 1,
              col_types = c(rep("text", a), rep("numeric", b))) %>%
    rename_all(tolower) %>%
    mutate_all(~replace(., is.na(.), 0))

  #### For year 2005 to 2012, the agency's ID information is stored in the "trs_id" column. The expense category information is stored in the "expense_category_desc" column. The operator's salary information is stored in the "op_sal_wage_amt" column. The other salary information is stored in the "other_sal_wage_amt" column. The fringe benefit information is stored in the "fringe_benefit_amt" column. Only rows showing the general administration's expense are what we need. The total salary of general administration is a sum of salary plus fringe by agency's ID. NTDID data is missing from the spreadsheet.  
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
  
  #### For year 2013, the agency's ID information is stored in the "ntdid" column. The expense category information is stored in the "operating expense category" column. The operator's salary information is stored in the "operators salaries and wage" column. The other salary information is stored in the "other salaries and wages" column. The fringe benefit information is stored in the "fringe benefits" column. 
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
  
  #### For year 2014, the agency's ID information is stored in the "4 digit ntdid" column. The NTDID information is stored in the "5 digit ntdid" column. The expense category information is stored in the "op exp type" column. The operator's salary information is stored in the "operators’ salaries and wages" column. The other salary information is stored in the "other salaries and wages" column. The fringe benefit information is stored in the "fringe benefits" column. 
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
  
  #### For year 2015 to 2017, the agency's ID information is stored in the 2nd column. The NTDID information is stored in the 1st column. The expense category information is stored in the "operating expense type" column. The operator's salary information is stored in the "operators' salaries and wages" column. The other salary information is stored in the "other salaries and wages" column. The fringe benefit information is stored in the "fringe benefits" column. 
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
  
  #### For year 2018, the agency's ID information is stored in the "legacy ntd id" column. The NTDID information is stored in the "ntd id" column. The expense category information is stored in the "operating expense type" column. The operator's salary information is stored in the "operators' salaries and wages" column. The other salary information is stored in three separate columns: "operators' paid absences", "other salaries and wages", and "other paid absences". The fringe benefit information is stored in the "fringe benefits" column. 
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
  
  #### For year 2019 and 2020, the NTDID information is stored in the "ntd id" column. The expense category information is stored in the "operating expense type" column. The operator's salary information is stored in the "operators' salaries and wages" column. The other salary information is stored in three separate columns: "operators' paid absences", "other salaries and wages", and "other paid absences". The fringe benefit information is stored in the "fringe benefits" column. The total salary of general administration is a sum of salary plus fringe by agency's NTDID. The agency's ID information is missing from the spreadsheet. 
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
  #### For year 2005 and 2006, the first 3 rows of the spreadsheet contain unwanted information that needs to be skipped. For other years, the 1st row needs to be skipped.
  skip <- ifelse(i < 2007, 3, 1)
  
  #### Since there are blank rows in the "State" and "Name" column, you should use the fill() function to fill them with the same information. 
  these_farebox <- here("NTD_data",
                        paste0("y", i),
                        paste0(i, "_Table_26.xlsx")) %>%
    read_xlsx(sheet = 1,
              skip = skip) %>%
    fill(State, Name)
  
  #### For year 2005, the fare revenue information is stored in the "Fare Revenues" column. For other years, it is stored in the "Fare Revenues Earned" column. You may rename the column in the beginning to simplify codes. 
  if (i > 2005) {
    these_farebox <- these_farebox %>%
      rename("Fare Revenues" = "Fare Revenues Earned")
  }
  
  #### For year 2005 to 2013, the agency's ID information is stored in the "ID" column. The state information is stored in the "State" column. The agency's name information is stored in the "Name" column. The vehicles operated in annual maximum service (VOM) information is stored in the "VOMS" column. The total operating expenses information is stored in the "Total Operating Expenses" column. All selected columns need to go through summary calculation by agency's ID. The fare recovery rate is fare revenue divided by operating expenses. The NTDID data is missing from the spreadsheet.
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
  
  #### For year 2014, the agency's ID information is stored in the "Legacy NTDID" column. The NTDID information is stored in the "NTDID" column.
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

#### For year 2015 and 2020, there is no spreadsheet as "_Table_26" which holds all information we need. Data needs to be found from different spreadsheets and combined together. 
for (i in 2015:2020) {
  #### R has problem with identifying the data type of several essential columns in the "_Operating_Expenses" spreadsheets and tends to fill them with a random date. To solve this, you may specify the data type in the beginning. As different spreadsheets have different number of columns, you may need the if else statement to clarify it for each year. 
  if (i < 2018) {a = 8
  b = 17} 
  else if (i == 2018) {a = 8
  b = 20}
  else {a = 7
  b = 19}
  
  #### From the "_agency_info" spreadsheets, you can get information about agency's ID, agency's NTDID, state, and agency's name.  
  #### For year 2015 to 2020, the agency's ID information is stored in the "Legacy NTD ID" column. The NTDID information is stored in the first column. The state information is stored in the "State" column. The agency's name information is stored in the "Agency Name" column. 
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

  #### From the "_Service" spreadsheets, you can get information about agency's NTDID and vehicles operated in annual maximum service (VOM). 
  #### Only rows showing the annual total service are what we need. For year 2015 to 2020, the NTDID information is stored in the 1st column. For year 2015 and 2020, the VOM information is stored in the "Vehicles/ Passenger Cars Operated in Maximum Service" column. For year 2016 to 2018, the VOM information is stored in the "Vehicles/Passenger Cars Operated in Maximum Service" column.
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

  #### From the "_Fare_Revenue" spreadsheets, you can get information about agency's NTDID and fare revenue. 
  #### The original spreadsheet's empty space is read in as NA, which influences the summary calculation. You may change NA value to O. For year 2015 to 2020, the NTDID information is stored in the 1st column. For year 2015 to 2017, the fare revenue information is stored in the "Fares" column. For year 2018 to 2020, the fare revenue information is stored in the "Total Fares" column. The total fare revenue is a summary of fares by agency's NTDID. 
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

  #### From the "_Operating_Expenses" spreadsheets, you can get information about agency's NTDID and operating expenses. 
  #### Only rows showing the total operating expenses are what we need. For year 2015 to 2020, the NTDID information is stored in the 1st column. The total operating expenses information is stored in the "Total Operating Expenses" column. The operating expenses we need is a summary of total operating expenses by agency's NTDID. 
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

  #### You may join all dataframes and rename columns to match previous spreadsheets. 
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
agencies_2005 <- agencies %>%
  filter(year < 2014)
agencies_2014 <- agencies %>%
  filter(year >= 2014)

service_2005 <- service %>%
  filter(year < 2014)
service_2014 <- service %>%
  filter(year >= 2014) %>%
  select(-ID)

farebox_2005 <- farebox %>%
  filter(year < 2014)
farebox_2014 <- farebox %>%
  filter(year >= 2014) %>%
  select(-ID, -Name)

salary_2005 <- salary %>%
  filter(year < 2014)
salary_2014 <- salary %>%
  filter(year >= 2014) %>%
  select(-ID)

service_area_2005 <- service_area %>%
  filter(year < 2014) %>%
  select(-"Transit Agency")
service_area_2014 <- service_area %>%
  filter(year >= 2014 & UZA != 0) %>%
  select(-ID, -"Transit Agency")

NTD_2005 <- full_join(agencies_2005, service_2005) %>%
  full_join(farebox_2005) %>%
  select(-Name) %>%
  full_join(salary_2005) %>%
  full_join(service_area_2005)

NTD_2014 <- full_join(agencies_2014, service_2014) %>%
  full_join(farebox_2014) %>%
  full_join(salary_2014) %>%
  full_join(service_area_2014)

NTD <- rbind(NTD_2005, NTD_2014)

NTD_data <- NTD %>%
  #### Since there is an empty row at the beginning of the spreadsheet, you may get rid of it by filter(). 
  filter(year > 0) %>%
  mutate(overhead = gen_admin_salary / op_exp) %>%
  select(ID,
         NTDID,
         Company_Nm,
         State,
         `Urbanized Area`,
         Population,
         `Population Density`,
         Agency_Type_Desc,
         Org_Type,
         Institution_Type_Desc,
         VOMS,
         VRM,
         fare_rev,
         op_exp,
         fare_recovery,
         gen_admin_salary,
         overhead,
         year) %>%
  #### You may group by 2 variable to get the number of agencies in one urbanized area in one year (First, by year. and then, by urbanized area). The "VRM_UZA_share" is the percentage that the agency's VRM holds in the whole urbanized area's VRM.
  group_by(year, `Urbanized Area`) %>%  
  mutate(n_in_uza = n(),
         VRM_UZA_share = VRM / sum(VRM)) 

# Load census data
#### The percentage of renter-occupied households data can be found in 2000 and 2010 decennial census. The 2020 decennial census has not made similar data public. You may use load_variables() to check the variable codes for "total households" and "renter-occupied households". 
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
  #### Only rows about urbanized areas are what we need.
  filter(str_detect(NAME, "Urbanized Area")) %>%
  #### After comparison, it can be noticed that the naming strategies of the decennial census data and the NTD data are different. For instance, "Beloit, WI--IL Urbanized Area" in the census data is "Beloit, WI-IL" in NTD data. In order to join by "Urbanized Area", you need to get rid of the "Urbanized Area" text in the "NAME" column of the census data and change "--" into "-". 
  mutate(`Urbanized Area` = str_replace(NAME, 
                                        pattern = " Urbanized Area",
                                        replacement = "")) %>%
  mutate(`Urbanized Area` = str_replace_all(`Urbanized Area`,
                                            pattern = "--",
                                            replacement = "-")) %>%
  select(GEOID, `Urbanized Area`, pct_rented)

UZAs2010 <- get_decennial(geography = "urban area",
                          variables = vars2010,
                          year = 2010,
                          output = "wide") %>%
  mutate(pct_rented = rented_homes / total_homes) %>%
  filter(str_detect(NAME, "Urbanized Area")) %>%
  #### Similar as above, "Beloit, WI--IL Urbanized Area (2010)" in the census data is "Beloit, WI-IL" in NTD data. In order to join by "Urbanized Area", you need to get rid of the "Urbanized Area (2010)" text in the "NAME" column of the census data and change "--" into "-". 
  mutate(`Urbanized Area` = str_replace(NAME, 
                                        pattern = " Urbanized Area [(]2010[)]",
                                        replacement = "")) %>%
  mutate(`Urbanized Area` = str_replace_all(`Urbanized Area`,
                                            pattern = "--",
                                            replacement = "-")) %>%
  select(GEOID, `Urbanized Area`, pct_rented)

#### For year 2005 to 2009, you may join 2000 decennial census data to the NTD data. For year 2010 to 2020, you may join 2010 decennial census data to the NTD data.    
NTD_data_2000 <- NTD_data %>%
  filter(year < 2010) %>%
  left_join(UZAs2000)

NTD_data_2010 <- NTD_data %>%
  filter(year >=2010) %>%
  left_join(UZAs2010)

all_data <- rbind(NTD_data_2000, NTD_data_2010)  

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
  read_csv() %>%
  rename(State = State_Desc)

ID_ref <- agencies %>%
  filter(year == 2005) %>%
  select(ID, Company_Nm)

dates <- dates %>%
  left_join(ID_ref)

ID_ref1 <- agencies %>%
  filter(year == 2014) %>%
  select(ID, NTDID)

dates <- dates %>%
  left_join(ID_ref1)

dates_2005 <- dates %>%
  select(-Company_Nm, -NTDID)
dates_2014 <- dates %>%
  select(-Company_Nm, -ID)

all_data_2005 <- all_data %>%
  filter(year < 2014) %>%
  left_join(dates_2005)
all_data_2014 <- all_data %>%
  filter(year >= 2014) %>%
  left_join(dates_2014)

final_data <- rbind(all_data_2005, all_data_2014) %>%
  mutate(adopted_year = as.numeric(str_sub(gtfs_date, -2, -1)) + 2000) %>%
  mutate(adopted_yet = adopted_year <= year & 
           gtfs_status == 1) %>%
  #### After the agency has adopted the GTFS standard, we don't want it to appear in the dataframe again. 
  filter(adopted_year >= year)

## Add adoption rates
agency_data <- here("assembled-data",
                    "agency-data.csv") %>%
  read_csv() %>%
  filter(!is.na(status))
  
adoption_rates <- tibble(Date = seq(ymd("2005-12-1"), 
                                    ymd("2022-12-1"), 
                                    by = "years"),
                         num_agencies = 0,
                         num_adopted = 0) 

for (i in 1:length(adoption_rates$Date)) {
  adoption_rates$num_agencies[i] = 
    sum(agency_data$status < 2) +
    sum(agency_data$status == 2 & agency_data$date > adoption_rates$Date[i])
  
  adoption_rates$num_adopted[i] = 
    sum(agency_data$gtfs_status == 1 & agency_data$date < adoption_rates$Date[i])
}

adoption_rates <- adoption_rates %>%
  mutate(`Percent adoption of GTFS data standard` = 
           num_adopted / num_agencies) %>%
  mutate(year = as.numeric(substr(Date, 1, 4)))

final_data <- final_data %>%
  inner_join(adoption_rates)

## Export csv file
### CTV note: updated so it will work regardless of whose computer is running it
write.csv(final_data, 
          file = here("assembled-data", 
                      "final-data.csv"))