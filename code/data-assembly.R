### The purpose of this file is to assemble the data to be
# used in a regression analysis predicting the time it takes
# for a transit agency to adopt GTFS.

library(tidyverse)
library(readxl)
library(tidycensus)
library(here)
library(lubridate)

### Load NTD data
agencies <- here("NTD_data",
                 "2005_agency_info.xlsx") %>%
  read_xlsx(sheet = "Sheet1") %>%
  select(Trs_Id,
         Agency_Type_Desc,
         Institution_Type_Desc,
         Company_Nm,
         State_Desc,
         Service_Area) %>%
  rename(ID = Trs_Id)

service <- here("NTD_data",
                "2005_Service.xlsx") %>%
  read_xlsx(sheet = "Sheet1") %>%
  filter(Time_Period_Desc == "Annual Total") %>%
  filter((passenger_Car_Sched_Rev_Miles > 0) | (vehicle_Sched_Miles > 0)) %>%
  group_by(Trs_Id) %>%
  summarise(trips = sum(Unlinked_Passenger_Trips),
            VRM = sum(vehicle_Or_Train_Rev_Miles)) %>%
  rename(ID = Trs_Id)

service_area <- here("NTD_data",
                                "2005_Appendix_D.xlsx") %>%
  read_xlsx(sheet = "sst7A9.rpt",
            skip = 4) %>%
  select(UZA, 
         `Urbanized Area`,
         Population,
         `Square Miles`,
         `Population Density`,
         `Transit Agency`,
         ID) %>%
  fill(UZA, 
       `Urbanized Area`,
       Population,
       `Square Miles`,
       `Population Density`)

salary <- here("NTD_data",
                     "2005_Operating_Expenses.xlsx") %>%
  read_xlsx(sheet = "Sheet1") %>%
  select(Trs_Id, 
         expense_category_desc,
         op_sal_wage_amt,
         other_sal_wage_amt,
         fringe_benefit_amt)  %>%
  rename(ID = Trs_Id) %>%
  filter(expense_category_desc == "General Administration") %>%
  group_by(ID) %>%
  mutate(salary_plus_fringe = op_sal_wage_amt +
                              other_sal_wage_amt +
                              fringe_benefit_amt) %>%
  summarise(gen_admin_salary = sum(salary_plus_fringe))

farebox <- here("NTD_data",
                "2005_Table_26.xlsx") %>%
  read_xlsx(sheet = "Pass_Fare_Recovery_Ratio.rpt",
            skip = 3) %>%
  fill(State, Name) %>%
  filter(!is.na(ID)) %>%
  select(-...9,
         -...11,
         -...13,
         -...15,
         -...17) %>%
  group_by(ID) %>%
  summarise(State = first(State),
            Name = first(Name),
            VOMS = sum(VOMS),
            fare_rev = sum(`Fare Revenues`),
            op_exp = sum(`Total Operating Expenses`)) %>%
  mutate(fare_recovery = fare_rev / op_exp)

NTD_data <- inner_join(service, agencies) %>%
  inner_join(farebox) %>%
  inner_join(salary) %>%
  inner_join(service_area) %>%
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
         overhead) %>%
  group_by(`Urbanized Area`) %>%
  mutate(n_in_uza = n(),
         VRM_UZA_share = VRM / sum(VRM))

# Load census data
vars <- c(
  total_homes = "H004001",
  rented_homes = "H004003"
)

UZAs <- get_decennial(geography = "urban area", 
                      variables = vars,
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

site_data <- dates %>%
  inner_join(all_data) %>%
  inner_join(regions) %>%
  mutate(gtfs_date = dmy(gtfs_date)) %>%
  mutate(time_to_adopt_gtfs = as.numeric(gtfs_date - my("Dec 2005"))/365.25) %>%
  mutate(inst_type = substr(Institution_Type_Desc, 1,2)) %>%
  mutate(agency_type = substr(Agency_Type_Desc, 1, 2)) %>%
  mutate(inst_type = as_factor(inst_type)) %>%
  mutate(agency_type = as_factor(agency_type)) %>%
  mutate(inst_type = relevel(inst_type, ref = "2.")) %>%
  mutate(agency_type = relevel(agency_type, ref = "2."))

here("assembled-data",
     "agency-data.csv") %>%
  write_csv(x = site_data, file = .)
