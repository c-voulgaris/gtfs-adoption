This folder should contain the following files, downloaded from the [National Transit Database](https://www.transit.dot.gov/ntd/ntd-data).

# Agency info
  * Data category: "Agency Information"
  * Link name: "2005 Annual Database Agency Information"
  * File name: 2005_agency_info.xlsx
  * Variables: Agency ID, Agency NTDID, Agency Type, Organization Type, Institution Type, Company Name, Service Area
  
# Service
  * Data category: "Service"
  * Link name: "2005 Annual Database Service"
  * File name: 2005_Service.xlsx
  * Variables: Time Period, Passenger Car Scheduled Revenue Miles, Vehicle Scheduled Miles, Unlinked Passenger Trips, Vehicle Revenue Miles
  
# Service area
  * Link name (2005-2013): "2005 Appendix D: 2000 U.S. Urbanized Areas (UZAs), Populations, Square Miles and Densities Reported by Transit Agencies"
  * Link name (2014-2018): "2014 Annual Database Agency UZAs"
  * File name: 2005_Appendix_D.xlsx
  
  * Link name (2019-2020): The information you'd need from this file is in the "Agency Information" spreadsheet
  * Variables: Urbanized Area, Population, Area (Square miles), Population Density
  
# Salary
  * Data category: "Expenses"
  * Link name: "2005 Annual Database Operating Expense"
  * File name: 2005_Operating_Expenses.xlsx
  * File (2005, 2007, 2011) : You need to delete the 20th column in the excel. 
  * File (2013): You need to save the downloaded xls file as an xlsx file
  * Variables: Expense Category, Operator's Salaries and Wages, Other Salaries and Wages, Fringe Benefits
  
# Fare per passenger
  * Data Category: "Expenses"
  * Link name: "2005 Table 26: Fare per Passenger and Recovery Ratio"
  * File name: 2005_Table_26.xlsx
  * File (2014): You need to save the downloaded xls file as an xlsx file
  
  * Table_26 is missing from 2015 to 2020. The needed information can be found separately from "_agency_info.xlsx", "_Service.xlsx", "_Fare_Revenue.xlsx", "_Operating_Expenses.xlsx"
  * Data category (2015-2020): Fares/Funding
  * Link name (2015-2020): "2015 Annual Database Fare Revenues"
  * File name (2015-2020): 2015_Fare_Revenue.xlsx
  * Variables: Fare Revenue, Vehicles Operated in Annual Maximum Service, Operating Expenses

(the .gitignore file includes the .xlsx extension, so these files won't show up in the remote repo. 