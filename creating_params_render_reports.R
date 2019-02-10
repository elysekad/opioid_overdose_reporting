library(dbplyr)
library(odbc)
library(rstudioapi)
library(DBI)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
httr::set_config(config(ssl_verifypeer = 0L))

#ENTER PARAMETERS FOR REPORT
##################################################################################################################

username <- ''
pw <- ''
#file where you would like data and reports to be saved
wd <- '~/Q4_2018'
#file path where you saved the rmarkdown script that renders the reports. This file will be called to render the actual reports. 
markdown_file <- '~/county_od_report.Rmd'
#last day of most recent quarter (must use %d%b%Y format)
end_date <- '31Dec2018'
#first day of first quarter when you would like data to be pulled
start_date <- '01Jan2018'
#4 digit user_id
user_id <- ''

##################################################################################################################

#SETTING UP ODBC CONNECTION
con <- dbConnect(odbc::odbc(), 
                 dsn = "Biosense_Platform", 
                 UID = paste0("BIOSENSE\\", username), 
                 PWD = pw)

MFT_table <- 'WA_MFT'

#PULLING IN DATA FROM MFT
MFT <- tbl(con, MFT_table)

MFT <- MFT %>%
  select(Facility_Name, Facility_Status, County_FIPS_Code, Facility_County, Date_Activated, Facility_Type) %>%
  filter(Facility_Type=='Emergency Care') %>%
  collect()

#GETTING ACTIVE DATE THAT FACILITY HIT 75% FOR EACH COUNTY
prod_date <- MFT %>%
  select(Facility_Name, Date_Activated)

dir.create(file.path(wd))
dir.create(file.path(wd, "data"))
setwd(file.path(wd, "data"))
write.csv(prod_date, 'prod_date.csv')
rm(prod_date)

date_active <- MFT %>%
  arrange(Facility_County, Date_Activated) %>%
  group_by(Facility_County, County_FIPS_Code) %>%
  mutate(count = 1, cumsum = cumsum(count)/n()) %>%
  filter(cumsum>=0.75) %>%
  slice(1) %>%
  select(Facility_County, County_FIPS_Code, Date_Activated, cumsum) %>%
  filter(!is.na(Date_Activated))

#GETTING MONTH AND YEAR 75% OF FACILITIES BECAME ACTIVE
date_active$year <- as.numeric(format(date_active$Date_Activated, '%Y'))
date_active$month <- as.numeric(format(date_active$Date_Activated, '%m'))
date_active[date_active$month %in% c(1:3), 'start_date'] <- '1Apr' 
date_active[date_active$month %in% c(4:6), 'start_date'] <- '1Jul' 
date_active[date_active$month %in% c(7:9), 'start_date'] <- '1Oct'
date_active[date_active$month %in% c(10:12), 'start_date'] <- '1Jan' 

#ADJUSTING YEAR FOR FACILITIES THAT BECAME ACTIVE IN 4TH QUARTER
date_active[date_active$month %in% c(10:12), 'year'] <- date_active[date_active$month %in% c(10:12), 'year'] + 1 

#CREATING START DATES
date_active$start_date <- paste0(date_active$start_date, date_active$year) 
date_active$end_date <- end_date

#REMOVING FACILITIES THAT BECAME 75% ACTIVE IN THE MOST RECENT QUARTER OR LATER
date_active$include <- as.Date(date_active$start_date, "%d%b%Y")<as.Date(date_active$end_date, "%d%b%Y")
date_active <- date_active[date_active$include==TRUE, ]

#EDITING CUMSUM COLUMN
date_active$cumsum <- paste0(round((date_active$cumsum*100), 2), '%')

#creating wd and subdirectories
write.csv(date_active, 'active_counties.csv')

#Pulling data details from APIs for three different drug overdose queries: all drug, opioid, and heroin. This data will be used to create table 3 and figure 2 of the reports. This pulls all visits detected by the three queries who were seen through the ED and are 11 years old or greater. Fields that are pulled in to R include: C_BioSense_ID, QuarterYear, DischargeDisposition, Race_Flat, Sex, HospitalRegion, HasBeenI, C_Death, HospitalName, AgeGroup, Age, C_Patient_County, CCDDCateogry

URLs <- c(paste0('https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=', end_date, '&ccddCategory=cdc%20all%20drug%20v1&percentParam=noPercent&geographySystem=region&datasource=va_er&field=C_BioSense_ID&field=QuarterYear&field=DischargeDisposition&field=Race_flat&field=Sex&field=HospitalRegion&field=HasBeenI&field=C_Death&field=HospitalName&field=Facility&field=AgeGroup&field=Age&field=C_Patient_County&field=CCDDCategory&detector=nodetectordetector&startDate=', start_date, '&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&ageNCHS=unknown&timeResolution=quarterly&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=', user_id, '&hospFacilityType=emergency%20care&aqtTarget=DataDetails'),
          paste0('https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=', end_date, '&ccddCategory=cdc%20opioid%20overdose%20v2&percentParam=noPercent&geographySystem=region&datasource=va_er&field=C_BioSense_ID&field=QuarterYear&field=DischargeDisposition&field=Disposition.Category&field=Race_flat&field=Sex&field=HospitalRegion&field=HasBeenI&field=C_Death&field=HospitalName&field=AgeGroup&field=Age&field=C_Patient_County&field=CCDDCategory&detector=nodetectordetector&startDate=', start_date, '&ageNCHS=unknown&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&timeResolution=quarterly&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=', user_id, '&hospFacilityType=emergency%20care&aqtTarget=DataDetails'),
          paste0('https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=', end_date, '&ccddCategory=cdc%20heroin%20overdose%20v4&percentParam=noPercent&geographySystem=region&datasource=va_er&field=C_BioSense_ID&field=QuarterYear&field=DischargeDisposition&field=Disposition.Category&field=Race_flat&field=Sex&field=HospitalRegion&field=HasBeenI&field=C_Death&field=HospitalName&field=AgeGroup&field=Age&field=C_Patient_County&field=CCDDCategory&detector=nodetectordetector&startDate=', start_date, '&ageNCHS=unknown&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&timeResolution=quarterly&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=', user_id, '&hospFacilityType=emergency%20care&aqtTarget=DataDetails'))

df <- lapply(URLs, function(u){
  
  api_response <- GET(u, authenticate(username, pw))
  api_response_json <- content(api_response, as = "text")
  api_data <- fromJSON(api_response_json, simplifyDataFrame = TRUE)
  api_data$dataDetails
  
})

#COMBINING LIST ITEMS INTO ONE DF
df <- Reduce(rbind, df)

#CLEANING DATA DETAILS
#HasBeenI
df[df$HasBeenI=='0', 'HasBeenI'] = 'No'
df[df$HasBeenI=='1', 'HasBeenI'] = 'Yes'

#Sex
df[!(df$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
df[df$Sex=='F', 'Sex'] <- 'Female'
df[df$Sex=='M', 'Sex'] <- 'Male'

#Race_flat
df$race <- 'Unknown'
races <- c(';1002-5;', ';2028-9;', ';2054-5;', ';2076-8;', ';2131-1;', ';2106-3;', ';NR;')
df[!(df$Race_flat %in% races), 'race'] <- 'Multiple Races'

df[df$Race_flat==';1002-5;', 'race'] <- 'American Indian/Alaska Native'
df[df$Race_flat==';2028-9;', 'race'] <- 'Asian'
df[df$Race_flat==';2054-5;', 'race'] <- 'Black or African American'
df[df$Race_flat==';2076-8;', 'race'] <- 'Native Hawaiian or Pacific Islander'
df[df$Race_flat==';2131-1;', 'race'] <- 'Other Race'
df[df$Race_flat==';2106-3;', 'race'] <- 'White'

#Age
df[df$AgeGroup=='05-17', 'AgeGroup'] <- '11-17'
df[df$AgeGroup=='65-1000', 'AgeGroup'] <- '65+'
df[df$AgeGroup=='Unknown', 'AgeGroup'] <- 'Unknown'

#QuarterYear
df$QuarterYear <- gsub('-1', '-Q1', df$QuarterYear)
df$QuarterYear <- gsub('-2', '-Q2', df$QuarterYear)
df$QuarterYear <- gsub('-3', '-Q3', df$QuarterYear)
df$QuarterYear <- gsub('-4', '-Q4', df$QuarterYear)


#Check to see if there's a data file in the wd and creates one if there isn't
dir.create(file.path(wd, "data"))
setwd(file.path(wd, "data"))
saveRDS(df, file='data_details.Rda')
remove(df)

dir.create(file.path(wd, "reports"))

render_report <- function(county, start_date, end_date) {
  
  parameters <- list(county = county,
                     start_date = start_date,
                     end_date = end_date,
                     wd=file.path(wd, "data"),
                     username=username,
                     pw=pw, 
                     user_id=user_id)
  
  rmarkdown::render(markdown_file,
                    output_file = paste0(wd, '/reports/', end_date, '_', gsub(' ', '_', county), '.html'),
                    params = parameters,
                    encoding="UTF-8")
  invisible(TRUE)
}

params_list <- list(as.list(date_active$Facility_County),
                    as.list(date_active$start_date),
                    as.list(date_active$end_date))

pmap(params_list, render_report)

