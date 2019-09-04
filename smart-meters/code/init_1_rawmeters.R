# This script loads the raw data and generates multiple (raw) Rdata files 
# (metadata, geographical information, smart meter observations, etc)

rm(list = ls())
source("config_paths.R")

library(data.table)
library(dplyr)
library(lubridate)
library(gdata)
library(parallel)


# Create Rdata files and save them in S3
DT <- fread(file.path(rawdata.folder, "edrp_elec.csv"))
DT <- DT %>% rename(IDMETER = ANON_ID) 

######### GEO DATA and META DATA #########
metaDT <- tbl_df(read.xls(file.path(rawdata.folder, "edrp_metadata.xlsx"), nrows = 14319, skip = 1)) %>%
  mutate_each(funs(ymd_hm), firstAdvance) %>% 
  mutate_each(funs(ymd_hm), lastAdvance) %>%
  rename(IDMETER = Hhold_ID) 

geodemoDT  <- tbl_df(read.xls(file.path(rawdata.folder, "edrp_geography_data.xlsx"))) %>% 
  rename(IDMETER = anonID) %>% # replace "--" entries in NUTS1 by "---" and in NUTS4 by "-------"
  mutate(NUTS1 = ifelse(NUTS1 == "--", "---", as.character(NUTS1))) %>%
  mutate(NUTS4 = ifelse(NUTS4 == "--" | NUTS4 == "", paste(NUTS1, "----", sep = ""), as.character(NUTS4))) %>%
  mutate(NUTS2 = substr(NUTS4, 1, 4)) %>%
  mutate(NUTS3 = substr(NUTS4, 1, 5))

geodemoDT <- geodemoDT %>% 
  mutate(ACORN_Category = ifelse(ACORN_Category == "" | is.na(ACORN_Category), "-", ACORN_Category)) %>%
  mutate(ACORN_Group = as.numeric(ifelse(ACORN_Group == "" | is.na(ACORN_Group), "99", ACORN_Group))) %>%
  mutate(ACORN_Type = as.numeric(ifelse(ACORN_Type == "" | is.na(ACORN_Type), "99", ACORN_Type))) %>% 
  mutate(DEMO1 = paste("D", ACORN_Category, sep = ""), 
         DEMO2 = paste(DEMO1, sprintf("%02d", ACORN_Group), sep = ""), 
         DEMO3 = paste(DEMO2, sprintf("%02d", ACORN_Type), sep = ""))

# infoDT <- inner_join(metaDT, geodemoDT, by = "IDMETER") # 14319 meters (not 14621 meters = 16249 - 1628)
infoDT <- inner_join(metaDT, geodemoDT, by = "IDMETER")


allmeters <- infoDT %>% dplyr::select(IDMETER) %>% .$IDMETER

print("Making info file")
# Create the info file
save(file = file.path(work.folder, "info.Rdata") , list = c("infoDT", "allmeters"))

# Create a file for each meter
do.it <- TRUE
if(do.it){
  print("Making files for each meter")
  setmeters <- allmeters
  
  setmeters <- allmeters[6685:length(allmeters)]
  
  res <- lapply(setmeters, function(idmeter){
    
    if(idmeter%%100 == 0)
    print(idmeter)
    
    infoMeter <- infoDT %>% filter(IDMETER == idmeter) %>% select(firstAdvance, lastAdvance) 
    firstAdvance <- infoMeter %>% .$firstAdvance
    lastAdvance  <- infoMeter %>% .$lastAdvance
    alldates <- seq(firstAdvance, lastAdvance, by = "30 min")
    
    navec <- rep(NA, length(alldates))
    dataset <- tbl_df(data.frame(ELECKWH = navec)) # ELECKWH	
    
    #dataset <- tbl_df(data.frame(TIME = alldates, HH = 2*(hour(alldates) + 1) - (minute(alldates) == 0), ELECKWH = navec)) # TIME HH ELECKWH	
    
    meterdata <- filter(DT, IDMETER == idmeter) %>% 
      mutate_each(funs(dmy_hms), ADVANCEDATETIME) %>%
      arrange(ADVANCEDATETIME) # IDMETER ADVANCEDATETIME HH ELECKWH
    
    datetime <- select(meterdata, ADVANCEDATETIME) %>% .$ADVANCEDATETIME
    index <- match(datetime, alldates)
    stopifnot(all(!is.na(index)))
    
    dataset[index, c("ELECKWH")] <- select(meterdata, ELECKWH)
    
    #dataset[index, c("TIME", "ELECKWH")] <- select(meterdata, ADVANCEDATETIME, ELECKWH)
    #dataset <- dataset %>% 						
    #  mutate(year = year(TIME)) %>% 
    #  mutate(month = month(TIME)) %>% 
    #  mutate(day = day(TIME)) %>%
    #  mutate(dayOfWeek = lubridate::wday(TIME, label = T)) %>%
    #  mutate(dayOfYear = yday(TIME))
    
    stopifnot(nrow(dataset) > 0)
    save(file = file.path(initmeters.folder, paste("meter-", idmeter, ".Rdata", sep = "")) , list = c("dataset"))
  })
}


