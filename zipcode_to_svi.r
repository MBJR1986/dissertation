# Script to create useable SVI dataset for analysis 
# input population: Sports Medicine Patients with Zipcodes, necessary to join to 
# County datasets, which is later joined to SVI data to produce SVI data at the county level,
# which is the most specific I could get at this time.

#load packages
library(sqldf)

#load data
setwd('D:/KUMC/Dissertation/data/Mark_dissertation_20170718/zipcode_svi/')
zipcode <- read.csv("zipcodes.csv") #patient-level zipcodes
county <- read.csv("zipcode_to_county.csv") #used for joining zipcodes to county name
#county-level SVI data
kansas <- read.csv("Kansas_COUNTY.csv") 
missouri <- read.csv("Missouri_COUNTY.csv")
florida <- read.csv("Florida_COUNTY.csv")
texas <- read.csv("Texas_COUNTY.csv")
washington <- read.csv("Washington_COUNTY.csv")
iowa <- read.csv("Iowa_COUNTY.csv")


#concat state files into one
svi <- rbind(kansas, missouri, florida, texas, washington, iowa)

#truncate '-****' of zipcodes, leaving just first 5 digits 
zipcode_trun <- as.integer(strtrim(zipcode$ZIP_CD, 5))
zipcode$zipcode_trun <- zipcode_trun
colnames(zipcode) <- c("MRN", "zipcode", "zipcode_trun")

#add county to zipcode DF
pt_zip <- sqldf("select x.MRN
                ,x.zipcode_trun as zipcode
                ,y.state
                ,y.state_abbr
                ,y.county
                from zipcode as x
                left join county as y
                on x.zipcode_trun = y.zipcode")

#remove Nulls. ONe person is from france, and thus no SVI data
pt_zip <- pt_zip[!is.na(pt_zip$county),]

##############################################################
###############join in svi data to pt_zip#####################
##############################################################

#select needed data from svi
cols <- c("STATE", "ST_ABBR", "COUNTY", "E_POV", "E_UNEMP", "E_PCI", "E_NOHSDP", "E_AGE65", "E_AGE17", "E_DISABL", "E_SNGPNT", "E_MINRTY",
          "E_LIMENG", "E_MUNIT", "E_MOBILE", "E_CROWD", "E_NOVEH", "E_GROUPQ", "SPL_THEME1", "RPL_THEME1",
          "SPL_THEME2", "RPL_THEME2", "SPL_THEME3", "RPL_THEME3", "SPL_THEME4", "RPL_THEME4", "SPL_THEMES", "RPL_THEMES",
          "E_UNINSUR")
#subset svi
svi_cols <- svi[,cols]

#join data into final dataframe
county_svi <- sqldf("select y.*
                    ,x.MRN
                    ,x.zipcode
                    from pt_zip as x
                    inner join svi_cols as y
                    on y.ST_ABBR = x.state_abbr
                    AND y.COUNTY = x.county")


#write out file
#setwd('D:/KUMC/Dissertation/data/Mark_dissertation_20170718')
#write.csv(county_svi, 'patient_svi_countylevel.csv')
