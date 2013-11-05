# ANALYZER GRoup #1

################### FURTHER DATA CLEAN UP: GENERATE PPX BASED ON CLEANED DATA ##################

# Load the data provided by data curators
cleandata <- read.csv("~/CleanData1938-2013.csv")

# Load ETAS package
# Uncomment next line to install the package if necessary
# install.packages("ETAS")
library(ETAS)

# Prepare for PPX Generation
event_date  = as.character(cleandata[["YYYY.MM.DD"]])
event_hms   = as.character(cleandata[["HH.mm.SS.ss"]])
event_time  = paste(event_date, event_hms)
event_year  = as.numeric(gsub("/.*","",event_date))
start_date  = paste(as.character(min(event_year)),sep="/","01/01 00:00:00")
time <- date2day(event_time, start_date, tz="GMT") 

data_ppx = ppx(data=data.frame(time=time, long=cleandata$LON, lat=cleandata$LAT,
                               mag=cleandata$MAG, depth=cleandata$DEPTH, date=event_time),
               domain=c(c(min(time),max(time)),
                        c(min(cleandata$LON),max(cleandata$LON)),
                        c(min(cleandata$LAT),max(cleandata$LAT))),
               coord.type =c("temporal","spatial","spatial","mark","mark","mark"))

plot(data_ppx, main="Scatter Plot of Epic Records", pch=20)

# Road Block: Too many data and make the plot hard to read
# Solution, filter the data based on some criteria
event_mag   = as.numeric(cleandata$MAG)
event_depth = as.numeric(cleandata$DEPTH)
event_lat   = as.numeric(cleandata$LAT)
event_lon   = as.numeric(cleandata$LON)

select_epic_data<-function(year_range  = c(min(event_year),max(event_year)), 
                          mag_range   = c(0,max(event_mag)), 
                          lat_range   = c(min(event_lat), max(event_lat)), 
                          lon_range   = c(min(event_lon), max(event_lon)), 
                          depth_range = c(0,max(event_depth))){
  # set up filters
  year_filter  =(event_year >= year_range[1]) & (event_year <= year_range[2])
  mag_filter   =(event_mag  >=  mag_range[1]) & (event_mag  <=  mag_range[2])
  lat_filter   =(event_lat  >=  lat_range[1]) & (event_lat  <=  lat_range[2])
  lon_filter   =(event_lon  >=  lon_range[1]) & (event_lon  <=  lon_range[2])
  depth_filter =(event_depth>=depth_range[1]) & (event_depth<=depth_range[2])
  
  # Apply filters to data.frame
  subdata=cleandata[year_filter & mag_filter & lat_filter & lon_filter & depth_filter, ]
  
  # Prepare for PPX generation
  sub_event_date  = as.character(subdata[["YYYY.MM.DD"]])
  sub_event_hms   = as.character(subdata[["HH.mm.SS.ss"]])
  sub_start_date  = paste(as.character(min(year_range[1])),sep="/","01/01 00:00:00")
  sub_event_time  = paste(sub_event_date, sub_event_hms)
  sub_time <- date2day(sub_event_time, sub_start_date, tz="GMT")

  # Generate PPX based on filtered data
  sub_data_ppx = ppx(data=data.frame(time=sub_time, long=subdata$LON, lat=subdata$LAT,
                                 mag=subdata$MAG, depth=subdata$DEPTH, date=sub_event_time),
                 domain=c(c(min(sub_time),max(sub_time)),
                          c(lon_range[1],lon_range[2]),
                          c(lat_range[1],lat_range[2])),
                 coord.type =c("temporal","spatial","spatial","mark","mark","mark"))
}

plot(select_epic_data(mag_range=c(3,10),depth_range=c(0,5),year_range=c(1960,1980)),
     main="Scatter Plot of Epic Records with Mag Greater Than 3 and Depth Less than 5 During 1960-1980", 
     pch=20)
