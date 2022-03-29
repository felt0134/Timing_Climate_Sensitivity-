# CLimate windows


library(climwin)
library(daymetr)
#https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0167980
#https://cran.r-project.org/web/packages/climwin/vignettes/climwin.html


# test ------

#example code
head(MassClimate)
head(Mass)

xvar = list(Temp = MassClimate$Temp)

cdate = MassClimate$Date

bdate = Mass$Date

#create a 'null' model
baseline <- lm(Mass ~ 1, data = Mass)

#time units
cinterval = "day"

#range of the window
range = c(150, 0)

#type (relative versus absolute)
type = "absolute"

#what is the starting day? May 20th. This will look at days before May 20th.
refday = c(20, 5)

#summary stat within the window. Could be 'sum' for precipitation
stat = "mean"

# The parameter ‘func’ allows users to select from a range of possible relationships, 
# including linear (“lin”), quadratic (“quad”), cubic (“cub”), logarithmic (“log”) and inverse (“inv”).

func = "lin"

MassWin <- slidingwin(xvar = list(Temp = MassClimate$Temp),
                      cdate = MassClimate$Date,
                      bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass),
                      cinterval = "day",
                      range = c(150, 0),
                      type = "absolute", refday = c(20, 05),
                      stat = "mean",
                      func = "lin")

head(MassWin)
head(MassWin[[1]]$Dataset)

#try to apply this to ppt and gpp--------

region_name = 'shortgrass_steppe'

#load in gpp data
gpp_list <- list()
period_list <- seq(1,15,1) #set periods
period_list <- as.character(period_list) #easier when they are characters
year_list <- seq(2003,2020,1) #set years
year_list <- as.character(year_list) 
Ecoregion = region_name

#run the loop
for(i in period_list){
  
  filepath <-dir(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP/Period/',i,'/'),full.names = T)
  test_gpp <- lapply(filepath,format_gpp_df)
  test_gpp <- data.frame(do.call('rbind',test_gpp))
  gpp_list[[i]] <- test_gpp
  
}

#convert list of dataframes to a single dataframe
gpp_df <- do.call('rbind',gpp_list)
rm(gpp_list,test_gpp)

#use gpp coordinates as the basis for the ppt data
gpp_df_mean <- aggregate(gpp~x+y, mean,data=gpp_df)
head(gpp_df_mean)

#testing
gpp_df_mean_2 <- gpp_df_mean[c(2000:2010),]

#full
library(splitstackshape)
gpp_df_mean_2<-stratified(gpp_df_mean, c("y"), 0.05)

window_list <- list()

for(i in 1:nrow(gpp_df_mean_2)){

print(i)
  
temp_lat <- gpp_df_mean_2[i,] %>% pull(y)
temp_lon <- gpp_df_mean_2[i,] %>% pull(x)

test <- download_daymet(
  lat = temp_lat,
  lon = temp_lon,
  start = 2003,
  end = 2020,
) %>%
  #--- just get the data part ---#
  .$data #%>%
# #--- convert to tibble (not strictly necessary) ---#
# as_tibble() %>%
# #--- assign site_id so you know which record is for which site_id ---#
# mutate(site_id = temp_site) %>%
# #--- get date from day of the year ---#
# mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))

test <- test %>%
  dplyr::filter(yday > 57) %>%
  dplyr::filter(yday < 297)

#dd/mm/yyyy is the correct units for date

#format dates to correct format
test$date <- as.Date((test$yday-1), origin = paste0(test$year,"-01-01"))

test$date_2 = format(test$date, "%d/%m/%Y")

#trim down columns
test <- test %>%
  select(yday,prcp..mm.day.,date_2)

test$x <- temp_lon
test$y <- temp_lat

#fix for merging so matches with GPP
# test$x <- round(test$x,9)
# test$y <- round(test$y,9)

#re-order columns
test <- test %>%
  select(x,y,yday,prcp..mm.day.,date_2)

#get sum of growing season gpp
gpp_sums <- gpp_df %>%
  group_by(x,y,year) %>%
  summarise(gpp_sum = sum(gpp))

#gpp_sums <- aggregate(gpp~x+y,sum,data=gpp_df)

gpp_sums$doy <- 296

#format dates to correct format
gpp_sums$date <- as.Date((gpp_sums$doy-1), origin = paste0(gpp_sums$year,"-01-01"))
gpp_sums$date_2 = format(gpp_sums$date, "%d/%m/%Y")

# gpp_sums$x <- round(gpp_sums$x,9)
# gpp_sums$y <- round(gpp_sums$y,9)

#as.character(temp_lon)

gpp_sums <- gpp_sums %>%
  select(x,y,gpp_sum,date_2) %>%
  filter(x == temp_lon) %>% #round so coordinates match
  filter(y == temp_lat)

# str(gpp_sums)
# str(test)
# 
# test_merge <- merge(gpp_sums,test,by=c("x","y"))
# gpp_sums$x - temp_lon
# full_join(gpp_sums,sgs.1000,by=c("x","y"))
# gpp_sums$id <- seq_along(gpp_sums[[1]])

#apply the same workflow now to climatw windows analysis
# xvar = list(Precip = test$prcp..mm.day.)
# 
# cdate = test$date_2
# 
# bdate = gpp_sums$date_2
# 
# #create a 'null' model
# baseline <- lm(gpp_sum ~ 1, data = gpp_sums)
# 
# #time units
# cinterval = "day"
# 
# #range of the window
# range = c(239, 0)
# 
# #type (relative versus absolute)
# type = "absolute"
# 
# #what is the starting day? May 20th. This will look at days before May 20th.
# refday = c(23, 10)
# 
# #summary stat within the window. Could be 'sum' for precipitation
# stat = "sum"
# 
# # The parameter ‘func’ allows users to select from a range of possible relationships, 
# # including linear (“lin”), quadratic (“quad”), cubic (“cub”), logarithmic (“log”) and inverse (“inv”).
# 
# func = "lin"

ccp_gpp_ppt <- slidingwin(xvar = list(Precip = test$prcp..mm.day.),
                      cdate = test$date_2,
                      bdate = gpp_sums$date_2,
                      baseline = lm(gpp_sum ~ 1, data = gpp_sums),
                      cinterval = "day",
                      range = c(225, 0),
                      type = "absolute", refday = c(20, 10),
                      stat = "sum",
                      func = "lin")

#get the climate window into a dataframe
day_end <- yday("20/10/2005")
window_open <- head(ccp_gpp_ppt[[1]]$Dataset,1)$WindowOpen
window_close <- head(ccp_gpp_ppt[[1]]$Dataset,1)$WindowClose

julian_window_close <- day_end - window_close
julian_window_open <- day_end - window_open
julian_window_length <- window_open - window_close

windows <- c('window_open')
values <- c(julian_window_open)

window_df <- data.frame(julian_window_open)
window_df$julian_window_close <- day_end - window_close
window_df$julian_window_length <- window_open - window_close
window_df$x <- temp_lon
window_df$y <- temp_lat
window_df$id <- i

window_list[[i]] <- window_df

}

window_df <- list_to_df(window_list)
head(window_df)
hist(window_df$julian_window_close)

summary(window_df_2)

#remove zeros
window_df_2 <- window_df %>%
  filter(julian_window_length > 0)

ggplot(window_df_2,aes(julian_window_length)) +
  geom_histogram(fill='white',color='black',bins=50) +
  geom_histogram(window_df,aes(julian_window_close))

end_date <- as.Date((252-1), origin = paste0(2003,"-01-01"))
end_date_2 = format(end_date, "%d/%m/%Y")



#next steps will be to look at annual NPP instead of cumulative or mean GPP

#for grasslands/rangeland specific data
#https://rangelands.app/products/#rangeland-carbon
#https://support.rangelands.app/article/63-export-annual-biomass-rasters

#could also look at modis NPP data through R. should be quicker due to just
#getting annual data.
#https://lpdaac.usgs.gov/products/myd17a3hgfv006/




