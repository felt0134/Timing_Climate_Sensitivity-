#

#estimate day of of maximum sensitivity

#setup ----
period_list <- seq(1,15,1) #set periods
period_list <- as.character(period_list) #easier when they are characters
year_list <- seq(2003,2020,1) #set years
year_list <- as.character(year_list) #easier when they are characters


#first do the GPP import -----

#loop through each year and period combination

#list to store outputs in
gpp_list <- list()

#run the loop
for(i in period_list){
  
  filepath <-dir(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP/Period/',i,'/'),full.names = T)
  test <- lapply(filepath,format_gpp_df)
  test <- data.frame(do.call('rbind',test))
  #test <- lapply(test,rasterToPoints)
  gpp_list[[i]] <- test
  
}

#convert list of dataframes to a single dataframe
gpp_df <- do.call('rbind',gpp_list)
rm(gpp_list,test) #get rid of excess stuff


#Now import and format precipitation data ------

#now loop through period and year

#create list to store values
ppt_list <- list()

#loop through each year and period
for(i in period_list){
  
  filepath <-dir(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/',i,'/'),full.names = T)
  test_ppt <- lapply(filepath,format_ppt_df)
  test_ppt <- data.frame(do.call('rbind',test_ppt))
  ppt_list[[i]] <- test_ppt
  
}

#convert to dataframe
ppt_df <- do.call('rbind',ppt_list)
rm(ppt_list,test_ppt)
 

# for later use: get average total growing season PPT for each pixel
# annual_ppt <- aggregate(ppt~x+y+year,sum,data=mean_ppt_gpp_2)
# annual_ppt <- aggregate(ppt~x+y,mean,data=annual_ppt)
#plot(rasterFromXYZ(annual_ppt))

#plot(gpp_dev~ppt_dev,data=mean_ppt_gpp_2)


#loop through each period to get GPP-PPT slopes (across years) -----

#merge the two dataframes by location, year, and period within each year
ppt_gpp <- merge(gpp_df,ppt_df,by=c('x','y','year','period'))
#head(ppt_gpp)


#get PPT and GPP means for each period and combine the data
mean_ppt <- aggregate(ppt~x+y+period,mean,data=ppt_gpp)
mean_gpp <- aggregate(gpp~x+y+period,mean,data=ppt_gpp)
mean_ppt_gpp <- merge(mean_ppt,mean_gpp,by=c('x','y','period'))
colnames(mean_ppt_gpp) <- c('x','y','period','ppt_mean','gpp_mean')
#head(mean_ppt_gpp)
rm(mean_ppt,mean_gpp)

#merge means with original data frame and get PPT and GPP deviations from the mean for each year-period combination
mean_ppt_gpp_2 <- merge(mean_ppt_gpp, ppt_gpp,by=c('x','y','period'))
mean_ppt_gpp_2$ppt_dev <- mean_ppt_gpp_2$ppt_mean - mean_ppt_gpp_2$ppt
mean_ppt_gpp_2$gpp_dev <- mean_ppt_gpp_2$gpp_mean - mean_ppt_gpp_2$gpp

#create two lists to store slopes as raster and data frames
period_slope_list <- list()
raster_slope_list <- list()

for(i in period_list){
  
  #subset to the period
  period_subset <- subset(mean_ppt_gpp_2,period==i)
  
  #get the slope between GPP and PPT deviation for that period
  period_slope <-  period_subset %>% group_by(x, y) %>%
    dplyr::do(model = lm(gpp_dev~ppt_dev, data = .)) %>%
    dplyr::mutate(coef=coef(model)[2])
  
  #convert to a dataframe
  period_slope <- data.frame(period_slope[c(1,2,4)])
  period_slope$period <- i
  
  #convert to a raster
  period_raster <- rasterFromXYZ(period_slope[c(1,2,3)])
  
  #store the outputs in the lists
  period_slope_list[[i]] <- period_slope
  raster_slope_list[[i]] <- period_raster 
  
}

#take a look
#plot(raster_slope_list$'10')

#import conversion of period to day of year to map period on to DOY
doy_conversion <- read.csv('./../../Data/GPP/period_day_match.csv')
#head(doy_conversion)

#convert list of dataframe to a single dataframe
full_period_slope_df <- do.call('rbind',period_slope_list)
head(full_period_slope_df)
full_period_slope_df <- merge(full_period_slope_df,doy_conversion[c(2,3)],by=c('period'))

#produce an ID column for later merging/subsetting specific coordinates
full_period_slope_df_id <- aggregate(coef ~x+y,mean,data=full_period_slope_df)
full_period_slope_df_id$id <- seq.int(nrow(full_period_slope_df_id))
full_period_slope_df <- merge(full_period_slope_df,
                              full_period_slope_df_id[c(1,2,4)],by=c('x','y'))

#remove excess data
rm(period_raster,period_list,period_slope,period_subset,period_slope_list,
   full_period_slope_df_id)

#merge with main slope ID

#look at maximum slopes
# max_sens <- aggregate(coef~x+y,max,data=full_period_slope_df)
# # head(max_sens)
# max_sens_2 <- merge(max_sens,full_period_slope_df,by=c('x','y','coef'))
# # head(max_sens_2)
# # str(max_sens_2)
# # summary(max_sens_2)
# #plot(rasterFromXYZ(max_sens_2[c(1,2,5)]))


#plot(raster(filename))


#estimate maximum sensitivity with splines/predictions ----


#run loop to get critical decision date for each pixel
id_list <- as.factor(unique(full_period_slope_df$id))
max_sens_list <- list()

for(i in id_list){
  
  #fitting smoothing splines
  full_period_slope_df_2_test <- full_period_slope_df %>%
    dplyr::filter(id==i)
  #dim(full_period_slope_df_2_test)
  
  x <- unique(full_period_slope_df_2_test %>% pull(x))
  y <- unique(full_period_slope_df_2_test %>% pull(y))
  
  #head(full_period_slope_df_2_test)
  
  #plot(coef~doy,data=full_period_slope_df_2_test)
  #https://stat.ethz.ch/R-manual/R-devel/library/stats/html/smooth.spline.html
  doy.spl <- with(full_period_slope_df_2_test, smooth.spline(doy, coef))
  #lines(doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from=65,to=297,by=1))
  sens_predicted <- data.frame(predict(doy.spl,doy))
  colnames(sens_predicted) <- c('day','sensitivity')
  #plot(sensitivity~day,col='black',pch=19,data=sens_predicted)
  
  #get day of predicted maximum sensitivity
  max_sens <- max(sens_predicted$sensitivity)
  max_sens <- subset(sens_predicted,sensitivity==max_sens)
  max_sens$id <- i
  max_sens$x <- x
  max_sens$y <- y
  
  max_sens <- max_sens[c(4,5,1,2,3)]
  
  
  max_sens_list[[i]] <- max_sens
  
  #rm(sens_predicted)
  
  
}

#day of maximum sensitivity 
max_sens_df <- data.frame(do.call('rbind', max_sens_list))
max_sens_raster <- rasterFromXYZ(max_sens_df[c(1,2,3)])
crs(max_sens_raster) <- "+proj=longlat +datum=WGS84"
#plot(max_sens_raster)
filename <- paste0('./../../Data/CDD/maximum_sensitivity/max_sens_day_',Ecoregion,'.tif')
writeRaster(max_sens_raster,filename)

#value of maximum sensitivity 
max_sens_raster_slope <- rasterFromXYZ(max_sens_df[c(1,2,4)])
crs(max_sens_raster_slope) <- "+proj=longlat +datum=WGS84"
#plot(max_sens_raster_slope)
filename <- paste0('./../../Data/CDD/maximum_sensitivity/max_sens_slope_',Ecoregion,'.tif')
writeRaster(max_sens_raster_slope,filename)



