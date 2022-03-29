# Figures 
library(scico)
#https://www.data-imaginist.com/2018/scico-and-the-colour-conundrum/

library(broom)
library(sp)

#julian day
#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html


#prep ------

#projection:

Albers <-
  crs(
    '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  )

# aea.proj <- 
#   "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# 
# aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#get shapefiles of states and update projection
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c(
  'Wyoming',#'Colorado'
  'Montana','Kansas',#'New Mexico',
  'North Dakota','South Dakota','Nebraska',
  'Oklahoma'),]

#states_all_sites <- sp::spTransform(states_all_sites, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
states_all_sites <- sp::spTransform(states_all_sites, 
                                    CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')) 

#prep for ggplotting
#https://stackoverflow.com/questions/62435609/plot-shapefile-with-ggplot2
states_all_sites_df <- fortify(states_all_sites)
states_all_sites_tidy <- tidy(states_all_sites)

# Recategorizes data as required for plotting
states_all_sites$id <- row.names(states_all_sites)
states_all_sites_tidy <- left_join(states_all_sites_tidy, states_all_sites@data)



#-------------------------------------------------------------------------------
# day of 90% growth-----

#import

#sgs
day_90_sgs <- raster('./../../Data/CDD/day_of_90/day_90_shortgrass_steppe.tif')
plot(day_90_sgs)
day_90_sgs_df <- data.frame(rasterToPoints(day_90_sgs))
day_90_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_90_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_90_sgs_df$doy) #263 = September 20

#nmp
day_90_nmp <- raster('./../../Data/CDD/day_of_90/day_90_northern_mixed_prairies.tif')
plot(day_90_nmp)
day_90_nmp_df <- data.frame(rasterToPoints(day_90_nmp))
day_90_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_90_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_90_nmp_df$doy) #248 = September 5

day_90_df <- rbind(day_90_nmp_df,day_90_sgs_df)

#filter out extreme high and low values
day_90_df <- day_90_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

day_90_pdf <- ggplot(day_90_df, aes(x = doy, fill = Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'black',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Day of 90% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day90_distributions.png')

print(day_90_pdf)

dev.off()


# nmp_look <- stack(day_90_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_90_sgs_nmp <- raster::merge(day_90_sgs,day_90_nmp,tolerance=0.2)
crs(day_90_sgs_nmp) <- Albers
plot(day_90_sgs_nmp)
day_90_sgs_nmp <- data.frame(rasterToPoints(day_90_sgs_nmp))

#filter out extreme high and low values
day_90_sgs_nmp <- day_90_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

str(day_90_sgs_nmp)
head(day_90_sgs_nmp)

#play around with creating day ranges
# day_90_sgs_nmp_2 <- day_90_sgs_nmp %>%
#   group_by(x,y) %>%
#   summarise(
#   layer2 = paste((layer-16), min(layer), sep = "-"))
# 
# head(day_90_sgs_nmp_2)
# day_90_sgs_nmp_2$layer2 <- as.numeric(day_90_sgs_nmp_2$layer2)

#turn to raster and fix projection, and then back to dataframe for plotting
day_90_sgs_nmp <- rasterFromXYZ(day_90_sgs_nmp)
proj4string(day_90_sgs_nmp) <- CRS("+proj=longlat")
day_90_sgs_nmp <-projectRaster(day_90_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_90_sgs_nmp_df <- data.frame(rasterToPoints(day_90_sgs_nmp))


day_90_sgs_nmp_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_90_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of 90% growth',palette = 'batlow',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1500,width=920,res=300,'./../../Figures/day_90_growth_map.png')

print(day_90_sgs_nmp_map)

dev.off()


#impact of drought on the 90% day of growth

#import

#sgs
day_90_drought_sgs <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtshortgrass_steppe.tif')
plot(day_90_drought_sgs)
day_90_drought_sgs <- stack(day_90_drought_sgs, day_90_sgs)
plot(day_90_drought_sgs)
day_90_drought_sgs_2 <-
  day_90_drought_sgs$day_90_droughtshortgrass_steppe -
  day_90_drought_sgs$day_90_shortgrass_steppe
plot(day_90_drought_sgs_2)
summary(day_90_drought_sgs_2)
#median = 4

plot(layer~y,data=rasterToPoints(day_90_drought_sgs_2))

#nmp
day_90_drought_nmp <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtnorthern_mixed_prairies.tif')
plot(day_90_drought_nmp)
day_90_drought_nmp <- stack(day_90_drought_nmp, day_90_nmp)
plot(day_90_drought_nmp)
day_90_drought_nmp_2 <-
  day_90_drought_nmp$day_90_droughtnorthern_mixed_prairies -
  day_90_drought_nmp$day_90_northern_mixed_prairies
plot(day_90_drought_nmp_2)
summary(day_90_drought_nmp_2)
#median = 0
hist(day_90_drought_nmp_2$layer)

#combine
day_90_drought <-
  raster::merge(day_90_drought_nmp_2, day_90_drought_sgs_2, tolerance = 0.20)
crs(day_90_drought) <- Albers
plot(day_90_drought)
summary(day_90_drought)
day_90_drought_df <- data.frame(rasterToPoints(day_90_drought))
str(day_90_drought_df)

day_90_sgs_nmp_drought_map <-
  ggplot(day_90_drought_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  coord_equal() +
  #geom_sf()
  scale_fill_scico(
    'Drought impact to day \n of 90% growth (days)',
    palette = 'vik',
    direction = -1,
    midpoint = 0) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(),
    #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color = 'black', size = 10),
    axis.title.y = element_text(color = 'black', size = 10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.30, 0.3),
    #legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

#save to file
png(
  height = 1500,
  width = 2000,
  res = 300,
  './../../Figures/day_90_drought_growth_map.png'
)

print(day_90_sgs_nmp_drought_map)

dev.off()

#PDF of drought impact to 90% growth

#nmp
day_90_drought_nmp_2_df <- data.frame(rasterToPoints(day_90_drought_nmp_2))
day_90_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_90_drought_sgs_2_df <- data.frame(rasterToPoints(day_90_drought_sgs_2))
day_90_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_90_drought_nmp_sgs_2_df <- rbind(day_90_drought_nmp_2_df,day_90_drought_sgs_2_df)
head(day_90_drought_nmp_sgs_2_df)

#plot it
drought_day90_pdf <- ggplot(day_90_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_histogram(color='black',binwidth = 1) +
  #geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Drought impact to day of 90% growth') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.77, 0.55),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day90_drought_distributions.png')

print(drought_day90_pdf)

dev.off()



#-------------------------------------------------------------------------------
# day of 50% growth-----

#import

#sgs
day_50_sgs <- raster('./../../Data/CDD/day_of_50/day_50_shortgrass_steppe.tif')
plot(day_50_sgs)
day_50_sgs_df <- data.frame(rasterToPoints(day_50_sgs))
day_50_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_50_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_50_sgs_df$doy) #179

#nmp
day_50_nmp <- raster('./../../Data/CDD/day_of_50/day_50_northern_mixed_prairies.tif')
plot(day_50_nmp)
day_50_nmp_df <- data.frame(rasterToPoints(day_50_nmp))
day_50_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_50_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_50_nmp_df$doy) #174

day_50_df <- rbind(day_50_nmp_df,day_50_sgs_df)

#filter out extreme high and low values
day_50_df <- day_50_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

day_50_pdf <- ggplot(day_50_df, aes(x = doy, fill = Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(130, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'black',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Day of 50% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day50_distributions.png')

print(day_50_pdf)

dev.off()


# nmp_look <- stack(day_50_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_50_sgs_nmp <- raster::merge(day_50_sgs,day_50_nmp,tolerance=0.2)
crs(day_50_sgs_nmp) <- Albers
plot(day_50_sgs_nmp)
day_50_sgs_nmp <- data.frame(rasterToPoints(day_50_sgs_nmp))

#filter out extreme high and low values
day_50_sgs_nmp <- day_50_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

str(day_50_sgs_nmp)
head(day_50_sgs_nmp)

#play around with creating day ranges
# day_50_sgs_nmp_2 <- day_50_sgs_nmp %>%
#   group_by(x,y) %>%
#   summarise(
#   layer2 = paste((layer-16), min(layer), sep = "-"))
# 
# head(day_50_sgs_nmp_2)
# day_50_sgs_nmp_2$layer2 <- as.numeric(day_50_sgs_nmp_2$layer2)

#turn to raster and fix projection, and then back to dataframe for plotting
day_50_sgs_nmp <- rasterFromXYZ(day_50_sgs_nmp)
proj4string(day_50_sgs_nmp) <- CRS("+proj=longlat")
day_50_sgs_nmp <-projectRaster(day_50_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_sgs_nmp_df <- data.frame(rasterToPoints(day_50_sgs_nmp))


day_50_sgs_nmp_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_50_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of 50% growth',palette = 'batlow',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1500,width=920,res=300,'./../../Figures/day_50_growth_map.png')

print(day_50_sgs_nmp_map)

dev.off()


#impact of drought on the 50% day of growth

#import

#sgs
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtshortgrass_steppe.tif')
plot(day_50_drought_sgs)
day_50_drought_sgs <- stack(day_50_drought_sgs, day_50_sgs)
plot(day_50_drought_sgs)
day_50_drought_sgs_2 <-
  day_50_drought_sgs$day_50_droughtshortgrass_steppe -
  day_50_drought_sgs$day_50_shortgrass_steppe
plot(day_50_drought_sgs_2)
summary(day_50_drought_sgs_2)
#median = -22
#179-22

plot(layer~y,data=rasterToPoints(day_50_drought_sgs_2))

#nmp
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtnorthern_mixed_prairies.tif')
plot(day_50_drought_nmp)
day_50_drought_nmp <- stack(day_50_drought_nmp, day_50_nmp)
plot(day_50_drought_nmp)
day_50_drought_nmp_2 <-
  day_50_drought_nmp$day_50_droughtnorthern_mixed_prairies -
  day_50_drought_nmp$day_50_northern_mixed_prairies
plot(day_50_drought_nmp_2)
summary(day_50_drought_nmp_2)
#median = -11
hist(day_50_drought_nmp_2$layer)

#combine
day_50_drought <-
  raster::merge(day_50_drought_nmp_2, day_50_drought_sgs_2, tolerance = 0.20)
proj4string(day_50_drought) <- CRS("+proj=longlat")
day_50_drought <-projectRaster(day_50_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_drought_df <- data.frame(rasterToPoints(day_50_drought))

# % of pixels with negative values (advanced day50)
(day_50_drought_df %>%
  filter(layer < 0) %>%
  summarise(length(layer)))/(length(day_50_drought_df$layer))
#0.90
  

day_50_sgs_nmp_drought_map <-
   ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_50_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Drought impact to day \n of 50% growth (days)',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#save to file
png(
  height = 1500,
  width = 2000,
  res = 300,
  './../../Figures/day_50_drought_growth_map.png'
)

print(day_50_sgs_nmp_drought_map)

dev.off()

#PDF of drought impact to 50% growth

#nmp
day_50_drought_nmp_2_df <- data.frame(rasterToPoints(day_50_drought_nmp_2))
day_50_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_50_drought_sgs_2_df <- data.frame(rasterToPoints(day_50_drought_sgs_2))
day_50_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_50_drought_nmp_sgs_2_df <- rbind(day_50_drought_nmp_2_df,day_50_drought_sgs_2_df)
head(day_50_drought_nmp_sgs_2_df)

#plot it
drought_day50_pdf <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  #geom_histogram(color='black',binwidth = 1) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  geom_vline(xintercept = 0,color='red') +
  xlab('Drought impact to day of 50% growth (days)') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = c(0.82, 0.7),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day50_drought_distributions.png')

print(drought_day50_pdf)

dev.off()

#try to make inset
library(grid)
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
library(grid)

full <- function() {
  print(day_50_sgs_nmp_drought_map)
  print(drought_day50_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'./../../Figures/day50_drought_inset_plot.png')

full()

dev.off()



#-------------------------------------------------------------------------------
# day of 25% growth -----

#import

#sgs
day_25_sgs <- raster('./../../Data/CDD/day_of_25/day_25_shortgrass_steppe.tif')
plot(day_25_sgs)
day_25_sgs_df <- data.frame(rasterToPoints(day_25_sgs))
day_25_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_25_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_25_sgs_df$doy) #136

#nmp
day_25_nmp <- raster('./../../Data/CDD/day_of_25/day_25_northern_mixed_prairies.tif')
plot(day_25_nmp)
day_25_nmp_df <- data.frame(rasterToPoints(day_25_nmp))
day_25_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_25_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_25_nmp_df$doy) #146

day_25_df <- rbind(day_25_nmp_df,day_25_sgs_df)

#filter out extreme high and low values
day_25_df <- day_25_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

day_25_pdf <- ggplot(day_25_df, aes(x = doy, fill = Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(100, 181)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'black',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Day of 25% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day25_distributions.png')

print(day_25_pdf)

dev.off()


# nmp_look <- stack(day_25_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_25_sgs_nmp <- raster::merge(day_25_sgs,day_25_nmp,tolerance=0.2)
crs(day_25_sgs_nmp) <- Albers
plot(day_25_sgs_nmp)
day_25_sgs_nmp <- data.frame(rasterToPoints(day_25_sgs_nmp))

#filter out extreme high and low values
day_25_sgs_nmp <- day_25_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

str(day_25_sgs_nmp)
head(day_25_sgs_nmp)

#play around with creating day ranges
# day_25_sgs_nmp_2 <- day_25_sgs_nmp %>%
#   group_by(x,y) %>%
#   summarise(
#   layer2 = paste((layer-16), min(layer), sep = "-"))
# 
# head(day_25_sgs_nmp_2)
# day_25_sgs_nmp_2$layer2 <- as.numeric(day_25_sgs_nmp_2$layer2)

#turn to raster and fix projection, and then back to dataframe for plotting
day_25_sgs_nmp <- rasterFromXYZ(day_25_sgs_nmp)
proj4string(day_25_sgs_nmp) <- CRS("+proj=longlat")
day_25_sgs_nmp <-projectRaster(day_25_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_sgs_nmp_df <- data.frame(rasterToPoints(day_25_sgs_nmp))


day_25_sgs_nmp_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_25_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of 25% growth',palette = 'batlow',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1250,width=920,res=300,'./../../Figures/day_25_growth_map.png')

print(day_25_sgs_nmp_map)

dev.off()


#impact of drought on the 25% day of growth

#import

#sgs
day_25_drought_sgs <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtshortgrass_steppe.tif')
plot(day_25_drought_sgs)
day_25_drought_sgs <- stack(day_25_drought_sgs, day_25_sgs)
plot(day_25_drought_sgs)
day_25_drought_sgs_2 <-
  day_25_drought_sgs$day_25_droughtshortgrass_steppe -
  day_25_drought_sgs$day_25_shortgrass_steppe
plot(day_25_drought_sgs_2)
summary(day_25_drought_sgs_2)
#median = -21

plot(layer~y,data=rasterToPoints(day_25_drought_sgs_2))

#nmp
day_25_drought_nmp <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtnorthern_mixed_prairies.tif')
plot(day_25_drought_nmp)
day_25_drought_nmp <- stack(day_25_drought_nmp, day_25_nmp)
plot(day_25_drought_nmp)
day_25_drought_nmp_2 <-
  day_25_drought_nmp$day_25_droughtnorthern_mixed_prairies -
  day_25_drought_nmp$day_25_northern_mixed_prairies
plot(day_25_drought_nmp_2)
summary(day_25_drought_nmp_2)
#median = -14
hist(day_25_drought_nmp_2$layer)

#combine
#combine
day_25_drought <-
  raster::merge(day_50_drought_nmp_2, day_50_drought_sgs_2, tolerance = 0.20)
proj4string(day_25_drought) <- CRS("+proj=longlat")
day_25_drought <-projectRaster(day_25_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_drought_df <- data.frame(rasterToPoints(day_25_drought))

day_25_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_25_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Drought impact to day \n of 25% growth (days)',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#save to file
png(
  height = 1500,
  width = 2000,
  res = 300,
  './../../Figures/day_25_drought_growth_map.png'
)

print(day_25_sgs_nmp_drought_map)

dev.off()

#PDF of drought impact to 25% growth

#nmp
day_25_drought_nmp_2_df <- data.frame(rasterToPoints(day_25_drought_nmp_2))
day_25_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_25_drought_sgs_2_df <- data.frame(rasterToPoints(day_25_drought_sgs_2))
day_25_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_25_drought_nmp_sgs_2_df <- rbind(day_25_drought_nmp_2_df,day_25_drought_sgs_2_df)
head(day_25_drought_nmp_sgs_2_df)

#plot it
drought_day25_pdf <- ggplot(day_25_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_histogram(color='black',binwidth = 1) +
  #geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Drought impact to day of 25% growth') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.77, 0.55),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day25_drought_distributions.png')

print(drought_day25_pdf)

dev.off()
#-------------------------------------------------------------------------------
# day of maximum GPP (in progress) ------




#-------------------------------------------------------------------------------
# peak sensitivity ----

#day of maximum sensitivity 

# notes: something weird happens when the two ecoregions are merged as dataframes
# and then plotted/converted to rasters as opposed to merging as raster. but it 
# would be nice to be able to seprate out the ecoregions by color in the bivariate plots.

#import data
max_sens_doy_sgs <- raster('./../../Data/CDD/maximum_sensitivity/max_sens_day_shortgrass_steppe.tif')
plot(max_sens_doy_sgs)
max_sens_doy_sgs_df <- data.frame(rasterToPoints(max_sens_doy_sgs))
max_sens_doy_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(max_sens_doy_sgs_df) <-c('x','y','doy','Ecoregion')
median(max_sens_doy_sgs_df$doy) #176 = June 25th

max_sens_doy_nmp <- raster('./../../Data/CDD/maximum_sensitivity/max_sens_day_northern_mixed_prairies.tif')
plot(max_sens_doy_nmp)
max_sens_doy_nmp_df <- data.frame(rasterToPoints(max_sens_doy_nmp))
max_sens_doy_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(max_sens_doy_nmp_df) <-c('x','y','doy','Ecoregion')
median(max_sens_doy_nmp_df$doy) #200 = July 19th

#bind them
max_sens_doy <- rbind(max_sens_doy_nmp_df,max_sens_doy_sgs_df)
head(max_sens_doy)

#filter out extreme high and low values
max_sens_doy <- max_sens_doy %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)
?annotate
max_sens_pdf <- ggplot(max_sens_doy,aes(x=doy,fill=Ecoregion)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  geom_density(color='black',alpha=0.5,aes(y=..scaled..)) +
  annotate(geom='text',x=150,y=0.90,label='June 17th') +
  annotate(geom='text',x=215,y=0.90,label='July 21') +
  # geom_vline(xintercept = 168) +
  # geom_vline(xintercept = 202) +
  scale_fill_manual(values=c('Northern mixed prairies'='black','Shortgrass steppe'='white')) +
  xlab('Day of maximum sensitivity to rainfall') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.75),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/max_sens_distributions.png')

print(max_sens_pdf)

dev.off()


# max_sens_doy <- rasterFromXYZ(max_sens_doy[c(1,2,3)])
# plot(max_sens_doy)

#plot(rasterFromXYZ(max_sens_doy))

#bind rasters
max_sens_sgs_nmp <- raster::merge(max_sens_doy_sgs,max_sens_doy_nmp,tolerance=0.2)
#crs(max_sens_sgs_nmp) <- Albers
plot(max_sens_sgs_nmp)
max_sens_sgs_nmp <- data.frame(rasterToPoints(max_sens_sgs_nmp))

#filter out extreme high and low values
max_sens_sgs_nmp <- max_sens_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

#turn to raster and fix projection, and then back to dataframe for plotting
max_sens_sgs_nmp <- rasterFromXYZ(max_sens_sgs_nmp)
proj4string(max_sens_sgs_nmp) <- CRS("+proj=longlat")
max_sens_sgs_nmp <-projectRaster(max_sens_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_sens_sgs_nmp_df <- data.frame(rasterToPoints(max_sens_sgs_nmp))

max_sens_day_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_sens_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of peak sensitivity',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file

png(height = 1500,width=920,res=300,'./../../Figures/max_sens_day_map.png')

print(max_sens_day_map)

dev.off()



#merge in names
max_sens_sgs_nmp_test <- merge(max_sens_sgs_nmp,max_sens_doy_nmp_df[c(1,2,4)],by=c('x','y'))

#bivariate plot of day of maximum sensitivity and latitude
max_sens_day_bivari <- ggplot(max_sens_sgs_nmp, aes(x = y, y = layer)) + 
  geom_point(alpha=0.25,size=1,pch=21,color='black',fill='white') + 
  #geom_sf()
  xlab('Latitude') +
  ylab('Day of of maximum sensitivity') +
  #geom_smooth(method='lm',color='red') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(0.15,0.5),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/max_sens_latitude.png')

print(max_sens_day_bivari)

dev.off()


#-------------------------------------------------------------------------------
# growth curves ------


#import SGS
#mean
growth_curve_absolute_mean_sgs <- 
  read_csv('./../../Data/CDD/growth_curves/growth_curve_absolute_shortgrass_steppe.csv')
head(growth_curve_absolute_mean_sgs,1)

#drought
growth_curve_drought_absolute_mean_sgs <- 
  read_csv('./../../Data/CDD/growth_curves/drought_growth_curve_absolute_shortgrass_steppe.csv')
head(growth_curve_drought_absolute_mean_sgs,1)

#growth dynamics
growth_drynamics_sgs <- 
  read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_shortgrass_steppe.csv')
head(growth_drynamics_sgs,1)


#import NMP
#mean
growth_curve_absolute_mean_nmp <- 
  read_csv('./../../Data/CDD/growth_curves/growth_curve_absolute_northern_mixed_prairies.csv')
head(growth_curve_absolute_mean_nmp,1)

#drought
growth_curve_drought_absolute_mean_nmp <- 
  read_csv('./../../Data/CDD/growth_curves/drought_growth_curve_absolute_northern_mixed_prairies.csv')
head(growth_curve_drought_absolute_mean_nmp,1)

#growth dynamics
growth_drynamics_nmp <- 
  read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_northern_mixed_prairies.csv')
head(growth_drynamics_nmp,1)

#SGS growth curve figure
png(height = 1500,width=3000,res=300,'./../../Figures/multi_panel_growth_curves')


par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,2,1,1),mar = c(3,3,3,3))

# plot it out: sgs
plot(gpp ~ day, growth_curve_absolute_mean_sgs,col='grey',pch=19,cex=0.1,
     ylab='',
     xlab='')
lines(gpp ~ day, growth_curve_drought_absolute_mean_sgs,col='red',pch=19,lwd=5)
lines(gpp ~ day, growth_curve_absolute_mean_sgs,col='grey',pch=19,lwd=5)
#abline(v=155)
text(160, 176, "June 28th",cex=1)
points(176, 176,pch=19,cex=3)
text(172, 97, "June 6th",cex=1)
points(157,102,pch=19,cex=3)
legend(175, 50, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
       col=c("grey", "red"), lty=1.1,lwd=4,cex=2,box.lty=0)
legend(175, 75, legend=c("50% of total production"),         #alpha legend: 0.015, 150
       col=c("black"), pch=19,box.lty=0,cex=2)
mtext('Julian day of year',side=1,line=3.0,cex=1.5)
mtext('Cumulative GPP',side=2,line=2.5,cex=1.5)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)


#dev.off()

#png(height = 1700,width=2000,res=300,'./../../Figures/growth_curves_absolute_northern_mixed_prairies.png')

# plot it out: nmp
plot(gpp ~ day, growth_curve_absolute_mean_nmp,col='grey',pch=19,
     ylab='',
     xlab='')
points(gpp ~ day, growth_curve_drought_absolute_mean_nmp,col='red',pch=19)
lines(gpp ~ day, growth_curve_drought_absolute_mean_nmp,col='red',pch=19,lwd=5)
lines(gpp ~ day, growth_curve_absolute_mean_nmp,col='grey',pch=19,lwd=5)
#abline(v=155)
text(158, 210, "June 23rd",cex=1)
points(174, 210,pch=19,cex=3)
text(180, 170, "June 12th",cex=1)
points(163,170,pch=19,cex=3)
# legend(200, 60, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
#        col=c("grey", "red"), lty=1.1,lwd=4,cex=1,box.lty=0)
# legend(200, 95, legend=c("50% of total production"),         #alpha legend: 0.015, 150
#        col=c("black"), pch=19,box.lty=0)
mtext('Julian day of year',side=1,line=3.0,cex=1.5)
#mtext('GPP',side=2,line=2.5,cex=1.5)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)

#dev.off()

#inset SGS
par(fig = c(0.05,0.30,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_sgs,cex=0.1,
     xlab='Julian day',ylab='Drought impact (% change in GPP)')
lines(perc_change~doy,data=growth_drynamics_sgs)
lines(upper~as.numeric(as.integer(doy)),growth_drynamics_sgs)
lines(lower~doy,growth_drynamics_sgs)
abline(h=0)
mtext('Julian day of year',side=1,line=2.35,cex=0.75)
mtext('GPP impact (%)',side=2,line=2.0,cex=0.75)

#inset NMP
par(fig = c(0.55,0.80,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_nmp,cex=0.1,
     xlab='Julian day',ylab='Drought impact (% change in GPP)')
lines(perc_change~doy,data=growth_drynamics_nmp)
lines(upper~as.numeric(as.integer(doy)),growth_drynamics_nmp)
lines(lower~doy,growth_drynamics_nmp)
abline(h=0)
mtext('Julian day of year',side=1,line=2.25,cex=0.75)
mtext('GPP impact (%)',side=2,line=2.35,cex=0.75)


dev.off()

#stopped here



#174-11
#-------------------------------------------------------------------------------
# maximum temperature change through time ------

#import SGS
max_temp_sgs <- import_temp(Ecoregion='shortgrass_steppe',temp='tmax',value=T)
head(max_temp_sgs)

#make year numeric
max_temp_sgs$year <- as.numeric(as.character(max_temp_sgs$year))

#get per-pixel slope (change in max temp/year)
temp_slope_sgs <- max_temp_sgs %>%
  group_by(x,y) %>%
  dplyr::do(model = lm(temp~year, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

temp_slope_sgs <- data.frame(temp_slope_sgs[c(1,2,4)])
summary(temp_slope_sgs)
#Median : 0.04397  

#take a look
head(temp_slope_sgs)
plot(rasterFromXYZ(temp_slope_sgs))

#import NMP
max_temp_nmp <- import_temp(Ecoregion='northern_mixed_prairies',temp='tmax',value=T)
head(max_temp_nmp)

#make year numeric
max_temp_nmp$year <- as.numeric(as.character(max_temp_nmp$year))

temp_slope_nmp <- max_temp_nmp %>%
  group_by(x,y) %>%
  dplyr::do(model = lm(temp~year, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

temp_slope_nmp <- data.frame(temp_slope_nmp[c(1,2,4)])

summary(temp_slope_nmp)
# Median :-0.001681

#take a look
head(temp_slope_nmp)
plot(rasterFromXYZ(temp_slope_nmp))


#bind them
rbind_temp <- rbind(temp_slope_nmp,temp_slope_sgs)

#combine for mapping
temp_slope_both <- raster::merge(rasterFromXYZ(temp_slope_nmp),rasterFromXYZ(temp_slope_sgs),tolerance=0.2)
crs(temp_slope_both) <- Albers
plot(temp_slope_both)
temp_slope_both_df <- data.frame(rasterToPoints(temp_slope_both))
# library(mapproj)
# library(ggalt)

max_temp_trend <- ggplot() + 
  geom_raster(data=temp_slope_both_df, aes(x = x, y = y, fill = layer)) + 
  # coord_cartesian()
  # coord_map("albers",lat0=32.5343, lat=142.0095) +
  # ggalt::coord_proj() +
  coord_equal() +
  #geom_sf()
  scale_fill_scico('Change in average maximum \n temperature (degrees/year)',
                   palette = 'roma',direction=-1,midpoint=0) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.25,0.3),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#save to file
png(height = 1500,width=2000,res=300,'./../../Figures/maximum_temperature_trend.png')

print(max_temp_trend)

dev.off()

#-------------------------------------------------------------------------------
# overlap of max sens and % growth ------

#import SGS and turn to raster
sens_growth_sgs <-
  read.csv(
    './../../Data/CDD/growth_sensitivity_overlap/growth_sens_overlap_shortgrass_steppe.csv'
  )
head(sens_growth_sgs, 2)
summary(sens_growth_sgs)
sens_growth_sgs_raster <-
  rasterFromXYZ(sens_growth_sgs[c(1, 2, 5)]) #%GPP at beginning of period of max sens
crs(sens_growth_sgs_raster) <- aea.proj
plot(sens_growth_sgs_raster)

#add ID to dataframe
sens_growth_sgs$region <- 'Shortgrass steppe'

#import NMP and turn to raster
sens_growth_nmp <-
  read.csv(
    './../../Data/CDD/growth_sensitivity_overlap/growth_sens_overlap_northern_mixed_prairies.csv'
  )
head(sens_growth_nmp, 2)
summary(sens_growth_nmp)
sens_growth_nmp_raster <-
  rasterFromXYZ(sens_growth_nmp[c(1, 2, 5)]) #%GPP at beginning of period of max sens
crs(sens_growth_nmp_raster) <- aea.proj
plot(sens_growth_nmp_raster)

#add ID to dataframe
sens_growth_nmp$region <- 'Northern mixed prairies'

sens_growth_sgs_nmp <- rbind(sens_growth_nmp,sens_growth_sgs)
head(sens_growth_sgs_nmp)

#boxplot
boxplot_sens_growth <- ggplot(sens_growth_sgs_nmp,aes(x=region,y=gpp_begin,color=gpp_begin)) +
  scale_color_scico('% GPP',palette = 'batlow',direction=-1) +
  geom_jitter(size=.5,width = 0.25,height=0.2,alpha=0.5) +
  geom_violin(width=1) +
  geom_boxplot(width=.15) +
  annotate("text", x=1, y=55, label= "57%") +
  annotate("text", x=2, y=37, label= "39%") +
  ylab('% Growth at start of period of maximum sensitivity') +
  xlab('') +
  # scale_x_discrete(labels=c("cropland" = "Cropland", "forest" = "Forest",
  #                           "grassland" = "Grassland",'shrubland'='Shrubland',
  #                           'tundra'='Tundra')) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.08,0.17),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 1500,width=2000,res=300,'./../../Figures/boxplot_sens_growth_overlap.png')

print(boxplot_sens_growth)

dev.off()

#map it out

sens_growth_sgs_merge <- raster::merge(sens_growth_nmp_raster,sens_growth_sgs_raster,tolerance = 0.1)
sens_growth_sgs_merge <- data.frame(rasterToPoints(sens_growth_sgs_merge))
head(sens_growth_sgs_merge,1)
summary(sens_growth_sgs_merge)

#get states
states <- map_data("state", "Colorado") %>% 
  select(lon = long, lat, group, id = region)
head(mi_counties)

sens_growth_overlap_map <- ggplot(sens_growth_sgs_merge, aes(x = x, y = y, fill = layer)) + 
  geom_raster() + 
  coord_equal() +
  #geom_sf()
  scale_fill_scico('% GPP',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.35,0.3),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1500,width=2000,res=300,'./../../Figures/begin_max_sens_perc_growth.png')

print(sens_growth_overlap_map)

dev.off()

#try with points
ggplot(data = states,aes(lon,lat)) + geom_polygon(fill=NA,color='black') + 
  geom_point(data= sens_growth_sgs_merge,aes(x=x, y=y, color = layer),size=0.1) +
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "white",
                        high = "red", space = "Lab" ) +
  ggtitle('Change in GSL 2001-2016 (days/year)') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    #legend.text = element_text(size=2.25),
    #legend.position = c(0.7,0.1),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = 'bottom',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))



#-------------------------------------------------------------------------------
# VPD change during drought by season ------

#import
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- read.csv(paste0(
     './../../Data/Climate/Ecoregion/',
     Ecoregion,
     '/PRISM/VPD_change.csv'
   )) 

Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 

seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
head(seasonal_vpd_sgs_nmp,1)

vpd_change <- ggplot(seasonal_vpd_sgs_nmp, aes(x = abs_change, fill = season)) +
  facet_wrap(~ecoregion,scales='free') +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  #geom_histogram()
  scale_fill_manual(values = c(
    'spring' = 'black',
    'summer' = 'white'
  )) +
  xlab('Increase in maximum VPD') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.4, 0.75),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2500,res=300,'./../../Figures/vpd_change.png')

print(vpd_change)

dev.off()

#-------------------------------------------------------------------------------
#gif of sensitivity -----


g <- ggplot(data = pdsi_df, aes(x = x, y = y, fill = PDSI))  +
  geom_tile() +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = states, fill = "transparent", color = "grey45", inherit.aes = FALSE) +
  geom_sf(data = borders, fill = "transparent", color = "grey33", inherit.aes = FALSE) +
  ggthemes::theme_map() +
  theme(legend.key.width = unit(.70,"cm"), legend.position = c(.10, .10))


g
#-------------------------------------------------------------------------------
# toy data -----


response_3 <- c(51,42,35,20,15,10,8)
predictor_3 <- c(5,10,15,20,25,30,35)
toy_bivar_2 <- data.frame(predictor_3,response_3)


#continuous variable 2
predictor <- c(1,6,15,21,25,30,49,55)
response <- c(8,10,7,11,5,10,60,8)
toy_bivar <- data.frame(predictor,response)


#continuous variable 3
predictor_4 <- 
  c('early 20th','mid 20th','late 20th','2000','early 21st','mid 21st','late 21st','2100')
response_4 <- c(450,449,425,415,412,400,399, 350)
toy_bivar_4 <- data.frame(predictor_4,response_4)


#barplot
predictor_2 <- c('Control','Fertilizer','Irrigation','Fertilizer + Irrigation')
response_2 <- c(8900,9000,9500,9750)
toy_bar <- data.frame(predictor_2,response_2)
str(toy_bar)

#barchart
bar_figure <- ggplot(toy_bar , aes(x = reorder(predictor_2,response_2), y=response_2)) +
  #scale_y_continuous(expand = c(8000,8000),limits = c(8000,10000)) + #expand = c(8000,8000),
  geom_segment(aes(xend = predictor_2, yend = 0), size = 12,color='black',lineend = "butt") +
  #geom_segment(aes(xend = predictor_2, yend = 8800), size = 12,color='black',lineend = "butt") +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  # geom_segment()
  # stat_summary(fun = 'mean',geom = 'segment') +
  # stat_smooth(method='lm',se=F)
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  # scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  # geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  # scale_fill_manual(values = c(
  #   'Northern mixed prairies' = 'black',
  #   'Shortgrass steppe' = 'white'
  # )) +
  xlab("") +
  ylab('Grain yield (kg per hectare)') +
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )


#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/barchart_fixed.png')

print(bar_figure)

dev.off()

#trend figure

trend_figure_1 <- ggplot(toy_bivar_2 , aes(x = predictor_3, y=response_3)) +
  #scale_y_continuous(expand = c(8000,8000),limits = c(8000,10000)) + #expand = c(8000,8000),
  #geom_segment(aes(xend = predictor_2, yend = 0), size = 12,color='black',lineend = "butt") +
  #geom_segment(aes(xend = predictor_2, yend = 8800), size = 12,color='black',lineend = "butt") +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  geom_point(size=5) +
  # stat_summary(fun = 'mean',geom = 'segment') +
  # stat_smooth(method='lm',se=F)
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  # scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  # geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  # scale_fill_manual(values = c(
  #   'Northern mixed prairies' = 'black',
  #   'Shortgrass steppe' = 'white'
# )) +
  xlab("Grazing intensity") +
  ylab('Rangeland carbon uptake') +
  #ylab(bquote('Net photosynthesis ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/bivar_1_bad.png')

print(trend_figure_1)

dev.off()

#spurious correlation
library(palmerpenguins)
penguin_df<- 
  palmerpenguins::penguins %>%
  na.omit()

head(penguin_df)
trend_figure_4 <- ggplot(penguin_df, aes(x = bill_length_mm, y=bill_depth_mm,fill=species)) +
  geom_point(size=5,pch=21) +
  scale_fill_manual(values=c('Adelie'='grey70','Chinstrap'='white',
                             'Gentoo'='red'),
                    labels=c('Adelie'='Species 1','Chinstrap'='Species 2',
                             'Gentoo'='Species 3')) +
  xlab("Grazing intensity (number of livestock per hectare)") +
  ylab(bquote('Net photosynthesis ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  # geom_smooth(data=penguin_df,mapping=aes(x = bill_length_mm, y=bill_depth_mm,group=2),fullrange=T,
  #             method='lm',color='black',se=T,legend=F) +
  geom_smooth(method='lm',se=F,color='black') +
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.12, 0.12),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/bivar_4_fixed.png')

print(trend_figure_4)

dev.off()


#bad example end of lecture problem set
bar_figure_4 <- ggplot(toy_bivar_4 , aes(x = reorder(predictor_4,response_4), y=response_4)) +
  #scale_y_continuous(expand = c(8000,8000),limits = c(8000,10000)) + #expand = c(8000,8000),
  geom_segment(aes(xend = predictor_4, yend = 0), size = 12,color='black',lineend = "butt") +
  xlab("") +
  ylab('Annual rainfall') +
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )


#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/bivar_4_bad.png')

print(bar_figure_4)

dev.off()

#using lines instead of points
head(penguin_df)
trend_figure_5 <- ggplot(penguin_df, aes(x = bill_length_mm, y=body_mass_g)) +
  #geom_line(size=1,pch=21) +
  geom_point(size=5,pch=21) +
  #geom_smooth(method = 'lm',se=F,color='red') +
  ylab('Happiness (number of smiles)') +
  xlab('Number of days skiing at Bridger Bowl') +
  # geom_smooth(data=penguin_df,mapping=aes(x = bill_length_mm, y=bill_depth_mm,group=2),fullrange=T,
  #             method='lm',color='black',se=T,legend=F) +
  #geom_smooth(method='lm',se=F,color='black') +
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.12, 0.12),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/bivar_5_bad_lines_fixed_2.png')

print(trend_figure_5)

dev.off()

#linear fit to nonlinear data
#continuous variable 2
predictor_5 <- c(1,6,15,21,25,27,30,35,40,45,50,60,75)
response_5 <- c(0,10,15,22,25,30,33,35,34,35,35,35,35)
toy_bivar_5 <- data.frame(predictor_5,response_5)


trend_figure_6 <- ggplot(toy_bivar_5, aes(x = predictor_5, y=response_5)) +
  geom_point(size=5) +
  ylab(bquote('Net photosynthesis ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  xlab('Soil moisture to 20 cm depths (%)') +
  # geom_smooth(data=penguin_df,mapping=aes(x = bill_length_mm, y=bill_depth_mm,group=2),fullrange=T,
  #             method='lm',color='black',se=T,legend=F) +
  geom_smooth(se=F,color='red',method='lm', formula=y~poly(x,2)) +
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.12, 0.12),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/bivar_6_poly_trendline.png')

print(trend_figure_6)

dev.off()



