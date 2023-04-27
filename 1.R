library(readr)
global_power_plant_database <- read_csv("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/1/global_power_plant_database_v_1_3/global_power_plant_database.csv")

library(dplyr)
indonesia_powerplants <- filter(global_power_plant_database, country_long == "Indonesia")

library(leaflet)
library(magrittr)

indonesia_map <- leaflet() %>% 
  setView(lng = 118.0149, lat = -2.5489, zoom = 6) %>% 
  addTiles() %>% 
  addMarkers(data = indonesia_powerplants, 
             lng = ~longitude, lat = ~latitude, 
             popup = ~paste("Power Plant Name: ", name, "<br>",
                            "Location: ", geolocation_source, ", ", country_long))

indonesia_map

library(leaflet.extras)

indonesia_map <- leaflet() %>% 
  setView(lng = 118.0149, lat = -2.5489, zoom = 6) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(data = indonesia_powerplants, 
                   lng = ~longitude, lat = ~latitude, 
                   radius = 2,
                   color = ~ifelse(primary_fuel == "Coal", "red",
                                   ifelse(primary_fuel == "Gas", "green",
                                          ifelse(primary_fuel == "Hydro", "blue",
                                                 ifelse(primary_fuel == "Oil", "orange", "purple")))),
                   popup = ~paste("Power Plant Name: ", name, "<br>",
                                  "Location: ", geolocation_source, ", ", country_long)) %>% 
  addLegend(position = "topright", 
            colors = c("red", "green", "blue", "orange", "purple"),
            labels = c("Coal", "Gas", "Hydro", "Oil", "Others"),
            title = "Primary Fuel Type")


indonesia_map

indonesia_powerplants$primary_fuel <- as.character(indonesia_powerplants$primary_fuel)
indonesia_powerplants$primary_fuel[indonesia_powerplants$primary_fuel == "Others"] <- "Geothermal"

indonesia_map <- leaflet() %>% 
  setView(lng = 118.0149, lat = -2.5489, zoom = 6) %>% 
  addTiles() %>% 
  addCircleMarkers(data = indonesia_powerplants, 
                   lng = ~longitude, lat = ~latitude, 
                   radius = 8,
                   color = ~ifelse(primary_fuel == "Coal", "red",
                                   ifelse(primary_fuel == "Gas", "green",
                                          ifelse(primary_fuel == "Hydro", "blue",
                                                 ifelse(primary_fuel == "Oil", "orange", "purple")))),
                   fillOpacity = 0.2, 
                   popup = ~paste("Power Plant Name: ", name, "<br>",
                                  "Location: ", geolocation_source, ", ", country_long)) %>%
  addLegend(position = "topright",
            colors = c("red", "green", "blue", "orange", "purple"),
            labels = c("Coal", "Gas", "Hydro", "Oil", "Geothermal"),
            title = "Plant Fuel Type")

indonesia_map

library(readr)
BENV0093_2ndAssignment_powerplants <- read_csv("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/1/BENV0093_2ndAssignment_powerplants.csv")

library(dplyr)

newpowerplant <- filter(BENV0093_2ndAssignment_powerplants, status %in% c("construction", "planned"))

write.csv(newpowerplant, file = "newpowerplant.csv", row.names = FALSE)


library(leaflet)

mymap <- leaflet() %>%
  addTiles() 

mymap <- mymap %>%
  addCircleMarkers(data = newpowerplant[newpowerplant$status %in% c("construction", "planned"),], 
                   lat = ~latitude, 
                   lng = ~longitude, 
                   radius = 8, 
                   color = ~ifelse(status == "construction", "red", "orange"), 
                   fillOpacity = 0.2)

mymap <- mymap %>%
  addLegend("topright",
            title = "Plant Status",
            colors = c("red", "orange"),
            labels = c("Construction", "Planned"))

mymap


library(mapview)

solar_radiation_indonesia <- brick("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/data1nc/data1.nc")

mapview(solar_radiation_indonesia)

library(ncdf4)
library(raster)
library(leaflet)
library(viridis)

ncfile <- nc_open("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/data1nc/data1.nc")

ncfile




###############################Solar radiance###############################

library(ncdf4)
era <- nc_open("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/data1nc/data1.nc")

#get Dimension :lon, lat, time
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")

tunits <- ncatt_get(era,"time","units")

library(chron)

#convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-") 
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

chron(time/24, origin=c(tmonth, tday, tyear) )

#get Variables :u10,v10, ssrd. similar to get dimension data, we are going to use the same method, ncvar_get()
ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards

#Get a single time slice of the data using ssrd_array
ssrd_slice <- ssrd_array[,,2] 

#Step 4#
library(lattice)
library(RColorBrewer)

#Plot the data using 'image'
image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )

#Step 5#
lonlat <- as.matrix(expand.grid(lon, lat))

ssrd_vec <- as.vector(ssrd_slice) 
ssrd_df <- data.frame(cbind(lonlat, ssrd_vec))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit(ssrd_df)

#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf(ssrd_df_value, coords = c("lon", "lat"))
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")


#################################Solar power##############################


ncatt_get(era,"ssrd","units")

radiation_to_power <- function(G, A=1, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "GnBu")


###########################Interpolation steps############################

library(gstat) #geostatistics used for interpolation. More details can be found [https://cran.r-project.org/web/packages/gstat/gstat.pdf] 
#https://www.gstat.org/gstat.pdf see Section 2.3
library(rgdal)
library(tmap)
library(terra)


indonesia = st_read("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/idn_admbnda_adm0_bps_20200401.shp")

ssrd_sf = st_transform(ssrd_sf, 4326)
indonesia = st_transform(indonesia, st_crs(ssrd_sf))

coor = as.data.frame(st_coordinates(ssrd_sf))
ssrd_sf$x = coor$X
ssrd_sf$y = coor$Y
ssrd_nogeom = st_drop_geometry(ssrd_sf) #get rid of geometry but keep all other attributes
ssrd_nogeom=na.omit(ssrd_nogeom)

gs <- gstat(formula=ssrd~1, locations=~x+y, data=ssrd_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs

indonesia = st_read("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/idn_admbnda_adm0_bps_20200401.shp")

st_bbox(indonesia)          

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template

#########################Interpolation results##############################

idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)

idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)

names(idw_mask) = c( "predicted","observed" )

#########################Interpolation visualisation#########################

tmap_mode("view")

tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "YlOrRd", legend.show = TRUE)


##########################Protected /sensitive natural areas################

library(tmap)
library(sf)


tmap_mode("view")


protected_areas <- st_read("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/Protected planet.shp.gpkg")


tm_shape(protected_areas) +
  tm_polygons("lightseagreen", border.col = "lightseagreen") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

####################################Road###################################

library(tmap)
library(sf)

# Read in the road data
road <- st_read("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/IDN_rds/IDN_roads.shp")

tmap_mode("view")

road_map <- tm_shape(road) +
  tm_lines(lwd = 0.5, palette = "Blues") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)



###############################Grid power grid###############################


library(tmap)
library(sf)


grid <- st_read("C:/Users/Eric Zhang/Desktop/Spatial Analysis of Energy Data/Assignment 2/Spatial coursework/grid.geojson")


grid <- st_transform(grid, crs = "+proj=longlat +datum=WGS84")


tm_shape(grid) +
  tm_lines(lwd = 1, col = "royalblue") +
  tm_layout(title = "Indonesia Power Grid Map")


###############################Combination###################################

library(tmap)


layer1 <- tm_shape(ssrd_sf) +
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")

layer2 <- tm_shape(protected_areas) +
  tm_polygons("lightseagreen", border.col = "lightseagreen") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

layer3 <- tm_shape(road) +
  tm_lines(lwd = 0.5, palette = "Blues") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)

layer4 <- tm_shape(grid) +
  tm_lines(lwd = 1, col = "royalblue") +
  tm_layout(title = "	Multi-criteria visualisation ")

layer5 <- tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "YlOrRd", legend.show = TRUE)

tm <- layer1 + layer2 + layer3 + layer4 + layer5

tmap_mode("view")

tm
##################################Plot region###############################

points_df <- data.frame(
  lon = c(109.189265, 97.209179, 115.780299, 118.820757, 110.573124),
  lat = c(0.084836, 5.104261, -3.624469, -3.137592, -6.871590)
)
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)


tm <- tm_shape(ssrd_sf) +
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd") +
  tm_shape(protected_areas) +
  tm_polygons("lightseagreen", border.col = "lightseagreen") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE) +
  tm_shape(road) +
  tm_lines(lwd = 0.5, palette = "Blues") +
  tm_layout(legend.outside.position = "bottom", legend.outside = TRUE) +
  tm_shape(grid) +
  tm_lines(lwd = 1, col = "royalblue") +
  tm_layout(title = "Multi-criteria visualisation") +
  tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "YlOrRd", legend.show = TRUE) +
  tm_shape(points_sf) +
  tm_symbols(size = 1, shape = 21, col = "lightskyblue", alpha = 0.8)

tmap_mode("view")
tm

#################################AHP#######################################



install.packages("AHPhybrid")
library(AHPhybrid)

judgment.matrix <- matrix(c(
  0.48, 0.24, 0.12, 0.16,
  0.52, 0.26, 0.09, 0.13,
  0.47, 0.35, 0.12, 0.06,
  0.375, 0.25, 0.25, 0.125
), nrow = 4)


if (!is.matrix(judgment.matrix) || nrow(judgment.matrix) != ncol(judgment.matrix)) {
  stop("The judgment matrix is not a square matrix.")
}
if (any(judgment.matrix <= 0)) {
  stop("All elements in the judgment matrix must be positive.")
}
if (any(colSums(judgment.matrix) != 1)) {
  stop("The sum of each column in the judgment matrix must be 1.")
}


normalized.matrix <- normalize.judgment.matrix(judgment.matrix, byrow = FALSE)


weights <- matrix(0, nrow = ncol(normalized.matrix), ncol = 1)
new_weights <- c(0.4658194, 0.2771405, 0.0959699, 0.1610702)

weights[,1] <- colMeans(normalized.matrix)


print(weights)
##################################Plot AHP#################################


library(ggplot2)

judgment.matrix <- matrix(c(
  1, 2, 4, 3,
  1/2, 1, 3, 2,
  1/4, 1/3, 1, 2,
  1/3, 1/2, 1/2, 1
), nrow = 4)


df <- data.frame(criteria = c("Criterion 1", "Criterion 2", "Criterion 3", "Criterion 4"),
                 weight = weights[, 1])


ggplot(df, aes(x = criteria, y = weight)) + 
  geom_bar(stat = "identity") +
  ggtitle("Criteria Weights") +
  xlab("Criteria") +
  ylab("Weight") +
  theme_bw()


df_new <- data.frame(criteria = c("Solar radiation", "Protected /sensitive natural areas", "Road networks", "Grid line"),
                     weight = new_weights)


ggplot(df_new, aes(x = criteria, y = weight)) + 
  geom_bar(stat = "identity") +
  ggtitle("Criteria Weights") +
  xlab("Criteria") +
  ylab("Weight") +
  theme_bw()

##############################AHP normal#####################################

judgment.matrix <- matrix(c(
  1, 2, 4, 3,
  1/2, 1, 3, 2,
  1/4, 1/3, 1, 1/2,
  1/3, 1/2, 2, 1
), nrow = 4)


normalized.matrix <- judgment.matrix / rowSums(judgment.matrix)


weights <- colMeans(normalized.matrix)


weights


n <- nrow(judgment.matrix)
ri.values <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
lambda.max <- Re(eigen(judgment.matrix)$values[1])
ci <- (lambda.max - n) / (n - 1)
cr <- ci / ri.values[n]
if (cr < 0.1) {
  cat("Consistency check passed (CR =", round(cr, 2), ")")
} else {
  cat("Consistency check failed (CR =", round(cr, 2), ")")
}


judgment.matrix

######################################NPV#################################

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) 
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1)
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}

npv= calc_NPV(annual_revenue = 7146057600,lifetime_yrs=25, CAPEX=38392200000 )
ifelse(npv>0, "Support","obeject" )

################################LCOE####################################
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.08, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 33000000 
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 