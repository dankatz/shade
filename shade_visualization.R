#vignette on visualizing and calculating shade in Austin based on LiDAR

library(lidR)
library(sf)
library(rayshader)
library(raster)
library(insol)
library(dplyr)
library(magick)

### animate shadows on a small AOI in Hyde Park on July 22, 2020 ###############################################

#working in WGS84, UMT 14N: CRS('+init=EPSG:32614')

### load in some imagery
#download an image from nearmap
#near <- stack("C:/Users/dsk856/Desktop/TX_remote_sensing/nearmap/tif1.tif") #hi res version
near <- stack("C:/Users/dsk856/Desktop/TX_remote_sensing/nearmap/hyde_park/small_summer/EPSG32614_Date20200509_Lat30.303155_Lon-97.729399_Mpp0.075 (1)/EPSG32614_Date20200509_Lat30.303155_Lon-97.729399_Mpp0.075.jpg")
plotRGB(near)
crs(near) <-CRS('+init=EPSG:32614')
extent_near <- extent(near)
near_res <- 0.075

#near2 <- projectRaster(from = near, to = chm_medium_rast_s)
#plotRGB(near2)
#?projectRaster


### load in LiDAR file and create a nSDM
#lidar downloaded from TINRIS, https://data.tnris.org/collection/0549d3ba-3f72-4710-b26c-28c65df9c70d
#name of tile with most of UT Austin
las <- readLAS("C:/Users/dsk856/Desktop/TX_remote_sensing/stratmap17-50cm_3097433a2.laz")
#las
#plot(las)

# trying it out for a small area
las_small <- lasclipRectangle(las, xleft = extent_near@xmin, xright = extent_near@xmax,
                                  ybottom = extent_near@ymin, ytop = extent_near@ymax) #center on main campus
plot(las_small)

#set the projection manually
crs <- sp::CRS("+init=epsg:6343") 
projection(las_small) <- crs

#create an nSDM
chm_small = grid_canopy(las_small, 0.5, pitfree(c(0, 2, 5, 10, 15), max_edge = c(0,1), subcircle = 0.3))
plot(chm_small)
chm_small_rast = chm_small#as.raster(chm_small)
kernel = matrix(1,3,3)
chm_small_rast_s = raster::focal(chm_small_rast, w = kernel, fun = median)
chm_small_rast_s = raster::focal(chm_small_rast_s, w = kernel, fun = median)
plot(chm_small_rast_s)

chm_small_rast_s <- projectRaster(from = chm_small_rast_s, to = near)
chm_small_rast_s <- chm_small_rast_s - min(chm_small_rast_s[], na.rm = TRUE)

# #trying it out for the whole tile
# chm = grid_canopy(las, 0.5, pitfree(c(0, 2, 5, 10, 15), max_edge = c(0,1), subcircle = 0.3))
# hist(chm)
# plot(chm)
# crs(chm) <- CRS('+init=EPSG:6343')
# chm_rast = as.raster(chm)
# chm_rast_s = raster::focal(chm_rast, w = kernel, fun = median)
# chm_rast_s = raster::focal(chm_rast_s, w = kernel, fun = median)
# ?raster::focal




### get image and nSDM to same extent
# near
# chm_small_rast_s
# plot(near)
# plot(chm_small_rast_s)

names(near) = c("r","g","b")

near_r = rayshader::raster_to_matrix(near$r)
near_g = rayshader::raster_to_matrix(near$g)
near_b = rayshader::raster_to_matrix(near$b)

nSDM_matrix = rayshader::raster_to_matrix(chm_small_rast_s)

near_rgb_array = array(0,dim=c(nrow(near_r),ncol(near_r),3))

near_rgb_array[,,1] = near_r/255 #Red layer
near_rgb_array[,,2] = near_g/255 #Blue layer
near_rgb_array[,,3] = near_b/255 #Green layer

near_rgb_array = aperm(near_rgb_array, c(2,1,3))

plot_map(near_rgb_array)



  
ambmat <- ambient_shade(nSDM_matrix, zscale = near_res)

sp <- seq(ISOdate(2020,7,22,0), ISOdate(2020,7,22,24), by="min") %>% 
  JD() %>%
  sunvector(30,-97.7,-5) %>% # lat, long, time zone in Austin
  sunpos() %>% 
  as.data.frame() %>%
  mutate(id = 1:nrow(.),
         zenith = 90-zenith) %>%
  filter(zenith >= -10, # exclue nighttime
         id %% 10 == 0) # keep every third frame

# open magick graphics device
img <- image_graph(width=900, height=720)

# generate hillshade image for each solar position
for(i in 1:nrow(sp)){
  azimuth <- sp$azimuth[i] #azimuth <- sp$azimuth[10] 
  zenith <- sp$zenith[i] #zenith <- sp$zenith[10] 
  raymat <- ray_shade(nSDM_matrix, zscale = near_res, #lambert = TRUE, sunaltitude=20, 
                      sunaltitude = zenith, sunangle = azimuth) #?ray_shade
  nSDM_matrix %>%
    sphere_shade(texture = "imhof4") %>%
    add_overlay(near_rgb_array, alphalayer = 0.9) %>%
    add_shadow(raymat, max_darken = 0.5) %>%
    add_shadow(ambmat, max_darken = 0.5) %>% 
    plot_map()
}

# save animation
dev.off()
img %>% image_animate(fps = 20) %>% image_write("C:/Users/dsk856/Documents/shade/shadows_hyde_july22.gif")





### animate shadows on a medium AOI in Hyde Park on July 22, 2020 ###############################################

### load in some imagery
#download an image from nearmap
near_med <- stack("C:/Users/dsk856/Desktop/TX_remote_sensing/nearmap/hyde_park/medium_summer/EPSG32614_Date20200509_Lat30.303233_Lon-97.729544_Mpp0.299.jpg")
plotRGB(near_med)
crs(near_med) <-CRS('+init=EPSG:32614')
extent_near_med <- extent(near_med)
near_med_res <- 0.299


### load in LiDAR file and create a nSDM
#lidar downloaded from TINRIS, https://data.tnris.org/collection/0549d3ba-3f72-4710-b26c-28c65df9c70d
#name of tile with most of UT Austin
las <- readLAS("C:/Users/dsk856/Desktop/TX_remote_sensing/stratmap17-50cm_3097433a2.laz")
#las
#plot(las)

# trying it out for a small area
las_medium <- lasclipRectangle(las, xleft = extent_near_med@xmin, xright = extent_near_med@xmax,
                              ybottom = extent_near_med@ymin, ytop = extent_near_med@ymax) #center on main campus
plot(las_medium)

#set the projection manually
crs <- sp::CRS("+init=epsg:6343") 
projection(las_medium) <- crs

#create an nSDM
chm_medium = grid_canopy(las_medium, 0.5, pitfree(c(0, 2, 5, 10, 15), max_edge = c(0,1), subcircle = 0.3))
plot(chm_medium)
chm_medium_rast = chm_medium#as.raster(chm_medium)
kernel = matrix(1,3,3)
chm_medium_rast_s = raster::focal(chm_medium_rast, w = kernel, fun = median)
chm_medium_rast_s = raster::focal(chm_medium_rast_s, w = kernel, fun = median)
plot(chm_medium_rast_s)

chm_medium_rast_s <- projectRaster(from = chm_medium_rast_s, to = near_med)
chm_medium_rast_s <- chm_medium_rast_s - min(chm_medium_rast_s[], na.rm = TRUE)


### get image and nSDM to same extent
# near_med
# chm_medium_rast_s
# plot(near_med)
# plot(chm_medium_rast_s)
names(near_med) = c("r","g","b")

near_med_r = rayshader::raster_to_matrix(near_med$r)
near_med_g = rayshader::raster_to_matrix(near_med$g)
near_med_b = rayshader::raster_to_matrix(near_med$b)

nSDM_matrix = rayshader::raster_to_matrix(chm_medium_rast_s)

near_med_rgb_array = array(0,dim=c(nrow(near_med_r),ncol(near_med_r),3))

near_med_rgb_array[,,1] = near_med_r/255 #Red layer
near_med_rgb_array[,,2] = near_med_g/255 #Blue layer
near_med_rgb_array[,,3] = near_med_b/255 #Green layer

near_med_rgb_array = aperm(near_med_rgb_array, c(2,1,3))

plot_map(near_med_rgb_array)


ambmat <- ambient_shade(nSDM_matrix, zscale = near_med_res)

sp <- seq(ISOdate(2020,7,22,0), ISOdate(2020,7,22,24), by="min") %>% 
  JD() %>%
  sunvector(30,-97.7,-5) %>% # lat, long, time zone in Austin
  sunpos() %>% 
  as.data.frame() %>%
  mutate(id = 1:nrow(.),
         zenith = 90-zenith) %>%
  filter(zenith >= -10, # exclue nighttime
         id %% 50 == 0) # keep every X frames

# open magick graphics device
img <- image_graph(width=900, height=720)

# generate hillshade image for each solar position
for(i in 1:nrow(sp)){
  azimuth <- sp$azimuth[i] #azimuth <- sp$azimuth[10] 
  zenith <- sp$zenith[i] #zenith <- sp$zenith[10] 
  raymat <- ray_shade(nSDM_matrix, zscale = near_med_res, #lambert = TRUE, sunaltitude=20, 
                      sunaltitude = zenith, sunangle = azimuth) #?ray_shade
  nSDM_matrix %>%
    sphere_shade(texture = "imhof4") %>%
    add_overlay(near_med_rgb_array, alphalayer = 0.9) %>%
    add_shadow(raymat, max_darken = 0.5) %>%
    add_shadow(ambmat, max_darken = 0.5) %>% 
    plot_map()
}

# save animation
dev.off()
img %>% image_animate(fps = 5) %>% image_write("C:/Users/dsk856/Documents/shade/shadows_hyde_medium_july22.gif")





  
### 3D version ###########################################################
### 3D rendering of some trees
# plot_3d(near_rgb_array, nSDM_matrix, windowsize = c(1100,900) , zscale = 0.597, shadowdepth = -50
#         #zoom=1, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B"
#         )
# render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
#                 title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
# 



  nSDM_matrix %>%
    sphere_shade(texture = "imhof4") %>%
    # add_overlay(near_rgb_array, alphalayer = 0.9) %>%
    # add_shadow(raymat, max_darken = 0.5) %>%
     add_shadow(ambmat, max_darken = 0.2) %>% 
    # add_shadow(ray_shade(nSDM_matrix, sunaltitude=20, #anglebreaks=seq(zenith-4, zenith+4, 1),
    #                     sunangle = azimuth, zscale=0.07), max_darken=0.1)  %>% 
   rayshader::plot_3d(hillshade = near_rgb_array, heightmap = nSDM_matrix, solid = FALSE,
                    windowsize = c(1100,900), zscale = 0.59, shadowdepth = -50,  baseshape = "rectangle",
            zoom=1, phi=90,theta=0,fov=90, background = "#F2E1D0", shadowcolor = "#523E2B")
  
  
  