setwd('/media/6TB/rilquer/lgeo_plat/')

require(terra)
require(tidyverse)
require(sf)
require(flexsdm)
require(pastclim)
require(raster)
needs::prioritize(dplyr)
require(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(returnclass = "sf")
theme_set(theme_bw()) #Setting theme

# Occurrence data####
## GBIF download ####
# Code below run only once, to request and download data for all species in the genus Latrodectus
require(rgbif)
d_gbif=occ_download(
  type="and",
  pred("taxonKey", 2157920),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN"))),
  pred_or(  
    pred_lt("coordinateUncertaintyInMeters",10000),
    pred_isnull("coordinateUncertaintyInMeters")
  ),
  format = "SIMPLE_CSV"
)
occ_download_wait('0008802-260221153910048')
d <- occ_download_get('0008802-260221153910048') %>%
  occ_download_import()

lat_occ <- lat_occ %>% filter(taxonRank == 'SPECIES') %>% filter(species != '')
# Correct L. hasselti typos
lat_occ$species <- gsub('hasseltii','hasselti',lat_occ$species)
write_csv(lat_occ,'data/latrodecus_GBIF_2157920.csv')

## A map of L. geometricus invasion####
# This is mostl for illustrative purposes and is done before coordinate cleaning
require(ggplot2)
require(ggspatial)
require(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw()) #Setting theme

lgeo_occ <- read_csv('data/occurrences/latrodectus_GBIF_2157920.csv') %>% 
  filter(species == 'Latrodectus geometricus') %>% 
  filter(basisOfRecord != 'HUMAN_OBSERVATION') %>% 
  select(decimalLongitude,decimalLatitude,year) %>% drop_na()

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = lgeo_occ %>% filter(year <= 1920), aes(x = decimalLongitude,y = decimalLatitude),
             size = 2,alpha=0.8,color='brown')+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('output/lgeo_maps/lgeo_upto_1920.png',width = 5, height = 2.5,dpi = 600)

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = lgeo_occ %>% filter(year <= 1950), aes(x = decimalLongitude,y = decimalLatitude),
             size = 2,alpha=0.8,color='brown')+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('output/lgeo_maps/lgeo_upto_1950.png',width = 5, height = 2.5,dpi = 600)

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = lgeo_occ %>% filter(year <= 1990), aes(x = decimalLongitude,y = decimalLatitude),
             size = 2,alpha=0.8,color='brown')+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('output/lgeo_maps/lgeo_upto_1990.png',width = 5, height = 2.5,dpi = 600)

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = lgeo_occ %>% filter(year <= 2026), aes(x = decimalLongitude,y = decimalLatitude),
             size = 2,alpha=0.8,color='brown')+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('output/lgeo_maps/lgeo_upto_2026.png',width = 5, height = 2.5,dpi = 600)

## Coordinate cleaning####
lat_occ <- read_csv('data/spatial/occurrences/latrodectus_GBIF_2157920.csv')

library(countrycode)
library(CoordinateCleaner)

# Changing country code format
lat_occ$countryCode <-  countrycode(lat_occ$countryCode, 
                                origin =  'iso2c',
                                destination = 'iso3c')



lat_occ_cc <- lat_occ %>%
  cc_val() %>%
  cc_equ() %>%
  cc_cap() %>%
  cc_cen() %>%
  cc_coun(iso3 = "countryCode") %>%
  cc_inst() %>% 
  cc_sea() %>%
  cc_zero() %>%
  cc_outl() %>%
  cc_dupl()
write_csv(lat_occ_cc,'data/spatial/occurrences/lat_occ_cc.csv')

# For geometricus, might be interesting to test out `cc_urb` to check for records outside and inside of cities

## Manual cleaning####
# Further manual cleaning for each species
# First, getting the coordinates for species that are all good
good_spp <- c('Latrodectus antheratus','Latrodectus cinctus',
              'Latrodectus elegans','Latrodectus erythromelas',
              'Latrodectus indistinctus','Latrodectus karrooensis',
              'Latrodectus katipo','Latrodectus lilianae',
              'Latrodectus menavodi','Latrodectus mirabilis',
              'Latrodectus obscurior','Latrodectus renivulvatus',
              'Latrodectus rhodesiensis','Latrodectus thoracicus',
              'Latrodectus umbukwane')

lat_occ_good <- lapply(good_spp,function(x){
  return(lat_occ_cc %>% filter(species == x))
}) %>% bind_rows()

# Now cleaning species by species
occs <- list()
### Latrodectus bishopi####
## Remove points outside Florida, by keeping only points below 30 degree latitude
occs[['l_bishopi']] <- lat_occ_cc %>% filter(species == 'Latrodectus bishopi') %>%
  filter(decimalLatitude < 30)

### Latrodectus corallinus####
## Remove Manaus point (probably institution) by keeping only points below -5 latitude
occs[['l_corallinus']] <- lat_occ_cc %>% filter(species == 'Latrodectus corallinus') %>%
  filter(decimalLatitude < -5)

### Latrodectus curacaviensis####
## Remove North America points, by keeping only points below latitude 15
occs[['l_curacaviensis']] <- lat_occ_cc %>% filter(species == 'Latrodectus curacaviensis') %>%
  filter(decimalLatitude < 15)

### Latrodectus dahli####
## Remove Sweden point, by keeping only points below latitude 50
occs[['l_dahli']] <- lat_occ_cc %>% filter(species == 'Latrodectus dahli') %>%
  filter(decimalLatitude < 50)

### Latrodectus diaguita####
# Too few, skipped!

### Latrodectus garbae####
# Too few, skipped!

### Latrodectus hasselti####
## Remove Iran and Europe points, by keeping only points east of longitude 60
occs[['l_hasselti']] <- lat_occ_cc %>% filter(species == 'Latrodectus hasselti') %>%
  filter(decimalLongitude > 60)

### Latrodectus hesperus####
## Remove northern BC point, eastern US points and Hawaii points,
## by keeping only points south of latitude 58, east of latitude -150 and west of latitude -94
occs[['l_hesperus']] <- lat_occ_cc %>% filter(species == 'Latrodectus hesperus') %>%
  filter(decimalLatitude < 58) %>%
  filter(decimalLongitude > -150) %>%
  filter(decimalLongitude < -94)

### Latrodectus hurtadoi####
# Too few, skipped!

### Latrodectus hystrix####
# Too few, skipped!

### Latrodectus mactans####
## Remove points in South America, Hawaii and eastern hemisphere
## by keeping only points north of latitude 9, east of latitude -150 and west of latitude -30
occs[['l_mactans']] <- lat_occ_cc %>% filter(species == 'Latrodectus mactans') %>%
  filter(decimalLatitude > 9) %>%
  filter(decimalLongitude > -150) %>%
  filter(decimalLongitude < -30)

### Latrodectus occidentalis####
# Too few, skipped!

### Latrodectus pallidus####
## Remove Madagascar by keeping only points north of latitude 9
occs[['l_pallidus']] <- lat_occ_cc %>% filter(species == 'Latrodectus pallidus') %>%
  filter(decimalLatitude > 9)

### Latrodectus quartus####
# Too few, skipped!

### Latrodectus revivensis####
# Too few, skipped!

### Latrodectus tredecimguttatus####
## Remove points in Germany by filtering out country code == DEU
occs[['l_tredecimguttatus']] <- lat_occ_cc %>% filter(species == 'Latrodectus tredecimguttatus') %>%
  filter(countryCode != 'DEU')

### Latrodectus variegatus####
# Too few, skipped!

### Latrodectus variolus####
## Remove western US point by keeping only points east of longitude -101
occs[['l_variolus']] <- lat_occ_cc %>% filter(species == 'Latrodectus variolus') %>%
  filter(decimalLongitude > -101)

# Merging all with previously okay species
lat_occ_final <- bind_rows(lat_occ_good,bind_rows(occs)) %>%
  rename(x = 'decimalLongitude',y='decimalLatitude')
write_csv(lat_occ_final,'data/spatial/occurrences/lat_occ_final.csv')

## Maps####
### CC cleaning####
require(ggplot2)
require(ggspatial)
require(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw()) #Setting theme
spp <- unique(lat_occ_cc$species)
for (name in spp) {
  coords <- lat_occ_cc %>% filter(species == name) %>% select(x,y) %>% drop_na()
  ggplot(data = world) +
    geom_sf(fill= "ghostwhite", size = 0.1)+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    geom_point(data = coords, aes(x = x,y = y),
               size = 1, color = 'black',alpha=0.6)+
    scale_x_continuous(name = "Longitude")+
    scale_y_continuous(name = "Latitude")+
    labs(title = name)+
    theme(legend.position = 'none',
          panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))
  ggsave(paste0('output/spp_maps/',tolower(gsub(' ','_',name)),'_cc_cleaned.png'),
         width = 8, height = 6,dpi = 600)
}

# Color coded by basis of record
world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw()) #Setting theme
spp <- unique(lat_occ_cc$species)
for (name in spp) {
  coords <- lat_occ_cc %>% filter(species == name) %>% 
    select(x,y,basisOfRecord)
  ggplot(data = world) +
    geom_sf(fill= "ghostwhite", size = 0.1)+
    geom_point(data = coords,
               aes(x = x,y = y, color = basisOfRecord),
               size = 1,alpha=0.6)+
    scale_x_continuous(name = "Longitude")+
    scale_y_continuous(name = "Latitude")+
    labs(title = name)+
    theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))
  ggsave(paste0('output/spp_maps/',tolower(gsub(' ','_',name)),'_cc_cleaned_basis.png'),
         width = 8, height = 6,dpi = 600)
}

### Final cleaned dataset####
spp <- unique(lat_occ_final$species)
for (name in spp) {
  coords <- lat_occ_final %>% filter(species == x) %>% 
    select(decimalLongitude,decimalLatitude,basisOfRecord)
  ggplot(data = name) +
    geom_sf(fill= "ghostwhite", size = 0.1)+
    geom_point(data = coords,
               aes(x = x,y = y, color = basisOfRecord),
               size = 1,alpha=0.6)+
    scale_x_continuous(name = "Longitude")+
    scale_y_continuous(name = "Latitude")+
    labs(title = name)+
    theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))
  ggsave(paste0('output/spp_maps/final_dataset/',tolower(gsub(' ','_',name)),'_finaldataset.png'),
         width = 8, height = 6,dpi = 600)
}

# Predictors####
## GISA dataset####
require(tidyverse)
require(terra)
gisa_files <- list.files('data/spatial/gisa_dataset/',full.names = T, pattern = '.tif')
# Aggregate to convert to 30s and save to file
lapply(gisa_files,function(x){
  message('Aggregating ',x)
  r <- rast(x) %>% terra::aggregate(fact = 30, fun = mean)
  writeRaster(r,gsub('\\.tif','_30s.tif',x))
  message('')
})

# Convert to presence-absence
gisa30s_files <- list.files('data/spatial/gisa_dataset/',full.names = T,pattern = '_30s.tif')
gisa30s <- lapply(gisa30s_files,rast)
# Get coordinates and change everything to 1, keeping 0 as 0
gisa30s_pa <- lapply(gisa30s,function(x){
  r <- data.frame(crds(x),as.data.frame(x)) %>%
    rename(is = 'remapped_min') %>%
    mutate(is = case_when(is != 0 ~ 1,
                          .default = 0)) %>%
    drop_na() %>% as_tibble() %>% 
    rast(type ='xyz', crs = crs(x)) %>%
    # Masking by woorld shapafile to differentiate water (NA) from non-urban land (0)
    mask(mask = world)
  return(r)
})
# Writing to file
lapply(1:length(gisa30s_pa), function(i){
  writeRaster(gisa30s_pa[[i]],gsub('\\_30s.tif','_30s_pa.tif',gisa30s_files[i]),overwrite=TRUE)
})

# Reading back
gisa30s_pa <- lapply(1:length(gisa30s_pa), function(i){
  rast(gsub('\\_30s.tif','_30s_pa.tif',gisa30s_files[i]))
})

### Comparing L. geometricus environments in South Africa and the United States

lgeo_urb <- lapply(lgeo_r,function(lgeo){
  # First getting the values from GISA for this specific Lgeo prediction
  urb <- lapply(gisa30s_pa,function(gisa){
    # First check if extents intersect
    if (tryCatch(!is.null(crop(lgeo,ext(gisa))), error=function(e) return(FALSE))) {
      # If so, we crop lgeo predictions
      cropped <- crop(lgeo,ext(gisa))
      
      # Then we resample GISA to match our prediction exactly
      gisa_resampled <- resample(gisa,cropped,method='bilinear')
      
      # We then stack to matching values
      stc <- c(cropped,gisa_resampled)
      
      # Getting coordinates of lgeo prediction
      coords <- crds(cropped)
      
      # We extract values from both raster...
      values <- extract(stc,coords) %>%
        # ...then remove suitability for when Impervious Surface is NA
        mutate(suit = case_when(is.na(is) ~ NA,
                                .default = max)) %>%
        select(suit)
      values <- bind_cols(coords,values) %>% 
        drop_na()
    }
  }) %>%
    bind_rows() # Binding all GISA into one
  urb_r <- rasterFromXYZ(urb,
                         res = res(lgeo),
                         crs = crs(lgeo))
  return(urb_r)
})

# Writing to file
lapply(1:length(lgeo_urb),function(i){
  writeRaster(lgeo_urb[[i]],gsub('\\.tif','_urb.tif',files[i]))
})

## Env Dataset####
require(pastclim)
#set_data_path('data/spatial/env_data/')
bio <- c("bio01","bio02","bio03","bio04","bio05","bio06","bio07","bio08",
         "bio09","bio10","bio11","bio12","bio13","bio14",
         "bio15","bio16","bio17","bio18","bio19")
#download_dataset(dataset = 'WorldClim_2.1_0.5m',
#                 bio_variables = bio)
env <- region_slice(
  time_ce = 1985,
  bio_variables = bio,
  dataset = "WorldClim_2.1_0.5m"
)

## Future data
time <- c('2021-2040','2041-2060','2061-2080','2081-2100')
ssp <- c('ssp126','ssp245','ssp370','ssp585')

env_future <- lapply(time,function(time){
  data <- lapply(ssp,function(ssp){
    x <- rast(paste0('data/spatial/env_data/wc_future/wc2.1_30s_bioc_MIROC6_',ssp,'_',time,'.tif'))
    names(x) <- bio
    return(x)
  })
  names(data) <- ssp
  return(data)
})
names(env_future) <- time

## Exploring predictor space for some species####
# For L. geometricus and other possibly invasive species, I will plot
# native range vs all invasive range, both for environment only and
# environment with the GISA dataset

# SDMS####
require(flexsdm)
lat_occ_final <- read_csv('data/spatial/occurrences/lat_occ_final.csv')

## Three species have slightly disjunct ranges that need to be better explored:
## L. geometricus, L. hasselti, L. cinctus and L. Pallidus
## L. geometricus and L. hasselti are known to be invasive, with native range in
## South Africa and Australia, respectively.
## L. cinctus and L. pallidus are disjunct in Middle East and Morocco, respectively.
## L. geometricus was already removed earlier: I didn't focus on it when I was doing
## the coordinate cleaning.
## I will now also remove L. hasselti, to keep L. geometricus as the only invasive species.
## I will first focus on modeling all species that have more consistent ranges.
## Then, later on, I will model L. geometricus for comparison with each native species.
## For L. geometricus, I might want to make separate models for each of its invasive region,
## using points from each of the regions only.
## Finally, as of Mar 15, 2026, I will focus on the native species in North America
## plus L. geometricus to have a working result for the symposium talks.
## So here I will just filter out to the four species in North America
## and model L. geomtricys separately later

# Filtering to North America species
# L. bishopi, L. hesperus, L. mactans, L. variolus
spp_namer <- c('Latrodectus bishopi','Latrodectus hesperus','Latrodectus mactans','Latrodectus variolus')
occs_model <- lat_occ_final %>% filter(species %in% spp_namer)

## Coordinates filtering ####
## First, we will use a geographic distance filtering, to reduce the number of points,
## making it more feasible to use if for environmental filtering and partitioning.
## This will also deal with some of the overclustering.
spp <- sort(unique(occs_model$species))
occs_filt <- lapply(spp,function(name) {
  occs <- occs_model %>% filter(species == name) %>% select(x,y)
  occs$id <- 1:nrow(occs) # adding unique id to each row
  if (name == 'Latrodectus bishopi'){
    occs <- occs %>%
           occfilt_geo(data = .,x = "x",y = "y",
                       env_layer = env, method = c('defined',d=5)) %>%
           left_join(occs, by = c("id", "x", "y"))
  } else {
    occs <- occs %>%
      occfilt_geo(data = .,x = "x",y = "y",
                  env_layer = env, method = c('defined',d=50)) %>%
      left_join(occs, by = c("id", "x", "y"))
  }
  return(occs)
})
names(occs_filt) <- spp

# Checking
name <- 'Latrodectus bishopi'
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  #Adding points
  geom_point(data=occs_filt[[name]],aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(min(occs_filt[[name]]$x)-1,max(occs_filt[[name]]$x)+1),
           ylim=c(min(occs_filt[[name]]$y)-1,max(occs_filt[[name]]$y)+1))
saveRDS(occs_filt,'rdata/sdms/occs_filt_geo.rds')

## Setting calibration area ####
occs_filt <- readRDS('rdata/sdms/occs_filt_geo.rds')
# Dataframe of buffer width
buffer_w <- data.frame(spp,
                       width = c(1.5e5,4e5,4e5,4e5))
ca <- lapply(1:length(occs_filt),function(i) {
  return(calib_area(data = occs_filt[[i]],x = "x",y = "y",method = c("buffer", width = buffer_w$width[i]),crs = crs(env)))
})
names(ca) <- spp
saveRDS(ca,'rdata/sdms/ca_namer.rds')

## Map check
require(ggplot2)
require(ggspatial)
require(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_bw()) #Setting theme
for (i in 1:length(occs_filt)) {
  ext_vec <- ext(ca[[i]]) %>% as.vector()
  ggplot(data = world) +
    geom_sf(fill= "ghostwhite", size = 0.1)+
    geom_sf(data=st_as_sf(ca[[i]]),fill='lightgrey',alpha=0.3)+
    #Adding points
    geom_point(data=occs_filt[[i]],aes(x=x,y=y),alpha=0.5)+
    scale_x_continuous(name = "Longitude")+
    scale_y_continuous(name = "Latitude")+
    labs(title = spp[i])+
    theme(legend.position = 'none',
          panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))+
    coord_sf(xlim=c(ext_vec[1]-1,ext_vec[2]+1),ylim=c(ext_vec[3]-1,ext_vec[4]+1))
  ggsave(paste0('output/sdms/occ_filt_ca_namer_maps/',tolower(gsub(' ','_',spp[i])),'.png'),
         width = 7, height = 9,dpi = 600)
}

## Spatial partition ####
occs_filt <- readRDS('rdata/sdms/occs_filt_geo.rds')

# Partitioning L. bishopi with jackknife
# Partitioning remaining species with part_senv to decide best clustering
## based on env similarity, spatial autocorrelation and
## number of presence points in each partition. 
set.seed(10)
part <- lapply(1:length(occs_filt),function(i) {
  occs_filt[[i]]$pr_ab <- rep(1,nrow(occs_filt[[i]]))
  message(' Partitioning ',names(occs_filt)[i])
  # If L. bishopi, use leave-one-out
  if (i == 1) {
    message('Leave-one-out...')
    return(occs_filt[[i]] %>%
             part_random(data = .,pr_ab = "pr_ab",
                         method = c(method = 'loocv')))
    
  } else {
    # All others, use spatial environmental partitioning
    message('Spatial partitioning...')
    return(occs_filt[[i]] %>% part_senv(data = .,pr_ab = "pr_ab",
                                            x = 'x', y = 'y',
                                            env_layer = env))
  }
  message('')
})
saveRDS(part,'rdata/sdms/namer/part_namer.rds')

occs_part <- lapply(1:length(part),function(i){
  if (is.null(nrow(part[[i]]))) {
    return(part[[i]]$part)
  } else {
    return(part[[i]])
  }
})

## Background and pseudo-absences ####
## Background points
set.seed(10)
bg <- lapply(1:length(occs_part), function(i) {
  npart <- sort(unique(occs_part[[i]]$.part))
  bg <- lapply(npart, function(x) {
    pts <- sample_background(
      data = occs_part[[i]],
      x = "x",
      y = "y",
      n = sum(occs_part[[i]]$.part == x) * 2,
      method = "random",
      rlayer = env[[1]],
      calibarea = ca[[i]])
    pts$.part <- rep(x,nrow(pts))
    return(pts)
  }) %>% bind_rows()
  return(bg)
})

## Pseudo-absence
set.seed(10)
psa <- lapply(1:length(occs_part), function(i) {
  psa <- sample_pseudoabs(
    data = occs_part[[i]],
    x = "x",
    y = "y",
    n = nrow(occs_part[[i]]),
    method = "random",
    rlayer = env[[1]],
    calibarea = ca[[i]]
  )
  # Randomizing partition vector to psa points
  psa$.part <- sample(occs_part[[i]]$.part)
  return(psa)
})

# Bind presences and pseudo-absences
occ_pa <- lapply(1:length(occs_part),function(i){
  return(bind_rows(occs_part[[i]], psa[[i]]))
})

# Extracting environmental data for presence-absence and background
# To repeat this by using the points already saved before, but with
# new environment, I added the select() bit
occ_pa <- lapply(occ_pa, function(x){
  return(x %>% select(-starts_with('bio')) %>%
           sdm_extract(
             data = .,
             x = "x",
             y = "y",
             env_layer = env,
             filter_na = TRUE))
})

bg <- lapply(bg,function(x){
  return(x %>% select(-starts_with('bio')) %>%
           sdm_extract(
             data = .,
             x = "x",
             y = "y",
             env_layer = env,
             filter_na = TRUE
           ))
})

saveRDS(occ_pa,'rdata/sdms/namer/occ_pa_namer.rds')
saveRDS(bg,'rdata/sdms/namer/bg_namer.rds')

## Model tuning ####

occ_pa <- readRDS('rdata/sdms/namer/occ_pa_namer.rds')
bg <- readRDS('rdata/sdms/namer/bg_namer.rds')
# Maxent
models <- vector('list',length(occ_pa))
for (i in 1:length(models)) {
  models[[i]] <- tune_max(
    data = occ_pa[[i]],
    response = "pr_ab",
    predictors = names(env),
    background = bg[[i]],
    partition = ".part",
    grid = expand.grid(
      regmult = seq(0.5, 5, 0.5),
      classes = c("l", "lq", "lqp","lqhp","lqhpt")
    ),
    thr = c("max_sens_spec"),
    metric = "TSS",
    clamp = TRUE,
    pred_type = "cloglog"
  )
}
saveRDS(models,paste0('rdata/sdms/namer/maxmodel_results_',Sys.Date(),'.rds'))

## Model output ####

# Reading model
#models <- readRDS('rdata/sdms/namer/maxmodel_results_2026-03-19.rds')

### Table of model results ####

lapply(models,function(x){return(x$performance)}) %>% bind_rows() %>%
  add_column(spp,.before = 'regmult') %>% 
  write_csv('output/sdms/namer/model_table.csv')

## Spatial prediction ####
# Making shapefile for prediction areas
# Using calibration area extent and adding two degrees in all directions
pred_area <- lapply(1:length(spp),function(i){
  ext_vec <- ext(ca[[i]]) %>% as.vector()
  crop_vec <- c(ext_vec[1]-2,ext_vec[2]+2,ext_vec[3]-2,ext_vec[4]+2)
  return(world %>% st_crop(crop_vec))
})

# Checking prediction area
i=4
ext_vec <- ext(ca[[i]]) %>% as.vector()
ggplot(data = world) +
 geom_sf(fill= "ghostwhite", size = 0.1)+
 geom_sf(data = pred_area[[i]],alpha=0.5,color='blue') +
 geom_sf(data = st_as_sf(ca[[i]]),alpha=0.5,color='red') +
 scale_x_continuous(name = "Longitude")+
 scale_y_continuous(name = "Latitude")+
 theme(legend.position = 'none',
       panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
       panel.background = element_rect(fill = "aliceblue"),
       plot.title = element_text(face = "italic"))+
 coord_sf(xlim=c(ext_vec[1]-2,ext_vec[2]+2),ylim=c(ext_vec[3]-2,ext_vec[4]+2))
  
# Performing predictions
## Present
lapply(1:length(models),function(i){
  pred <- sdm_predict(models = models[[i]],pred = env,predict_area = vect(pred_area[[i]]))
  writeRaster(pred$max,paste0('output/sdms/namer/pred_rasters/present/present_',tolower(gsub(' ','_',spp[i])),'.tif'))
})

## Plots
pred_cur <- lapply(list.files('output/sdms/namer/pred_rasters/present/',full.names = T),rast)
for (i in 1:length(pred_cur)) {
  plotdata <- data.frame(crds(pred_cur[[i]]$max),as.data.frame(pred_cur[[i]]$max))
  # Without points
  ggplot(data = plotdata) +
    geom_tile(aes(x=x,y=y,fill=max))+
    coord_fixed()+scale_fill_gradient2(name = 'Predicted suitability',
                                       low = '#4575b4',mid= '#ffffbf', high = '#d73027',
                                       midpoint = min(plotdata$max)+((max(plotdata$max))-min(plotdata$max))/2)+
    labs(title = spp[i])+
    theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))
  #coord_sf(xlim=c(min(plotdata$x),max(plotdata$x)),ylim=c(min(plotdata$y),max(plotdata$y)))
  ggsave(paste0('output/sdms/namer/prediction_maps/present_',tolower(gsub(' ','_',spp[i])),'_woPoints.png'),
         width = 8, height = 8,dpi = 600)
  
  # With points
  ggplot(data = plotdata) +
    geom_tile(aes(x=x,y=y,fill=max))+
    coord_fixed()+scale_fill_gradient2(name = 'Predicted suitability',
                                       low = '#4575b4',mid= '#ffffbf', high = '#d73027',
                                       midpoint = min(plotdata$max)+((max(plotdata$max))-min(plotdata$max))/2)+
    #Adding points
    geom_point(data=occs_filt[[i]],aes(x=x,y=y),alpha=0.5)+
    labs(title = spp[i])+
    theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))
    #coord_sf(xlim=c(min(plotdata$x),max(plotdata$x)),ylim=c(min(plotdata$y),max(plotdata$y)))
  ggsave(paste0('output/sdms/namer/prediction_maps/present_',tolower(gsub(' ','_',spp[i])),'_withPoints.png'),
        width = 8, height = 8,dpi = 600)
}

## Future
lapply(1:length(models),function(i){
  lapply(1:length(time),function(j){
    lapply(1:length(ssp),function(k){
      pred <- sdm_predict(models = models[[i]], pred = env_future[[j]][[k]], predict_area = vect(pred_area[[i]]))
      writeRaster(pred$max,paste0('output/sdms/namer/pred_rasters/future/future_',
                                  tolower(gsub(' ','_',spp[i])),'_',time[j],'_',ssp[k],'.tif'))
    })
  })
})

# SDMS for L. geometricus ####

# Filtering for L. geometricus in North America
lgeo_occs <- read_csv('data/spatial/occurrences/lat_occ_cc.csv') %>%
  filter(species == "Latrodectus geometricus") %>%
  rename(x = 'decimalLongitude', y = 'decimalLatitude') %>% 
  filter(x > -136.53 & x < -62.43 & y > 12 & y < 57.90)
  
# Checking
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  #Adding points
  geom_point(data=lgeo_occs,aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))

## Coordinates filtering ####
## First, we will use a geographic distance filtering, to reduce the number of points,
## making it more feasible to use if for environmental filtering and partitioning.
## This will also deal with some of the overclustering.
occs <- lgeo_occs %>% select(x,y) %>%
  mutate(id = 1:nrow(lgeo_occs))
lgeo_filt <- occfilt_geo(data = occs,x = "x",y = "y",
                         env_layer = env, method = c('defined',d=50)) %>%
  left_join(occs, by = c("id", "x", "y"))

# Checking
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  #Adding points
  geom_point(data=lgeo_filt,aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(min(lgeo_filt$x)-1,max(lgeo_filt$x)+1),
           ylim=c(min(lgeo_filt$y)-1,max(lgeo_filt$y)+1))
write_csv(lgeo_filt,'output/sdms/lgeo/lgeo_filt_geo.csv')

## Setting calibration area ####

lgeo_filt <- read_csv('output/sdms/lgeo/lgeo_filt_geo.csv')
# Dataframe of buffer width

ca <- calib_area(data = lgeo_filt,x = "x",y = "y",method = c("buffer", width = 4e5),crs = crs(env))


## Map check
ext_vec <- ext(ca) %>% as.vector()
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_sf(data=st_as_sf(ca),fill='lightgrey',alpha=0.3)+
  #Adding points
  geom_point(data=lgeo_filt,aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(ext_vec[1]-1,ext_vec[2]+1),ylim=c(ext_vec[3]-1,ext_vec[4]+1))
ggsave('output/sdms/lgeo/lgeo_filt_ca_map.png',
       width = 7, height = 9,dpi = 600)

## Spatial partition ####
#occs_filt <- readRDS('rdata/sdms/occs_filt_geo.rds')

# Partitioning L. bishopi with jackknife
# Partitioning remaining species with part_senv to decide best clustering
## based on env similarity, spatial autocorrelation and
## number of presence points in each partition. 
set.seed(10)
lgeo_filt$pr_ab <- rep(1,nrow(lgeo_filt))
part <- lgeo_filt %>%
  part_senv(data = .,pr_ab = "pr_ab",
            x = 'x', y = 'y',
            env_layer = env)
occs_part <- part$part
write_csv(occs_part,'output/sdms/lgeo/occs_part_lgeo.csv')

## Background and pseudo-absences ####
## Background points
set.seed(10)

npart <- sort(unique(occs_part$.part))
bg <- lapply(npart, function(x) {
  pts <- sample_background(
    data = occs_part,
    x = "x",
    y = "y",
    n = sum(occs_part$.part == x) * 2,
    method = "random",
    rlayer = env[[1]],
    calibarea = ca)
  pts$.part <- rep(x,nrow(pts))
  return(pts)
}) %>% bind_rows()

## Pseudo-absence
set.seed(10)
psa <- sample_pseudoabs(data = occs_part,
                        x = "x",
                        y = "y",
                        n = nrow(occs_part),
                        method = "random",
                        rlayer = env[[1]],
                        calibarea = ca)
# Randomizing partition vector to psa points
psa$.part <- sample(occs_part$.part)

# Bind presences and pseudo-absences
occ_pa <- bind_rows(occs_part, psa)

# Extracting environmental data for presence-absence and background
# To repeat this by using the points already saved before, but with
# new environment, I added the select() bit
occ_pa <- occ_pa %>% select(-starts_with('bio')) %>%
  sdm_extract(data = ., x = "x",y = "y",
              env_layer = env,
              filter_na = TRUE)

bg <- bg %>% select(-starts_with('bio')) %>%
           sdm_extract(
             data = .,
             x = "x",
             y = "y",
             env_layer = env,
             filter_na = TRUE
           )
write_csv(occ_pa,'output/sdms/lgeo/occ_pa.csv')
write_csv(bg,'output/sdms/lgeo/bg.csv')

## Model tuning ####

#occ_pa <- readRDS('rdata/sdms/namer/occ_pa_namer.rds')
#bg <- readRDS('rdata/sdms/namer/bg_namer.rds')
# Maxent
lgeo_model <- tune_max(
    data = occ_pa,
    response = "pr_ab",
    predictors = names(env),
    background = bg,
    partition = ".part",
    grid = expand.grid(
      regmult = seq(0.5, 5, 0.5),
      classes = c("l", "lq", "lqp","lqhp","lqhpt")
    ),
    thr = c("max_sens_spec"),
    metric = "TSS",
    clamp = TRUE,
    pred_type = "cloglog"
  )
saveRDS(lgeo_model,paste0('rdata/sdms/lgeo/lgeo_maxmodel_results_',Sys.Date(),'.rds'))

## Model output ####

# Reading model
lgeo_model <- readRDS('rdata/sdms/lgeo/lgeo_maxmodel_results_2026-03-21.rds')

## Spatial prediction ####
# Making shapefile for prediction areas
# Using calibration area extent and adding two degrees in all directions
ext_vec <- ext(ca) %>% as.vector()
crop_vec <- c(ext_vec[1]-2,ext_vec[2]+2,ext_vec[3]-2,ext_vec[4]+2)
pred_area_lgeo <- world %>% st_crop(crop_vec)
  
# Checking prediction area
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_sf(data = pred_area_lgeo,alpha=0.5,color='blue') +
  geom_sf(data = st_as_sf(ca),alpha=0.5,color='red') +
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(ext_vec[1]-2,ext_vec[2]+2),ylim=c(ext_vec[3]-2,ext_vec[4]+2))

# Performing predictions
## Present
pred <- sdm_predict(models = lgeo_model,pred = env,predict_area = vect(pred_area_lgeo))
writeRaster(pred$max,'output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus.tif')

## Future
lapply(1:length(time),function(j){
  lapply(1:length(ssp),function(k){
    message('Predicting for ',time[j],' and ',ssp[k])
    pred <- sdm_predict(models = lgeo_model, pred = env_future[[j]][[k]], predict_area = vect(pred_area_lgeo))
    writeRaster(pred$max,paste0('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_',
                                time[j],'_',ssp[k],'.tif'))
  })
})

# Plots
future_ssp126 <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126.tif')
plotdata <- data.frame(crds(future_ssp126$max),as.data.frame(future_ssp126$max))

ggplot(data = plotdata) +
  geom_tile(aes(x=x,y=y,fill=max))+
  coord_fixed()+scale_fill_gradient2(name = 'Predicted suitability',
                                     low = '#4575b4',mid= '#ffffbf', high = '#d73027',
                                     midpoint = min(plotdata$max)+((max(plotdata$max))-min(plotdata$max))/2)+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))
#coord_sf(xlim=c(min(plotdata$x),max(plotdata$x)),ylim=c(min(plotdata$y),max(plotdata$y)))
ggsave('output/sdms/lgeo/prediction_maps/future_latrodectus_geometricus_2041-2060_ssp126.png',
       width = 8, height = 8,dpi = 600)

# Pessimistic
future_ssp585 <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585.tif')
plotdata <- data.frame(crds(future_ssp585$max),as.data.frame(future_ssp585$max))

ggplot(data = plotdata) +
  geom_tile(aes(x=x,y=y,fill=max))+
  coord_fixed()+scale_fill_gradient2(name = 'Predicted suitability',
                                     low = '#4575b4',mid= '#ffffbf', high = '#d73027',
                                     midpoint = min(plotdata$max)+((max(plotdata$max))-min(plotdata$max))/2)+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))
#coord_sf(xlim=c(min(plotdata$x),max(plotdata$x)),ylim=c(min(plotdata$y),max(plotdata$y)))
ggsave('output/sdms/lgeo/prediction_maps/future_latrodectus_geometricus_2041-2060_ssp585.png',
       width = 8, height = 8,dpi = 600)


# Post-prediction processing ####
# Focusing only on 2041-2060

## Urban filtering####
# Let's filter predictions of L. geometricus by urban areas
# Present
files <- c(present = 'output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus.tif',
           optimistic = 'output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126.tif',
           pessimistic = 'output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585.tif')
lgeo_r <- c(rast(files['present']),
            rast(files['optimistic']),
            rast(files['pessimistic']))
lgeo_urb <- lapply(lgeo_r,function(lgeo){
  # First getting the values from GISA for this specific Lgeo prediction
  urb <- lapply(gisa30s_pa,function(gisa){
    # First check if extents intersect
    if (tryCatch(!is.null(crop(lgeo,ext(gisa))), error=function(e) return(FALSE))) {
      # If so, we crop lgeo predictions
      cropped <- crop(lgeo,ext(gisa))
    
      # Then we resample GISA to match our prediction exactly
      gisa_resampled <- resample(gisa,cropped,method='bilinear')
      
      # We then stack to matching values
      stc <- c(cropped,gisa_resampled)
    
      # Getting coordinates of lgeo prediction
      coords <- crds(cropped)
      
      # We extract values from both raster...
      values <- extract(stc,coords) %>%
        # ...then remove suitability for when Impervious Surface is NA
        mutate(suit = case_when(is.na(is) ~ NA,
                                .default = max)) %>%
        select(suit)
      values <- bind_cols(coords,values) %>% 
        drop_na()
    }
  }) %>%
    bind_rows() # Binding all GISA into one
  urb_r <- rasterFromXYZ(urb,
                         res = res(lgeo),
                         crs = crs(lgeo))
  return(urb_r)
})

# Writing to file
lapply(1:length(lgeo_urb),function(i){
  writeRaster(lgeo_urb[[i]],gsub('\\.tif','_urb.tif',files[i]))
})

## Calculating overlap####
### Climate-only####
# Read all present raster
spp_namer <- c('Latrodectus bishopi','Latrodectus hesperus','Latrodectus mactans','Latrodectus variolus')

lgeo_cur <- rast('output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus.tif')
namer_cur <- lapply(paste0('output/sdms/namer/pred_rasters/present_',
                                      tolower(gsub(' ','_',spp_namer)),'.tif'),
                    rast)

# Do overlap for each namer species
lapply(1:length(namer_cur),function(i){
  # Crop lgeo_cur by namer
  lgeo_cropped <- crop(lgeo_cur,namer_cur[[i]])
  x <- crop(namer_cur[[i]],lgeo_cropped) # To assure extent is the same
  mean_raster <- (lgeo_cropped + x) / 2
  writeRaster(mean_raster,paste0('output/sdms/overlap/rasters/present_overlap_',
                                 tolower(gsub(' ','_',spp_namer[i])),'.tif'),
              overwrite=TRUE)
})

# Future
# Optimistic
lgeo_ssp126 <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126.tif')
namer_ssp126 <- lapply(list.files('output/sdms/namer/pred_rasters/',pattern = '^future.*2041-2060_ssp126.*',full.names = T),
                    rast)

# Do overlap for each namer species
lapply(1:length(namer_ssp126),function(i){
  # Crop lgeo_cur by namer
  lgeo_cropped <- crop(lgeo_ssp126,namer_ssp126[[i]])
  x <- crop(namer_ssp126[[i]],lgeo_cropped) # To assure extent is the same
  mean_raster <- (lgeo_cropped + x) / 2
  writeRaster(mean_raster,paste0('output/sdms/overlap/rasters/future_overlap_2050_ssp126_',
                                 tolower(gsub(' ','_',spp_namer[i])),'.tif'),
              overwrite=TRUE)
})

# Pessimistic
lgeo_ssp585 <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585.tif')
namer_ssp585 <- lapply(list.files('output/sdms/namer/pred_rasters/',pattern = '^future.*2041-2060_ssp585.*',full.names = T),
                       rast)

# Do overlap for each namer species
lapply(1:length(namer_ssp585),function(i){
  # Crop lgeo_cur by namer
  lgeo_cropped <- crop(lgeo_ssp585,namer_ssp585[[i]])
  x <- crop(namer_ssp585[[i]],lgeo_cropped) # To assure extent is the same
  mean_raster <- (lgeo_cropped + x) / 2
  writeRaster(mean_raster,paste0('output/sdms/overlap/rasters/future_overlap_2050_ssp585_',
                                 tolower(gsub(' ','_',spp_namer[i])),'.tif'),
              overwrite=TRUE)
})

### Climate and urban####
# Read all present raster
spp_namer <- c('Latrodectus bishopi','Latrodectus hesperus','Latrodectus mactans','Latrodectus variolus')

lgeo_cur <- rast('output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus_urb.tif')
namer_cur <- lapply(paste0('output/sdms/namer/pred_rasters/present_',
                           tolower(gsub(' ','_',spp_namer)),'.tif'),
                    rast)

# Do overlap for each namer species
lapply(1:length(namer_cur),function(i){
  # Crop lgeo_cur by namer
  lgeo_cropped <- crop(lgeo_cur,namer_cur[[i]])
  x <- crop(namer_cur[[i]],lgeo_cropped) # To assure extent is the same
  mean_raster <- (lgeo_cropped + x) / 2
  writeRaster(mean_raster,paste0('output/sdms/overlap/rasters/present_overlap_',
                                 tolower(gsub(' ','_',spp_namer[i])),'_urb.tif'),
              overwrite=TRUE)
})

# Future
# Optimistic
lgeo_ssp126 <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126_urb.tif')
namer_ssp126 <- lapply(list.files('output/sdms/namer/pred_rasters/',pattern = '^future.*2041-2060_ssp126.*',full.names = T),
                       rast)

# Do overlap for each namer species
lapply(1:length(namer_ssp126),function(i){
  # Crop lgeo_cur by namer
  lgeo_cropped <- crop(lgeo_ssp126,namer_ssp126[[i]])
  x <- crop(namer_ssp126[[i]],lgeo_cropped) # To assure extent is the same
  mean_raster <- (lgeo_cropped + x) / 2
  writeRaster(mean_raster,paste0('output/sdms/overlap/rasters/future_overlap_2050_ssp126_',
                                 tolower(gsub(' ','_',spp_namer[i])),'_urb.tif'),
              overwrite=TRUE)
})

# Pessimistic
lgeo_ssp585 <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585_urb.tif')
namer_ssp585 <- lapply(list.files('output/sdms/namer/pred_rasters/',pattern = '^future.*2041-2060_ssp585.*',full.names = T),
                       rast)

# Do overlap for each namer species
lapply(1:length(namer_ssp585),function(i){
  # Crop lgeo_cur by namer
  lgeo_cropped <- crop(lgeo_ssp585,namer_ssp585[[i]])
  x <- crop(namer_ssp585[[i]],lgeo_cropped) # To assure extent is the same
  mean_raster <- (lgeo_cropped + x) / 2
  writeRaster(mean_raster,paste0('output/sdms/overlap/rasters/future_overlap_2050_ssp585_',
                                 tolower(gsub(' ','_',spp_namer[i])),'_urb.tif'),
              overwrite=TRUE)
})

## Plotting all predictions####

### Lgeo ####
#### Climate only ####
r <- rast('output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus.tif')

# With points
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/present_latrodectus_geometricus_wPoints_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
points(data.frame(lgeo_filt$x,lgeo_filt$y), pch = 20, cex = 1, col = 'black')
dev.off()

# Without points
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/present_latrodectus_geometricus_woPoints_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

# Optimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126.tif')
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp126_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

# Pessimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585.tif')
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp585_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

#### Climate and urban ####
r <- rast('output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus_urb.tif')
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/present_latrodectus_geometricus_urb_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
dev.off()

# Optimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126_urb.tif')
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp126_urb_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

# Pessimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585_urb.tif')
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp585_urb_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

### Lgeo NE region####
#### Climate only ####
r <- rast('output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus.tif')
r <- crop(r,extent(-86.155776,-67.793821,36.405098,45.062625))
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/present_latrodectus_geometricus_NE_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
points(data.frame(lgeo_filt$x,lgeo_filt$y), pch = 20, cex = 2, col = 'black')
dev.off()

# Optimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126.tif')
r <- crop(r,extent(-86.155776,-67.793821,36.405098,45.062625))
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp126_NE_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

# Pessimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585.tif')
r <- crop(r,extent(-86.155776,-67.793821,36.405098,45.062625))
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp585_NE_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()


#### Climate and urban ####
r <- rast('output/sdms/lgeo/pred_rasters/present_latrodectus_geometricus_urb.tif')
r <- crop(r,extent(-86.155776,-67.793821,36.405098,45.062625))
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/present_latrodectus_geometricus_urb_NE_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
points(data.frame(lgeo_filt$x,lgeo_filt$y), pch = 20, cex = 2, col = 'black')
dev.off()

# Optimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp126_urb.tif')
r <- crop(r,extent(-86.155776,-67.793821,36.405098,45.062625))
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp126_urb_NE_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

# Pessimistic
r <- rast('output/sdms/lgeo/pred_rasters/future_latrodectus_geometricus_2041-2060_ssp585_urb.tif')
r <- crop(r,extent(-86.155776,-67.793821,36.405098,45.062625))
png(filename = 'output/sdms/lgeo/prediction_maps/baseR/future_latrodectus_geometricus_2050_ssp585_urb_NE_baseR.png',
    width = 1200, height = 1200)
my_palette <- colorRampPalette(c('#4575b4','#ffffbf','#d73027'))(100)
plot(r, col = my_palette, colNA = "aliceblue")
plot(as(world, "Spatial"),add=T,col = NA, border = "black")
plot(as((states %>% filter(admin == 'United States of America')),'Spatial'), add=T,col=NA,border = "black")
dev.off()

### Native species####

### Overlaps####
lapply(list.files('output/sdms/overlap/rasters/',full.names = T, pattern = '.tif'),function(x){
  r <- rast(x)
  plotdata <- data.frame(crds(r),as.data.frame(r))
  colnames(plotdata) <- c('x','y','suit')
  # Removing lower values
  plotdata <- plotdata %>% 
    mutate(suit = case_when(suit < (max(suit)-0.1) ~ NA,
                            .default = suit))
                            #suit < (max(suit)-2) ~ 1))
  ggplot()+
    geom_sf(data = world,fill='grey')+
    geom_sf(data = states %>% filter(admin == 'United States of America'),fill=NA,size=0.1)+
    geom_tile(data = plotdata,aes(x=x,y=y,fill=suit))+
    scale_fill_gradient(name = 'Average suitability',
                        low = '#ffffbf', high = '#d73027',
                        na.value='grey')+
                         #low = '#4575b4',mid= '#ffffbf', high = '#d73027',
                         #midpoint = max(plotdata$max)-0.2)+
    theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
          panel.background = element_rect(fill = "aliceblue"),
          plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(min(plotdata$x),max(plotdata$x)),ylim=c(min(plotdata$y),max(plotdata$y)))
  ggsave(str_replace(x,'rasters','maps') %>% str_replace('.tif','.png'),
       width = 8, height = 8,dpi = 600)
})

# Comparing L_geo envs in S Africa vs N AMerica ####
# Here, we will use the geographically filtered points for North America, utilized in the models above
# We will also extract coordinates for South Africa and apply spatial thinning

# USA filtered points
lgeo_na_filt <- read_csv('output/sdms/lgeo/lgeo_filt_geo.csv')

## South Africa points####
# Filtering for L. geometricus in North America
lgeo_sa <- read_csv('data/spatial/occurrences/lat_occ_cc.csv') %>%
  filter(species == "Latrodectus geometricus") %>%
  rename(x = 'decimalLongitude', y = 'decimalLatitude') %>% 
  filter(x > 11.818326 & x < 39.421501 & y > -35.792463 & y < -21.217595)

# Checking
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  #Adding points
  geom_point(data=lgeo_sa,aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))

## Geographic filtering
occs <- lgeo_sa %>% select(x,y) %>%
  mutate(id = 1:nrow(lgeo_sa))
lgeo_sa_filt <- occfilt_geo(data = occs,x = "x",y = "y",
                         env_layer = env, method = c('defined',d=50)) %>%
  left_join(occs, by = c("id", "x", "y"))

# Checking
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  #Adding points
  geom_point(data=lgeo_sa_filt,aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(min(lgeo_sa_filt$x)-1,max(lgeo_sa_filt$x)+1),
           ylim=c(min(lgeo_sa_filt$y)-1,max(lgeo_sa_filt$y)+1))

## Extracting GISA info####
lgeo_coords <- lgeo_na_filt %>% select(x,y) %>%
  mutate(region = 'na') %>% 
  bind_rows(lgeo_sa_filt %>% select(x,y) %>% mutate(region = 'sa'))

gisa_values <- lapply(gisa30s_pa,function(gisa){
  return(data.frame(lgeo_coords,extract(gisa,lgeo_coords %>% select(x,y))) %>%
           drop_na() %>% select(-ID))
}) %>% bind_rows()

# Calculating prop and plotting
gisa_values %>% 
  group_by(region,is) %>% count() %>% 
  mutate(prop = case_when(region == 'na' ~ n/(gisa_values %>% filter(region == 'na') %>% nrow()),
                          region == 'sa' ~ n/(gisa_values %>% filter(region == 'sa') %>% nrow()))) %>%
  mutate(is = as.character(is)) %>% 
  ggplot(aes(x=region,y=prop,fill=is))+geom_bar(stat='identity')+
  xlab('Region')+ylab('Proportion')+
  scale_x_discrete(labels = c('North America','South Africa'))+
  scale_fill_discrete(name = 'Surface',labels = c('Non-impervious surface','Impervious surface'))+
  theme_bw()
ggsave('output/gisa/lgeo_gisa_comparison_na_sa.png',width = 10, height = 7,dpi = 600)

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  #Adding points
  geom_point(data=gisa_values %>% filter(region == 'na' & is == 0),
             aes(x=x,y=y),alpha=0.5)+
  scale_x_continuous(name = "Longitude")+
  scale_y_continuous(name = "Latitude")+
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = gray(.9),linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(face = "italic"))+
  coord_sf(xlim=c(min(lgeo_na_filt$x)-1,max(lgeo_na_filt$x)+1),
           ylim=c(min(lgeo_na_filt$y)-1,max(lgeo_na_filt$y)+1))
ggsave('output/gisa/lgeo_na_nonimpervious_points.png',width = 10, height = 10,dpi = 600)