

# Load libraries ----------------------------------------------------------
source('./start.R')

# Load data ---------------------------------------------------------------

## Tabular
fles <- dir_ls('D:/DATA/dataverse_files/spam2010v2r0_global_harv_area.csv', regexp = '.csv$')
hrvs <- grep('_H_TA', fles, value = T) |> read_csv()
colnames(hrvs) |> grep('cffe', x  = _, value = T)
hrvs <- dplyr::select(hrvs, iso3, prod_level, alloc_key, cell5m, x, y, 'acof_a')

## Raster arabica
rstr.ara <- dir_ls('D:/DATA/dataverse_files/folders/spam2010v2r0_global_harv_area.geotiff', regexp = '.tif$') |> as.character()
rstr.ara <- grep('ACOF', rstr.ara, value = T)
rstr.ara <- grep('_A.tif$', rstr.ara, value = T)
rstr.ara <- rast(rstr.ara)

## Raster robusta 
rstr.rob <- dir_ls('D:/DATA/dataverse_files/folders/spam2010v2r0_global_harv_area.geotiff', regexp = '.tif$')
rstr.rob <- grep('RCOF', rstr.rob, value = T)
rstr.rob <- as.character(rstr.rob)
rstr.rob <- grep('_A.tif$', rstr.rob, value = T)
rstr.rob <- rast(rstr.rob)

## Vector data
wrld <- geodata::world(resolution = 1, path = './tmpr')

# Tidy the raster ---------------------------------------------------------
rstr.ara <- terra::classify(rstr.ara, rcl = matrix(c(0, NA), ncol = 2), others = NULL)
rstr.rob <- terra::classify(rstr.rob, rcl = matrix(c(0, NA), ncol = 2), others = NULL)

# Zonal statistics --------------------------------------------------------
znal.ara <- exactextractr::exact_extract(x = rstr.ara, y = st_as_sf(wrld), fun = function(value, coverage_fraction){sum(value, na.rm = TRUE)})
znal.rob <- exactextractr::exact_extract(x = rstr.rob, y = st_as_sf(wrld), fun = function(value, coverage_fraction){sum(value, na.rm = TRUE)})

wrld$zonal_ara <- znal.ara
wrld$zonal_rob <- znal.rob
wrld$zonal_cff <- wrld$zonal_ara + wrld$zonal_rob

# Testing -----------------------------------------------------------------
bra <- wrld[wrld$GID_0 == 'BRA',] 
bra.ara <- rstr.ara |> terra::crop(bra) |> terra::mask(bra)
bra.rob <- rstr.rob |> terra::crop(bra) |> terra::mask(bra)

# FaoSTAT Dataset ---------------------------------------------------------
faos <- st_read('./gpkg/faostat_summary_classes_2020-2024.gpkg')
faos <- faos |> rename(value_faostat_hrv = value)
faos <- faos |> dplyr::select(GID_0, NAME_0, value_faostat_hrv, percentage, perc_cum, interval)
faos.tble <- faos |> st_drop_geometry() |> as_tibble()

# MapSPAM -----------------------------------------------------------------
wrld
fnal <- full_join(st_as_sf(wrld), faos.tble, by = c('GID_0', 'NAME_0'))
fnal <- st_transform(fnal, crs = st_crs(4326))
fnal |> filter(NAME_0 == 'Colombia')

# To write ----------------------------------------------------------------
st_write(fnal, './gpkg/faostat_mapspam.gpkg')

# To draw a map -----------------------------------------------------------
