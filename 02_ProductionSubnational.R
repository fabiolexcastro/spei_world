

# Load libraries ----------------------------------------------------------
source('start.R')

# Function to draw the map ------------------------------------------------
make.map <- function(shp){
  
  ##
  cat('To start the analysis!\n')
  shp
  cnt <- unique(shp$ISO)
  yea <- unique(shp$year)
  bse <- geodata::gadm(country = cnt, level = 1, path = './tmpr')
  
  ##
  ggm <- ggplot() + 
    geom_sf(
      data = shp, aes(fill = hrv)
    ) + 
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(n = 9, name = 'YlOrBr')[3:9],
      na.value = 'white'
    ) +
    geom_sf(
      data = st_as_sf(bse), fill = NA, col = 'grey40'
    ) +
    coord_sf(
    ) +
    labs(
      fill = 'Harvested area'
    ) +
    ggtitle(
      label = glue('{cnt} - {yea}')
    ) +
    theme_bw() +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(3, 'line'), 
      legend.key.height = unit(0.5, 'line'),
      legend.title = element_text(hjust = 0.5), 
      legend.title.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'),
      axis.text.x = element_text(size = 6, hjust = 0.5), 
      axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.1),
      panel.grid.minor = element_line(colour = "grey93", linewidth = 0.1),
      panel.border = element_rect(colour = 'grey50', linewidh = 0.1)
    ) 
  
  ## To save the map 
  # ggsave(
  #   plot = ggm, 
  #   filename = glue('./png/maps/{cnt}_hrv_{yea}.jpg'),
  #   units = 'in', width = 8, height = 7, dpi = 300, create.dir = T
  # )
  
  return(ggm)
  
}


# Data --------------------------------------------------------------------
fles <- dir_ls('D:/OneDrive - CGIAR/Projects/World Coffee/data/shapefile/cff/adm1', regexp = '.shp$')
shpf <- st_read('./gpkg/faostat_summary_classes.gpkg')

to12 <- shpf |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  dplyr::select(GID_0, NAME_0, percentage, perc_cum) |> 
  arrange(desc(percentage)) |> 
  top_n(n = 12, wt = percentage)

to12 <- to12 |> 
  mutate(data = c('Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'No', 'No', 'No', 'Yes', 'Yes', 'No'))

# BRA ---------------------------------------------------------------------
bra <- grep('Brazil', fles, value = T) |> 
  as.character() |> 
  grep('2018', x = _, value = T) |> 
  st_read() |> 
  mutate(hrv = as.numeric(hrv))

gg.bra <- make.map(bra)
ggsave(plot = gg.bra, filename = './png/maps/BRA_hrv_2018.jpg', units = 'in', width = 4, height = 5, dpi = 300, create.dir = T)


# VNM ---------------------------------------------------------------------
vnm <- grep('Vietnam', fles, value = T) |> 
  as.character() |> 
  st_read() |> 
  mutate(hrv = as.numeric(hrv), 
         yea = 'SD')

gg.vnm <- make.map(vnm) + 
  ggtitle(
    label = 'VNM'
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),    
    legend.position = 'right', 
    legend.key.width = unit(0.5, 'line'), 
    legend.key.height = unit(3, 'line')
  )

ggsave(plot = gg.vnm, filename = './png/maps/VNM_hrv_sd.jpg', units = 'in', width = 4, height = 5.75, dpi = 300, create.dir = T)

# Indonesia ---------------------------------------------------------------
idn <- grep('har_idn', fles, value = T) |> 
  as.character() |> 
  grep('2000', x = _, value = T) |> 
  st_read() |> 
  mutate(hrv = as.numeric(harvested))

gg.idn <- make.map(idn)
ggsave(plot = gg.idn, filename = './png/maps/IDN_hrv_200.jpg', units = 'in', width = 7, height = 4.2, dpi = 300, create.dir = T)

# COL ---------------------------------------------------------------------
col <- read_csv('D:/Projects/SPEI-SPI/data/tbl/eva-colombia_mpios_2007-2024.csv', show_col_types = FALSE) |> 
  filter(periodo == 2024) |> 
  group_by(Depto) |> 
  reframe(
    area_cosechada = sum(area_cosechada, na.rm = T)
  ) |> 
  ungroup()

col.shp <- geodata::gadm(country = 'COL', level = 1, path = './tmpr')
col <- inner_join(st_as_sf(col.shp), col, by = c('NAME_1' = 'Depto'))
col <- mutate(col, hrv = as.numeric(area_cosechada), year = 2024, ISO = 'COL')

gg.col <- make.map(col) +
  theme(
    legend.position = 'right', 
    legend.key.width = unit(0.5, 'line'), 
    legend.key.height = unit(3, 'line')
  )
gg.col
ggsave(plot = gg.col, filename = './png/maps/COL_hrv_2024.jpg', units = 'in', width = 4, height = 4, dpi = 300, create.dir = T)

# ETH ---------------------------------------------------------------------
eth <- grep('etp', fles, value = T) |> 
  grep('2011', x = _, value = T) |> 
  st_read() |> 
  mutate(hrv = as.numeric(Hrvstd_), year = '2011')

gg.eth <- make.map(eth)
ggsave(plot = gg.eth, filename = './png/maps/ETH_hrv_2011.jpg', units = 'in', width = 4, height = 4, dpi = 300, create.dir = T)

# IND ---------------------------------------------------------------------
ind <- grep('ind', fles, value = T) |> 
  as.character() |> 
  grep('1998', x = _, value = T) |> 
  st_read() |> 
  mutate(hrv = as.numeric(harvested))

gg.ind <- make.map(ind)
ggsave(plot = gg.ind, filename = './png/maps/IND_hrv_1998.jpg', units = 'in', width = 4, height = 5, dpi = 300, create.dir = T)

# HND ---------------------------------------------------------------------
grep('hnd', fles, value = T)

# PER ---------------------------------------------------------------------
grep('per', fles, value = T)

# UGA ---------------------------------------------------------------------
grep('uga', fles, value = T)

# GTM ---------------------------------------------------------------------
gtm <- grep('gtm', fles, value = T) |> 
  st_read() |> 
  mutate(hrv = as.numeric(harvested))

gg.gtm <- make.map(gtm)
ggsave(plot = gg.gtm, filename = './png/maps/GTM_hrv_2005.jpg', units = 'in', width = 4, height = 5, dpi = 300, create.dir = T)

# MEX ---------------------------------------------------------------------
mex <- grep('mex', fles, value = T) |>
  grep('2018', x = _, value = T) |> 
  st_read() |> 
  mutate(hrv = as.numeric(cosechd))
gg.mex <- make.map(mex)
ggsave(plot = gg.mex, filename = './png/maps/MEX_hrv_2018.jpg', units = 'in', width = 6, height = 4.5, dpi = 300, create.dir = T)

# CIV ---------------------------------------------------------------------
grep('civ', fles, value = T)


fle <- dir_ls('D:/') |> grep('.asc$', x = _, value = T) |> as.character()
rst <- rast(fle)
vect(fle)

