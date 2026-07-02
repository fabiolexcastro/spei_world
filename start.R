


# Load --------------------------------------------------------------------
require(pacman)
p_load(
  terra, fs, sf, tidyverse, gtools, stringr, geodata, ggspatial
)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)
