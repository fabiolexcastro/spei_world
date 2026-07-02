
# Load libraries ----------------------------------------------------------
source('start.R')

## FAO API: https://www.fao.org/faostat/en/#developer-portal/sign-in

# Data --------------------------------------------------------------------

##
tble <- get_faostat_bulk(code = "QCL", data_folder = tempdir())
tble <- as_tibble(tble)

## Filtering
tble <- tble |> filter(item == 'Coffee, green')
tble <- tble |> filter(year > 2000)

# Production --------------------------------------------------------------
tble <- tble |> filter(element == 'production') 
tble <- tble |> filter(area_code < 5000)

# Summary -----------------------------------------------------------------

prod <- tble |> 
  group_by(area, item, element) |> 
  reframe(
    value = mean(value, na.rm = T)
  ) |> 
  arrange(
    desc(value)
  ) |> 
  drop_na() |> 
  mutate(
    percentage = value / sum(value, na.rm = T) * 100, 
    perc_cum = cumsum(percentage)
  )

# To write the table ------------------------------------------------------

write.csv(prod, './tbl/')

