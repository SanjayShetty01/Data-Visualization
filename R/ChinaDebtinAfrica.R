library(here)
library(readxl)
library(dplyr)
library(ggplot2)
library(countrycode)
library(maps)
library(afrilearndata)
library(sf)


plot(sf::st_geometry(africountries))

path = paste0(here::here(), "/R/")
setwd(path)

data = readxl::read_xlsx("Data/CLA-Database-Data-2023_EN.xlsx", skip =  1) |> 
  as.data.frame()

cleaned_data_for_vis <- data |> 
                  dplyr::select(c(Country, `Loan (USD M)`)) |>
                  dplyr::group_by(Country) |>
                  dplyr::summarise(Total_loan = sum(`Loan (USD M)`))


geo_data <- ggplot2::map_data("world") |>
              dplyr::filter(region != "Antarctica") |>
              ggplot2::fortify()


geo_data$iso3_code <- countrycode::countrycode(geo_data$region, 
                                               origin = "country.name",
                                               destination =  'iso3c')

cleaned_data_for_vis$iso3_code <- countrycode::countrycode(cleaned_data_for_vis$Country,
                                                   origin = "country.name",
                                                   destination =  'iso3c')



data_for_vis <- dplyr::inner_join(geo_data, cleaned_data_for_vis, 
                                  by = "iso3_code")


ggplot2::ggplot(data_for_vis) +
  geom_map(data = data_for_vis, map = data_for_vis, 
           aes(group = data_for_vis$group, map_id = data_for_vis$region), 
          fill = "white", colour = "#7f7f7f", size=0.5)+
          expand_limits(x = data_for_vis$long, y= data_for_vis$lat) 


