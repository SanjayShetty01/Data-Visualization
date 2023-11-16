options(scipen = 999)

library(here)
library(readxl)
library(dplyr)
library(ggplot2)
library(countrycode)
library(maps)
library(afrilearndata)
library(sf)
library(patchwork)
library(viridis)
library(extrafont)


extrafont::font_import(pattern = "GIL", prompt = FALSE)  # Import Gill family
extrafont::loadfonts(device="win") 

path = paste0(here::here(), "/R/")
setwd(path)

data = readxl::read_xlsx("Data/CLA-Database-Data-2023_EN.xlsx", skip =  1) |> 
  as.data.frame()

cleaned_data_for_vis <- data |> 
                  dplyr::group_by(Country) |>
                  dplyr::summarise(Total_loan = sum(`Loan (USD M)`))


geo_data <- afrilearndata::africountries


cleaned_data_for_vis$iso3_code <- countrycode::countrycode(cleaned_data_for_vis$Country,
                                                   origin = "country.name",
                                                   destination =  'iso3c')

data_for_vis <- base::merge(geo_data, cleaned_data_for_vis, 
                             by.x = "iso_a3",
                             by.y = "iso3_code", all = TRUE)

map_of_africa <- ggplot2::ggplot(data_for_vis) +
                    scale_fill_viridis(guide="none")+
                    geom_sf(aes(fill = Total_loan), show.legend = FALSE)+
                    coord_sf( crs= "+proj=ortho +lat_0=10 +lon_0=30")  +
                    theme_void()

map_of_africa

data_for_bar_vis <- base::subset(data_for_vis, 
                                 data_for_vis$Country != "Regional") |>
                    dplyr::arrange(desc(Total_loan)) |> 
                    utils::head(30)


country_name <- data_for_bar_vis$Country
id = as.numeric(row.names(data_for_bar_vis))
number_of_bar <- base::length(country_name)
angle <-  90 - 360 * (id - 0.5) /number_of_bar

data_for_bar_vis$hjust <- ifelse(angle < -90, 1,0)
data_for_bar_vis$angle <- ifelse(angle < -90, angle + 180, angle)


ymax = (base::ceiling(max(data_for_bar_vis$Total_loan)/10000) * 10000) * 10
ymin = (-ymax*6/2 - 30000)

ymax = 70000
ymin =  -25000

africa_bar_chart <- ggplot(data_for_bar_vis, aes(x = as.factor(Country), 
                                                 y =Total_loan, 
                                                 fill = Total_loan)) +
                      geom_bar(stat="identity") +
                      ylim(ymin,ymax) +
                      theme_minimal() +
                      coord_polar(start = 0) +
                      geom_text(data=data_for_bar_vis, aes(x=id, y=Total_loan+10, 
                                                           label=Country,
                                                           hjust=hjust),
                                color="black", fontface="bold",alpha=0.6, 
                                size=2.5,angle= data_for_bar_vis$angle, 
                                inherit.aes = FALSE) +
                      labs(title="Chinese Debt to African Countries",
                           caption="Data from  | Chart by Sanjaya J Shetty",
                           fill="Debt in $") +
                      viridis::scale_fill_viridis(limits= c(1000,50000), 
                                guide=guide_colorbar(title.position = "top",
                                          barwidth = 10,
                                          title.hjust = 0.5)) +
  
                      theme(
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        #plot.margin = unit(rep(1,4), "mm"),
                        legend.position="top",
                        plot.background = element_rect(fill = "white", color = NA),
                        panel.background = element_rect(fill = "white", color = NA),
                        text = element_text(family="Gill Sans MT"))



africa_bar_chart
#map_of_africa


final_plot <- africa_bar_chart + patchwork::inset_element(map_of_africa, 
                                                          left = 0.3, 
                                                          bottom = 0.28, 
                                                          right = 0.7, 
                                                          top = 0.72)

ggplot2::ggsave(plot = africa_bar_chart, filename = "plot.png", 
                height = 10, width = 7, limitsize = F)
