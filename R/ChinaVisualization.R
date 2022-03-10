#Load the Libraries
(if(!require(pacman)) install.packages('pacman'))

pacman::p_load(wbstats, dplyr, ggplot2, bbplot, wesanderson)

# Load the Data

chinaPop14 <- wb_data('SP.POP.0014.TO.ZS', country = 'China')
chinaPopB64 <- wb_data('SP.POP.1564.TO.ZS', country = 'China')
chinaPopA64 <- wb_data('SP.POP.65UP.TO.ZS', country = 'China')

# Since the population column has a weird name and different names, we need to change the same

names(chinaPop14)[names(chinaPop14) == 'SP.POP.0014.TO.ZS'] = 'population'
names(chinaPopB64)[names(chinaPopB64) == 'SP.POP.1564.TO.ZS'] = 'population'
names(chinaPopA64)[names(chinaPopA64) == 'SP.POP.65UP.TO.ZS'] = 'population'

# Create a new Column `Age Group` to differentiate the Age groups

chinaPop14$AgeGroup = 'Under14'
chinaPopB64$AgeGroup = 'Btw1464'
chinaPopA64$AgeGroup = 'Above64'

# Combine all the data in china dataframe. 

chinaComPop = rbind(chinaPop14, chinaPopA64) |> 
                            rbind(chinaPopB64)
colnames(chinaComPop)

chinaComPop =  chinaComPop |> 
                      select('date', 'population', 'AgeGroup')

chinaComPop$AgeGroup = factor(chinaComPop$AgeGroup)

Levels = levels(chinaComPop$AgeGroup)
chinaComPop$AgeGroup = factor(chinaComPop$AgeGroup, levels = sort(Levels))

# Data Visualization 

ggplot(chinaComPop, aes(x= date, y = population, fill = AgeGroup))+
  geom_area(position = "fill", size=1, colour="black")+
  scale_y_continuous(labels = scales::percent, 
                     expand = c(0,0), limits = c(0, NA))+
  scale_x_continuous(expand = c(0,0), limits = c(1960,NA))+
  geom_vline(xintercept = 1979, linetype = 'longdash', col = 'black')+
  labs(x = 'Year', y = '',
       title = 'Breakdown of Chineese Population by Age Group',
       subtitle = 'Proportion of total population (1960-2020)',
       caption = 'Data Source: World Bank')+
  theme(plot.title = element_text(size  = 13, face = 'bold'),
        plot.caption = element_text(face = 'italic', hjust = 0),
        plot.background = element_rect(fill = "#FFFACD"),
        legend.background = element_rect(fill = "#FFFACD"),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#FFFACD"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())+
  geom_curve(aes(x = 1985, y = 0.625, xend = 1979.2, yend = 0.60), 
      arrow = arrow(type = "open", length = unit(0.02, "npc")), col = 'black')+
  annotate('text', x = 1986.5, y = 0.61  ,label = '\n1979 \nOne Child Policy is \nIntroduced',
           colour = 'black')+
  scale_fill_manual(values = c("#EABE94", "#E1BD6D", "#0B775E"),
                      limits = c('Under14','Btw1464', 'Above64'),
                      name = 'Demography', labels = c('Under 14', 'Between 14 and 64', 'Above 64'))

