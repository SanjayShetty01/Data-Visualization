library(dplyr)
library(rvest)
library(ggplot2)
library(ggpubr)


data = read.csv('TCPD_GE_all_2022-3-23.csv')

## Congress 

dataINC = data |>
  subset(Party_ID == '3482')


head(dataINC)

table(dataINC$Deposit_Lost)


depositINC = dataINC |>
  select(Year, Deposit_Lost) 

depositINC = data.frame(rbind(table(depositINC)))

depositINC$Year = row.names(depositINC)
row.names(depositINC) = 1:nrow(depositINC)


url = 'https://en.wikipedia.org/wiki/List_of_Indian_general_elections'

wikidata = read_html(url) |>
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table/tbody') |>
  html_table() |>
  data.frame()

electionYear = wikidata$Election.year

depositINC = depositINC[depositINC$Year %in%(electionYear),]
row.names(depositINC) = 1:nrow(depositINC)

depositINC$TotalSeats = depositINC$yes + depositINC$no

INCplot = ggplot(depositINC, aes(Year))+
  geom_col(aes(y =TotalSeats , fill = 'No. of Seats Contested'))+
  geom_text(aes(y = TotalSeats, label = TotalSeats),vjust = -0.25)+
  geom_col(aes(y = yes,fill = 'No. of Seats where they lost deposit'), width = 0.5)+
  geom_text(aes(y = yes, label = yes),vjust = -0.25)+
  labs(title = 'A Sign of Decline?', 
       subtitle = 'A Closer Look into the Indian National Congress',
       x = 'Year', y = 'Seats',
       caption = 'Data Source: Lok Dhaba, Ashoka University')+
  scale_fill_manual(values = c('Orange','#1fba00'))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill ='#fbffae'),
        plot.title = element_text(size  = 18, face = 'bold'),
        plot.caption = element_text(face = 'italic', hjust = 0),
        plot.background = element_rect(fill = '#fbffae'),
        legend.title = element_blank(),
        legend.background = element_rect(fill = '#fbffae'))



## BJP 

dataBJP = data |>
  subset(Party_ID == '1605')


head(dataBJP)

table(dataBJP$Deposit_Lost)



depositBJP = dataBJP |>
  select(Year, Deposit_Lost) 

depositBJP = data.frame(rbind(table(depositBJP)))

depositBJP$Year = row.names(depositBJP)
row.names(depositBJP) = 1:nrow(depositBJP)

electionYear = wikidata$Election.year

depositBJP = depositBJP[depositBJP$Year %in%(electionYear),]
row.names(depositBJP) = 1:nrow(depositBJP)

depositBJP$TotalSeats = depositBJP$yes + depositBJP$no

BJPplot = ggplot(depositBJP, aes(Year))+
  geom_col(aes(y =TotalSeats , fill = 'No. of Seats Contested'))+
  geom_text(aes(y = TotalSeats, label = TotalSeats),vjust = -0.25)+
  geom_col(aes(y = yes,fill = 'No. of Seats where they lost deposit'), width = 0.5)+
  geom_text(aes(y = yes, label = yes),vjust = -0.25)+
  labs(title = 'What About the BJP?', 
       x = 'Year', y = 'Seats',
       caption = 'Data Source: Lok Dhaba, Ashoka University')+
  scale_fill_manual(values = c('Orange','#1fba00'))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill ='#fbffae'),
        plot.title = element_text(size  = 18, face = 'bold'),
        plot.caption = element_text(face = 'italic', hjust = 0),
        plot.background = element_rect(fill = '#fbffae'),
        legend.title = element_blank(),
        legend.background = element_rect(fill = '#fbffae'))


ggarrange(INCplot, BJPplot)
