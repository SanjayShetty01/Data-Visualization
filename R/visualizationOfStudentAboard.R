## what am i trying to do?
## the following code would load the pdf document from the GOI website and would clean the PDF. The 
## necessary data would be later stored in a dataframe, which later would be used for visualzations

# load the Library

library(dplyr)
library(pdftools)
library(ggplot2)
library(wesanderson)
library(scales)
library(countrycode)
library(readr)

# Loading the data

data = pdf_text('https://mea.gov.in/Images/CPV/ru396_new1.pdf') %>%   #Pulling the data
         read_lines()                                                 #For better format

#Cleaning the data
# Since the first five lines contain info which do not provide any useful information, hence
#they're not necessary for the visualizations, hence we could ignore it

data = data[-(1:5)]

# Splitting the values so that it could be easier to convert it to dataframe.
values = ((strsplit(data[2:length(data)], "\\s{2,}")))              

values

# Since the data has `"` as its elements in the list hence we need to remove them. 
values = values[lapply(values, length) > 2 ]


region = sapply(values[1:99], \(x) x[2])

NStudents = sapply(values[1:99], \(x) x[3])
# Some values have commas hence we need to remove them to convert it to numeric form.

NStudents = as.numeric(gsub(',', '', NStudents))

df = data.frame(region, NStudents)

tail(df)

dim(df)


df = df[complete.cases(df),]

row.names(df) = 1:nrow(df)

# Loading the World Map data from ggplot2 library. 
geoData = map_data('world') |>  
      filter(region != "Antarctica") |>                           # removing Antarctica 
      fortify()

# The country names in both the datafrane, i.e `geoData` and `df` are not the same format.
#(eg. the df dataframe has UK and geoData has United Kingdom ) hence to have a uniformity in 
#the format we could use convert them to iso3 codes and have the same format. 

geoData$region = countrycode(geoData$region, origin = 'country.name', destination =  'iso3c') 
df$region = countrycode(df$region,origin = 'country.name', destination =  'iso3c')

# Visualzations

ggplot(geoData)+
  geom_map(data = geoData, map = geoData,
           aes(group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5)+
  expand_limits(x = geoData$long, y= geoData$lat)+
  geom_map(df, map = geoData,
           mapping = aes(fill = NStudents, map_id = region),
           colour="#7f7f7f", size=0.5)+
  scale_fill_gradientn(colours = c('#EBCC2A', '#E1AF00','#78B7C5','#3B9AB2', '#F21A00'),
                       values = rescale(c(20, 1000, 50000, 75000, 200000)))+
  annotate('text', x= 79.30,  y = 23.30, label = 'Home \n Country',
           colour = 'black', size = 2.6, fontface = 'italic')+
  labs(x="", y = "", title ='Where do Indian Students go to Study Abroad?',
       caption = 'Source:External Affairs Ministry, GOI')+
  theme(axis.text = element_blank(),
        panel.background = element_rect(fill ='#e5fffa'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size  = 18, face = 'bold', hjust = 0.5),
        plot.caption = element_text(face = 'italic', hjust = 0),
        legend.position = 'bottom',
        legend.key.width = unit(5, units = 'cm'),
        legend.background = element_rect(fill = '#e5fffa'),
        legend.title = element_blank(),
        plot.background = element_rect(fill = '#e5fffa'))



