{
  library(dplyr)
  library(ggplot2)
  library(forecast)
  library(lubridate)
  library(viridis)
  library(gganimate)
  library(knitr)
  library(SpatialPosition)
  library(cartography) 
  library(rgdal)
  library(networkD3) 
  library(DT)
  library(openxlsx)
  library(tidyr)
  library(zoo)
}

options(scipen=999)

## PRE PROCESSING

#loading data

tfp_nafta <- read.csv('TFP.csv', stringsAsFactors = FALSE)
tfp_nafta$rtfpna <- as.numeric(tfp_nafta$rtfpna)



##  EXPLORATORY ANALYSIS


## metrics and histogram
for (i in unique(tfp_nafta$isocode)){
  
  print(paste0(i, ' Variability and Central Tendency values:'))
  
  print(summary(select(filter(tfp_nafta,isocode== i),rtfpna)))
  
  print(paste0('Standard Deviation: ', sd(tfp_nafta[tfp_nafta[,1]==i,3])))
  
  hist(tfp_nafta[tfp_nafta[,1]==i,3], main = paste0('Histogram of the ', i, ' TFP from 1950 to 2011'), xlab = "Total factor productivity")
  
}

# gathered bloxplot
boxplot(tfp_nafta$rtfpna~isocode,data=tfp_nafta, main= "TFP variation from 1950 to 2011",
        xlab="Country", ylab="Total factor productivity")

# line graph
tfp_nafta %>%
  ggplot(aes(x=year, y=rtfpna, group=isocode, color=isocode)) +
  geom_line()+
  ggtitle('Evolution of TFP from 1950 to 2011')


# preparing for average growth rate and moving average
wide_tfp_nafta <- spread(tfp_nafta,isocode,rtfpna) %>% 
  mutate(CAN_var = (CAN - lag(CAN) / 1) / lag(CAN) * 100,
         MEX_var = (MEX - lag(MEX) / 1) / lag(MEX) * 100,
         USA_var = (USA - lag(USA) / 1) / lag(USA) * 100,
         CAN_ma = ma(CAN,order = 5),
         MEX_ma = ma(MEX,order = 5),
         USA_ma = ma(USA,order = 5))

# average growth rate
sapply(wide_tfp_nafta[,c('CAN_var','MEX_var','USA_var')], mean, na.rm = T)


# filling moving average NA values
wide_tfp_nafta[,8:10] <- replace(wide_tfp_nafta[,8:10], list = TRUE, lapply(wide_tfp_nafta[,8:10], na.aggregate))


#plotting moving average values
gather(wide_tfp_nafta, type, value, CAN:USA_ma, factor_key = FALSE) %>%
  filter(type %in% c('CAN','CAN_ma','USA','USA_ma','MEX','MEX_ma')) %>% 
  arrange(type) %>% 
  ggplot(aes(x=year, y=value, group=type, color=type)) +
  geom_line()+
  ggtitle('Moving Average Along With Real TFP Values from 1950 to 2011')+
  scale_color_brewer(palette = "Paired")

# decomposing 
for (i in c(2,3,4)){
  
plot(stl(ts(wide_tfp_nafta[,i], frequency = 5), 'periodic'), main=paste0(colnames(wide_tfp_nafta)[i], ' Season, Trend and Irregularities 1950 to 2011'))

}

## FORECAST

# dataframe to receive the predictions
tfp_prediction <- setNames(data.frame(matrix(nrow = 10, ncol = 4)), c('years','USA','CAN','MEX'))
tfp_prediction$years <- 2011:2020

# predicting, plotting and 
for (i in unique(tfp_nafta$isocode)) {
  
  prediction <- forecast(auto.arima(tfp_nafta[tfp_nafta$isocode==i, "rtfpna"]), h = 10)
  tfp_prediction[, i]<- prediction$mean
  
  plot(forecast(auto.arima(tfp_nafta[tfp_nafta$isocode==i, "rtfpna"]), h = 10), 
       main = paste0('Forecast of the ', i, ' TFP'), xlab = 'Year', ylab="Total factor productivity", xaxt= 'n')
  axis(1, at = 1:72, labels = 1950:2021)
  
}



####### CASE 2



# pre processing 
trade_br=read.csv('data_comexstat.csv',stringsAsFactors = FALSE)
trade_br$date <- ymd(trade_br$date)



# setting and plotting monthly evolution
trade_br %>% 
  filter(type=='Export') %>% 
  filter(product %in% c('soybean_meal', 'soybean_oil', 'soybeans')) %>% 
  group_by(date,product) %>% 
  summarise(tons=sum(tons)) %>% 
  ggplot(aes(x=date, y=tons, group=product, color=product)) +
  geom_line()+
  ggtitle('Evolution of Monthly Soy Products Exports from 1997 to 2019')

# setting and plotting annual evolution
trade_br %>% 
  mutate(year = format(date, "%Y")) %>% 
  filter(type=='Export') %>% 
  filter(product %in% c('soybean_meal', 'soybean_oil', 'soybeans')) %>% 
  group_by(year,product) %>% 
  summarise(tons=sum(tons)) %>% 
  ggplot(aes(x=year, y=tons, group=product, color=product)) +
  geom_line()+
  ggtitle('Evolution of Annual Soy Products Exports from 1997 to 2019') +
  ylab("Tons of products exported")

# setting and plotting gif viz
anim <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(type=='Export') %>% 
  filter(product %in% c('soybean_meal', 'soybean_oil', 'soybeans')) %>% 
  group_by(year,product) %>% 
  summarise(tons=sum(tons)) %>% 
  ggplot(aes(x=year, y=tons, fill=product))+
  geom_bar(stat="identity")+
  labs(title = 'Evolution of Annual Soy Products Exports from 1997 to 2019')+
  transition_time(year)+
  ease_aes('linear')+
  theme(legend.position = "left")+
  shadow_mark()+
  enter_fade()+
  theme(legend.position = "left",panel.background = element_blank(),plot.background = element_rect(fill = "#EEF3F9"),
        panel.grid = element_blank(), panel.grid.major.y = element_line(color="#DDD9EC"))+
  scale_fill_manual(values = c("#FF7B80","#484848","#BCB3D9"))

animate(anim, fps=19, nframes=200, type = "cairo-png",width = 1920, height= 1080, res=200,
        antialias = "subpixel",end_pause=35, renderer = gifski_renderer())
anim_save("teste_1_animado.gif")



#############


export_br <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(type=='Export' & year>=2015) %>%
  group_by(product) %>% 
  summarise(tons=sum(tons),usd=sum(usd))


#preparing plot
  
export_br$fraction_usd = export_br$usd / sum(export_br$usd)
export_br$fraction_tons = export_br$tons / sum(export_br$tons)

# Compute the cumulative percentages (top of each rectangle)
export_br$ymax_usd = cumsum(export_br$fraction_usd)
export_br$ymax_tons = cumsum(export_br$fraction_tons)

# Compute the bottom of each rectangle
export_br$ymin_usd = c(0, head(export_br$ymax_usd, n=-1))
export_br$ymin_tons = c(0, head(export_br$ymax_tons, n=-1))


#plot tons
ggplot(export_br, aes(ymax=ymax_tons, ymin=ymin_tons, xmax=4, xmin=3.1, fill=product)) +
  geom_rect() +
  scale_fill_brewer(palette="Set3") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")+
  ggtitle('Composition of Exported Products by Tons')
  
#plot USD
ggplot(export_br, aes(ymax=ymax_usd, ymin=ymin_usd, xmax=4, xmin=3.1, fill=product)) +
  geom_rect() +
  scale_fill_brewer(palette="Set3") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right") +
  ggtitle('Composition of Exported Products by USD')



######

# setting
trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='corn' & type=='Export' & year>=2010) %>% 
  group_by(route) %>% 
  summarise(tons=sum(tons))

# plotting
trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(type=='Export' & year>=2010) %>% 
  group_by(product,route) %>% 
  summarise(tons=sum(tons)) %>% 
  ggplot(aes(fill=route, y=tons, x=product)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Exporting Routes by product from 2010 to 2019") +
  xlab("")+
  scale_fill_brewer(palette='Dark2')


##########

# setting corn routes
corn <- trade_br %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='corn' & year >=2017) %>% 
  group_by(product,country) %>% 
  summarise(tons=sum(tons)) 
  

# setting sugar routes
sugar <- trade_br %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='sugar' & year >=2017) %>% 
  group_by(product,country) %>% 
  summarise(tons=sum(tons)) 

# preparing plot
# Download the shape file from the web:
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
my_spdf <- readOGR(dsn="world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp", verbose=FALSE) 

my_spdf@data <- left_join(my_spdf@data, corn[,2:3], by=c('NAME'='country'))
my_spdf@data <- left_join(my_spdf@data, sugar[,2:3], by=c('NAME'='country'))

#plotting corn
choroLayer(spdf = my_spdf, df = my_spdf, var = "tons.x",legend.pos = 'left',legend.title.txt = 'Tons',method = 'kmeans')
title("Brazil's  Corn Trade Partners from 2017 to 2019")

#plotting sugar
choroLayer(spdf = my_spdf, df = my_spdf, var = "tons.y",legend.pos = 'left',legend.title.txt = 'Tons',method = 'kmeans')
title("Brazil's Sugar Trade from 2017 to 2019")



###################

export_state <- trade_br %>% 
  filter(type=='Export') %>% 
  group_by(product,state) %>% 
  summarise(tons=sum(tons))

# setting
export_top_states <- trade_br %>% 
  filter(type=='Export') %>% 
  group_by(product,state) %>% 
  summarise(tons=sum(tons)) %>% 
  top_n(n=5,tons)

# preparing plot
nodes <- data.frame(name=c(as.character(export_state$product), as.character(export_state$state)) %>% unique())

export_state$product=match(export_state$product, nodes$name)-1 
export_state$state=match(export_state$state, nodes$name)-1 

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# plotting 
sankeyNetwork(Links = export_state, Nodes = nodes,
              Source = "product", Target = "state",
              Value = "tons", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20, height = 1000, width = 800)

