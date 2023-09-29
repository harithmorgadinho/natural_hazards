#pipeline_probability_threat_disasters version 2
library(sf)
sf_use_s2(F)
library(dplyr)
library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')
library(raster)

r = raster(extent(world_map),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)
r_grid = st_transform(r_grid,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

#vulcanos----
vulcanos = st_read('/Users/gdt366/Dropbox/disaster_project/hazards/1950-present (311 events).kml')
vulcanos = st_transform(vulcanos,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

library(stringr)



vulcanos$Description = gsub(vulcanos$Description,pattern = '</th><td>',replacement = '')
vulcanos$Description = gsub(vulcanos$Description,pattern = '</td></tr>',replacement = '')
vulcanos$Description = gsub(vulcanos$Description,pattern = '<tr><th>',replacement = '')
vulcanos$Description = gsub(vulcanos$Description,pattern = "https://www.ngdc.noaa.gov/nndc/struts/results?t=102557&s=1&d=230,145,175,180&nd=display&eq_0=6126\">Get more details from NGDC Natural Hazards Website</a> <br>",replacement = '')
vulcanos$Description = gsub(vulcanos$Description,pattern = "(VEI)",replacement = '_VEI_',fixed=TRUE)




test1 = str_split(vulcanos$Description,pattern = 'VEI_:')


func_vei = function(x){
  return(str_split(test1[[x]][2],pattern = ' ')[[1]][1])
}

vulcanos$magnitude = unlist(lapply(1:length(test1),FUN =func_vei))

vulcanos$Name

test2 = str_split(vulcanos$Name,pattern = ': ')


func_year = function(x){
  return(tail(test2[[x]],1))
}


vulcanos$year = unlist(lapply(1:length(test2),FUN =func_year))

vulcanos_subset = vulcanos[vulcanos$year>1969 & vulcanos$year < 2017,]
nrow(vulcanos_subset)
library(scales)
vulcanos_subset$magnitude_scaled <- rescale(as.numeric(vulcanos_subset$magnitude), to = c(1, 10))



sf_intersected = st_intersects(vulcanos_subset,r_grid)

x = t(sf_intersected)

sf_to_use = t(sf_intersected)

metrics_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value1 = NA
    value2 = NA
    value3 = NA
    value4 = NA
  }else{
    
    value = vulcanos_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)),
                                                                    medianmagnitude = median(as.numeric(magnitude_scaled)))
    value1 = value$totalmagnitude
    value2 = value$medianmagnitude
    value3 = vulcanos_subset[t(sf_intersected)[[x]],] %>% group_by(Name,year) %>% count() %>% nrow()
    value4 = vulcanos_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    value4 = value4 %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)))
    value4 = value4$totalmagnitude
  }
  
  
  return(list(value1,value2,value3,value4))
}

vulcanos_metrics <- lapply(1:length(sf_to_use), FUN = metrics_layer)


fun_value1 = function(x){
  return(unlist(vulcanos_metrics[[x]])[1])
}

fun_value2 = function(x){
  return(unlist(vulcanos_metrics[[x]])[2])
}

fun_value3 = function(x){
  return(unlist(vulcanos_metrics[[x]])[3])
}

fun_value4 = function(x){
  return(unlist(vulcanos_metrics[[x]])[4])
}

value1_vulcanos = unlist(lapply(1:length(vulcanos_metrics),fun_value1))
value2_vulcanos = unlist(lapply(1:length(vulcanos_metrics),fun_value2))
value3_vulcanos = unlist(lapply(1:length(vulcanos_metrics),fun_value3))
value4_vulcanos = unlist(lapply(1:length(vulcanos_metrics),fun_value4))

r_grid$sum_vulcanos = as.numeric(value1_vulcanos)
r_grid$median_vulcanos = as.numeric(value2_vulcanos)
r_grid$vulcanos_freq = as.numeric(value3_vulcanos)
r_grid$sum_vulcanos_by_year = as.numeric(value4_vulcanos)

vulcanos_subset$magnitude_scaled = 11 - vulcanos_subset$magnitude_scaled
sf_intersected = st_intersects(vulcanos_subset,r_grid)
sf_to_use = t(sf_intersected)

weigth0 = max(na.omit(r_grid$sum_vulcanos_by_year))

weight_layer_fixed = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = vulcanos_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    }else{
      temp_sf = vulcanos_subset[t(sf_intersected)[[x]],]   
    }
    
    
    list_ones = as.numeric(temp_sf$year) - 1969
    
    list_zeros[list_ones] = 1
    
    sf_weight = rep(weigth0,length(years))
    sf_weight_magnitude = temp_sf$magnitude_scaled
    
    sf_weight[list_ones] = sf_weight_magnitude 
    
    
    value =  predict(glm(list_zeros ~ 1, weights = 1/sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

years = 1970:2016
table(vulcanos_subset$magnitude)

probability_vulcanos_fixed <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer_fixed))

r_grid$vulcanos_prob_b = probability_vulcanos_fixed


library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'
library(ggplot2)
world_map_moll = st_transform(world_map,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))


save(r_grid,file = 'r_grid_hurricanes_plus_vulcanos.Rdata')

vulcano_buffer = st_buffer(st_centroid(r_grid[r_grid$vulcanos_freq>0,]),400000)
plot(vulcano_buffer$geometry)
plot(r_grid$geometry)
p1 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = vulcano_buffer,aes(col = vulcanos_freq),linewidth = 0.2,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[r_grid$vulcanos_freq>0,],aes(fill = vulcanos_freq),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Frequency (vulcanos)',colours = pal1(5),breaks = c(1,5,10,13))+
  scale_colour_gradientn('Frequency (vulcanos)',colours = alpha(pal1(5),0.75),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

p2 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = vulcano_buffer,aes(col = sum_vulcanos),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$median_vulcanos),],aes(fill = median_vulcanos),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Median magnitude (vulcanos)',colours = pal1(5))+
  scale_colour_gradientn('Median magnitude (vulcanos)',colours = alpha(pal1(5),0.75),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9))+ 
  guides(fill = guide_colourbar(title.position = "top"))

p3 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = vulcano_buffer,aes(col = sum_vulcanos),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$sum_vulcanos),],aes(fill = sum_vulcanos),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Total magnitude (vulcanos)',colours = pal1(5))+
  scale_colour_gradientn('Total magnitude (vulcanos)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9))+ 
  guides(fill = guide_colourbar(title.position = "top"))

p4 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = vulcano_buffer,aes(col = vulcanos_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$vulcanos_prob_b),],aes(fill = vulcanos_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (vulcanos)',colours = pal1(5))+
  scale_colour_gradientn('Impact (vulcanos)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

library(gridExtra)
grid_plot1 = arrangeGrob(p1,p2,p3,p4,ncol =4)


cor(r_grid[r_grid$vulcanos_freq>0,]$vulcanos_freq,r_grid[r_grid$vulcanos_freq>0,]$sum_vulcanos)
cor(r_grid[r_grid$vulcanos_freq>0,]$vulcanos_freq,r_grid[r_grid$vulcanos_freq>0,]$vulcanos_prob_b)
cor(r_grid[r_grid$vulcanos_freq>0,]$sum_vulcanos,r_grid[r_grid$vulcanos_freq>0,]$vulcanos_prob_b)
cor(r_grid[r_grid$vulcanos_freq>0,]$median_vulcanos,r_grid[r_grid$vulcanos_freq>0,]$vulcanos_prob_b)
cor(r_grid[r_grid$vulcanos_freq>0,]$median_vulcanos,r_grid[r_grid$vulcanos_freq>0,]$vulcanos_freq)

ggsave(filename = 'vulcanos_moll_grid_plot1vmedian.png',plot = grid_plot1,dpi = 2000,width = 10,height = 3)


#earthqwake----

earthqwake = st_read('/Users/gdt366/Dropbox/disaster_project/1950-present (2407 events)_earthqwake.kml')
earthqwake = st_transform(earthqwake,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

plot(earthqwake$geometry)

library(stringr)


earthqwake$Description = gsub(earthqwake$Description,pattern = '</th><td>',replacement = '')
earthqwake$Description = gsub(earthqwake$Description,pattern = '</td></tr>',replacement = '')

test1 = str_split(earthqwake$Description,pattern = 'Earthquake Magnitude:')


func_vei = function(x){
  return(str_split(test1[[x]][2],pattern = ' ')[[1]][1])
}

earthqwake$magnitude = unlist(lapply(1:length(test1),FUN = func_vei))

func_year = function(x){
  if(length(grep(earthqwake$Name[x],pattern = '/'))>0){
    value = unlist(str_split(earthqwake$Name[x],pattern = '/'))[1]
  }else{
    value = unlist(str_split(earthqwake$Name[x],pattern = ': '))[2]
  }
  return(value)
}


earthqwake$year = unlist(lapply(1:nrow(earthqwake),FUN =func_year))

earthqwake[earthqwake$magnitude == 'Unknown',]$year
earthqwake_subset = earthqwake[earthqwake$year>1969 & earthqwake$year < 2017,]

earthqwake_subset[earthqwake_subset$magnitude == 'Unknown',]$year

earthqwake_subset = earthqwake_subset[earthqwake_subset$magnitude!= 'Unknown',]
nrow(earthqwake_subset)

years = 1970:max(earthqwake_subset$year)

table(earthqwake_subset$magnitude)

earthqwake_subset$magnitude_scaled <- rescale(as.numeric(earthqwake_subset$magnitude), to = c(1, 10))


sf_intersected = st_intersects(earthqwake_subset,r_grid)

x = t(sf_intersected)

metrics_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value1 = NA
    value2 = NA
    value3 = NA
    value4 = NA
  }else{
    
    value = earthqwake_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)),
                                                                   medianmagnitude = median(as.numeric(magnitude_scaled)))
    value1 = value$totalmagnitude
    value2 = value$medianmagnitude
    value3 = earthqwake_subset[t(sf_intersected)[[x]],] %>% group_by(Name,year) %>% count() %>% nrow()
    value4 = earthqwake_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    value4 = value4 %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)))
    value4 = value4$totalmagnitude
  }
  
  
  return(list(value1,value2,value3,value4))
}
sf_to_use = t(sf_intersected)

earthquake_metrics <- lapply(1:length(sf_to_use), FUN = metrics_layer)


fun_value1 = function(x){
  return(unlist(earthquake_metrics[[x]])[1])
}

fun_value2 = function(x){
  return(unlist(earthquake_metrics[[x]])[2])
}

fun_value3 = function(x){
  return(unlist(earthquake_metrics[[x]])[3])
}

fun_value4 = function(x){
  return(unlist(earthquake_metrics[[x]])[4])
}

value1_earthquakes = unlist(lapply(1:length(earthquake_metrics),fun_value1))
value2_earthquakes = unlist(lapply(1:length(earthquake_metrics),fun_value2))
value3_earthquakes = unlist(lapply(1:length(earthquake_metrics),fun_value3))
value4_earthquakes = unlist(lapply(1:length(earthquake_metrics),fun_value4))

# 
# sum_layer = function(x){
#   print(x)
#   if(length(unlist(sf_to_use[x])) == 0){
#     value = NA
#   }else{
#     
#     value = earthqwake_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)))
#     value = value$totalmagnitude
#   }
#   
#   
#   return(value)
# }
# median_layer = function(x){
#   print(x)
#   if(length(unlist(sf_to_use[x])) == 0){
#     value = NA
#   }else{
#     
#     value = earthqwake_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = median(as.numeric(magnitude_scaled)))
#     value = value$totalmagnitude
#   }
#   
#   
#   return(value)
# }


# sum_earthqwakes <- unlist(lapply(1:length(sf_to_use), FUN = sum_layer))
# median_earthqwakes <- unlist(lapply(1:length(sf_to_use), FUN = median_layer))


r_grid$sum_earthquakes = as.numeric(value1_earthquakes)
r_grid$median_earthquakes = as.numeric(value2_earthquakes)
r_grid$earthquakes_freq = as.numeric(value3_earthquakes)
r_grid$sum_earthquakes_by_year = as.numeric(value4_earthquakes)


earthqwake_subset$magnitude_scaled = 11 - earthqwake_subset$magnitude_scaled

sf_intersected = st_intersects(earthqwake_subset,r_grid)

weigth0 = max(na.omit(r_grid$sum_earthquakes))

weight_layer_fixed = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(earthqwake_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(earthqwake_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = earthqwake_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    }else{
      temp_sf = earthqwake_subset[t(sf_intersected)[[x]],]   
    }
    
    
    list_ones = as.numeric(temp_sf$year) - 1969
    
    list_zeros[list_ones] = 1
    
    sf_weight = rep(weigth0,length(years))
    sf_weight_magnitude = temp_sf$magnitude_scaled
    
    sf_weight[list_ones] = sf_weight_magnitude 
    
    
    value =  predict(glm(list_zeros ~ 1, weights = 1/sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

#frequency = unlist(lapply(sf_to_use, FUN = length))

table(r_grid$earthqwakes_freq)

# probability_vulcanos <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
# r_grid$vulcanos_prob = probability_vulcanos
sf_to_use = t(sf_intersected)

probability_earthqwakes_fixed <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer_fixed))

r_grid$earthquakes_prob_b = probability_earthqwakes_fixed


save(r_grid,file = 'r_grid_hurricanes_plus_vulcanos_plus_earthquakes.Rdata')



# probability_earthqwake <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
# r_grid$earthqwake_prob = probability_earthqwake


pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
earthquake_buffer = st_buffer(st_centroid(r_grid[r_grid$earthquakes_freq>0,]),200000)

p1 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = earthquake_buffer[order(earthquake_buffer$earthquakes_freq),],aes(col = earthquakes_freq),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[r_grid$earthquakes_freq>0,],aes(fill = earthquakes_freq,col = earthquakes_freq),linewidth = 0.01)+
  scale_fill_gradientn('Frequency (earthquakes)',colours = pal1(5),breaks = c(1,4,8))+
  scale_colour_gradientn('Impact (earthquakes)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p2 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = earthquake_buffer[order(earthquake_buffer$median_earthquakes),],aes(col = median_earthquakes),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$median_earthquakes),],aes(fill = median_earthquakes, col = median_earthquakes),linewidth = 0.01)+
  scale_fill_gradientn('Median magnitude (earthquakes)',colours = pal1(5))+
  scale_colour_gradientn('Impact (earthquakes)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p3 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = earthquake_buffer[order(earthquake_buffer$sum_earthquakes),],aes(col = sum_earthquakes),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$sum_earthquakes),],aes(fill = sum_earthquakes,col = sum_earthquakes),linewidth = 0.01)+
  scale_fill_gradientn('Total magnitude (earthquakes)',colours = pal1(5))+
  scale_colour_gradientn('Impact (earthquakes)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p4 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = earthquake_buffer[order(earthquake_buffer$earthquakes_prob_b),],aes(col = earthquakes_prob_b),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$earthquakes_prob_b),],aes(fill = earthquakes_prob_b,col = earthquakes_prob_b),linewidth = 0.01)+
  scale_fill_gradientn('Impact (earthquakes)',colours = pal1(5))+
  scale_colour_gradientn('Impact (earthquakes)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

library(gridExtra)
grid_plot2 = arrangeGrob(p1,p2,p3,p4,ncol =4)


cor(r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq,r_grid[r_grid$earthquakes_freq>0,]$sum_earthquakes)
cor(r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$sum_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$median_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$median_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq)




ggsave(filename = 'earthqwakes_moll_grid_plot1vmedian_fixed.png',plot = grid_plot2,dpi = 2000,width = 10,height = 3)

save(r_grid,file = 'r_grid_pipeline2.Rdata')


max(na.omit(r_grid$tsunamis_prob_b))
max(na.omit(r_grid$earthquakes_prob_b))
max(na.omit(r_grid$vulcanos_prob_b))

## tsunamis ----
library(sf)
sf_use_s2(F)
library(ggplot2)
tsunamis = st_read('/Users/gdt366/Dropbox/disaster_project/tsunamis_qgis_final.shp')
tsunamis = st_transform(tsunamis,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")



colnames(tsunamis)


tsunamis$descriptio = gsub(tsunamis$descriptio,pattern = '</th><td>',replacement = '')
tsunamis$descriptio = gsub(tsunamis$descriptio,pattern = '</td></tr>',replacement = '')


tsunamis$magnitude = gsub(tsunamis$snippet,pattern = 'Max Water Height: ',replacement = '')
tsunamis$magnitude = gsub(tsunamis$magnitude,pattern = ' m',replacement = '')

library(stringr)
test1 = str_split(tsunamis$descriptio,pattern = 'Source Event: ')



func_year = function(x){
  
  return(unlist(str_split(test1[[x]][2],pattern = '/'))[1])
  
}


tsunamis$year = unlist(lapply(1:nrow(tsunamis),FUN =func_year))



unique(tsunamis$year)

tsunamis[tsunamis$magnitude == 'Unknown',]$year

tsunamis_subset = tsunamis[tsunamis$year>1969 & tsunamis$year < 2017,]

tsunamis_subset[tsunamis_subset$magnitude == 'Unknown',]$year

tsunamis_subset = tsunamis_subset[tsunamis_subset$magnitude!= 'Unknown',]

tsunamis_subset = tsunamis_subset[!is.na(tsunamis_subset$year),]

tsunamis_subset$magnitude = as.numeric(tsunamis_subset$magnitude)

table(tsunamis_subset$magnitude)
###


#https://indico.ictp.it/event/a06194/session/9/contribution/7/material/0/0.pdf

tsunamis_subset$magnitude_scaled = NA
tsunamis_subset[tsunamis_subset$magnitude<2,]$magnitude_scaled = 0
tsunamis_subset[tsunamis_subset$magnitude>1.99 & tsunamis_subset$magnitude<4,]$magnitude_scaled = 1
tsunamis_subset[tsunamis_subset$magnitude>3.99 & tsunamis_subset$magnitude<8,]$magnitude_scaled = 2
tsunamis_subset[tsunamis_subset$magnitude>7.99 & tsunamis_subset$magnitude<16,]$magnitude_scaled = 3
tsunamis_subset[tsunamis_subset$magnitude>15.99 & tsunamis_subset$magnitude<32,]$magnitude_scaled = 4
tsunamis_subset[tsunamis_subset$magnitude>31.99,]$magnitude_scaled = 5

table(tsunamis_subset$magnitude_scaled)

tsunamis_subset$magnitude_scaled <- rescale(as.numeric(tsunamis_subset$magnitude_scaled), to = c(1, 10))
nrow(tsunamis_subset)
sf_intersected = st_intersects(tsunamis_subset,r_grid)

x = t(sf_intersected)
sf_to_use = t(sf_intersected)

metrics_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value1 = NA
    value2 = NA
    value3 = NA
    value4 = NA
  }else{
    
    value = tsunamis_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)),
                                                                   medianmagnitude = median(as.numeric(magnitude_scaled)))
    value1 = value$totalmagnitude
    value2 = value$medianmagnitude
    value3 = tsunamis_subset[t(sf_intersected)[[x]],] %>% group_by(Name,year) %>% count() %>% nrow()
    value4 = tsunamis_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    value4 = value4 %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)))
    value4 = value4$totalmagnitude
  }
  
  
  return(list(value1,value2,value3,value4))
}

tsunamis_metrics <- lapply(1:length(sf_to_use), FUN = metrics_layer)


fun_value1 = function(x){
  return(unlist(tsunamis_metrics[[x]])[1])
}

fun_value2 = function(x){
  return(unlist(tsunamis_metrics[[x]])[2])
}

fun_value3 = function(x){
  return(unlist(tsunamis_metrics[[x]])[3])
}

fun_value4 = function(x){
  return(unlist(tsunamis_metrics[[x]])[4])
}
value1_tsunamis = unlist(lapply(1:length(tsunamis_metrics),fun_value1))
value2_tsunamis = unlist(lapply(1:length(tsunamis_metrics),fun_value2))
value3_tsunamis = unlist(lapply(1:length(tsunamis_metrics),fun_value3))
value4_tsunamis = unlist(lapply(1:length(tsunamis_metrics),fun_value4))

r_grid$sum_tsunamis = as.numeric(value1_tsunamis)
r_grid$median_tsunamis = as.numeric(value2_tsunamis)
r_grid$tsunamis_freq = as.numeric(value3_tsunamis)
r_grid$sum_tsunamis_by_year = as.numeric(value4_tsunamis)


tsunamis_subset$magnitude_scaled = 11 - tsunamis_subset$magnitude_scaled
sf_intersected = st_intersects(tsunamis_subset,r_grid)
sf_to_use = t(sf_intersected)

weigth0 = max(na.omit(r_grid$sum_tsunamis_by_year))

weight_layer_fixed = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(tsunamis_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(tsunamis_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = tsunamis_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    }else{
      temp_sf = tsunamis_subset[t(sf_intersected)[[x]],]   
    }
    
    
    list_ones = as.numeric(temp_sf$year) - 1969
    
    list_zeros[list_ones] = 1
    
    sf_weight = rep(weigth0,length(years))
    sf_weight_magnitude = temp_sf$magnitude_scaled
    
    sf_weight[list_ones] = sf_weight_magnitude 
    
    
    value =  predict(glm(list_zeros ~ 1, weights = 1/sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}


# probability_vulcanos <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
# r_grid$vulcanos_prob = probability_vulcanos

probability_tsunamis_fixed <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer_fixed))

r_grid$tsunamis_prob_b = probability_tsunamis_fixed


# probability_earthqwake <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
# r_grid$earthqwake_prob = probability_earthqwake


pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))
pal1 = colorRampPalette(c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'))
tsunamis_buffer = st_buffer(st_centroid(r_grid[r_grid$tsunamis_freq>0,]),200000)
p1 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$tsunamis_freq),],aes(col = tsunamis_freq),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[r_grid$tsunamis_freq>0,],aes(fill = tsunamis_freq,col = tsunamis_freq),linewidth = 0.02)+
  scale_fill_gradientn('Frequency (tsunamis)',colours = pal1(5),breaks = c(1,30,50,89))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p2 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$median_tsunamis),],aes(col = median_tsunamis),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$median_tsunamis),],aes(fill = median_tsunamis, col = median_tsunamis),linewidth = 0.02)+
  scale_fill_gradientn('Median magnitude (tsunamis)',colours = pal1(5))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p3 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$sum_tsunamis_by_year),],aes(col = sum_tsunamis_by_year),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$sum_tsunamis_by_year),],aes(fill = sum_tsunamis_by_year,col = sum_tsunamis_by_year),linewidth = 0.02)+
  scale_fill_gradientn('Total magnitude (tsunamis)',colours = pal1(5))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p4 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$tsunamis_prob_b),],aes(col = tsunamis_prob_b),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$tsunamis_prob_b),],aes(fill = tsunamis_prob_b,col = tsunamis_prob_b),linewidth = 0.02)+
  scale_fill_gradientn('Impact (tsunamis)',colours = pal1(5))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

library(gridExtra)
grid_plot3 = arrangeGrob(p1,p2,p3,p4,ncol =4)


cor(r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq,r_grid[r_grid$earthquakes_freq>0,]$sum_earthquakes)
cor(r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$sum_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$median_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$median_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq)

ggsave(filename = 'tsunamis_moll_grid_plot1vmedian.png',plot = grid_plot3,dpi = 2000,width = 10,height = 3)

save(r_grid,file = 'r_grid_pipeline3.Rdata')

colnames(r_grid)

#hurricanes ----

library(sf)
sf_use_s2(F)
library(ggplot2)
library(raster)

load('r_grid_pipeline3.Rdata')


hurricane = st_read('/Users/gdt366/Dropbox/disaster_project/IBTrACS.ALL.list.v04r00.lines/IBTrACS.ALL.list.v04r00.lines.shp')
hurricane_subset = hurricane[hurricane$year>1969 & hurricane$year < 2017,]
nrow(hurricane_subset)
library(raster)
e = extent(-179.9,179.9,-89.9,89.9)
hurricane_subset = st_crop(hurricane_subset,e)
hurricane_subset = st_transform(hurricane_subset,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

plot(hurricane_subset$geometry)

colnames(hurricane_subset)

head(hurricane_subset)
table(hurricane_subset$win)

table(hurricane_subset$USA_SSHS)
library(scales)
hurricane_subset$magnitude_scaled <- rescale(as.numeric(hurricane_subset$USA_SSHS), to = c(1, 10))

colnames(hurricane_subset)[6] = 'Name'
hurricane_subset$Name
sf_intersected = st_intersects(hurricane_subset,r_grid)

x = t(sf_intersected)

sf_to_use = t(sf_intersected)
x = 18350


metrics_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value1 = NA
    value2 = NA
    value3 = NA
    value4 = NA
  }else{
    
    value = hurricane_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)),
                                                                   medianmagnitude = median(as.numeric(magnitude_scaled)))
    value1 = value$totalmagnitude
    value2 = value$medianmagnitude
    value3 = hurricane_subset[t(sf_intersected)[[x]],] %>% group_by(Name,year) %>% count() %>% nrow()
    value4 = hurricane_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    value4 = value4 %>% summarise(totalmagnitude = sum(as.numeric(magnitude_scaled)))
    value4 = value4$totalmagnitude
  }
  
  
  return(list(value1,value2,value3,value4))
}

hurricanes_metrics <- lapply(1:length(sf_to_use), FUN = metrics_layer)


fun_value1 = function(x){
  return(unlist(hurricanes_metrics[[x]])[1])
}

fun_value2 = function(x){
  return(unlist(hurricanes_metrics[[x]])[2])
}

fun_value3 = function(x){
  return(unlist(hurricanes_metrics[[x]])[3])
}

fun_value4 = function(x){
  return(unlist(hurricanes_metrics[[x]])[4])
}

value1_hurricanes = unlist(lapply(1:length(hurricanes_metrics),fun_value1))
value2_hurricanes = unlist(lapply(1:length(hurricanes_metrics),fun_value2))
value3_hurricanes = unlist(lapply(1:length(hurricanes_metrics),fun_value3))
value4_hurricanes = unlist(lapply(1:length(hurricanes_metrics),fun_value4))

r_grid$sum_hurricanes = as.numeric(value1_hurricanes)
r_grid$median_hurricanes = as.numeric(value2_hurricanes)
r_grid$hurricanes_freq = as.numeric(value3_hurricanes)
r_grid$sum_hurricanes_by_year = as.numeric(value4_hurricanes)


save(r_grid,file = 'r_grid_hurricane_metrics_temp.Rdata')

hurricane_subset$magnitude_scaled = 11 - hurricane_subset$magnitude_scaled
sf_intersected = st_intersects(hurricane_subset,r_grid)
sf_to_use = t(sf_intersected)

weigth0 = max(na.omit(r_grid$sum_hurricanes_by_year))

weight_layer_fixed = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(hurricane_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(hurricane_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = hurricane_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude_scaled = max(magnitude_scaled))
    }else{
      temp_sf = hurricane_subset[t(sf_intersected)[[x]],]   
    }
    
    
    list_ones = as.numeric(temp_sf$year) - 1969
    
    list_zeros[list_ones] = 1
    
    sf_weight = rep(weigth0,length(years))
    sf_weight_magnitude = temp_sf$magnitude_scaled
    
    sf_weight[list_ones] = sf_weight_magnitude 
    
    
    value =  predict(glm(list_zeros ~ 1, weights = 1/sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}


# probability_vulcanos <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
# r_grid$vulcanos_prob = probability_vulcanos

years = 1970:max(hurricane_subset$year)

probability_hurricanes_fixed <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer_fixed))

r_grid$hurricanes_prob_b = probability_hurricanes_fixed

#save(r_grid,file ='r_grid_pipeline4.Rdata')

# probability_earthqwake <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
# r_grid$earthqwake_prob = probability_earthqwake


pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))
pal1 = colorRampPalette(c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'))
tsunamis_buffer = st_buffer(st_centroid(r_grid[r_grid$tsunamis_freq>0,]),200000)
p1 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$tsunamis_freq),],aes(col = tsunamis_freq),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[r_grid$tsunamis_freq>0,],aes(fill = tsunamis_freq,col = tsunamis_freq),linewidth = 0.02)+
  scale_fill_gradientn('Frequency (tsunamis)',colours = pal1(5),breaks = c(1,30,50,89))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p2 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$median_tsunamis),],aes(col = median_tsunamis),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$median_tsunamis),],aes(fill = median_tsunamis, col = median_tsunamis),linewidth = 0.02)+
  scale_fill_gradientn('Median magnitude (tsunamis)',colours = pal1(5))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p3 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$sum_tsunamis_by_year),],aes(col = sum_tsunamis_by_year),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$sum_tsunamis_by_year),],aes(fill = sum_tsunamis_by_year,col = sum_tsunamis_by_year),linewidth = 0.02)+
  scale_fill_gradientn('Total magnitude (tsunamis)',colours = pal1(5))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

p4 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[order(tsunamis_buffer$tsunamis_prob_b),],aes(col = tsunamis_prob_b),linewidth = 0.05,fill = NA,alpha = 0.75)+
  geom_sf(data = r_grid[!is.na(r_grid$tsunamis_prob_b),],aes(fill = tsunamis_prob_b,col = tsunamis_prob_b),linewidth = 0.02)+
  scale_fill_gradientn('Impact (tsunamis)',colours = pal1(5))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal1(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.title = element_text(family = myfont,size = 9,hjust = 0.5))+
  guides(fill = guide_colourbar(title.position = "top"))

library(gridExtra)
grid_plot3 = arrangeGrob(p1,p2,p3,p4,ncol =4)


cor(r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq,r_grid[r_grid$earthquakes_freq>0,]$sum_earthquakes)
cor(r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$sum_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$median_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_prob_b)
cor(r_grid[r_grid$earthquakes_freq>0,]$median_earthquakes,r_grid[r_grid$earthquakes_freq>0,]$earthquakes_freq)

ggsave(filename = 'tsunamis_moll_grid_plot1vmedian.png',plot = grid_plot3,dpi = 2000,width = 10,height = 3)

save(r_grid,file = 'r_grid_pipeline2.Rdata')



library(sf)

hurricane = st_read('/Users/gdt366/Dropbox/disaster_project/IBTrACS.ALL.list.v04r00.lines/IBTrACS.ALL.list.v04r00.lines.shp')
hurricane = st_transform(hurricane,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


hurricane_subset = hurricane[hurricane$year>1970,]
plot(hurricane_subset[1,]$geometry)
library(dplyr)
test = hurricane_subset %>% group_by(SID,STORM_SPD,year) %>% summarise()
nrow(test)
plot(test[test$SID == '1970364S15165',]$geometry)
years = 1970:max(hurricanes_subset$year)
library(dplyr)
weight_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(hurricanes_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(hurricanes_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = hurricanes_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude = max(magnitude))
    }else{
      temp_sf = hurricanes_subset[t(sf_intersected)[[x]],]   
    }
    
    
    list_ones = as.numeric(temp_sf$year) - 1970
    
    list_zeros[list_ones] = 1
    
    sf_weight = rep(1,length(years))
    sf_weight_magnitude = temp_sf$magnitude
    
    sf_weight[list_ones] = sf_weight[list_ones] + as.numeric(sf_weight_magnitude) 
    
    
    value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}
hurricanes_subset = test


# library(rnaturalearth)
# world_map = ne_countries(scale = 10,returnclass = 'sf')
# library(raster)
# r = raster(extent(world_map),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
# 
# r_grid = st_as_sf(rasterToPolygons(r))
# r_grid$layer = 1:nrow(r_grid)
hurricanes_subset[is.na(hurricanes_subset$year),]
hurricanes_subset$magnitude = hurricanes_subset$STORM_SPD
sf_intersected = st_intersects(hurricanes_subset,r_grid)
sf_to_use = t(sf_intersected)
probability_hurricane <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))

r_grid$hurricane_probs = probability_hurricane
colnames(r_grid)
save(r_grid,file = 'r_grid_all_moll.Rdata')

r_grid_final = r_grid

save(r_grid_final,file = 'r_grid_all_moll.Rdata')


load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/r_grid_hurricane.Rdata')

r_grid_final$hurricane_prob = r_grid$hurricane_probs

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

colnames(r_grid)
p = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid_final[!is.na(r_grid_final$hurricane_prob),],aes(fill = hurricane_prob,col = hurricane_prob),linewidth = 0.01)+
  scale_fill_gradientn('Probability of impact (hurricanes)',colours = pal1(10))+
  scale_colour_gradientn('Probability of impact (hurricanes)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(3,'cm'),
        legend.title = element_text(family = myfont))


ggsave(filename = 'hurricanes.png',plot = p,dpi = 1000,width = 10,height = 5)


#the four threats
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))


p1 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid_final[!is.na(r_grid_final$vulcanos_prob),],aes(fill = vulcanos_prob,col = vulcanos_prob),linewidth = 0.01)+
  scale_fill_gradientn('Probability of impact (vulcano)',colours = pal1(10))+
  scale_colour_gradientn('Probability of impact (vulcano)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(2,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p2 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid_final[!is.na(r_grid_final$earthqwake_prob),],aes(fill = earthqwake_prob,col = earthqwake_prob),linewidth = 0.01)+
  scale_fill_gradientn('Probability of impact (earthqwake)',colours = pal1(10))+
  scale_colour_gradientn('Probability of impact (earthqwake)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(2,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p3 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid_final[!is.na(r_grid_final$tsunami_prob),],aes(fill = tsunami_prob,col = tsunami_prob),linewidth = 0.01)+
  scale_fill_gradientn('Probability of impact (tsunami)',colours = pal1(10))+
  scale_colour_gradientn('Probability of impact (tsunami)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(2,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid_final[!is.na(r_grid_final$hurricane_prob),],aes(fill = hurricane_prob,col = hurricane_prob),linewidth = 0.01)+
  scale_fill_gradientn('Probability of impact (hurricane)',colours = pal1(10))+
  scale_colour_gradientn('Probability of impact (hurricane)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(2,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

library(gridExtra)
gridded_maps = arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'all_disasters.png',plot = gridded_maps,dpi = 2000,width = 8,height = 4)

save(r_grid_final,file = 'r_grid_final_all_disasters.Rdata')



library(corrplot)
colnames(r_grid)

r_grid_geom_removed = r_grid
st_geometry(r_grid_geom_removed) = NULL
str(r_grid_geom_removed)


#corrplot----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))
correlation_matrix <- cor(r_grid_geom_removed[,c('vulcanos_freq','median_vulcanos','sum_vulcanos','vulcanos_prob_b')][r_grid_geom_removed$vulcanos_freq>0,])
corrplot1 = corrplot(correlation_matrix, method = "circle",type = "lower",tl.srt = 45,addCoef.col = "black",tl.col = 'black',
                     col = pal1(100))

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
correlation_matrix <- cor(r_grid_geom_removed[,c('earthquakes_freq','median_earthquakes','sum_earthquakes_by_year','earthquakes_prob_b')][!is.na(r_grid_geom_removed$earthquakes_freq),])
corrplot2 = corrplot(correlation_matrix, method = "circle",type = "lower",tl.srt = 45,addCoef.col = "black",tl.col = 'black',
                     col = pal1(100))
pal1 = colorRampPalette(c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'))
correlation_matrix <- cor(r_grid_geom_removed[,c('tsunamis_freq','median_tsunamis','sum_tsunamis_by_year','tsunamis_prob_b')][!is.na(r_grid_geom_removed$tsunamis_freq),])
corrplot3 = corrplot(correlation_matrix, method = "circle",type = "lower",tl.srt = 45,addCoef.col = "black",tl.col = 'black',
                     col = pal1(100))

library(reshape2)
melted_gamb <- melt(corrplot1$corr)
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1 = ggplot(data = melted_gamb, aes(x = Var1, y = Var2, fill = as.numeric(value))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal1(10),limits = c(-1, +1)) +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(family = myfont),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm')) +
  scale_x_discrete(labels = c('Frequency','Median','Sum','Impact'))+
  scale_y_discrete(labels = c('Frequency','Median','Sum','Impact'))+
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            size = 3,family = myfont)

melted_gamb <- melt(corrplot2$corr)
pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p2 = ggplot(data = melted_gamb, aes(x = Var1, y = Var2, fill = as.numeric(value))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal1(10),limits = c(-1, +1)) +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(family = myfont),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm')) +
  scale_x_discrete(labels = c('Frequency','Median','Sum','Impact'))+
  scale_y_discrete(labels = c('Frequency','Median','Sum','Impact'))+
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            size = 3,family = myfont)

melted_gamb <- melt(corrplot3$corr)
pal1 = colorRampPalette(c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'))

p3 = ggplot(data = melted_gamb, aes(x = Var1, y = Var2, fill = as.numeric(value))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal1(10),limits = c(-1, +1)) +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(family = myfont),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm')) +
  scale_x_discrete(labels = c('Frequency','Median','Sum','Impact'))+
  scale_y_discrete(labels = c('Frequency','Median','Sum','Impact'))+
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black",size = 3,family = myfont)


grid_plot4 = arrangeGrob(p1,p2,p3,ncol = 3)
ggsave(filename = 'correlations.png',plot = grid_plot4,dpi = 1000,width = 10,height = 3)

