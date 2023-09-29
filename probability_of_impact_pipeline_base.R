#pipeline_probability_threat_disasters
library(sf)
sf_use_s2(F)
library(dplyr)
library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')
library(raster)
#world_map = st_transform(world_map,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

r = raster(extent(world_map),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)
r_grid = st_transform(r_grid,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


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
table(vulcanos$magnitude)
vulcanos$Name

test2 = str_split(vulcanos$Name,pattern = ': ')


func_year = function(x){
  return(tail(test2[[x]],1))
}


vulcanos$year = unlist(lapply(1:length(test2),FUN =func_year))


vulcanos_subset = vulcanos[vulcanos$year > 1969,]


years = 1970:max(vulcanos$year)
vulcanos_subset$magnitude = 7-as.numeric(vulcanos_subset$magnitude)
sf_intersected = st_intersects(vulcanos_subset,r_grid)
table(vulcanos_subset$magnitude)



x = t(sf_intersected)


# weight_layer = function(x){
#   if(length(unlist(sf_to_use[x])) == 0){
#     value = NA
#   }else{
#     list_zeros = rep(0,length(years))
#     list_ones = as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year) - 1970
#     
#     list_zeros[list_ones] = 1
# 
#     sf_weight = rep(1,length(years))
#     sf_weight_magnitude = vulcanos_subset[t(sf_intersected)[[x]],]$magnitude
#     
#     sf_weight[list_ones] = sf_weight[list_ones] + as.numeric(sf_weight_magnitude) 
#     
#     
#     value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
#                          family = binomial(link = 'logit')),type = 'response')[1]
#   }
#   return(value)
# }

weight_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = vulcanos_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude = max(magnitude))
    }else{
      temp_sf = vulcanos_subset[t(sf_intersected)[[x]],]   
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
weight_layer_fixed = function(x){
  weigth0 = 34
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(vulcanos_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = vulcanos_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude = max(magnitude))
    }else{
      temp_sf = vulcanos_subset[t(sf_intersected)[[x]],]   
    }
    
    
    list_ones = as.numeric(temp_sf$year) - 1969
    
    list_zeros[list_ones] = 1
    
    sf_weight = rep(weigth0,length(years))
    sf_weight_magnitude = temp_sf$magnitude
    
    sf_weight[list_ones] = sf_weight_magnitude 
    
    
    value =  predict(glm(list_zeros ~ 1, weights = 1/sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

x = 131621
sum_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    
    value = vulcanos_subset[t(sf_intersected)[[x]],] %>% summarise(totalmagnitude = sum(as.numeric(magnitude)))
    value = value$totalmagnitude
    }
    
    
  return(value)
}


sf_to_use = t(sf_intersected)
class(sf_intersected)
frequency = unlist(lapply(sf_to_use, FUN = length))
r_grid$vulcanos_freq = frequency
table(r_grid$vulcanos_freq)

ggplot()+
  geom_sf(data = r_grid,aes(col = vulcanos_freq))


sum_vulcanos <- unlist(lapply(1:length(sf_to_use), FUN = sum_layer))
length(sum_vulcanos)
r_grid$vulcanos_sum = sum_vulcanos
max(na.omit(r_grid$vulcanos_sum))
max(na.omit(r_grid$vulcanos_freq))
r_grid[r_grid$vulcanos_freq>10,]

probability_vulcanos <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))
probability_vulcanos_fixed <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer_fixed))

r_grid$vulcanos_prob = probability_vulcanos
r_grid$vulcanos_prob_b = probability_vulcanos_fixed

r_grid[r_grid$vulcanos_freq > 10,]

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'
library(ggplot2)
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))
p = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid[!is.na(r_grid$vulcanos_prob_b),],aes(fill = vulcanos_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Probability of impact (vulcanos)',colours = pal1(5))+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(3,'cm'),
        legend.title = element_text(family = myfont))


ggsave(filename = 'vulcanos_moll_b.png',plot = p,dpi = 1000,width = 10,height = 5)


#earthqwake

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

earthqwake_subset = earthqwake[earthqwake$year > 1970,]

earthqwake_subset[earthqwake_subset$magnitude == 'Unknown',]$year

earthqwake_subset = earthqwake_subset[earthqwake_subset$magnitude!= 'Unknown',]

years = 1970:max(earthqwake_subset$year)

sf_intersected = st_intersects(earthqwake_subset,r_grid)

x = t(sf_intersected)

# weight_layer = function(x){
#   if(length(unlist(sf_to_use[x])) == 0){
#     value = NA
#   }else{
#     list_zeros = rep(0,length(years))
#     list_ones = as.numeric(earthqwake_subset[t(sf_intersected)[[x]],]$year) - 1970
#     
#     list_zeros[list_ones] = 1
#     
#     sf_weight = rep(1,length(years))
#     sf_weight_magnitude = earthqwake_subset[t(sf_intersected)[[x]],]$magnitude
#     
#     sf_weight[list_ones] = sf_weight[list_ones] + as.numeric(sf_weight_magnitude) 
#     
#     
#     value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
#                          family = binomial(link = 'logit')),type = 'response')[1]
#   }
#   return(value)
# }

weight_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(earthqwake_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(earthqwake_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = earthqwake_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude = max(magnitude))
    }else{
      temp_sf = earthqwake_subset[t(sf_intersected)[[x]],]   
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

sf_to_use = t(sf_intersected)
probability_earthqwake <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))

r_grid$earthqwake_prob = probability_earthqwake


pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
p = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid[!is.na(r_grid$earthqwake_prob),],aes(fill = earthqwake_prob),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Probability of impact (earthqwakes)',colours = pal1(5))+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(3,'cm'),
        legend.title = element_text(family = myfont))


ggsave(filename = 'earthqwakes_moll.png',plot = p,dpi = 1000,width = 10,height = 5)

## tsunamis
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

tsunamis_subset = tsunamis[tsunamis$year > 1970,]

tsunamis_subset[tsunamis_subset$magnitude == 'Unknown',]$year

tsunamis_subset = tsunamis_subset[tsunamis_subset$magnitude!= 'Unknown',]

tsunamis_subset = tsunamis_subset[!is.na(tsunamis_subset$year),]

tsunamis_subset$magnitude = as.numeric(tsunamis_subset$magnitude)



tsunamis_subset[tsunamis_subset$magnitude>50,]

years = 1970:max(tsunamis_subset$year)

sf_intersected = st_intersects(tsunamis_subset,r_grid)

x = t(sf_intersected)

max(as.numeric(tsunamis_subset$magnitude))

tsunamis_subset$magnitude = as.numeric(tsunamis_subset$magnitude)
tsunamis_subset[tsunamis_subset$magnitude >180,]$descriptio

tsunamis_subset$tessellate
table(tsunamis_subset$layer)
hist(as.numeric(tsunamis_subset$magnitude))

tsunamis_subset$descriptio


x = 33198
weight_layer = function(x){
  print(x)
  if(length(unlist(sf_to_use[x])) == 0){
    value = NA
  }else{
    list_zeros = rep(0,length(years))
    
    if(length(as.numeric(tsunamis_subset[t(sf_intersected)[[x]],]$year)) != length(unique(as.numeric(tsunamis_subset[t(sf_intersected)[[x]],]$year)))){
      temp_sf = tsunamis_subset[t(sf_intersected)[[x]],] %>% group_by(year) %>% summarise(magnitude = max(magnitude))
    }else{
      temp_sf = tsunamis_subset[t(sf_intersected)[[x]],]   
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

sf_to_use = t(sf_intersected)

weight_layer(33198)


probability_tsunamis <- unlist(lapply(1:length(sf_to_use), FUN = weight_layer))

r_grid$tsunami_prob = probability_tsunamis


pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))
p = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05)+
  geom_sf(data = r_grid[!is.na(r_grid$tsunami_prob),],aes(fill = tsunami_prob),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Probability of impact (tsunami)',colours = pal1(10))+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(3,'cm'),
        legend.title = element_text(family = myfont))


ggsave(filename = 'tsunami_moll.png',plot = p,dpi = 1000,width = 10,height = 5)


#hurricanes ----

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

