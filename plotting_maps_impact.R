#plotting_maps_impact

library(tidyverse)
library(sf)
sf_use_s2(F)
library(ggplot2)

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'

load('r_grid_pipeline4.Rdata')

library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')
world_map_moll = st_transform(world_map,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

tsunamis_buffer = st_buffer(st_centroid(r_grid[r_grid$tsunamis_freq>0,]),200000)
earthquake_buffer = st_buffer(st_centroid(r_grid[r_grid$earthquakes_freq>0,]),200000)
vulcano_buffer = st_buffer(st_centroid(r_grid[r_grid$vulcanos_freq>0,]),400000)
hurricane_buffer = st_buffer(st_centroid(r_grid[r_grid$hurricanes_freq>0,]),200000)


pal_vulcanos= colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))
pal_tsunami = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
pal_hurricanes = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

#original----
p1 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = vulcano_buffer,aes(col = vulcanos_prob_b),linewidth = 0.3,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$vulcanos_prob_b),],aes(fill = vulcanos_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (vulcanos)',colours = pal_vulcanos(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (vulcanos)',colours = alpha(pal_vulcanos(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'vulcanos_final.png',plot = p1,dpi = 2000,width = 4,height = 3)

p2 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer,aes(col = tsunamis_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$tsunamis_prob_b),],aes(fill = tsunamis_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (tsunamis)',colours = pal_tsunami(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal_tsunami(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'tsunamis_final.png',plot = p2,dpi = 2000,width = 4,height = 3)

p3 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = earthquake_buffer,aes(col = earthquakes_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$earthquakes_prob_b),],aes(fill = earthquakes_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (earthquakes)',colours = pal_earthquakes(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (earthquakes)',colours = alpha(pal_earthquakes(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'earthquakes_final.png',plot = p3,dpi = 2000,width = 4,height = 3)

p4 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  #geom_sf(data = hurricane_buffer,aes(col = hurricanes_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$hurricanes_prob_b),],aes(fill = hurricanes_prob_b,col = hurricanes_prob_b),linewidth = 0.01)+
  scale_fill_gradientn('Impact (Hurricanes)',colours = pal_hurricanes(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (Hurricanes)',colours = alpha(pal_hurricanes(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'hurricanes_final.png',plot = p4,dpi = 2000,width = 4,height = 3)

library(gridExtra)
grid_disasters= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'maps_disasters.png',plot = grid_disasters,width = 8,height = 5,dpi = 2000)

#greyed---
load('greyed_cells.Rdata')

vulcano_buffer$layer = 1:nrow(vulcano_buffer)
p1 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = vulcano_buffer[vulcano_buffer$layer %in% greyed_cells,],aes(col = vulcanos_prob_b),linewidth = 0.3,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$vulcanos_prob_b) & !r_grid$layer%in% greyed_cells,],fill = 'grey90',linewidth = 0.01,col = 'black')+
  geom_sf(data = r_grid[!is.na(r_grid$vulcanos_prob_b) & r_grid$layer%in% greyed_cells,],aes(fill = vulcanos_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (vulcanos)',colours = pal_vulcanos(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (vulcanos)',colours = alpha(pal_vulcanos(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'vulcanos_final_greyed.png',plot = p1,dpi = 2000,width = 4,height = 3)

tsunamis_buffer$layer = 1:nrow(tsunamis_buffer)

p2 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = tsunamis_buffer[tsunamis_buffer$layer %in% greyed_cells,],aes(col = tsunamis_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$tsunamis_prob_b) & !r_grid$layer%in% greyed_cells,],fill = 'grey90',linewidth = 0.01,col = 'black')+
  geom_sf(data = r_grid[!is.na(r_grid$tsunamis_prob_b) & r_grid$layer%in% greyed_cells,],aes(fill = tsunamis_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (tsunamis)',colours = pal_tsunami(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (tsunamis)',colours = alpha(pal_tsunami(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'tsunamis_final_greyed.png',plot = p2,dpi = 2000,width = 4,height = 3)

earthquake_buffer$layer = 1:nrow(earthquake_buffer)

p3 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  geom_sf(data = earthquake_buffer[earthquake_buffer$layer %in% greyed_cells,],aes(col = earthquakes_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$earthquakes_prob_b) & !r_grid$layer%in% greyed_cells,],fill = 'grey90',linewidth = 0.01,col = 'black')+
  geom_sf(data = r_grid[!is.na(r_grid$earthquakes_prob_b) & r_grid$layer%in% greyed_cells,],aes(fill = earthquakes_prob_b),linewidth = 0.01,col = 'black')+
  scale_fill_gradientn('Impact (earthquakes)',colours = pal_earthquakes(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (earthquakes)',colours = alpha(pal_earthquakes(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'earthquakes_final_greyed.png',plot = p3,dpi = 2000,width = 4,height = 3)

p4 = ggplot()+
  geom_sf(data = r_grid,bg = 'lightblue',col = 'lightblue')+
  geom_sf(data = world_map_moll,bg = 'white',linewidth = 0.02,col = 'grey90')+
  #geom_sf(data = hurricane_buffer,aes(col = hurricanes_prob_b),linewidth = 0.2,fill = NA)+
  geom_sf(data = r_grid[!is.na(r_grid$hurricanes_prob_b) & !r_grid$layer%in% greyed_cells,],fill = 'grey90',linewidth = 0.01,col = 'grey90')+
  geom_sf(data = r_grid[!is.na(r_grid$hurricanes_prob_b) & r_grid$layer%in% greyed_cells,],aes(fill = hurricanes_prob_b,col = hurricanes_prob_b),linewidth = 0.01)+  
  scale_fill_gradientn('Impact (Hurricanes)',colours = pal_hurricanes(5),limits = c(0,1))+
  scale_colour_gradientn('Impact (Hurricanes)',colours = alpha(pal_hurricanes(5),0.5),guide = 'none')+
  #scale_colour_gradientn(colours = pal1(5))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont,size = 9))+
  guides(fill = guide_colourbar(title.position = "top"))

ggsave(filename = 'hurricanes_final_greyed.png',plot = p4,dpi = 2000,width = 4,height = 3)

library(gridExtra)
grid_disasters= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'maps_disasters_greyed_lr.png',plot = grid_disasters,width = 8,height = 5,dpi = 2000)



