#plotting----
load('birds_breeding_resident_filtered.Rdata')
load('mammals_filtered.Rdata')
load('reptiles_filtered.Rdata')
load('amphibians_filtered.Rdata')

nrow(birds_breeding_resident)+nrow(mammals)+nrow(reptiles)+nrow(amphibians)




library(raster)
library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')

r = raster(extent(world_map),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)
r_grid = st_transform(r_grid,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


amphibians_grid = st_intersects(amphibians,r_grid)
reptiles_grid = st_intersects(reptiles,r_grid)
mammals_grid = st_intersects(mammals,r_grid)
#birds_grid = st_intersects(birds,r_grid)
birds_breeding_grid = st_intersects(birds_breeding_resident,r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))
r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))
r_grid$richness_m = unlist(lapply(t(mammals_grid),length))
r_grid$richness_b = unlist(lapply(t(birds_breeding_grid),length))


save(r_grid,file = 'r_grid_richness_2500km_plus_1100_breeding_july_19.Rdata')

load('r_grid_richness_2500km_plus_1100_breeding_july_19.Rdata')


#save(r_grid,file = 'r_grid_richness_2500km_plus_1100.Rdata')

#load('r_grid_richness_2500km.Rdata')

#load('r_grid_richness_2500km_plus_1100.Rdata')

r_grid_temp = r_grid
st_geometry(r_grid_temp) = NULL
r_grid_temp[is.na(r_grid_temp)]= 0

r_grid$all_vertebrates = unlist(rowSums(r_grid_temp[,c('richness_a','richness_r','richness_m','richness_b')]))
r_grid[r_grid$all_vertebrates==0,]$all_vertebrates = NA



pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'

library(raster)
library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')

pal1 = colorRampPalette(rev(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')))
world_map = st_transform(world_map,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


p1 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  scale_fill_gradientn('Number of species (amphibians)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (amphibians)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians.png',plot = p1,dpi = 2000,width = 6,height = 4)


p2 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  scale_fill_gradientn('Number of species (reptiles)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (reptiles)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles.png',plot = p2,dpi = 2000,width = 6,height = 4)


p3 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  scale_fill_gradientn('Number of species (mammals)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (mammals)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals.png',plot = p3,dpi = 2000,width = 6,height = 4)


p4 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  scale_fill_gradientn('Number of species (birds)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (birds)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds.png',plot = p4,dpi = 2000,width = 6,height = 4)


# p5 = ggplot()+
#   geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
#   geom_sf(data = r_grid[!is.na(r_grid$richness_b_breeding),],aes(fill = richness_b_breeding,col = richness_b_breeding),linewidth = 0.01)+
#   scale_fill_gradientn('Number of species (birds-breeding)',colours = pal1(10))+
#   scale_colour_gradientn('Number of species (birds-breeding)',colours = pal1(10))+
#   theme_void()+
#   theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
#         legend.key.width = unit(1.5,'cm'),
#         legend.key.height =  unit(0.25,'cm'),
#         legend.title = element_text(family = myfont))+
#   guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

p6 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[!is.na(r_grid$all_vertebrates),],aes(fill = all_vertebrates,col = all_vertebrates),linewidth = 0.01)+
  scale_fill_gradientn('Number of species (combined)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (combined)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

library(gridExtra)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_all_species.png',plot = p4,dpi = 2000,width = 6,height = 4)


gridded_maps = arrangeGrob(p1,p2,p3,p4,ncol = 2)

#gridded_maps = arrangeGrob(p1,p2,p3,p4,p5,p6,ncol = 2)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed.png',plot = gridded_maps,dpi = 2000,width = 10,height = 8)




