#richness maps
library(sf)
sf_use_s2(F)
library(ggplot2)
library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')



pop_data_a = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Amphibians/all_other_fields.csv')
pop_data_r = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Reptiles/all_other_fields.csv')
pop_data_m = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Mammals/all_other_fields.csv')
pop_data_b1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/all_other_fields.csv')
pop_data_b2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/all_other_fields.csv')
pop_data_b = rbind(pop_data_b1,pop_data_b2)

nrow(pop_data_a[pop_data_a$PopulationSize.range == '',])/nrow(pop_data_a)
nrow(pop_data_r[pop_data_r$PopulationSize.range == '',])/nrow(pop_data_r)
nrow(pop_data_m[pop_data_m$PopulationSize.range == '',])/nrow(pop_data_m)
nrow(pop_data_b[pop_data_b$PopulationSize.range == '',])/nrow(pop_data_b)

library(stringr)




min_func = function(x){

min(unlist(as.numeric(temp[x][[1]])))
}
max_func =  function(x){
  max(unlist(as.numeric(temp[x][[1]])))
  
}
mean_func =  function(x){
  mean(unlist(as.numeric(temp[x][[1]])))
  
}
med_func =  function(x){
 median(unlist(as.numeric(temp[x][[1]])))
  
}

temp = gsub(pop_data_a$PopulationSize,range,pattern = '-',replacement = ',')
temp = str_split(temp,pattern = ',')
pop_data_a$min_f = unlist(lapply(1:length(temp),function(x) min_func(x)))
pop_data_a$max_f = unlist(lapply(1:length(temp),function(x) max_func(x)))
pop_data_a$mean_f = unlist(lapply(1:length(temp),function(x) mean_func(x)))
pop_data_a$median_f = unlist(lapply(1:length(temp),function(x) med_func(x)))

temp = gsub(pop_data_r$PopulationSize,range,pattern = '-',replacement = ',')
temp = str_split(temp,pattern = ',')
pop_data_r$min_f = unlist(lapply(1:length(temp),function(x) min_func(x)))
pop_data_r$max_f = unlist(lapply(1:length(temp),function(x) max_func(x)))
pop_data_r$mean_f = unlist(lapply(1:length(temp),function(x) mean_func(x)))
pop_data_r$median_f = unlist(lapply(1:length(temp),function(x) med_func(x)))

temp = gsub(pop_data_m$PopulationSize,range,pattern = '-',replacement = ',')
temp = str_split(temp,pattern = ',')
pop_data_m$min_f = unlist(lapply(1:length(temp),function(x) min_func(x)))
pop_data_m$max_f = unlist(lapply(1:length(temp),function(x) max_func(x)))
pop_data_m$mean_f = unlist(lapply(1:length(temp),function(x) mean_func(x)))
pop_data_m$median_f = unlist(lapply(1:length(temp),function(x) med_func(x)))

temp = gsub(pop_data_b$PopulationSize,range,pattern = '-',replacement = ',')
temp = str_split(temp,pattern = ',')
pop_data_b$min_f = unlist(lapply(1:length(temp),function(x) min_func(x)))
pop_data_b$max_f = unlist(lapply(1:length(temp),function(x) max_func(x)))
pop_data_b$mean_f = unlist(lapply(1:length(temp),function(x) mean_func(x)))
pop_data_b$median_f = unlist(lapply(1:length(temp),function(x) med_func(x)))

library(openxlsx)
write.xlsx(pop_data_b,'pop_data_b.xlsx')

library(dplyr)
pop_data_a %>% filter(min_f < 1100) %>% nrow()
pop_data_a %>% filter(max_f < 1100) %>% nrow()
pop_data_a %>% filter(mean_f < 1100) %>% nrow()
pop_data_a %>% filter(median_f < 1100) %>% nrow()

pop_data_r %>% filter(min_f < 1100) %>% nrow()
pop_data_r %>% filter(max_f < 1100) %>% nrow()
pop_data_r %>% filter(mean_f < 1100) %>% nrow()
pop_data_r %>% filter(median_f < 1100) %>% nrow()

pop_data_m %>% filter(min_f < 1100) %>% nrow()
pop_data_m %>% filter(max_f < 1100) %>% nrow()
pop_data_m %>% filter(mean_f < 1100) %>% nrow()
pop_data_m %>% filter(median_f < 1100) %>% nrow()

pop_data_b %>% filter(min_f < 1100) %>% nrow()
pop_data_b %>% filter(max_f < 1100) %>% nrow()
pop_data_b %>% filter(mean_f < 1100) %>% nrow()
pop_data_b %>% filter(median_f < 1100) %>% nrow()

amphibians_df = c(min = pop_data_a %>% filter(min_f < 1100) %>% nrow(),
          max = pop_data_a %>% filter(max_f < 1100) %>% nrow(),
          mean = pop_data_a %>% filter(mean_f < 1100) %>% nrow(),
          median = pop_data_a %>% filter(median_f < 1100) %>% nrow())

reptiles_df = c(min = pop_data_r %>% filter(min_f < 1100) %>% nrow(),
          max = pop_data_r %>% filter(max_f < 1100) %>% nrow(),
          mean = pop_data_r %>% filter(mean_f < 1100) %>% nrow(),
          median = pop_data_r %>% filter(median_f < 1100) %>% nrow())

mammals_df = c(min = pop_data_m %>% filter(min_f < 1100) %>% nrow(),
          max = pop_data_m %>% filter(max_f < 1100) %>% nrow(),
          mean = pop_data_m %>% filter(mean_f < 1100) %>% nrow(),
          median = pop_data_m%>% filter(median_f < 1100) %>% nrow())

birds_df = c(min = pop_data_b %>% filter(min_f < 1100) %>% nrow(),
          max = pop_data_b %>% filter(max_f < 1100) %>% nrow(),
          mean = pop_data_b %>% filter(mean_f < 1100) %>% nrow(),
          median = pop_data_b %>% filter(median_f < 1100) %>% nrow())

# taxa_names = c('Amphibians','Reptiles','Mammals','Birds')
# df = rbind(amphibians_df,reptiles_df,mammals_df,birds_df)
# df = cbind.data.frame(taxa_names,df)
# 
# amphibians_df2 = c(min_new_sp = length(setdiff(pop_data_a[pop_data_a$min_f < 1100,]$scientificName,amphibians$binomial)),
#                   max_new_sp = length(setdiff(pop_data_a[pop_data_a$max_f < 1100,]$scientificName,amphibians$binomial)),
#                   mean_new_sp = length(setdiff(pop_data_a[pop_data_a$mean_f < 1100,]$scientificName,amphibians$binomial)),
#                   median_new_sp = length(setdiff(pop_data_a[pop_data_a$median_f < 1100,]$scientificName,amphibians$binomial)))
# 
# reptiles_df2 = c(min_new_sp = length(setdiff(pop_data_r[pop_data_r$min_f < 1100,]$scientificName,reptiles$binomial)),
#                    max_new_sp = length(setdiff(pop_data_r[pop_data_r$max_f < 1100,]$scientificName,reptiles$binomial)),
#                    mean_new_sp = length(setdiff(pop_data_r[pop_data_r$mean_f < 1100,]$scientificName,reptiles$binomial)),
#                  median_new_sp = length(setdiff(pop_data_r[pop_data_r$median_f < 1100,]$scientificName,reptiles$binomial)))
# 
# mammals_df2 = c(min_new_sp = length(setdiff(pop_data_m[pop_data_m$min_f < 1100,]$scientificName,mammals$binomial)),
#                    max_new_sp = length(setdiff(pop_data_m[pop_data_m$max_f < 1100,]$scientificName,mammals$binomial)),
#                    mean_new_sp = length(setdiff(pop_data_m[pop_data_m$mean_f < 1100,]$scientificName,mammals$binomial)),
#                 median_new_sp = length(setdiff(pop_data_m[pop_data_m$median_f < 1100,]$scientificName,mammals$binomial)))
# 
# birds_df2 = c(min_new_sp = length(setdiff(pop_data_b[pop_data_b$min_f < 1100,]$scientificName,birds$binomial)),
#                    max_new_sp = length(setdiff(pop_data_b[pop_data_b$max_f < 1100,]$scientificName,birds$binomial)),
#                    mean_new_sp = length(setdiff(pop_data_b[pop_data_b$mean_f < 1100,]$scientificName,birds$binomial)),
#               median_new_sp = length(setdiff(pop_data_b[pop_data_b$median_f < 1100,]$scientificName,birds$binomial)))
# 
# 
# df2 = rbind(amphibians_df2,reptiles_df2,mammals_df2,birds_df2)
# df3 = cbind.data.frame(df,df2)
# write.csv(df3,file = 'df3.csv')
load('amphibians_sf_summ_all_sps_present.Rdata')
amphibians = amphibians_sf_summ[amphibians_sf_summ$range_size < 2500 | amphibians_sf_summ$binomial %in% unique(pop_data_a[pop_data_a$max_f < 1100,]$scientificName),]
amphibians = st_transform(amphibians,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

save(amphibians,file = 'amphibians_filtered.Rdata')

load('reptiles_sf_summ_all_sps_present.Rdata')
reptiles = reptiles_sf_summ[reptiles_sf_summ$range_size < 2500 | reptiles_sf_summ$binomial %in% unique(pop_data_r[pop_data_r$max_f < 1100,]$scientificName),]
reptiles = st_transform(reptiles,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

save(reptiles,file = 'reptiles_filtered.Rdata')


load('mammals_sf_summ_all_sps_present.Rdata')
mammals = mammals_sf_summ[mammals_sf_summ$range_size < 2500 | mammals_sf_summ$binomial %in% unique(pop_data_m[pop_data_m$max_f < 1100,]$scientificName) ,]
mammals = st_transform(mammals,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

save(mammals,file = 'mammals_filtered.Rdata')



load('birds_sf_summ_all.Rdata')

birds_sf_summ$range_size = st_area(birds_sf_summ)/1000000
units(birds_sf_summ$range_size) = NULL
birds = birds_sf_summ[birds_sf_summ$range_size < 2500 | birds_sf_summ$binomial %in% unique(pop_data_b[pop_data_b$max_f < 1100,]$scientificName) ,]
world_map = ne_countries(scale = 10,returnclass = 'sf')
birds=st_crop(birds,world_map)
birds = st_transform(birds,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

save(birds,file = 'birds_filtered.Rdata')


load('birds_sf_summ_breeding.Rdata')

birds_breeding = birds_sf_summ[birds_sf_summ$range_size < 2500 | birds_sf_summ$binomial %in% unique(pop_data_b[pop_data_b$max_f < 1100,]$scientificName) ,]
world_map = ne_countries(scale = 10,returnclass = 'sf')
birds_breeding=st_crop(birds_breeding,world_map)
birds_breeding = st_transform(birds_breeding,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

save(birds_breeding,file = 'birds_breeding_filtered.Rdata')


load('birds_sf_summ_breeding_resident.Rdata')

birds_breeding = birds_sf_summ[birds_sf_summ$range_size < 2500 | birds_sf_summ$binomial %in% unique(pop_data_b[pop_data_b$max_f < 1100,]$scientificName) ,]
world_map = ne_countries(scale = 10,returnclass = 'sf')
birds_breeding=st_crop(birds_breeding,world_map)
birds_breeding = st_transform(birds_breeding,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
birds_breeding_resident = birds_breeding
save(birds_breeding_resident,file = 'birds_breeding_resident_filtered.Rdata')



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






