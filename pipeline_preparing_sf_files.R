library(sf)
sf_use_s2(F)
amphibians_sf = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/AMPHIBIANS/AMPHIBIANS.shp')
library(dplyr)

colnames(amphibians_sf)


amphibians_sf_summ = amphibians_sf[amphibians_sf$presence ==1,] %>% group_by(binomial,category) %>% summarise

amphibians_sf_summ$range_size = st_area(amphibians_sf_summ)/1000000

units(amphibians_sf_summ$range_size) = NULL

save(amphibians_sf_summ,file = 'amphibians_sf_summ_all_sps_present.Rdata')

amphibians_sf_summ = amphibians_sf_summ[!amphibians_sf_summ$range_size > 100,]

save(amphibians_sf_summ,file = 'amphibians_sf_summ.Rdata')

##

mammals_sf = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp')
library(dplyr)
mammals_sf_summ = mammals_sf[mammals_sf$presence ==1,] %>% group_by(binomial,category) %>% summarise

mammals_sf_summ$range_size = st_area(mammals_sf_summ)/1000000

units(mammals_sf_summ$range_size) = NULL

save(mammals_sf_summ,file = 'mammals_sf_summ_all_sps_present.Rdata')

mammals_sf_summ = mammals_sf_summ[!mammals_sf_summ$range_size > 100,]

save(mammals_sf_summ,file = 'mammals_sf_summ.Rdata')

library(raster)

load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_reptiles_summarized.Rdata')

sf_reptiles_1 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/CROCODILES_ALLIGATORS/CROCODILES_ALLIGATORS.shp')
sf_reptiles_2 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/SCALED_REPTILES/SCALED_REPTILES.shp')
sf_reptiles_3 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/TURTLES/TURTLES.shp')
sf_reptiles_4 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/redlist_species_data_6ef81a4f-8fee-46e3-a74e-f4364b0a87a2/data_0.shp')

colnames(sf_reptiles_4) = tolower(colnames(sf_reptiles_4))
sf_reptiles_4$category = "LC"


to_keep = intersect(colnames(sf_reptiles_4),colnames(sf_reptiles_3))

sf_reptiles_final = rbind(sf_reptiles_1[,to_keep],sf_reptiles_2[,to_keep],sf_reptiles_3[,to_keep],sf_reptiles_4[,to_keep])

rm(sf_reptiles_1,sf_reptiles_2,sf_reptiles_3,sf_reptiles_4)


reptiles_sf_summ = sf_reptiles_final[sf_reptiles_final$presence ==1,] %>% group_by(binomial,category) %>% summarise

reptiles_sf_summ$range_size = st_area(reptiles_sf_summ)/1000000

units(reptiles_sf_summ$range_size) = NULL

save(reptiles_sf_summ,file = 'reptiles_sf_summ_all_sps_present.Rdata')

reptiles_sf_summ = reptiles_sf_summ[!reptiles_sf_summ$range_size > 100,]

save(reptiles_sf_summ,file = 'reptiles_sf_summ.Rdata')
#####

check_species = c('Amazona arausiaca','Amazona imperialis','Amazona vittata','Icterus oberi','Colostethus jacobuspetersi')
sf_birds1 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_1.shp')
# sf_birds1v2=sf_birds1[sf_birds1$seasonal == 2,]
sf_birds1v3=sf_birds1[sf_birds1$seasonal %in% c(1,2),]

# sf_birds1v3[sf_birds1v3$sci_name%in% check_species,]

# sf_birds1=sf_birds1[sf_birds1$presence == 1,]


sf_birds2 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_2.shp')
# sf_birds2v2=sf_birds2[sf_birds2$seasonal == 2,]
sf_birds2v3=sf_birds2[sf_birds2$presence %in% c(1,2),]

# sf_birds2=sf_birds2[sf_birds2$presence == 1,]


sf_birds3 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_3.shp')
# sf_birds3v2=sf_birds3[sf_birds3$seasonal == 2,]
sf_birds3v3=sf_birds3[sf_birds3$presence %in% c(1,2),]

# sf_birds3=sf_birds3[sf_birds3$presence == 1,]

sf_birds4 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_4.shp')
# sf_birds4v2=sf_birds4[sf_birds4$seasonal == 2,]
sf_birds4v3=sf_birds4[sf_birds4$presence %in% c(1,2),]

# sf_birds4=sf_birds4[sf_birds4$presence == 1,]


sf_birds5 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_5.shp')
# sf_birds5v2=sf_birds5[sf_birds5$seasonal == 2,]
sf_birds5v3=sf_birds5[sf_birds5$presence %in% c(1,2),]


# sf_birds5=sf_birds5[sf_birds5$presence == 1,]
# 
# sf_birds = rbind(sf_birds1v2,sf_birds2v2,sf_birds3v2,sf_birds4v2,sf_birds5v2)

sf_birds = rbind(sf_birds1v3,sf_birds2v3,sf_birds3v3,sf_birds4v3,sf_birds5v3)


nrow(sf_birds)

# sf_birds = rbind(sf_birds1,sf_birds2,sf_birds3,sf_birds4,sf_birds5)
rm(sf_birds1,sf_birds2,sf_birds3,sf_birds4,sf_birds5)
rm(mammals_sf,amphibians_sf)

bird_cat1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/assessments.csv')
bird_cat2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/assessments.csv')



bird_cat = rbind(bird_cat1,bird_cat2)
colnames(bird_cat)
bird_cat[bird_cat$scientificName == 'Amazona guildingii',]



library(dplyr)
birds_sf_summ = sf_birds %>% group_by(sci_name) %>% summarise()
birds_sf_summ_merged = merge(birds_sf_summ,bird_cat[,c(3,4)],by.x = 'sci_name',by.y = 'scientificName')
unique(birds_sf_summ_merged$redlistCategory)

birds_sf_summ_merged = birds_sf_summ_merged[birds_sf_summ_merged$redlistCategory != 'Extinct',]
birds_sf_summ_merged = birds_sf_summ_merged[birds_sf_summ_merged$redlistCategory != 'Extinct in the Wild',]

unique(birds_sf_summ_merged$redlistCategory)

sf_birds = birds_sf_summ_merged
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Least Concern',
                                replacement = 'LC')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Near Threatened',
                                replacement = 'NT')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Vulnerable',
                                replacement = 'VU')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Endangered',
                                replacement = 'EN')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Critically EN',
                                replacement = 'CR')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Data Deficient',
                                replacement = 'DD')

unique(sf_birds$redlistCategory)

colnames(sf_birds)[1:2] = c('binomial','category')

birds_sf_summ = sf_birds


birds_sf_summ$range_size = st_area(birds_sf_summ)/1000000
units(birds_sf_summ$range_size) = NULL

save(birds_sf_summ,file = 'birds_sf_summ_breeding_resident.Rdata')


birds_sf_summ = birds_sf_summ[!birds_sf_summ$range_size > 100,]
nrow(birds_sf_summ)


save(birds_sf_summ,file = 'birds_sf_summ.Rdata')





vulcanos = raster('/Users/gdt366/Desktop/volcan_inc.tiff')
plot(vulcanos)


