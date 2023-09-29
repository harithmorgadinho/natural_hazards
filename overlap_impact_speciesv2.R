#
library(tidyverse)
library(sf)
sf_use_s2(F)

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'


load('r_grid_pipeline4.Rdata')
load('amphibians_filtered.Rdata')
nrow(amphibians)

load('reptiles_filtered.Rdata')
nrow(reptiles)

load('mammals_filtered.Rdata')
nrow(mammals)

# load('birds_filtered.Rdata')
# load('birds_breeding_filtered.Rdata')
# nrow(birds_breeding)

load('birds_breeding_resident_filtered.Rdata')

check_species = c('Amazona arausiaca','Amazona imperialis','Amazona vittata','Icterus oberi','Colostethus jacobuspetersi')

birds_breeding_resident[birds_breeding_resident$binomial %in% check_species,]

birds_breeding_resident[birds_breeding_resident$binomial == 'Pteropus pelagicus',]


birds_breeding = birds_breeding_resident
nrow(birds_breeding)

r_grid_final = r_grid
max(na.omit(r_grid_final$vulcanos_prob_b))
max(na.omit(r_grid_final$hurricanes_prob_b))
max(na.omit(r_grid_final$earthquakes_prob_b))
max(na.omit(r_grid_final$tsunamis_prob_b))

r_grid_final[is.na(r_grid_final$vulcanos_prob_b),]$vulcanos_prob_b = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_b),]$tsunamis_prob_b = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_b),]$earthquakes_prob_b = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_b),]$hurricanes_prob_b = 0


amphibians$range_size = st_area(amphibians)/1000000
units(amphibians$range_size) = NULL
nrow(amphibians)
reptiles$range_size = st_area(reptiles)/1000000
units(reptiles$range_size) = NULL

mammals$range_size = st_area(mammals)/1000000
units(mammals$range_size) = NULL

birds_breeding$range_size = st_area(birds_breeding)/1000000
units(birds_breeding$range_size) = NULL





amphibians_grid = st_intersection(amphibians,r_grid_final)
reptiles = st_buffer(reptiles,dist = 0)
reptiles_grid = st_intersection(reptiles,r_grid_final)
mammals_grid = st_intersection(mammals,r_grid_final)
#birds_grid = st_intersection(birds,r_grid_final)
birds_breeding = st_buffer(birds_breeding,dist = 0)
birds_breeding_grid = st_intersection(birds_breeding,r_grid_final)


save(amphibians_grid,file = 'amphibians_grid.Rdata')
save(reptiles_grid,file = 'reptiles_grid.Rdata')
save(mammals_grid,file = 'mammals_grid.Rdata')

save(birds_breeding_grid,file = 'birds_breeding_grid.Rdata')



amphibians_grid$range_size_intersect = st_area(amphibians_grid)/1000000
units(amphibians_grid$range_size_intersect) = NULL

reptiles_grid$range_size_intersect = st_area(reptiles_grid)/1000000
units(reptiles_grid$range_size_intersect) = NULL

mammals_grid$range_size_intersect = st_area(mammals_grid)/1000000
units(mammals_grid$range_size_intersect) = NULL

birds_breeding_grid$range_size_intersect = st_area(birds_breeding_grid)/1000000
units(birds_breeding_grid$range_size_intersect) = NULL


amphibians_grid$percent_range = 100*amphibians_grid$range_size_intersect/amphibians_grid$range_size
reptiles_grid$percent_range = 100*reptiles_grid$range_size_intersect/reptiles_grid$range_size
mammals_grid$percent_range = 100*mammals_grid$range_size_intersect/mammals_grid$range_size
birds_breeding_grid$percent_range = 100*birds_breeding_grid$range_size_intersect/birds_breeding_grid$range_size


# amphibians_grid$square_area = st_area(amphibians_grid)/1000000
# units(amphibians_grid$square_area) = NULL
# hist(amphibians_grid$square_area)
# 
# reptiles_grid$square_area = st_area(reptiles_grid)/1000000
# units(reptiles_grid$square_area) = NULL
# hist(reptiles_grid$square_area)
# 
# mammals_grid$square_area = st_area(mammals_grid)/1000000
# units(mammals_grid$square_area) = NULL
# hist(mammals_grid$square_area)
# 
# birds_breeding_grid$square_area = st_area(birds_breeding_grid)/1000000
# units(birds_breeding_grid$square_area) = NULL
# hist(birds_breeding_grid$square_area)


amphibians_grid2 = amphibians_grid
reptiles_grid2 = reptiles_grid
mammals_grid2 = mammals_grid
birds_breeding_grid2 = birds_breeding_grid
st_geometry(amphibians_grid2) = NULL
st_geometry(reptiles_grid2) = NULL
st_geometry(mammals_grid2) = NULL
st_geometry(birds_breeding_grid2) = NULL




# amphibians_grid2$weighted_range_size_intersect = amphibians_grid2$range_size_intersect/amphibians_grid2$square_area
# reptiles_grid2$weighted_range_size_intersect = reptiles_grid2$range_size_intersect/reptiles_grid2$square_area
# mammals_grid2$weighted_range_size_intersect = mammals_grid2$range_size_intersect/mammals_grid2$square_area
# birds_breeding_grid2$weighted_range_size_intersect = birds_breeding_grid2$range_size_intersect/birds_breeding_grid2$square_area
# 

amphibians_grid2$weighted_vulcanos_prob_b = (amphibians_grid2$vulcanos_prob_b*amphibians_grid2$range_size_intersect)/amphibians_grid2$range_size
reptiles_grid2$weighted_vulcanos_prob_b = (reptiles_grid2$vulcanos_prob_b*reptiles_grid2$range_size_intersect)/reptiles_grid2$range_size
mammals_grid2$weighted_vulcanos_prob_b = (mammals_grid2$vulcanos_prob_b*mammals_grid2$range_size_intersect)/mammals_grid2$range_size
birds_breeding_grid2$weighted_vulcanos_prob_b = (birds_breeding_grid2$vulcanos_prob_b*birds_breeding_grid2$range_size_intersect)/birds_breeding_grid2$range_size


# amphibians_grid2_vul = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))
# reptiles_grid2_vul = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))
# mammals_grid2_vul = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))
# birds_breeding_grid2_vul = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))

library(ggplot2)


amphibians_grid2_vul_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))
reptiles_grid2_vul_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact = sum(weighted_vulcanos_prob_b))
mammals_grid2_vul_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))
birds_breeding_grid2_vul_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_vulcanos_prob_b))



library(RColorBrewer)
pal_reds = colorRampPalette(RColorBrewer::brewer.pal(9,'Reds'))

theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'bottom',
                               legend.key.height = unit(0.25,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(1,'cm'),
                               legend.title = element_text(size = 8))

p1 = ggplot()+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  labs(title = 'Amphibians (vulcanos)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_reds(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))
guides(fill = guide_colorbar(title.position = 'top'))+
  theme_points_disasters


p2 = ggplot()+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="red", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Reptiles (vulcanos)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="red", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Mammals (vulcanos)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="red", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Birds (vulcanos)')

library(gridExtra)

grid_vulcanos = arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_vucanos_2.png',plot = grid_vulcanos,width = 10,height = 8,dpi = 600)

###tsunamis

amphibians_grid2$weighted_tsunamis_prob_b = (amphibians_grid2$tsunamis_prob_b*amphibians_grid2$range_size_intersect)/amphibians_grid2$range_size
reptiles_grid2$weighted_tsunamis_prob_b = (reptiles_grid2$tsunamis_prob_b*reptiles_grid2$range_size_intersect)/reptiles_grid2$range_size
mammals_grid2$weighted_tsunamis_prob_b = (mammals_grid2$tsunamis_prob_b*mammals_grid2$range_size_intersect)/mammals_grid2$range_size
birds_breeding_grid2$weighted_tsunamis_prob_b = (birds_breeding_grid2$tsunamis_prob_b*birds_breeding_grid2$range_size_intersect)/birds_breeding_grid2$range_size

amphibians_grid2_tsu_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b))
reptiles_grid2_tsu_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b))
mammals_grid2_tsu_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b))
birds_breeding_grid2_tsu_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b))


p1 = ggplot()+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Amphibians (tsunamis)')


p2 = ggplot()+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Reptiles (tsunamis)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Mammals (tsunamis)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Birds (tsunamis)')

library(gridExtra)

grid_tsunamis= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_tsunamis.png',plot = grid_tsunamis,width = 6,height = 6,dpi = 600)

###hurricanes


amphibians_grid2$weighted_hurricanes_prob_b = (amphibians_grid2$hurricanes_prob_b*amphibians_grid2$range_size_intersect)/amphibians_grid2$range_size
reptiles_grid2$weighted_hurricanes_prob_b = (reptiles_grid2$hurricanes_prob_b*reptiles_grid2$range_size_intersect)/reptiles_grid2$range_size
mammals_grid2$weighted_hurricanes_prob_b = (mammals_grid2$hurricanes_prob_b*mammals_grid2$range_size_intersect)/mammals_grid2$range_size
birds_breeding_grid2$weighted_hurricanes_prob_b = (birds_breeding_grid2$hurricanes_prob_b*birds_breeding_grid2$range_size_intersect)/birds_breeding_grid2$range_size

amphibians_grid2_hur_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_hurricanes_prob_b))
reptiles_grid2_hur_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_hurricanes_prob_b))
mammals_grid2_hur_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_hurricanes_prob_b))
birds_breeding_grid2_hur_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_hurricanes_prob_b))

library(openxlsx)



p1 = ggplot()+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Amphibians (hurricanes)')


p2 = ggplot()+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Reptiles (hurricanes)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Mammals (hurricanes)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Birds (hurricanes)')

library(gridExtra)

grid_hurricanes= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_hurricanes.png',plot = grid_hurricanes,width = 6,height = 6,dpi = 600)

###earthquake

amphibians_grid2$weighted_earthquakes_prob_b = (amphibians_grid2$earthquakes_prob_b*amphibians_grid2$range_size_intersect)/amphibians_grid2$range_size
reptiles_grid2$weighted_earthquakes_prob_b = (reptiles_grid2$earthquakes_prob_b*reptiles_grid2$range_size_intersect)/reptiles_grid2$range_size
mammals_grid2$weighted_earthquakes_prob_b = (mammals_grid2$earthquakes_prob_b*mammals_grid2$range_size_intersect)/mammals_grid2$range_size
birds_breeding_grid2$weighted_earthquakes_prob_b = (birds_breeding_grid2$earthquakes_prob_b*birds_breeding_grid2$range_size_intersect)/birds_breeding_grid2$range_size


amphibians_grid2_ear_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))
reptiles_grid2_ear_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))
mammals_grid2_ear_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))
birds_breeding_grid2_ear_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))

library(openxlsx)
write.xlsx(amphibians_grid2_vul_0,file = 'amphibians_grid2_vul_0_weighted3.xlsx')
write.xlsx(reptiles_grid2_vul_0,file = 'reptiles_grid2_vul_0_weighted3.xlsx')
write.xlsx(mammals_grid2_vul_0,file = 'mammals_grid2_vul_0_weighted3.xlsx')
write.xlsx(birds_breeding_grid2_vul_0,file = 'birds_grid2_vul_0_weighted3.xlsx')











a_v_25 = amphibians_grid2_vul_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25)%>% nrow()
r_v_25 = reptiles_grid2_vul_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25)%>% nrow()
m_v_25 = mammals_grid2_vul_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25)%>% nrow()
b_v_25 = birds_breeding_grid2_vul_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25)%>% nrow()

write.xlsx(amphibians_grid2_ear_0,file = 'amphibians_grid2_ear_0_weighted3.xlsx')
write.xlsx(reptiles_grid2_ear_0,file = 'reptiles_grid2_ear_0_weighted3.xlsx')
write.xlsx(mammals_grid2_ear_0,file = 'mammals_grid2_ear_0_weighted3.xlsx')
write.xlsx(birds_breeding_grid2_ear_0,file = 'birds_grid2_ear_0_weighted3.xlsx')

a_e_25 = amphibians_grid2_ear_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
r_e_25 = reptiles_grid2_ear_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
m_e_25 = mammals_grid2_ear_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
b_e_25 = birds_breeding_grid2_ear_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()

write.xlsx(amphibians_grid2_tsu_0,file = 'amphibians_grid2_tsu_0_weighted3.xlsx')
write.xlsx(reptiles_grid2_tsu_0,file = 'reptiles_grid2_tsu_0_weighted3.xlsx')
write.xlsx(mammals_grid2_tsu_0,file = 'mammals_grid2_tsu_0_weighted3.xlsx')
write.xlsx(birds_breeding_grid2_tsu_0,file = 'birds_grid2_tsu_0_weighted3.xlsx')

a_t_25=amphibians_grid2_tsu_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
r_t_25=reptiles_grid2_tsu_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
m_t_25=mammals_grid2_tsu_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
b_t_25=birds_breeding_grid2_tsu_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()

write.xlsx(amphibians_grid2_hur_0,file = 'amphibians_grid2_hur_0_weighted3.xlsx')
write.xlsx(reptiles_grid2_hur_0,file = 'reptiles_grid2_hur_0_weighted3.xlsx')
write.xlsx(mammals_grid2_hur_0,file = 'mammals_grid2_hur_0_weighted3.xlsx')
write.xlsx(birds_breeding_grid2_hur_0,file = 'birds_grid2_hur_0_weighted3.xlsx')

a_h_25=amphibians_grid2_hur_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
r_h_25 = reptiles_grid2_hur_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
m_h_25 = mammals_grid2_hur_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()
b_h_25 = birds_breeding_grid2_hur_0 %>% filter(total_perc_afected>25 & avg_impact > 0.25) %>% nrow()


a_h_0=amphibians_grid2_hur_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
r_h_0 = reptiles_grid2_hur_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
m_h_0 = mammals_grid2_hur_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
b_h_0 = birds_breeding_grid2_hur_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()

a_t_0=amphibians_grid2_tsu_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
r_t_0=reptiles_grid2_tsu_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
m_t_0=mammals_grid2_tsu_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
b_t_0=birds_breeding_grid2_tsu_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()

a_e_0 = amphibians_grid2_ear_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
r_e_0 = reptiles_grid2_ear_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
m_e_0 = mammals_grid2_ear_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()
b_e_0 = birds_breeding_grid2_ear_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0) %>% nrow()

a_v_0 = amphibians_grid2_vul_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0)%>% nrow()
r_v_0 = reptiles_grid2_vul_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0)%>% nrow()
m_v_0 = mammals_grid2_vul_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0)%>% nrow()
b_v_0 = birds_breeding_grid2_vul_0 %>% filter(total_perc_afected>0 & avg_impact > 0.0)%>% nrow()


df_table_MS = cbind.data.frame(Disaster = c(rep('Volcanos',4),rep('Tsunamis',4),rep('Earthquakes',4),rep('Hurricanes',4)),
                 Group = rep(c('Amphibians','Reptiles','Mammals','Birds'),4),
                 Total = c(a_v_0,r_v_0,m_v_0,b_v_0,
                       a_t_0,r_t_0,m_t_0,b_t_0,
                       a_e_0,r_e_0,m_e_0,b_e_0,
                       a_h_0,r_h_0,m_h_0,b_h_0), 
                 Threshold_25 = c(a_v_25,r_v_25,m_v_25,b_v_25,
                           a_t_25,r_t_25,m_t_25,b_t_25,
                           a_e_25,r_e_25,m_e_25,b_e_25,
                           a_h_25,r_h_25,m_h_25,b_h_25))


write.xlsx(df_table_MS,file = 'df_table_MS_v2.xlsx')

p1 = ggplot()+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="brown", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Amphibians (earthquakes)')


p2 = ggplot()+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="brown", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Reptiles (earthquakes)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="brown", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Mammals (earthquakes)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="brown", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Birds (earthquakes)')

library(gridExtra)

grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_earthquakesv2.png',plot = grid_earthquakes,width = 6,height = 6,dpi = 600)


##amphibians----

theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'none',
                               legend.key.height = unit(1,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(0.25,'cm'),
                               legend.title = element_text(size = 10,angle = 90,family = myfont),
                               axis.text.y = element_blank(),
                               axis.text.x = element_text(family = myfont),
                               axis.title.y = element_blank(),
                               axis.title.x = element_text(colour = 'white',family = myfont),
                               legend.text = element_text(family = myfont))

pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_a_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (vulcanos)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Vulcanos)',colors = alpha(pal1(10),0.5),limits = c(0,1))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_a_t = ggplot()+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Tsunamis)',colors = alpha(pal1(10),0.5),limits = c(0,1))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_a_e = ggplot()+
  #labs(title = 'Amphibians (earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Earthquakes)',colors = alpha(pal1(10),0.5),limits = c(0,1))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_a_h = ggplot()+
  #labs(title = 'Amphibians (hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Hurricanes)',colors = alpha(pal1(10),0.5),limits = c(0,1))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_amphibians.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)

library(gridExtra)

##reptiles----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_r_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Reptiles (vulcanos)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_r_t = ggplot()+
  #labs(title = 'Reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_r_e = ggplot()+
  #labs(title = 'Reptiles (earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_r_h = ggplot()+
  #labs(title = 'Reptiles (hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters


# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_reptiles.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)

##mammals----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_m_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Mammals (vulcanos)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_m_t = ggplot()+
  #labs(title = 'Mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_m_e = ggplot()+
  #labs(title = 'Mammals (earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_m_h = ggplot()+
  #labs(title = 'Mammals (hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters


# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_mammals.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)


##birds----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_b_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Birds (breeding) (vulcanos)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_b_t = ggplot()+
  #labs(title = 'Birds (breeding) (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_b_e = ggplot()+
  #labs(title = 'Birds (breeding) (earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_b_h = ggplot()+
  #labs(title = 'Birds (breeding) (hurricanes)')+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.x = element_text(colour = 'black',hjust = 1,size = 15))



# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_birds_breeding.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)

## grid all ----

grid_all= arrangeGrob(p1_a_v,p1_r_v,p1_m_v,p1_b_v,
                      p2_a_t,p2_r_t,p2_m_t,p2_b_t,
                      p3_a_e,p3_r_e,p3_m_e,p3_b_e,
                      p4_a_h,p4_r_h,p4_m_h,p4_b_h,
                      ncol = 4,heights=c(1,1,1,1), widths=c(1.1,0.933,0.933,0.933))

ggsave(filename = 'points_all_weighted3.png',plot = grid_all,width = 17,height = 10,dpi = 600)


ggplot()+
  geom_sf(data = amphibians_grid[amphibians_grid$binomial== 'Ichthyophis hypocyaneus',],aes(fill = vulcanos_prob_b))


#maps_greyed----

list_amphibians_cells = unique(amphibians_grid$layer)
list_reptiles_cells = unique(reptiles_grid$layer)
list_mammals_cells = unique(mammals_grid$layer)
list_birds_cells = unique(birds_breeding_grid$layer)

greyed_cells = unique(c(list_amphibians_cells,list_reptiles_cells,list_mammals_cells,list_birds_cells))
save(greyed_cells,file = 'greyed_cells.Rdata')



colnames(amphibians_grid2_ear_0)[2] = paste0(colnames(amphibians_grid2_ear_0)[2],'_ear')
colnames(amphibians_grid2_ear_0)[3] = paste0(colnames(amphibians_grid2_ear_0)[3],'_ear')

colnames(amphibians_grid2_tsu_0)[2] = paste0(colnames(amphibians_grid2_tsu_0)[2],'_tsu')
colnames(amphibians_grid2_tsu_0)[3] = paste0(colnames(amphibians_grid2_tsu_0)[3],'_tsu')

colnames(amphibians_grid2_vul_0)[2] = paste0(colnames(amphibians_grid2_vul_0)[2],'_vul')
colnames(amphibians_grid2_vul_0)[3] = paste0(colnames(amphibians_grid2_vul_0)[3],'_vul')

colnames(amphibians_grid2_hur_0)[2] = paste0(colnames(amphibians_grid2_hur_0)[2],'_hur')
colnames(amphibians_grid2_hur_0)[3] = paste0(colnames(amphibians_grid2_hur_0)[3],'_hur')

amphibians_0 = merge(amphibians_grid2_ear_0,amphibians_grid2_tsu_0,by = 'binomial',all.x=T,all.y = T)
amphibians_1 = merge(amphibians_0,amphibians_grid2_vul_0,by = 'binomial',all.x=T,all.y = T)
amphibians_final = merge(amphibians_1,amphibians_grid2_hur_0,by = 'binomial',all.x=T,all.y = T)

write.xlsx(amphibians_final,file = 'amphibians_grid2_all_disasters.xlsx')

## reptiles

colnames(reptiles_grid2_ear_0)[2] = paste0(colnames(reptiles_grid2_ear_0)[2],'_ear')
colnames(reptiles_grid2_ear_0)[3] = paste0(colnames(reptiles_grid2_ear_0)[3],'_ear')

colnames(reptiles_grid2_tsu_0)[2] = paste0(colnames(reptiles_grid2_tsu_0)[2],'_tsu')
colnames(reptiles_grid2_tsu_0)[3] = paste0(colnames(reptiles_grid2_tsu_0)[3],'_tsu')

colnames(reptiles_grid2_vul_0)[2] = paste0(colnames(reptiles_grid2_vul_0)[2],'_vul')
colnames(reptiles_grid2_vul_0)[3] = paste0(colnames(reptiles_grid2_vul_0)[3],'_vul')

colnames(reptiles_grid2_hur_0)[2] = paste0(colnames(reptiles_grid2_hur_0)[2],'_hur')
colnames(reptiles_grid2_hur_0)[3] = paste0(colnames(reptiles_grid2_hur_0)[3],'_hur')

reptiles_0 = merge(reptiles_grid2_ear_0,reptiles_grid2_tsu_0,by = 'binomial',all.x=T,all.y = T)
reptiles_1 = merge(reptiles_0,reptiles_grid2_vul_0,by = 'binomial',all.x=T,all.y = T)
reptiles_final = merge(reptiles_1,reptiles_grid2_hur_0,by = 'binomial',all.x=T,all.y = T)

write.xlsx(reptiles_final,file = 'reptiles_grid2_all_disasters.xlsx')
nrow(reptiles_final)

## mammals

colnames(mammals_grid2_ear_0)[2] = paste0(colnames(mammals_grid2_ear_0)[2],'_ear')
colnames(mammals_grid2_ear_0)[3] = paste0(colnames(mammals_grid2_ear_0)[3],'_ear')

colnames(mammals_grid2_tsu_0)[2] = paste0(colnames(mammals_grid2_tsu_0)[2],'_tsu')
colnames(mammals_grid2_tsu_0)[3] = paste0(colnames(mammals_grid2_tsu_0)[3],'_tsu')

colnames(mammals_grid2_vul_0)[2] = paste0(colnames(mammals_grid2_vul_0)[2],'_vul')
colnames(mammals_grid2_vul_0)[3] = paste0(colnames(mammals_grid2_vul_0)[3],'_vul')

colnames(mammals_grid2_hur_0)[2] = paste0(colnames(mammals_grid2_hur_0)[2],'_hur')
colnames(mammals_grid2_hur_0)[3] = paste0(colnames(mammals_grid2_hur_0)[3],'_hur')

mammals_0 = merge(mammals_grid2_ear_0,mammals_grid2_tsu_0,by = 'binomial',all.x=T,all.y = T)
mammals_1 = merge(mammals_0,mammals_grid2_vul_0,by = 'binomial',all.x=T,all.y = T)
mammals_final = merge(mammals_1,mammals_grid2_hur_0,by = 'binomial',all.x=T,all.y = T)

write.xlsx(mammals_final,file = 'mammals_grid2_all_disasters.xlsx')
nrow(mammals_final)

## birds_breeding

colnames(birds_breeding_grid2_ear_0)[2] = paste0(colnames(birds_breeding_grid2_ear_0)[2],'_ear')
colnames(birds_breeding_grid2_ear_0)[3] = paste0(colnames(birds_breeding_grid2_ear_0)[3],'_ear')

colnames(birds_breeding_grid2_tsu_0)[2] = paste0(colnames(birds_breeding_grid2_tsu_0)[2],'_tsu')
colnames(birds_breeding_grid2_tsu_0)[3] = paste0(colnames(birds_breeding_grid2_tsu_0)[3],'_tsu')

colnames(birds_breeding_grid2_vul_0)[2] = paste0(colnames(birds_breeding_grid2_vul_0)[2],'_vul')
colnames(birds_breeding_grid2_vul_0)[3] = paste0(colnames(birds_breeding_grid2_vul_0)[3],'_vul')

colnames(birds_breeding_grid2_hur_0)[2] = paste0(colnames(birds_breeding_grid2_hur_0)[2],'_hur')
colnames(birds_breeding_grid2_hur_0)[3] = paste0(colnames(birds_breeding_grid2_hur_0)[3],'_hur')

birds_breeding_0 = merge(birds_breeding_grid2_ear_0,birds_breeding_grid2_tsu_0,by = 'binomial',all.x=T,all.y = T)
birds_breeding_1 = merge(birds_breeding_0,birds_breeding_grid2_vul_0,by = 'binomial',all.x=T,all.y = T)
birds_breeding_final = merge(birds_breeding_1,birds_breeding_grid2_hur_0,by = 'binomial',all.x=T,all.y = T)

write.xlsx(birds_breeding_final,file = 'birds_breeding_grid2_all_disasters.xlsx')

nrow(amphibians_final)+
nrow(reptiles_final)+
nrow(mammals_final)+
nrow(birds_breeding_final)


amphibians_final$threshold_any = ifelse(amphibians_final$total_perc_afected_ear>24.5 & amphibians_final$avg_impact_ear>0.245 |
                                          amphibians_final$total_perc_afected_tsu>24.5 & amphibians_final$avg_impact_tsu>0.245|
                                          amphibians_final$total_perc_afected_vul>24.5 & amphibians_final$avg_impact_vul>0.245|
                                          amphibians_final$total_perc_afected_hur>24.5 & amphibians_final$avg_impact_hur>0.245,1,0)


amphibians_final %>% filter(threshold_any==1) %>% nrow()/amphibians_final %>% nrow()
write.xlsx(amphibians_final,file = 'amphibians_grid2_all_disasters.xlsx')


reptiles_final$threshold_any = ifelse(reptiles_final$total_perc_afected_ear>24.5 & reptiles_final$avg_impact_ear>0.245 |
                                          reptiles_final$total_perc_afected_tsu>24.5 & reptiles_final$avg_impact_tsu>0.245|
                                          reptiles_final$total_perc_afected_vul>24.5 & reptiles_final$avg_impact_vul>0.245|
                                          reptiles_final$total_perc_afected_hur>24.5 & reptiles_final$avg_impact_hur>0.245,1,0)


reptiles_final %>% filter(threshold_any==1) %>% nrow()/reptiles_final %>% nrow()
write.xlsx(reptiles_final,file = 'reptiles_grid2_all_disasters.xlsx')

mammals_final$threshold_any = ifelse(mammals_final$total_perc_afected_ear>24.5 & mammals_final$avg_impact_ear>0.245 |
                                        mammals_final$total_perc_afected_tsu>24.5 & mammals_final$avg_impact_tsu>0.245|
                                        mammals_final$total_perc_afected_vul>24.5 & mammals_final$avg_impact_vul>0.245|
                                        mammals_final$total_perc_afected_hur>24.5 & mammals_final$avg_impact_hur>0.245,1,0)


mammals_final %>% filter(threshold_any==1) %>% nrow()/mammals_final %>% nrow()
write.xlsx(mammals_final,file = 'mammals_grid2_all_disasters.xlsx')


birds_breeding_final$threshold_any = ifelse(birds_breeding_final$total_perc_afected_ear>24.5 & birds_breeding_final$avg_impact_ear>0.245 |
                                       birds_breeding_final$total_perc_afected_tsu>24.5 & birds_breeding_final$avg_impact_tsu>0.245|
                                       birds_breeding_final$total_perc_afected_vul>24.5 & birds_breeding_final$avg_impact_vul>0.245|
                                       birds_breeding_final$total_perc_afected_hur>24.5 & birds_breeding_final$avg_impact_hur>0.245,1,0)


birds_breeding_final %>% filter(threshold_any==1) %>% nrow()/birds_breeding_final %>% nrow()
write.xlsx(birds_breeding_final,file = 'birds_breeding_grid2_all_disasters.xlsx')


#I just checked the data, the number of species that meet our criteria is 8131. Birds = 918, Mammals = 811, Reptiles = 3138, amphibians = 3264
nrow(amphibians_final)/3264
  nrow(reptiles_final)/3138
  nrow(mammals_final)/811
  nrow(birds_breeding_final)/918

  
#amphibians
amphibians_taxonomy = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/taxonomy.csv')  
amphibians_assessments = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/assessments.csv')  
colnames(amphibians_taxonomy)
colnames(amphibians_assessments)
amphibians_taxonomy_assessments = merge(amphibians_taxonomy[,c(2,5,6,7)],amphibians_assessments[,c(3,4)],by = 'scientificName')  
amphibians_final = merge(amphibians_final,amphibians_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  
  


#reptiles
reptiles_taxonomy1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/taxonomy.csv')  
reptiles_taxonomy2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/taxonomy.csv')  
reptiles_taxonomy = rbind(reptiles_taxonomy1,reptiles_taxonomy2)
reptiles_assessments1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/assessments.csv')
reptiles_assessments2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/assessments.csv')
reptiles_assessments = rbind(reptiles_assessments1,reptiles_assessments2)
colnames(reptiles_taxonomy)
colnames(reptiles_assessments)
reptiles_taxonomy_assessments = merge(reptiles_taxonomy[,c(2,5,6,7)],reptiles_assessments[,c(3,4)],by = 'scientificName')  
reptiles_final = merge(reptiles_final,reptiles_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  

reptiles_final[is.na(reptiles_final$className),]

#mammals
mammals_taxonomy = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Mammals/taxonomy.csv')  
mammals_assessments = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Mammals/assessments.csv')  
colnames(mammals_taxonomy)
colnames(mammals_assessments)
mammals_taxonomy_assessments = merge(mammals_taxonomy[,c(2,5,6,7)],mammals_assessments[,c(3,4)],by = 'scientificName')  
mammals_final = merge(mammals_final,mammals_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  

mammals_final[is.na(mammals_final$className),]

#birds
birds_taxonomy1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/taxonomy.csv') 
birds_taxonomy2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/taxonomy.csv')  
birds_taxonomy = rbind(birds_taxonomy1,birds_taxonomy2)

birds_assessments1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/assessments.csv')
birds_assessments2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/assessments.csv') 
birds_assessments = rbind(birds_assessments1,birds_assessments2)


colnames(birds_taxonomy)
colnames(birds_assessments)
birds_taxonomy_assessments = merge(birds_taxonomy[,c(2,5,6,7)],birds_assessments[,c(3,4)],by = 'scientificName')  
birds_final = merge(birds_breeding_final,birds_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  

birds_final[is.na(birds_final$className),]
write.xlsx(amphibians_final,file = 'amphibians_grid2_all_disasters.xlsx')
write.xlsx(reptiles_final,file = 'reptiles_grid2_all_disasters.xlsx')
write.xlsx(mammals_final,file = 'mammals_grid2_all_disasters.xlsx')
write.xlsx(birds_final,file = 'birds_breeding_grid2_all_disasters.xlsx')

