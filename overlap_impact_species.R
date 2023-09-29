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
load('birds_breeding_resident_filtered.Rdata')

check_species = c('Amazona arausiaca','Amazona imperialis','Amazona vittata','Icterus oberi','Colostethus jacobuspetersi')

birds_breeding_resident[birds_breeding_resident$binomial %in% check_species,]

birds_breeding = birds_breeding_resident



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




amphibians_grid2$weighted_range_size_intersect = amphibians_grid2$range_size_intersect/amphibians_grid2$square_area
reptiles_grid2$weighted_range_size_intersect = reptiles_grid2$range_size_intersect/reptiles_grid2$square_area
mammals_grid2$weighted_range_size_intersect = mammals_grid2$range_size_intersect/mammals_grid2$square_area
birds_breeding_grid2$weighted_range_size_intersect = birds_breeding_grid2$range_size_intersect/birds_breeding_grid2$square_area


amphibians_grid2_vul = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b*weighted_range_size_intersect))
reptiles_grid2_vul = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b*weighted_range_size_intersect))
mammals_grid2_vul = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b*weighted_range_size_intersect))
birds_breeding_grid2_vul = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>25 & vulcanos_prob_b > 0.25) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b*weighted_range_size_intersect))

library(ggplot2)


amphibians_grid2_vul_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b))
reptiles_grid2_vul_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b))
mammals_grid2_vul_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b))
birds_breeding_grid2_vul_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & vulcanos_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(vulcanos_prob_b))



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

amphibians_grid2_tsu_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))
reptiles_grid2_tsu_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))
mammals_grid2_tsu_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))
birds_breeding_grid2_tsu_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))


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


amphibians_grid2_hur_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))
reptiles_grid2_hur_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))
mammals_grid2_hur_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))
birds_breeding_grid2_hur_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))

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

amphibians_grid2_ear_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(earthquakes_prob_b))
reptiles_grid2_ear_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(earthquakes_prob_b))
mammals_grid2_ear_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(earthquakes_prob_b))
birds_breeding_grid2_ear_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(earthquakes_prob_b))

library(openxlsx)
write.xlsx(amphibians_grid2_vul_0,file = 'amphibians_grid2_vul_0.xlsx')
write.xlsx(reptiles_grid2_vul_0,file = 'reptiles_grid2_vul_0.xlsx')
write.xlsx(mammals_grid2_vul_0,file = 'mammals_grid2_vul_0.xlsx')
write.xlsx(birds_breeding_grid2_vul_0,file = 'birds_grid2_vul_0.xlsx')

write.xlsx(amphibians_grid2_ear_0,file = 'amphibians_grid2_ear_0.xlsx')
write.xlsx(reptiles_grid2_ear_0,file = 'reptiles_grid2_ear_0.xlsx')
write.xlsx(mammals_grid2_ear_0,file = 'mammals_grid2_ear_0.xlsx')
write.xlsx(birds_breeding_grid2_ear_0,file = 'birds_grid2_ear_0.xlsx')

write.xlsx(amphibians_grid2_tsu_0,file = 'amphibians_grid2_tsu_0.xlsx')
write.xlsx(reptiles_grid2_tsu_0,file = 'reptiles_grid2_tsu_0.xlsx')
write.xlsx(mammals_grid2_tsu_0,file = 'mammals_grid2_tsu_0.xlsx')
write.xlsx(birds_breeding_grid2_tsu_0,file = 'birds_grid2_tsu_0.xlsx')

write.xlsx(amphibians_grid2_hur_0,file = 'amphibians_grid2_hur_0.xlsx')
write.xlsx(reptiles_grid2_hur_0,file = 'reptiles_grid2_hur_0.xlsx')
write.xlsx(mammals_grid2_hur_0,file = 'mammals_grid2_hur_0.xlsx')
write.xlsx(birds_breeding_grid2_hur_0,file = 'birds_grid2_hur_0.xlsx')



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

ggsave(filename = 'points_earthquakes.png',plot = grid_earthquakes,width = 6,height = 6,dpi = 600)


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

ggsave(filename = 'points_all.png',plot = grid_all,width = 17,height = 10,dpi = 600)



#maps_greyed----

list_amphibians_cells = unique(amphibians_grid$layer)
list_reptiles_cells = unique(reptiles_grid$layer)
list_mammals_cells = unique(mammals_grid$layer)
list_birds_cells = unique(birds_breeding_grid$layer)

greyed_cells = unique(c(list_amphibians_cells,list_reptiles_cells,list_mammals_cells,list_birds_cells))
save(greyed_cells,file = 'greyed_cells.Rdata')

