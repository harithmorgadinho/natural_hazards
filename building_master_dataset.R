#top species

library(openxlsx)

sf_hur = read.xlsx('birds_grid2_hur_0_weighted3.xlsx',sheet = 1)
sf_ear = read.xlsx('birds_grid2_ear_0_weighted3.xlsx',sheet = 1)
sf_vul = read.xlsx('birds_grid2_vul_0_weighted3.xlsx',sheet = 1)
sf_vul[sf_vul$total_perc_afected>24.5 & sf_vul$avg_impact>0.245,]



sf_tsu = read.xlsx('birds_grid2_tsu_0_weighted3.xlsx',sheet = 1)
length(unique(c(sf_hur$binomial,sf_ear,sf_vul,sf_tsu)))

length(unique(c(sf_hur[sf_hur$total_perc_afected>24.5 & sf_hur$avg_impact>0.245,]$binomial,
                sf_ear[sf_ear$total_perc_afected>24.5 & sf_ear$avg_impact>0.245,]$binomial,
                sf_vul[sf_vul$total_perc_afected>24.5 & sf_vul$avg_impact>0.245,]$binomial,
                sf_tsu[sf_tsu$total_perc_afected>24.5 & sf_tsu$avg_impact>0.245,]$binomial)))

496/572

sf_hur = read.xlsx('mammals_grid2_hur_0_weighted3.xlsx',sheet = 1)
sf_ear = read.xlsx('mammals_grid2_ear_0_weighted3.xlsx',sheet = 1)
sf_vul = read.xlsx('mammals_grid2_vul_0_weighted3.xlsx',sheet = 1)
sf_tsu = read.xlsx('mammals_grid2_tsu_0_weighted3.xlsx',sheet = 1)
length(unique(c(sf_hur$binomial,sf_ear,sf_vul,sf_tsu)))

length(unique(c(sf_hur[sf_hur$total_perc_afected>24.5 & sf_hur$avg_impact>0.245,]$binomial,
                sf_ear[sf_ear$total_perc_afected>24.5 & sf_ear$avg_impact>0.245,]$binomial,
                sf_vul[sf_vul$total_perc_afected>24.5 & sf_vul$avg_impact>0.245,]$binomial,
                sf_tsu[sf_tsu$total_perc_afected>24.5 & sf_tsu$avg_impact>0.245,]$binomial)))
401/418

sf_hur = read.xlsx('reptiles_grid2_hur_0_weighted3.xlsx',sheet = 1)
sf_ear = read.xlsx('reptiles_grid2_ear_0_weighted3.xlsx',sheet = 1)
sf_vul = read.xlsx('reptiles_grid2_vul_0_weighted3.xlsx',sheet = 1)
sf_tsu = read.xlsx('reptiles_grid2_tsu_0_weighted3.xlsx',sheet = 1)
length(unique(c(sf_hur$binomial,sf_ear,sf_vul,sf_tsu)))

length(unique(c(sf_hur[sf_hur$total_perc_afected>24.5 & sf_hur$avg_impact>0.245,]$binomial,
                sf_ear[sf_ear$total_perc_afected>24.5 & sf_ear$avg_impact>0.245,]$binomial,
                sf_vul[sf_vul$total_perc_afected>24.5 & sf_vul$avg_impact>0.245,]$binomial,
                sf_tsu[sf_tsu$total_perc_afected>24.5 & sf_tsu$avg_impact>0.245,]$binomial)))

1721/1805

sf_hur = read.xlsx('amphibians_grid2_hur_0_weighted3.xlsx',sheet = 1)
sf_ear = read.xlsx('amphibians_grid2_ear_0_weighted3.xlsx',sheet = 1)
sf_vul = read.xlsx('amphibians_grid2_vul_0_weighted3.xlsx',sheet = 1)
sf_tsu = read.xlsx('amphibians_grid2_tsu_0_weighted3.xlsx',sheet = 1)
length(unique(c(sf_hur$binomial,sf_ear,sf_vul,sf_tsu)))

length(unique(c(sf_hur[sf_hur$total_perc_afected>24.5 & sf_hur$avg_impact>0.245,]$binomial,
                sf_ear[sf_ear$total_perc_afected>24.5 & sf_ear$avg_impact>0.245,]$binomial,
                sf_vul[sf_vul$total_perc_afected>24.5 & sf_vul$avg_impact>0.245,]$binomial,
                sf_tsu[sf_tsu$total_perc_afected>24.5 & sf_tsu$avg_impact>0.245,]$binomial)))

1264/1299



1299+1805+418+572

4094/8131 



