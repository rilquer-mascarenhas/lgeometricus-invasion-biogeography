setwd('/home/rilquer/lgeo_plat/')
require(tidyverse)

## Sample data ####
# Reading sample information
mastersheet <- read_csv('data/sample_data/york_master_samplesheet.csv')

### Sampling map####
require(rnaturalearthdata)
require(terra)
require(ggrepel)
require(sf)

####Lgeo####
lgeo_sf <- st_as_sf(mastersheet %>% filter(binomial == 'Latrodectus geometricus') %>%
                      add_count(locality) %>% distinct(locality,.keep_all = T),
                    coords = c('longitude','latitude'),crs=4326)
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_sf(data = lgeo_sf
          ,aes(fill=radseq),size=3.5,pch=21)+
  geom_sf_text(data = lgeo_sf, aes(label=n),size=2)+
  labs(x = 'Longitude',y='Latitude')+
  #xlim(-80,-60)+ylim(-10,15)+
  #coord_sf(xlim = c(29.19,40.74),ylim = c(28.17,36.49))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = 'none')+
  ggtitle(expression(paste("Sampling Map of ", italic("Latrodectus geometricus"))))
ggsave('output/sample_maps/lgeo_all.png',width = 10,height = 8,dpi = 1200)

# Simpler map without sample size
# First made for CUNY EEB Sympisium Mar 2 2026
ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = mastersheet %>% filter(binomial == 'Latrodectus geometricus') %>%
               distinct(region1,.keep_all = T),
             aes(x = longitude,y = latitude),
             size = 2,alpha=0.8,color='black',fill='brown',pch=21)+
  coord_sf(xlim = c(-149,147.351841),ylim = c(-50.918958,60.247206))+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave('output/sample_maps/lgeo_all.png',width = 6, height = 3,dpi = 600)

####Plat####
plat_sf <- st_as_sf(mastersheet %>%
                      filter(binomial == 'Philolema latrodecti' & wgs == 'no') %>% 
                      add_count(region1) %>% distinct(region1,.keep_all = T),
                    coords = c('longitude','latitude'),crs=4326)
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_sf(data = plat_sf,
          aes(fill=radseq),size=3.5,pch=21)+
  geom_sf_label(data = plat_sf,
                aes(label=n),size=3,
                vjust = -0.5,
                fun.geometry = st_centroid,
                colour = "black")+
  labs(x = 'Longitude',y='Latitude')+
  #coord_sf(xlim = c(29.19,40.74),ylim = c(28.17,36.49))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = 'none')+
  ggtitle(expression(paste("Sampling Map of ", italic("Philolema latrodecti"))))
ggsave('output/sample_maps/plat_all.png',width = 10,height = 8,dpi = 1200)

## L. geo RADSeq####
### Samples info####
samples <- read_csv('data/lgeometricus_samples.csv') %>%
  # Adding broader region categorization
  mutate(region1 = case_when(Population %in% c('Beer Sheva','Eilat','Haifa',
                                               'Sede Boqer','Tel Aviv','Yeruham') ~ 'Israel',
                             Population %in% c('Cape Town, South Africa','George, South Africa',
                                               'Johannesburg, South Africa','Kimberley, South Africa',
                                               'Modimolle, South Africa','Pretoria, South Africa',
                                               'Riebeeck Kasteel, South Africa') ~ 'South Africa',
                             Population %in% c('Edisto Island, South Carolina','Gainesville Florida',
                                               'Los Angeles California','Texas') ~ 'United States',
                             Population == 'Hatzerim pallidus' ~ 'H. pallidus')) %>%
  mutate(region1 = fct_relevel(factor(region1), "South Africa","Israel","United States","H. pallidus"))
# Making some maps to come up with some finer geographic groups
require(rnaturalearthdata)
require(terra)
pop_colors = c(RColorBrewer::brewer.pal(name='Paired',n=12),
               RColorBrewer::brewer.pal(name='Accent',n=6))

# Israel map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = samples %>% filter(region1 == 'Israel'),
             aes(x=long,y=lat,color=Population),size=5,pch=20)+
  scale_color_manual(values = pop_colors)+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(29.19,40.74),ylim = c(28.17,36.49))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"))
## Will separate into 4 groups:
# 1) Haifa at the north;
# 2) Tel Aviv;
## 3) Beer Sheva, Sede Boqer and Yeruham in the center;
## 4) Eilat in the south.

# South Africa map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = samples %>% filter(region1 == 'South Africa'),
             aes(x=long,y=lat,color=Population),size=5,pch=20)+
  scale_color_manual(values = pop_colors)+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(3.57,38.06),ylim = c(-37.64,-10.43))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"))
## Will separate into 4 groups:
# 1) Cape Town and Riebeeck Kasteel
# 2) George
# 3) Kimberley
# 4) Johannesburg, Pretoria and Modimolle

## United States will be separated into the four states: South Carolina, Florida, Texas and California

# Adding finer geographic grouping

samples <- samples %>% 
  mutate(region2 = case_when(Population == 'Haifa' ~ 'Northern Israel',
                             Population == 'Tel Aviv' ~ 'Tel Aviv, Israel',
                             Population %in% c('Beer Sheva','Sede Boqer',
                                               'Yeruham') ~ 'Central Israel',
                             Population == 'Eilat' ~ 'South Israel',
                             Population %in% c('Cape Town, South Africa',
                                               'Riebeeck Kasteel, South Africa') ~ 'Southwestern South Africa',
                             Population == 'George, South Africa' ~ 'Southern South Africa',
                             Population == 'Kimberley, South Africa' ~ 'Central South Africa',
                             Population %in% c('Johannesburg, South Africa',
                                               'Modimolle, South Africa',
                                               'Pretoria, South Africa') ~ 'Northeastern South Africa',
                             Population == 'Edisto Island, South Carolina' ~ 'South Carolina, US',
                             Population == 'Gainesville Florida' ~ 'Florida, US',
                             Population == 'Texas' ~ 'Texas, US',
                             Population == 'Los Angeles California' ~ 'California, US',
                             Population == 'Hatzerim pallidus' ~ 'H. pallidus')) %>% 
  mutate(region2 = fct_relevel(factor(region2),
                               'Southwestern South Africa','Southern South Africa',
                               'Central South Africa','Northeastern South Africa',
                               'Northern Israel','Tel Aviv, Israel','Central Israel',
                               'South Israel','South Carolina, US','Florida, US',
                               'Texas, US','California, US','H. pallidus')) %>%
  # Adding iPyrad data for checks
  left_join(read_csv('data/ipyrad/lgeometricus_ipyrad_s2_stats.csv'),by='sample_name') %>%
  mutate(filter_dropped = reads_raw - reads_passed_filter)

### Map of localities####
## Final zoomed in maps, settings colors that will be used for PCA
require(rnaturalearthdata)
require(terra)
pop_colors = c(RColorBrewer::brewer.pal(name='Paired',n=12),
               RColorBrewer::brewer.pal(name='Accent',n=6))

# Following color order of factors above
# 1. South Africa
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = samples %>% filter(region1 == 'South Africa'),
             aes(x=long,y=lat,fill=region2),size=11,pch=21)+
  scale_fill_manual(values = pop_colors[1:4])+
  coord_sf(xlim = c(12.57,36.06),ylim = c(-37.64,-20.43))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none')
ggsave('output/sample_maps/South_Africa.png',width = 8,height = 8,dpi = 1200)

# 2. Israel map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = samples %>% filter(region1 == 'Israel'),
             aes(x=long,y=lat,fill=region2),size=11,pch=21)+
  scale_fill_manual(values = pop_colors[c(5,6,7,18)])+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(32.19,38.74),ylim = c(28.57,33.49))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none')
ggsave('output/sample_maps/Israel.png',width = 8,height = 8,dpi = 1200)

# US map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = samples %>% filter(region1 == 'United States'),
             aes(x=long,y=lat,fill=region2),size=13,pch=21)+
  scale_fill_manual(values = pop_colors[9:12])+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(-122.93,-75.86),ylim = c(23.75,44.57))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none')
ggsave('output/sample_maps/United_States.png',width = 10,height = 8,dpi = 1200)

### iPyrad info####
require(ggrepel)
ggplot(samples, aes(x=total_DNA,y=reads_passed_filter,label=sample_name))+
  geom_point()+
  geom_text_repel()

ggplot(samples, aes(x=total_DNA,y=filter_dropped,label=sample_name))+
  geom_point()+
  geom_text_repel()

ggplot(samples,aes(x=sample_name,y=reads_passed_filter,label=sample_name,
                   color = Population))+geom_point()+
  geom_text_repel()+
  geom_hline(yintercept = 5e5, linetype = 'dashed')+
  geom_hline(yintercept = 1e6, linetype = 'dashed')+
  geom_hline(yintercept = 5e6, linetype = 'dashed')+
  geom_hline(yintercept = 1e7, linetype = 'dashed')+
  scale_y_continuous(breaks = c('0' = 0,'500,000'=5e5,'1,000,000'=1e6,
                                '5,000,000'=5e6,'10,000,000'=1e7,
                                '15,000,000'=1.5e7))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

## Putting a filter of equal or above 500000 reads is a good compromise at this point between
## number of reads and losing pop
# List of samples to drop
samples %>% filter(reads_passed_filter < 5e5) %>% select(sample_name) %>% unlist() %>%
  as.character()

### PCA####

#### Metadata####
# Creating dataframe with some information on the assemblies
# Keeps them in order when reading, easier to plot results later

assemblies <- data.frame(name = c("lgeo_posts5_globalmin",
                                  "lgeo_posts5_globalmin_min75",
                                  "lgeo_posts5_globalmin_ct85step6",
                                  "lgeo_posts5_globalmin_ct85step6_min75",
                                  "lgeo_posts5_globalmin_ct80step6",
                                  "lgeo_posts5_globalmin_ct80step6_min75",
                                  "lgeo_posts5_globalmin_woOutgroup",
                                  "lgeo_posts5_globalmin_woOutgroup_min75",
                                  "lgeo_posts5_globalmin_woOutgroup_ct85",
                                  "lgeo_posts5_globalmin_woOutgroup_ct85_min75",
                                  "lgeometricus_ref_S2min5e5_postS345_ingroup",
                                  "lgeometricus_ref_S2min5e5_postS345_ingroup_min75",
                                  "lgeometricus_ref_woLowCov",
                                  "lgeometricus_ref_woLowCov_min75"),
                         type = c("denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "denovo",
                                  "reference",
                                  "reference",
                                  "reference",
                                  "reference"),
                         in_out = c("In + Out",
                                    "In + Out",
                                    "In + Out",
                                    "In + Out",
                                    "In + Out",
                                    "In + Out",
                                    "Ingroup only",
                                    "Ingroup only",
                                    "Ingroup only",
                                    "Ingroup only",
                                    "Ingroup only",
                                    "Ingroup only",
                                    "Ingroup only",
                                    "Ingroup only"),
                         s6_CT = c("0.90",
                                   "0.90",
                                   "0.85",
                                   "0.85",
                                   "0.80",
                                   "0.80",
                                   "0.90",
                                   "0.90",
                                   "0.85",
                                   "0.85",
                                   "0.85",
                                   "0.85",
                                   "0.85",
                                   "0.85"),
                         minN = c("50%",
                                  "75%",
                                  "50%",
                                  "75%",
                                  "50%",
                                  "75%",
                                  "50%",
                                  "75%",
                                  "50%",
                                  "75%",
                                  "50%",
                                  "75%",
                                  "50%",
                                  "75%"))

## Reading VCF files
require(vcfR)
require(SNPfiltR)
vcfs <- lapply(assemblies$name,function(x){read.vcfR(paste0('data/vcfs/',x,'.vcf'))})
names(vcfs) <- assemblies$name

# Retrieving N samples, N RAD loci and total of N SNPs
assemblies_info <- lapply(vcfs,function(x){
  info <- c(nsamples = x@gt %>% ncol()-1,
            nradloci = x@fix[,3] %>% str_split_i(pattern = '_',1) %>% unique() %>% length(),
            nsnps = x@gt %>% nrow())
}) %>% bind_rows() %>%
  add_column(assemblies,.before = 'nsamples')

# Visualizing info on these assemblies
# First denovo assemblies
# Labeller for the facet of NLoci and NSNPs
labels <- c(nradloci = 'N of Loci',
            nsnps = 'Total N of SNPs')

# Ingroup and outgroup
library(scales)
ggplot(assemblies_info %>%
         # Pivotting to plot Loci and SNPS with facet
         pivot_longer(c('nradloci','nsnps'),
                      names_to = 'data',
                      values_to = 'N') %>%
         # Filtering for ingroup
         filter(in_out == 'In + Out') %>%
         # Keeping only denovo
         filter(type == 'denovo'),
       aes(x=s6_CT,
           y=N,
           fill=minN))+
  geom_point(size=5,pch=21)+
  facet_wrap(~data,ncol=2,scales = 'free',
             labeller = labeller(data = labels))+
  theme_minimal()+
  labs(x = 'Clustering Threshold on Step 6',
       y = 'N')+
  scale_y_continuous(labels = label_comma())+
  scale_fill_discrete(name = 'Min. N of represented individuals (% of total)')+
  theme(legend.position = "top",
        legend.text = element_text(size=11),
        strip.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12))+
  ggtitle('Variation in N of Loci and SNPs across assembly parameters',
          subtitle = 'de dnvo - Ingroup + Outgroup')
ggsave('output/assemblies/denovo_stats_in_out.png',width = 12,height = 6,dpi = 1200)

# Ingroup only
ggplot(assemblies_info %>%
         # Pivotting to plot Loci and SNPS with facet
         pivot_longer(c('nradloci','nsnps'),
                      names_to = 'data',
                      values_to = 'N') %>%
         # Filtering for ingroup
         filter(in_out == 'Ingroup only') %>%
         # Keeping only denovo
         filter(type == 'denovo'),
       aes(x=s6_CT,
           y=N,
           fill=minN))+
  geom_point(size=5,pch=21)+
  facet_wrap(~data,ncol=2,scales = 'free',
             labeller = labeller(data = labels))+
  theme_minimal()+
  labs(x = 'Clustering Threshold on Step 6',
       y = 'N')+
  scale_y_continuous(labels = label_comma())+
  scale_fill_discrete(name = 'Min. N of represented individuals (% of total)')+
  theme(legend.position = "top",
        legend.text = element_text(size=11),
        strip.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12))+
  ggtitle('Variation in N of Loci and SNPs across assembly parameters',
          subtitle = 'de novo - Ingroup only')
ggsave('output/assemblies/denovo_stats_in_only.png',width = 12,height = 6,dpi = 1200)

## Plotting changes in N RAD Loci and NSNPs per missing data for reference assemblies
ggplot(assemblies_info %>%
         mutate(nradloci10 = nradloci * 10) %>% 
         # Pivotting to plot Loci and SNPS with facet
         pivot_longer(c('nradloci10','nsnps'),
                      names_to = 'data',
                      values_to = 'N') %>%
         # Keeping only denovo
         filter(type == 'reference'),
       aes(x=minN,
           y=N))+
  geom_point(size=5,pch=21,aes(fill=data))+
  scale_fill_manual(name = 'Stat',
                    labels = c('N of Loci','Total N of SNPs'),
                    values = c("#F8766D","#00BFC4"))+
  geom_line(aes(group=data))+
  theme_minimal()+
  labs(x = 'Min. N of represented individuals (% of total)',
       y = 'Total N of SNPs')+
  # Add a second axis and specify its features
  scale_y_continuous(labels = label_comma(),
                     sec.axis = sec_axis(trans=~./10, name="N of Loci"))+
  
  theme(legend.position = "top",
        strip.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12))+
  ggtitle('Variation in N of Loci and SNPs across minimum required N of represented individuals',
          subtitle = 'Reference - Ingroup only')
ggsave('output/assemblies/ref_stats_in_only.png',width = 12,height = 6,dpi = 1200)

#### Adegenet PCA####

## PCA through adegenet on genind object
## https://adegenet.r-forge.r-project.org/files/PRstats/practical-MVAintro.1.0.pdf
require(adegenet)
pca_results <- lapply(13:length(vcfs),function(x){
  vcfR2genind(vcfs[[x]]) %>% tab(freq=TRUE, NA.method="mean") %>%
    dudi.pca(center=TRUE, scale=FALSE, scannf = FALSE, nf = 10)
})
saveRDS(pca_results,'rds/pca_results_adegenet.rds')

# Plotting results
require(ggrepel)
lapply(1:length(pca_results),function(i){
  pcadata <- pca_results[[i]]$li %>% mutate(sample_name = rownames(pca_results[[i]]$li)) %>%
    as_tibble() %>% left_join(samples,by='sample_name')
  
  if (i == 1) {
    # Plotting all and zoom on some groups
    # Whole PCA with somewhat small points, zoomed images with bigger points
    # All
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=4,alpha=1,pch=21)+
      #geom_text_repel(max.overlaps = 15)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_classic()+
      ggtitle(paste0('PCA - ',assemblies_info$type[i],', ',assemblies_info$in_out[i]),
              subtitle = paste0('CT = ',assemblies_info$s6_CT[i],'; minN = ',
                                assemblies_info$minN[i],'\nN Samples = ',assemblies_info$nsamples[i],
                                '; N Loci = ',assemblies_info$nradloci[i],
                                '; N SNPs = ',assemblies_info$nsnps[i]))
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_all.png'),width = 10,height = 8,dpi = 1200)
    
    
    # Focused on the cluster in the middle
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=13,alpha=1,pch=21)+
      #geom_text_repel(max.overlaps = 28)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_void()+
      theme(legend.position = 'none',
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.tick = element_blank())+
      xlim(-0.35,0.15)+ylim(-0.05,0.25)
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_middle.png'),width = 8,height = 8,dpi = 1200)
    
    # Focused on the Israel cluster
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=13,alpha=1,pch=21)+
      #geom_text_repel(max.overlaps = 28)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_void()+
      theme(legend.position = 'none',
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.tick = element_blank())+
      xlim(-76,-66)+ylim(-4,-1.5)
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_Israel.png'),width = 8,height = 8,dpi = 1200)
    
    # Focused on the United States cluster
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=13,alpha=1,pch=21)+
      #geom_text_repel(max.overlaps = 28)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_void()+
      theme(legend.position = 'none',
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.tick = element_blank())+
      xlim(28,36)+ylim(25,40)
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_US.png'),width = 8,height = 8,dpi = 1200)
    
    # Repeating with labels
    # All
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=4,alpha=1,pch=21)+
      geom_text_repel(max.overlaps = 15, size = 7)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_classic()+
      ggtitle(paste0('PCA - ',assemblies_info$type[i],', ',assemblies_info$in_out[i]),
              subtitle = paste0('CT = ',assemblies_info$s6_CT[i],'; minN = ',
                                assemblies_info$minN[i],'\nN Samples = ',assemblies_info$nsamples[i],
                                '; N Loci = ',assemblies_info$nradloci[i],
                                '; N SNPs = ',assemblies_info$nsnps[i]))
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_all_labels.png'),width = 10,height = 8,dpi = 1200)
    
    
    # Focused on the cluster in the middle
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=13,alpha=1,pch=21)+
      geom_text_repel(max.overlaps = 28,size = 10)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_void()+
      theme(legend.position = 'none',
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.tick = element_blank())+
      xlim(-0.35,0.15)+ylim(-0.05,0.25)
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_middle_labels.png'),width = 8,height = 8,dpi = 1200)
    
    # Focused on the Israel cluster
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=13,alpha=1,pch=21)+
      geom_text_repel(max.overlaps = 28,size = 10)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_void()+
      theme(legend.position = 'none',
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.tick = element_blank())+
      xlim(-76,-66)+ylim(-4,-1.5)
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_Israel_labels.png'),width = 8,height = 8,dpi = 1200)
    
    # Focused on the United States cluster
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=13,alpha=1,pch=21)+
      geom_text_repel(max.overlaps = 28,size = 10)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_void()+
      theme(legend.position = 'none',
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.tick = element_blank())+
      xlim(28,36)+ylim(25,40)
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_US_labels.png'),width = 8,height = 8,dpi = 1200)
    
  } else {
    ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
      geom_point(size=6,alpha=1,pch=21)+
      #geom_text_repel(max.overlaps = 15)+
      # Grey for outgroup
      scale_fill_manual(name = 'Region',values = c(pop_colors,'grey'))+
      xlab('PC1')+ylab('PC2')+
      theme_classic()+
      ggtitle(paste0('PCA - ',assemblies_info$type[i],', ',assemblies_info$in_out[i]),
              subtitle = paste0('CT = ',assemblies_info$s6_CT[i],'; minN = ',
                                assemblies_info$minN[i],'\nN Samples = ',assemblies_info$nsamples[i],
                                '; N Loci = ',assemblies_info$nradloci[i],
                                '; N SNPs = ',assemblies_info$nsnps[i]))
    ggsave(paste0('output/pca_results/',i,'_',names(pca_results)[i],'_all.png'),width = 10,height = 8,dpi = 1200)
  }
})

##### Reference w/o problematic samples####
##### lgeometricus_ref_woLowCov
require(ggrepel)
pcadata <- pca_results[[1]]$li %>% mutate(sample_name = rownames(pca_results[[1]]$li)) %>% 
  as_tibble() %>% left_join(samples,by='sample_name')

# All - with label
ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
  geom_point(size=4,alpha=1,pch=21)+
  geom_text_repel(max.overlaps = 15, size = 7)+
  # Grey for outgroup, new set of colors to account for removed Eilat
  scale_fill_manual(name = 'Region',values = c(pop_colors[c(1:7,9:12)],'grey'))+
  xlab('PC1')+ylab('PC2')+
  theme_classic()+
  ggtitle(paste0('PCA - ',assemblies_info$type[13],', ',assemblies_info$in_out[13]),
          subtitle = paste0('CT = ',assemblies_info$s6_CT[13],'; minN = ',
                            assemblies_info$minN[13],'\nN Samples = ',assemblies_info$nsamples[13],
                            '; N Loci = ',assemblies_info$nradloci[13],
                            '; N SNPs = ',assemblies_info$nsnps[13]))
ggsave('output/pca_results/13_lgeometricus_ref_woLowCov_all_labels.png',width = 10,height = 8,dpi = 1200)

# Focused on the Israel cluster
ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
  geom_point(size=13,alpha=1,pch=21)+
  #geom_text_repel(max.overlaps = 28,size = 10)+
  # Grey for outgroup
  scale_fill_manual(name = 'Region',values = c(pop_colors[c(1:7,9:12)],'grey'))+
  xlab('PC1')+ylab('PC2')+
  theme_void()+
  theme(legend.position = 'none',
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.tick = element_blank())+
  xlim(-95,-66)+ylim(-4,-1.5)
ggsave('output/pca_results/13_lgeometricus_ref_woLowCov_Israel.png',width = 8,height = 8,dpi = 1200)

# Focused on the United States cluster
ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
  geom_point(size=13,alpha=1,pch=21)+
  #geom_text_repel(max.overlaps = 28,size = 10)+
  # Grey for outgroup
  scale_fill_manual(name = 'Region',values = c(pop_colors[c(1:7,9:12)],'grey'))+
  xlab('PC1')+ylab('PC2')+
  theme_void()+
  theme(legend.position = 'none',
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.tick = element_blank())+
  xlim(35,40)+ylim(43,50)
ggsave('output/pca_results/13_lgeometricus_ref_woLowCov_US.png',width = 8,height = 8,dpi = 1200)

# lgeometricus_ref_woLowCov_min75
pcadata <- pca_results[[2]]$li %>% mutate(sample_name = rownames(pca_results[[2]]$li)) %>% 
  as_tibble() %>% left_join(samples,by='sample_name')

# All - with label
ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=sample_name))+
  geom_point(size=4,alpha=1,pch=21)+
  #geom_text_repel(max.overlaps = 15, size = 7)+
  # Grey for outgroup, new set of colors to account for removed Eilat
  scale_fill_manual(name = 'Region',values = c(pop_colors[c(1:7,9:12)],'grey'))+
  xlab('PC1')+ylab('PC2')+
  theme_classic()+
  ggtitle(paste0('PCA - ',assemblies_info$type[14],', ',assemblies_info$in_out[14]),
          subtitle = paste0('CT = ',assemblies_info$s6_CT[14],'; minN = ',
                            assemblies_info$minN[14],'\nN Samples = ',assemblies_info$nsamples[14],
                            '; N Loci = ',assemblies_info$nradloci[14],
                            '; N SNPs = ',assemblies_info$nsnps[14]))
ggsave('output/pca_results/14_lgeometricus_ref_woLowCov_min75_all_labels.png',width = 10,height = 8,dpi = 1200)

###Filtered SNPs
#library(SNPfiltR)
#distance_thin(vcfs[[3]], min.distance = 1000)
#distance_thin(vcfs[[12]], min.distance = 1000)

#### PCADAPT####
#### Checking sample coverage####
stats <- lapply((list.files('data/gen_data/ipyrad/assemblies_final_stats/',pattern='.csv') %>%
                   str_sub(1,-5)),
                function(x){
                  read_csv(paste0('data/gen_data/ipyrad/assemblies_final_stats/',x,'.csv')) %>%
                    select(sample_name,reads_consens,loci_in_assembly) %>%
                    mutate(assembly = x)}) %>%
  bind_rows()
require(scales)
ggplot(stats,aes(x=sample_name,y=loci_in_assembly,
                 fill=assembly,group=assembly))+
  geom_bar(stat='identity',
           position=position_dodge())+
  scale_y_continuous(labels = label_comma())+
  labs(x='Sample',y='N Loci')+
  theme_minimal()+
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## P. latrodecti RADSeq####

### Barcode file####
require(tidyverse)
plate_bc <- read_csv('data/plat_radseq/plate_barcode.csv')

plat_rad <- read_csv('data/plat_radseq/plat_radseq.csv') %>% 
  rename(sample_name = 'Specimen ID',
         plate = 'Plate number',
         pos = 'Plate position') %>%
  select(sample_name,plate,pos) %>% 
  mutate(sample_name = str_replace_all(sample_name,' ','-'),
         plate = as.numeric(str_replace(plate,'plate ',''))) %>%
  left_join(plate_bc,
            by = "pos")

# Plate 1
plat_rad %>% filter(plate==1) %>%
  select(sample_name,bc) %>%
  write_delim('data/plat_radseq/plat_bc_plate1.txt',delim = ' ',col_names = F)
# Plate 2
plat_rad %>% filter(plate==2) %>%
  select(sample_name,bc) %>%
  write_delim('data/plat_radseq/plat_bc_plate2.txt',delim = ' ',col_names = F)

### Samples info####
plat_samples <- mastersheet %>%
  filter(binomial %in% c('Philolema latrodecti','Philolema palanichamyi') & radseq == 'yes') %>%
  mutate(region1 = case_when(region1 == 'Spain' ~ 'Outgroup',
                             .default = region1),
         region2 = case_when(region2 == 'Spain' ~ 'Outgroup',
                             .default = region2)) %>%
  mutate(region2 = fct_relevel(region2,'Israel','Tahiti','USA','Outgroup'))
require(rnaturalearthdata)
require(terra)
pop_colors = c(RColorBrewer::brewer.pal(name='Paired',n=12),
               RColorBrewer::brewer.pal(name='Accent',n=6))

# Emulating ggplot default palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# World map with sampling
plat_sf <- st_as_sf(plat_samples %>% filter(region2 != 'Outgroup') %>% 
                      add_count(region2) %>% distinct(region2,.keep_all = T),
                    coords = c('longitude','latitude'),crs=4326)
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_sf(data = plat_sf,
          aes(fill=region2),size=3.5,pch=21)+
  scale_fill_manual(values = gg_color_hue(4))+
  geom_sf_label(data = plat_sf,
                aes(label=n),size=3,
                vjust = -0.5,
                fun.geometry = st_centroid,
                colour = "black")+
  labs(x = 'Longitude',y='Latitude')+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = 'none')

# Israel map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = plat_samples %>% filter(region2 == 'Israel'),
             aes(x=longitude,y=latitude),fill=gg_color_hue(4)[1],size=5,pch=21)+
  #scale_fill_manual(values = gg_color_hue(3)[1])+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(33.5,36.5),ylim = c(29.5,32.5))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"))

# USA map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = plat_samples %>% filter(region2 == 'USA'),
             aes(x=longitude,y=latitude),fill=gg_color_hue(4)[2],size=5,pch=21)+
  #scale_fill_manual(values = pop_colors)+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(-90,-75),ylim = c(23,35))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"))

# Tahiti map
ggplot()+
  geom_sf(data = countries50, linewidth = 0.1)+
  geom_point(data = plat_samples %>% filter(region2 == 'Tahiti'),
             aes(x=longitude,y=latitude),fill=gg_color_hue(4)[3],size=5,pch=21)+
  #scale_fill_manual(values = pop_colors)+
  labs(x = 'Longitude',y='Latitude')+
  coord_sf(xlim = c(-150,-149),ylim = c(-18,-17.2))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.9), linetype = "dashed", size = 0),
        panel.background = element_rect(fill = "aliceblue"))

### Checking sample stats####
#### Proportion of mapped #####
stats <- read_table('data/gen_data/ipyrad/plat_all_stats.tsv') %>%
  mutate(refmapped_prop = refseq_mapped_reads/reads_passed_filter)

require(scales)
ggplot(stats,aes(x=sample,y=refmapped_prop))+
  geom_bar(stat='identity',
           position=position_dodge())+
  scale_y_continuous(labels = label_comma())+
  labs(x='Sample',y='Proportion of reads mapped to reference')+
  geom_hline(yintercept = 0.7,color='red')+
  geom_hline(yintercept = 0.9,color='blue')+
  theme_minimal()+
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(paste0('output/radseq/plat_cov_stats.png'),width = 15,height = 8,dpi = 1200)

#### Prop mapped to WGS data #####
names <- c('arizona','hawaii','israel','luzon')
stats <- lapply(names,
                function(x){
                  read_table(paste0('data/gen_data/ipyrad/plat_stats/refstats_',
                                    x,'WGS.tsv')) %>%
                    mutate(refmapped_prop = refseq_mapped_reads/reads_passed_filter)
                })
names(stats) <- names

require(scales)
lapply(names,
       function(x){
         ggplot(stats[[x]],aes(x=sample,y=refmapped_prop))+
           geom_bar(stat='identity',
                    position=position_dodge())+
           scale_y_continuous(labels = label_comma())+
           labs(x='Sample',y='Proportion of reads mapped to reference')+
           geom_hline(yintercept = 0.7,color='red')+
           geom_hline(yintercept = 0.9,color='blue')+
           theme_minimal()+
           theme(legend.position = 'top',
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
         ggsave(paste0('output/radseq/plat_cov_',x,'WGS_stats.png'),width = 15,height = 8,dpi = 1200)
       })

### PCA####
require(vcfR)
require(SNPfiltR)
#### With outgroup####
vcf <- read.vcfR('data/vcfs/plat_goodsamples.vcf')

##### Adegenet PCA####

## PCA through adegenet on genind object
## https://adegenet.r-forge.r-project.org/files/PRstats/practical-MVAintro.1.0.pdf
require(adegenet)
pca_results <- vcfR2genind(vcf) %>% tab(freq=TRUE, NA.method="mean") %>%
  dudi.pca(center=TRUE, scale=FALSE, scannf = FALSE, nf = 10)
saveRDS(pca_results,'rds/plat_all_pca_adegenet.rds')

##### Plotting results####
pca_results <- readRDS('rds/plat_all_pca_adegenet.rds')
require(ggrepel)
pcadata <- pca_results$li %>% mutate(code = rownames(pca_results$li)) %>%
  as_tibble() %>% left_join(plat_samples,by='code') %>%
  mutate(region2 = fct_relevel(region2,'Israel','Tahiti','USA','Outgroup'))

ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=code))+
  geom_point(size=4,alpha=1,pch=21)+
  geom_text_repel(max.overlaps = 15)+
  scale_fill_manual(name = 'Region',values = gg_color_hue(4))+
  xlab('PC1')+ylab('PC2')+
  theme_classic()+
  ggtitle(paste0('PCA - Philolema latrodecti (in + outgroup)'),
          subtitle = paste0('CT = 0.85; minN = 50%\nN Samples = 118; N Loci = 19038; N SNPs = 250600'))
ggsave(paste0('output/pca_results/plat_all.png'),width = 10,height = 8,dpi = 1200)

# Focused on Tahiti and US cluster
ggplot(pcadata,aes(x=Axis1,Axis2,fill=region2,label=code))+
  geom_point(size=13,alpha=1,pch=21)+
  #geom_text_repel(max.overlaps = 28)+
  # Grey for outgroup
  scale_fill_manual(name = 'Region',values = gg_color_hue(4))+
  xlab('PC1')+ylab('PC2')+
  theme_void()+
  theme(legend.position = 'none',
        panel.border = element_rect(color = "black", fill = NA, linewidth = 2), # Adds the box border
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.tick = element_blank())+
  xlim(-217,-195)+ylim(-50,-25)
ggsave(paste0('output/pca_results/pca_plat_US_Tahiti.png'),width = 8,height = 8,dpi = 1200)

#### Ingroup only####
vcf <- read.vcfR('data/vcfs/plat_ingroup.vcf')

##### Adegenet PCA####

## PCA through adegenet on genind object
## https://adegenet.r-forge.r-project.org/files/PRstats/practical-MVAintro.1.0.pdf
require(adegenet)
pca_results <- vcfR2genind(vcf) %>% tab(freq=TRUE, NA.method="mean") %>%
  dudi.pca(center=TRUE, scale=FALSE, scannf = FALSE, nf = 10)
saveRDS(pca_results,'rds/plat_ingroup_pca_adegenet.rds')

##### Plotting results####
pca_results <- readRDS('rds/plat_ingroup_pca_adegenet.rds')
require(ggrepel)
pcadata <- pca_results$li %>% mutate(code = rownames(pca_results$li)) %>%
  as_tibble() %>% left_join(plat_samples,by='code')

ggplot(pcadata,aes(x=Axis1,Axis2,fill=locality,label=code))+
  geom_point(size=4,alpha=1,pch=21)+
  #geom_text_repel(max.overlaps = 15)+
  scale_fill_manual(name = 'Region',values = pop_colors)+
  xlab('PC1')+ylab('PC2')+
  theme_classic()+
  #theme(legend.position = 'none')+
  ggtitle(paste0('PCA - Philolema latrodecti (ingroup only)'),
          subtitle = paste0('CT = 0.85; minN = 50%\nN Samples = 108; N Loci = 19149; N SNPs = 193349'))
ggsave(paste0('output/pca_results/plat_ingroup.png'),width = 10,height = 8,dpi = 1200)

## Plotting over host
# Formatting species name
pcadata %>% mutate(spider_host = str_replace_all(spider_host,'_',' ')) %>%
  ggplot(aes(x=Axis1,Axis2,fill=spider_host,label=code))+
  geom_point(size=4,alpha=1,pch=21)+
  scale_fill_manual(name = 'Host spider species',values = pop_colors)+
  xlab('PC1')+ylab('PC2')+
  theme_classic()+
  theme(legend.text = element_text(face='italic'))
ggtitle(paste0('PCA - Philolema latrodecti (ingroup only)'),
        subtitle = paste0('Colored by host spider species'))
ggsave(paste0('output/pca_results/plat_ingroup_host_species.png'),width = 10,height = 8,dpi = 1200)

### Relatedness####
a=read_delim('data/vcfs/plat_ingroup.relatedness') %>% 
  rename(ajk = 'RELATEDNESS_AJK')
#left_join(plat_samples %>% select(code,region2),by=c(INDV1 = 'code')) %>%
#left_join(plat_samples %>% select(code,region2),by=c(INDV2 = 'code')) %>%
#rename(c(INDV1_region = 'region2.x',INDV2_region = 'region2.y')) %>%
#mutate(INDV1 = paste0(INDV1_region,' - ',INDV1),
#       INDV2 = paste0(INDV2_region,' - ',INDV2)) %>% 
#mutate(INDV2 = fct_rev(INDV2)) %>% 
ggplot(data=a,aes(x=INDV1,y=INDV2,fill=ajk))+geom_tile(color=NA)+
  scale_fill_gradient2(name = 'PHI',
                       low = '#2b83ba', mid = '#fffebd', high = '#d7191c')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave(paste0('output/relatedness.png'),width = 17,height = 15,dpi = 1200)

## Pca map ####

### Lgeo####
# Adding tree clustering info
lgeo_tree <- mastersheet %>% filter(binomial == 'Latrodectus geometricus') %>%
  distinct(region1,.keep_all = T) %>%
  mutate(tree_group = case_when(region1 == 'Israel' ~ 'Israel',
                                region1 %in% c('Texas','California',
                                               'Central Florida','Southeastern US') ~ 'United States',
                                region1 %in% c('SW South Africa','Southern South Africa') ~ 'Southern South Africa',
                                region1 %in% c('NE South Africa','Central South Africa') ~ 'Northern South Africa',
                                .default = 'Not sequenced')) %>%
  mutate(point_size = case_when(tree_group == 'Not sequenced' ~ 2,
                                .default = 3),
         point_alpha = case_when(tree_group == 'Not sequenced' ~ 0.5,
                                 .default = 0.9)) %>% 
  select(region1,region2,latitude,longitude,tree_group,point_size,point_alpha)

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = lgeo_tree,
             aes(x = longitude,y = latitude,fill = tree_group,
                 size = point_size, alpha = point_alpha),
             #size = 2,alpha=0.8,
             pch=21)+
  scale_size(range=c(2,3))+
  scale_alpha(range=c(0.4,0.9))+
  scale_fill_manual(values = c('chocolate2','chartreuse3','brown','cornflowerblue','purple'))+
  coord_sf(xlim = c(-149,147.351841),ylim = c(-50.918958,60.247206))+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none')
ggsave('output/sample_maps/lgeo_all_treecolored.png',width = 6, height = 3,dpi = 600)

### Plat####
# Adding tree clustering info
plat_tree <- mastersheet %>% filter(binomial == 'Philolema latrodecti') %>%
  filter(wgs == 'no') %>% 
  distinct(region1,.keep_all = T) %>%
  mutate(tree_group = case_when(region1 == 'Israel' ~ 'Israel',
                                region1 %in% c('Central Florida','Southeastern US') ~ 'United States',
                                region1 == 'Tahiti' ~ 'Tahiti',
                                .default = 'Not sequenced')) %>%
  mutate(point_size = case_when(tree_group == 'Not sequenced' ~ 2,
                                .default = 3),
         point_alpha = case_when(tree_group == 'Not sequenced' ~ 0.5,
                                 .default = 0.9)) %>% 
  select(region1,region2,latitude,longitude,tree_group,point_size,point_alpha)

ggplot(data = world) +
  geom_sf(fill= "ghostwhite", size = 0.1)+
  geom_point(data = plat_tree,
             aes(x = longitude,y = latitude,fill = tree_group,
                 size = point_size, alpha = point_alpha),
             #size = 2,alpha=0.8,
             pch=21)+
  scale_size(range=c(2,3))+
  scale_alpha(range=c(0.4,0.9))+
  scale_fill_manual(values = c('darkorange','brown','bisque2','purple'))+
  coord_sf(xlim = c(-149,147.351841),ylim = c(-50.918958,60.247206))+
  theme(panel.grid.major = element_line(color = gray(.9),linetype = "dashed", linewidth = 0),
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none')
ggsave('output/sample_maps/plat_all_treecolored.png',width = 6, height = 3,dpi = 600)

## Pop stats####
require(vcfR)
needs::prioritize(dplyr)

lgeo_vcf <- read.vcfR('data/gen_data/vcf_files/lgeometricus_ref_woLowCov.vcf')

pops <- read_csv('data/sample_data/york_master_samplesheet.csv') %>%
  filter(genus == 'Latrodectus' & radseq == 'yes') %>%
  select(code,region2) %>% 
  mutate(pop = case_when(region2 %in% c('Southern US','West Coast US','Florida','Southeastern US') ~ 'United States',
                         .default = region2)) %>% 
  left_join(x = colnames(lgeo_vcf@gt)[-1] %>% as_tibble() %>% rename(code = 'value')) %>%
  select(pop) %>% unlist() %>% as.character() %>% as.factor()

# Local Heterozigosity
hs <- genetic_diff(lgeo_vcf, pops = pops, method = 'nei') %>% as_tibble()
write_csv(hs,'output/genetic/hs.csv')
# Pairwise differentiationm
pairdiff <- pairwise_genetic_diff(lgeo_vcf, pops = pops, method = 'nei') %>% as_tibble()

pairs <- pairdiff %>% as_tibble() %>% select(starts_with('Gst')) %>% colnames()

gst <- lapply(pairs,function(x){
  pop1 <- str_split_i(x,'_',2)
  pop2 <- str_split_i(x,'_',3)
  gst_vec <- pairdiff %>% select(all_of(x)) %>% drop_na() %>% unlist() %>% as.numeric()
  mean_gst <- mean(gst_vec)
  median_gst <- median(gst_vec)
  sd_gst <- sd(gst_vec)
  return(data.frame(pop1,pop2,mean_gst,median_gst,sd_gst))
}) %>% bind_rows()

write_csv(gst,'output/genetic/gst.csv')
gst %>% arrange(pop2) %>% pivot_wider(id_cols = c('pop1'),names_from = pop2,values_from = mean_gst)


## I can also calculate Fst from the formula Fst = (Ht - Hs)/Ht, getting that info from vcfR::genetic_diff


gi <- vcfR2genlight(lgeo_vcf)
gl <- vcfR2genlight(lgeo_vcf)

# Getting populatikon
pop(gi) <- 
  
gisep <- seppop(gi)
################################################################################
all <- genind2hierfstat(gi,pop=pops)
all.stats <- basic.stats(all)

egregius <- genind2hierfstat(GIsep$egregius)
insularis <- genind2hierfstat(GIsep$insularis,pop=c(rep(1,12)))
lividus <- genind2hierfstat(GIsep$lividus,pop=c(rep(1,12)))
onocrepis <- genind2hierfstat(GIsep$onocrepis,pop=c(rep(1,16)))
similis <- genind2hierfstat(GIsep$similis,pop=c(rep(1,14)))

eg.stats <- basic.stats(egregius)
in.stats <- basic.stats(insularis)
lv.stats <- basic.stats(lividus)
on.stats <- basic.stats(onocrepis)
sm.stats <- basic.stats(similis)

write.csv(eg.stats$overall,"eg.stats.csv")
write.csv(in.stats$overall,"in.stats.csv")
write.csv(lv.stats$overall,"lv.stats.csv")
write.csv(on.stats$overall,"on.stats.csv")
write.csv(sm.stats$overall,"sm.stats.csv")

fst <- boot.ppfst(all.ordered)
################################################################################
#####   FOR THE ISLANDS     #####

island <- readLines("StructureIslands.csv")
island <- as.numeric(as.factor(island))
pop(GI) <- island

GIsep <- seppop(GI)

BPKEast <- genind2hierfstat(GIsep$'4', pop=c(rep(1,10)))
BPKWest <- genind2hierfstat(GIsep$'5', pop=c(rep(1,5)))
BHK <- genind2hierfstat(GIsep$'1', pop=c(rep(1,3)))
scale <- genind2hierfstat(GIsep$'10', pop=c(rep(1,3)))
seahorse <- genind2hierfstat(GIsep$'11', pop=c(rep(1,3)))
north <- genind2hierfstat(GIsep$'8', pop=c(rep(1,6)))


BPKEast.stats <- basic.stats(BPKEast)
BPKWest.stats <- basic.stats(BPKWest)
BHK.stats <- basic.stats(BHK)
scale.stats <- basic.stats(scale)
seahorse.stats <- basic.stats(seahorse)
north.stats <- basic.stats(north)


write.csv(BPKEast.stats$overall,"BPKEast.stats.csv")
write.csv(BPKWest.stats$overall,"BPKWest.stats.csv")
write.csv(BHK.stats$overall,"BHK.stats.csv")
write.csv(scale.stats$overall,"scale.stats.csv")
write.csv(seahorse.stats$overall,"seahorse.stats.csv")
write.csv(north.stats$overall,"north.stats.csv")

fst <- boot.ppfst(all.ordered)




## SNMF####
# VCFtools was previously run on the original vcf files to thin variants by
# 10k bp windows. We exported the thinned VCF as a genotype matrix.
# Files utilized were:
# `lgeometricus_ref_woLowCov` for L. geometricus
# `plat_ingroup.vcf` for P. latrodecti

### Formatting input####
spp = c('Latrodectus geometricus','Philolema latrodecti')
spp.acr = c('lgeo','plat')
## Individuals info
indnames <- lapply(list.files('data/vcfs/',full.names = T, pattern = '_unlinked.012.indv'),
                   function(x){return(read.table(x) %>% unlist() %>% as.character())})

### Reading vcftools matrices
matrixfiles <- list.files('data/vcfs',full.names = T, pattern = '_unlinked.012$')
mat <- lapply(matrixfiles,read.table) %>%
  # Transpose
  lapply(t) %>% lapply(as_tibble) %>%
  lapply(slice,-1) %>% lapply(mutate_if,is.numeric,
                              ~ case_when(
                                . == 0 ~ 0,
                                . == 1 ~ 1,
                                . == 2 ~ 2,
                                . == -1 ~ 9))

## Save geno file for SNMF
# Saving geno files
lapply(1:length(mat),function(i){
  write.table(mat[[i]] %>% as.matrix(), 
              paste0('data/geno/',spp.acr[i],'_unlinked.geno'),
              col.names = FALSE,
              row.names = FALSE,
              sep = "")
})

### Running####
genofiles <- list.files('data/geno/',recursive = T,pattern = '_unlinked.geno$',full.names = T)

require(LEA)
require(parallel)
snmf_run <- mclapply(1:length(spp),function(i){
  res <- snmf(input.file = genofiles[i],
              K = 1:10,
              entropy=TRUE,
              repetitions=5,
              project='new',
              alpha=100)
  return(res)
},mc.cores = 35)
saveRDS(snmf_run,'rds/snmf_run.rds')
### Investigating output####
# Plotting cross-entropy values
lapply(1:length(spp),function(i) {
  png(paste0('output/snmf/bestK_plots/',spp.acr[i],'_bestK.png'))
  plot(snmf_run[[i]], cex = 1.2, col = "lightblue", pch = 19)
  dev.off()
})

# Formatting for barplot
snmf_tab <- lapply(1:length(spp),function(i) {
  tab <- lapply(2:4,function(k) {
    best_run <- which.min(cross.entropy(snmf_run[[i]], K = k))
    q_mat <- LEA::Q(snmf_run[[i]], K = k, run = best_run)
    colnames(q_mat) <- paste0("P", 1:ncol(q_mat))
    
    # Making sure the meta table follows the right individual order in genofiles
    tab <- indnames[[i]] %>% as_tibble() %>%
      rename(code = 'value') %>% left_join(mastersheet %>% select(code,locality,region1,region2,),by='code') %>%
      add_column(q_mat %>% as_tibble())
    return(tab)
  })
  names(tab) <- paste0('K',2:4)
  return(tab)
})
names(snmf_tab) <- spp.acr
saveRDS(snmf_tab,'rds/snmf_tab.rds')

### Barplot####
lapply(1:length(snmf_tab),function(i) {
  lapply(1:3,function(k) {
    plotdata <- snmf_tab[[i]][[k]] %>%
      #mutate(ind_loc = paste0(code,'_',rad_pop)) %>% 
      pivot_longer(cols = starts_with("P"), names_to = "pop", values_to = "q") %>% 
      # assign the population assignment according to the max q value (ancestry proportion)
      # and include the assignment probability of the population assignment
      group_by(code) %>%
      mutate(likely_assignment = pop[which.max(q)],
             assignment_prob = max(q)) %>%
      # arrange the data set by the ancestry coefficients
      arrange(likely_assignment, assignment_prob) %>% 
      # this ensures that the factor levels for the individuals follow the ordering we just did. This is necessary for plotting
      ungroup() %>% 
      mutate(code = forcats::fct_inorder(factor(code)))
    
    # With ind labels
    ggplot(plotdata) +
      geom_col(aes(x = code, y = q, fill = pop)) +
      labs(fill = "Population")+
      scale_fill_manual(values = gg_color_hue(4))+
      #scale_fill_manual(values = colors[1:length(grep('^P',colnames(snmf_tab[[i]][[k]])))])+
      theme_minimal() +
      # some formatting details to make it pretty
      theme(panel.spacing.x = unit(0, "lines"),
            axis.line = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5,hjust=1,size=9),
            strip.background = element_rect(fill = "transparent", color = "black"),
            panel.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            legend.position = 'none')
    ggsave(paste0('output/snmf/barplots/',spp.acr[i],'_K',k+1,'_indnames.png'),
           width = 12,height = 7)
    
    # Without ind labels
    ggplot(plotdata) +
      geom_col(aes(x = code, y = q, fill = pop)) +
      labs(fill = "Population") +
      scale_fill_manual(values = gg_color_hue(4))+
      #scale_fill_manual(values = colors[1:length(grep('^P',colnames(res_snmf[[i]])))])+
      theme_minimal() +
      # some formatting details to make it pretty
      theme(panel.spacing.x = unit(0, "lines"),
            axis.line = element_blank(),
            axis.text = element_blank(),
            strip.background = element_rect(fill = "transparent", color = "black"),
            panel.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            legend.position = 'none')
    ggsave(paste0('output/snmf/barplots/',spp.acr[i],'_K',k+1,'.png'),
           width = 12,height = 7)
  })
})

## RADSEQ TREES ####
### RAxML ####
# Trees were run in Unity
require(ggtree)
library(tidyverse)
library(treeio)
library(ggtree)

setwd('Documents/research/york_postdoc/')

samples <- read_csv('1_samples/york_master_samplesheet.csv') %>%
  filter(wgs == 'yes')

tree <- read.tree('5_wgs/p_latrodecti/plat_wgs_filtered/plat_wgs_filtered.min4.phy.varsites.phy.contree')
# Changing sample code to original code (simpler to visualize)
tree$tip.label <- sapply(tree$tip.label,function(x){
  return(samples$original_code[which(samples$code==gsub('.rmd.bam','',x))])}
)

# Rooting on philolema arnoldi
tree <- ape::root.phylo(tree,outgroup = '1215.1')

# Plotting to check rooting
ggtree(tree, branch.length="none")+
  geom_tiplab(as_ylab = T, color="purple")+
  geom_nodelab(nudge_x = 0.3)

# Plotting localities
ggtree(tree, branch.length = 'none') %<+% (samples %>% relocate(original_code))+
  geom_tiplab(aes(color=spider_host,
                  label=paste0(binomial,' - ',region1,' - ',date_collected)),
              align=TRUE, linesize=.5)+
  geom_nodelab(nudge_x = 0.3)+
  xlim(0, 15)

ggtree(tree, branch.length="none")+
  geom_tiplab(as_ylab = T, color="purple")+
  geom_nodelab(aes(label = node), hjust = -0.3)

geom_cladelab(node=34, label="another clade", align=TRUE, 
              offset = .2, textcolor='blue', barcolor='blue')


## StairwayPlot ####

### Creating SFS ####

# Create popfile
# For L. geometricus, use S_SAfrica, N_SAfrica, USA, Israel
# For P. latrodecti, use USA, Tahiti and the two pops in Israel
# First, let's read the vcf files to check on individuals
require(vcfR)
lgeo_vcf <- read.vcfR('data/gen_data/vcf_files/lgeometricus_ref_woLowCov.vcf')
plat_vcf <- read.vcfR('data/gen_data/vcf_files/plat_ingroup.vcf')

radsamples <- c(colnames(lgeo_vcf@gt)[-1],colnames(plat_vcf@gt)[-1]) %>% as_tibble() %>%
  rename(code = 'value') %>% 
  left_join(read_csv('data/sample_data/york_master_samplesheet.csv') %>%
              select(code,binomial,locality,region1,region2),
            by = 'code')

library(ape)
require(ggtree)
tree <- read.tree('data/gen_data/radseq_raxml_trees/RAxML_bestTree.lelegans')
tree$
tree <- ape::root.phylo(tree,outgroup = 'Htz1')
ggtree(tree, branch.length="none")+
  geom_tiplab(as_ylab = T, color="purple")+
  geom_nodelab(hjust = -0.3)

#L. geometricus
lgeo_popfile <- radsamples %>% filter(binomial == 'Latrodectus geometricus') %>% 
  mutate(pop = case_when(region2 == 'Israel' ~ 'israel',
                         region2 %in% c('West Coast US','Florida','Southeastern US','Southern US') ~ 'usa',
                         region2 %in% c('NE South Africa','Central South Africa') ~ 'n_safrica',
                         region2 %in% c('SW South Africa','Southern South Africa') ~ 's_safrica')) %>%
  select(code,pop)
write_tsv(lgeo_popfile,paste0('data/gen_data/popfiles/lgeometricus_popfile.txt'),col_names = F)

# Two groups of P. latrodecti are based on RAxML tree
library(ape)
require(ggtree)
tree <- read.tree('data/gen_data/radseq_raxml_trees/RAxML_bipartitions.platrodecti')
tree <- ape::root.phylo(tree,outgroup = 'Ppalan_1')
ggtree(tree, branch.length="none")+
  geom_tiplab(as_ylab = T, color="purple")+
  geom_nodelab(aes(label = node),hjust = -0.3)
# 125 - israel1
# 175 - israel2
# 160 - tahiti and one florida
# 137 - florida
plat_pops <- data.frame(node = c(125,175,160,137),
                        pop = c('israel1','israel2','tahiti','florida'))

tips <- lapply(plat_pops$node,function(node){
  subtree <- extract.clade(phy = tree, node = node)
  return(subtree$tip.label)
})
  
#P. latrodecti
plat_popfile <- radsamples %>% filter(binomial == 'Philolema latrodecti') %>% select(code) %>% 
  mutate(pop = case_when(code %in% tips[[1]] ~ 'israel1',
                         code %in% tips[[2]] ~ 'israel2',
                         code %in% tips[[3]] ~ 'tahiti',
                         code %in% tips[[4]] ~ 'florida')) %>%
  select(code,pop)
write_tsv(plat_popfile,paste0('data/gen_data/popfiles/platrodecti_popfile.txt'),col_names = F)


# Preview projection values
# Projecting L. geometricus with -y -a and length 2012047
# Criteria: basically, getting the value above half sample size that is the end of that up and down fluctuations
# around the peak of segregating sites
# Israel: (33, 23766) - N = 66
# usa: (18, 48321) - N = 30
# s_safrica: (38, 38290) - N = 60
# n_safrica: (36, 68403) - N = 56

# Projecting L. geometricus with -y -a and length 4222626
# Here I used the peak of segregating sites, which was pretty close to sample size,
# always above half sample size
# israel1: (34, 23409) - N = 42
# florida: (36, 19235) - N = 48
# tahiti: (26, 9537) - N = 32
# israel2: (76, 38359) - N = 94

### Stairway input####
template <- read_lines('data/gen_data/stairwayplot/stairwayplot_template') %>% as_tibble() %>%
  mutate(param = str_split_i(value,':',1)) %>% select(param) %>% filter(!(str_detect(param,'#')))

# Creating SFS data
sfs_data <- bind_rows(lgeo_popfile %>% mutate(binomial = 'latrodectus_geometricus'),
                      plat_popfile %>% mutate(binomial = 'philolema_latrodecti')) %>%
  distinct(binomial,pop) %>%
    ## Adding proj and nseq
  mutate(proj = c(33,18,38,36,34,36,26,76)) %>% 
  mutate(nseq = as.character(proj*2))

# Reading
sfs <- lapply(paste0('data/gen_data/sfs_files/',sfs_data$binomial,'_sfs/dadi/',sfs_data$pop,
                     '-',as.character(sfs_data$proj),'.sfs'),function(x){
                       return(read_delim(x,skip=1,col_names = F) %>%
                                slice_head(n=1) %>%
                                select(-1) %>% # Remove zero bin
                                unlist() %>%
                                as.character() %>% paste(collapse = ' '))}) %>% do.call(what = rbind.data.frame) %>%
  unlist()

sfs_data <- sfs_data %>% add_column(SFS=sfs) %>% select(-proj)

# Plotting SFSs, just for fun
a=sfs_data %>% mutate(sfs_numeric = str_split(sfs_data$SFS,' '))

a$sfs_numeric[1] %>% unlist() %>% as.numeric %>% as_tibble()
  
ggplot(aes(x=value))+geom_histogram()

# Seq length from phy file and generation time from literature/pers. comm
# L. geometricus generation time: twice a year (years per generation = 0.5)
# P. latrodecti generation time: four times a year (twice L. geometricus; years per generation = 0.25)

seqlength_gt <- data.frame(binomial = c('latrodectus_geometricus','philolema_latrodecti'),
                        L = c('2012047','4222626'),
                        year_per_generation = c('0.5','0.25'))

# Adding additional data
stairway_data <- sfs_data %>% 
  # Renaming pop to popid, to match what needs to be in the stairwayplot input file
  rename(popid = 'pop') %>%
  # Replacing "_" in n_africa and s_africa to facilitate managing files later on
  mutate(popid = str_replace_all(popid,'_','-')) %>% 
  ## Adding species-specific info: L and gt
  left_join(seqlength_gt,by='binomial') %>%
  ## Calculating nsequences with (proj + 1)*2
  mutate(whether_folded = 'true',
         pct_training = '0.67',
         nrand = paste0(as.integer((as.numeric(nseq)-2)/4),' ',as.integer((as.numeric(nseq)-2)/2),' ',
                        as.integer((as.numeric(nseq)-2)*3/4),' ',as.integer((as.numeric(nseq)-2))),
         project_dir = paste0('/home/rilquer/lgeo_plat/output/stairwayplot/',
                              binomial,'_',popid,'_mu6e-9/'),
         stairway_plot_dir = '/media/sda/rilquer/af-demographic-syndromes/project/bin/stairway_plot_v2.1.1/stairway_plot_es/',
         ninput = '200',
         mu = '6e-9',
         plot_title = paste0(binomial,'_',popid,'_mu6e-9_gt',year_per_generation),
         xrange = '0.1,10000',
         yrange = '0,0',
         xspacing = '2',
         yspacing = '2',
         fontsize = '12')

# Writing file
for (i in 1:nrow(stairway_data)) {
  values <- stairway_data %>% slice(i) %>% pivot_longer(everything(),values_to = 'value',names_to = 'param')
  template %>% left_join(values,by = 'param') %>% mutate(blueprint = paste0(param,": ",value)) %>%
    dplyr::select(blueprint) %>% 
    write_delim(paste0('data/gen_data/stairwayplot/',gsub('\\.','-',stairway_data$plot_title[i]),'.blueprint'),
                col_names = F,quote = 'none')
}

### Stairway results####
require(tidyverse)
# getting pop names
pops <- list.files(path = 'output/stairwayplot/',
                   pattern = '.final.summary$',recursive = T) %>%
  str_split_i('/',1)

# Reading results
stair <- lapply(list.files(path = 'output/stairwayplot/',
                           pattern = '.final.summary$',full.names = T, recursive = T),
                read_tsv)

# adding species name
stair <- lapply(1:length(stair),function(i){
  return(stair[[i]] %>%
           mutate(pop = pops[i]))
}) %>% bind_rows()
saveRDS(stair,'rdata/gen_analyses/stairwayplot.rds')

# Colors for plotting
colors <- c('#e66101','#8c510a','#0570b0','#f768a1',
            '#f768a1','#e66101','#e66101','#238b45')

# Plotting all pops in one
require(ggplot2)
require(ggtext)
stair_plot <- stair %>% select(pop,year,Ne_median,`Ne_2.5%`,`Ne_97.5%`) %>% 
  rename(lower_95 = 'Ne_2.5%',upper_95='Ne_97.5%') %>% 
  mutate(year = year/1000, Ne_median = Ne_median/1000,
         lower_95 = lower_95/1000, upper_95=upper_95/1000) %>%
  filter(year > 0.5)

ggplot(data=stair_plot %>% slice(grep('latrodectus',stair_plot$pop)),aes(x=year,y=Ne_median))+
  geom_line(aes(group=pop,color=pop),linewidth=1,linetype=1,alpha=0.8)+
  #geom_ribbon(aes(ymin=lower_95, ymax=upper_95,group=pop,fill=pop),linetype=2,alpha=0.2)+
  scale_x_log10(breaks = c(1,5,8,10,13,15,1825,50,100,200,400))+
  scale_y_log10(breaks = c(0,50,100,300,500,800,1000,1500,2500,5000,10000))+
  scale_color_manual(name = 'Population',
                     labels = c('<p><i>Latrodectus geometricus</i></p><p>Israel</p>',
                                '<p><i>Latrodectus geometricus</i></p><p>Northern South Africa</p>',
                                '<p><i>Latrodectus geometricus</i></p><p>Southern South Africa</p>',
                                '<p><i>Latrodectus geometricus</i></p><p>USA</p>',
                                '<p><i>Philolema latrodecti</i></p><p>USA</p>',
                                '<p><i>Philolema latrodecti</i></p><p>Israel 1</p>',
                                '<p><i>Philolema latrodecti</i></p><p>Israel 2</p>',
                                '<p><i>Philolema latrodecti</i></p><p>Tahiti</p>'),
                     values = colors)+
  scale_fill_manual(name = 'Population',
                    labels = c('<p><i>Latrodectus geometricus</i></p><p>Israel</p>',
                               '<p><i>Latrodectus geometricus</i></p><p>Northern South Africa</p>',
                               '<p><i>Latrodectus geometricus</i></p><p>Southern South Africa</p>',
                               '<p><i>Latrodectus geometricus</i></p><p>USA</p>',
                               '<p><i>Philolema latrodecti</i></p><p>USA</p>',
                               '<p><i>Philolema latrodecti</i></p><p>Israel 1</p>',
                               '<p><i>Philolema latrodecti</i></p><p>Israel 2</p>',
                               '<p><i>Philolema latrodecti</i></p><p>Tahiti</p>'),
                    values = colors)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size=18,vjust = 3),
        axis.title.x = element_text(size=18,vjust = -2),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches"),
        strip.text = element_text(size=13),
        strip.background = element_blank(),
        legend.justification = "top",
        legend.text = element_markdown(size=11),
        legend.title = element_text(size=12),
        legend.spacing.y = unit(1.5, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE),
         color = guide_legend(byrow = TRUE))+
  xlab('Years before present (kya)')+
  ylab('Effective population size')
ggsave('output/stairwayplot/plots/latrodectus_stairwayplot.png',width = 13,height = 8)

ggplot(data=stair_plot %>% slice(grep('philolema',stair_plot$pop)),aes(x=year,y=Ne_median))+
  geom_line(aes(group=pop,color=pop),linewidth=1,linetype=1,alpha=0.8)+
  #geom_ribbon(aes(ymin=lower_95, ymax=upper_95,group=pop,fill=pop),linetype=2,alpha=0.2)+
  scale_x_log10(breaks = c(1,5,8,10,13,15,1825,50,100,200,400),limit = c(0.5009547,342.3974))+
  scale_y_log10(breaks = c(0,50,100,300,500,800,1000,1500,2500,5000,10000))+
  scale_color_manual(name = 'Population',
                     labels = c('<p><i>Philolema latrodecti</i></p><p>USA</p>',
                                '<p><i>Philolema latrodecti</i></p><p>Israel 1</p>',
                                '<p><i>Philolema latrodecti</i></p><p>Israel 2</p>',
                                '<p><i>Philolema latrodecti</i></p><p>Tahiti</p>'),
                     values = colors[4:8])+
  scale_fill_manual(name = 'Population',
                    labels = c('<p><i>Philolema latrodecti</i></p><p>USA</p>',
                               '<p><i>Philolema latrodecti</i></p><p>Israel 1</p>',
                               '<p><i>Philolema latrodecti</i></p><p>Israel 2</p>',
                               '<p><i>Philolema latrodecti</i></p><p>Tahiti</p>'),
                    values = colors[4:8])+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size=18,vjust = 3),
        axis.title.x = element_text(size=18,vjust = -2),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), 
                           "inches"),
        strip.text = element_text(size=13),
        strip.background = element_blank(),
        legend.justification = "top",
        legend.text = element_markdown(size=11),
        legend.title = element_text(size=12),
        legend.spacing.y = unit(1.5, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE),
         color = guide_legend(byrow = TRUE))+
  xlab('Years before present (kya)')+
  ylab('Effective population size')
ggsave('output/stairwayplot/plots/philolema_stairwayplot.png',width = 13,height = 8)
