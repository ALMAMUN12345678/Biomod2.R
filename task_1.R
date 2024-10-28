getwd()
setwd('E:/AND')
df = read.csv('abundance.csv')
str(df)
# Step-1: Convert it into date fromat.  
df$Date<- as.Date(df$Date, format = "%m/%d/%Y")
number_of_ob= df %>% 
  dplyr::group_by(Longitude,Latitude) %>% 
  dplyr::summarise(n_ob = n()) %>% 
  ungroup()

# Observation Point
world <- map_data("world")
number_of_ob_sf <- st_as_sf(number_of_ob, coords = c("Longitude", "Latitude"), crs = 4326)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour = "darkgrey", fill = "grey", alpha = 1) +
  geom_sf(data = number_of_ob_sf, aes(color=log(n_ob)), size = 2) +  # Use geom_sf for sf object
  scale_color_viridis_c() +ggtitle("Total Number of Observations")+
  labs(x = "Longitude", y = "Latitude", colour = "Observations") +
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_line(color = "white", size = 0.8),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  coord_sf(expand = F)
  #theme(panel.grid = element_blank())+


#################### January ######################

Sp_1= df %>% 
  select(Longitude,Latitude,Corrected.name.entry,Date) %>% 
  filter(month(Date)==1)%>% 
  dplyr::group_by(Longitude,Latitude,Corrected.name.entry) %>% 
  dplyr::summarise() %>% 
  ungroup()

##### Species Richness ###############

sp_richness = Sp_1 %>% 
  dplyr::group_by(Longitude,Latitude) %>% 
  dplyr::summarise(number_of_sp = n()) %>% 
  ungroup()
  
  

world <- map_data("world")
number_of_ob_sf <- st_as_sf(sp_richness, coords = c("Longitude", "Latitude"), crs = 4326)
p1 = ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour = "darkgrey", fill = "grey", alpha = 1) +
  geom_sf(data = number_of_ob_sf, aes(color=number_of_sp), size = 3) +  # Use geom_sf for sf object
  scale_color_viridis_c(option = "H") +ggtitle("Species Richness in January")+
  labs(x = "Longitude", y = "Latitude", colour = "Species Richness") +
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_line(color = "white", size = 0.8),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  #theme(panel.grid = element_blank())+
  coord_sf(expand = F)



#################### July ######################

Sp_7= df %>% 
  select(Longitude,Latitude,Corrected.name.entry,Date) %>% 
  filter(month(Date)==7)%>% 
  dplyr::group_by(Longitude,Latitude,Corrected.name.entry) %>% 
  dplyr::summarise() %>% 
  ungroup()

##### Species Richness ###############

sp_richness_7 = Sp_7 %>% 
  dplyr::group_by(Longitude,Latitude) %>% 
  dplyr::summarise(number_of_sp = n()) %>% 
  ungroup()

world <- map_data("world")
number_of_ob_sf_7 <- st_as_sf(sp_richness_7, coords = c("Longitude", "Latitude"), crs = 4326)

p2 = ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour = "darkgrey", fill = "grey", alpha = 1) +
  geom_sf(data = number_of_ob_sf_7, aes(color=number_of_sp), size = 3) +  # Use geom_sf for sf object
  scale_color_viridis_c(option = "H") +ggtitle("Species Richness in July")+
  labs(x = "Longitude", y = "Latitude", colour = "Species Richness") +
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_line(color = "white", size = 0.8),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  #theme(panel.grid = element_blank())+
  coord_sf(expand = F)

######### For get legend ##################################

legend <- get_legend(
  ggplot(sp_richness_7, aes(Longitude, Latitude, color = number_of_sp)) + 
    geom_point() + scale_color_viridis_c(option = "H")+
    labs(x = "Longitude", y = "Latitude", colour = "Species Richness")+
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),          
      legend.title = element_text(size = 14, face = "bold"),
      legend.key.size = unit(3, "lines")
    )+
    theme(legend.position = "right")
)
plot(legend)

############## Pair Plot #################################
plot_grid(p1, p2,rel_widths = c(1, 1, 0.3))

grid.arrange(p1,p2)


## Most common species

 total_sp = df %>% 
  select(Corrected.name.entry) %>% 
  group_by(Corrected.name.entry) %>% 
  dplyr::summarise(n = n())


sp = df %>% 
  select(Longitude,Latitude,Corrected.name.entry) %>% 
  dplyr::group_by(Longitude,Latitude,Corrected.name.entry) %>% 
  dplyr::summarise(n_sp =n()) %>% 
  ungroup()


as.character(total_sp[252, 1])
p = sp %>% 
  filter(Corrected.name.entry=='Achnanthes longipes') %>% 
  summarise(x =n())

list_number = list()
list_sp_name  = list()
for (i in 1:nrow(total_sp)){
  list_number[i] = sp %>% 
    filter(Corrected.name.entry==as.character(total_sp[i, 1])) %>% 
    summarise(x =n())
  list_sp_name[i] = as.character(total_sp[i, 1])
  
}

species_most_obj = data.frame(
  species = unlist(list_sp_name),
  Count = unlist(list_number),
  stringsAsFactors = FALSE
)

common_sp = species_most_obj %>%
  arrange(desc(Count)) %>% 
  head(15)
ggplot(common_sp,aes(x=fct_inorder(species), y= Count ))+
  geom_col(fill='darkgreen')+
  labs(y="Total Number Of Observation",x='Top 15 Species')+theme_bw()+theme(legend.position = 'None')+
  theme(axis.text.x = element_text(angle =45,hjust =1,vjust=1,color = "black",size = 10,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 10,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))


########################### Abundance histogram ################ 

df %>%
  select(Corrected.name.entry,Abundance..cells.L.1.) %>% 
  filter(Corrected.name.entry =="Pennate spp.") %>% 
  ggplot(aes(x=Abundance..cells.L.1.))+
  geom_histogram(aes(x=log(Abundance..cells.L.1.)),bins = 50)+
  ggtitle('Histogram of Pennate spp.')+
  labs(y="Frequency",x='log(Abundance(cell/L))')+theme_bw()+theme(legend.position = 'None')+
  theme(axis.text.x = element_text(angle =0,hjust =1,vjust=1,color = "black",size = 10,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 10,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14),
        plot.title = element_text(hjust = 0.5))
  
  
  

### Shows in Map
median = df %>%
  select(Longitude, Latitude,Corrected.name.entry,Abundance..cells.L.1.) %>% 
  filter(Corrected.name.entry == "Pennate spp.") 

med_point = median %>% 
  filter(Abundance..cells.L.1.==median(Abundance..cells.L.1.))



world <- map_data("world")
num_med = st_as_sf(med_point,coords = c("Longitude", "Latitude"), crs = 4326)
number_of_ob_sf <- st_as_sf(median, coords = c("Longitude", "Latitude"), crs = 4326)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), colour = "darkgrey", fill = "grey", alpha = 1) +
  geom_sf(data = number_of_ob_sf, aes(color=log(Abundance..cells.L.1.)), size = 3,alpha=0.4) +
  geom_sf(data = num_med,aes(color=Abundance..cells.L.1.),size =6,color='red')+
  scale_color_viridis_c(option = "D") +ggtitle('Red color indicates the median abundance of Pennate spp.')+
  labs(x = "Longitude", y = "Latitude", colour = "Log(Abundance(cell/L))") +
  theme_bw()+theme(panel.background = element_rect(fill = "aliceblue"),
                   panel.grid = element_line(color = "white", size = 0.8),
                   #legend.position = "none",
                   plot.title = element_text(hjust = 0.5))+
  coord_sf(expand = F)+
  theme(axis.text.x = element_text(angle =0,hjust =1,vjust=1,color = "black",size = 10,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 10,vjust =0.5,hjust =1))+
  #scale_y_continuous(breaks = seq(0.01,35,by=8))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14),
        plot.title = element_text(hjust = 0.5))



   
    legend = FALSE,
    display_title = FALSE,
    data_species = get_formal_data(ProLau_models,'resp.var')
  )
