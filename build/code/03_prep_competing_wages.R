
library(readxl)

#################"C:\Users\magst\Desktop\git\ihc_retention\build\inputs\inputs\bls"
#read data by year
#flist <- dir(".build/inputs/bls/",pattern = ".xlsx")
flist <- dir("C:\\Users\\magst\\Desktop\\git\\ihc_retention\\build\\inputs\\inputs\\bls\\",pattern = ".xlsx")

oe_dat <- vector("list",length(flist))

#Reading in large xlsx files.  There was a structural change in 2017 (i=6) so the read command changed to ensure correct datatype in read.
for(i in c(1:5)){ 
    #read data
  oe_dat[[i]] <- read_excel(str_c("C:\\Users\\magst\\Desktop\\git\\ihc_retention\\build\\inputs\\inputs\\bls\\",flist[i]),sheet = 1,
                            col_types = c(rep("text",9),rep("numeric",20)))#"build/inputs/bls/"
    
}

for(i in c(6:8)){
  #read data
  oe_dat[[i]] <- read_excel(str_c("C:\\Users\\magst\\Desktop\\git\\ihc_retention\\build\\inputs\\inputs\\bls\\",flist[i]),sheet = 1,
                            col_types = c(rep("text",10),rep("numeric",20)))#"build/inputs/bls/"
  
}

excevar_select <- c("area","area_title","area_type","naics","naics_title","own_code","occ_code","occ_title","tot_emp")

#add year and subset fields
merge_dat <- map2_dfr(oe_dat,flist,function(df,y){
  df %>%
    janitor::clean_names() %>%
    select(all_of(var_select),starts_with("a_")) %>%
    add_column(year=str_extract(y,"[:digit:]{4}")) 
})
  
#cache data
write_csv(merge_dat,"build/cache/oe_dat.csv")

#########################
#load related occ_codes
related <- read_csv("build/inputs/bls/related_occupations.csv")

cbsa <- tigris::core_based_statistical_areas(cb=T) %>%
  st_transform(4326) %>%
  clean_names()


hb_text <- read_csv("build/inputs/home_base/IHC_home_base_locations_wFIPS.txt") %>%
  st_as_sf(coords=c("LongitudeFromAddress","LatitudeFromAddress")) %>%
  st_set_crs(4326)

# mapview(hb_text) +
#   mapview(cbsa)

merge_dat <- read_csv("build/cache/oe_dat.csv")

occ_subset <- inner_join(select(related,occ_code) %>% mutate(occ_code=str_sub(occ_code,1,7)),
                       merge_dat) %>%
  filter(naics=="000000")


#Create composite alternative wage (employment-weighted average of top 5 closest jobs)
metro_occ <- occ_subset %>%
  filter(naics=="000000",area_type=="4") %>%
  group_by(area,year) %>%
  summarize(a_median_m=mean(a_median,na.rm=T),
            a_median_wm=weighted.mean(a_median,tot_emp,na.rm=T)) %>%
  ungroup() %>%
  mutate(med_wage=coalesce(a_median_wm,a_median_m)) %>%
  select(-starts_with("a_"))

nonmetro_occ <- occ_subset %>%
  filter(naics=="000000",area_type=="6") %>%
  group_by(geoid=str_sub(area,1,2),year) %>%
  summarize(a_median_m=mean(a_median,na.rm=T),
            a_median_wm=weighted.mean(a_median,tot_emp,na.rm=T)) %>%
  ungroup() %>%
  mutate(med_wage=coalesce(a_median_wm,a_median_m)) %>%
  select(-starts_with("a_"))


#Create alternative wages based on top 5 closest jobs (employment-weighted average across areas)
metro_occ_top5 <- occ_subset %>%
  filter(naics=="000000",area_type=="4") %>%
  group_by(area,year,occ_title) %>%
  summarize(a_median_m=mean(a_median,na.rm=T),
            a_median_wm=weighted.mean(a_median,tot_emp,na.rm=T)) %>%
  ungroup() %>%
  mutate(med_wage=coalesce(a_median_wm,a_median_m)) %>%
  select(-starts_with("a_"))

nonmetro_occ_top5 <- occ_subset %>%
  filter(naics=="000000",area_type=="6") %>%
  group_by(geoid=str_sub(area,1,2),year,occ_title) %>%
  summarize(a_median_m=mean(a_median,na.rm=T),
            a_median_wm=weighted.mean(a_median,tot_emp,na.rm=T)) %>%
  ungroup() %>%
  mutate(med_wage=coalesce(a_median_wm,a_median_m)) %>%
  select(-starts_with("a_"))


#Join crew homes with cbsa's
hb_cbsa <- st_join(hb_text,cbsa)

#Join crews that did match with corresponding cbsa areas
hb_cbsa_metro <- hb_cbsa %>%
  filter(!is.na(geoid)) %>% 
  st_set_geometry(NULL)  

#weighted average of top 5 alt occupations
hb_metro_occ <- hb_cbsa_metro %>%
  inner_join(metro_occ,by=c("geoid"="area"))

hb_metro_occ_top5 <- hb_cbsa_metro %>%
  inner_join(metro_occ_top5,by=c("geoid"="area"))

#check that all crews matched
unique(hb_metro_occ$Crew_Name)
hb_cbsa_metro$Crew_Name[!(hb_cbsa_metro$Crew_Name %in% unique(hb_metro_occ$Crew_Name))]
unique(hb_metro_occ_top5$Crew_Name)



#Join crews that did not match with state nonmetro wages
hb_cbsa_nonmetro <- hb_cbsa %>%
  filter(is.na(geoid) | !(Crew_Name %in% unique(hb_metro_occ$Crew_Name))) %>% 
  st_set_geometry(NULL) %>%
  select(-c(csafp:awater)) 

#weighted average of top 5 alt occupations
hb_nonmetro_occ <- hb_cbsa_nonmetro %>%
  inner_join(nonmetro_occ,by=c("STATE_FIPS"="geoid"))

hb_nonmetro_occ_top5 <- hb_cbsa_nonmetro %>%
  inner_join(nonmetro_occ_top5,by=c("STATE_FIPS"="geoid"))

#check that all crews matched
unique(hb_nonmetro_occ$Crew_Name)
unique(hb_nonmetro_occ_top5$Crew_Name)

#bind rows
hb_occ <- bind_rows(
  hb_metro_occ,
  hb_nonmetro_occ
)

hb_occ_top5 <- bind_rows(
  hb_metro_occ_top5,
  hb_nonmetro_occ_top5
)

write_csv(hb_occ,"build/cache/hb_occ.csv")
write_csv(hb_occ_top5,"build/cache/hb_occ_top5.csv")


###################################
#Consider imputation of missing values using missforest
# hb_occ_imp <- hb_occ %>%
#   select(med_wage,year,STATE_NAME,Agency,GACC) %>%
#   mutate(across(where(is.character), factor)) %>% 
#   as.data.frame() %>%
#   missForest(.,verbose = T) %>%
#   pluck(1)
# 
# check <- hb_occ %>%
#   select(med_wage,Crew_Name,year,CNTY_NAME,STATE_NAME) %>%
#   bind_cols(hb_occ_imp)


