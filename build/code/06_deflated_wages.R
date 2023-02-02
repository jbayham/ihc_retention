################################################################################
################################################################################
# (6) To compute income bin for each year ######################################
################################################################################
################################################################################
#library(cartography)

dataPRD3 <- readRDS(file = "build/cache/dataPRD_wSURV.rds")

#Using package quantmod to pull the CPI series from FRED (St Louis Fed database)
getSymbols("CPIAUCSL", src='FRED') #Note this creates an object called CPIAUSL
avg_cpi <- apply.yearly(CPIAUCSL, mean) %>% #calculate annual average
  as_tibble(rownames = "measure_date") %>%  #coerce to dataframe with rownames as a field
  mutate(year=year(as_date(measure_date))) %>%  #Extract year from date
  select(year,cpi=CPIAUCSL) %>% 
  mutate(adj=cpi/cpi[year==2019]) #construct the deflator

#Joining the deflator and applying it to the wage variables
deflated_prd <- dataPRD3 %>%
  inner_join(select(avg_cpi,-cpi),by="year") %>% 
  mutate(real_wage=SeniorFFxLoc/adj,
         real_competing_wage=med_wage/adj)
  

#Plotting data to see if the adjustment makes sense
# deflated_prd %>%
#   select(res_id,year,SeniorFFxLoc,real_wage,real_competing_wage) %>%
#   pivot_longer(-c(res_id,year)) %>%
#   ggplot(aes(x=factor(year),y=value,color=name)) +
#   geom_boxplot() +
#   scale_color_discrete(name="") +
#   theme_bw(base_size = 14) +
#   labs(x="Year",y="Wages")


#Cache dataset
write_csv(deflated_prd, file = "build/cache/prd_deflated_wages.csv")

################################################################################
################################################################################
################################################################################
