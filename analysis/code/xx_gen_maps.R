#Script to create maps to visualize the wage data

library(tigris)
library(USAboundaries)
library(mapview)
library(leaflet)
library(patchwork)


dataset <- readRDS("./analysis/inputs/dataPRD_dataforModeling.rds")

reg_dat <- dataset %>%
  select(res_id,crew_name,fips,year_start,year_end,surv,surv2,
         real_wage,real_competing_wage,days_assigned,cumusum_da,year,agency,GACC) %>%
  mutate(fips=str_pad(fips,5,"left","0"),
         wage_diff=real_wage-real_competing_wage,
         across(c(agency,GACC,crew_name),factor),
         agency=relevel(agency,ref="USFS"),
         GACC=relevel(GACC,ref = "CA-OSCC"),
         wage_diff_scaled = scale(wage_diff)) %>%
  filter(abs(wage_diff_scaled)<3) #Remove obs >3 sd from mean

us_counties <- us_counties(resolution = "low") %>%
  select(geoid,name,state_abbr) %>%
  st_transform(5070)

us_states <- us_states(resolution = "low") %>%
  select(geoid,stusps) %>%
  st_transform(5070)

map_wages <- reg_dat %>%
  group_by(year,fips) %>%
  summarize(across(contains("wage"),~mean(.,na.rm=T))) %>%
  inner_join(us_counties,.,by=c("geoid"="fips"))

st_outline <- reg_dat %>%
  group_by(year,fips=str_sub(fips,1,2)) %>%
  summarize(across(contains("wage"),~mean(.,na.rm=T))) %>%
  inner_join(us_states,.,by=c("geoid"="fips"))

to_plot <- map_wages %>%
  filter(year==2015) 

# mapview(to_plot,zcol=c("wage_diff"),layer.name = "Wage Diff") %>%
#   addPopups(to_plot, popup = ~paste("Crew: ", crew_name, "<br>",
#                                     "Own Wage: ", real_wage))


diff <- ggplot() +
  geom_sf(data=map_wages,aes(fill=wage_diff)) +
  geom_sf(data=st_outline,fill=NA,color="black",alpha=.5) +
  scale_fill_gradient2() +
  facet_wrap(~year,nrow=1) +
  theme_void()

own <- ggplot() +
  geom_sf(data=map_wages,aes(fill=real_wage)) +
  geom_sf(data=st_outline,fill=NA,color="black",alpha=.5) +
  scale_fill_viridis() +
  facet_wrap(~year,nrow=1) +
  theme_void()

compete <- ggplot() +
  geom_sf(data=map_wages,aes(fill=real_competing_wage)) +
  geom_sf(data=st_outline,fill=NA,color="black",alpha=.5) +
  scale_fill_viridis() +
  facet_wrap(~year,nrow=1) +
  theme_void()

diff/own/compete

reg_dat %>%
  group_by(year,fips) %>%
  summarize(across(contains("wage"),~mean(.,na.rm=T))) %>%
  ggplot(aes(x=wage_diff)) +
  geom_histogram()