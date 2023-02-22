##### Erin's code - making plots and running a few alternative model specs #####


#  run project_init to get libraries loaded
# a few extra libs for this script
library(gridExtra)
library(patchwork)

################################################################
### Import modeling data ###
################################################################

dataset <- readRDS("./analysis/inputs/dataPRD_dataforModeling.rds")

reg_dat <- dataset %>%
  select(res_id,crew_name,state_name,fips,year_start,year_end,surv,surv2,
         real_wage,real_competing_wage,days_assigned,cumusum_da,year,agency,GACC, first_year) %>%
  mutate(fips=str_pad(fips,5,"left","0"),
         wage_diff=real_wage-real_competing_wage,  #Compute difference between own and competing wage
         across(contains("wage"),~./1000),
         across(c(agency,GACC,crew_name),factor),
         agency=relevel(agency,ref="USFS"),
         GACC=relevel(GACC,ref = "CA-OSCC"),
         wage_diff_scaled = scale(wage_diff)) %>%
  filter(abs(wage_diff_scaled)<3) %>% #Remove obs >3 sd from mean 
  mutate(surv2_name = ifelse(surv2 == 1, "Left", "Stayed"))


################################################################
### Additional salary from one day on assignment 2023 -  dollars ###
################################################################

# base pay pulled from OPM
# this is NOT RUS - this is BASE - need to adjust for whichever
# locality area we care about.

base_pay_table = tibble(year = 
                          c(2011,  2012,  2013,  2014,  2015,  2016,  2017,  2018,  2019,  2020,  2021, 2022, 2023),
                        base_pay_hourly_GS5_step2 = 
                          c(13.58, 13.58, 13.58, 13.72, 13.85, 13.99, 14.13, 14.33, 14.53, 14.91, 15.06, 15.39, 16.02),
                        base_pay_hourly_GS8_step5 = 
                          c(20.43, 20.43, 20.43, 20.64, 20.85, 21.05, 21.26, 21.56, 21.86, 22.43, 22.66, 23.16, 24.11))


## from hours to annual pay:
## GS9 step 5 for supts 26-0 - not estimated here
## Asst supt: CA: GS8 step 5 26-0 - not estimated here
##            non-CA: GS8 step5 18-8 : YES, estimated here
## Squad leader: CA: GS7 step 5 26-0 0 - not estimated here
##               non-CA option 1: GS7 step5 18-8 - not estimated here
##               non-CA option 1: GS7 step5 13-13 ( majority are 7s, except for first year when they are 6s)
## Senior FF: CA GS5 step 2 26-0 - not estimated here
##            non-CA GS5 step 2 13-13 - YES esimated here
## Temp: GS4 no additional days - not estimated here

single_day_hours = tibble(scenario =                 c("Off assignment", "On assignment", "On assignment (day off)"), 
                          paid_base_only =           c(8,                 0,               0),
                          paid_base_plus_OT =        c(0,                 0,               0),
                          paid_base_plus_hazard =    c(0,                 8,               0),
                          paid_base_plus_hazard_and_OT = c(0,             8,              16)) %>%
  mutate(wage_2023_GS5_step2 = (paid_base_only * base_pay_table$base_pay_hourly_GS5_step2[base_pay_table$year == 2023] +
                                  paid_base_plus_OT * base_pay_table$base_pay_hourly_GS5_step2[base_pay_table$year == 2023] * 1.5 +
                                  paid_base_plus_hazard * base_pay_table$base_pay_hourly_GS5_step2[base_pay_table$year == 2023] * 1.25 +
                                  paid_base_plus_hazard_and_OT * base_pay_table$base_pay_hourly_GS5_step2[base_pay_table$year == 2023] * 1.75),
         wage_2023_GS8_step5 = (paid_base_only * base_pay_table$base_pay_hourly_GS8_step5[base_pay_table$year == 2023] +
                                  paid_base_plus_OT * base_pay_table$base_pay_hourly_GS8_step5[base_pay_table$year == 2023] * 1.5 +
                                  paid_base_plus_hazard * base_pay_table$base_pay_hourly_GS8_step5[base_pay_table$year == 2023] * 1.25 +
                                  paid_base_plus_hazard_and_OT * base_pay_table$base_pay_hourly_GS8_step5[base_pay_table$year == 2023] * 1.75)
  )

#wages in RUS (2023), comparing days at station to days on fire assignment
single_day_hours$wage_2023_GS5_step2 * 1.162
single_day_hours$wage_2023_GS8_step5 * 1.162





################################################################
### Alternative model specs (robustness checks) ###
################################################################
rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
                        'Empty row', '-', '-',
                        'Another empty row', '?', '?')



# - model 1, no CA data, to ensure CA is not driving the model
model1_no_CA <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat %>% filter(GACC != "CA-OSCC" & GACC != "CA-ONCC"),cluster=crew_name) 


# - model 1, no competing wages (only days assigned and cumulative experience)
model1_no_comp_wage <- coxph(Surv(year_start,year_end,surv2) ~ days_assigned + cumusum_da + year + agency + GACC, 
                                            data = reg_dat,cluster=crew_name) 

# - model 1, but max earning potential instead of competing wage
model1_only_max_pot <- coxph(Surv(year_start,year_end,surv2) ~ real_wage + days_assigned + cumusum_da + year + agency + GACC, 
                             data = reg_dat,cluster=crew_name) 

# - model 1, but remove everyone who has first year == 2008 (is cum days not adequately capturing human capital b/c dataset is short in relation to careers - cut out everyone whose first year is 2008 - model seems fine)
model1_no_2008_entry <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                          data = reg_dat %>% filter(first_year>2008),cluster=crew_name) 

model1_no_days_assigned <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + cumusum_da + year + agency + GACC, 
               data = reg_dat, cluster=crew_name)
 

model_list <- list(`1`=model1_no_CA,`2`=model1_no_comp_wage,`3`=model1_only_max_pot, `4`=model1_no_2008_entry)



rows <- tibble::tribble(~term, ~M1, ~M2, ~M3, ~M4,
                        'Fixed Effects', '', '', '', '',
                        'Agency', 'x', 'x', 'x', 'x',
                        'GACC', 'x', 'x', 'x', 'x',
                        'Crew', '', '', '', '')

attr(rows, 'position') <- c(21:24) # tells model summary where to put these extra rows


modelsummary(model_list,
             #coef_omit = "crew_name|GACC|year|agency",
             coef_map = c("days_assigned"="Days Assigned","cumusum_da"="Cumulative Experience (days)",
                          "real_competing_wage"="Competing Wage ($1000)","real_wage"="FF Wage ($1000)","wage_diff"="FF-Competing Wage ($1000)",
                          "year2013"="Year 2013","year2014"="Year 2014","year2015"="Year 2015","year2016"="Year 2016","year2017"="Year 2017","year2018"="Year 2018"),
             stars = T,
             title = "Cox PH regression results \\label{tab:main_results}",
             statistic = "robust.se",
             gof_omit = "AIC|RMSE",
             add_rows = rows,
             notes = 'Standard errors are clustered at the IH Crew level to account for correlation between crew members.',
             output = "analysis/outputs/main_result_tab.tex")



################################################################
### Data histograms ###
################################################################

GACC_abb_to_full_name <- function(x) {
  GACC_name = case_when(x == "CA-ONCC" ~ "Northern California",
                        x == "CA-OSCC" ~ "Southern California",
                        x == "AK-ACC" ~ "Alaska",
                        x == "CO-RMC" ~ "Rocky Mountain",
                        x == "MT-NRC" ~ "Northern Rockies",
                        x == "NM-SWC" ~ "Southwest",
                        x == "OR-NWC" ~ "Northwest",
                        x == "UT-GBC" ~ "Great Basin",
                        TRUE ~ "")
  return(GACC_name)
}




# histogram of first year by GACC
ggplot(data = reg_dat %>% 
         mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
         select(first_year, GACC_name, res_id) %>%
         group_by(first_year, GACC_name, res_id) %>%
         summarize(count = n())) +
  geom_histogram(aes(x=first_year, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "First Year", fill = "Geographic Area") +
  theme_bw()

ggsave("analysis/outputs/hist_first_year.png")


# histogram of last year by GACC
ggplot(data = reg_dat %>% 
         mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
         filter(surv2 == 1 | year == 2018) %>%
         mutate(last_year = ifelse(surv2 == 1, year_start + first_year, 2019))) +
  geom_histogram(aes(x=last_year, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Last Year", fill = "Geographic Area") +
  theme_bw()

ggsave("analysis/outputs/hist_last_year.png")

# - hist of competing wage (deflated)
comp_wage_hist = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC))) +
  geom_histogram(aes(x=real_competing_wage, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Real Competing Wage", fill = "Geographic Area") +
  theme_bw() +
  theme(legend.position = "none")  +
  facet_grid(rows = vars(surv2_name))

# days assigned
days_assigned_hist = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC))) +
  geom_histogram(aes(x=days_assigned, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Days Assigned", fill = "Geographic Area") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(surv2_name))


#max crew earning potential (deflated - color by GACC?)
crew_earning_pot_hist = ggplot(data = reg_dat %>% 
                                 mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
                                 filter(real_wage >20)) +
  geom_histogram(aes(x=real_wage, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Maximum Crew Earning Potential", fill = "Geographic Area") +
  theme_bw()  +
  facet_grid(rows = vars(surv2_name))

#max crew earning potential (deflated - color by GACC?)
cum_sum_days_hist = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC))) +
  geom_histogram(aes(x=cumusum_da, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Cumulative Experience (days)", fill = "Geographic Area") +
  theme_bw()+
  theme(legend.position = "none")  +
  facet_grid(rows = vars(surv2_name))

cum_sum_final_yr = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
                            filter(surv2 == 1 | year == 2018)) +
  geom_histogram(aes(x=cumusum_da, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Cumulative Experience (days)", fill = "Geographic Area") +
  theme_bw()+
  theme(legend.position = "none")  +
  facet_grid(rows = vars(surv2_name))



all_graphs = grid.arrange(comp_wage_hist,days_assigned_hist,cum_sum_days_hist,crew_earning_pot_hist,
             widths = c(1,1,1,1.8))

ggsave(plot = all_graphs, filename = "analysis/outputs/histograms.png",
       width = 10, height = 3)


# scatter of real_wage and comp_wage

scatter_real_comp_vs_real_wg = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC))) +
  geom_point(aes(x=real_wage, y = real_competing_wage, col = GACC_name)) +
  labs(x = "Maximum Crew Earning Potential", y = "Real Competing Wage", col = "Geographic Area") +
  theme_bw() +
  scale_color_viridis_d()
scatter_real_comp_vs_real_wg

# boxplot of final year and real_wage 
box_final_yr_wg = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
                           filter(surv2 == 1)) +
  geom_boxplot(aes(x=year, y = real_wage)) +
  theme_bw() +
  labs(x="Seperation Year", y = "Maximum Crew Earning Potential")
box_final_yr_wg

# boxplot of final year and comp_wage
box_final_yr_comp_wg = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
                           filter(surv2 == 1)) +
  geom_boxplot(aes(x=year, y = real_competing_wage)) +
  theme_bw() +
  labs(x="Seperation Year", y = "Real Competing Wage")
box_final_yr_comp_wg

# boxplot of final year and days assigned
box_final_yr_days_assigned = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
                                filter(surv2 == 1)) +
  geom_boxplot(aes(x=year, y = days_assigned)) +
  theme_bw() +
  labs(x="Seperation Year", y = "Days Assigned")
box_final_yr_days_assigned

# boxplot of final year and cumusum_da
box_final_yr_days_assigned = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC)) %>%
                                      filter(surv2 == 1)) +
  geom_boxplot(aes(x=year, y = cumusum_da)) +
  theme_bw() +
  labs(x="Seperation Year", y = "Cumulative Experience (days)")
box_final_yr_days_assigned


# map of duty stations of IHCs, with map/dots colored by COLA percent in 2018

## NOT FINISHED WITH THE MAP - make this if time








