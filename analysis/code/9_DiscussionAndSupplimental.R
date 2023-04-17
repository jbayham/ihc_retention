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
         real_wage,real_competing_wage,days_assigned,cumusum_da,year,agency,GACC, first_year,
         year_start_reentry_older_cohort, year_end_reentry_older_cohort,
         year_start_reentry_clock, year_end_reentry_clock) %>%
  mutate(fips=str_pad(fips,5,"left","0"),
         wage_diff=real_wage-real_competing_wage,  #Compute difference between own and competing wage
         cumusum_da=cumusum_da/100,
         across(contains("wage"),~./1000),
         across(c(agency,GACC,crew_name),factor),
         agency=relevel(agency,ref="USFS"),
         GACC=relevel(GACC,ref = "CA-OSCC"),
         wage_diff_scaled = scale(wage_diff)) %>%
  filter(abs(wage_diff_scaled)<3) %>% #Remove obs >3 sd from mean 
  #filter(cumusum_da<10) %>% # remove cumusum days of >=1000 - taken out March 23, 2023
  # for crew name model, need a crew id that is completely de-identified
  left_join(tibble(crew_name = unique(dataset$crew_name)) %>%
              mutate(crew_id = str_c("crew_",row_number())), 
            by = c("crew_name" = "crew_name")) %>%
  mutate(year = relevel(year, ref="2015")) %>%
  select(res_id, crew_id, year_start, year_end, surv2,real_wage, real_competing_wage, days_assigned, 
         cumusum_da,wage_diff, year, agency, GACC, first_year,
         year_start_reentry_older_cohort, year_end_reentry_older_cohort,
         year_start_reentry_clock, year_end_reentry_clock) %>%
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
### Check PH assumption ###
################################################################

# original preferred model (no id=res_id) reentry = Shayne's method
model1 <- coxph(Surv(year_start,year_end,surv2) ~ 
                  real_competing_wage + days_assigned + cumusum_da + year + agency + GACC,
                cluster=crew_id,
                data = reg_dat)

# wage diff instead of competing wage
model2 <-coxph(Surv(year_start,year_end,surv2) ~ 
                 wage_diff + days_assigned + cumusum_da + year + agency + GACC,
               cluster=crew_id,
               data = reg_dat)

# crew fixed effect (competing wages plus crew id)
model3 <-coxph(Surv(year_start,year_end,surv2) ~ 
                 real_competing_wage + days_assigned + cumusum_da + year + agency + GACC + crew_id,
               data = reg_dat)


# diagnostics
ph_assumption_check1 = ggcoxdiagnostics(model1, type = "schoenfeld")
ph_assumption_check1
ggsave(plot = ph_assumption_check1, filename = "analysis/outputs/ph_assumptions1.png",
       height = 6, width = 10, unit = "in")

ph_assumption_check2 = ggcoxdiagnostics(model2, type = "schoenfeld")
ph_assumption_check2
ggsave(plot = ph_assumption_check2, filename = "analysis/outputs/ph_assumptions2.png",
       height = 6, width = 10, unit = "in")

ph_assumption_check3 = ggcoxdiagnostics(model3, type = "schoenfeld")
ph_assumption_check3
ggsave(plot = ph_assumption_check3, filename = "analysis/outputs/ph_assumptions3.png",
       height = 6, width = 10, unit = "in")

# this test is known to be anti-conservative, so we're not too worried
# see paragraph in the appendix
zph <- cox.zph(model1)
par(mfrow = c(2, 3))
plot(zph, var = 1, df=3)
abline(0, 0, lty=3) 
abline(lm(zph$y[,1] ~ zph$x)$coefficients, lty=4, col=3) 
plot(zph, var = 2, df=3)
abline(0, 0, lty=3) 
abline(lm(zph$y[,2] ~ zph$x)$coefficients, lty=4, col=3) 
plot(zph, var = 3, df=3)
abline(0, 0, lty=3) 
abline(lm(zph$y[,3] ~ zph$x)$coefficients, lty=4, col=3) 
plot(zph, var = 4, df=3)
abline(0, 0, lty=3) 
abline(lm(zph$y[,4] ~ zph$x)$coefficients, lty=4, col=3) 
plot(zph, var = 5, df=3)
abline(0, 0, lty=3) 
abline(lm(zph$y[,5] ~ zph$x)$coefficients, lty=4, col=3) 
plot(zph, var = 6, df=3)
abline(0, 0, lty=3) 
abline(lm(zph$y[,6] ~ zph$x)$coefficients, lty=4, col=3) 


################################################################
### Alternative model specs (robustness checks) ###
################################################################
# model1 : the preferred spec in our paper, included for comparison
model1 <- coxph(Surv(year_start,year_end,surv2) ~ 
                  real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat, cluster=crew_id) 

# - model 1, no CA data, to ensure CA is not driving the model
model1_no_CA <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat %>% filter(GACC != "CA-OSCC" & GACC != "CA-ONCC"),cluster=crew_id) 


# - model 1, no competing wages (only days assigned and cumulative experience)
model1_no_comp_wage <- coxph(Surv(year_start,year_end,surv2) ~ days_assigned + cumusum_da + year + agency + GACC, 
                                            data = reg_dat,cluster=crew_id) 

# - model 1, but max earning potential instead of competing wage
model1_only_max_pot <- coxph(Surv(year_start,year_end,surv2) ~ real_wage + days_assigned + cumusum_da + year + agency + GACC, 
                             data = reg_dat,cluster=crew_id) 

# - model 1, but remove everyone who has first year == 2008 (is cum days not adequately capturing human capital b/c dataset is short in relation to careers - cut out everyone whose first year is 2008 - model seems fine)
model1_no_2008_entry <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                          data = reg_dat %>% filter(first_year>2008),cluster=crew_id) 

model1_no_days_assigned <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + cumusum_da + year + agency + GACC, 
               data = reg_dat, cluster=crew_id)
 

model_list <- list(`Model 1`=model1,
                   `No California`=model1_no_CA,
                   `No Competing Wage`=model1_no_comp_wage,
                   `MCEP and No Competing Wage`=model1_only_max_pot, 
                   `Start date after 2008`=model1_no_2008_entry, 
                   `No Days Assigned`=model1_no_days_assigned)



rows <- tibble::tribble(~term, ~M1, ~M2, ~M3, ~M4, ~M5, ~M6,
                        'Fixed Effects', '', '', '', '','','',
                        'Agency', 'x', 'x', 'x', 'x','x','x',
                        'GACC', 'x', 'x', 'x', 'x','x','x',
                        'Crew', '', '', '', '', '','',)

attr(rows, 'position') <- c(19:22) # tells model summary where to put these extra rows


modelsummary(model_list,
             coef_omit = "crew_name|GACC|agency",
             coef_map = c("days_assigned"="Days Assigned","cumusum_da"="Cumulative Experience (100 days)",
                          "real_competing_wage"="Competing Wage ($1000)","real_wage"="MCEP ($1000)","wage_diff"="MCEP-Competing Wage ($1000)",
                          "year2013"="Year 2013","year2014"="Year 2014","year2015"="Year 2015","year2016"="Year 2016","year2017"="Year 2017","year2018"="Year 2018"),
             stars = T,
             title = "Cox PH regression results \\label{tab:main_results}",
             statistic = "robust.se",
             gof_omit = "AIC|RMSE",
             add_rows = rows,
             notes = 'Standard errors are clustered at the IH Crew level to account for correlation between crew members.',
             output = "analysis/outputs/robustness_coef_for_supp_mat.tex")



# We did stratify by year to see results given that year is the most eggregious 
# offender of PH assumptions
# model 1 stratified by year
model1_stratified <- coxph(Surv(year_start_reentry_older_cohort,year_end_reentry_older_cohort,surv2) ~ real_competing_wage + days_assigned + cumusum_da + agency + GACC + strata(year), 
                     data = reg_dat, cluster=crew_id) 

################################################################
### Alternative model specs (re-entry data structure) ###
################################################################

# re entry alternative data structures

# original preferred model - reentry was treated as though they'd been there all years
# we DO NOT SHOW this in the paper as we decided it wasn't the correct structure - it is here only for 
# internal documentation
model1_orig <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat, cluster=crew_id) 

# model1 : the preferred spec in our paper, included for comparison
model1 <- coxph(Surv(year_start_reentry_older_cohort,year_end_reentry_older_cohort,surv2) ~ 
                  real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat, cluster=crew_id) 

# restart the clock (must cluster on res_id)
model1_clock <- coxph(Surv(year_start_reentry_clock,year_end_reentry_clock,surv2) ~ 
                        real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                      data = reg_dat, cluster=res_id) 

# using surv instead of surv2 - gap is ignored
model1_no_gap <- coxph(Surv(year_start,year_end,surv) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                     data = reg_dat, cluster=crew_id) 

# put res_id and crew_id in random effects structure
# this takes ~45 minutes to run
library(coxme)
model1_coxme <- coxme(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC +
                    (1 | res_id) + (1 | crew_id), 
                  data = reg_dat) 


rows <- tibble::tribble(~term, ~M1, ~M2, ~M3, ~M4, ~M5, 
                        'Fixed Effects', '', '', '','', '',
                        'Agency', 'x', 'x', 'x','x', 'x',
                        'GACC',   'x', 'x', 'x','x', 'x',
                        'Crew',    '',  '',  '', '', '',)

attr(rows, 'position') <- c(21:24)

model_list <- list(`Model 1`=model1,
                   `Final exit is only exit`=model1_a,
                   `New entry for each re-entry`=model1_b,
                   `Random effects of individual and crew`=model1_c,
                   `cluster crew id,\nid is res id`=model1_d)
#`frailty res id`=model1_e) #,
#`coxme crew and res id RE`=model1_e)

modelsummary(model_list,
             #coef_omit = "crew_name|GACC|year|agency",
             coef_map = c("days_assigned"="Days Assigned","cumusum_da"="Cumulative Experience (100 days)",
                          "real_competing_wage"="Competing Wage ($1000)","real_wage"="FF Wage ($1000)","wage_diff"="FF-Competing Wage ($1000)",
                          "year2013"="Year 2013","year2014"="Year 2014","year2015"="Year 2015","year2016"="Year 2016","year2017"="Year 2017","year2018"="Year 2018"),
             stars = T,
             title = "Cox PH regression results \\label{tab:main_results}",
             statistic = "robust.se",
             gof_omit = "AIC|RMSE",
             add_rows = rows,
             notes = 'Standard errors are clustered at the IHC level to account for correlation between crew members, except for the third and fifth columns.')#,
#output = "analysis/outputs/main_result_tab.tex")



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

wage_diff_hist = ggplot(data = reg_dat %>% mutate(GACC_name = GACC_abb_to_full_name(GACC)) ) +
  geom_histogram(aes(x=wage_diff, fill = GACC_name)) +
  scale_fill_viridis_d() +
  labs(y="count", x = "Wage Difference (MCEP-Competing Wage; $1000)", fill = "Geographic Area") +
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




################################################################
### Num IHCs per year (Fig 1) ###
################################################################


### import data pulled from ROSS (directs to Erin's personal files b/c - faster)
assign_data_raw = read_csv(file = "C:/Users/erinjbelval/Documents/2023/IHC retention/Personnel Retentaion data 2008 to 2021 IHCs only v3.csv") %>%
  mutate(unique_id = str_c(ross_res_id, "-", irwin_res_id))

#import crew lookup
IHC.filename = "C:/Users/erinjbelval/Documents/2023/IHC retention/All IHC IDs.csv"
crew.lookup = tibble(read_csv(IHC.filename))
# give Ventanna a ROSS ID for easier handling - id picked randomly by Erin
crew.lookup$`Primary ID from ROSS` = ifelse(crew.lookup$`Crew Name` == "Ventanna", 4567890, crew.lookup$`Primary ID from ROSS`)

### make data frame with:
# one row per person
# columns:
#  - unique ID
#  - first year as IHC
#  - last year as IHC
#  - where did they come from (year prior to first year as IHC)
#  - where did they go to (year after their last year as IHC)
#  - IHC res ID in 2008 (NA if not IHC in 2008)
#  - IHC res ID in 2009 (NA if not IHC in 2009)
#  - ...
#  - IHC res ID in 2021 (NA if not IHC in 2021)

num_pers_in_data = length(unique(assign_data_raw$unique_id))
IHCs_wide = tibble(unique_id = unique(assign_data_raw$unique_id),
                   first_year = rep(0, num_pers_in_data),
                   last_year = rep(0, num_pers_in_data),
                   source_ag = rep("Not in system", num_pers_in_data),
                   source_cont_st = rep("Not in system", num_pers_in_data),
                   destination_ag = rep("Not in system", num_pers_in_data),
                   destination_cont_st = rep("Not in system", num_pers_in_data),
                   IHC_id_2008 = rep(NA, num_pers_in_data), 
                   IHC_id_2009 = rep(NA, num_pers_in_data), 
                   IHC_id_2010 = rep(NA, num_pers_in_data), 
                   IHC_id_2011 = rep(NA, num_pers_in_data), 
                   IHC_id_2012 = rep(NA, num_pers_in_data),
                   IHC_id_2013 = rep(NA, num_pers_in_data), 
                   IHC_id_2014 = rep(NA, num_pers_in_data), 
                   IHC_id_2015 = rep(NA, num_pers_in_data), 
                   IHC_id_2016 = rep(NA, num_pers_in_data), 
                   IHC_id_2017 = rep(NA, num_pers_in_data),
                   IHC_id_2018 = rep(NA, num_pers_in_data), 
                   IHC_id_2019 = rep(NA, num_pers_in_data), 
                   IHC_id_2020 = rep(NA, num_pers_in_data), 
                   IHC_id_2021 = rep(NA, num_pers_in_data),
                   num_yrs_IHC = rep(0, num_pers_in_data))

for (i in c(0:num_pers_in_data)){
  id = IHCs_wide$unique_id[i]
  idx = which(assign_data_raw$unique_id == id & assign_data_raw$personnel_type == "IHC" & assign_data_raw$days_assigned >= 28)
  if (length(idx)>0){
    small_df = assign_data_raw[idx,]
    IHCs_wide$first_year[i] = min(small_df$year)
    IHCs_wide$last_year[i] = max(small_df$year)
    #get source
    tmp_idx = which(assign_data_raw$unique_id == id & assign_data_raw$personnel_type != "IHC" & 
                      assign_data_raw$year == IHCs_wide$first_year[i]-1)
    if (length(tmp_idx)>0){
      source_df = assign_data_raw[tmp_idx,]
      ag_idx = which(source_df$days_assigned == max(source_df$days_assigned))[1]
      IHCs_wide$source_ag[i] = source_df$agency[ag_idx]
      IHCs_wide$source_cont_st[i] = source_df$contrat_status[ag_idx]
    }
    #get destination
    tmp_idx = which(assign_data_raw$unique_id == id & assign_data_raw$personnel_type != "IHC" & 
                      assign_data_raw$year == IHCs_wide$first_year[i]+1)
    if (length(tmp_idx)>0){
      dest_df = assign_data_raw[tmp_idx,]
      ag_idx = which(dest_df$days_assigned == max(dest_df$days_assigned))[1]
      IHCs_wide$destination_ag[i] = dest_df$agency[ag_idx]
      IHCs_wide$destination_cont_st[i] = dest_df$contrat_status[ag_idx]
    }
    
    IHCs_wide$IHC_id_2008[i] = ifelse(2008 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2008], NA)
    IHCs_wide$IHC_id_2009[i] = ifelse(2009 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2009], NA)
    IHCs_wide$IHC_id_2010[i] = ifelse(2010 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2010], NA)
    IHCs_wide$IHC_id_2011[i] = ifelse(2011 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2011], NA)
    IHCs_wide$IHC_id_2012[i] = ifelse(2012 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2012], NA)
    IHCs_wide$IHC_id_2013[i] = ifelse(2013 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2013], NA)
    IHCs_wide$IHC_id_2014[i] = ifelse(2014 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2014], NA)
    IHCs_wide$IHC_id_2015[i] = ifelse(2015 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2015], NA)
    IHCs_wide$IHC_id_2016[i] = ifelse(2016 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2016], NA)
    IHCs_wide$IHC_id_2017[i] = ifelse(2017 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2017], NA)
    IHCs_wide$IHC_id_2018[i] = ifelse(2018 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2018], NA)
    IHCs_wide$IHC_id_2019[i] = ifelse(2019 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2019], NA)
    IHCs_wide$IHC_id_2020[i] = ifelse(2020 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2020], NA)
    IHCs_wide$IHC_id_2021[i] = ifelse(2021 %in% small_df$year, small_df$IHC_res_ID[small_df$year == 2021], NA)
    
  }
  IHCs_wide$num_yrs_IHC[i] = (ifelse(is.na(IHCs_wide$IHC_id_2008[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2009[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2010[i]),0,1) + 
                                ifelse(is.na(IHCs_wide$IHC_id_2011[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2012[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2013[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2014[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2015[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2016[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2017[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2018[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2019[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2020[i]),0,1) +
                                ifelse(is.na(IHCs_wide$IHC_id_2021[i]),0,1))
}

#try to fix 2020
IHCs_wide$IHC_id_2020 = ifelse(is.na(IHCs_wide$IHC_id_2020) & is.na(IHCs_wide$IHC_id_2019)==FALSE & is.na(IHCs_wide$IHC_id_2021) == FALSE, 
                               IHCs_wide$IHC_id_2021, IHCs_wide$IHC_id_2020)

IHCs_wide$IHC_in_2008 = ifelse(is.na(IHCs_wide$IHC_id_2008),0,1)
IHCs_wide$IHC_in_2009 = ifelse(is.na(IHCs_wide$IHC_id_2009),0,1)
IHCs_wide$IHC_in_2010 = ifelse(is.na(IHCs_wide$IHC_id_2010),0,1)
IHCs_wide$IHC_in_2011 = ifelse(is.na(IHCs_wide$IHC_id_2011),0,1)
IHCs_wide$IHC_in_2012 = ifelse(is.na(IHCs_wide$IHC_id_2012),0,1)
IHCs_wide$IHC_in_2013 = ifelse(is.na(IHCs_wide$IHC_id_2013),0,1)
IHCs_wide$IHC_in_2014 = ifelse(is.na(IHCs_wide$IHC_id_2014),0,1)
IHCs_wide$IHC_in_2015 = ifelse(is.na(IHCs_wide$IHC_id_2015),0,1)
IHCs_wide$IHC_in_2016 = ifelse(is.na(IHCs_wide$IHC_id_2016),0,1)
IHCs_wide$IHC_in_2017 = ifelse(is.na(IHCs_wide$IHC_id_2017),0,1)
IHCs_wide$IHC_in_2018 = ifelse(is.na(IHCs_wide$IHC_id_2018),0,1)
IHCs_wide$IHC_in_2019 = ifelse(is.na(IHCs_wide$IHC_id_2019),0,1)
IHCs_wide$IHC_in_2020 = ifelse(is.na(IHCs_wide$IHC_id_2020),0,1)
IHCs_wide$IHC_in_2021 = ifelse(is.na(IHCs_wide$IHC_id_2021),0,1)

IHCs_wide$prev_yrs_2008 = 0
IHCs_wide$prev_yrs_2009 = IHCs_wide$IHC_in_2008
IHCs_wide$prev_yrs_2010 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009  )
IHCs_wide$prev_yrs_2011 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010  )
IHCs_wide$prev_yrs_2012 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 )
IHCs_wide$prev_yrs_2013 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 )
IHCs_wide$prev_yrs_2014 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 )
IHCs_wide$prev_yrs_2015 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 )
IHCs_wide$prev_yrs_2016 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 + IHCs_wide$IHC_in_2015 )
IHCs_wide$prev_yrs_2017 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 + IHCs_wide$IHC_in_2015 + IHCs_wide$IHC_in_2016 )
IHCs_wide$prev_yrs_2018 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 + IHCs_wide$IHC_in_2015 + IHCs_wide$IHC_in_2016 + 
                             IHCs_wide$IHC_in_2017 )
IHCs_wide$prev_yrs_2019 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 + IHCs_wide$IHC_in_2015 + IHCs_wide$IHC_in_2016 + 
                             IHCs_wide$IHC_in_2017 + IHCs_wide$IHC_in_2018 )
IHCs_wide$prev_yrs_2020 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 + IHCs_wide$IHC_in_2015 + IHCs_wide$IHC_in_2016 + 
                             IHCs_wide$IHC_in_2017 + IHCs_wide$IHC_in_2018 + IHCs_wide$IHC_in_2019 )
IHCs_wide$prev_yrs_2021 = (IHCs_wide$IHC_in_2008 + IHCs_wide$IHC_in_2009 + IHCs_wide$IHC_in_2010 + 
                             IHCs_wide$IHC_in_2011 + IHCs_wide$IHC_in_2012 + IHCs_wide$IHC_in_2013 + 
                             IHCs_wide$IHC_in_2014 + IHCs_wide$IHC_in_2015 + IHCs_wide$IHC_in_2016 + 
                             IHCs_wide$IHC_in_2017 + IHCs_wide$IHC_in_2018 + IHCs_wide$IHC_in_2019 + 
                             IHCs_wide$IHC_in_2020 )

# a few stats
# number IHCs in a given year
length(which(is.na(IHCs_wide$IHC_id_2021)==FALSE))
# what was turnover 2019-2021
length(which(is.na(IHCs_wide$IHC_id_2019) == FALSE & is.na(IHCs_wide$IHC_id_2021) == FALSE))
# That's half the workforce! Let's see if this is in line with 2017-2019 and 2016-2018
length(which(is.na(IHCs_wide$IHC_id_2017) == FALSE & is.na(IHCs_wide$IHC_id_2019) == FALSE))
length(which(is.na(IHCs_wide$IHC_id_2016) == FALSE & is.na(IHCs_wide$IHC_id_2018) == FALSE))
# wow. The 2019-2021 turnover is LOWER than 2017-2019 and 2016-2018


### make long data frame with
# one row per person/year, filtered on if they were an IHC this year (i.e, num_yrs_ihc>0)
# columns:
#  - unique ID
#  - year
#  - was IHC this year
#  - was IHC next year
#  - was IHC last year
#  - was ever IHC
#  - retained into next year (looking at destination)
#  - lost next year (looking at destination)
#  - recruited new this year (for looking at source)
#  - recruited back this year (for looking at source)
#  - retained this year (for looking at source)
unique_yrs = c(2008:2021)
IHCs_long = IHCs_wide %>% filter(num_yrs_IHC>0) %>% 
  expand(nesting(unique_id), unique_yrs) %>% rename("year" = "unique_yrs") %>%
  left_join(IHCs_wide, by = c("unique_id" = "unique_id")) %>%
  mutate(was_IHC_this_year = ifelse(year == 2008, IHC_in_2008,
                                    ifelse(year == 2009, IHC_in_2009,
                                           ifelse(year == 2010, IHC_in_2010, 
                                                  ifelse(year == 2011, IHC_in_2011,
                                                         ifelse(year == 2012, IHC_in_2012,
                                                                ifelse(year == 2013, IHC_in_2013,
                                                                       ifelse(year == 2014, IHC_in_2014,
                                                                              ifelse(year == 2015, IHC_in_2015,
                                                                                     ifelse(year == 2016, IHC_in_2016,
                                                                                            ifelse(year == 2017, IHC_in_2017,
                                                                                                   ifelse(year == 2018, IHC_in_2018,
                                                                                                          ifelse(year == 2019, IHC_in_2019,
                                                                                                                 ifelse(year == 2020, IHC_in_2020,
                                                                                                                        ifelse(year == 2021, IHC_in_2021,NA)))))))))))))),
         was_IHC_next_year = ifelse(year == 2008, IHC_in_2009,
                                    ifelse(year == 2009, IHC_in_2010,
                                           ifelse(year == 2010, IHC_in_2011, 
                                                  ifelse(year == 2011, IHC_in_2012,
                                                         ifelse(year == 2012, IHC_in_2013,
                                                                ifelse(year == 2013, IHC_in_2014,
                                                                       ifelse(year == 2014, IHC_in_2015,
                                                                              ifelse(year == 2015, IHC_in_2016,
                                                                                     ifelse(year == 2016, IHC_in_2017,
                                                                                            ifelse(year == 2017, IHC_in_2018,
                                                                                                   ifelse(year == 2018, IHC_in_2019,
                                                                                                          ifelse(year == 2019, IHC_in_2020,
                                                                                                                 ifelse(year == 2020, IHC_in_2021,NA))))))))))))),
         was_IHC_last_year = ifelse(year == 2008, NA,
                                    ifelse(year == 2009, IHC_in_2008,
                                           ifelse(year == 2010, IHC_in_2009, 
                                                  ifelse(year == 2011, IHC_in_2010,
                                                         ifelse(year == 2012, IHC_in_2011,
                                                                ifelse(year == 2013, IHC_in_2012,
                                                                       ifelse(year == 2014, IHC_in_2013,
                                                                              ifelse(year == 2015, IHC_in_2014,
                                                                                     ifelse(year == 2016, IHC_in_2015,
                                                                                            ifelse(year == 2017, IHC_in_2016,
                                                                                                   ifelse(year == 2018, IHC_in_2017,
                                                                                                          ifelse(year == 2019, IHC_in_2018,
                                                                                                                 ifelse(year == 2020, IHC_in_2019,
                                                                                                                        ifelse(year == 2021, IHC_in_2020, NA)))))))))))))),
         total_yrs_prev_IHC = ifelse(year == 2008, prev_yrs_2008,
                                     ifelse(year == 2009, prev_yrs_2009,
                                            ifelse(year == 2010, prev_yrs_2010,
                                                   ifelse(year == 2011, prev_yrs_2011,
                                                          ifelse(year == 2012, prev_yrs_2012,
                                                                 ifelse(year == 2013, prev_yrs_2013,
                                                                        ifelse(year == 2014, prev_yrs_2014,
                                                                               ifelse(year == 2015, prev_yrs_2015,
                                                                                      ifelse(year == 2016, prev_yrs_2016,
                                                                                             ifelse(year == 2017, prev_yrs_2017,
                                                                                                    ifelse(year == 2018, prev_yrs_2018,
                                                                                                           ifelse(year == 2019,prev_yrs_2019,
                                                                                                                  ifelse(year == 2020, prev_yrs_2020,
                                                                                                                         ifelse(year == 2021, prev_yrs_2021, NA)))))))))))))) 
  ) %>%
  mutate(was_ever_prev_IHC = ifelse(total_yrs_prev_IHC>0,1,0)) %>%
  mutate(retained_thru_next_yr = ifelse(was_IHC_this_year == 1 & was_IHC_next_year == 1, 1, 0),
         lost_next_yr = ifelse(was_IHC_this_year == 1 & was_IHC_next_year == 0, 1, 0),
         recruited_new_this_yr = ifelse(was_IHC_this_year == 1 & was_ever_prev_IHC == 0, 1, 0),
         recruited_back_this_yr = ifelse(was_IHC_this_year == 1 & was_ever_prev_IHC == 1 & was_IHC_last_year == 0, 1, 0),
         retained_this_year = ifelse(was_IHC_this_year == 1 & was_IHC_last_year == 1, 1, 0))


### summary tables
# one row per year/ count type
# columns:
#  - year
#  - count type:
#     - number IHCs in system each year
#     - number IHCs lost at end of year
#     - number IHCs retained at end of year
#     - number IHCs recruited new this year
#     - number IHCs recruited back this year


recruitment_retention_summary = IHCs_long %>%
  select(year, recruited_new_this_yr, recruited_back_this_yr, retained_this_year, was_IHC_this_year, lost_next_yr) %>%
  filter(was_IHC_this_year==1) %>%
  pivot_longer(!year, names_to = "category", values_to = "value" ) %>%
  mutate(value = as.numeric(ifelse(is.na(value), 0 , value))) %>%
  group_by(year, category) %>%
  summarise(count = sum(value)) %>%
  ungroup() %>%
  mutate(category2 = factor(case_when(category == "lost_next_yr" ~ "Left at end of season",
                                      category == "recruited_back_this_yr" ~ "Active",
                                      category == "recruited_new_this_yr" ~ "Active",
                                      category == "retained_this_year" ~ "Active",
                                      category == "was_IHC_this_year" ~ "Active",
                                      TRUE ~ NA_character_), 
                            levels = c("Active", "Left at end of season"))) %>%
  mutate(category = factor(case_when(category == "lost_next_yr" ~ "Left at end\n of season",
                                     category == "recruited_back_this_yr" ~ "Recruited back",
                                     category == "recruited_new_this_yr" ~ "Recruited new",
                                     category == "retained_this_year" ~ "Retained",
                                     category == "was_IHC_this_year" ~ "Total during season",
                                     TRUE ~ NA_character_), 
                           levels = c("Recruited back","Recruited new", "Retained","Total during season", "Left at end\n of season"))) %>%
  mutate(count = ifelse(category == "Recruited new" & year == 2008, 0 , count)) 

# final plot
# notes - make this a single graph. Have to do some manual work.
# first geom_bar is associated with the active status, will be stacked by employement status, graphed at year-0.25.
# second geom_bar is associated with left status, will be stacked by employement status, will be graphed at year + 0.25
# labels are only at the year mark 
recruit_ret_p = ggplot() +
  geom_bar(data = recruitment_retention_summary %>% filter(category != "Total during season" & category2 == "Active" & year <= 2018 & year >=2012),
           aes(x=year-0.25, y = count, fill = category), stat = "identity", position = "stack", width = 0.4) +
  geom_bar(data = recruitment_retention_summary %>% filter(category != "Total during season" & category2 == "Left at end of season" & year <= 2018 & year >=2012),
           aes(x=year+0.25, y = count, fill = category), stat = "identity", position = "stack", width = 0.4) +
  scale_x_discrete(limits = c(2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  labs(x = "", y = "Active IHC Members", fill = "Status") 
recruit_ret_p
ggsave(plot = recruit_ret_p, filename = "analysis/outputs/personnel_by_yr_and_leave_status.png",
       height = 2, width = 5.5, units = "in")
       
       
       




# original attempts

recruit_ret_p = ggplot(data = recruitment_retention_summary %>% filter(category != "Total during season" & year <= 2018 & year >=2012)) +
  geom_bar(aes(x=factor(year), y = count, fill = category), stat = "identity", position = "stack") +
  scale_fill_viridis_d() +
  theme_bw() +
  labs(x = "", y = "Active IHC Members", fill = "Employment\nStatus") +
  facet_grid(rows = vars(category2), scales = "free")
recruit_ret_p
ggsave(plot = recruit_ret_p, filename = "analysis/outputs/personnel_by_yr_and_leave_status.png",
       height = 2.5, width = 6, units = "in")


recruit_ret_p = ggplot(data = recruitment_retention_summary %>% filter(category != "Total during season" & year <= 2019)) +
  geom_bar(aes(x=factor(year), y = count, fill = category), stat = "identity", position = "dodge") +
  scale_fill_viridis_d() +
  theme_bw() +
  labs(x = "", y = "Count", fill = "Employment\nStatus", 
       title = "b)"  
       #title = "DELIBERATIVE, PRE-DECISIONAL, FOR INTERNAL COORDINATION ONLY\nInteragency Hotshot Crew Members\nRecruitment and Retention counts by year"
  ) 
recruit_ret_p
ggsave(plot = recruit_ret_p, filename = "graphs/rec_ret_by_yr_v2.png",
       height = 2.5, width = 8, units = "in")

total_by_yr = ggplot(data = recruitment_retention_summary %>% filter(category == "Total during season" & year <= 2019)) +
  geom_bar(aes(x=factor(year), y = count), stat = "identity") +
  labs(x = "", y = "Count", fill = "", 
       title = "a)"  
       #title = "DELIBERATIVE, PRE-DECISIONAL, FOR INTERNAL COORDINATION ONLY\nInteragency Hotshot Crew Members\nPersonnel counts by year"
  ) +
  theme_bw()

library(cowplot)
library(gridExtra)

rec_legend = get_legend(recruit_ret_p)
recruit_ret_p_v2 = ggplot(data = recruitment_retention_summary %>% filter(category != "Total during season" & year <= 2019)) +
  geom_bar(aes(x=factor(year), y = count, fill = category), stat = "identity", position = "dodge") +
  scale_fill_viridis_d() +
  theme_bw() +
  labs(x = "", y = "Count", fill = "", 
       title = "b)"
       #title = "DELIBERATIVE, PRE-DECISIONAL, FOR INTERNAL COORDINATION ONLY\nInteragency Hotshot Crew Members\nRecruitment and Retention counts by year"
  ) +
  theme(legend.position = "none")

ret_and_tot_plot = grid.arrange(total_by_yr,recruit_ret_p_v2,rec_legend,
                                layout_matrix = rbind(c(1,NA),
                                                      c(2,3)),
                                widths=c(3.5,0.85))
ggsave(plot = ret_and_tot_plot, filename = "graphs/personnel_by_yr_and_leave_status.png",
       height = 4.5, width = 6.5)

ggsave(plot = total_by_yr, filename = "graphs/IHC personnel by yr_v2.png",
       height = 4, width = 12)


###################################################
### Models we tested prior to finalizing the spec (reentry and random effects)
### Plus some notes
###################################################


# We tested a plain Anderson-Gill model, a frailty model, a model clustered on crew id (model1),
# a model with random effects for res_id only (basically frailty), a model with random
# effects for both crew and res id (mixed effects, must use coxme). All of those gave 
# near identical estimates of coefficients and standard errors. The specifications with 
# MCEP as the wage measure are shown below

a_g_model = coxph(Surv(year_start,year_end,surv2) ~ 
                    real_wage + days_assigned + cumusum_da + year + agency + GACC,
                  data = reg_dat)
a_g_cluster_crew_id = coxph(Surv(year_start,year_end,surv2) ~ 
                              real_wage + days_assigned + cumusum_da + year + agency + GACC,
                            cluster = crew_id,
                            data = reg_dat)
frailty_model = coxph(Surv(year_start,year_end,surv2) ~ 
                        real_wage + days_assigned + cumusum_da + year + agency + GACC + frailty(res_id),
                      data = reg_dat)
random_effects_res_only = coxme(Surv(year_start,year_end,surv2) ~ 
                                  real_wage + days_assigned + cumusum_da + year + agency + GACC +
                                  (1 | res_id), 
                                data = reg_dat) 
random_effects_res_and_crew = coxme(Surv(year_start,year_end,surv2) ~ 
                                      real_wage + days_assigned + cumusum_da + year + agency + GACC +
                                      (1 | res_id) + (1 | crew_id), 
                                    data = reg_dat) 

# original preferred model (no id=res_id) reentry = Shayne's method, 
# crew and res id as random effects
model1 <- coxme(Surv(year_start,year_end,surv2) ~ 
                  real_competing_wage + days_assigned + cumusum_da + year + agency + GACC +
                  (1 | res_id) + (1 | crew_id), 
                data = reg_dat) 
# save the models, since they now take quite a while to run
saveRDS(model1, "analysis/outputs/model1.rds")

# wage diff instead of competing wage
model2 <- coxme(Surv(year_start,year_end,surv2) ~ 
                  wage_diff + days_assigned + cumusum_da + year + agency + GACC +
                  (1 | res_id) + (1 | crew_id), 
                data = reg_dat) 
saveRDS(model2, "analysis/outputs/model2.rds")

# MCEP rather than competing wage
model3 <- coxme(Surv(year_start,year_end,surv2) ~ 
                  real_wage + days_assigned + cumusum_da + year + agency + GACC + 
                  (1 | res_id) + (1 | crew_id), 
                data = reg_dat) 
saveRDS(model3, "analysis/outputs/model3.rds")



