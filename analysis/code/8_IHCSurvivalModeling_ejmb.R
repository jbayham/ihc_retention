################################################################################
################################################################################
# (8) Modeling #################################################################
################################################################################
################################################################################

# run project_init first to get all libraries loaded
# libraries below are not included in project_init
library(patchwork)
library(coxed)

source("functions/helper_functions.R") #This loads a helper function for the package used to generate tables below.


################################################################
# import data
################################################################

dataset <- readRDS("./analysis/inputs/dataPRD_dataforModeling.rds")

reg_dat <- dataset %>%
  select(res_id,crew_name,state_name,fips,year_start,year_end,surv,surv2,
         real_wage,real_competing_wage,days_assigned,cumusum_da,year,agency,GACC) %>%
  mutate(fips=str_pad(fips,5,"left","0"),
         wage_diff=real_wage-real_competing_wage,  #Compute difference between own and competing wage
         cumusum_da=cumusum_da/100,
         across(contains("wage"),~./1000),
         across(c(agency,GACC,crew_name),factor),
         agency=relevel(agency,ref="USFS"),
         GACC=relevel(GACC,ref = "CA-OSCC"),
         wage_diff_scaled = scale(wage_diff)) %>%
  filter(abs(wage_diff_scaled)<3) %>% #Remove obs >3 sd from mean 
  # for crew name model, need a crew id that is completely de-identified
  left_join(tibble(crew_name = unique(dataset$crew_name)) %>%
              mutate(crew_id = str_c("crew_",row_number())), 
            by = c("crew_name" = "crew_name"))

################################################################
# summary stats
################################################################

library(stargazer)
dat_fewer = reg_dat %>% select(days_assigned, cumusum_da, real_competing_wage, real_wage, wage_diff, year,agency, GACC)
stargazer(dat_fewer, type = "latex", 
          covariate.labels = c("Days Assigned", "Cumulative Experience (100 days)", "Competing Wage ($1000)",
                               "Max Crew Earning Potential ($1000)", "MCEP - Competing Wage ($1000)"),
          summary.stat = c("min", "p25", "mean", "median", "p75", "max"))

# data by year
yr_dat = reg_dat %>% select(year) %>% group_by(year) %>% summarise(total_observations = n()) %>% arrange(year) %>%
  left_join(reg_dat %>% filter(surv2 == 1) %>% select(year) %>% group_by(year) %>% summarise(number_leave_events = n()) %>% arrange(year),
            by = c("year" = "year")) %>%
  left_join(reg_dat %>% filter(surv2 == 0) %>% select(year) %>% group_by(year) %>% summarise(number_stay_events = n()) %>% arrange(year),
            by = c("year" = "year")) %>%
  left_join(reg_dat %>% select(days_assigned, year) %>% group_by(year) %>% summarise(med_days_assigned = median(days_assigned)),
            by = c("year" = "year")) %>%
  left_join(reg_dat %>% select(cumusum_da, year) %>% group_by(year) %>% summarise(med_cumusum_da = median(cumusum_da)),
            by = c("year" = "year")) %>%
  left_join(reg_dat %>% select(real_competing_wage, year) %>% group_by(year) %>% summarise(med_real_competing_wage = median(real_competing_wage)),
            by = c("year" = "year")) %>%
  left_join(reg_dat %>% select(real_wage, year) %>% group_by(year) %>% summarise(med_real_wage = median(real_wage)),
            by = c("year" = "year")) %>%
  left_join(reg_dat %>% distinct(year, crew_id) %>% select(year) %>% group_by(year) %>% summarise(number_crews_represented = n()),
            by = c("year" = "year")) %>%
  #remove_rownames() %>% column_to_rownames(var="year") %>%
  pivot_longer(cols= -1) %>% pivot_wider(names_from = "year",values_from = "value") %>% rename("Statistic" = "name") %>%
  mutate(Statistic = case_when(Statistic == "total_observations" ~ "Total Observations",
                               Statistic == "number_crews_represented" ~ "Number Crews Represented",
                               Statistic == "number_leave_events" ~ "Number Leave Events",
                               Statistic == "number_stay_events" ~ "Number Stay Events",
                               Statistic == "med_days_assigned" ~ "Median Days Assigned",
                               Statistic == "med_cumusum_da" ~ "Median Cumulative Experience (100 days)",
                               Statistic == "med_real_competing_wage" ~ "Median Competing Wage ($1000)",
                               Statistic == "med_real_wage" ~ "Median MCEP ($1000)",
                               TRUE ~ ""))

library(xtable)
xtable(yr_dat)

# number individuals in data set
length(unique(reg_dat$res_id))
# number crews in data set
length(unique(reg_dat$crew_id))
################################################################
# Estimate baseline Kaplan-Meier curve
################################################################

km_baseline_model <- survfit2(Surv(year_start,year_end,surv2) ~ 1, data = reg_dat) 

km_baseline = ggsurvplot(km_baseline_model, data = reg_dat,
                         conf.int = TRUE, 
                         conf.int.alpha = 0.6,
                         xlim = c(0,11), 
                         break.x.by = 1,
                         xlab="Total Years as IHC", 
                         ylab="Retention Probability",
                         ggtheme = theme_survminer(base_size = 10),
                         legend = "none",
                         size=1,
                         palette = c("black"),
                         linetype = c(1))
km_baseline

ggsave(plot = km_baseline$plot, filename = "analysis/outputs/baseline_km.png",
       height = 3, width = 6, unit = "in")


# get numbers in text:
# median survival
km_med = ggsurvplot(km_baseline_model, data = reg_dat,
                         conf.int = TRUE, 
                         conf.int.alpha = 0.6,
                         xlim = c(0,11), 
                         break.x.by = 1,
                         xlab="Total Years as IHC", 
                         ylab="Retention Probability",
                         ggtheme = theme_survminer(base_size = 10),
                         legend = "none",
                         size=1,
                         palette = c("black"),
                         linetype = c(1),
                         surv.median.line = "hv")
km_med

# can also see median retention in the summary
km_baseline_model

# median retention is 2 yrs

# look at percentages of specific years
km_baseline_model$surv
km_baseline_model$lower
km_baseline_model$upper

################################################################
#Estimating the Cox_Proportional Hazard models with levels
################################################################

rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
                        'Empty row', '-', '-',
                        'Another empty row', '?', '?')

rows <- tibble::tribble(~term, ~M1, ~M2, ~M3,
                        'Fixed Effects', '', '', '',
                        'Agency', 'x', 'x', 'x',
                        'GACC', 'x', 'x', 'x',
                        'Crew', '', '', 'x',)

attr(rows, 'position') <- c(21:24)


model1 <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat,cluster=crew_name) 

model2 <- coxph(Surv(year_start,year_end,surv2) ~ wage_diff + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat, cluster=crew_name)

model3 <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC + crew_id, 
                data = reg_dat, cluster=crew_name)

# model4 <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + cumusum_da + year + agency + GACC, 
#                 data = reg_dat, cluster=crew_name)
# 
# model5 <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da+ year + agency + GACC + crew_name, 
#                 data = reg_dat, cluster=crew_name)


model_list <- list(`1`=model1,`2`=model2,`3`=model3)

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
             notes = 'Standard errors are clustered at the IH Crew level to account for correlation between crew members.',
             output = "analysis/outputs/main_result_tab.tex")


modelsummary(model_list,
             #coef_omit = "crew_name|GACC|year|agency",
             #coef_map = c("days_assigned"="Days Assigned","cumusum_da"="Cumulative Experience (days)",
            #              "real_competing_wage"="Competing Wage ($1000)","real_wage"="FF Wage ($1000)","wage_diff"="FF-Competing Wage ($1000)",
            #              "year2013"="Year 2013","year2014"="Year 2014","year2015"="Year 2015","year2016"="Year 2016","year2017"="Year 2017","year2018"="Year 2018"),
             stars = T,
             title = "Cox PH regression results \\label{tab:main_results_supp}",
             statistic = "robust.se",
             #gof_omit = "AIC|RMSE",
             #add_rows = rows,
             notes = 'Standard errors are clustered at the IH Crew level to account for correlation between crew members.',
             output = "analysis/outputs/main_result_tab_full_coef_for_supp_mat.tex")



# get etimates to compare median years of retention at different levels:
#Setting new data
new_dat <- as_tibble(model.matrix(model1)) %>%
  dplyr::summarize(across(1:3,median)) %>%
  add_column(year=2015,agency="USFS",GACC="CA-OSCC") %>%
  mutate(across(c(year,agency,GACC),factor))

md1 <- coxed(model1,newdata = new_dat, method="npsf",id="res_id")
da_med <- summary(md1, stat="median")

#Days assigned
new_dat_90 <- new_dat %>%
  mutate(days_assigned=quantile(as.data.frame(model.matrix(model1))$days_assigned,prob=.9)) #setting to the 90th percentile
md1_90 <- coxed(model1, newdata = new_dat_90, method="npsf",id="res_id")
da_90 <- summary(md1_90, stat="median")

da_pc <- (da_90-da_med)/da_med
da_med; da_90; da_pc

#Cumulative workload
new_dat_90 <- new_dat %>%
  mutate(cumusum_da=quantile(as.data.frame(model.matrix(model1))$cumusum_da,prob=.9)) #setting to the 90th percentile
md1_90 <- coxed(model1, newdata = new_dat_90, method="npsf",id="res_id")
da_90 <- summary(md1_90, stat="median")

da_pc <- (da_90-da_med)/da_med
da_med; da_90; da_pc

#Competing wages (90th percentile)
new_dat_90 <- new_dat %>%
  mutate(real_competing_wage=quantile(as.data.frame(model.matrix(model1))$real_competing_wage,prob=.9)) #setting to the 90th percentile
md1_90 <- coxed(model1, newdata = new_dat_90, method="npsf",id="res_id")
da_90 <- summary(md1_90, stat="median")

da_pc <- (da_90-da_med)/da_med
da_med; da_90; da_pc

#Competing wages (middle of top bin = $90,000 -> 90)
new_dat_90 <- new_dat %>%
  mutate(real_competing_wage=90) #setting to the 90th percentile
md1_90 <- coxed(model1, newdata = new_dat_90, method="npsf",id="res_id")
da_90 <- summary(md1_90, stat="median")

da_pc <- (da_90-da_med)/da_med
da_med; da_90; da_pc

# competing wages - compare bins - 90 to 20 (need to have just run the above code chunk to have da_90)
new_dat_20 <- new_dat %>%
  mutate(real_competing_wage=20) #setting to the 90th percentile
md1_20 <- coxed(model1, newdata = new_dat_20, method="npsf",id="res_id")
da_20 <- summary(md1_20, stat="median")

da_pc <- (da_90-da_20)/da_20
da_20; da_90; da_pc



################################################################
# Estimating the Cox_Proportional Hazard model with binned/level coefficients 
################################################################

# using model1 as the base
# binning the coefficients separately (one model per each binned/level variable)
# to ensure other coefficients stay consistent

reg_dat_binned <- reg_dat %>%
  mutate(cumusum_da_b = quantcut(cumusum_da,q=10),
         real_competing_wage_b = quantcut(real_competing_wage,q=10),
         days_assigned_b = quantcut(days_assigned,q=10),
         real_competing_wage_r = cut_width(real_competing_wage,width=10))


model1_cumsum_binned <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da_b + year + agency + GACC, 
                data = reg_dat_binned, cluster=crew_name) 

model1_days_assigned_binned <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned_b + cumusum_da + year + agency + GACC, 
                                     data = reg_dat_binned, cluster=crew_name) 

model1_comp_wg_round <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage_r + days_assigned + cumusum_da + year + agency + GACC, 
                               data = reg_dat_binned, cluster=crew_name) 

# extract binned coefficients, associated linear coefficients

model_list_bin <- list(`Cumulative Experience`=model1_cumsum_binned,
                       `Days Assigned`=model1_days_assigned_binned,
                       `Competing Wage`=model1_comp_wg_round)


# make a table of coef, p-value, exp(coef), exp(upp.95), epx(lower.95) by model for graphing
i=1
for(m in model_list_bin){
  mod_name = names(model_list_bin)[i]
  coef_names = rownames(exp(confint(m)))
  exp_lower.95 = exp(confint(m))[,1]
  exp_upper.95 = exp(confint(m))[,2]
  exp_coef = exp(m$coefficients)
  coef = m$coefficients
  p_value = summary(m)$coefficients[,6]
  
  est_tbl_tmp = tibble(coef_names, coef, p_value, exp_coef, exp_upper.95, exp_lower.95, mod_name = rep(mod_name, times = length(coef)))
  if(i==1){est_tbl = est_tbl_tmp} else{est_tbl = bind_rows(est_tbl,est_tbl_tmp)}
  i = i+1
}

##############
# Graph decile values for faceted plot

est_tbl_graph_dec = est_tbl %>%
  # only want the binned/rounded coef
  filter(grepl('_b|_r',coef_names)) %>%
  mutate(bin_widths = str_replace(coef_names, "cumusum_da_b\\(|days_assigned_b\\(|real_competing_wage_r\\(", "")) %>%
  mutate(bin_widths = str_replace(bin_widths, "\\]", "")) %>%
  mutate(bin_widths = str_replace(bin_widths, ",", "-")) %>%
  # order factors 
  mutate(floor_of_bin = str_replace(bin_widths, "-.*", "")) 
  

deciles_cumusum = ggplot(data = est_tbl_graph_dec %>% filter(mod_name == "Cumulative Experience") %>%
                           mutate(bin_widths = factor(bin_widths, 
                                                      levels =c("0.64-0.89","0.89-1.13","1.13-1.61","1.61-2.06",
                                                                "2.06-2.66","2.66-3.32","3.32-4.16","4.16-5.39","5.39-7.98")))) +
  geom_segment(aes(x = bin_widths, y = exp_upper.95, xend = bin_widths, yend = exp_lower.95), size = 1.25) +
  #add heavy line at 1+
  geom_segment(aes(x = 0, xend = 10, y = 1, yend=1), color = "gray60") +
  geom_point(aes(x=bin_widths, y = exp_coef), size = 2) +
  #facet_grid(cols=vars(mod_name)) +
  ggtitle("(b)") +
  theme_bw() +
  coord_flip() +
  labs(x="Days of experience in 100 days\n(deciles)", y = "Hazard Ratio Estimate") +
  theme(plot.title = element_text(size = 16, face = "bold"))
deciles_cumusum

deciles_days = ggplot(data = est_tbl_graph_dec %>% filter(mod_name == "Days Assigned") %>%
                           mutate(bin_widths = factor(bin_widths, levels =c("45-58","58-67","67-75","75-82","82-89","89-94","94-100","100-110","110-154")))) +
  geom_segment(aes(x = bin_widths, y = exp_upper.95, xend = bin_widths, yend = exp_lower.95), size = 1.25) +
  #add heavy line at 1+
  geom_segment(aes(x = 0, xend = 10, y = 1, yend=1), color = "gray60") +
  geom_point(aes(x=bin_widths, y = exp_coef), size = 2) +
  #facet_grid(cols=vars(mod_name)) +
  #ggtitle("Days Assigned") +
  ggtitle("(a)") +
  theme_bw() +
  coord_flip() +
  labs(x="Days on assignment\n(deciles)", y = "Hazard Ratio Estimate") +
  theme(plot.title = element_text(size = 16, face = "bold"))

deciles_days

levels_comp_wage = ggplot(data = est_tbl_graph_dec %>% filter(mod_name == "Competing Wage") %>%
                        mutate(bin_widths = factor(bin_widths, levels =c("25-35","35-45","45-55","55-65","65-75","75-85","85-95")))) +
  geom_segment(aes(x = bin_widths, y = exp_upper.95, xend = bin_widths, yend = exp_lower.95), size = 1.25) +
  #add heavy line at 1+
  geom_segment(aes(x = 0, xend = 8, y = 1, yend=1), color = "gray60") +
  geom_point(aes(x=bin_widths, y = exp_coef), size = 2) +
  #facet_grid(cols=vars(mod_name)) +
  #ggtitle("Competing Wage") +
  ggtitle("(c)") +
  theme_bw() +
  coord_flip() +
  labs(x="Cmpeteing Wage in $1000 \n(10K intervals)", y = "Hazard Ratio Estimate") +
  theme(plot.title = element_text(size = 16, face = "bold"))

levels_comp_wage


# combine all three plots
# occassionally this line gets mad - just re-load the patchwork library
all_three_p = deciles_days + deciles_cumusum + levels_comp_wage

all_three_p

ggsave(plot = all_three_p, filename = "analysis/outputs/deciles_p.png", height = 3.5, width = 9.25, units = "in")

################################################################
# predict using model 1 (shifted KM curves)
################################################################

# get median, and 90th percentile values (skip 10 for now)
quantile(reg_dat$real_competing_wage, 0.5)
real_comp_wg_50 = 47.81647 
quantile(reg_dat$real_competing_wage, 0.975)
real_comp_wg_90 = 65.83317
real_comp_wage_50k = 50
real_comp_wage_90k = 90
  
quantile(reg_dat$days_assigned, 0.5)
days_assigned_50 = 82
quantile(reg_dat$days_assigned, 0.90)
days_assigned_90 = 110


quantile(reg_dat$cumusum_da, 0.5)
cumusum_da_50 = 2.06
quantile(reg_dat$cumusum_da, 0.9)
cumusum_da_90 = 5.39


## make sure we're working with the correct cox PH model
model1 <- coxph(Surv(year_start,year_end,surv2) ~ real_competing_wage + days_assigned + cumusum_da + year + agency + GACC, 
                data = reg_dat,cluster=crew_name) 
model1


# shifted KM1 everything at baseline except real_comp_wage
# two versions of this - one with percentiles and one with 50k and 90k

##### Percentiles!!################
# we construct a new data frame with two rows, 
# one row for each value of the variable of interest; the other covariates are fixed to their median values 
# Create the new data  

comp_wg_df_p = with(reg_dat,
              data.frame(real_competing_wage = c(real_comp_wg_50,real_comp_wg_90),
                         days_assigned = rep(days_assigned_50,2),
                         cumusum_da = rep(cumusum_da_50,2),
                         year = factor(rep(2015, 2),levels = c(2012,2013,2014,2015,2016,2017,2018)),
                         agency = rep("USFS", 2),
                         GACC = rep("CA-ONCC", 2)
              ))

fit_comp_wg_p <- survfit(model1, newdata = comp_wg_df_p, data = reg_dat)
shifted_curves_comp_wg_p = ggsurvplot(fit_comp_wg_p, 
           xlim = c(0,11), 
           break.x.by = 1,
           conf.int = TRUE, 
           xlab="Total Years as IHC", 
           ylab="Retention Probability",
           ggtheme = theme_survminer(base_size = 10),
           linetype = c(1,6),
           palette = c("#006e90", "#fe7f2d"),
           color = 'strata',
           legend.title="Competing Wage \n(thousands USD)",
           #legend="bottom",
           #legend="none",
           legend = c(0.7,.6),
           legend.labs = c("50th percentile","90th percentile"),
           size=1, 
           title = "(c)",
           font.title = c(18, "bold")) 
shifted_curves_comp_wg_p

# shifted KM2 everything at baseline except days_assigned

days_assigned_df_p = with(reg_dat,
                        data.frame(real_competing_wage = rep(real_comp_wg_50,2),
                                   days_assigned = c(days_assigned_50,days_assigned_90),
                                   cumusum_da = rep(cumusum_da_50,2),
                                   year = factor(rep(2015, 2),levels = c(2012,2013,2014,2015,2016,2017,2018)),
                                   agency = rep("USFS", 2),
                                   GACC = rep("CA-ONCC", 2)
                        ))

fit_days_assigned_p <- survfit(model1, newdata = days_assigned_df_p, data = reg_dat)
shifted_curves_days_assigned_p = ggsurvplot(fit_days_assigned_p, 
                                    xlim = c(0,11), 
                                    break.x.by = 1,
                                    conf.int = TRUE, 
                                    xlab="Total Years as IHC", 
                                    ylab="Retention Probability",
                                    ggtheme = theme_survminer(base_size = 10),
                                    linetype = c(1,6),
                                    palette = c("#006e90", "#fe7f2d"),
                                    color = 'strata',
                                    legend.title="Days Assigned",
                                    #legend="bottom",
                                    #legend="none",
                                    legend = c(0.7,.6),
                                    legend.labs = c("50th percentile","90th percentile"),
                                    size=1,
                                    title = "(a)",
                                    font.title = c(18, "bold"))
shifted_curves_days_assigned_p

# shifted KM3 everything at baseline except cumusum days
cumusum_da_df_p = with(reg_dat,
                     data.frame(real_competing_wage = rep(real_comp_wg_50,2),
                                days_assigned = rep(days_assigned_50,2),
                                cumusum_da = c(cumusum_da_50,cumusum_da_90),
                                year = factor(rep(2015, 2),levels = c(2012,2013,2014,2015,2016,2017,2018)),
                                agency = rep("USFS", 2),
                                GACC = rep("CA-ONCC", 2)
                     ))

fit_cumusum_da_p <- survfit(model1, newdata = cumusum_da_df_p, data = reg_dat)
shifted_curves_cumusum_da_p = ggsurvplot(fit_cumusum_da_p, 
                                          xlim = c(0,11), 
                                          break.x.by = 1,
                                          conf.int = TRUE, 
                                          xlab="Total Years as IHC", 
                                          ylab="Retention Probability",
                                          ggtheme = theme_survminer(base_size = 10),
                                          linetype = c(1,6),
                                          palette = c("#006e90", "#fe7f2d"),
                                          color = 'strata',
                                          legend.title="Cumulative Experience",
                                          #legend="bottom",
                                          #legend="none",
                                          legend = c(0.7,.6),
                                          legend.labs = c("50th percentile","90th percentile"),
                                          size=1,
                                       title = "(b)",
                                       font.title = c(18, "bold"))
shifted_curves_cumusum_da_p



ggsave(plot = (shifted_curves_days_assigned_p$plot + shifted_curves_cumusum_da_p$plot + shifted_curves_comp_wg_p$plot),
       filename = "analysis/outputs/shifted_km_percentiles.png",
       height = 3.5, width = 11, units = "in")



##### Raw Values!!################
# we construct a new data frame with two rows, 
# one row for each value of the variable of interest; the other covariates are fixed to their median values 
# Create the new data  

real_comp_wage_50k = 50
real_comp_wage_90k = 90

comp_wg_df_r = with(reg_dat,
                    data.frame(real_competing_wage = c(real_comp_wage_50k,real_comp_wage_90k),
                               days_assigned = rep(days_assigned_50,2),
                               cumusum_da = rep(cumusum_da_50,2),
                               year = factor(rep(2015, 2),levels = c(2012,2013,2014,2015,2016,2017,2018)),
                               agency = rep("USFS", 2),
                               GACC = rep("CA-ONCC", 2)
                    ))

fit_comp_wg_r <- survfit(model1, newdata = comp_wg_df_r, data = reg_dat)
shifted_curves_comp_wg_r = ggsurvplot(fit_comp_wg_r, 
                                      xlim = c(0,11), 
                                      break.x.by = 1,
                                      conf.int = TRUE, 
                                      xlab="Total Years as IHC", 
                                      ylab="Retention Probability",
                                      ggtheme = theme_survminer(base_size = 10),
                                      linetype = c(1,6),
                                      palette = c("#006e90", "#fe7f2d"),
                                      color = 'strata',
                                      legend.title="Competing Wage \n(USD)",
                                      #legend="bottom",
                                      #legend="none",
                                      legend = c(0.7,.6),
                                      legend.labs = c("$50,000","$90,000"),
                                      size=1, 
                                      title = "(c)",
                                      font.title = c(18, "bold"))
shifted_curves_comp_wg_r

# shifted KM2 everything at baseline except days_assigned


days_assigned_df_r = with(reg_dat,
                          data.frame(real_competing_wage = rep(real_comp_wg_50,2),
                                     days_assigned = c(days_assigned_50,days_assigned_90),
                                     cumusum_da = rep(cumusum_da_50,2),
                                     year = factor(rep(2015, 2),levels = c(2012,2013,2014,2015,2016,2017,2018)),
                                     agency = rep("USFS", 2),
                                     GACC = rep("CA-ONCC", 2)
                          ))

fit_days_assigned_r <- survfit(model1, newdata = days_assigned_df_r, data = reg_dat)
shifted_curves_days_assigned_r = ggsurvplot(fit_days_assigned_r, 
                                            xlim = c(0,11), 
                                            break.x.by = 1,
                                            conf.int = TRUE, 
                                            xlab="Total Years as IHC", 
                                            ylab="Retention Probability",
                                            ggtheme = theme_survminer(base_size = 10),
                                            linetype = c(1,6),
                                            palette = c("#006e90", "#fe7f2d"),
                                            color = 'strata',
                                            legend.title="Days Assigned",
                                            #legend="bottom",
                                            #legend="none",
                                            legend = c(0.7,.6),
                                            legend.labs = c("82 days","110 days"),
                                            size=1,
                                            title = "(a)",
                                            font.title = c(18, "bold"))
shifted_curves_days_assigned_r

# shifted KM3 everything at baseline except cumusum days


cumusum_da_df_r = with(reg_dat,
                       data.frame(real_competing_wage = rep(real_comp_wg_50,2),
                                  days_assigned = rep(days_assigned_50,2),
                                  cumusum_da = c(cumusum_da_50,cumusum_da_90),
                                  year = factor(rep(2015, 2),levels = c(2012,2013,2014,2015,2016,2017,2018)),
                                  agency = rep("USFS", 2),
                                  GACC = rep("CA-ONCC", 2)
                       ))

fit_cumusum_da_r <- survfit(model1, newdata = cumusum_da_df_r, data = reg_dat)
shifted_curves_cumusum_da_r = ggsurvplot(fit_cumusum_da_r, 
                                         xlim = c(0,11), 
                                         break.x.by = 1,
                                         conf.int = TRUE, 
                                         xlab="Total Years as IHC", 
                                         ylab="Retention Probability",
                                         ggtheme = theme_survminer(base_size = 10),
                                         linetype = c(1,6),
                                         palette = c("#006e90", "#fe7f2d"),
                                         color = 'strata',
                                         legend.title="Cumulative Experience\n100 days",
                                         #legend="bottom",
                                         #legend="none",
                                         legend = c(0.7,.6),
                                         legend.labs = c("2.06 days","5.39 days"),
                                         size=1,
                                         title = "(b)",
                                         font.title = c(18, "bold"))
shifted_curves_cumusum_da_r




ggsave(plot = (shifted_curves_days_assigned_r$plot + shifted_curves_cumusum_da_r$plot + shifted_curves_comp_wg_r$plot),
       filename = "analysis/outputs/shifted_km_raw.png",
       height = 3.5, width = 11, units = "in")

ggsave(plot = (shifted_curves_days_assigned_p$plot + shifted_curves_cumusum_da_p$plot + shifted_curves_comp_wg_r$plot),
       filename = "analysis/outputs/shifted_km_mix.png",
       height = 3.5, width = 11, units = "in")





  