################################################################################
################################################################################
# (8) Modeling #################################################################
################################################################################
################################################################################


dataset <- readRDS("./analysis/inputs/dataPRD_dataforModeling.rds")

#### covariate explanation ####
#
#
# dataset$Wage = wage by income bin (0 represents lowest 10% paid and 9 represents highest 10% paid)
#
# dataset$AverageDaysAssigned = cumulative days/cumulative years (ie cumulative average of days assigned)
#
# dataset$Year = current year (2012-2018)
#
# dataset$CumulativeDays = cumulative days to date rounded to nearest 100 days
#
# dataset$GACC = the Geographic Area Coordination Center
# 
# dataset$Agency = the federal agency
#
# dataset$CompetingWage = competing wages rounded to nearest $10,000
#
# dataset$surv = final exit is an event (individuals can only have one event)
#
# dataset$surv2 = each year skipped is also an event (individuals can have multiple events)

# Kaplan-Meier non-parametric ###
survfit2(Surv(year_start,year_end,surv2) ~ 1, data = dataset) |>
ggsurvfit(size = 1) +
  add_confidence_interval() +
  add_risktable() +
  #add_quantile(y_value = 0.6, color = "gray50", size = 0.75) +
  #coord_cartesian(xlim = c(0, 8)) +
# update figure labels/titles
labs(
  y = "Percentage Retention",
  title = "Kaplan-Meier survival estimate of time to resignation",
  x = "Total Years as IHC"
) +
  scale_y_continuous(label = scales::percent, 
                     breaks = seq(0, 1, by = 0.25),
                     expand = c(0.015, 0)) +
  scale_x_continuous(breaks = 0:11, 
                     expand = c(0.02, 0))+ 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

#
##
###
#####
# 
####
###
##
#
# Hazard ratio plots
model <- coxph(Surv(year_start,year_end,surv2) ~ CompetingWageDifference + Year+DaysAssigned+Agency+CumulativeDays, data = dataset)#, cluster=dataset$crew_name)
summary(model)

p <- ggforest(model, data = dataset, main = "Hazard Ratio Estimate",
              #cpositions = c(0.05, 0.22, 0.4),
              fontsize = 0.6,
              refLabel = "reference",
              noDigits = 1,
              cpositions = c(0.02, 0.18, 0.4))
p
#ggsave(plot = p, width = 80, height = 120, dpi = 300, filename = "C:\\Users\\magst\\Desktop\\IHCretention\\Figures\\IHCModelFigures\\CompetingWageDifferenceForestPlot2.jpg")
dev.off()

#
##
###
####
# 
####
###
##
#
# Some draft figures of shifting survival curves based

fit <- survfit(Surv(year_start,year_end,surv2)
               ~ CompetingWageDifference, data = dataset)

p <- ggsurvplot(fit, conf.int = FALSE, 
                xlab="Total Years as IHC", 
                ylab="Survival probability",
                ggtheme = theme_survminer(base_size = 10),
                palette = viridis(18),
                color = 'strata',
                legend.title="Competing Wage Difference \n(thousands USD)",
                legend="bottom",
                #legend="none",
                legend.labs = c("0","-30","-25","-20","-15","-10","-5","5","10","15","20","25","30","35","40","45","50","55"),
                size=0.7)
p

fit <- survfit(Surv(year_start,year_end,surv2)
               ~ Wage, data = dataset)

p <- ggsurvplot(fit, conf.int = TRUE, 
                xlab="Total Years as IHC", 
                ylab="Survival probability",
                ggtheme = theme_survminer(base_size = 10),
                palette = viridis(10),
                color = 'strata',
                legend.title="Wage Quantile",
                legend="bottom",
                #legend="none",
                legend.labs = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10"),
                size=0.7)
p

fit <- survfit(Surv(year_start,year_end,surv2)
               ~ CompetingWage, data = dataset)

p <- ggsurvplot(fit, conf.int = TRUE, 
                xlab="Total Years as IHC", 
                ylab="Survival probability",
                ggtheme = theme_survminer(base_size = 10),
                palette = viridis(10),
                color = 'strata',
                legend.title="Competing Wage \n(thousands USD)",
                legend="bottom",
                #legend="none",
                legend.labs = c("20","30","40","50","60","70","80","90","100","110"),
                size=0.7)
p

##
###
####
#####
#####
# End clean figures -- Below is scratch test figures and exploritory modeling 
#####
#####
####
###
##
#




#### Surv single exit ####
m1 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+Year,data = dataset)
summary(m1)
ggforest(m1, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m2 <- coxph(Surv(year_start,year_end,surv) ~ Wage+CumulativeDays+Year,data = dataset)
summary(m2)
ggforest(m2, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m3 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+GACC+Year,data = dataset)
summary(m3)
ggforest(m3, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m4 <- coxph(Surv(year_start,year_end,surv) ~ Wage+CumulativeDays+GACC+Year,data = dataset)
summary(m4)
ggforest(m4, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m5 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+Agency+Year,data = dataset)
summary(m5)
ggforest(m5, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m6 <- coxph(Surv(year_start,year_end,surv) ~ Wage+CumulativeDays+Agency+Year,data = dataset)
summary(m6)
ggforest(m6, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m7 <- coxph(Surv(year_start,year_end,surv) ~ AverageDaysAssigned+GACC+Agency,data = dataset)
summary(m7)
ggforest(m7, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m8 <- coxph(Surv(year_start,year_end,surv) ~ CumulativeDays+GACC+Agency,data = dataset)#
summary(m8)
ggforest(m8, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)



m9 <- coxph(Surv(year_start,year_end,surv) ~ DaysAssigned+Year+Agency, data = dataset)#
summary(m9)
ggforest(m9, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)



m10 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+Year+Agency, data = dataset)#
summary(m10)
ggforest(m10, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m11 <- coxph(Surv(year_start,year_end,surv) ~ Year+CompetingWage+DaysAssigned, data = dataset)#
summary(m11)
ggforest(m11, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)

m12 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+Year+CumulativeDays+Wage, data = dataset)#
summary(m12)
ggforest(m12, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)

m13 <- coxph(Surv(year_start,year_end,surv) ~ Year+cumusum_year+Wage, data = dataset)#
summary(m13)
ggforest(m13, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)



################################################################################

#### Surv multiexit ####
m1 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+AverageDaysAssigned+Year,data = dataset)
summary(m1)
ggforest(m1, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m2 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+CumulativeDays+Year,data = dataset)
summary(m2)
ggforest(m2, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m3 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+AverageDaysAssigned+GACC+Year,data = dataset)
summary(m3)
ggforest(m3, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m4 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+CumulativeDays+GACC+Year,data = dataset)
summary(m4)
ggforest(m4, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m5 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+AverageDaysAssigned+Agency+Year,data = dataset)
summary(m5)
ggforest(m5, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m6 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+CumulativeDays+Agency+Year,data = dataset)
summary(m6)
ggforest(m6, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m7 <- coxph(Surv(year_start,year_end,surv2) ~AverageDaysAssigned+GACC+Agency,data = dataset)
summary(m7)
ggforest(m7, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m8 <- coxph(Surv(year_start,year_end,surv2) ~ CumulativeDays+GACC+Agency,data = dataset)#
summary(m8)
ggforest(m8, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)



m9 <- coxph(Surv(year_start,year_end,surv2) ~ DaysAssigned+Year+Agency, data = dataset)#
summary(m9)
ggforest(m9, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)



m10 <- coxph(Surv(year_start,year_end,surv2) ~ CompetingWage+Year+Agency, data = dataset)#
summary(m10)
ggforest(m10, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m11 <- coxph(Surv(year_start,year_end,surv2) ~ Year+CompetingWage+DaysAssigned, data = dataset)#
summary(m11)
ggforest(m11, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)

m12 <- coxph(Surv(year_start,year_end,surv2) ~ CompetingWage+Year+CumulativeDays+Wage, data = dataset)#
summary(m12)
ggforest(m12, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)

m13 <- coxph(Surv(year_start,year_end,surv2) ~ Year+cumusum_year+Wage, data = dataset)#
summary(m13)
ggforest(m13, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)
################################################################################



#### FOR FOREST PLOT ####
m1 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+Year,data = dataset)
summary(m1)
ggforest(m1, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m2 <- coxph(Surv(year_start,year_end,surv) ~ Wage+CumulativeDays+Year,data = dataset)
summary(m2)
ggforest(m2, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m3 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+GACC+Year,data = dataset)
summary(m3)
ggforest(m3, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m4 <- coxph(Surv(year_start,year_end,surv) ~ Wage+CumulativeDays+GACC+Year,data = dataset)
summary(m4)
ggforest(m4, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m5 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+Agency+Year,data = dataset)
summary(m5)
ggforest(m5, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m6 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+AverageDaysAssigned+Year,data = dataset)
summary(m1)
ggforest(m1, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m7 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+CumulativeDays+Year,data = dataset)
summary(m2)
ggforest(m2, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m8 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+AverageDaysAssigned+GACC+Year,data = dataset)
summary(m3)
ggforest(m3, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m9 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+CumulativeDays+GACC+Year,data = dataset)
summary(m4)
ggforest(m4, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

m10 <- coxph(Surv(year_start,year_end,surv) ~ CompetingWage+AverageDaysAssigned+Agency+Year,data = dataset)
summary(m5)
ggforest(m5, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

#################
#!#!#!


m10 <- coxph(Surv(year_start,year_end,surv2) ~ CompetingWageDifference + Year+DaysAssigned+Agency+CumulativeDays, data = dataset)#, cluster=dataset$crew_name)
summary(m10)
m10

table(m10)

ggforest(m10, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.5,
         refLabel = "reference",
         noDigits = 2)

p <- ggforest(m10, data = dataset, main = "Hazard Ratio Estimate",
              #cpositions = c(0.05, 0.22, 0.4),
              fontsize = 2.6,
              refLabel = "reference",
              noDigits = 2)

ggsave(plot = p, width = 80, height = 120, dpi = 300, filename = "C:\\Users\\magst\\Desktop\\IHCretention\\Figures\\IHCModelFigures\\CompetingWageDifferenceForestPlot2.jpg")
dev.off()

cluster <- cluster(dataset$year)


pred <- predict(m10, data = dataset, type = "survival")



plot(pred$x)
dataset$senior_f_fx_loc_copy <- plyr::round_any(dataset$senior_f_fx_loc_copy, 1000, f = ceiling) 
dataset$med_wageCOPY <- plyr::round_any(dataset$med_wageCOPY, 1000, f = ceiling) 



wagelist <- dataset$senior_f_fx_loc_copy
#wagelist <- dataset$WageDifference

compwagelist <- dataset$med_wageCOPY



datapred <- data.frame(pred,wagelist,compwagelist)

plot(datapred$wagelist, datapred$pred)

#plot(datapred$wagelist, datapred$compwagelist, pch = 16, col = as.factor(datapred$pred),
#    xlim = c(20000,70000))
#library(plotly)

# Data: volcano is provided by plotly


# Plot
#p <- plot_ly(z = volcano, type = "surface")
p <- plot_ly(z=datapred$pred,
             x=datapred$wagelist,
             y=datapred$compwagelist, 
             type = "histogram2d", xaxis = 30000,70000,
             mode="zsmooth", xlab="Own Wage")


p

fig <- plot_ly(
  datapred, x = ~wagelist, y = ~compwagelist,
  color = ~pred, type = "scatter",colorscale = 'heatmap'
)
fig

fig <- plot_ly(z=datapred$pred,
               x=datapred$wagelist,
               y=datapred$compwagelist,
               colors = colorRamp(c("blue", "green")), type = "heatmap", xlim=c(30000,70000))
fig

#fig <- plot_ly(
#  datapred, x = ~wagelist, y = ~compwagelist,
#  z = ~pred, type = "scatter3d", mode="lines+markers"
#)

fig

d<-kde3d(z=datapred$pred,
      x=datapred$wagelist,
      y=datapred$compwagelist, n=40)

plot(d$x,d$z)

contour3d(d$d, exp(-12), d$x/22, d$y/28, d$z/640,
          color = "green", color2 = "gray", scale=FALSE,
          engine = "standard")

fig <- plot_ly(datapred, x = ~wagelist, y = ~compwagelist, text = ~wagelist, 
               type = 'scatter', mode = 'markers',
               marker = list(size = ~wagelist, opacity = 0.5))
fig <- fig %>% layout(title = 'Gender Gap in Earnings per University',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))

fig




fig <- plot_ly(z=datapred$pred,
               x=datapred$wagelist,
               y=datapred$compwagelist,
               colors = colorRamp(c("red", "green")), type = "heatmap")
fig

kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))

fig <- plot_ly(z=datapred$pred,
               x=datapred$wagelist,
               y=datapred$compwagelist, color = datapred$pred)

fig

fig <- plot_ly(
  datapred, x = ~wagelist, y = ~compwagelist,
  color = ~pred
)

fig



rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)


require(akima)
require(rgl)

x = runif(1000)
y = runif(1000)
z = rnorm(1000)
s = interp(x,y,z)
dim(s$z)

surface3d(s$x,s$y,s$z)


fig <- plot_ly(x = datapred$wagelist, y = datapred$compwagelist)
fig2 <- subplot(
  fig %>% add_markers(alpha = 0.2),
  fig %>% add_histogram2d()
)

fig2

p <- ggforest(m10, data = dataset, main = "Hazard Ratio Estimate",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)



ggsave(plot = p, width = 10, height = 14, dpi = 300, filename = "C:\\Users\\magst\\Desktop\\IHCretention\\Figures\\IHCModelFigures\\CompetingWageDifferenceForestPlot.jpg")
dev.off()


dataset$CompetingWage <- as.numeric(as.character(dataset$CompetingWage))
dataset$Wage <- as.numeric(as.character(dataset$Wage))


dataset$WageInteraction <- dataset$CompetingWage * dataset$Wage

dataset$CompetingWage <- as.factor(dataset$CompetingWage)
dataset$Wage <- as.factor(dataset$Wage)

res.cox <- coxph(Surv(year_start,year_end,surv2) ~ Year + Wage + CompetingWage, data=dataset)
summary(res.cox, conf.int = FALSE)

ggforest(res.cox, data = dataset)

################################################################################

res.cox <- coxph(Surv(year_start,year_end,surv2) ~ WageInteraction + Year + Wage + CompetingWage, data=dataset)
res.cox <- coxph(Surv(year_start,year_end,surv2) ~ Year + Wage * CompetingWage, data=dataset)

################################################################################
# Forest Plot Multiple Models ##################################################
################################################################################

library(glue)
library(gtsummary)
library(survival)
library(dplyr)
library(broom)

model1 <- tidy(m1)
model1$HazardRatio <- exp(model1$estimate)
model1$Equation <- "~ Wage+AverageDaysAssigned+Year"

model2 <- tidy(m2)
model2$HazardRatio <- exp(model2$estimate)
model2$Equation <- "~ Wage+CumulativeDays+Year"

model3 <- tidy(m3)
model3$HazardRatio <- exp(model3$estimate)
model3$Equation <- "~ Wage+AverageDaysAssigned+GACC+Year"

model4 <- tidy(m4)
model4$HazardRatio <- exp(model4$estimate)
model4$Equation <- "~ Wage+CumulativeDays+GACC+Year"

model5 <- tidy(m5)
model5$HazardRatio <- exp(model5$estimate)
model5$Equation <- "~ Wage+AverageDaysAssigned+Agency+Year"

model6 <- tidy(m6)
model6$HazardRatio <- exp(model6$estimate)
model6$Equation <- "~ CompetingWage+AverageDaysAssigned+Year"

model7 <- tidy(m7)
model7$HazardRatio <- exp(model7$estimate)
model7$Equation <- "~ CompetingWage+CumulativeDays+Year"

model8 <- tidy(m8)
model8$HazardRatio <- exp(model8$estimate)
model8$Equation <- "~ CompetingWage+AverageDaysAssigned+GACC+Year"

model9 <- tidy(m9)
model9$HazardRatio <- exp(model9$estimate)
model9$Equation <- "~ CompetingWage+CumulativeDays+GACC+Year"

model10 <- tidy(m10)
model10$HazardRatio <- exp(model10$estimate)
model10$Equation <- "~ CompetingWage+AverageDaysAssigned+Agency+Year"


df <- rbind(model1,model2,model3,model4,model5,model6,model7,model8, model9, model10)

df$term

#cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
#          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
#          "#0072B2","#0072B2")
#library(viridis)
#cbp1 <- viridis(10)
cbp1 <- gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL, rev = FALSE)
ggforestplot::forestplot(
  df = df,
  name = term,
  estimate = HazardRatio,
  se = std.error,
  colour = Equation,
  fontsize = 10.6,
  #pvalue = TRUE,
  xlab = "Hazard Ratio (95% CI)",
  xlim = c(-.5,3)
  #cex  = 2,
  #pallette = cbp1
  #psignif = 0.2
  #zero = 9,
  #logodds = TRUE,
  #lines = list( # as many parameters as CI
  #  gpar(lwd = 10), gpar(lwd = 5),
  #  gpar(), gpar(),
  #  gpar(lwd = 200), gpar(lwd = 1)
)+ geom_vline(xintercept = 1)+ geom_vline(xintercept = 3)+
  guides(colour = guide_legend(override.aes = list(size=5)))+ 
  geom_vline(xintercept = 2)+  scale_colour_manual(values=cbp1)+
  theme(legend.position = 'right', legend.direction = "vertical")

#+ scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

#+                                              # Moving ggplot2 legend to the bottom
#  theme(legend.position = "Right")
################################################################################
################################################################################
################################################################################

plot(dfall0$HazardRatio,dfall0$HazardRatio,pch = 19, xlim = c(0,2), ylim=c(0,2), xlab = "ALL events Hazard Ratio", ylab = "Random 50-50 Exits Hazard Ratio")
abline(0,1)


list <- tbl_regression(m1)
t1
###############
################
#################
m1 <- coxph(Surv(year_start,year_end,surv) ~ Wage+AverageDaysAssigned+Year,data = dataset)%>% 
  tbl_regression(exp = TRUE) 
m1+m2

library(broom)
m1 <- coxph(Surv(year_start,year_end,surv2) ~ Wage+AverageDaysAssigned+Year,data = dataset)
summary(m1)
ggforest(m1, data = dataset, main = "Hazard ratio",
         #cpositions = c(0.05, 0.22, 0.4),
         fontsize = 0.6,
         refLabel = "reference",
         noDigits = 2)

model2 <- tidy(m1)
model2$HazardRatio <- exp(model1$estimate)
model2$model <- "Model 1"

plot(model1$estimate,model2$estimate)
abline(0,1)


# Load and attach the package

#devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)

# install.packages("devtools")
#devtools::install_github("NightingaleHealth/ggforestplot")

# Load and attach other useful packages
# install.packages("tidyverse")
library(tidyverse)
library(fpColors)

df <- read.csv("C:\\Users\\magst\\Desktop\\IHCretention\\scratch\\8_31_2022_testmultipleforestplot4.txt", sep="\t", header = TRUE)

# Filter only associations to BMI for the first 30 biomarkers of the example
# dataset
#df <-
#  ggforestplot::df_linear_associations %>%
#  filter(
#    trait == "BMI",
#    dplyr::row_number() <= 30
#  )

# Draw a forestplot of cross-sectional, linear associations
ggforestplot::forestplot(
  df = df,
  name = term,
  estimate = HazardRatio,
  se = std.error,
  colour = model,
  fontsize = 10.6,
  pvalue = TRUE,
  #psignif = 0.2
  #zero = 9,
  #logodds = TRUE,
  #lines = list( # as many parameters as CI
  #  gpar(lwd = 10), gpar(lwd = 5),
  #  gpar(), gpar(),
  #  gpar(lwd = 200), gpar(lwd = 1)
  )
  

#col = fpColors(lines = "#990000", box = "#660000", zero = "darkblue")
  #col = fpColors(box = c("blue", "darkred"))
#)


################################################################################

dataset <- read.csv("C:\\Users\\magst\\Desktop\\IHCretention\\IHCdata_MultipleSkipsANDcontinuous.csv")


dataset$Wage <- dataset$senior_f_fx_loc
dataset$Wage <- as.factor(dataset$Wage)
dataset$Wage <- as.factor(dataset$Wage)
dataset$Year <- as.factor(dataset$year)
dataset$AverageDaysAssigned <- as.factor(dataset$AverageDaysAssignedROUND)
dataset$GACC <- dataset$gacc_x
#dataset$Agency <- dataset$agency
dataset$Agency <- factor(dataset$agency.x, levels=c('USFS', 'NPS', 'BLM', 'BIA'))
dataset$CumulativeDays <- dataset$cumulativedaysROUND
dataset$DaysAssigned <- factor(dataset$days_assigned)
dataset$CompetingWage <- factor(dataset$med_wage)
dataset$cumusum_year <- factor(dataset$cumusum_year, levels=c('11','10','9','8','7','6','5','4','3','2','1'))
dataset$total_years <- factor(dataset$total_years)
# for multiple skips data
dataset$year_start <- dataset$year_start.x
dataset$year_end <- dataset$year_end.x 


m1 <- coxph(Surv(year_start,year_end,TRUESURV) ~ Wage+AverageDaysAssigned+Year,data = dataset)
summary(m1)
#summary <- table(summary)
#write.csv(summary,"C:\\Users\\magst\\Desktop\\IHCretention\\scratch\\M1.csv")
p <- ggforest(m1, data = dataset, main = "Hazard ratio",
              #cpositions = c(0.05, 0.22, 0.4),
              fontsize = 0.6,
              refLabel = "reference",
              noDigits = 2)

plot(p)




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# NOT RUN {
data <- read.table(textConnection('
id               group pe       ci.l ci.u style      value.A    value.B 
"Study 1"  1         0.35 0.08 0.92 "normal" "2/46"     "7/46" 
"Study 2"  1         0.43 0.15 1.14 "normal" "4/50"     "8/49" 
"Study 3"  2         0.31 0.07 0.74 "normal" "2/97"     "10/100"
"Study 4"  2         0.86 0.34 2.90 "normal" "9/104"    "6/105" 
"Study 5"  2         0.33 0.10 0.72 "normal" "4/74"     "14/74" 
"Study 6"  2         0.47 0.23 0.91 "normal" "11/120" "22/129"
"Pooled"     NA      0.42 0.15 1.04 "pooled" NA             NA 
'), header=TRUE)
data
data$pe <- log(data$pe)
data$ci.l <- log(data$ci.l)
data$ci.u <- log(data$ci.u)

blobbogram(data, group.labels=c('GROUP 1', 'GROUP 2'),
           columns=c('value.A', 'value.B'), column.labels=c('r/n', 'r/n'),
           column.groups=c(1, 2), grouped=TRUE,
           column.group.labels=c('Intervention', 'Control'),
           id.label="Trial", ci.label="Odds Ratio (95% CrI)", log.scale=TRUE)
# }


################################################################################
################################################################################
library(survival)

surv_object = Surv(time = dataset$total_hours,
                   #time2 = timedata2,
                   event = dataset$surv)
# Regress on a constant
fit <- survfit(surv_object ~ 1)
# Plot the fit
ggsurvplot(fit, 
           data.frame(time=total_hours, event=dataset), 
           conf.int=FALSE)




fit <- survfit(Surv(year_start,year_end,surv)~1, data=dataset)

ggsurvplot(fit, data = lung)


dataset$cumusum_year <- as.numeric((dataset$cumusum_year))
fit <- survfit(Surv(cumusum_year,surv)
              ~ 1, data = dataset)
class(fit)

library("survminer")
ggsurvplot(fit, data = dataset)


library(ggsurvfit)



survfit2(Surv(year_start,year_end,surv) ~ 1, data = dataset) %>% 
  ggsurvfit()
) + 
  add_confidence_interval()



survfit2(Surv(year_start,year_end,surv) ~ 1, data = dataset) |>
  # build Kaplan-Meier plot ----------------------------------------------------
  ggsurvfit(size = 1) +
  add_confidence_interval() +
  add_risktable() +
  #add_quantile(y_value = 0.6, color = "gray50", size = 0.75) +
  # use ggplot2 functions to style the plot and update the labels --------------
# limit plot to show 8 years and less
  #coord_cartesian(xlim = c(0, 8)) +
  # update figure labels/titles
  labs(
    y = "Percentage Retention",
    title = "Kaplan-Meier survival estimate of time to resignation",
    x = "Total Years as IHC"
  ) +
  # reduce padding on edges of figure and format axes
  scale_y_continuous(label = scales::percent, 
                     breaks = seq(0, 1, by = 0.25),
                     expand = c(0.015, 0)) +
  scale_x_continuous(breaks = 0:11, 
                     expand = c(0.02, 0))+ 
  theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change axis line
  axis.line = element_line(colour = "black")
)


  