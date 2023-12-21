# R code for replicating the dataset and the analyses in the paper #
# The effect of Withania somnifera (Ashwagandha) on mental illness: a systematic review and meta-analysis #
# R code by Mattia Marchi (mattiamarchimd@gmail.com) 
# December 21, 2023
###------------------------------------------------------------------------------------------------------
###------------------------Meta Analysis - Ashwagandha in psychiatry disorders---------------------------
###------------------------------------------------------------------------------------------------------
#Load required packages
library(meta)
library(metafor)
library(tidyverse)
library(metaforest)
library(dmetar)

#Import data
df <- structure(list(ID = structure(1:13, levels = c("Andrade et al, 2000", "Chengappa et al, 2013", "Chengappa et al, 2018", "Choudhary et al, 2017", 
                                                     "Cooley et al, 2009", "Fuladi et al, 2021", "Fulzele et al, 2014", "Hosseini et al, 2019",
                                                     "Jahanbakhsh et al, 2016", "Khyati et al, 2013", "Langade et al, 2019", "Langade et al, 2021",
                                                     "Majeed et al, 2023"), class = "factor"),
                     Country = structure(c(2L, 4L, 4L, 2L, 1L, 3L, 2L, 3L, 3L, 2L, 2L, 2L, 2L), levels = c("Canada", "India", "Iran", "USA"), class = "factor"),
                     Study.design = structure(c(2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L), levels = c("Parallel RCT", "RCT"), class = "factor"),
                     Setting = structure(c(2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L), levels = c("NR", "Outpatients clinic"), class = "factor"),
                     Diagnosis_cat = structure(c(2L, 3L, 8L, 4L, 2L, 2L, 6L, 1L, 7L, 2L, 5L, 5L, 2L), levels = c("ADHD", "Anxiety", "BD", "Chronic stress", "Insomnia", "MDD", "OCD", "SZ"), class = "factor"), 
                     Diagnosis.tool = structure(c(6L, 7L, 7L, 8L, 4L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 5L), levels = c("DSM 5", "DSM-IV", "DSM-IV-TR", "DSM-IV, Beck Anxiety Inventory (BAI)", "HDRS/HARS", "ICD-10", 
                                                                                                                  "Mini-International Neuropsychiatric Interview (MINI)-DSM IV", "Perceived stress scale (PSS)"), class = "factor"),
                     Duration_weeks = c(6L, 8L, 12L, 8L, 12L, 6L, 6L, 6L, 6L, 8L, 10L, 8L, 12L), T_dose = c(500L, 500L, 1000L, 600L, 600L, 1000L, NA, 10L, 1000L, 12000L, 600L, 600L, 500L),
                     T = structure(c(4L, 4L, 2L, 5L, 5L, 2L, 6L, 1L, 2L, 3L, 5L, 5L, 4L), levels = c("Withania somnifera 10 mg po daily", "Withania somnifera 1000 mg po daily", "Withania somnifera 12000 mg po daily", "Withania somnifera 500 mg po daily",
                                                                                                     "Withania somnifera 600 mg po daily", "Withania somnifera po daily"), class = "factor"),
                     C = structure(c(2L,  2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("CBT+PBO", "PBO"), class = "factor"),
                     Use = structure(c(1L, 1L, 1L,  2L, 2L, 1L, 1L, 1L, 1L, NA, NA, 2L, NA), levels = c("Add-on", "Sole"), class = "factor"),
                     X.Females = c(38.5, 61.7, 48.5, 26.9, 63, 45, NA, 39.3, 90, NA, 22.5, NA, 38.6), Mean.age = c(41.3, 46.4, 46.3, NA, 51.7, 40.2, NA, 9.5, 33.1, NA, 39.2, 37.3, 40.7),
                     NT = c(20L, 24L, 33L, 25L, 36L, 18L, 15L, 14L, 15L, 44L, 40L, 20L, 34L), NC = c(19L, 29L, 33L, 25L, 39L, 22L, 15L, 14L, 15L, 42L, 20L, 20L, 36L),
                     Outcome1 = structure(c(NA, 4L, 1L, NA, NA, NA, 3L, NA, NA, NA, NA, NA, 2L), levels = c("Depre - from PANSS cluster items, from Gannon et al 2019", "Depre - HDRS", "Depre - HDRS-17", "Depre - MADRS"), class = "factor"), 
                     Mean1_T = c(NA, 4.8, 8.89, NA, NA, NA, 9.13, NA, NA, NA, NA, NA, 7.15), SD1_T = c(NA, 4.3, 2.15, NA, NA, NA, 2.48, NA, NA, NA, NA, NA, 2.93), Mean1_C = c(NA, 4.1, 9.97, NA, NA, NA, 11.53, NA, NA, NA, NA, NA, 13.42), SD1_C = c(NA, 4.7, 2.92, NA, NA, NA, 2.07, NA, NA, NA, NA, NA, 2.18),
                     Outcome2 = structure(c(NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = "Mania - YMRS", class = "factor"), Mean2_T = c(NA, 2.8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD2_T = c(NA, 2.4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     Mean2_C = c(NA, 3.2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD2_C = c(NA, 2.7, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Outcome3 = structure(c(2L, 3L, NA, NA, 1L, 2L, NA, 4L, NA, 3L, 2L, 2L, 3L), levels = c("Anxiety - BAI","Anxiety - HAM-A", "Anxiety - HARS", "Anxiety - Revised Children’s Manifest Anxiety (RCMA)"), class = "factor"),
                     Mean3_T = c(8.5, 3.2, NA, NA, 10.89, 15.27, NA, 2.07, NA, 1, 18.49, 15.92, 8.18), SD3_T = c(5.3, 5.6, NA, NA, 11.69, 9.86, NA, 1, NA, 0.15, 3.48, 1, 3.2), Mean3_C = c(12.2, 4.1, NA, NA, 16.28, 22.22, NA, 4.29, NA, 1.79, 21.53, 21.09, 16.14), SD3_C = c(5.6, 5.8, NA, NA, 10.89, 9.49, NA, 1.54, NA, 0.1, 3.22, 1.08, 3.09),
                     Outcome4 = structure(c(NA, NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = "PANSS tot", class = "factor"), Mean4_T = c(NA, NA, 53.91, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD4_T = c(NA, NA, 11.4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Mean4_C = c(NA, NA, 61.48, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD4_C = c(NA, NA, 14.89, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     Outcome5 = structure(c(NA, NA, NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = "Stress - PSS", class = "factor"), Mean5_T = c(NA, NA, NA, 13.65, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD5_T = c(NA, NA, NA, 3.14, NA, NA, NA, NA, NA, NA, NA, NA, NA), Mean5_C = c(NA, NA, NA, 17.83, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD5_C = c(NA, NA, NA, 5.16, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     Outcome7 = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA), levels = "OCD - Yale-Brown Obsessive Compulsive Scale (Y-BOCS)", class = "factor"), Mean7_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 14, NA, NA, NA, NA), SD7_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 8.7, NA, NA, NA, NA),
                     Mean7_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 16, NA, NA, NA, NA), SD7_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA, NA), Outcome8 = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2L, 2L, 1L), levels = c("Groningen Sleep Quality Scale (GSQS-15)", "PSQI - Pittsburgh sleep quality index"), class = "factor"), 
                     Mean8_T = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 9.15, 3.14, 4.06), SD8_T = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1.83, 0.23, 2.7), Mean8_C = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 11.84, 5.01, 8.89), SD8_C = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1.46, 0.25, 2.53),
                     N_dropoutsany_T = c(9L, 5L, 1L, 1L, 3L, 3L, NA, 2L, 0L, NA, 1L, 0L, 0L), N_dropoutsany_C = c(10L, 1L, 1L, 1L, 8L, 2L, NA, 1L, 2L, NA, 1L, 3L, 0L), N_adverseevents_T = c(2L, 19L, 35L, 1L, 7L, NA, NA, NA, 0L, NA, 0L, 0L, 18L), N_adverseevents_C = c(3L, 26L, 25L, 1L, 7L, NA, NA, NA, 0L, NA, 0L, 0L, 16L)), class = "data.frame", row.names = c(NA, -13L))
###----------------------------------------------------------------------------------------
###-----------------------------1. Depression----------------------------------------------
###----------------------------------------------------------------------------------------
depre <- metacont(n.e = NT, mean.e = Mean1_T, sd.e = SD1_T,
                  n.c = NC, mean.c = Mean1_C, sd.c = SD1_C,
                  data = df, studlab = ID, sm = "SMD")
depre
forest.meta(depre, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "WS", label.c = "Controls",
            label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)
#----------------------------------Publication Bias
#Funnel Plot
funnel(depre, xlab = "Hedges' g")
#Egger's test
#Calculating Effect Size (ES)
dep_rma <- escalc(measure = "SMD",
                  m1i = Mean1_T, sd1i = SD1_T, n1i = NT,
                  m2i = Mean1_C, sd2i = SD1_C, n2i = NC,
                  data = df)
#Pooling ES
dep_re <- rma(yi = dep_rma$yi, vi = dep_rma$vi)
dep_re
regtest(dep_re)
#Leave-one out analysis
dep_loo <- leave1out(dep_re)
dep_loo
###-------------------------------Meta-regression
#Age
mreg_dep_age <- metareg(depre, ~ Mean.age)
mreg_dep_age
#% Female
mreg_dep_fem <- metareg(depre, ~ X.Females)
mreg_dep_fem
#Country
mreg_dep_country <- metareg(depre, ~ Country)
mreg_dep_country
#WS dose
mreg_dep_dose <- metareg(depre, ~ T_dose)
mreg_dep_dose
#Duration
mreg_dep_duration <- metareg(depre, ~ Duration_weeks)
mreg_dep_duration

###----------------------------------------------------------------------------------------
###-------------------------------2. Anxiety-----------------------------------------------
###----------------------------------------------------------------------------------------
anx <- metacont(n.e = NT, mean.e = Mean3_T, sd.e = SD3_T,
                n.c = NC, mean.c = Mean3_C, sd.c = SD3_C,
                data = df, studlab = ID, sm = "SMD")
anx
forest.meta(anx, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "WS", label.c = "Controls",
            label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)
#----------------------------------Publication Bias
#Funnel Plot
funnel(anx, xlab = "Hedges' g")
#Egger's test
#Calculating Effect Size (ES)
anx_rma <- escalc(measure = "SMD",
                  m1i = Mean3_T, sd1i = SD3_T, n1i = NT,
                  m2i = Mean3_C, sd2i = SD3_C, n2i = NC,
                  data = df)
#Pooling ES
anx_re <- rma(yi = anx_rma$yi, vi = anx_rma$vi)
anx_re
regtest(anx_re)
#-----Carry out trim-and-fill analysis
m_taf <- trimfill(anx_re)
m_taf
#Leave-one out analysis
anx_loo <- leave1out(anx_re)
anx_loo
###-------------------------------Meta-regression
#Age
mreg_anx_age <- metareg(anx, ~ Mean.age)
mreg_anx_age
#% Female
mreg_anx_fem <- metareg(anx, ~ X.Females)
mreg_anx_fem
#Country
mreg_anx_country <- metareg(anx, ~ Country)
mreg_anx_country
#Diagnosis
mreg_anx_diagnosis <- metareg(anx, ~ Diagnosis_cat)
mreg_anx_diagnosis
#WS dose
mreg_anx_dose <- metareg(anx, ~ T_dose)
mreg_anx_dose
#Duration
mreg_anx_duration <- metareg(anx, ~ Duration_weeks)
mreg_anx_duration
#Concurrent medication
mreg_anx_currT <- metareg(anx, ~ Use)
mreg_anx_currT

###----------------------------------------------------------------------------------------
###-------------------------------3. Sleep quality-----------------------------------------
###----------------------------------------------------------------------------------------
ins <- metacont(n.e = NT, mean.e = Mean8_T, sd.e = SD8_T,
                n.c = NC, mean.c = Mean8_C, sd.c = SD8_C,
                data = df, studlab = ID, sm = "SMD")
ins
forest.meta(ins, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "WS", label.c = "Controls",
            label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)

###----------------------------------------------------------------------------------------
###-------------------------------4. Other outcomes----------------------------------------
###----------------------------------------------------------------------------------------
#--------OCD
ocd <- metacont(n.e = NT, mean.e = Mean7_T, sd.e = SD7_T,
                n.c = NC, mean.c = Mean7_C, sd.c = SD7_C,
                data = df, studlab = ID, sm = "SMD")
#--------PANSS
panss <- metacont(n.e = NT, mean.e = Mean4_T, sd.e = SD4_T,
                  n.c = NC, mean.c = Mean4_C, sd.c = SD4_C,
                  data = df, studlab = ID, sm = "SMD")
#--------Stress
stress <- metacont(n.e = NT, mean.e = Mean5_T, sd.e = SD5_T,
                   n.c = NC, mean.c = Mean5_C, sd.c = SD5_C,
                   data = df, studlab = ID, sm = "SMD")
#--------Mania
mania <- metacont(n.e = NT, mean.e = Mean2_T, sd.e = SD2_T,
                  n.c = NC, mean.c = Mean2_C, sd.c = SD2_C,
                  data = df, studlab = ID, sm = "SMD")

###----------------------------------------------------------------------------------------
###---------------------------5. Safety and Tolerability-----------------------------------
###----------------------------------------------------------------------------------------
#-----Dropouts due to any cause
do <- metabin(event.e = N_dropoutsany_T, n.e = NT,
              event.c = N_dropoutsany_C, n.c = NC,
              data = df, studlab = ID, sm = "OR")
do
forest.meta(do, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "WS", label.c = "Controls",
            label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)
#-----N adverse events
ae <- metabin(event.e = N_adverseevents_T, n.e = NT,
              event.c = N_adverseevents_C, n.c = NC,
              data = df, studlab = ID, sm = "OR")
ae
forest.meta(ae, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "WS", label.c = "Controls",
            label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)