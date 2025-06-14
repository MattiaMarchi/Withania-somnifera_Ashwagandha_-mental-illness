# R code for replicating the dataset and the analyses in the paper #
# The effect of Withania somnifera (Ashwagandha) on mental health symptoms: a systematic review and meta-analysis #
# R code by Mattia Marchi (mattiamarchimd@gmail.com) 
# June 14, 2025

###------------------------------------------------------------------------------------------------------
###------------------------Meta Analysis - Ashwagandha in psychiatry disorders---------------------------
###------------------------------------------------------------------------------------------------------
#Load required packages
library(meta)
library(metafor)
library(tidyverse)
library(metaforest)
library(dmetar)
library(brms)
library(bayesplot)

#Import data
df <- structure(list(ID = structure(1:14, levels = c("Andrade et al 2000", "Chengappa et al 2013", "Chengappa et al 2018", "Choudhary et al 2017", "Cooley et al 2009", "Fuladi et al 2021", "Fulzele et al 2014", 
                                                     "Hosseini et al 2019", "Jahanbakhsh et al 2016", "Khyati et al 2013", "Langade et al 2019", "Langade et al 2021", "Majeed et al 2023", "Pandit et al 2024"), class = "factor"),
                     Trial.ID = structure(c(10L, 8L, 9L, 10L, 7L, 6L, 10L, 4L, 5L, 10L, 10L, 1L, 3L, 2L), levels = c("CTRI/2019/03/018074 ", "CTRI/2019/11/022100", "CTRI/2022/05/042640", "IRCT201506215280N18", 
                                                                                                                     "IRCT2015070523079N1 ", "IRCT20180615040105N1", "ISRCTN78958974", "NCT00761761", "NCT01793935", "NR"), class = "factor"),
                     Country = structure(c(2L, 4L, 4L, 2L, 1L, 3L, 2L, 3L, 3L, 2L, 2L, 2L, 2L, 2L), levels = c("Canada", "India", "Iran", "USA"), class = "factor"), Study.design = structure(c(2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L), levels = c("Parallel RCT", "RCT"), class = "factor"),
                     Setting = structure(c(2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L), levels = c("NR", "Outpatients clinic"), class = "factor"),
                     Diagnosis_full = structure(c(2L, 3L, 10L, 4L, 2L, 6L, 8L, 1L, 9L, 6L, 7L, 7L, 5L, 4L), levels = c("ADHD", "Anxiety", "Bipolar disorder I, II, or NOS", "Chronic stress", "Depression/Anxiety", "Generalized anxiety disorder", "Insomnia", "Major Depression", "OCD", "Schizophrenia, schizo-affective disorder"), class = "factor"),
                     Diagnosis = structure(c(2L, 3L, 8L, 4L, 2L, 2L, 6L, 1L, 7L, 2L, 5L, 5L, 2L, 2L), levels = c("ADHD", "Anxiety", "BD", "Chronic stress", "Insomnia", "MDD", "OCD", "SZ"), class = "factor"),
                     Diagnosis.tool = structure(c(6L, 7L, 7L, 8L, 4L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 5L, 8L), levels = c("DSM 5", "DSM-IV", "DSM-IV-TR", "DSM-IV, Beck Anxiety Inventory (BAI)", "HDRS/HARS", "ICD-10", "Mini-International Neuropsychiatric Interview (MINI)-DSM IV", "Perceived stress scale (PSS)"), class = "factor"),
                     Stage.of.illness = structure(c(8L, 6L, 5L, 7L, 3L, 4L, 1L, 4L, 4L, 4L, 7L, 7L, 2L, 7L), levels = c("Mild or moderate depression at baseline", "Mild to moderate symptoms at baseline", "Moderate to high anxiety but no more than mild depression", "NR", "Patients with persisting symptoms", "Stable", "Symptomatic", "Symptomatic to warrant further therapeutic interventions"), class = "factor"),
                     Severity_cat = structure(c(1L, 2L, 1L, 1L, 1L, NA, 3L, NA, NA, NA, 1L, 1L, 3L, 1L), levels = c("High", "Low", "Moderate"), class = "factor"), Duration = structure(c(3L, 4L, 2L, 4L, 2L, 3L, 3L, 3L, 3L, 4L, 1L, 4L, 2L, 4L), levels = c("10 weeks", "12 weeks", "6 weeks", "8 weeks"), class = "factor"), Duration_weeks = c(6L, 8L, 12L, 8L, 12L, 6L, 6L, 6L, 6L, 8L, 10L, 8L, 12L, 8L), 
                     T_dose = c(500L, 500L, 1000L, 600L, 600L, 1000L, NA, 10L, 1000L, 12000L, 600L, 600L, 500L, 500L), Withanolides. = c(NA, 8, 8, 5, 1.5, NA, NA, NA, NA, NA, 5, 5, 2.5, 8), dose_with_int = c(NA, 4000L, 8000L, 3000L, 900L, NA, NA, NA, NA, NA, 3000L, 3000L, 1250L, 4000L),
                     Tr = structure(c(4L, 4L, 2L, 5L, 5L, 2L, 6L, 1L, 2L, 3L, 5L, 5L, 4L, 4L), levels = c("Withania somnifera 10 mg po daily", "Withania somnifera 1000 mg po daily", "Withania somnifera 12000 mg po daily", "Withania somnifera 500 mg po daily", "Withania somnifera 600 mg po daily", "Withania somnifera po daily"), class = "factor"),
                     C = structure(c(2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("CBT+PBO", "PBO"), class = "factor"),
                     Concurrent.treatments = structure(c(3L, 4L, 2L, 6L, 6L, 8L, 5L, 1L, 8L, 7L, 7L, 6L, 7L, 6L), levels = c("Anti-ADHD treatments", "Any medication, but stable antipsychotics", "Any stable treatment", "Mood stabilizer or other BD medications", "Nardostachys jatamansi and Lavandula stoechas", "None", "NR", "SSRI"), class = "factor"),
                     Use = structure(c(1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, NA, NA, 2L, NA, 2L), levels = c("Add-on", "Sole"), class = "factor"), N.tot....females. = structure(c(4L, 9L, 10L, 7L, 13L, 5L, 2L, 1L, 3L, 12L, 8L, 6L, 11L, 14L), levels = c("28 (39.3%)", "30 (NR)", "32 (90%)", "39 (38.5%)", "40 (45%)", "40 (NR)", "52 (26.9%)", "60 (22.5%)", "60 (61.7%)", "68 (48.5%)", "70 (38.6%)", "86 (NR)", "87 (63%)", "98 (28.6%)"), class = "factor"), 
                     X.Females = c(38.5, 61.7, 48.5, 26.9, 63, 45, NA, 39.3, 90, NA, 22.5, NA, 38.6, 28.6), Mean.age..SD. = structure(c(10L, 12L, 11L, 2L, 13L, 8L, 3L, 14L, 4L, 1L, 7L, 6L, 9L, 5L), levels = c("16-60", "18-60", "20-65", "33.1 (10.8)", "35.1 (10.3)", "37.3 (6.4)", "39.2 (5.4)", "40.2 (8.8)", "40.7 (11.3)", "41.3 (13.8)", "46.3 (12.1)", "46.4 (10.3)", "51.7 (9.6)", "9.5 (1.6)"), class = "factor"), 
                     Mean.age = c(41.3, 46.4, 46.3, NA, 51.7, 40.2, NA, 9.5, 33.1, NA, 39.2, 37.3, 40.7, 35.1), NT = c(20L, 24L, 33L, 25L, 36L, 18L, 15L, 14L, 15L, 44L, 40L, 20L, 34L, 22L), NC = c(19L, 29L, 33L, 25L, 39L, 22L, 15L, 14L, 15L, 42L, 20L, 20L, 36L, 24L),
                     Outcome1 = structure(c(NA, 5L, 1L, NA, NA, NA, 4L, NA, NA, NA, NA, NA, 3L, 2L), levels = c("Depre - from PANSS cluster items, from Gannon et al 2019", "Depre - HAM-D", "Depre - HDRS", "Depre - HDRS-17", "Depre - MADRS"), class = "factor"),
                     Mean1_T = c(NA, 4.8, 8.89, NA, NA, NA, 9.13, NA, NA, NA, NA, NA, 7.15, 6.27), SD1_T = c(NA, 4.3, 2.15, NA, NA, NA, 2.48, NA, NA, NA, NA, NA, 2.93, 2.27), Mean1_C = c(NA, 4.1, 9.97, NA, NA, NA, 11.53, NA, NA, NA, NA, NA, 13.42, 13.04), SD1_C = c(NA, 4.7, 2.92, NA, NA, NA, 2.07, NA, NA, NA, NA, NA, 2.18, 2.42), NT_1 = c(NA, 24L, 33L, NA, NA, NA, 15L, NA, NA, NA, NA, NA, 34L, 22L),
                     NC_1 = c(NA, 29L, 33L, NA, NA, NA, 15L, NA, NA, NA, NA, NA, 36L, 24L), Outcome2 = structure(c(NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = "Mania - YMRS", class = "factor"), Mean2_T = c(NA, 2.8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD2_T = c(NA, 2.4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Mean2_C = c(NA, 3.2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD2_C = c(NA, 2.7, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     Outcome3 = structure(c(2L, 3L, NA, NA, 1L, 2L, NA, 4L, NA, 3L, 2L, 2L, 3L, 2L), levels = c("Anxiety - BAI", "Anxiety - HAM-A", "Anxiety - HARS", "Anxiety - Revised Children’s Manifest Anxiety (RCMA)"), class = "factor"), Mean3_T = c(8.5, 3.2, NA, NA, 10.89, 15.27, NA, 2.07, NA, 1, 18.49, 15.92, 8.18, 9.5), SD3_T = c(5.3, 5.6, NA, NA, 11.69, 9.86, NA, 1, NA, 0.15, 3.48, 4.47, 3.2, 2.89),
                     Mean3_C = c(12.2, 4.1, NA, NA, 16.28, 22.22, NA, 4.29, NA, 1.79, 21.53, 21.09, 16.14, 15.15), SD3_C = c(5.6, 5.8, NA, NA, 10.89, 9.49, NA, 1.54, NA, 0.1, 3.22, 4.83, 3.09, 2.1), NT_3 = c(20L, 24L, NA, NA, 36L, 18L, NA, 14L, NA, 44L, 40L, 20L, 34L, 22L), NC_3 = c(19L, 29L, NA, NA, 39L, 22L, NA, 14L, NA, 42L, 20L, 20L, 36L, 24L), Outcome4 = structure(c(NA, NA, 1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = "PANSS tot", class = "factor"), 
                     Mean4_T = c(NA, NA, 53.91, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD4_T = c(NA, NA, 11.4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Mean4_C = c(NA, NA, 61.48, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), SD4_C = c(NA, NA, 14.89, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Outcome5 = structure(c(NA, NA, 1L, 2L, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L), levels = c("Stess - PSS", "Stress - PSS"), class = "factor"),
                     Mean5_T = c(NA, NA, 14.67, 13.65, NA, NA, NA, NA, NA, NA, NA, NA, NA, 23.73), SD5_T = c(NA, NA, 5.87, 3.14, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4.67), Mean5_C = c(NA, NA, 18.09, 17.83, NA, NA, NA, NA, NA, NA, NA, NA, NA, 31.5), SD5_C = c(NA, NA, 6.64, 5.16, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5.76), NT_5 = c(NA, NA, 33L, 25L, NA, NA, NA, NA, NA, NA, NA, NA, NA, 22L), NC_5 = c(NA, NA, 33L, 25L, NA, NA, NA, NA, NA, NA, NA, NA, NA, 24L),
                     Outcome6 = structure(c(NA, NA, NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA, NA, NA), levels = "ADHD Hypersensitivity - RCMA (RCMA measures anxiety, therefore do not consider this)", class = "factor"), Mean6_T = c(NA, NA, NA, NA, NA, NA, NA, 3.29, NA, NA, NA, NA, NA, NA), SD6_T = c(NA, NA, NA, NA, NA, NA, NA, 1.27, NA, NA, NA, NA, NA, NA), Mean6_C = c(NA, NA, NA, NA, NA, NA, NA, 5.86, NA, NA, NA, NA, NA, NA),
                     SD6_C = c(NA, NA, NA, NA, NA, NA, NA, 1.56, NA, NA, NA, NA, NA, NA), Outcome7 = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, 1L, NA, NA, NA, NA, NA), levels = "OCD - Yale-Brown Obsessive Compulsive Scale (Y-BOCS)", class = "factor"), Mean7_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 14, NA, NA, NA, NA, NA), SD7_T = c(NA, NA, NA, NA, NA, NA, NA, NA, 8.7, NA, NA, NA, NA, NA),
                     Mean7_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 16, NA, NA, NA, NA, NA), SD7_C = c(NA, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA, NA, NA), Outcome8 = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2L, 2L, 1L, 2L), levels = c("Groningen Sleep Quality Scale (GSQS-15)", "PSQI - Pittsburgh sleep quality index"), class = "factor"), Mean8_T = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 9.15, 3.14, 4.06, 7.45), SD8_T = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1.83, 1.52, 2.7, 3.86),
                     Mean8_C = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 11.84, 5.01, 8.89, 10.96), SD8_C = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1.46, 1.66, 2.53, 4.41), NT_8 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 40L, 20L, 34L, 22L), NC_8 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 20L, 20L, 36L, 24L), N_dropoutsany_T = c(9L, 5L, 1L, 1L, 3L, 3L, NA, 2L, 0L, NA, 1L, 0L, 0L, NA),
                     N_dropoutsany_C = c(10L, 1L, 1L, 1L, 8L, 2L, NA, 1L, 2L, NA, 1L, 3L, 0L, NA), N_adverseevents_T = c(2L, 19L, 35L, 1L, 7L, NA, NA, NA, 0L, NA, 0L, 0L, 18L, NA), N_adverseevents_C = c(3L, 26L, 25L, 1L, 7L, NA, NA, NA, 0L, NA, 0L, 0L, 16L, NA),
                     N_dropoutsAE_T = c(2L, 0L, 0L, 0L, 0L, NA, NA, NA, 0L, NA, 0L, 0L, 0L, NA), N_dropoutsAE_C = c(3L, 0L, 0L, 0L, 0L, NA, NA, NA, 0L, NA, 0L, 0L, 0L, NA)), row.names = c(NA, -14L), class = "data.frame")
###----------------------------------------------------------------------------------------
###-----------------------------1. Depression----------------------------------------------
###----------------------------------------------------------------------------------------
depre <- metacont(n.e = NT_1, mean.e = Mean1_T, sd.e = SD1_T,
                  n.c = NC_1, mean.c = Mean1_C, sd.c = SD1_C,
                  data = df, studlab = ID, sm = "SMD")
depre
forest(depre, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
       label.e = "WS", label.c = "Controls",
       label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)
###-------------------------------Subgroup meta-analysis by Diagnosis
dep_sg <- metacont(n.e = NT_1, mean.e = Mean1_T, sd.e = SD1_T,
                   n.c = NC_1, mean.c = Mean1_C, sd.c = SD1_C,
                   data = df, studlab = ID, sm = "SMD",
                   byvar = Diagnosis) #remove byvar = if you don't want display subgroups
dep_sg
forest(dep_sg, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
       overall = T, overall.hetstat = TRUE, print.subgroup.labels = TRUE, #remove this line if you don't want display subgroups
       label.e = "WS", label.c = "Controls",
       label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)
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
###-------------------------------Subgroup meta-analysis by Diagnosis_cat
anx_sg <- metacont(n.e = NT_3, mean.e = Mean3_T, sd.e = SD3_T,
                   n.c = NC_3, mean.c = Mean3_C, sd.c = SD3_C,
                   data = df, studlab = ID, sm = "SMD",
                   byvar = Diagnosis) #remove byvar = if you don't want display subgroups
anx_sg
forest(anx_sg, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
       overall = T, overall.hetstat = TRUE, print.subgroup.labels = TRUE, #remove this line if you don't want display subgroups
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
#--------Stress
stress <- metacont(n.e = NT_5, mean.e = Mean5_T, sd.e = SD5_T,
                   n.c = NC_5, mean.c = Mean5_C, sd.c = SD5_C,
                   data = df, studlab = ID, sm = "SMD")
stress
forest(stress, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
       label.e = "WS", label.c = "Controls",
       label.left = "Favours WS", label.right = "Favours Controls", allstudies = F)

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


###------------------------------------------------------------------------------------------------------
###--------------------Bayesian Meta Analysis - Ashwagandha in psychiatry disorders----------------------
###------------------------------------------------------------------------------------------------------

###----------------------------------------------------------------------------------------
###-----------------------------1. Depression----------------------------------------------
###----------------------------------------------------------------------------------------
depre <- metacont(n.e = NT_1, mean.e = Mean1_T, sd.e = SD1_T,
                  n.c = NC_1, mean.c = Mean1_C, sd.c = SD1_C,
                  data = df, studlab = ID, sm = "SMD")
depre
dep_bayes <- data.frame(TE = depre$TE, seTE = depre$seTE, Author = depre$studlab)
#Remove NA
dep_bayes <- dep_bayes %>% drop_na(TE, seTE)
dep_bayes <- merge(dep_bayes, dosedf, by = "Author")
#Set weakly informative priors
priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))
#Fit the Bayesian MA model with random effects
dep.brm <- brm(TE|se(seTE) ~ 1 + (1|Author),
               # this line means that we aim to estimate the intercept (the population effect) from the effect size of each study, weighted for the study precision (se), in a random effects model (specified by (1|Author), if you want fixed effect model remove it)
               data = dep_bayes, #specify the data to be used
               prior = priors, # priors set above
               iter = 4000) #increase the number of iterations if you have a complex model (default is 2000)
#Before interpreting the model, check convergence
#1. check Rhat Potential Scale Reduction Factor (PSRF), should be <1.01 - if not converged, increase number of iterations
summary(dep.brm)
#2. check similarity between simulated and observed data
pp_check(dep.brm)
#Leave-one-out analysis
dep.loo_result <- loo(dep.brm)
plot(dep.loo_result)
#See the estimated deviation of each study's true effect size from the pooled effect
ranef(dep.brm)
#Estimate the posterior distribution of tau^2 and mu
variables(dep.brm)
#anx.post.samples <- posterior_samples(anx.brm, c("^b", "^sd")) --- Deprecated, the new function is below:
dep.post.samples <- as_draws_df(dep.brm, variable = c("b_Intercept", "sd_Author__Intercept"))
dep.post.samples.df <- data.frame(dep.post.samples$b_Intercept, dep.post.samples$sd_Author__Intercept)
names(dep.post.samples.df)
names(dep.post.samples.df) <- c("smd", "tau")
#Generate a density plot of the posterior distribution
dep_pd_smd <- ggplot(aes(x = smd), data = dep.post.samples.df) +
  geom_density(fill = "lightblue",                # set the color
               color = "lightblue", alpha = 0.7) +  
  geom_point(y = 0,                               # add point at mean
             x = mean(dep.post.samples.df$smd)) +
  labs(x = expression(italic(SMD)),
       y = element_blank()) +
  theme_minimal()
dep_pd_tau <- ggplot(aes(x = tau), data = dep.post.samples.df) +
  geom_density(fill = "lightgreen",               # set the color
               color = "lightgreen", alpha = 0.7) +  
  geom_point(y = 0, 
             x = mean(dep.post.samples.df$tau)) +        # add point at mean
  labs(x = expression(tau),
       y = element_blank()) +
  theme_minimal()
#Check probability smd is smaller than a given value
dep_smd.ecdf <- ecdf(dep.post.samples.df$smd)
dep_smd.ecdf(-1)
#Prepare the data to create forestplot
dep.study.draws <- spread_draws(dep.brm, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)
dep.pooled.effect.draws <- spread_draws(dep.brm, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")
dep.forest.data <- bind_rows(dep.study.draws, 
                             dep.pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))
dep.forest.data.summary <- group_by(dep.forest.data, Author) %>% 
  mean_qi(b_Intercept)
#Generate forestplot: (The effect sizes displayed in the forest plot do not represent the observed effect sizes of the original studies, but the estimate of the “true” effect size (θk) of a study based on the Bayesian model)
dep_fp <- ggplot(dep.forest.data, 
                 aes(x = b_Intercept, y = relevel(Author, "Pooled Effect", after = Inf))) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(dep.brm)[1, 1], color = "grey", linewidth = 1) +
  geom_vline(xintercept = fixef(dep.brm)[1, 3:4], color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  # Add densities
  geom_density_ridges(aes(height = ..density..), stat = "density",
                      scale = 1, rel_min_height = 0.01, fill = "blue", 
                      color = NA, alpha = 0.8) +
  geom_pointinterval(data = dep.forest.data.summary,
                     aes(x = b_Intercept, y = Author, xmin = .lower, xmax = .upper), linewidth = 1) +
  # Add text and labels
  geom_text(data = mutate_if(dep.forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf, y = Author), hjust = "inward") +
  scale_x_continuous(limits = c(-4, 1.5), breaks = c(-4,-3,-2,-1,0,1)) +
  labs(x = "Standardized Mean Difference", y = NULL) +
  theme_minimal()
dep_fp
###----------------------------------------------------------------------------------------
###-------------------------------2. Anxiety-----------------------------------------------
###----------------------------------------------------------------------------------------
anx <- metacont(n.e = NT_3, mean.e = Mean3_T, sd.e = SD3_T,
                n.c = NC_3, mean.c = Mean3_C, sd.c = SD3_C,
                data = df, studlab = ID, sm = "SMD")
#Prepare the dataset for Bayesian MA
anx_bayes <- data.frame(TE = anx$TE, seTE = anx$seTE, Author = anx$studlab)
#Remove NA
anx_bayes <- anx_bayes %>% drop_na(TE, seTE)
anx_bayes <- merge(anx_bayes, dosedf, by = "Author")
#Set weakly informative priors
priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))
#Fit the Bayesian MA model with random effects
anx.brm <- brm(TE|se(seTE) ~ 1 + (1|Author),
               # this line means that we aim to estimate the intercept (the population effect) from the effect size of each study, weighted for the study precision (se), in a random effects model (specified by (1|Author), if you want fixed effect model remove it)
               data = anx_bayes, #specify the data to be used
               prior = priors, # priors set above
               iter = 4000) #increase the number of iterations if you have a complex model (default is 2000)
#Before interpreting the model, check convergence
#1. check Rhat Potential Scale Reduction Factor (PSRF), should be <1.01 - if not converged, increase number of iterations
summary(anx.brm)
#2. check similarity between simulated and observed data
pp_check(anx.brm)
#Leave-one-out analysis
anx.loo_result <- loo(anx.brm)
plot(anx.loo_result)
#See the estimated deviation of each study's true effect size from the pooled effect
ranef(anx.brm)
#Estimate the posterior distribution of tau^2 and mu
variables(anx.brm)
#anx.post.samples <- posterior_samples(anx.brm, c("^b", "^sd")) --- Deprecated, the new function is below:
anx.post.samples <- as_draws_df(anx.brm, variable = c("b_Intercept", "sd_Author__Intercept"))
anx.post.samples.df <- data.frame(anx.post.samples$b_Intercept, anx.post.samples$sd_Author__Intercept)
names(anx.post.samples.df)
names(anx.post.samples.df) <- c("smd", "tau")
#Generate a density plot of the posterior distribution
anx_pd_smd <- ggplot(aes(x = smd), data = anx.post.samples.df) +
  geom_density(fill = "lightblue",                # set the color
               color = "lightblue", alpha = 0.7) +  
  geom_point(y = 0,                               # add point at mean
             x = mean(anx.post.samples.df$smd)) +
  labs(x = expression(italic(SMD)),
       y = element_blank()) +
  theme_minimal()
anx_pd_tau <- ggplot(aes(x = tau), data = anx.post.samples.df) +
  geom_density(fill = "lightgreen",               # set the color
               color = "lightgreen", alpha = 0.7) +  
  geom_point(y = 0, 
             x = mean(anx.post.samples.df$tau)) +        # add point at mean
  labs(x = expression(tau),
       y = element_blank()) +
  theme_minimal()
#Check probability smd is smaller than a given value
anx_smd.ecdf <- ecdf(anx.post.samples.df$smd)
anx_smd.ecdf(-1)
#Prepare the data to create forestplot
anx.study.draws <- spread_draws(anx.brm, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)
anx.pooled.effect.draws <- spread_draws(anx.brm, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")
anx.forest.data <- bind_rows(anx.study.draws, 
                             anx.pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))
anx.forest.data.summary <- group_by(anx.forest.data, Author) %>% 
  mean_qi(b_Intercept)
#Generate forestplot: (The effect sizes displayed in the forest plot do not represent the observed effect sizes of the original studies, but the estimate of the “true” effect size (θk) of a study based on the Bayesian model)
anx_fp <- ggplot(anx.forest.data, 
                 aes(x = b_Intercept, y = relevel(Author, "Pooled Effect", after = Inf))) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(anx.brm)[1, 1], color = "grey", linewidth = 1) +
  geom_vline(xintercept = fixef(anx.brm)[1, 3:4], color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  # Add densities
  geom_density_ridges(aes(height = ..density..), stat = "density",
                      scale = 1, rel_min_height = 0.01, fill = "blue", 
                      color = NA, alpha = 0.8) +
  geom_pointinterval(data = anx.forest.data.summary,
                     aes(x = b_Intercept, y = Author, xmin = .lower, xmax = .upper), linewidth = 1) +
  # Add text and labels
  geom_text(data = mutate_if(anx.forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf, y = Author), hjust = "inward") +
  scale_x_continuous(limits = c(-7.5, 1.5), breaks = c(-7,-6,-5,-4,-3,-2,-1,0,1)) +
  labs(x = "Standardized Mean Difference", y = NULL) +
  theme_minimal()
anx_fp
#----Meta-regression of the dose
anx_bayes$logDose_c <- log(anx_bayes$Dose) - mean(log(anx_bayes$Dose))
anx.brm.meta.reg <- brm(TE | se(seTE) ~ logDose_c + (1 | Author),
                        data = anx_bayes, prior = priors, iter = 4000)
summary(anx.brm.meta.reg)


###------------------------------------------------------------------------------------------------------
###------------------Bayesian Meta Analysis - Comparing model with different priors----------------------
###------------------------------------------------------------------------------------------------------
#Comparing two weakly informative priors for ANXIETY
#1.
priors1 <- c(prior(normal(0,1), class = Intercept), prior(cauchy(0,0.5), class = sd)) # cit DOI: 10.1214/06-BA117A
anx.brm1 <- brm(TE|se(seTE) ~ 1 + (1|Author), data = anx_bayes, prior = priors1, iter = 4000) #cit https://doi.org/10.3390/e19100555
summary(anx.brm1)
#2.
priors2 <- c(prior(normal(0,1), class = Intercept), prior(exponential(1), class = sd))
anx.brm2 <- brm(TE|se(seTE) ~ 1 + (1|Author), data = anx_bayes, prior = priors2, iter = 4000)
summary(anx.brm2)
# Compare models
summary(anx.brm1)
summary(anx.brm2)
loo_cauchy <- loo(anx.brm1)
loo_exp <- loo(anx.brm2)
loo_compare(loo_cauchy, loo_exp)
posterior_cauchy <- as_draws_df(anx.brm1)
posterior_exp <- as_draws_df(anx.brm2)
df_tau <- data.frame("Cauchy(0, 0.5)" = posterior_cauchy$`sd_Author__Intercept`,"Exponential(1)" = posterior_exp$`sd_Author__Intercept`)
a <- mcmc_areas(df_tau, prob = 0.95) + ggtitle("Posterior of τ under Different Priors")

#Comparing two weakly informative priors for DEPRESSION
#1.
dep.brm1 <- brm(TE|se(seTE) ~ 1 + (1|Author), data = dep_bayes, prior = priors1, iter = 4000)
summary(dep.brm1)
#2.
dep.brm2 <- brm(TE|se(seTE) ~ 1 + (1|Author), data = dep_bayes, prior = priors2, iter = 4000)
summary(dep.brm2)
# Compare models
summary(dep.brm1)
summary(dep.brm2)
loo_cauchy1 <- loo(dep.brm1)
loo_exp1 <- loo(dep.brm2)
loo_compare(loo_cauchy1, loo_exp1)
posterior_cauchy1 <- as_draws_df(dep.brm1)
posterior_exp1 <- as_draws_df(dep.brm2)
df_tau.dep <- data.frame("Cauchy(0, 0.5)" = posterior_cauchy1$`sd_Author__Intercept`,"Exponential(1)" = posterior_exp1$`sd_Author__Intercept`)
d <- mcmc_areas(df_tau, prob = 0.95) + ggtitle("Posterior of τ under Different Priors")