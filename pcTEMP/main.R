# R script to run temperature preference statistics
# Author: Bass Dye
# Created: August 1st, 2023
# Last revision: August 22nd, 2023

# House keeping; clean environment 
rm(list=ls())

# Install and load packages ####
if(!require(readxl))install.packages("readxl")
if(!require(fs)) install.packages("fs")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(here))install.packages("here")
if(!require(ggpubr))install.packages("ggpubr")
if(!require(ggpubr))install.packages("gtools")

# Load packages
library("readxl")
library("fs")
library("tidyverse")
library("here")
library("ggpubr")
library("gtools")

# Update experiment specific information ####
# Number of experimental subjects 
number_subjects <- 6

# Number of experimental groups
number_groups <- 2

# Number of treatments
number_treatments <- 2 # Leave blank if no treatments
treatments <- c("high", "low") # Corresponding to order in excel files, we can make code so user doesn't have to define order

# Number of experimental phases
number_phases <- 3

# Length of observation phase in seconds
length_phase <- 3600

# Ethovision tracking analysis frequency (per second)
# e.g. 25 means the subject(s) were tracked 25 times per second; 1 means the subject(s) tracked 1 time per second
track_freq <- 1

# Temperature range of preference chamber
# Starting with warmest temperature to coldest
temp_range <-  c("33*C", "30*C", "27*C", "24*C", "21*C")

#########################################
############## Load in data #############
#########################################

# Load in excel files and organize data ####
# Remove all rows above column names
# Make sure columns are named in order from left to right: time, compartment_1, ... compartment_8
# Each tab should be for each subject, name doesn't matter but should be in order from subject 1 - last subject
# Tip make sure each sheet tab is highlighted to easily format all sheets
# Put all excel files into data folder

# Function to read in multiple sheets within one excel file ####
multiplesheets <- function(fname) {
  
  # Get info about all excel sheets
  sheets <- excel_sheets(fname)
  sheets2 <- sheets[1:number_subjects] # Number of sheets in each excel file; 1 sheet per tracked subject
  tibble <- lapply(sheets2, function(x) read_excel(fname, col_names = TRUE, na = "-", sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # Change names of sheets
  sheets_new <- paste0("subject_", 1:number_subjects) # Rename sheets for each subject in the experiment; 1 sheet per tracked subject
  names(data_frame) <- sheets_new
  
  #Print data frame 
  print(data_frame)
}

# Read in multiple excel files ####
file_list <- mixedsort(list.files(pattern = "*xlsx")) # Get list of all excel files in R project folder; mixedsort puts in order from 1-12
df_list <- lapply(set_names(file_list), multiplesheets) # Create data frame containing each excel file and sheets 

#########################################
############## Organize data ############
#########################################

# Create new lists for each experimental observational phase
non_gradient <- df_list[grepl("NG", names(df_list))]
acute <- df_list[grepl("A", names(df_list))]
final <- df_list[grepl("F", names(df_list))]

# Clear space in environment
rm(df_list)

# Function to combine (by stacking) matching list rows and then sum the columns ####
combined_rows <- function(input_list) {
  test <- bind_rows(input_list) %>%
    summarise_all(~ sum(., na.rm = TRUE))
}

# Sum the total count of fish in each compartment over all experiments for each individual experimental phase
# First step - Apply function to each experiment, summing total count of fish in each compartment
non_gradient_separate <- lapply(non_gradient, combined_rows)
acute_separate <- lapply(acute, combined_rows)
final_separate <- lapply(final, combined_rows)

# Clear space in environment
rm(list = c("non_gradient", "acute", "final"))

zones_1_8 <- c(2:9) #column index
# Create new data frame with the number of experiments as objects and each temperature zone (1-8) as variables
non_grad_dat <- data.frame(t(sapply(non_gradient_separate, "[", zones_1_8)))
non_grad_dat$count <- rowSums(matrix(unlist(non_grad_dat), ncol = 8, byrow = FALSE)) # add column with total count per experiment

acute_dat <- data.frame(t(sapply(acute_separate, "[", zones_1_8)))
acute_dat$count <- rowSums(matrix(unlist(acute_dat), ncol = 8, byrow = FALSE)) # add column with total count per experiment

final_dat <- data.frame(t(sapply(final_separate, "[", zones_1_8)))
final_dat$count <- rowSums(matrix(unlist(final_dat), ncol = 8, byrow = FALSE)) # add column with total count per experiment

# Combine into non gradient, acute, and final observations into one data frame
combined_dat <- data.frame(rbind(non_grad_dat, acute_dat, final_dat))

# Create new data frame to match statistics script from Schram et al. 2013
tmp_dat <- combined_dat %>%
  add_column(experiment = c(row.names.data.frame(non_grad_dat), row.names.data.frame(acute_dat), row.names.data.frame(final_dat)),
             phase = c(rep("I", time = length(non_gradient_separate)), rep("II", time = length(acute_separate)), rep("III", time = length(final_separate))),
             .before = colnames(non_grad_dat[1])) 

if(exists("number_treatments")) {
  tmp_dat <- tmp_dat %>% 
    add_column(treatment = rep(treatments, times = nrow(non_grad_dat) / number_treatments * number_phases), .after = "phase") # Add treatments column; times = # experiments / # treatments * 3 phases
  rownames(tmp_dat) <- NULL # Remove row names
  colnames(tmp_dat)[c(4:11)] <- c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8")
} else {
  rownames(tmp_dat) <- NULL # Remove row names
  colnames(tmp_dat)[c(3:10)] <- c("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8")
}
  
# Calculate percent use of each compartment per experiment
if(exists("number_treatments")) {
  stats_df <- data.frame(tmp_dat[, 1:11])
  stats_df[, 4:11] <- NA
  for (ii in 1:nrow(combined_dat)) {
    for (tt in 4:11)
      stats_df[ii,tt] <- unlist(tmp_dat[ii, tt]) / tmp_dat$count[ii]
  }
} else {
  stats_df <- data.frame(tmp_dat[, 1:10])
  stats_df[, 3:10] <- NA
  for (ii in 1:nrow(combined_dat)) {
    for (tt in 3:10)
      stats_df[ii,tt] <- unlist(tmp_dat[ii, tt]) / tmp_dat$count[ii]
  }
}

# Clear space in environment
rm(list = c("non_grad_dat", "acute_dat", "final_dat", "combined_dat", "tmp_dat"))

# Step 2: create data set with proportions per temperature zone, by summing up the proportions of ####
# usage of compartments with the same temperature 
# C1: 33 degrees: compartment 4
# C2: 30 degrees: compartments 3 and 5
# C3: 27 degrees: compartments 2 and 6
# C4: 24 degrees: compartments 1 and 7
# C5: 21 degrees: compartment 8
if(exists("number_treatments")) {
  prop_dat <- stats_df[, 1:3]; 
} else {
  prop_dat <- stats_df[, 1:2]; 
}
prop_dat$C1 <- stats_df$z4; 
prop_dat$C2 <- stats_df$z3 + stats_df$z5;
prop_dat$C3 <- stats_df$z2 + stats_df$z6;
prop_dat$C4 <- stats_df$z1 + stats_df$z7;
prop_dat$C5 <- stats_df$z8;

# Step 3: compute log-ratios ####
# An appropriate statistical framework for handling compositional data is to replace the observed proportions 
# With a set of ratios by choosing the observed fraction of one particular component as the denominator by which all other fractions are divided
# The results of the analyses do not depend upon the choice of denominator.
# In this way, the unit sum constraint on the composition is broken (Aitchison (1986) The statistical analysis of compositional data. London: Chapman and Hall. 416 p.)

# A version of the central limit theorem exists stating why random variation in log-ratios can often be assumed to be normally distributed (Aitchison, 1986)
# Take the compartment C3 (21*C temperature compartment) as the denominator
prop_dat$LR1 <- log(prop_dat$C1/prop_dat$C3);
prop_dat$LR2 <- log(prop_dat$C2/prop_dat$C3);
prop_dat$LR3 <- log(prop_dat$C4/prop_dat$C3);
prop_dat$LR4 <- log(prop_dat$C5/prop_dat$C3);

# Step 4: offset log-ratios of usage with log-ratios of availability of temperature zones ####
# I.e. when the temperature gradient is present within the chamber, 1/8 of the chamber is 15ºC while 1/4 is 18ºC.
# E.g. prop_dat$LRA1 <- prop_dat$LR1 - log(0.5) is d = yu - ya (pairwise habitat differences = utilized habitat - available habitat; Aebischer et al. 1993)
prop_dat$LRA1 <- prop_dat$LR1 - log(0.5); # 0.5 = (1/8)/(1/4)
prop_dat$LRA2 <- prop_dat$LR2 - log(1); # 1 = (1/4)/(1/4)
prop_dat$LRA3 <- prop_dat$LR3 - log(1);
prop_dat$LRA4 <- prop_dat$LR4 - log(0.5);

#########################################
######## GET TRACKING ACCURACY ##########
#########################################

# Second step - Apply function again to sum total count over all experiments
non_gradient_count <- combined_rows(non_gradient_separate)
acute_count <- combined_rows(acute_separate)
final_count <- combined_rows(final_separate)

# Calculate total missing counts (~ Ethovision tracking error)
if(exists("number_treatments")) {
  for(i in 1:length(treatments)) {
  non_gradient_treatment_count <- combined_rows(non_gradient_separate[str_detect(names(non_gradient_separate), treatments[i])])
  non_gradient_treatment_total_count <- sum(non_gradient_treatment_count[,zones_1_8])
  non_gradient_treatment_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases / length(treatments))) - non_gradient_treatment_total_count
  non_gradient_treatment_percent_missed <- 100 * non_gradient_treatment_missing_counts / non_gradient_treatment_total_count; 
  
  print(paste(round(non_gradient_treatment_percent_missed, digits = 2), "% missed tracks during non-gradient phase,", treatments[i],"treatment", sep = " "))
  }
  
  for(i in 1:length(treatments)) {
    acute_treatment_count <- combined_rows(acute_separate[str_detect(names(acute_separate), treatments[i])])
    acute_treatment_total_count <- sum(acute_treatment_count[,zones_1_8])
    acute_treatment_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases / length(treatments))) - acute_treatment_total_count
    acute_treatment_percent_missed <- 100 * acute_treatment_missing_counts / acute_treatment_total_count; 
    
    print(paste(round(acute_treatment_percent_missed, digits = 2), "% missed tracks during acute phase,", treatments[i],"treatment", sep = " "))
  }
  
  for(i in 1:length(treatments)) {
    final_treatment_count <- combined_rows(final_separate[str_detect(names(final_separate), treatments[i])])
    final_treatment_total_count <- sum(final_treatment_count[,zones_1_8])
    final_treatment_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases / length(treatments))) - final_treatment_total_count
    final_treatment_percent_missed <- 100 * final_treatment_missing_counts / final_treatment_total_count; 
    
    print(paste(round(final_treatment_percent_missed, digits = 2), "% missed tracks during final phase,", treatments[i],"treatment", sep = " "))
  }
  
  non_gradient_total_count <- sum(non_gradient_count[,zones_1_8])
  non_gradient_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases)) - non_gradient_total_count
  non_gradient_percent_missed <- 100 * non_gradient_missing_counts / non_gradient_total_count; 
  print(paste(round(non_gradient_percent_missed, digits = 2), "% missed tracks during non-gradient phase", sep = " "))
  
  acute_total_count <- sum(acute_count[,zones_1_8])
  acute_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases)) - acute_total_count
  acute_percent_missed <- 100 * acute_missing_counts / acute_total_count; 
  print(paste(round(acute_percent_missed, digits = 2), "% missed tracks during acute phase", sep = " "))
  
  final_total_count <- sum(final_count[,zones_1_8])
  final_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases)) - final_total_count
  final_percent_missed <- 100 * final_missing_counts / final_total_count; 
  print(paste(round(final_percent_missed, digits = 2), "% missed tracks during final phase", sep = " ")) 
  
} else {
  non_gradient_total_count <- sum(non_gradient_count[,zones_1_8])
  non_gradient_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases)) - non_gradient_total_count
  non_gradient_percent_missed <- 100 * non_gradient_missing_counts / non_gradient_total_count; 
  print(paste(round(non_gradient_percent_missed, digits = 2), "% missed tracks during non-gradient phase", sep = " "))
  
  acute_total_count <- sum(acute_count[,zones_1_8])
  acute_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases)) - acute_total_count
  acute_percent_missed <- 100 * acute_missing_counts / acute_total_count; 
  print(paste(round(acute_percent_missed, digits = 2), "% missed tracks during acute phase", sep = " "))
  
  final_total_count <- sum(final_count[,zones_1_8])
  final_missing_counts <-  (number_subjects * track_freq * length_phase * (nrow(prop_dat) / number_phases)) - final_total_count
  final_percent_missed <- 100 * final_missing_counts / final_total_count; 
  print(paste(round(final_percent_missed, digits = 2), "% missed tracks during final phase", sep = " ")) 
}

#########################################
######## ASSESS NON-RANDOM USE ##########
#########################################

# Assess evidence of non-random usage (i.e. preference) during the three observational phases:

# Select which phase and if applicable treatment to test 
non_test_phase <- "I"
non_test_treatment <- "high" # If no treatment leave blank

if(exists("number_treatments")) {
  BAdat <- subset(prop_dat, phase == non_test_phase & treatment == non_test_treatment) 
} else {
  BAdat <- subset(prop_dat, phase == non_test_phase) 
}
BALR1_non <- BAdat$LRA1
BALR2_non <- BAdat$LRA2 
BALR3_non <- BAdat$LRA3
BALR4_non <- BAdat$LRA4

CM_non <- rbind(BALR1_non, BALR2_non, BALR3_non, BALR4_non);

# 5b: compute the generalized likelihood ratio statistic which compares a model with no preference
# against a model with preference (Aebischer, 1993).
# H1MAT - reduced model (m2); matrix of raw sums of squares and cross products calculated from the matrix ‘CM’ of log-ratios
# H2MAT - general model (m1); matrix of mean-corrected sums of squares and cross-products calculated from the matrix ‘CM’ of log-ratios
H1MAT <- H2MAT <- matrix(data= NA, nrow = 4, ncol = 4); 
for (i in 1:4 ) {
  for (j in 1:4) {
    H1MAT[i, j] <- sum(CM_non[i, ]  * CM_non[j, ]);
    H2MAT[i, j] <- sum((CM_non[i, ] - mean(CM_non[i, ])) * (CM_non[j, ] - mean(CM_non[j, ])));
  };
};

# Test for general preference (deviation from random use)
# -N*ln($) $=|r1|/|r2| ~ ln(det(H2MAT)/det(H1MAT)) ~ ln(general model/reduced model) - Aebischer et al. 1993
TEST <- -ncol(CM_non) * log(det(H2MAT) / det(H1MAT)); TEST;

# By default, pchisq() gives the proportion of the distribution to the left of the value.
# To get the proportion more extreme than your difference, you can specify lower.tail = FALSE or subtract the result from 1.
# Used to compute cumulative chi square density for a vector of elements
# If significant (p<0.05), evidence for non-random use (null - no preference;  alternative - preference)
chi2_result <- 1 - pchisq(TEST, df = 4);

if(exists("number_treatments")) {
  print(paste("the chi square p value for phase", non_test_phase, "and treatment", non_test_treatment, "is =", chi2_result))
} else {
  print(paste("the chi square p value for phase", non_test_phase, "is =", chi2_result))
}

rm(H1MAT, H2MAT, TEST) ##remove variables

#################################################
######## ASSESS TREATMENTS USING MANOVA #########
#################################################

# 5c: The matrix of log-ratios may also be analysed using MANOVA, for example to assess
# evidence of treatment effects:
# MANOVA tests for mullet temperature preference experiments
# Conduct permutation test so we can ignore MANOVA assumptions


#################################################
######## RANKING TEMPERATURE PREFERENCE #########
#################################################
# Step 6: Once overall evidence of non-random use has been assessed,
# the next step is to rank compartments from least to most preferred 
# This can be done by computing a cross-table with pairwise differences between matching log-ratios of usage 
# and availability of temperature zones, and counting the number of times a particular temperature zone
# has been observed to be preferred over other temperature zones (e.g. see table 1 in Aebischer et al. 1993)

# Subset for non-gradient, acute, or final proportions; choose (I,II,III) and nutritional status (low, high) accordingly 
test_phase <- "II" 
test_treatment <- "high" # Leave commented if no treatment

if(exists("number_treatments")) {
  exp_data <- subset(prop_dat, phase == test_phase & treatment == test_treatment)
} else {
  exp_data <- subset(prop_dat, phase == phase)
}

# Proportion of available space per compartment and temperature 
c1 <- 1/8 # C1: 33 degrees: compartment 4
c2 <- 2/8 # C2: 30 degrees: compartments 3 and 5
c3 <- 2/8 # C3: 27 degrees: compartments 2 and 6
c4 <- 2/8 # C4: 24 degrees: compartments 1 and 7
c5 <- 1/8 # C5: 21 degrees: compartment 8

# Create empty list to store matrices
all_mat <- list()
for (tt in 1:nrow(exp_data)) {
  all_mat[[tt]] <- matrix(data = NA, nrow = nrow(exp_data), ncol = 5); 
}

# Fill in matrix for each experimental group (n = 5); (table 1 in Aebischer et al. 1993)
for (i in 1:nrow(exp_data)) {
  # Create blank matrix to temporarily hold data for each iteration
  rank_mat <- matrix(data = NA, nrow = 5, ncol = 5); 
  # Fill in matrix 
  # Column 1
  rank_mat[2,1] <- log(exp_data$C2[i]/exp_data$C1[i]) - log(c2/c1) 
  rank_mat[3,1] <- log(exp_data$C3[i]/exp_data$C1[i]) - log(c3/c1)
  rank_mat[4,1] <- log(exp_data$C4[i]/exp_data$C1[i]) - log(c4/c1)
  rank_mat[5,1] <- log(exp_data$C5[i]/exp_data$C1[i]) - log(c5/c1)
  
  # Column 2
  rank_mat[1,2] <- log(exp_data$C1[i]/exp_data$C2[i]) - log(c1/c2)
  rank_mat[3,2] <- log(exp_data$C3[i]/exp_data$C2[i]) - log(c3/c2)
  rank_mat[4,2] <- log(exp_data$C4[i]/exp_data$C2[i]) - log(c4/c2)
  rank_mat[5,2] <- log(exp_data$C5[i]/exp_data$C2[i]) - log(c5/c2)
  
  # Column 3
  rank_mat[1,3] <- log(exp_data$C1[i]/exp_data$C3[i]) - log(c1/c3)
  rank_mat[2,3] <- log(exp_data$C2[i]/exp_data$C3[i]) - log(c2/c3)
  rank_mat[4,3] <- log(exp_data$C4[i]/exp_data$C3[i]) - log(c4/c3)
  rank_mat[5,3] <- log(exp_data$C5[i]/exp_data$C3[i]) - log(c5/c3)
  
  # Column 4
  rank_mat[1,4] <- log(exp_data$C1[i]/exp_data$C4[i]) - log(c1/c4)
  rank_mat[2,4] <- log(exp_data$C2[i]/exp_data$C4[i]) - log(c2/c4)
  rank_mat[3,4] <- log(exp_data$C3[i]/exp_data$C4[i]) - log(c3/c4)
  rank_mat[5,4] <- log(exp_data$C5[i]/exp_data$C4[i]) - log(c5/c4)
  
  # Column 5
  rank_mat[1,5] <- log(exp_data$C1[i]/exp_data$C5[i]) - log(c1/c5)
  rank_mat[2,5] <- log(exp_data$C2[i]/exp_data$C5[i]) - log(c2/c5)
  rank_mat[3,5] <- log(exp_data$C3[i]/exp_data$C5[i]) - log(c3/c5)
  rank_mat[4,5] <- log(exp_data$C4[i]/exp_data$C5[i]) - log(c4/c5)
  
  all_mat[[i]] <- rank_mat
  rm(rank_mat) # Remove variable for next iteration
}

# Calculate element wise mean, sd, se
# Make a 3D array from list of matrices for calculation purposes
arr <- array(unlist(all_mat), c(5, 5, nrow(exp_data)))
# Calculate mean, sd, se of third dimension (i.e. mean of all experiments)
rank_mat_mean <- apply(arr, 1:2, mean)
rank_mat_sd <- apply(arr, 1:2, sd)
rank_mat_var <- apply(arr, 1:2, var)
rank_mat_se <-  rank_mat_sd / sqrt(nrow(arr))

# Sum the number of positive values for each row to rank the habitats in increasing order of preference (i.e. greatest number = most preferred)
# Add empty column to matrix to store data
rank_mat_mean <- cbind(rank_mat_mean, NA)
rank_mat_mean[is.na(rank_mat_mean)] <- 0 # Change NAs to 0 (changing the diagonal NAs to zero doesn't change calculation)
for (ii in 1:nrow(rank_mat_mean)) {
  tt <- rank_mat_mean[ii, ] > 0
  rank_mat_mean[ii, 6] <- length(tt[tt] == TRUE)
}
# Temperature ranking matrix
row.names(rank_mat_mean) <- temp_range
colnames(rank_mat_mean) <- c(temp_range, "Rankings")

# Show this as output in function
print("The temperature preference ranking matrix"); rank_mat_mean

# Test for normality: Shapiro-Wilk's method
# p value > 0.05 implies distribution of data is not significantly different from normal distribution (i.e. normally distributed).
test_norm_mean <- c(rank_mat_mean[1, c(2:5)], rank_mat_mean[2, c(3:5)], rank_mat_mean[3, c(4:5)], rank_mat_mean[4, c(5)])
test_norm_se <- c(rank_mat_se[1, c(2:5)], rank_mat_se[2, c(3:5)], rank_mat_se[3, c(4:5)], rank_mat_se[4, c(5)])

norm_mean <- shapiro.test(test_norm_mean)
norm_se <- shapiro.test(test_norm_se) 

if(exists("number_treatments")) {
  print("Test for normality: Shapiro-Wilk's method. A p value > 0.05 implies distribution of data is not significantly different from normal distribution (i.e. normally distributed)")
  print(paste("the MEAN p value for phase", test_phase, "and treatment", test_treatment, "is =", norm_mean$p.value))
  print(paste("the STANDARD ERROR p value for phase", test_phase, "and treatment", test_treatment, "is =", norm_se$p.value))
} else {
  print("Test for normality: Shapiro-Wilk's method. A p value > 0.05 implies distribution of data is not significantly different from normal distribution (i.e. normally distributed)")
  print(paste("the MEAN p value for phase", test_phase, "is =", norm_mean$p.value))
  print(paste("the STANDARD ERROR p value for phase", test_phase, "is =", norm_se$p.value))
}

#################################################
########## TEST RANKING SIGNIFICANCE ############
#################################################

# Test significance between compartments ####
# Subset of non-gradient, acute, or final proportions; choose (I,II,III) accordingly
# rank_data <- exp_data # Same subset as line 255 to reduce any potential mistakes

# Clear space in environment 
# Don't need to remove everything because in function nothing will be saved to environment (I think at least)
# rm(list=setdiff(ls(), c("rank_data", "prop_dat", "rank_mat_mean", "c1", "c2", "c3", "c4", "c5")))

# Temperature compartments comparison
compare_seq <- c(paste("compare_", temp_range[1],"_",temp_range[2], sep = ""), 
                 paste("compare_", temp_range[1],"_",temp_range[3], sep = ""),
                 paste("compare_", temp_range[1],"_",temp_range[4], sep = ""), 
                 paste("compare_", temp_range[1],"_",temp_range[5], sep = ""),
                 paste("compare_", temp_range[2],"_",temp_range[3], sep = ""), 
                 paste("compare_", temp_range[2],"_",temp_range[4], sep = ""),
                 paste("compare_", temp_range[2],"_",temp_range[5], sep = ""),
                 paste("compare_", temp_range[3],"_",temp_range[4], sep = ""),
                 paste("compare_", temp_range[3],"_",temp_range[5], sep = ""), 
                 paste("compare_", temp_range[4],"_",temp_range[5], sep = ""))
                
# Create empty data frame to store matrices
OG_mat <- list()
# Create empty data frame to store p values
OG_p_vals <- data.frame(p_values = rep(NA, 1, 10))
# Rename rows corresponding to temperature compartment comparisons
row.names(OG_p_vals) <- compare_seq

# Run loop to calculate test statistic and resulting p value for each temperature compartment comparison and experimental phase
for (ttt in 1:length(compare_seq)) {
  # Counter
  test <- compare_seq[ttt] 
  for (xxx in 1:nrow(exp_data)) { 
    # Create empty data frame to store loop value
    OG_mat[[xxx]] <- matrix(data=NA, nrow=1, ncol=1); 
    for (i in 1:nrow(exp_data)) {
      # Create blank matrix to temporarily hold data for each iteration
      OG_rank_mat <- matrix(data=NA, nrow=1, ncol=1); 
      
      # Fill in matrix 
      if (test == compare_seq[1]) {
        OG_rank_mat[1,1] <- log(exp_data$C1[i]/exp_data$C2[i]) - log(c1/c2) 
      } else if (test == compare_seq[2]) {
        OG_rank_mat[1,1] <- log(exp_data$C1[i]/exp_data$C3[i]) - log(c1/c3) 
      } else if (test == compare_seq[3]) {
        OG_rank_mat[1,1] <- log(exp_data$C1[i]/exp_data$C4[i]) - log(c1/c4) 
      } else if (test == compare_seq[4]) {
        OG_rank_mat[1,1] <- log(exp_data$C1[i]/exp_data$C5[i]) - log(c1/c5) 
      } else if (test == compare_seq[5]) {
        OG_rank_mat[1,1] <- log(exp_data$C2[i]/exp_data$C3[i]) - log(c2/c3)
      } else if (test == compare_seq[6]) {
        OG_rank_mat[1,1] <- log(exp_data$C2[i]/exp_data$C4[i]) - log(c2/c4) 
      } else if (test == compare_seq[7]) {
        OG_rank_mat[1,1] <- log(exp_data$C2[i]/exp_data$C5[i]) - log(c2/c5) 
      } else if (test == compare_seq[8]) {
        OG_rank_mat[1,1] <- log(exp_data$C3[i]/exp_data$C4[i]) - log(c3/c4) 
      } else if (test == compare_seq[9]) {
        OG_rank_mat[1,1] <- log(exp_data$C3[i]/exp_data$C5[i]) - log(c3/c5) 
      } else if (test == compare_seq[10]) {
        OG_rank_mat[1,1] <- log(exp_data$C4[i]/exp_data$C5[i]) - log(c4/c5) 
      }
      OG_mat[[i]] <- OG_rank_mat
      rm(OG_rank_mat) # Remove variable for next iteration
      
      # Calculate element wise mean, sd, se
      # Make a 3D array from list of matrices for calculation purposes
      OG_arr <- array(unlist(OG_mat), c(1, 1, nrow(exp_data)))
      # Get mean, sd, se of third dimension (i.e. mean of all experiments)
      OG_mean <- apply(OG_arr, 1:2, mean)
      OG_sd <- apply(OG_arr, 1:2, sd)
      OG_var <- apply(OG_arr, 1:2, var)
      OG_se <- OG_sd / sqrt(nrow(exp_data))
      OG_test_stat <- abs(OG_mean / OG_se)
      
      # Calculate P value from test statistic (T score = mean / se)
      OG_p_vals[ttt, 1] <- 2 * pt(q = OG_test_stat, df = 4, lower.tail = FALSE)
      rm(OG_test_stat)
    } 
  }
}
# P values for chosen phase and if applicable treatment compartment comparison
OG_p_vals <- format(OG_p_vals, scientific = FALSE)
print("Results of significance testing between compartments, p value calculated from test statistic (T score = mean / se)"); OG_p_vals

# Clear space in environment
rm(list=setdiff(ls(), c("prop_dat")))



