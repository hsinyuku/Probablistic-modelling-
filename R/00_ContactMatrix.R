# ----------------------------------------------------------------------------#
# ContactMatrix.R
# 
# This script produces two contact matrices and the corresponding graphs: one
# for China, the other for Europe (used in all European regions)
# ----------------------------------------------------------------------------#

# list_surveys()
   # loaded from the socialmixr-package; lists available surveys

#### Get 2 CMs from zenodo ####
 
# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
source("setup.R")
# set this to TRUE if you want visual inspection of parameters
visualise = F
# ----------------------------------------------------------------------------#
 

# ----------------------------------------------------------------------------#
# contact matrix from Shanghai - used for Hubei ####
# ----------------------------------------------------------------------------#

# Making sure we do not download the survey over and over again. Execute the
# according lines individually to force a download.
{
  if (!file.exists("data/contact_matrix_shanghai.Rds")) {
    print("Downloading contact matrix:")
    shanghai_survey <- get_survey("https://doi.org/10.5281/zenodo.3366396")
    print("Saving contact matrix to data/contact_matrix_shanghai.Rds:")
    saveRDS(shanghai_survey, "data/contact_matrix_shanghai.Rds")
  } else if (!exists("shanghai_survey")) {
    print("Loading contact matrix from data/contact_matrix_shanghai.Rds")
    shanghai_survey <- readRDS("data/contact_matrix_shanghai.Rds")
  } else {
    print("shanghai_survey already loaded")
  }
}

m <- contact_matrix(survey = shanghai_survey, 
                    age.limits=c(0, 10, 20, 30, 40, 50, 60, 70, 80), symmetric = TRUE)

if (visualise) {
  tibble(contacts=as.numeric(m$matrix)) %>%
    mutate(age1=rep(1:9,9),age2=rep(1:9,each=9)) %>%
    ggplot() +
    geom_tile(aes(x=age2, y=age1, fill=contacts))
}

# the actual contact matrix
m <- structure(m$matrix,dim=c(9,9))

paste(t(m),sep="' '", collapse=", ")  
# This result is hard-wired in run_model16_China
# to get a vector to export to other scripts:
contact_matrix_Hubei <- c(t(m))

# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# POLyMOD contact matrix - used for European regions ####
# ----------------------------------------------------------------------------#

{
  if (!file.exists("data/contact_matrix_POLYMOD.Rds")) {
    print("Downloading contact matrix for European regions.")
    europe_survey <- get_survey("https://doi.org/10.5281/zenodo.1043437")
    print("Saving contact matrix to data/contact_matrix_POLYMOD.Rds.")
    saveRDS(europe_survey, "data/contact_matrix_POLYMOD.Rds")
  } else if (!exists("europe_survey")) {
    print("Loading contact matrix from data/contact_matrix_POLYMOD.Rds")
    europe_survey <- readRDS("data/contact_matrix_POLYMOD.Rds")
  } else {
    print("Object europe_survey already loaded")
  }
}

m <- contact_matrix(survey = europe_survey,
                    age.limits=c(0,10,20,30,40,50,60,70,80),
                    symmetric = TRUE)
if (visualise) {
  tibble(contacts=as.numeric(m$matrix)) %>%
    mutate(age1=rep(1:9,9),age2=rep(1:9,each=9)) %>%
    ggplot() +
    geom_tile(aes(x=age2,y=age1,fill=contacts))
}

m <- structure(m$matrix,dim=c(9,9))
paste(t(m), sep="' '", collapse=", ")  
# This result is hard-wired in run_model16 for all european countries
# to get a vector to export to other scripts:
contact_matrix_POLYMOD <- c(t(m))

remove(m, contact_matrix_Hubei, contact_matrix_POLYMOD)
