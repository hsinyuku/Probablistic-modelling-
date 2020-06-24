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
                    age.limits=c(0,10,20,30,40,50,60,70,80),symmetric = TRUE)
if (visualise) {
  tibble(contacts=as.numeric(m$matrix)) %>%
    mutate(age1=rep(1:9,9),age2=rep(1:9,each=9)) %>%
    ggplot() +
    geom_tile(aes(x=age2,y=age1,fill=contacts))
}

contact_cn = structure(m$matrix,dim=c(9,9))
apply(contact_cn,1,sum)
# sum(contact)
paste(t(contact_cn),sep="' '", collapse=", ")  
# This result is hard-wired in run_model16_China
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# POLyMOD contact matrix - used for European regions ####
# ----------------------------------------------------------------------------#
europe_survey <- get_survey("https://doi.org/10.5281/zenodo.1043437")
m <- contact_matrix(survey = italy_survey, age.limits=c(0,10,20,30,40,50,60,70,80),symmetric = TRUE)
data.frame(contacts=as.numeric(m$matrix)) %>%
  tbl_df() %>%
  mutate(age1=rep(1:9,9),age2=rep(1:9,each=9)) %>%
  ggplot() +
  geom_tile(aes(x=age2,y=age1,fill=contacts))
contact_it = structure(m$matrix,dim=c(9,9))
apply(contact_it,1,mean)
# sum(contact)
paste(t(contact_it), sep="' '", collapse=", ")  
# This result is hard-wired in run_model16 for all european countries
# ----------------------------------------------------------------------------#

### Draw 2 CM graphs ##### 

  # The contact matrix graph for China ----
  contacts = matrix(contact_cn,nrow=9,byrow=T) %>%
    data.frame() %>%
    tbl_df()
  names(contacts) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
  contacts$i = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
  CN_cm = gather(contacts,"j","c",1:9) %>%
    ggplot() +
    geom_tile(aes(x=i,y=j,fill=c)) +
    scale_fill_gradient(low="darkblue",high="darkgoldenrod1") +
    labs(x="Age group",y="Age group",fill="Contacts")
  CN_cm  

  # The contact matrix graph for all europe---- 
  contacts = matrix(contact_it,nrow=9,byrow=T) %>%
    data.frame() %>%
    tbl_df()
  names(contacts) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
  contacts$i = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
  EU_cm = gather(contacts,"j","c",1:9) %>%
    ggplot() +
    geom_tile(aes(x=i,y=j,fill=c)) +
    scale_fill_gradient(low="darkblue",high="darkgoldenrod1") +
    labs(x="Age group",y="Age group",fill="Contacts")
  EU_cm  