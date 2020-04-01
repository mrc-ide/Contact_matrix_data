

library(tidyverse); library(readxl); library(socialmixr);

setwd("your folder")


##### MATRICIES FROM SOCIALMIXR #####
Belgium_matrix<-contact_matrix(polymod, countries = "Belgium", age.limits = seq(0, 75,by=5))$matrix
Finland_matrix<-contact_matrix(polymod, countries = "Finland", age.limits = seq(0, 75,by=5))$matrix
Germany_matrix<-contact_matrix(polymod, countries = "Germany", age.limits = seq(0, 75,by=5))$matrix
Italy_matrix<-contact_matrix(polymod, countries = "Italy", age.limits = seq(0, 75,by=5))$matrix
Luxembourg_matrix<-contact_matrix(polymod, countries = "Luxembourg", age.limits = seq(0, 75,by=5))$matrix
Netherlands_matrix<-contact_matrix(polymod, countries = "Netherlands", age.limits = seq(0, 75,by=5))$matrix
Poland_matrix<-contact_matrix(polymod, countries = "Poland", age.limits = seq(0, 75,by=5))$matrix
UK_matrix<-contact_matrix(polymod, countries = "United Kingdom", age.limits = seq(0, 75,by=5))$matrix
France_matrix <- contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1157918")), age.limits = seq(0, 75,by=5))$matrix
HK_matrix <- contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1165561")), age.limits = seq(0, 75,by=5))$matrix
poly_ages<-seq(0,80,by=5)

Peru_matrix<-contact_matrix(clean(get_survey("https://doi.org/10.5281/zenodo.1095664")),age.limits = c(seq(0, 65,by=5)))$matrix
Peru_ages<-c(seq(0,65,by=5),80)

##### GET INDIA CONTACT MATRIX (https://pdfs.semanticscholar.org/844f/1baaaa84a7c1bd9fe26536bca068911784d2.pdf)#####
India_tib<-tbl_df(read.csv("IndiaContactMixing_deidentified.csv"))
India_tib$India_respondent_cat<-floor(India_tib$respondent_age/5)+1
India_tib$India_respondent_cat[India_tib$India_respondent_cat>16]=16
India_tib$India_contact_cat<-floor(India_tib$d4age/5)+1
India_tib$India_contact_cat[India_tib$India_contact_cat>16]=16

India_no_na<-India_tib[!is.na(India_tib$India_respondent_cat),]
resp_Age<-array(dim=length(unique(India_no_na$pid)))
for(i in 1:length(resp_Age)){
  resp_Age[i]=India_no_na$India_respondent_cat[which(India_no_na$pid==unique(India_no_na$pid)[i])][1]
}

contact_mat=array(dim=c(16,16))
tot_age_cat<-array(dim=c(16))

for(i in 1:16){
  tot_age_cat[i]=length(resp_Age[resp_Age==i])
  for(j in 1:16){
    contact_mat[i,j]<-length(India_tib$India_contact_cat[India_tib$India_respondent_cat==i&India_tib$India_contact_cat==j])
  }
}

India_matrix<-contact_mat
for(i in 1:16){
  India_matrix[,i]<-contact_mat[,i]/tot_age_cat
}

India_ages<-seq(0,80,by=5)

####GET MATRICES FROM LITERATURE #####
China_matrix<-read_xlsx("contact_matrix_data.xlsx",sheet="China_matrix",col_names=F)
China_ages<-unlist(read_xlsx("contact_matrix_data.xlsx",sheet="China_ages",col_names=F))


Kenya_matrix<-read_xlsx("contact_matrix_data.xlsx",sheet="Kenya_matrix",col_names=F)
Kenya_ages<-unlist(read_xlsx("contact_matrix_data.xlsx",sheet="Kenya_ages",col_names=F))

Russia_matrix<-read_xlsx("contact_matrix_data.xlsx",sheet="Russia_matrix",col_names=F)
Russia_ages<-unlist(read_xlsx("contact_matrix_data.xlsx",sheet="Russia_ages",col_names=F))

South_Africa_matrix<-read_xlsx("contact_matrix_data.xlsx",sheet="South_Africa_matrix",col_names=F)
SA_ages<-unlist(read_xlsx("contact_matrix_data.xlsx",sheet="South_Africa_ages",col_names=F))

Uganda_matrix<-read_xlsx("contact_matrix_data.xlsx",sheet="Uganda_matrix",col_names=F)
Uganda_ages<-unlist(read_xlsx("contact_matrix_data.xlsx",sheet="Uganda_ages",col_names=F))

Zimbabwe_matrix<-read_xlsx("contact_matrix_data.xlsx",sheet="Zimbabwe_matrix",col_names=F)
Zimbabwe_ages<-unlist(read_xlsx("contact_matrix_data.xlsx",sheet="Zimbabwe_ages",col_names=F))


### GET RELEVANT DEMOGRAPHY INPUTS   ###
country_demogs<-read.csv("country_inputs.csv",header=T)
Belgium_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Belgium",7:21],sum(country_demogs[country_demogs$Country_or_region=="Belgium",22:27])))
Finland_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Finland",7:21],sum(country_demogs[country_demogs$Country_or_region=="Finland",22:27])))
Germany_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Germany",7:21],sum(country_demogs[country_demogs$Country_or_region=="Germany",22:27])))
Italy_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Italy",7:21],sum(country_demogs[country_demogs$Country_or_region=="Italy",22:27])))
Luxembourg_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Luxembourg",7:21],sum(country_demogs[country_demogs$Country_or_region=="Luxembourg",22:27])))
Netherlands_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Netherlands",7:21],sum(country_demogs[country_demogs$Country_or_region=="Netherlands",22:27])))
Poland_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Poland",7:21],sum(country_demogs[country_demogs$Country_or_region=="Poland",22:27])))
UK_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="United Kingdom",7:21],sum(country_demogs[country_demogs$Country_or_region=="United Kingdom",22:27])))
France_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="France",7:21],sum(country_demogs[country_demogs$Country_or_region=="France",22:27])))
HK_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Hong Kong SAR, China",7:21],sum(country_demogs[country_demogs$Country_or_region=="Hong Kong SAR, China",22:27])))

China_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="China",7:21],sum(country_demogs[country_demogs$Country_or_region=="China",22:27])))
Peru_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Peru",7:19],sum(country_demogs[country_demogs$Country_or_region=="Peru",20:27])))
Russia_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Russian Federation",7:17],sum(country_demogs[country_demogs$Country_or_region=="Russian Federation",18:27])))
SA_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="South Africa",7:15],sum(country_demogs[country_demogs$Country_or_region=="South Africa",16:27])))

Kenya_demog_fine<-unlist(c(country_demogs[country_demogs$Country_or_region=="Kenya",7:21],sum(country_demogs[country_demogs$Country_or_region=="Kenya",22:27])))
Kenya_demog<-c(Kenya_demog_fine[1],sum(Kenya_demog_fine[2:3]),Kenya_demog_fine[4],sum(Kenya_demog_fine[5:11]),sum(Kenya_demog_fine[12:16]))
India_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="India",7:21],sum(country_demogs[country_demogs$Country_or_region=="India",22:27])))
Uganda_demog_fine<-unlist(c(country_demogs[country_demogs$Country_or_region=="Uganda",7:21],sum(country_demogs[country_demogs$Country_or_region=="Uganda",22:27])))
Uganda_demog<-c(Uganda_demog_fine[1:3],sum(Uganda_demog_fine[4:5]),sum(Uganda_demog_fine[6:7]),sum(Uganda_demog_fine[8:9]),sum(Uganda_demog_fine[10:11]),sum(Uganda_demog_fine[12:13]),sum(Uganda_demog_fine[14:16]))
Zimbabwe_demog<-unlist(c(country_demogs[country_demogs$Country_or_region=="Zimbabwe",7:21],sum(country_demogs[country_demogs$Country_or_region=="Zimbabwe",22:27])))

