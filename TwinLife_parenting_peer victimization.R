
###1. =========Data preparation==========

####______1.1. Selecting the variables of interest______

library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("foreign")
library(foreign)



# Set the working directory
setwd ('/Users/soncioiu/Documents/1. OXFORD PROJECTS/4. COLLABORATION ARTICLES/TwinLife project/ZA6701_TwinLife_v4-0/SPSS_en')


#read the twin data
tl_data1 <-read.spss('ZA6701_en_zygosity_v4-0-0.sav', to.data.frame = TRUE)
tl_data2 <-read.spss('ZA6701_en_person_wid3_v4-0-0.sav', to.data.frame = TRUE)
tl_data4 <-read.spss('ZA6701_en_person_wid1_v4-0-0.sav', to.data.frame = TRUE)

#variables of interest

#fid = family ID
#pid = person ID
#wid = data collection, we want wid1 (F2F1, parenting variables) and wid3 (F2F2, bullying variables)
#cgr = twin birth cohort, we are using cgr2 (born 2003/2004)
#ptypt = twin one respondent
#ptypu = twin two respondent 
#zyg0102 = zygosity, monozygotic (1) or dizygotic (2)




#create a subset of the data with variables of interest
ss_twin_b <- subset (tl_data4, select=c(fid, pid, wid,cgr,ptyp,zyg0102, sex,                                            fam0100, hpc,inc0411,dia0900, dia1000, dia1100, 
                                        int0100, int0101, int0102, int0103, int0104,
                                        int0105, int0106, int0107, int0109,
                                        
                                        ext0100, ext0101, ext0102, ext0103, ext0104, 
                                        ext0105, ext0106, ext0107, ext0108, ext0109,
                                        
                                        int0100t, int0101t, int0102t, int0103t, int0104t,
                                        int0105t, int0106t, int0107t, int0109t,
                                        
                                        int0100u, int0101u, int0102u, int0103u, int0104u,
                                        int0105u, int0106u, int0107u, int0109u,
                                        
                                        ext0100t, ext0101t, ext0102t, ext0103t, ext0104t,
                                        ext0105t, ext0106t, ext0107t, ext0108t, ext0109t,
                                        
                                        ext0100u, ext0101u, ext0102u, ext0103u, ext0104u,
                                        ext0105u, ext0106u, ext0107u, ext0108u, ext0109u,
                                        
                                        par0100t, par0101t, par0102t, par0103t, 
                                        par0104t, par0105t, par0106t,
                                        par0107t, par0108t,par0109t, par0110t,
                                        par0111t, par0112t,
                                        par0100u, par0101u, par0102u, par0103u, 
                                        par0104u, par0105u, par0106u,
                                        par0107u, par0108u,par0109u, par0110u,
                                        par0111u, par0112u))

ss_twin_b$pid_num <-as.numeric(as.character(ss_twin_b$pid))

ss_twin_c <- subset (tl_data2, select=c(bul0500, bul0600, bul0700, bul0800,
                                        bul0501, bul0601, bul0701, bul0801,
                                        bul0100, bul0200, bul0300, bul0400,
                                        bul0101, bul0201, bul0301, bul0401))

ss_twin_c$pid_num <-as.numeric(as.character(tl_data2$pid))

ss_twin_c$wid2 <- tl_data2$wid
ss_twin_c$cgr2 <- tl_data2$cgr
ss_twin_c$fid2 <- tl_data2$fid

db_full1 <-merge (ss_twin_b, ss_twin_c, by='pid_num') #all=TRUE
db_full1$fid_num <- as.numeric (as.character(db_full1$fid))


#transform variables for scores into numeric values
x1<-subset (db_full1, select=c(fid_num, pid_num,
                               int0100, int0101, int0102, int0103, int0104,
                               int0105, int0106, int0107, int0109,
                               
                               ext0100, ext0101, ext0102, ext0103, ext0104, 
                               ext0105, ext0106, ext0107, ext0108, ext0109,
                               
                               int0100t, int0101t, int0102t, int0103t, int0104t,
                               int0105t, int0106t, int0107t, int0109t,
                               
                               int0100u, int0101u, int0102u, int0103u, int0104u,
                               int0105u, int0106u, int0107u, int0109u,
                               
                               ext0100t, ext0101t, ext0102t, ext0103t, ext0104t,
                               ext0105t, ext0106t, ext0107t, ext0108t, ext0109t,
                               
                               ext0100u, ext0101u, ext0102u, ext0103u, ext0104u,
                               ext0105u, ext0106u, ext0107u, ext0108u, ext0109u,
                               
                               par0100t, par0101t, par0102t, par0103t, 
                               par0104t, par0105t, par0106t,
                               par0107t, par0108t,par0109t, par0110t,
                               par0111t, par0112t,
                               par0100u, par0101u, par0102u, par0103u, 
                               par0104u, par0105u, par0106u,
                               par0107u, par0108u,par0109u, par0110u,
                               par0111u, par0112u)) 


#this transforms the factors into numeric variables
parent_items1 <- mutate_if(x1, is.factor, ~ as.numeric(.x))

####_________________OUTCOME VARIABLE______________
####Harsh parenting by mother of twin 1 and twin 2
####_______________________________________________

#### HARSH PARENTING
### Cronbach Alpha calculation 

# Psychological Control: par0104(t/u/s), par0105(t/u/s), par0106(t/u/s)
# par0104(t/u/s): If <name of child> does something against your will, you punish him/her.
# par0105(t/u/s): You are disappointed and sad when <name of child> misbehaved.
# par0106(t/u/s): You make it clear to <name of child> that he/she is not to break the rules or question your decisions.

# Negative Communication: par0107(t/u/s), par0108(t/u/s)
# par0107(t/u/s): You yell at <name of child>because he/she did something wrong.
# par0108(t/u/s): You scold <name of child> because you are angry at him/her.

#Inconsistent Parenting: par0111(t/u/s), par0112(t/u/s)
#par0111(t/u/s): You threaten <name of child> with a punishment but don't actually follow through.
#par0112(t/u/s): You find it hard to set and keep consistent rules for <name of child>.

#coded in negative direction -keep it like this
#par0104(t/u/s), par0105(t/u/s), par0106(t/u/s),
#par0107(t/u/s), par0108(t/u/s)
#par0111(t/u/s), par0112(t/u/s)


#create a new column for both twin one (t) and twin two (u) where we sum all the items of the negative parenting scale


##twin one of the pair
parent_items1$harshpar_bymom_t <- sum.n(parent_items1[,
                                                      c('par0104t', 'par0105t', 'par0106t',
                                                        'par0107t', 'par0108t',
                                                        'par0111t', 'par0112t')],7)

#twin two of the pair
parent_items1$harshpar_bymom_u <- sum.n(parent_items1[,
                                                      c('par0104u', 'par0105u', 'par0106u',
                                                        'par0107u', 'par0108u',
                                                        'par0111u', 'par0112u')],7)


###_______subscales__________

#create new column for both younger and older twins where we sum the scores for each subscale (psychological control, negative communication, inconsistent parenting) 

##twin one of the pair
parent_items1$ctrl_byparent_t <- sum.n(parent_items1[,c('par0104t', 'par0105t', 'par0106t')],3)
parent_items1$negcom_byparent_t <- sum.n(parent_items1[,c('par0107t', 'par0108t')],2)
parent_items1$incnsist_byparent_t <- sum.n(parent_items1[,c('par0111t', 'par0112t')],2)

#twin two of the pair
parent_items1$ctrl_byparent_u <- sum.n(parent_items1[,c('par0104u', 'par0105u', 'par0106u')],3)
parent_items1$negcom_byparent_u <- sum.n(parent_items1[,c('par0107u', 'par0108u')],2)
parent_items1$incnsist_byparent_u <- sum.n(parent_items1[,c('par0111u', 'par0112u')],2)
###_________________

#creates two new columns for each twin with 0 if the data is missing and 1 if the data is available in the harshpar_bymom columns
#parent_items1$harshpar_bymom_t_avail <- ifelse (is.na(parent_items1$harshpar_bymom_t), 0, 1)
#parent_items1$harshpar_bymom_u_avail <- ifelse (is.na(parent_items1$harshpar_bymom_u), 0, 1)


########___________EXPOSURE_____________
#######____________VICTIMIZATION________


######VICTIMIZATION COHORT 1####
#######COHORT 1######

#convert victimization items in cohort 1 to numeric
db_full1$vic_it1_coh1<- as.numeric(db_full1$bul0500)
db_full1$vic_it2_coh1<- as.numeric(db_full1$bul0600)
db_full1$vic_it3_coh1<- as.numeric(db_full1$bul0700)
db_full1$vic_it4_coh1<- as.numeric(db_full1$bul0800)

#sum the victimization items in cohort 1
db_full1$victim_sum_coh1 <- sum.n(db_full1[,c('vic_it1_coh1', 'vic_it2_coh1', 'vic_it3_coh1', 'vic_it4_coh1')],4)

#standardize the sum
db_full1$victim_sum_coh1_scale <- as.numeric (scale (db_full1$victim_sum_coh1, center=TRUE, scale=TRUE))


######VICTIMIZATION COHORT 2####
#######COHORT 2####
##victimization frequency cohort 2

#convert victimization items in cohort 2 to numeric
db_full1$vic_it1_coh2<- as.numeric(db_full1$bul0100)
db_full1$vic_it2_coh2<- as.numeric(db_full1$bul0200)
db_full1$vic_it3_coh2<- as.numeric(db_full1$bul0300)
db_full1$vic_it4_coh2<- as.numeric(db_full1$bul0400)

#sum the victimization items in cohort 2
db_full1$victim_sum_coh2 <- sum.n(db_full1[,c('vic_it1_coh2', 'vic_it2_coh2', 'vic_it3_coh2', 'vic_it4_coh2')],4)

#standardize the sum
db_full1$victim_sum_coh2_scale <- as.numeric(scale (db_full1$victim_sum_coh2, center=TRUE, scale=TRUE))


#add parenting reported by mother for each twin into db_full1
db_full1$harshpar_bymom_t<-parent_items1$harshpar_bymom_t
db_full1$harshpar_bymom_u<-parent_items1$harshpar_bymom_u


#add the parenting subscales for each twin into db_full1
db_full1$ctrl_byparent_t <- parent_items1$ctrl_byparent_t
db_full1$negcom_byparent_t <-parent_items1$negcom_byparent_t
db_full1$incnsist_byparent_t <-parent_items1$incnsist_byparent_t

db_full1$ctrl_byparent_u <- parent_items1$ctrl_byparent_u
db_full1$negcom_byparent_u <-parent_items1$negcom_byparent_u
db_full1$incnsist_byparent_u <-parent_items1$incnsist_byparent_u


#calculate the difference in negative parenting between twins
db_full1$harshpar_bymom_diff_t1_t2=diff.n(db_full1,c('harshpar_bymom_t', 'harshpar_bymom_u'),2)


#calculate the difference in negative parenting subscales between twins
db_full1$ctrl_byparent_diff_t1_t2=diff.n(db_full1,c('ctrl_byparent_t', 'ctrl_byparent_u'), 2)
db_full1$negcom_byparent_diff_t1_t2=diff.n(db_full1,c('negcom_byparent_t', 'negcom_byparent_u'), 2)
db_full1$incnsist_byparent_diff_t1_t2=diff.n(db_full1,c('incnsist_byparent_t', 'incnsist_byparent_u'), 2)




#####COVARIATES: internalizing, externalizing symptoms - deriving the score
###_________________

#the "int..." variables take the values 1, 2 and 3. high score means high internalizing
#int0106: I have one good friend or more. (i)
#int0107: In general I am popular with others. (i)

#REVERSE SCORING
#this function reverses the score of the "int.." variables
reverse_score <- function(x) {
  ifelse(x %in% 1:3, 4 - x, NA)
}
#we choose which columns to reverse 
columns_to_reverse <- c("int0106", "int0107", "int0106t", "int0107t", "int0106u", "int0107u")

#we apply the function and add the resulting variables in new columns:

parent_items1 <- parent_items1 %>%
  mutate(across(all_of(columns_to_reverse), ~ reverse_score(.), .names = "{.col}_rev"))


#create three new columns where we sum internalizing variables -
parent_items1$int_self_c2 <- sum.n(parent_items1[,
                                                 c('int0100', 'int0101', 'int0102', 'int0103', 'int0104',
                                                   'int0105', 'int0106_rev', 'int0107_rev', 'int0109')],9)

parent_items1$int_par_c1_t <- sum.n(parent_items1[,
                                                  c('int0100t', 'int0101t', 'int0102t', 'int0103t', 'int0104t',
                                                    'int0105t', 'int0106t_rev', 'int0107t_rev', 'int0109t')],9)

parent_items1$int_par_c1_u <- sum.n(parent_items1[,
                                                  c('int0100u', 'int0101u', 'int0102u', 'int0103u', 'int0104u',
                                                    'int0105u', 'int0106u_rev', 'int0107u_rev', 'int0109u')],9)


#this function reverses the score of the ext variables
reverse_score <- function(x) {
  ifelse(x %in% 1:3, 4 - x, NA)
}
#we choose which columns to reverse 
columns_to_reverse <- c("ext0103", "ext0104", "ext0106", "ext0103t", "ext0104t", "ext0106t", "ext0103u", "ext0104u", "ext0106u")

#we apply the function and add the resulting variables in new columns:
parent_items1 <- parent_items1 %>%
  mutate(across(all_of(columns_to_reverse), ~ reverse_score(.), .names = "{.col}_rev"))

#create three new columns where we sum externalizing variables
parent_items1$ext_self_c2 <- sum.n(parent_items1[,
                                                 c('ext0100', 'ext0101', 'ext0102', 'ext0103_rev', 'ext0104_rev', 
                                                   'ext0105', 'ext0106_rev', 'ext0107', 'ext0108', 'ext0109')],10)

parent_items1$ext_par_c1_t <- sum.n(parent_items1[,
                                                  c('ext0100t', 'ext0101t', 'ext0102t', 'ext0103t_rev', 'ext0104t_rev', 
                                                    'ext0105t', 'ext0106t_rev', 'ext0107t', 'ext0108t', 'ext0109t')],10)

parent_items1$ext_par_c1_u <- sum.n(parent_items1[,
                                                  c('ext0100u', 'ext0101u', 'ext0102u', 'ext0103u_rev', 'ext0104u_rev', 
                                                    'ext0105u', 'ext0106u_rev', 'ext0107u', 'ext0108u', 'ext0109u')],10)



####add covariates in the main dataset

db_full1$ext_par_c1_t <-parent_items1$ext_par_c1_t 
db_full1$ext_par_c1_u <-parent_items1$ext_par_c1_u 

#difference between twins in ext par (externalizing symptoms as rated by mother)
db_full1$ext_par_c1_diff_t1_t2=diff.n(db_full1,c('ext_par_c1_t', 'ext_par_c1_u'), 2)


#create a new column to indicate whether the value in the original column is missing or not
db_full1$ext_par_c1_t_avail<- ifelse (is.na(db_full1$ext_par_c1_t), 0, 1)
db_full1$ext_par_c1_u_avail<- ifelse (is.na(db_full1$ext_par_c1_u), 0, 1)


db_full1$int_par_c1_t <-parent_items1$int_par_c1_t 
db_full1$int_par_c1_u <-parent_items1$int_par_c1_u 

# calculate the difference between twins in int par - what is int par?
db_full1$int_par_c1_diff_t1_t2=diff.n(db_full1,c('int_par_c1_t', 'int_par_c1_u'), 2)


#Creates a new column, ending in 'avail', to indicate whether the value in the original column is missing (NA) or not
#db_full1$int_par_c1_t_avail<- ifelse (is.na(db_full1$int_par_c1_t), 0, 1)
#db_full1$int_par_c1_u_avail<- ifelse (is.na(db_full1$int_par_c1_u), 0, 1)


db_full1$ext_self_c2 <-parent_items1$ext_self_c2
#db_full1$ext_self_c2_avail<- ifelse (is.na(db_full1$ext_self_c2), 0, 1)

db_full1$int_self_c2 <-parent_items1$int_self_c2
#db_full1$int_self_c2_avail<- ifelse (is.na(db_full1$int_self_c2), 0, 1)


### income 

db_full1$income <- as.numeric (as.character(db_full1$inc0411))
#db_full1$income_outliers1 <- ifelse(db_full1$income>=20000, 1,0)
#db_full1$income_outliers2 <- ifelse(db_full1$income>=15000, 1,0)

db_full1$income2 <-ifelse (db_full1$income>=15000, NA, db_full1$income)
db_full1$income3 <-ifelse (db_full1$income>=10000, NA, db_full1$income)
#bb$income <-bb$income2

#######maternal mental health: depression or anxiety or alcohol

db_full1$mom_MH_yes <- ifelse (db_full1$dia0900=="1: mentioned"|db_full1$dia1000=="1: mentioned"| db_full1$dia1100=="1: mentioned",1,0)


####cohabiting
db_full1$cohabit_num <- as.numeric (db_full1$fam0100)
db_full1$cohabit_yes <- ifelse (db_full1$cohabit_num==1|db_full1$cohabit_num==3, 1, 0)

####number of people in the household
db_full1$ppl_hh <- db_full1$hpc


#################preapre the datasets used for the analyses: cohort 1 & 2
db_full1_coh1 <- subset (db_full1, cgr=='1: twins born 2009/2010' & 
                           (db_full1$ptyp=='1: firstborn twin - t'| db_full1$ptyp=='2: secondborn twin - u'))
db_full1_coh2 <- subset (db_full1, cgr=='2: twins born 2003/2004'& 
                           (db_full1$ptyp=='1: firstborn twin - t'| db_full1$ptyp=='2: secondborn twin - u'))






#####______1.2. Restructuring the dataset containing mother-reported variables__________________

###Several parenting-related variables (e.g., harsh parenting, negative communication, psychological control, 
#inconsistent parenting, externalizing, and internalizing behaviour) are originally stored in separate columns for each twin.

##### MOVE COLUMNS TO ROWS######
####_____________________________________________#######
####__________MOTHER REPORTED VARIABLES__________#######
db_full1_twins_mom <- subset (db_full1,  db_full1$ptyp=='1: firstborn twin - t'| db_full1$ptyp=='2: secondborn twin - u' |
                                db_full1$ptyp=='300: mother - m')


check_par_twins <- subset (db_full1_twins_mom, select=c(fid_num, pid_num, ptyp, wid, cgr,  zyg0102,
                                                        harshpar_bymom_t, harshpar_bymom_u, harshpar_bymom_diff_t1_t2,
                                                        
                                                        negcom_byparent_t, negcom_byparent_u, negcom_byparent_diff_t1_t2, 
                                                        ctrl_byparent_t, ctrl_byparent_u, ctrl_byparent_diff_t1_t2,
                                                        
                                                        incnsist_byparent_t, incnsist_byparent_u, incnsist_byparent_diff_t1_t2,
                                                        
                                                        ext_par_c1_t, ext_par_c1_u, ext_par_c1_diff_t1_t2,
                                                        int_par_c1_t, int_par_c1_u, int_par_c1_diff_t1_t2)) 


#twin_covar <- subset (db_full1, select=c(fid_num, pid_num, wid,cgr,ptyp,zyg0102, sex,
#                 income, income2,income3,mom_MH_yes,cohabit_yes,ppl_hh))


#twin_covar <- subset (twin_covar,  twin_covar$ptyp=='1: firstborn twin - t'| twin_covar$ptyp=='2: secondborn twin - u' |
#                     twin_covar$ptyp=='300: mother - m')
#twin_covar$pid_num <-as.numeric(as.character(twin_covar$pid))
#twin_covar$fid_num <-as.numeric(as.character(twin_covar$fid))


#These loops identify pairs of twins and their mother and assign the correct values by checking family ID matches.
#The script below moves these variables to rows so that each twin has a separate entry, instead of being split across columns.

# Ensure twins are correctly paired by sorting by family ID
check_par_twins <- check_par_twins[order(check_par_twins$fid_num), ]


aa<-dim (check_par_twins)[2]
yy<-which(colnames(check_par_twins) == "fid_num")
tt <- which(colnames(check_par_twins) == "harshpar_bymom_t")
uu <- which(colnames(check_par_twins) == "harshpar_bymom_u")


# 1. harsh parenting
for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+1]=check_par_twins[z,tt] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+1]=check_par_twins[z,uu] 
  }
  
  i=i+2
}

# 2. negative communication

tt <- which(colnames(check_par_twins) == "negcom_byparent_t")
uu <- which(colnames(check_par_twins) == "negcom_byparent_u")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+2]=check_par_twins[z,tt] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+2]=check_par_twins[z,uu] 
  }
  
  i=i+2
}


# 3. psychological control

tt <- which(colnames(check_par_twins) == "ctrl_byparent_t")
uu <- which(colnames(check_par_twins) == "ctrl_byparent_u")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+3]=check_par_twins[z,tt] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+3]=check_par_twins[z,uu] 
  }
  
  i=i+2
}


# 4. inconsistent parenting

tt <- which(colnames(check_par_twins) == "incnsist_byparent_t")
uu <- which(colnames(check_par_twins) == "incnsist_byparent_u")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+4]=check_par_twins[z, tt] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+4]=check_par_twins[z, uu] 
  }
  
  i=i+2
}



# 5. externalizing by mom 


tt <- which(colnames(check_par_twins) == "ext_par_c1_t")
uu <- which(colnames(check_par_twins) == "ext_par_c1_u")


for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+5]=check_par_twins[z, tt] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+5]=check_par_twins[z, uu] 
  }
  
  i=i+2
}

# 6. internalizing by mom 


tt <- which(colnames(check_par_twins) == "int_par_c1_t")
uu <- which(colnames(check_par_twins) == "int_par_c1_u")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+6]=check_par_twins[z, tt] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+6]=check_par_twins[z, uu] 
  }
  
  i=i+2
}



#####____MOVE THE DIFFERENCES T1-T2 FROM COLOUM TO ROW


tu<- which(colnames(check_par_twins) == "harshpar_bymom_diff_t1_t2")


for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+7]=check_par_twins[z,tu] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+7]=check_par_twins[z,tu] 
  }
  
  i=i+2
}


#2. negcom_byparent


tu<- which(colnames(check_par_twins) == "negcom_byparent_diff_t1_t2")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+8]=check_par_twins[z,tu] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+8]=check_par_twins[z,tu] 
  }
  
  i=i+2
}


# 3. psychological control
tu <- which(colnames(check_par_twins) == "ctrl_byparent_diff_t1_t2")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+9]=check_par_twins[z,tu] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+9]=check_par_twins[z,tu] 
  }
  
  i=i+2
}


# 4. inconsistent parenting
tu <- which(colnames(check_par_twins) == "incnsist_byparent_diff_t1_t2")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+10]=check_par_twins[z,tu] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+10]=check_par_twins[z,tu] 
  }
  
  i=i+2
}


# 5. externalizing by mother
tu <- which(colnames(check_par_twins) == "ext_par_c1_diff_t1_t2")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+11]=check_par_twins[z,tu] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+11]=check_par_twins[z,tu] 
  }
  
  i=i+2
}

# 6. internalizing by mother

tu <- which(colnames(check_par_twins) =="int_par_c1_diff_t1_t2")

for (i in 1:nrow(check_par_twins)){
  j=i+1
  z=i+2
  rev= -1*check_par_twins[z,yy]
  dif_fam=sum(check_par_twins[i,yy], rev, na.rm = TRUE)
  dif_fam2=sum(check_par_twins[j,yy], rev, na.rm = TRUE)
  if (dif_fam==0) {
    check_par_twins[i,aa+12]=check_par_twins[z,tu] 
  }
  if (dif_fam2==0) {
    check_par_twins[j,aa+12]=check_par_twins[z,tu] 
  }
  
  i=i+2
}



colnames(check_par_twins)[aa+1] <- "harshpar_bymom"
colnames(check_par_twins)[aa+2] <- "negcom_byparent"
colnames(check_par_twins)[aa+3] <- "ctrl_byparent"
colnames(check_par_twins)[aa+4] <- "incnsist_byparent"
colnames(check_par_twins)[aa+5] <- "ext_par_c1"
colnames(check_par_twins)[aa+6] <- "int_par_c1"
colnames(check_par_twins)[aa+7] <- "harshpar_bymom_diffT1T2"
colnames(check_par_twins)[aa+8] <- "negcom_bymom_diffT1T2"
colnames(check_par_twins)[aa+9] <- "ctrl_bymom_diffT1T2"
colnames(check_par_twins)[aa+10] <- "incnsist_bymom_diffT1T2"
colnames(check_par_twins)[aa+11] <- "ext_bymom_c1_diffT1T2"
colnames(check_par_twins)[aa+12] <- "int_bymom_c1_diffT1T2"


check_par_by_mom1 <-subset (check_par_twins,  check_par_twins$ptyp=='1: firstborn twin - t'| check_par_twins$ptyp=='2: secondborn twin - u')

check_par_by_mom1 <- subset (check_par_by_mom1, select= c(pid_num, harshpar_bymom,      harshpar_bymom_diffT1T2, 
                                                          negcom_byparent, negcom_bymom_diffT1T2,
                                                          ctrl_byparent,ctrl_bymom_diffT1T2,
                                                          incnsist_byparent,incnsist_bymom_diffT1T2,
                                                          ext_par_c1, ext_bymom_c1_diffT1T2,
                                                          int_par_c1, int_bymom_c1_diffT1T2))






###mom to twin: needed only for the variables that are about the mother, not about the household

mom_to_twin <- subset (db_full1, select=c(fid_num,  pid_num,ptyp,
                                          cohabit_yes, mom_MH_yes))


mom_to_twin <- subset (mom_to_twin,  mom_to_twin$ptyp=='1: firstborn twin - t'| mom_to_twin$ptyp=='2: secondborn twin - u' |mom_to_twin$ptyp=='300: mother - m')

#most likely income2
# Ensure twins are correctly paired by sorting by family ID
mom_to_twin <- mom_to_twin[order(mom_to_twin$fid_num), ]
bb<-which(colnames(mom_to_twin) == "fid_num")
dd<-which(colnames(mom_to_twin) == "pid_num")
zz<-dim (mom_to_twin)[2]

#maternal mental health
cc<-which(colnames(mom_to_twin) == "mom_MH_yes")

for (i in 1:nrow(mom_to_twin)){
  rev= -1*mom_to_twin[i,bb]
  rev2=-1*mom_to_twin[i+1,bb]
  dif_fam=sum(mom_to_twin[i+1,bb], rev, na.rm = TRUE)
  dif_fam2=sum(mom_to_twin[i+2,bb], rev, na.rm = TRUE)
  dif_fam3=sum(mom_to_twin[i+2,bb], rev2, na.rm = TRUE)
  sum_dif=sum(dif_fam, dif_fam2, dif_fam3, na.rm=TRUE)
  mom_id=-1*(mom_to_twin[i,bb]*1000+300)
  
  if (sum_dif==0){
    j=i
    z=i+2
    for (j in i:z) {
      mom_id_diff=sum(mom_id, mom_to_twin[j,dd], na.rm = TRUE)
      
      if ( mom_id_diff==0){
        mom_to_twin[i,zz+1]=mom_to_twin[j,cc]
        mom_to_twin[i+1,zz+1]=mom_to_twin[j,cc]
        mom_to_twin[i+2,zz+1]=mom_to_twin[j,cc]
      }
    }
  }
  i=i+2
}


#cohabitation with partner
cc<-which(colnames(mom_to_twin) == "cohabit_yes")

for (i in 1:nrow(mom_to_twin)){
  rev= -1*mom_to_twin[i,bb]
  rev2=-1*mom_to_twin[i+1,bb]
  dif_fam=sum(mom_to_twin[i+1,bb], rev, na.rm = TRUE)
  dif_fam2=sum(mom_to_twin[i+2,bb], rev, na.rm = TRUE)
  dif_fam3=sum(mom_to_twin[i+2,bb], rev2, na.rm = TRUE)
  sum_dif=sum(dif_fam, dif_fam2, dif_fam3, na.rm=TRUE)
  mom_id=-1*(mom_to_twin[i,bb]*1000+300)
  
  if (sum_dif==0){
    j=i
    z=i+2
    for (j in i:z) {
      mom_id_diff=sum(mom_id, mom_to_twin[j,dd], na.rm = TRUE)
      
      if ( mom_id_diff==0){
        mom_to_twin[i,zz+2]=mom_to_twin[j,cc]
        mom_to_twin[i+1,zz+2]=mom_to_twin[j,cc]
        mom_to_twin[i+2,zz+2]=mom_to_twin[j,cc]
      }
    }
  }
  i=i+2
}


colnames( mom_to_twin)[zz+1] <- "mom_MH"
colnames(mom_to_twin)[zz+2] <- "cohabit"


covar_by_mom <-subset (mom_to_twin,  mom_to_twin$ptyp=='1: firstborn twin - t'| mom_to_twin$ptyp=='2: secondborn twin - u')


db_full1_twins <- subset (db_full1,  db_full1$ptyp=='1: firstborn twin - t'| db_full1$ptyp=='2: secondborn twin - u')

db_full1_twins <-merge (db_full1_twins, check_par_by_mom1, by="pid_num", all=TRUE )

db_full1_twins$cohabit <-covar_by_mom$cohabit
db_full1_twins$mom_MH <-covar_by_mom$mom_MH

#db_full1_twinscoh1 <- subset (db_full1_twins , cgr=='1: twins born 2009/2010')
#db_full1_twinscoh2 <- subset (db_full1_twins , cgr=='2: twins born 2003/2004')




########1.3. Creating Twin-Pair Variables by Swapping Values Between Twins in a Dataset with Both Row- and Column-Wise Twin Data

#This code processes a dataset where each twin pair is represented in separate rows but also has variables structured by twin (e.g., harshpar_bymom_t1 for Twin 1). 
#Using the family ID (fid_num), it identifies co-twins and swaps their values for specific parenting and behavioural variables. The process ensures that for each measure, 
#values from Twin 1 are assigned to Twin 2 and vice versa, while maintaining the row-wise twin structure. Newly generated columns (_t2 variables) store the swapped values to
#facilitate twin-comparison analyses.



##
# Ensure twins are correctly paired by sorting by family ID
db_full1_twins <- db_full1_twins[order(db_full1_twins$fid_num), ]

zz<-dim (db_full1_twins)[2]
pp<-which(colnames(db_full1_twins) == "fid_num")

####harshpar
db_full1_twins$harshpar_bymom_t1 <- db_full1_twins$harshpar_bymom
t=zz+1
for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+1]=db_full1_twins[j,t] 
    db_full1_twins[j,t+1]=db_full1_twins[i,t] 
  }
  i=i+1
}

####incnsist_byparent
db_full1_twins$incnsist_byparent_t1 <- db_full1_twins$incnsist_byparent

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+3]=db_full1_twins[j,t+2] 
    db_full1_twins[j,t+3]=db_full1_twins[i,t+2] 
  }
  i=i+1
}


######ctrl_byparent
db_full1_twins$ctrl_byparent_t1 <- db_full1_twins$ctrl_byparent

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+5]=db_full1_twins[j,t+4] 
    db_full1_twins[j,t+5]=db_full1_twins[i,t+4] 
  }
  i=i+1
}


######neg_COMMUNICATION
db_full1_twins$negcom_byparent_t1 <- db_full1_twins$negcom_byparent

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+7]=db_full1_twins[j,t+6] 
    db_full1_twins[j,t+7]=db_full1_twins[i,t+6] 
  }
  i=i+1
}


######INTERNALIZING  COHORT 1:
db_full1_twins$int_c1 <- db_full1_twins$int_par_c1

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+9]=db_full1_twins[j,t+8] 
    db_full1_twins[j,t+9]=db_full1_twins[i,t+8] 
  }
  i=i+1
}


#####EXTERNALIZING  COHORT 1:
db_full1_twins$ext_c1 <- db_full1_twins$ext_par_c1

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+11]=db_full1_twins[j,t+10] 
    db_full1_twins[j,t+11]=db_full1_twins[i,t+10] 
  }
  i=i+1
}




#variables for which information for twin one and twin two is available on the rows, but not on the columns; so we just need to add an extra-column with tha values for twin two. we will also add an extra column for twin 1 and name it in a similar way with the twin two variable.


db_full1_twins$victim_coh1_t1 <- db_full1_twins$victim_sum_coh1

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+13]=db_full1_twins[j,t+12] 
    db_full1_twins[j,t+13]=db_full1_twins[i,t+12] 
  }
  i=i+1
}


#enter the victim coh2 variable for twin2 ()

db_full1_twins$victim_coh2_t1 <- db_full1_twins$victim_sum_coh2

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+15]=db_full1_twins[j,t+14] 
    db_full1_twins[j,t+15]=db_full1_twins[i,t+14] 
  }
  i=i+1
}


#enter the externalizing coh2 variable for twin2 ()

db_full1_twins$ext_self_c2_t1 <- db_full1_twins$ext_self_c2

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+17]=db_full1_twins[j,t+16] 
    db_full1_twins[j,t+17]=db_full1_twins[i,t+16] 
  }
  i=i+1
}

#enter the internalizing coh2 variable for twin2 ()

db_full1_twins$int_self_c2_t1 <- db_full1_twins$int_self_c2

for (i in 1:nrow(db_full1_twins)){
  j=i+1
  rev= -1*db_full1_twins[j,pp]
  dif_fam=sum(db_full1_twins[i,pp], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins[i,t+19]=db_full1_twins[j,t+18] 
    db_full1_twins[j,t+19]=db_full1_twins[i,t+18] 
  }
  i=i+1
}


#### add variables to the dataset

colnames(db_full1_twins)[t+1] <- "harshpar_bymom_t2"
colnames(db_full1_twins)[t+3] <- 'incnsist_byparent_t2'
colnames(db_full1_twins)[t+5] <- "ctrl_byparent_t2"
colnames(db_full1_twins)[t+7] <- "negcom_byparent_t2"
colnames(db_full1_twins)[t+9] <- "int_c1_t2"
colnames(db_full1_twins)[t+11] <- "ext_c1_t2"
colnames(db_full1_twins)[t+13] <- "victim_coh1_t2"
colnames(db_full1_twins)[t+15] <- "victim_coh2_t2"
colnames(db_full1_twins)[t+17] <- "ext_self_c2_t2"
colnames(db_full1_twins)[t+19] <- "int_self_c2_t2"


#########______1.4. Add variable to select at random one twin per family___________

#####___________________________________________________############

################______________________________________############
###keep only the pairs with both twins and create a random variable
#############_________________________________________#############
#twins_par_pv: 57 var  38   44 ; 48; 50

#keep only the variables needed for further analyses



twins_par_pv <- subset (db_full1_twins, select= c(fid_num, pid_num, wid,cgr,ptyp,zyg0102, sex,
                                                  
                                                  harshpar_bymom_t1, harshpar_bymom_t2, 
                                                  negcom_byparent_t1, negcom_byparent_t2, 
                                                  ctrl_byparent_t1, ctrl_byparent_t2, 
                                                  incnsist_byparent_t1,incnsist_byparent_t2,
                                                  
                                                  ext_par_c1, ext_c1_t2, 
                                                  int_par_c1, int_c1_t2,
                                                  
                                                  ext_self_c2, ext_self_c2_t2, 
                                                  int_self_c2, int_self_c2_t2, 
                                                  victim_coh1_t1, victim_coh1_t2,
                                                  victim_coh2_t1, victim_coh2_t2, income,
                                                  mom_MH, cohabit, ppl_hh))





#####this dataset has missing ID For Family
db_full1_twins_pairs <- twins_par_pv

for (i in 1:nrow(db_full1_twins_pairs)){
  j=i+1
  rev= -1*db_full1_twins_pairs[j,1]
  dif_fam=sum (db_full1_twins_pairs[i,1], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins_pairs[i,32]= 99
    db_full1_twins_pairs[j,32]=99
  }
  i=i+1
}

db_full1_twins_pairs$bothtwins_yes <- ifelse (is.na(db_full1_twins_pairs$V32), -9, db_full1_twins_pairs$V32)

db_full1_twins_pairs <-subset (db_full1_twins_pairs, bothtwins_yes==99)






n <-dim(db_full1_twins_pairs)[1]/2 #2572 # sample size; twin pairs in db_full1_twins2
rand <-sample(c(0,1), replace=TRUE, size=n)

###add the random variable in the data set;
#for each pair, the first twin takes the value 0 or 1, the second take values -9
for (i in 1:nrow(db_full1_twins_pairs)){
  j=i+1
  z=j/2
  rev= -1*db_full1_twins_pairs[j,1]
  dif_fam=sum (db_full1_twins_pairs[i,1], rev, na.rm = TRUE)
  if (dif_fam==0) {
    db_full1_twins_pairs[i,33]= rand[z]
    db_full1_twins_pairs[j,33]= -9
  }
  i=i+1
}

#if twin 1 is 1, then twin 2 is 0 and vice-versa

for (i in 1:nrow(db_full1_twins_pairs)){
  j=i+1
  rev= -1*db_full1_twins_pairs[j,1]
  dif_fam=sum (db_full1_twins_pairs[i,1], rev, na.rm = TRUE)
  if (dif_fam==0 & db_full1_twins_pairs[i,33]==1) {
    db_full1_twins_pairs[i,34]=1
    db_full1_twins_pairs[j,34]=0
  }
  if (dif_fam==0 & db_full1_twins_pairs[i,33]==0) {
    db_full1_twins_pairs[i,34]=0
    db_full1_twins_pairs[j,34]=1
  }
  
  i=i+1
}


db_full1_twins_pairs$rand <-db_full1_twins_pairs$V34




#db_full1_twins_pairs$vict_par_c1_avail <-ifelse (!is.na (db_full1_twins_pairs$victim_sum_coh1) & !is.na (db_full1_twins_pairs$harshpar_bymom), 1,0) #N=1294
#b_full1_twins_pairs$vict_par_c2_avail <-ifelse (!is.na (db_full1_twins_pairs$victim_sum_coh2) & !is.na (db_full1_twins_pairs$harshpar_bymom), 1,0) #N=2934

twins_par_pv_rand <-subset (db_full1_twins_pairs, db_full1_twins_pairs$rand==1)


###2. =========GEE ANALYSES==========

#####________________2.1. GEE overall___________


library(gee)
data.copy <- db_full1_twins_pairs

data.copy$zygos_num <-as.numeric (data.copy$zyg0102)
data.copy$zygos <- NA; data.copy$zygos <- factor(data.copy$zygos_num, levels = c(1,2), labels = c("MZ", "DZSS"))

data.copy$ppl_hh <-as.numeric (data.copy$ppl_hh)
data.copy$cohabit <-as.factor (data.copy$cohabit)
data.copy$mom_MH <-as.factor (data.copy$mom_MH)

data.copy$victim_coh1_t1_scale <- as.numeric (scale(data.copy$victim_coh1_t1, center=TRUE, scale=TRUE))
data.copy$victim_coh1_t2_scale <- as.numeric (scale(data.copy$victim_coh1_t2, center=TRUE, scale=TRUE))

db_full1_coh1 <- subset (db_full1, cgr=='1: twins born 2009/2010')


VecExposure <- c("harshpar_bymom_t1","harshpar_bymom_t2",
                 "negcom_byparent_t1","negcom_byparent_t2", 
                 "ctrl_byparent_t1","ctrl_byparent_t2",  
                 "incnsist_byparent_t1", "incnsist_byparent_t2")

TwinResults_coh1 <- matrix(nrow = length(VecExposure),ncol=12) #3 Ns, 3 for phenotypic, 3 for phenotypic adj,  3 for MZ 
TwinResults_suppl_coh1 <- matrix(nrow = length(VecExposure),ncol=8) #2 Ns, 3 for pheno adj, 3 for MZ adj


#### COHORT 1 #####
####_________#####

system.time(
  for (i in seq(from=1,to=length(VecExposure),by=2))
  {
    print(i)
    
    # Define EXPOSURE
    data.copy[c("X1","X2")]<-NA; 
    data.copy[c("X1","X2")]<-data.copy[,c(VecExposure[i],VecExposure[i+1])] 
    
    # Define complete cases
    data.copy<-data.copy[!is.na(data.copy$victim_coh1_t1_scale)&!is.na(data.copy$victim_coh1_t2_scale) &
                           !is.na(data.copy$X1) & !is.na(data.copy$X2),]
    
    data.copy_adj <-data.copy[!is.na(data.copy$victim_coh1_t1_scale)&!is.na(data.copy$victim_coh1_t2_scale) &
                                !is.na(data.copy$X1) & !is.na(data.copy$X2) & !is.na(data.copy$sex) & !is.na (data.copy$income) & !is.na(data.copy$mom_MH) & !is.na(data.copy$cohabit) &!is.na (data.copy$ppl_hh),]
    
    
    data.copy_adj2 <-data.copy[!is.na(data.copy$victim_coh1_t1_scale)&!is.na(data.copy$victim_coh1_t2_scale) &
                                 !is.na(data.copy$X1) & !is.na(data.copy$X2) & !is.na(data.copy$sex) & !is.na (data.copy$income) & !is.na(data.copy$mom_MH) & !is.na(data.copy$cohabit) &!is.na (data.copy$ppl_hh) & !is.na (data.copy$ext_c1_t2) & !is.na (data.copy$int_par_c1) & !is.na (data.copy$int_c1_t2), ]
    
    data.copy_adj2_MZ <-data.copy[!is.na(data.copy$victim_coh1_t1_scale)&!is.na(data.copy$victim_coh1_t2_scale) &
                                    !is.na(data.copy$X1) & !is.na(data.copy$X2) & !is.na (data.copy$ext_par_c1) & !is.na (data.copy$ext_c1_t2) & !is.na (data.copy$int_par_c1) & !is.na (data.copy$int_c1_t2), ]
    
    
    
    
    
    # Extract Ns for pairs
    
    data.copy_rand <-subset (data.copy, data.copy$rand==1)
    data.copy_adj_rand <-subset (data.copy_adj, data.copy_adj$rand==1)
    data.copy_adj2_rand <-subset (data.copy_adj2, data.copy_adj2$rand==1)
    data.copy_adj2_MZ_rand <-subset (data.copy_adj2, data.copy_adj2_MZ$rand==1)
    
    Ndesc <- data.frame(table(data.copy_rand$zygos), useNA="always")
    TotalN <- sum(Ndesc$Freq)
    N_adj <-dim(data.copy_adj_rand)[1]
    N_adj_2 <-dim(data.copy_adj2_rand)[1]
    
    NMZ <- Ndesc[1,"Freq"]
    Ndesc2 <- data.frame(table(data.copy_adj2_MZ_rand), useNA="always")
    N_adj2_MZ <-Ndesc2[1,"Freq"]
    
    
    
    # Run phenotypic regression continuous outcome; not adjusted for covariates
    
    print("gaussian pheno reg")
    
    gee_phenotypic=summary(gee(victim_coh1_t1_scale~X1, data=data.copy, id=fid_num, corstr="exchangeable"))
    
    # Extract phenotypic estimates
    est_P <- gee_phenotypic$coefficients[2,1]
    # low ci
    low_CI_P <- gee_phenotypic$coefficients[2,1] - (1.96 * gee_phenotypic$coefficients[2,4])
    # high ci
    upp_CI_P <- gee_phenotypic$coefficients[2,1] + (1.96 * gee_phenotypic$coefficients[2,4])
    
    
    
    #####Run the pheontyic adjusted for shared factors - the family characteristics
    
    
    gee_phenotypic_adj=summary(gee(victim_coh1_t1_scale~X1+sex+income+mom_MH+cohabit+ppl_hh, data=data.copy_adj, id=fid_num, corstr="exchangeable"))
    
    # Extract phenotypic estimates
    est_P_adj <- gee_phenotypic_adj$coefficients[2,1]
    # low ci
    low_CI_P_adj <- gee_phenotypic_adj$coefficients[2,1] - (1.96 * gee_phenotypic_adj$coefficients[2,4])
    # high ci
    upp_CI_P_adj <- gee_phenotypic_adj$coefficients[2,1] + (1.96 * gee_phenotypic_adj$coefficients[2,4])
    
    
    
    
    
    ####Run co-twin control analyses for MZ twins
    
    # Calculate harsh parent family mean and twin difference scores for co-twin control analyses
    data.copy$par_mean <- NA; data.copy$par_mean <- rowMeans(data.copy[,c("X1", "X2")]) #family mean (Bb)
    data.copy$par_diff1 <- NA; data.copy$par_diff1 <- data.copy$X1- data.copy$par_mean #difference score for twin 1 (Bw)
    
    #Subset to MZ twins
    data.copy_mz <- subset(data.copy, zygos=="MZ")
    
    # Run MZ twin model
    
    # print("gaussian mz reg")
    gee_mz_twins <- summary(gee(victim_coh1_t1_scale ~ par_mean + par_diff1, data=data.copy_mz, id=fid_num, corstr="exchangeable"))
    #
    
    # Extract MZ results - within-twin estimate (Bw)
    # estimate
    est_MZ <- gee_mz_twins$coefficients[3,1]
    # low ci
    low_CI_MZ <- gee_mz_twins$coefficients[3,1] - (1.96 * gee_mz_twins$coefficients[3,4])
    # high ci
    upp_CI_MZ <- gee_mz_twins$coefficients[3,1] + (1.96 * gee_mz_twins$coefficients[3,4])
    
    
    
    
    #####analyses adjusted for factors specific to each twin: externalizing and internalizing symptoms
    
    ### Run phenotypic adjusted for shared factors (family characteristics) & factors specific to each twin (externalizing & internalizing problems)
    
    gee_phenotypic_adj2=summary(gee(victim_coh1_t1_scale~X1+sex+income+mom_MH+cohabit+ppl_hh+ ext_par_c1+int_par_c1, data=data.copy_adj2, id=fid_num, corstr="exchangeable"))
    
    # Extract phenotypic estimates
    est_P_adj2 <- gee_phenotypic_adj2$coefficients[2,1]
    # low ci
    low_CI_P_adj2 <- gee_phenotypic_adj2$coefficients[2,1] - (1.96 * gee_phenotypic_adj2$coefficients[2,4])
    # high ci
    upp_CI_P_adj2 <- gee_phenotypic_adj2$coefficients[2,1] + (1.96 * gee_phenotypic_adj2$coefficients[2,4])
    
    
    ####Run co-twin control analyses for MZ twins with additional adjustment for extrnalizing and internalizing symptoms
    
    # Calculate mean and twin difference scores for co-twin control analyses
    
    # Calculate harsh parent family mean and twin difference scores for co-twin control analyses
    data.copy_adj2_MZ$par_mean <- NA;  data.copy_adj2_MZ$par_mean <- rowMeans( data.copy_adj2_MZ[,c("X1", "X2")]) #family mean (Bb)
    data.copy_adj2_MZ$par_diff1 <- NA;  data.copy_adj2_MZ$par_diff1 <-  data.copy_adj2_MZ$X1-  data.copy_adj2_MZ$par_mean #difference score for twin 1 (Bw)
    
    
    data.copy_adj2_MZ$int_mean <- NA;  data.copy_adj2_MZ$int_mean <- rowMeans( data.copy_adj2_MZ[,c("int_par_c1", "int_c1_t2")]) #family mean (Bb)
    data.copy_adj2_MZ$int_diff1 <- NA; data.copy_adj2_MZ$int_diff1 <- data.copy_adj2_MZ$int_par_c1- data.copy_adj2_MZ$int_mean #difference score for twin 1 (Bw)
    
    data.copy_adj2_MZ$ext_mean <- NA;  data.copy_adj2_MZ$ext_mean <- rowMeans( data.copy_adj2_MZ[,c("ext_par_c1", "ext_c1_t2")]) #family mean (Bb)
    data.copy_adj2_MZ$ext_diff1 <- NA; data.copy_adj2_MZ$ext_diff1 <- data.copy_adj2_MZ$ext_par_c1- data.copy_adj2_MZ$ext_mean #difference score for twin 1 (Bw)
    
    
    data.copy_mz_adj <- subset(data.copy_adj2_MZ, zygos=="MZ")
    
    # print("gaussian mz reg")
    gee_mz_twins_adj <- summary(gee(victim_coh1_t1_scale ~ par_mean + par_diff1 + int_mean+ int_diff1+ext_mean+ext_diff1, data=  data.copy_mz_adj, id=fid_num, corstr="exchangeable"))
    #
    
    # Extract MZ results - within-twin estimate (Bw)
    # estimate
    est_MZ_adj <- gee_mz_twins_adj$coefficients[3,1]
    # low ci
    low_CI_MZ_adj <- gee_mz_twins_adj$coefficients[3,1] - (1.96 * gee_mz_twins_adj$coefficients[3,4])
    # high ci
    upp_CI_MZ_adj <- gee_mz_twins_adj$coefficients[3,1] + (1.96 * gee_mz_twins_adj$coefficients[3,4])
    
    
    
    VecResults <- c(TotalN, N_adj, NMZ, est_P, low_CI_P, upp_CI_P, est_P_adj,low_CI_P_adj, upp_CI_P_adj, est_MZ, low_CI_MZ, upp_CI_MZ) 
    TwinResults_coh1[i,1:12]=VecResults
    
    VecResults_suppl <- c(N_adj_2, N_adj2_MZ, est_P_adj2,  low_CI_P_adj2,upp_CI_P_adj2, est_MZ_adj, low_CI_MZ_adj, upp_CI_MZ_adj)
    TwinResults_suppl_coh1[i,1:8]= VecResults_suppl 
  })


# Label columns in matrix
colnames(TwinResults_coh1)=c("TotalN", 'N_adj', "NMZ", "est_P","lowCI_P","upCI_P", "est_P_adj","low_CI_P_adj", "upp_CI_P_adj", 'est_MZ', 'low_CI_MZ', 'upp_CI_MZ')


colnames(TwinResults_suppl_coh1)=c('N_adj_2', 'N_adj2_MZ', 'est_P_adj2',  "low_CI_P_adj2", 'upp_CI_P_adj2', 'est_MZ_adj', 'low_CI_MZ_adj', 'upp_CI_MZ_adj')

# Round results to 2 DPs
TwinResults_coh1 <- round(TwinResults[seq(from=1,to=length(VecExposure),by=2),],2)
TwinResults_suppl_coh1 <- round(TwinResults_suppl[seq(from=1,to=length(VecExposure),by=2),],2)



#### COHORT 2 #####
####_________#####

VecExposure <- c("harshpar_bymom_t1","harshpar_bymom_t2",
                 "negcom_byparent_t1","negcom_byparent_t2", 
                 "ctrl_byparent_t1","ctrl_byparent_t2",  
                 "incnsist_byparent_t1", "incnsist_byparent_t2")

data.copy2 <- db_full1_twins_pairs

data.copy2$victim_coh2_t1_scale <- as.numeric (scale(data.copy2$victim_coh2_t1, center=TRUE, scale=TRUE))
data.copy2$victim_coh2_t2_scale <- as.numeric (scale(data.copy2$victim_coh2_t2, center=TRUE, scale=TRUE))

data.copy2$zygos_num <-as.numeric (data.copy2$zyg0102)
data.copy2$zygos <- NA; data.copy2$zygos <- factor(data.copy2$zygos_num, levels = c(1,2), labels = c("MZ", "DZSS"))

data.copy2$ppl_hh <-as.numeric (data.copy2$ppl_hh)
data.copy2$cohabit <-as.factor (data.copy2$cohabit)
data.copy2$mom_MH <-as.factor (data.copy2$mom_MH)

data.copy2 <- subset (data.copy2, cgr=='2: twins born 2003/2004')






TwinResults_coh2 <- matrix(nrow = length(VecExposure),ncol=12) #3 Ns, 3 for phenotypic, 3 for phenotypic adj,  3 for MZ 
TwinResults_suppl_coh2 <- matrix(nrow = length(VecExposure),ncol=8) #2 Ns, 3 for pheno adj, 3 for MZ adj

system.time(
  for (i in seq(from=1,to=length(VecExposure),by=2))
  {
    print(i)
    
    # Define EXPOSURE
    data.copy2[c("X1","X2")]<-NA; 
    data.copy2[c("X1","X2")]<-data.copy2[,c(VecExposure[i],VecExposure[i+1])] 
    
    # Define complete cases
    data.copy2<-data.copy2[!is.na(data.copy2$victim_coh2_t1_scale)&!is.na(data.copy2$victim_coh2_t2_scale) &
                             !is.na(data.copy2$X1) & !is.na(data.copy2$X2),]
    
    data.copy2_adj <-data.copy2[!is.na(data.copy2$victim_coh2_t1_scale)&!is.na(data.copy2$victim_coh2_t2_scale) &
                                  !is.na(data.copy2$X1) & !is.na(data.copy2$X2) & !is.na(data.copy2$sex) & !is.na (data.copy2$income) & !is.na(data.copy2$mom_MH) & !is.na(data.copy2$cohabit) &!is.na (data.copy2$ppl_hh),]
    
    
    data.copy2_adj2 <-data.copy2[!is.na(data.copy2$victim_coh2_t1_scale)&!is.na(data.copy2$victim_coh2_t2_scale) &
                                   !is.na(data.copy2$X1) & !is.na(data.copy2$X2) & !is.na(data.copy2$sex) & !is.na (data.copy2$income) & !is.na(data.copy2$mom_MH) & !is.na(data.copy2$cohabit) &!is.na (data.copy2$ppl_hh) & !is.na (data.copy2$ext_self_c2) & !is.na (data.copy2$ext_self_c2_t2) & !is.na (data.copy2$int_self_c2) & !is.na (data.copy2_adj2$int_self_c2_t2), ]
    
    data.copy2_adj2_MZ <-data.copy2[!is.na(data.copy2$victim_coh2_t1_scale)&!is.na(data.copy2$victim_coh2_t2_scale) &
                                      !is.na(data.copy2$X1) & !is.na(data.copy2$X2) & !is.na (data.copy2$ext_self_c2) & !is.na (data.copy2$ext_self_c2_t2) & !is.na (data.copy2$int_self_c2) & !is.na (data.copy2_adj2$int_self_c2_t2), ]
    
    
    # Extract Ns for pairs
    
    data.copy2_rand <-subset (data.copy2, data.copy2$rand==1)
    data.copy2_adj_rand <-subset (data.copy2_adj, data.copy2_adj$rand==1)
    data.copy2_adj2_rand <-subset (data.copy2_adj2, data.copy2_adj2$rand==1)
    data.copy2_adj2_MZ_rand <-subset (data.copy2_adj2, data.copy2_adj2_MZ$rand==1)
    
    Ndesc <- data.frame(table(data.copy2_rand$zygos), useNA="always")
    TotalN <- sum(Ndesc$Freq)
    N_adj <-dim(data.copy2_adj_rand)[1]
    N_adj_2 <-dim(data.copy2_adj2_rand)[1]
    
    NMZ <- Ndesc[1,"Freq"]
    Ndesc2 <- data.frame(table(data.copy2_adj2_MZ_rand), useNA="always")
    N_adj2_MZ <-Ndesc2[1,"Freq"]
    
    
    # Run phenotypic regression continuous outcome; not adjusted for covariates
    
    print("gaussian pheno reg")
    
    gee_phenotypic=summary(gee(victim_coh2_t1_scale~X1, data=data.copy2, id=fid_num, corstr="exchangeable"))
    
    # Extract phenotypic estimates
    est_P2 <- gee_phenotypic$coefficients[2,1]
    # low ci
    low_CI_P2 <- gee_phenotypic$coefficients[2,1] - (1.96 * gee_phenotypic$coefficients[2,4])
    # high ci
    upp_CI_P2 <- gee_phenotypic$coefficients[2,1] + (1.96 * gee_phenotypic$coefficients[2,4])
    
    
    
    #####Run the pheontyic adjusted for shared factors - the family characteristics
    
    
    gee_phenotypic_adj=summary(gee(victim_coh2_t1_scale~X1+sex+income+mom_MH+cohabit+ppl_hh, data=data.copy2, id=fid_num, corstr="exchangeable"))
    
    # Extract phenotypic estimates
    est_P_adj2 <- gee_phenotypic_adj$coefficients[2,1]
    # low ci
    low_CI_P_adj2 <- gee_phenotypic_adj$coefficients[2,1] - (1.96 * gee_phenotypic_adj$coefficients[2,4])
    # high ci
    upp_CI_P_adj2 <- gee_phenotypic_adj$coefficients[2,1] + (1.96 * gee_phenotypic_adj$coefficients[2,4])
    
    
    
    
    
    ####Run co-twin control analyses for MZ twins
    
    # Calculate harsh parent family mean and twin difference scores for co-twin control analyses
    data.copy2$par_mean <- NA; data.copy2$par_mean <- rowMeans(data.copy2[,c("X1", "X2")]) #family mean (Bb)
    data.copy2$par_diff1 <- NA; data.copy2$par_diff1 <- data.copy2$X1- data.copy2$par_mean #difference score for twin 1 (Bw)
    
    #Subset to MZ twins
    data.copy2_mz <- subset(data.copy2, zygos=="MZ")
    
    # Run MZ twin model
    
    # print("gaussian mz reg")
    gee_mz_twins <- summary(gee(victim_coh2_t1_scale ~ par_mean + par_diff1, data=data.copy2_mz, id=fid_num, corstr="exchangeable"))
    #
    
    # Extract MZ results - within-twin estimate (Bw)
    # estimate
    est_MZ2 <- gee_mz_twins$coefficients[3,1]
    # low ci
    low_CI_MZ2 <- gee_mz_twins$coefficients[3,1] - (1.96 * gee_mz_twins$coefficients[3,4])
    # high ci
    upp_CI_MZ2 <- gee_mz_twins$coefficients[3,1] + (1.96 * gee_mz_twins$coefficients[3,4])
    
    
    
    
    #####analyses adjusted for factors specific to each twin: externalizing and internalizing symptoms
    
    ### Run phenotypic adjusted for shared factors (family characteristics) & factors specific to each twin (externalizing & internalizing problems)
    
    gee_phenotypic_adj2=summary(gee(victim_coh2_t1_scale~X1+sex+income+mom_MH+cohabit+ppl_hh+ ext_self_c2+int_self_c2, data=  data.copy2_adj2_MZ, id=fid_num, corstr="exchangeable"))
    
    # Extract phenotypic estimates
    est_P_adj2_2 <- gee_phenotypic_adj2$coefficients[2,1]
    # low ci
    low_CI_P_adj2_2 <- gee_phenotypic_adj2$coefficients[2,1] - (1.96 * gee_phenotypic_adj2$coefficients[2,4])
    # high ci
    upp_CI_P_adj2_2 <- gee_phenotypic_adj2$coefficients[2,1] + (1.96 * gee_phenotypic_adj2$coefficients[2,4])
    
    
    ####Run co-twin control analyses for MZ twins with additional adjustment for extrnalizing and internalizing symptoms
    
    # Calculate mean and twin difference scores for co-twin control analyses
    
    # Calculate harsh parent family mean and twin difference scores for co-twin control analyses
    data.copy2_adj2_MZ$par_mean <- NA;  data.copy2_adj2_MZ$par_mean <- rowMeans( data.copy2_adj2_MZ[,c("X1", "X2")]) #family mean (Bb)
    data.copy2_adj2_MZ$par_diff1 <- NA;  data.copy2_adj2_MZ$par_diff1 <-  data.copy2_adj2_MZ$X1-  data.copy2_adj2_MZ$par_mean #difference score for twin 1 (Bw)
    
    
    data.copy2_adj2_MZ$int_mean <- NA;  data.copy2_adj2_MZ$int_mean <- rowMeans( data.copy2_adj2_MZ[,c("int_self_c2", "int_self_c2_t2")]) #family mean (Bb)
    data.copy2_adj2_MZ$int_diff1 <- NA; data.copy2_adj2_MZ$int_diff1 <- data.copy2_adj2_MZ$int_self_c2- data.copy2_adj2_MZ$int_mean #difference score for twin 1 (Bw)
    
    data.copy2_adj2_MZ$ext_mean <- NA;  data.copy2_adj2_MZ$ext_mean <- rowMeans(data.copy2_adj2_MZ[,c("ext_self_c2", "ext_self_c2_t2")]) #family mean (Bb)
    data.copy2_adj2_MZ$ext_diff1 <- NA; data.copy2_adj2_MZ$ext_diff1 <- data.copy2_adj2_MZ$ext_self_c2- data.copy2_adj2_MZ$ext_mean #difference score for twin 1 (Bw)
    
    
    data.copy2_mz_adj <- subset(data.copy2_adj2_MZ, zygos=="MZ")
    
    # print("gaussian mz reg")
    gee_mz_twins_adj <- summary(gee(victim_coh2_t1_scale ~ par_mean + par_diff1 + int_mean+ int_diff1+ext_mean+ext_diff1, data=  data.copy2_mz_adj, id=fid_num, corstr="exchangeable"))
    #
    
    # Extract MZ results - within-twin estimate (Bw)
    # estimate
    est_MZ_adj2 <- gee_mz_twins_adj$coefficients[3,1]
    # low ci
    low_CI_MZ_adj2 <- gee_mz_twins_adj$coefficients[3,1] - (1.96 * gee_mz_twins_adj$coefficients[3,4])
    # high ci
    upp_CI_MZ_adj2 <- gee_mz_twins_adj$coefficients[3,1] + (1.96 * gee_mz_twins_adj$coefficients[3,4])
    
    
    
    VecResults <- c(TotalN, N_adj, NMZ, est_P2,   low_CI_P2,  upp_CI_P2,  est_P_adj2,   low_CI_P_adj2, upp_CI_P_adj2,  est_MZ2, low_CI_MZ2,  upp_CI_MZ2) 
    
    
    TwinResults_coh2[i,1:12]=VecResults
    
    VecResults_suppl <- c(N_adj_2, N_adj2_MZ, est_P_adj2_2,  low_CI_P_adj2_2,   upp_CI_P_adj2_2, est_MZ_adj2 ,  low_CI_MZ_adj2 ,  upp_CI_MZ_adj2)
    TwinResults_suppl_coh2[i,1:8]= VecResults_suppl 
  })


# Label columns in matrix
colnames(TwinResults_coh2)=c("TotalN", 'N_adj', "NMZ", "est_P","lowCI_P","upCI_P", "est_P_adj","low_CI_P_adj", "upp_CI_P_adj", 'est_MZ', 'low_CI_MZ', 'upp_CI_MZ')


colnames(TwinResults_suppl_coh2)=c('N_adj_2', 'N_adj2_MZ', 'est_P_adj2',  "low_CI_P_adj2", 'upp_CI_P_adj2', 'est_MZ_adj', 'low_CI_MZ_adj', 'upp_CI_MZ_adj')

# Round results to 2 DPs
TwinResults_coh2 <- round(TwinResults_coh2[seq(from=1,to=length(VecExposure),by=2),],2)
TwinResults_suppl_coh2 <- round(TwinResults_suppl_coh2[seq(from=1,to=length(VecExposure),by=2),],2)



#######________2.2. GEE Sex differences___________

######________2.2.1. COHORT 1________________


######______GEE_SEX_DIFFERENCES_____#####

###COHORT 1

data.copy_int <- db_full1_twins_pairs

data.copy_int$sex_num <- as.numeric (data.copy_int$sex)

data.copy_int$sex_recod_f <-NA
data.copy_int$sex_recod_f[data.copy_int$sex_num==2] <- "0" # female
data.copy_int$sex_recod_f[data.copy_int$sex_num==1] <- "1" # male


# Recode sex so males are coded as 0 and females as 1
data.copy_int$sex_recod1 <-NA
data.copy_int$sex_recod1[data.copy_int$sex_num==1] <- "0" # male
data.copy_int$sex_recod1[data.copy_int$sex_num==2] <- "1" # female


data.copy_int$victim_coh1_t1_scale <- as.numeric (scale(data.copy_int$victim_coh1_t1, center=TRUE, scale=TRUE))
data.copy_int$victim_coh1_t2_scale <- as.numeric (scale(data.copy_int$victim_coh1_t2, center=TRUE, scale=TRUE))



VecExposure <- c("harshpar_bymom_t1","harshpar_bymom_t2",
                 "negcom_byparent_t1","negcom_byparent_t2", 
                 "ctrl_byparent_t1","ctrl_byparent_t2",  
                 "incnsist_byparent_t1", "incnsist_byparent_t2")


#outcomes: victim_coh1_scale, victim_coh1_t2_scale
# victim_coh2_scale, victim_coh2_t2_scale


library(dplyr)

#data.copyss <- subset (data.copyss, cgr=='2: twins born 2003/2004')


# standardize among same-sex twins the pv variable (check both options)



# Define results matrix
SexDiffsResults = matrix(nrow = length(VecExposure),ncol=18) #3 Ns, 3 for CV (effect of cyber-victimisation in females compared to no cv), 3 for CV (effect of cyber-victimisation in males compared to no cv), 3 for sex*CV (effect of cyber-victimisation in males compared to cv in females)

system.time (
  for (i in seq(from=1,to=length(VecExposure),by=2))
  {# Define EXPOSURE
    
    data.copy_int[c("X1","X2")]<-NA; 
    data.copy_int[c("X1","X2")]<-data.copy_int[,c(VecExposure[i],VecExposure[i+1])] 
    
    # Subset to complete cases unadjusted
    data.copyss<-data.copy_int[!is.na(data.copy_int$victim_coh1_t1_scale)&!is.na(data.copy_int$victim_coh1_t2_scale) 
                               & !is.na(data.copy_int$X1) & !is.na(data.copy_int$X2), ]
    #data.copyss <- subset (data.copyss, cgr=='2: twins born 2003/2004')
    data.copyss <- subset (data.copyss, cgr=='1: twins born 2009/2010')
    
    
    #data.copyssc<-data.copyss[!is.na(data.copyss$Y1) & !is.na(data.copyss$Y2)
    #         &!is.na(data.copyss$lpsbeht1) & !is.na(data.copyss$lpsbeht2) &
    #  !is.na(data.copyss$lnvbcog_1) & !is.na(data.copyss$lnvbcog_2),] 
    
    #data.copy_rand <-subset (data.copyss, data.copyss$rand==1)
    # Ndesc <- data.frame(table(data.copy_rand$zygos), useNA="always")
    # TotalN <- sum(Ndesc$Freq)
    #NDZ <- Ndesc[2,"Freq"]
    # NMZ <- Ndesc[1,"Freq"]
    
    
    
    
    
    # Standardise outcome variable - we get the same results if we use the variables standardized before in TEDS_6800
    #data.copyssc[,"Y1"] <- scale(data.copyssc[,"Y1"]); data.copyssc[,"Y2"] <- scale(data.copyssc[,"Y2"])
    
    
    # Run phenotypic regression (to obtain effect estimates in females, as they are coded as 0 in sex var)
    
    print("gaussian pheno reg")
    #sex_diffs_model  <- summary(gee(Y1 ~ DLD1_bin + sex1 + DLD1_bin:sex1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    
    sex_diffs_model  <- summary(gee(victim_coh1_t1_scale ~ X1 + sex_recod_f + X1:sex_recod_f, data=data.copyss, id=fid_num, corstr="exchangeable"))
    
    #ADJUSTED
    # sex_diffs_model  <- summary(gee(Y1 ~ DLD1_bin + sex1 + DLD1_bin:sex1 +  lnvbcog_1 + lpsbeht1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    # sex_diffs_model  <- summary(gee(Y1 ~ prag_yes1 + sex1 + prag_yes1:sex1 +str_yes1 + str_yes1:sex1, lnvbcog_1 + lpsbeht1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    
    # Extract estimates
    # CV estimate (effect of DLD versus no DLD in females)
    CV_est_f <- round (sex_diffs_model$coefficients[2,1],3)
    # low ci
    CV_low_CI_f <- round  (sex_diffs_model$coefficients[2,1] - (1.96 * sex_diffs_model$coefficients[2,4]),3)
    # high ci
    CV_upp_CI_f <- round (sex_diffs_model$coefficients[2,1] + (1.96 * sex_diffs_model$coefficients[2,4]),3)
    
    # Sex*cyber-vic interaction estimate 
    Sex_CV_est <- round (sex_diffs_model$coefficients[4,1],3) ###4 for unadjusted model, 6 for adjusted model WITH dld
    # low ci
    Sex_CV_low_CI <- round (sex_diffs_model$coefficients[4,1] - (1.96 * sex_diffs_model$coefficients[4,4]),3)
    # high ci
    Sex_CV_upp_CI <- round (sex_diffs_model$coefficients[4,1] + (1.96 * sex_diffs_model$coefficients[4,4]),3)
    
    # Recode sex so males are coded as 0 and females as 1
    #data.copyss$sex_recod1 <-NA
    #data.copyss$sex_recod1[data.copyss$sex_num==1] <- "0" # male
    #data.copyss$sex_recod1[data.copyss$sex_num==2] <- "1" # female
    
    
    # Run phenotypic regression (to obtain effect estimates in males)
    
    print("gaussian pheno reg")
    sex_diffs_model_males <- summary(gee(victim_coh1_t1_scale ~ X1 + sex_recod1 + X1:sex_recod1, data=data.copyss, id=fid_num, corstr="exchangeable"))
    #sex_diffs_model_males <- summary(gee(Y1 ~ prag_yes1 + sex_recod1 + prag_yes1:sex_recod1 + str_yes1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    #ADJUSTED
    # sex_diffs_model_males <- summary(gee(Y1 ~ DLD1_bin + sex_recod1 + DLD1_bin:sex_recod1+ lnvbcog_1+lpsbeht1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    # Extract CV estimate (effect of cyber-victimisation versus no cyber-victimisation in males)
    CV_est_m <- round (sex_diffs_model_males$coefficients[2,1],3)
    # low ci
    CV_low_CI_m <- round (sex_diffs_model_males$coefficients[2,1] - (1.96 * sex_diffs_model_males$coefficients[2,4]),3)
    # high ci
    CV_upp_CI_m <- round (sex_diffs_model_males$coefficients[2,1] + (1.96 * sex_diffs_model_males$coefficients[2,4]), 3)
    
    
    
    
    
    ####### DZ, MZ ESTIMATES 
    # Calculate DLD mean and twin difference scores for co-twin control analyses
    
    
    data.copyss$par_mean <- NA; data.copyss$par_mean <- rowMeans(data.copyss[,c("X1", "X2")]) #family mean (Bb)
    data.copyss$par_diff1 <- NA; data.copyss$par_diff1 <- data.copyss$X1- data.copyss$par_mean #difference score for twin 1 (Bw)
    
    
    
    #######MZ ESTIMATES
    
    data.copy_mz <- subset(data.copyss, zyg0102=="1: monozygotic")
    
    
    # Run MZ twin model
    ### PV estimate (effect of DLD versus no DLD in females; FEMALES=0, MALES=1)
    print("gaussian mz reg")
    
    # sex_diffs_MZ_female <- summary(gee(Y1 ~ DLD_mean_fam+ DLD_diff1 + sex1 + DLD_diff1:sex1 ,  data=data.copyc_mz, id=randomfamid, corstr="exchangeable"))
    # sex_diffs_MZ_female <- summary(gee(Y1 ~ prag_mean_fam+prag_diff1 + sex1 + prag_diff1:sex1 + str_mean_fam + str_diff1,  data=data.copyc_mz, id=randomfamid, corstr="exchangeable"))
    
    #Adjusted
    sex_diffs_MZ_female <- summary(gee(victim_coh1_t1_scale ~ par_mean+ par_diff1 + sex_recod_f + par_diff1:sex_recod_f,
                                       data=data.copy_mz, id=fid_num, corstr="exchangeable"))
    
    # Extract MZ results - within-twin estimate (Bw)
    # estimate PV FOR FEMALES
    est_MZ_f <-  round (sex_diffs_MZ_female$coefficients[3,1],3)
    # low ci
    low_CI_MZ_f <-  round(sex_diffs_MZ_female$coefficients[3,1] - (1.96 *  sex_diffs_MZ_female$coefficients[3,4]),3)
    # high ci
    upp_CI_MZ_f <-  round (sex_diffs_MZ_female$coefficients[3,1] + (1.96 *  sex_diffs_MZ_female$coefficients[3,4]),3)
    
    
    # Sex*dld interaction estimate 
    Sex_mz_est <- round (sex_diffs_MZ_female$coefficients[5,1],3) #5 for DLD crude, #7 for pragmatic+ str #9 for DLD ADJUSTED
    # low ci
    Sex_mz_low_CI <- round (sex_diffs_MZ_female$coefficients[5,1] - (1.96 * sex_diffs_MZ_female$coefficients[5,4]),3)
    # high ci
    Sex_mz_upp_CI <- round (sex_diffs_MZ_female$coefficients[5,1] + (1.96 * sex_diffs_MZ_female$coefficients[5,4]),3)
    
    
    # Run MZ regression (to obtain effect estimates in males)
    
    print("gaussian pheno reg")
    
    
    sex_diffs_MZ_male <- summary(gee(victim_coh1_t1_scale ~ par_mean+ par_diff1 + sex_recod1 + par_diff1:sex_recod1,
                                     data=data.copy_mz, id=fid_num, corstr="exchangeable"))
    
    # Extract CV estimate (effect of cyber-victimisation versus no cyber-victimisation in males)
    est_MZ_m <-  round (sex_diffs_MZ_male$coefficients[3,1],3)
    # low ci
    low_CI_MZ_m <-  round (sex_diffs_MZ_male$coefficients[3,1] - (1.96 *  sex_diffs_MZ_male$coefficients[3,4]), 3)
    # high ci 
    upp_CI_MZ_m <-  round (sex_diffs_MZ_male$coefficients[3,1] + (1.96 *  sex_diffs_MZ_male$coefficients[3,4]), 3)
    
    
    
    
    # Put results into a vector and then into the results matrix    
    Sexdiffs_Vec_Res=c(CV_est_f, CV_low_CI_f, CV_upp_CI_f, CV_est_m, CV_low_CI_m, CV_upp_CI_m, Sex_CV_est, Sex_CV_low_CI, Sex_CV_upp_CI,
                       est_MZ_f,low_CI_MZ_f, upp_CI_MZ_f,est_MZ_m,low_CI_MZ_m, upp_CI_MZ_m, Sex_mz_est,Sex_mz_low_CI,  Sex_mz_upp_CI)
    
    
    SexDiffsResults[i,1:18]=Sexdiffs_Vec_Res
    
  })



SexDiffsResults





# Label columns in SexDiffsResultsLoop
colnames(SexDiffsResults)=c('CV_est_f',' CV_low_CI_f', 'CV_upp_CI_f', 'CV_est_m', 'CV_low_CI_m', 'CV_upp_CI_m', 'Sex_CV_est', 'Sex_CV_low_CI', 'Sex_CV_upp_CI',
                            'est_MZ_f','low_CI_MZ_f', 'upp_CI_MZ_f','est_MZ_m','low_CI_MZ_m', 'upp_CI_MZ_m', 'Sex_mz_est','Sex_mz_low_CI',  'Sex_mz_upp_CI')

# Round results to 3 DPs and cut rows with missing data
SexDiffsResults = round(SexDiffsResults[seq(from=1,to=length(VecExposure),by=2),],2)



######________2.2.2. COHORT 2_______________

###COHORT 2

data.copy_int <- db_full1_twins_pairs

data.copy_int$sex_num <- as.numeric (data.copy_int$sex)

data.copy_int$sex_recod_f <-NA
data.copy_int$sex_recod_f[data.copy_int$sex_num==2] <- "0" # female
data.copy_int$sex_recod_f[data.copy_int$sex_num==1] <- "1" # male


# Recode sex so males are coded as 0 and females as 1
data.copy_int$sex_recod1 <-NA
data.copy_int$sex_recod1[data.copy_int$sex_num==1] <- "0" # male
data.copy_int$sex_recod1[data.copy_int$sex_num==2] <- "1" # female


data.copy_int$victim_coh2_t1_scale <- as.numeric (scale(data.copy_int$victim_coh2_t1, center=TRUE, scale=TRUE))
data.copy_int$victim_coh2_t2_scale <- as.numeric (scale(data.copy_int$victim_coh2_t2, center=TRUE, scale=TRUE))



VecExposure <- c("harshpar_bymom_t1","harshpar_bymom_t2",
                 "negcom_byparent_t1","negcom_byparent_t2", 
                 "ctrl_byparent_t1","ctrl_byparent_t2",  
                 "incnsist_byparent_t1", "incnsist_byparent_t2")


#outcomes: victim_coh1_scale, victim_coh1_t2_scale
# victim_coh2_scale, victim_coh2_t2_scale


library(dplyr)

#data.copyss <- subset (data.copyss, cgr=='2: twins born 2003/2004')


# standardize among same-sex twins the pv variable (check both options)



# Define results matrix
SexDiffsResults = matrix(nrow = length(VecExposure),ncol=18) #3 Ns, 3 for CV (effect of cyber-victimisation in females compared to no cv), 3 for CV (effect of cyber-victimisation in males compared to no cv), 3 for sex*CV (effect of cyber-victimisation in males compared to cv in females)

system.time (
  for (i in seq(from=1,to=length(VecExposure),by=2))
  {# Define EXPOSURE
    
    data.copy_int[c("X1","X2")]<-NA; 
    data.copy_int[c("X1","X2")]<-data.copy_int[,c(VecExposure[i],VecExposure[i+1])] 
    
    # Subset to complete cases unadjusted
    data.copyss<-data.copy_int[!is.na(data.copy_int$victim_coh2_t1_scale)&!is.na(data.copy_int$victim_coh2_t2_scale) 
                               & !is.na(data.copy_int$X1) & !is.na(data.copy_int$X2), ]
    data.copyss <- subset (data.copyss, cgr=='2: twins born 2003/2004')
    
    
    #data.copyssc<-data.copyss[!is.na(data.copyss$Y1) & !is.na(data.copyss$Y2)
    #         &!is.na(data.copyss$lpsbeht1) & !is.na(data.copyss$lpsbeht2) &
    #  !is.na(data.copyss$lnvbcog_1) & !is.na(data.copyss$lnvbcog_2),] 
    
    #data.copy_rand <-subset (data.copyss, data.copyss$rand==1)
    # Ndesc <- data.frame(table(data.copy_rand$zygos), useNA="always")
    # TotalN <- sum(Ndesc$Freq)
    #NDZ <- Ndesc[2,"Freq"]
    # NMZ <- Ndesc[1,"Freq"]
    
    
    
    
    
    # Standardise outcome variable - we get the same results if we use the variables standardized before in TEDS_6800
    #data.copyssc[,"Y1"] <- scale(data.copyssc[,"Y1"]); data.copyssc[,"Y2"] <- scale(data.copyssc[,"Y2"])
    
    
    # Run phenotypic regression (to obtain effect estimates in females, as they are coded as 0 in sex var)
    
    print("gaussian pheno reg")
    #sex_diffs_model  <- summary(gee(Y1 ~ DLD1_bin + sex1 + DLD1_bin:sex1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    
    sex_diffs_model  <- summary(gee(victim_coh2_t1_scale ~ X1 + sex_recod_f + X1:sex_recod_f, data=data.copyss, id=fid_num, corstr="exchangeable"))
    
    #ADJUSTED
    # sex_diffs_model  <- summary(gee(Y1 ~ DLD1_bin + sex1 + DLD1_bin:sex1 +  lnvbcog_1 + lpsbeht1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    # sex_diffs_model  <- summary(gee(Y1 ~ prag_yes1 + sex1 + prag_yes1:sex1 +str_yes1 + str_yes1:sex1, lnvbcog_1 + lpsbeht1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    
    # Extract estimates
    # CV estimate (effect of DLD versus no DLD in females)
    CV_est_f <- round (sex_diffs_model$coefficients[2,1],3)
    # low ci
    CV_low_CI_f <- round  (sex_diffs_model$coefficients[2,1] - (1.96 * sex_diffs_model$coefficients[2,4]),3)
    # high ci
    CV_upp_CI_f <- round (sex_diffs_model$coefficients[2,1] + (1.96 * sex_diffs_model$coefficients[2,4]),3)
    
    # Sex*cyber-vic interaction estimate 
    Sex_CV_est <- round (sex_diffs_model$coefficients[4,1],3) ###4 for unadjusted model, 6 for adjusted model WITH dld
    # low ci
    Sex_CV_low_CI <- round (sex_diffs_model$coefficients[4,1] - (1.96 * sex_diffs_model$coefficients[4,4]),3)
    # high ci
    Sex_CV_upp_CI <- round (sex_diffs_model$coefficients[4,1] + (1.96 * sex_diffs_model$coefficients[4,4]),3)
    
    # Recode sex so males are coded as 0 and females as 1
    #data.copyss$sex_recod1 <-NA
    #data.copyss$sex_recod1[data.copyss$sex_num==1] <- "0" # male
    #data.copyss$sex_recod1[data.copyss$sex_num==2] <- "1" # female
    
    
    # Run phenotypic regression (to obtain effect estimates in males)
    
    print("gaussian pheno reg")
    sex_diffs_model_males <- summary(gee(victim_coh2_t1_scale ~ X1 + sex_recod1 + X1:sex_recod1, data=data.copyss, id=fid_num, corstr="exchangeable"))
    #sex_diffs_model_males <- summary(gee(Y1 ~ prag_yes1 + sex_recod1 + prag_yes1:sex_recod1 + str_yes1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    #ADJUSTED
    # sex_diffs_model_males <- summary(gee(Y1 ~ DLD1_bin + sex_recod1 + DLD1_bin:sex_recod1+ lnvbcog_1+lpsbeht1, data=data.copyssc, id=randomfamid, corstr="exchangeable"))
    # Extract CV estimate (effect of cyber-victimisation versus no cyber-victimisation in males)
    CV_est_m <- round (sex_diffs_model_males$coefficients[2,1],3)
    # low ci
    CV_low_CI_m <- round (sex_diffs_model_males$coefficients[2,1] - (1.96 * sex_diffs_model_males$coefficients[2,4]),3)
    # high ci
    CV_upp_CI_m <- round (sex_diffs_model_males$coefficients[2,1] + (1.96 * sex_diffs_model_males$coefficients[2,4]), 3)
    
    
    
    
    
    ####### DZ, MZ ESTIMATES 
    # Calculate DLD mean and twin difference scores for co-twin control analyses
    
    
    data.copyss$par_mean <- NA; data.copyss$par_mean <- rowMeans(data.copyss[,c("X1", "X2")]) #family mean (Bb)
    data.copyss$par_diff1 <- NA; data.copyss$par_diff1 <- data.copyss$X1- data.copyss$par_mean #difference score for twin 1 (Bw)
    
    
    
    #######MZ ESTIMATES
    
    data.copy_mz <- subset(data.copyss, zyg0102=="1: monozygotic")
    
    
    # Run MZ twin model
    ### PV estimate (effect of DLD versus no DLD in females; FEMALES=0, MALES=1)
    print("gaussian mz reg")
    
    # sex_diffs_MZ_female <- summary(gee(Y1 ~ DLD_mean_fam+ DLD_diff1 + sex1 + DLD_diff1:sex1 ,  data=data.copyc_mz, id=randomfamid, corstr="exchangeable"))
    # sex_diffs_MZ_female <- summary(gee(Y1 ~ prag_mean_fam+prag_diff1 + sex1 + prag_diff1:sex1 + str_mean_fam + str_diff1,  data=data.copyc_mz, id=randomfamid, corstr="exchangeable"))
    
    #Adjusted
    sex_diffs_MZ_female <- summary(gee(victim_coh2_t1_scale ~ par_mean+ par_diff1 + sex_recod_f + par_diff1:sex_recod_f,
                                       data=data.copy_mz, id=fid_num, corstr="exchangeable"))
    
    # Extract MZ results - within-twin estimate (Bw)
    # estimate PV FOR FEMALES
    est_MZ_f <-  round (sex_diffs_MZ_female$coefficients[3,1],3)
    # low ci
    low_CI_MZ_f <-  round(sex_diffs_MZ_female$coefficients[3,1] - (1.96 *  sex_diffs_MZ_female$coefficients[3,4]),3)
    # high ci
    upp_CI_MZ_f <-  round (sex_diffs_MZ_female$coefficients[3,1] + (1.96 *  sex_diffs_MZ_female$coefficients[3,4]),3)
    
    
    # Sex*dld interaction estimate 
    Sex_mz_est <- round (sex_diffs_MZ_female$coefficients[5,1],3) #5 for DLD crude, #7 for pragmatic+ str #9 for DLD ADJUSTED
    # low ci
    Sex_mz_low_CI <- round (sex_diffs_MZ_female$coefficients[5,1] - (1.96 * sex_diffs_MZ_female$coefficients[5,4]),3)
    # high ci
    Sex_mz_upp_CI <- round (sex_diffs_MZ_female$coefficients[5,1] + (1.96 * sex_diffs_MZ_female$coefficients[5,4]),3)
    
    
    # Run MZ regression (to obtain effect estimates in males)
    
    print("gaussian pheno reg")
    
    
    sex_diffs_MZ_male <- summary(gee(victim_coh2_t1_scale ~ par_mean+ par_diff1 + sex_recod1 + par_diff1:sex_recod1,
                                     data=data.copy_mz, id=fid_num, corstr="exchangeable"))
    
    # Extract CV estimate (effect of cyber-victimisation versus no cyber-victimisation in males)
    est_MZ_m <-  round (sex_diffs_MZ_male$coefficients[3,1],3)
    # low ci
    low_CI_MZ_m <-  round (sex_diffs_MZ_male$coefficients[3,1] - (1.96 *  sex_diffs_MZ_male$coefficients[3,4]), 3)
    # high ci 
    upp_CI_MZ_m <-  round (sex_diffs_MZ_male$coefficients[3,1] + (1.96 *  sex_diffs_MZ_male$coefficients[3,4]), 3)
    
    
    
    
    # Put results into a vector and then into the results matrix    
    Sexdiffs_Vec_Res=c(CV_est_f, CV_low_CI_f, CV_upp_CI_f, CV_est_m, CV_low_CI_m, CV_upp_CI_m, Sex_CV_est, Sex_CV_low_CI, Sex_CV_upp_CI,
                       est_MZ_f,low_CI_MZ_f, upp_CI_MZ_f,est_MZ_m,low_CI_MZ_m, upp_CI_MZ_m, Sex_mz_est,Sex_mz_low_CI,  Sex_mz_upp_CI)
    
    
    SexDiffsResults[i,1:18]=Sexdiffs_Vec_Res
    
  })



# Label columns in SexDiffsResultsLoop
colnames(SexDiffsResults)=c('CV_est_f',' CV_low_CI_f', 'CV_upp_CI_f', 'CV_est_m', 'CV_low_CI_m', 'CV_upp_CI_m', 'Sex_CV_est', 'Sex_CV_low_CI', 'Sex_CV_upp_CI',
                            'est_MZ_f','low_CI_MZ_f', 'upp_CI_MZ_f','est_MZ_m','low_CI_MZ_m', 'upp_CI_MZ_m', 'Sex_mz_est','Sex_mz_low_CI',  'Sex_mz_upp_CI')

# Round results to 3 DPs and cut rows with missing data
SexDiffsResults = round(SexDiffsResults[seq(from=1,to=length(VecExposure),by=2),],2)

SexDiffsResults









