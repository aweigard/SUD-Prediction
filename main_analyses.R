rm(list=ls())

# load necessary packages 
library(foreign)
library(fastDummies)
library(FactoMineR)
library(randomForest)
library(missForest)
library(missForestPredict)
library(pROC)
library(ROCR)
library(cvAUC)
library(data.table)
library(corrplot)

######################################
#### load and pre-process data #######
######################################

# load master wide format file of all summary variables for each participant
load("G2_master.RData")

# load long format files for variables that must be re-calculated within each fold
load("MLS_long_files.RData")

# load file with basic participant demographics and DIS diagnoses during ages 20-26
mls.dat<-read.spss("DIS_G2_04202023.sav",to.data.frame = TRUE)

# re-code DIS diagnostic data
for (c in c(paste0("dud.",20:26),paste0("aud.",20:26))){
  mls.dat[,c]<-as.character(mls.dat[,c])
  mls.dat[mls.dat[,c]%in%"Present",c]<-1
  mls.dat[,c]<-as.numeric(mls.dat[,c])
}

# create binary variable for the presence of alcohol use disorder (AUD),
# drug use disorder (DUD), or any substance use disorder (SUD) during ages 20-16
mls.dat$AUD<-as.numeric(rowMeans(mls.dat[,paste0("aud.",20:26)],na.rm=TRUE)>0)
mls.dat$DUD<-as.numeric(rowMeans(mls.dat[,paste0("dud.",20:26)],na.rm=TRUE)>0)
mls.dat$SUD<-as.numeric(rowMeans(mls.dat[,c("AUD","DUD")],na.rm=FALSE)>0)

# figure out how many DIS assessments each person had
mls.dat$dis.comp<-rowSums(!is.na(mls.dat[,paste0("aud.",20:26)]))

# compile main data set with outcomes and basic demographics
dat<-mls.dat[,c("target","AUD","DUD","SUD","sex","race","anypardx","dis.comp")]

# re-code family history (anypardx) variable
dat$anypardx<-as.character(dat$anypardx)
dat[dat$anypardx%in%"none of the parents have life time alcohol diagnosis" & !is.na(dat$anypardx),]$anypardx<-0
dat[dat$anypardx%in%"One or both parents have alcohol diagnosis life time" & !is.na(dat$anypardx),]$anypardx<-1
dat$anypardx<-as.numeric(dat$anypardx)


# merge with all other variables
dat<-merge(dat,G2.master,by="target")

# establish reference levels for categorical variables

dat$race<-as.character(dat$race)
dat[dat$race%in%"Caucasian",]$race<-"White"
dat[dat$race%in%c("African American","Native American",
                          "Bi-Racial","Hispanic-caucasian race"),]$race<-"Other"
dat$race<-as.factor(dat$race)
dat$race <- relevel(dat$race, ref = 'White')

dat$sex <- relevel(dat$sex, ref = 'male')
dat$high.edu<-as.factor(dat$high.edu); dat$high.edu<-relevel(dat$high.edu, ref = 'High School')
dat$ABCD.inc.child<-as.factor(dat$ABCD.inc.child); dat$ABCD.inc.child<-relevel(dat$ABCD.inc.child, ref = '50k-100k')
dat$ABCD.inc.adol<-as.factor(dat$ABCD.inc.adol); dat$ABCD.inc.adol<-relevel(dat$ABCD.inc.adol, ref = '50k-100k')

############################################
### name specific columns for prediction ###
############################################

# manifest variables
mani.v<-c("sex","race","anypardx","high.edu","ABCD.inc.child","ABCD.inc.adol",
          "ever.low.overall","ever.high.overall",
          "reli_catholic", "reli_baptist" , "reli_methodist", "reli_lutheran",
          "reli_none", "reli_other", "reli_regatt", "reli_how",
         "reli_self_catholic", "reli_self_baptist", "reli_self_methodist",
          "reli_self_lutheran", "reli_self_none", "reli_self_other", "reli_self_regatt" ,"reli_self_how",
          "school_enjoy", "school_hate", "school_dobest", "school_hard", "school_interesting",
          "school_failontime", "school_office", "part_paper", "part_music", "part_ath", "part_other", "work",
          "overall.estM.IQ","twthdrw_0_9","tsomati_0_9","tanxdep_0_9","tsocpro_0_9","tthotpr_0_9",
          "tattent_0_9","tdelinq_0_9","taggres_0_9","twthdrw_10_13","tsomati_10_13","tanxdep_10_13",
          "tsocpro_10_13", "tthotpr_10_13","tattent_10_13","tdelinq_10_13","taggres_10_13",
          "twthdrw_14_18","tsomati_14_18","tanxdep_14_18","tsocpro_14_18","tthotpr_14_18",
          "tattent_14_18","tdelinq_14_18","taggres_14_18", "twthdrw_teacher", "tsomati_teacher",
          "tanxdep_teacher","tsocpro_teacher","tthotpr_teacher","tattent_teacher", "tdelinq_teacher",
          "taggres_teacher","twthdrw_ysr_10_13","tsomati_ysr_10_13","tsocpro_ysr_10_13","tanxdep_ysr_10_13",
          "tthotpr_ysr_10_13","tattent_ysr_10_13","tdelinq_ysr_10_13","taggres_ysr_10_13","twthdrw_ysr_14_18",
          "tsomati_ysr_14_18","tsocpro_ysr_14_18", "tanxdep_ysr_14_18", "tthotpr_ysr_14_18", "tattent_ysr_14_18",
          "tdelinq_ysr_14_18", "taggres_ysr_14_18","alc_probs_10_13","alc_probs_14_15","alc_probs_16_17",
          "alc_probs_18_19", "drug_probs_10_13","drug_probs_14_15","drug_probs_16_17","drug_probs_18_19",
          "binge_10_13","binge_14_15","binge_16_17","binge_18_19","bingedk_10_13","bingedk_14_15",
          "bingedk_16_17","bingedk_18_19","drunk_10_13","drunk_14_15","drunk_16_19","maxamt_10_13",
          "maxamt_14_15","maxamt_16_17","maxamt_18_19","daypermo_10_13","daypermo_14_15","daypermo_16_17",
          "daypermo_18_19","drinkday_10_13","drinkday_14_15","drinkday_16_17","drinkday_18_19",
          "age_drink_final","behv1_10_13","behv2_10_13","behv3_10_13","behv45_10_13","behv6_10_13",
          "behv7a_10_13","behv7b_10_13","behv10_10_13","behv1_14_15",
          "behv2_14_15","behv3_14_15","behv45_14_15","behv6_14_15","behv7a_14_15","behv7b_14_15",
          "behv10_14_15","behv1_16_19","behv2_16_19","behv3_16_19",
          "behv45_16_19","behv6_16_19","behv7a_16_19","behv7b_16_19",
          "behv10_16_19","age_cig_est","cig_10_13","cig_14_15","cig_16_17","cig_18_19","can_m_est_10_13",
          "can_m_est_14_15","can_m_est_16_17","can_m_est_18_19","age_can_final","inhal","amph",
          "rita","meth","psyche","cocaine","sedat","narco","presc.narco","club","age_drunk_final",
          "pmC_10_13","pmC_14_18","pmP","cohechl_10_13","expschl_10_13","confchl_10_13","indechl_10_13",
          "achichl_10_13","intechl_10_13","actichl_10_13","morachl_10_13","orgnchl_10_13","cntlchl_10_13",
          "cohechl_14_18","expschl_14_18","confchl_14_18","indechl_14_18","achichl_14_18", "intechl_14_18",
          "actichl_14_18","morachl_14_18","orgnchl_14_18","cntlchl_14_18","cohepar_childhood",
          "expspar_childhood","confpar_childhood","indepar_childhood","achipar_childhood",
          "intepar_childhood","actipar_childhood","morapar_childhood","orgnpar_childhood",
          "cntlpar_childhood","cohepar_adolescence","expspar_adolescence","confpar_adolescence",
          "indepar_adolescence","achipar_adolescence","intepar_adolescence","actipar_adolescence",
          "morapar_adolescence","orgnpar_adolescence","cntlpar_adolescence","can_est_mom_childhood",
          "cig_mom_childhood","drink_days_m_mom_childhood","drinks_day_mom_childhood",
          "binge_mom_childhood","bingedk_mom_childhood","can_est_mom_adolescence","cig_mom_adolescence",
          "drink_days_m_mom_adolescence","drinks_day_mom_adolescence","binge_mom_adolescence",
          "bingedk_mom_adolescence","can_est_dad_childhood","cig_dad_childhood","drink_days_m_dad_childhood",
          "drinks_day_dad_childhood","binge_dad_childhood","bingedk_dad_childhood","can_est_dad_adolescence",
          "cig_dad_adolescence","drink_days_m_dad_adolescence","drinks_day_dad_adolescence",
          "binge_dad_adolescence","bingedk_dad_adolescence","hassles_childhood","uplifts_childhood",
          "hassles_adolescence", "uplifts_adolescence","ACEs","vioong2","peer_rel_10_13","peer_rel_14_18",
          "counsel","sch","soc","ath","phy","beh","glo","school","social","athlet","appear","job",
          "romance","conduct","friend","selfwor")

# variables computed at the level of each fold
comp.v<-c("hand_res.comp","oral_res.comp","tmtc_timea","tmtc_timeb","tmt_timea_M","tmt_timeb_M",
          "stroop_wordread_M","stroop_colornam_M","stroop_colorwor_M","v_res","a_res","Ter_res","gf_res",
          "q_resl","q_nege","q_reacont","activ", "sleep","appro","flex","mood","rhymslp","rhymeat",
          "rhymhab","task","distrac","persist","self_activ","self_sleep","self_appro","self_flex",
          "self_mood","self_rhymslp","self_rhymeat","self_rhymhab","self_task","neurotys","extravys",
          "openys","agreeys","consys","DoG","pub_timing")

# all variables that can be used for prediction
pred.cols<-c(mani.v,comp.v)

# cognitive variables
cog.cols<-c("overall.estM.IQ","hand_res.comp","oral_res.comp","tmtc_timea","tmtc_timeb","tmt_timea_M","tmt_timeb_M",
                    "stroop_wordread_M","stroop_colornam_M","stroop_colorwor_M","v_res","a_res","Ter_res","gf_res","DoG")

# psychosocial variables not drawn from self-report
psysoc.nosr.cols<-c("sex","race","anypardx","high.edu","ABCD.inc.child","ABCD.inc.adol",
             "ever.low.overall","ever.high.overall",
             "reli_catholic", "reli_baptist" , "reli_methodist", "reli_lutheran",
             "reli_none", "reli_other", "reli_regatt", "reli_how",
            "q_resl","q_nege","q_reacont","ACEs",
             "twthdrw_0_9","tsomati_0_9","tanxdep_0_9","tsocpro_0_9","tthotpr_0_9",
             "tattent_0_9","tdelinq_0_9","taggres_0_9","twthdrw_10_13","tsomati_10_13","tanxdep_10_13",
             "tsocpro_10_13", "tthotpr_10_13","tattent_10_13","tdelinq_10_13","taggres_10_13",
             "twthdrw_14_18","tsomati_14_18","tanxdep_14_18","tsocpro_14_18","tthotpr_14_18",
             "tattent_14_18","tdelinq_14_18","taggres_14_18", "twthdrw_teacher", "tsomati_teacher",
             "tanxdep_teacher","tsocpro_teacher","tthotpr_teacher","tattent_teacher", "tdelinq_teacher",
             "taggres_teacher","pmP","cohepar_childhood",
             "expspar_childhood","confpar_childhood","indepar_childhood","achipar_childhood",
             "intepar_childhood","actipar_childhood","morapar_childhood","orgnpar_childhood",
             "cntlpar_childhood","cohepar_adolescence","expspar_adolescence","confpar_adolescence",
             "indepar_adolescence","achipar_adolescence","intepar_adolescence","actipar_adolescence",
             "morapar_adolescence","orgnpar_adolescence","cntlpar_adolescence","can_est_mom_childhood",
             "cig_mom_childhood","drink_days_m_mom_childhood","drinks_day_mom_childhood",
             "binge_mom_childhood","bingedk_mom_childhood","can_est_mom_adolescence","cig_mom_adolescence",
             "drink_days_m_mom_adolescence","drinks_day_mom_adolescence","binge_mom_adolescence",
             "bingedk_mom_adolescence","can_est_dad_childhood","cig_dad_childhood","drink_days_m_dad_childhood",
             "drinks_day_dad_childhood","binge_dad_childhood","bingedk_dad_childhood","can_est_dad_adolescence",
             "cig_dad_adolescence","drink_days_m_dad_adolescence","drinks_day_dad_adolescence",
             "binge_dad_adolescence","bingedk_dad_adolescence","hassles_childhood","uplifts_childhood",
             "hassles_adolescence", "uplifts_adolescence","vioong2",
             "counsel","activ", "sleep", "appro", "flex", "mood", "rhymslp",
            "rhymeat","rhymhab","task","distrac","persist")

# self-report variables
sr.cols<-pred.cols[!pred.cols%in%c(cog.cols,psysoc.nosr.cols)]
sr.cols

# self-reported measures specific to youths' substance use (drinking & drug history):
su.cols<-c("alc_probs_10_13","alc_probs_14_15","alc_probs_16_17",
           "alc_probs_18_19", "drug_probs_10_13","drug_probs_14_15","drug_probs_16_17","drug_probs_18_19",
           "binge_10_13","binge_14_15","binge_16_17","binge_18_19","bingedk_10_13","bingedk_14_15",
           "bingedk_16_17","bingedk_18_19","drunk_10_13","drunk_14_15","drunk_16_19","maxamt_10_13",
           "maxamt_14_15","maxamt_16_17","maxamt_18_19","daypermo_10_13","daypermo_14_15","daypermo_16_17",
           "daypermo_18_19","drinkday_10_13","drinkday_14_15","drinkday_16_17","drinkday_18_19",
           "age_drink_final","behv1_10_13","behv2_10_13","behv3_10_13","behv45_10_13","behv6_10_13",
           "behv7a_10_13","behv7b_10_13","behv10_10_13","behv1_14_15",
           "behv2_14_15","behv3_14_15","behv45_14_15","behv6_14_15","behv7a_14_15","behv7b_14_15",
           "behv10_14_15","behv1_16_19","behv2_16_19","behv3_16_19",
           "behv45_16_19","behv6_16_19","behv7a_16_19","behv7b_16_19",
           "behv10_16_19","age_cig_est","cig_10_13","cig_14_15","cig_16_17","cig_18_19","can_m_est_10_13",
           "can_m_est_14_15","can_m_est_16_17","can_m_est_18_19","age_can_final","inhal","amph",
           "rita","meth","psyche","cocaine","sedat","narco","presc.narco","club","age_drunk_final")

# make all logical columns into factors 

for (c in pred.cols){
  if(is.logical(dat[,c])){
    dat[,c]<-as.factor(dat[,c])
  }
}


##############################################################
### exclusions based on missingness ##########################
##############################################################

####  distribution of missing data by participant

missing.per.sub<-rowMeans(is.na(dat[,pred.cols]))
names(missing.per.sub)<-dat$target

# some people with very high rates of missing data
hist(missing.per.sub)
quantile(missing.per.sub)

quantile(missing.per.sub,probs = c(.80,.90,.95))

sort(missing.per.sub,decreasing=T)[1:100]

####  distribution of missing data by variable

missing.pc<-colMeans(is.na(dat[,pred.cols]))

quantile(missing.pc)

### exclude those missing everything from one domain

# how many missing self-report?
dat$sr.miss<-rowMeans(is.na(dat[,sr.cols]))
dat$sr.present<-rowSums(!is.na(dat[,sr.cols]))

# how many missing cognitive?
dat$cog.miss<-rowMeans(is.na(dat[,cog.cols]))
dat$cog.present<-rowSums(!is.na(dat[,cog.cols]))

# how many missing non-sr psychosocial?
dat$nosr.miss<-rowMeans(is.na(dat[,psysoc.nosr.cols]))

# how many missing substance use? 
dat$su.miss<-rowMeans(is.na(dat[,su.cols]))

# Exclude if one domain is completely missing

dat<-dat[dat$cog.miss!=1 & dat$sr.miss!=1 & 
               dat$su.miss!=1 & dat$nosr.miss!=1,]


#####################################
### make folds ######################
#####################################

### make folds by family

families<-data.frame(fam=unique(dat$family))
families$fold<-sample(1:5,length(families$fam),replace=TRUE)

dat$fold<-NA

for (f in 1:5){
  dat[dat$family%in%families[families$fold==f,]$fam,]$fold<-f} 

table(dat$fold)

table(dat$fold,dat$SUD)

table(dat$fold,dat$AUD)

table(dat$fold,dat$DUD)

save(dat,file="folds_MLS_SUD.RData")

###########################################################################
### re-compute summary variables by fold and make training and test sets ##
###########################################################################

# function to estimate normative model in training data and
# compute average individuals' residuals from this model within both 
# the training and test data
standardize.fold<-function(train,test,long,vars,names,formula,id="target"){
  fids<-test[,id]
  for (v in 1:length(vars)){
  lmt<-lm(paste0(vars[v],"~",formula),data = long[!long[,id]%in%fids,],na.action=na.exclude)
  long[,paste0(vars[v],"_tmp")]<-(long[,vars[v]]-predict(lmt,newdata=long))
  tmp<-tapply(long[,paste0(vars[v],"_tmp")], long[,id], mean,na.rm=TRUE)
  train[,names[v]]<-c(tmp[as.character(train$target)])
  test[,names[v]]<-c(tmp[as.character(test$target)])
  }
  out<-list(train=train,test=test) 
}

folds<-list()

for (f in 1:5){
  
  fold<-list(train=dat[!dat$fold==f,],test=dat[dat$fold==f,]) 
  
  # SDMT
  fold<-standardize.fold(fold$train,fold$test,sdmt_G2,
                        vars=c("handscr","oralscr"),
                        names=c("hand_res.comp","oral_res.comp"),
                        formula="sdmt.age+age_sq")
  
  # Trail making, child version
  fold<-standardize.fold(fold$train,fold$test,tmtc_G2,
                         vars=c("log_timea","log_timeb"),
                         names=c("tmtc_timea","tmtc_timeb"),
                         formula="tmt.age+age_sq")

  # Trail making
  fold<-standardize.fold(fold$train,fold$test,tmt_G2,
                         vars=c("log_timea","log_timeb"),
                         names=c("tmt_timea_M","tmt_timeb_M"),
                         formula="tmt.age+age_sq")
  
  # Stroop
  fold<-standardize.fold(fold$train,fold$test,stroop_G2,
                         vars=c("wordread","colornam","colorwor"),
                         names=c("stroop_wordread_M","stroop_colornam_M","stroop_colorwor_M"),
                         formula="stroop.age+age_sq")
  
  # DDM parameters
  fold<-standardize.fold(fold$train,fold$test,sst_G2,
                         vars=c("v","a","Ter","gf"),
                         names=c("v_res","a_res","Ter_res","gf_res"),
                         formula="sst.age+age_sq+version")
  
  # Q sort
  fold<-standardize.fold(fold$train,fold$test,qsort_G2,
                         vars=c("resl","nege","reacont"),
                         names=c("q_resl","q_nege","q_reacont"),
                         formula="qsort.age+age_sq")

  # dots (parent and child)
  dots.c<-c("activ", "sleep","appro","flex","mood","rhymslp","rhymeat",
            "rhymhab","task","distrac","persist")
  fold<-standardize.fold(fold$train,fold$test,dotsR_PonC_G2,
                         vars=dots.c,names=dots.c,
                         formula="dotsPonC.age+age_sq")
  fold<-standardize.fold(fold$train,fold$test,dotsR_ConC_G2,
                         vars=dots.c[1:9],names=paste0("self_",dots.c[1:9]),
                         formula="dotsR_ConC.age+age_sq")
  
  # neo
  fold<-standardize.fold(fold$train,fold$test,neo_G2,
                         vars=c("neurotys","extravys","openys","agreeys","consys"),
                         names=c("neurotys","extravys","openys","agreeys","consys"),
                         formula="neo.age+age_sq+version")
  
  # delay of gratification
  fold<-standardize.fold(fold$train,fold$test,DoG_G2,
                         vars=c("totw"),
                         names=c("DoG"),
                         formula="DoG.age+age_sq")

  # pubertal development
  fold<-standardize.fold(fold$train,fold$test,pd_M,
                         vars=c("tanner"),names=c("pd_male"),formula="pd.age")
  fold<-standardize.fold(fold$train,fold$test,pd_F,
                         vars=c("tanner"),names=c("pd_female"),formula="pd.age")
  fold$train$pub_timing<-rowMeans(fold$train[,c("pd_male","pd_female")],na.rm = T)
  fold$test$pub_timing<-rowMeans(fold$test[,c("pd_male","pd_female")],na.rm = T)
  
  # insert data into structure
  folds[[f]]<-fold 
  
}


###############################################
##### impute all training and test data########
###############################################

#make custom function for imputing in each fold:

impute.folds <- function(folds,pred.cols,save.imp=FALSE){
  
  out<-list()
  
  for (f in 1:length(folds)){
    
    tmp.train<-folds[[f]]$train[,pred.cols]
    tmp.test<-folds[[f]]$test[,pred.cols]
    
    tmp.train.imp <- missForest(xmis = tmp.train)
    tmp.train<-tmp.train.imp$ximp
    tmp.test <- missForestPredict(tmp.train.imp,tmp.test)
    
    out[[f]]<-list(train=NA,test=NA,imp=NA)
    out[[f]]$train<-tmp.train
    out[[f]]$test<-tmp.test
    
    if(save.imp){out[[f]]$imp<-tmp.train.imp}

  }
  
  names(out)<-paste0("lo.",folds)
  
  out
  
}

# impute for all combinations of columns

ps.all<-impute.folds(folds,pred.cols)
save(ps.all,file="ps_all_final.RData")

ps.cog<-impute.folds(folds,cog.cols)
ps.sr<-impute.folds(folds,sr.cols)
ps.psysoc.nosr<-impute.folds(folds,psysoc.nosr.cols)
ps.su<-impute.folds(folds,su.cols)
save(ps.cog,ps.sr,ps.psysoc.nosr,ps.su,file="ps_only_final.RData")

ps.NOcog<-impute.folds(folds,c(sr.cols,psysoc.nosr.cols))
ps.NOsr<-impute.folds(folds,c(cog.cols,psysoc.nosr.cols))
ps.NOpsysoc.nosr<-impute.folds(folds,c(sr.cols,cog.cols))
ps.NOsu<-impute.folds(folds,c(cog.cols,psysoc.nosr.cols,sr.cols[!sr.cols%in%su.cols]))
save(ps.NOcog,ps.NOsr,ps.NOpsysoc.nosr,ps.NOsu,file="ps_without_final.RData")


#################################################################
##### define functions for PCA regression and prediction ########
#################################################################

# general function for PCA logistic regression and tuning

PCA.logreg<-function(outcome, PCA.cols, data, folds=5,num.PCs=NA,
                     max.comp=50,cv.reps=100){
  
  if(folds>length(data[,1])){
    stop("number of folds greater than number of observations")}
  
  out<-list()
  
  if(length(outcome)>1){data = cbind(data,OuTcOmE=outcome);outcome="OuTcOmE"}
  
  pred.names<- colnames(data)[colnames(data)!=outcome]
  num.preds<-colnames(data)[colnames(data)%in%PCA.cols & 
                              colnames(data)%in%pred.names]
  cat.preds<-colnames(data)[!colnames(data)%in%PCA.cols& 
                              colnames(data)%in%pred.names]
  
  out$pca<-PCA(scale(data[,num.preds]),
               ncp=(length(num.preds)-1),graph=FALSE)
  
  scaled.comps<-scale(out$pca$ind$coord)
  
  if (is.na(num.PCs)){
    
    nc.range<-1:min(length(num.preds)-1,max.comp)
    
    cv.mat<-matrix(NA,cv.reps,length(nc.range))
    
    for(r in 1:cv.reps){
    
      cv.vec<-sample(rep(1:folds,(length(data[,1])/folds)+1)[1:length(data[,1])])
    
      cv.dat<-expand.grid(nc=nc.range,test.set=1:folds,test.r=NA)
    
      for (f in 1:length(cv.dat$nc)){
        train.dat<-data[cv.vec!=cv.dat$test.set[f],c(outcome,cat.preds),drop=FALSE]
        train.dat<-cbind(train.dat,
                        scaled.comps[cv.vec!=cv.dat$test.set[f],
                                    1:cv.dat$nc[f],drop=FALSE])
        test.dat<-data[cv.vec==cv.dat$test.set[f],c(outcome,cat.preds),drop=FALSE]
        test.dat<-cbind(test.dat,
                       scaled.comps[cv.vec==cv.dat$test.set[f],
                                   1:cv.dat$nc[f],drop=FALSE])
        tmp.mod<-glm(paste0(outcome,"~."),train.dat,family = "binomial")
        cv.dat$test.r[f]<-auc(test.dat[,outcome],predict(tmp.mod,newdata = test.dat,type="response"),quiet=TRUE)

      }
    
      cv.mat[r,]<-tapply(cv.dat$test.r,cv.dat$nc,mean)
      
    }

    out$cv.mat<-cv.mat
    out$cv.means<-colMeans(out$cv.mat)
    
    out$winning.nc<-which(out$cv.means==max(out$cv.means))
    
  } else {out$winning.nc<-num.PCs}
  
  #estimate winnings model
  out$winning.model<-glm(paste0(outcome,"~."),
                        cbind(data[,c(outcome,cat.preds),drop=FALSE],
                              scaled.comps[,1:out$winning.nc,drop=FALSE]),family = "binomial")
  
  # project feature weights
  beta.mat<-out$winning.model$coefficients
  beta.mat<-as.matrix(beta.mat[paste0("Dim.",1:out$winning.nc)])
  
  out$fw<-out$pca$var$coord[,1:out$winning.nc]%*%beta.mat
  
  out
  
}

# general function for PCA logistic regression prediction in new data

PCA.logpredict <-function(pcr,newdata){
  
  vars<-row.names(pcr$pca$var$coord)
  
  # component scores in new data
  new.comps<-matrix(dim(as.matrix(pcr$pca$var$coord[,1:pcr$winning.nc])))
  
  loadings<-as.matrix(pcr$pca$var$coord[,1:pcr$winning.nc,drop=FALSE])
  
  for(l in 1:pcr$winning.nc){
    loadings[,l]<-loadings[,l]/sqrt(pcr$pca$eig[l,1])
  }
  
  scores<-as.data.frame(t(t(loadings)%*%t(as.matrix(newdata[,vars]))))
  
  # standardize with reference to training data components
  s.means<-sapply(data.frame(pcr$pca$ind$coord[,1:pcr$winning.nc,drop=FALSE]),mean)
  s.sds<-sapply(data.frame(pcr$pca$ind$coord[,1:pcr$winning.nc,drop=FALSE]),sd)
  for (v in names(s.means)){
    scores[,v]<-(scores[,v]-s.means[v])/s.sds[v]}
  
  
  #predict with new data and new component scores
  new<-cbind(scores,newdata[,!colnames(newdata)%in%vars,drop=FALSE])
  out<-predict(pcr$winning.model,new,type="response")
  out
}

# PCR function specialized for MLS prediction and cross-validation

pcr.MLS<-function(train, test,train.out,test.out,num.PCs=NA){
  
  #scale continuous
  s.means<-sapply(train[,sapply(train,is.numeric)],mean)
  s.sds<-sapply(train[,sapply(train,is.numeric)],sd)
  for (v in names(s.means)){
    train[,v]<-(train[,v]-s.means[v])/s.sds[v]
    test[,v]<-(test[,v]-s.means[v])/s.sds[v]}
  
  #dummy variables
  if(sum(sapply(train,is.factor))>0){
  train<-dummy_cols(train,remove_first_dummy = TRUE,
                    remove_selected_columns = TRUE)
  test<-dummy_cols(test,remove_first_dummy = TRUE,
                   remove_selected_columns = TRUE)}
  
  out<-list()
  
  out$fits<-PCA.logreg(outcome = train.out,PCA.cols = colnames(train),data = train,
                       num.PCs=num.PCs)
  out$train<-predict(out$fits$winning.model,type="response")
  out$test<-PCA.logpredict(out$fits,test)
  
  out$results<-data.frame(prob=out$train,
                          act=train.out, type="train")
  out$results<-rbind(out$results,data.frame(prob=out$test,
                                          act=test.out, type="test") )
  out
}


##########################################################
##### prediction of substance use disorders ##############
##########################################################

# load saved structures
load("ps_all_final.RData")
load("ps_only_final.RData")
load("ps_without_final.RData")
load("folds_MLS_SUD.RData")

# drug use disorder (DUD) 

pcr.DUD.all<-list()
pcr.DUD.cog<-list()
pcr.DUD.sr<-list()
pcr.DUD.su<-list()
pcr.DUD.psysoc.nosr<-list()
pcr.DUD.NOcog<-list()
pcr.DUD.NOsr<-list()
pcr.DUD.NOsu<-list()
pcr.DUD.NOpsysoc.nosr<-list()

pcr.DUD.results<-data.frame()


for (f in 1:5){
  
  
  pcr.DUD.all[[as.character(f)]]<-pcr.MLS(ps.all[[f]]$train, ps.all[[f]]$test,
                                          dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.all[[as.character(f)]]$results,fold=f,ps="all"))
  
  pcr.DUD.cog[[as.character(f)]]<-pcr.MLS(ps.cog[[f]]$train, ps.cog[[f]]$test,
                                          dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.cog[[as.character(f)]]$results,fold=f,ps="cog"))
  
  pcr.DUD.sr[[as.character(f)]]<-pcr.MLS(ps.sr[[f]]$train, ps.sr[[f]]$test,
                                          dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.sr[[as.character(f)]]$results,fold=f,ps="sr"))
  
  pcr.DUD.su[[as.character(f)]]<-pcr.MLS(ps.su[[f]]$train, ps.su[[f]]$test,
                                         dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.su[[as.character(f)]]$results,fold=f,ps="su"))
  
  
  pcr.DUD.psysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.psysoc.nosr[[f]]$train, ps.psysoc.nosr[[f]]$test,
                                         dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.psysoc.nosr[[as.character(f)]]$results,fold=f,ps="psysoc.nosr"))

  pcr.DUD.NOcog[[as.character(f)]]<-pcr.MLS(ps.NOcog[[f]]$train, ps.NOcog[[f]]$test,
                                          dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.NOcog[[as.character(f)]]$results,fold=f,ps="NOcog"))
  
  pcr.DUD.NOsr[[as.character(f)]]<-pcr.MLS(ps.NOsr[[f]]$train, ps.NOsr[[f]]$test,
                                         dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.NOsr[[as.character(f)]]$results,fold=f,ps="NOsr"))
 
  pcr.DUD.NOsu[[as.character(f)]]<-pcr.MLS(ps.NOsu[[f]]$train, ps.NOsu[[f]]$test,
                                           dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.NOsu[[as.character(f)]]$results,fold=f,ps="NOsu"))
   
  pcr.DUD.NOpsysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.NOpsysoc.nosr[[f]]$train, ps.NOpsysoc.nosr[[f]]$test,
                                                  dat[!dat$fold==f,]$DUD,dat[dat$fold==f,]$DUD)
  pcr.DUD.results<-rbind(pcr.DUD.results,
                         data.frame(pcr.DUD.NOpsysoc.nosr[[as.character(f)]]$results,fold=f,ps="NOpsysoc.nosr"))
  
}

save.image("PCR_robust_DUD.RData")


# Alcohol use disorder (AUD)

pcr.AUD.all<-list()
pcr.AUD.cog<-list()
pcr.AUD.sr<-list()
pcr.AUD.su<-list()
pcr.AUD.psysoc.nosr<-list()
pcr.AUD.NOcog<-list()
pcr.AUD.NOsr<-list()
pcr.AUD.NOsu<-list()
pcr.AUD.NOpsysoc.nosr<-list()

pcr.AUD.results<-data.frame()


for (f in 1:5){
  
  # PCR
  
  pcr.AUD.all[[as.character(f)]]<-pcr.MLS(ps.all[[f]]$train, ps.all[[f]]$test,
                                          dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.all[[as.character(f)]]$results,fold=f,ps="all"))
  
  pcr.AUD.cog[[as.character(f)]]<-pcr.MLS(ps.cog[[f]]$train, ps.cog[[f]]$test,
                                          dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.cog[[as.character(f)]]$results,fold=f,ps="cog"))
  
  pcr.AUD.sr[[as.character(f)]]<-pcr.MLS(ps.sr[[f]]$train, ps.sr[[f]]$test,
                                         dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.sr[[as.character(f)]]$results,fold=f,ps="sr"))
  
  pcr.AUD.su[[as.character(f)]]<-pcr.MLS(ps.su[[f]]$train, ps.su[[f]]$test,
                                         dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.su[[as.character(f)]]$results,fold=f,ps="su"))
  
  
  pcr.AUD.psysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.psysoc.nosr[[f]]$train, ps.psysoc.nosr[[f]]$test,
                                                  dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.psysoc.nosr[[as.character(f)]]$results,fold=f,ps="psysoc.nosr"))
  
  pcr.AUD.NOcog[[as.character(f)]]<-pcr.MLS(ps.NOcog[[f]]$train, ps.NOcog[[f]]$test,
                                            dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.NOcog[[as.character(f)]]$results,fold=f,ps="NOcog"))
  
  pcr.AUD.NOsr[[as.character(f)]]<-pcr.MLS(ps.NOsr[[f]]$train, ps.NOsr[[f]]$test,
                                           dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.NOsr[[as.character(f)]]$results,fold=f,ps="NOsr"))
  
  pcr.AUD.NOsu[[as.character(f)]]<-pcr.MLS(ps.NOsu[[f]]$train, ps.NOsu[[f]]$test,
                                           dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.NOsu[[as.character(f)]]$results,fold=f,ps="NOsu"))
  
  pcr.AUD.NOpsysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.NOpsysoc.nosr[[f]]$train, ps.NOpsysoc.nosr[[f]]$test,
                                                    dat[!dat$fold==f,]$AUD,dat[dat$fold==f,]$AUD)
  pcr.AUD.results<-rbind(pcr.AUD.results,
                         data.frame(pcr.AUD.NOpsysoc.nosr[[as.character(f)]]$results,fold=f,ps="NOpsysoc.nosr"))
}

save.image("PCR_robust_AUD.RData")


# Substance use disorder (SUD)

pcr.SUD.all<-list()
pcr.SUD.cog<-list()
pcr.SUD.sr<-list()
pcr.SUD.su<-list()
pcr.SUD.psysoc.nosr<-list()
pcr.SUD.NOcog<-list()
pcr.SUD.NOsr<-list()
pcr.SUD.NOsu<-list()
pcr.SUD.NOpsysoc.nosr<-list()

pcr.SUD.results<-data.frame()

for (f in 1:5){
  
  # PCR
  
  pcr.SUD.all[[as.character(f)]]<-pcr.MLS(ps.all[[f]]$train, ps.all[[f]]$test,
                                          dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.all[[as.character(f)]]$results,fold=f,ps="all"))
  
  pcr.SUD.cog[[as.character(f)]]<-pcr.MLS(ps.cog[[f]]$train, ps.cog[[f]]$test,
                                          dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.cog[[as.character(f)]]$results,fold=f,ps="cog"))
  
  pcr.SUD.sr[[as.character(f)]]<-pcr.MLS(ps.sr[[f]]$train, ps.sr[[f]]$test,
                                         dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.sr[[as.character(f)]]$results,fold=f,ps="sr"))
  
  pcr.SUD.su[[as.character(f)]]<-pcr.MLS(ps.su[[f]]$train, ps.su[[f]]$test,
                                         dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.su[[as.character(f)]]$results,fold=f,ps="su"))
  
  
  pcr.SUD.psysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.psysoc.nosr[[f]]$train, ps.psysoc.nosr[[f]]$test,
                                                  dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.psysoc.nosr[[as.character(f)]]$results,fold=f,ps="psysoc.nosr"))
  
  pcr.SUD.NOcog[[as.character(f)]]<-pcr.MLS(ps.NOcog[[f]]$train, ps.NOcog[[f]]$test,
                                            dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.NOcog[[as.character(f)]]$results,fold=f,ps="NOcog"))
  
  pcr.SUD.NOsr[[as.character(f)]]<-pcr.MLS(ps.NOsr[[f]]$train, ps.NOsr[[f]]$test,
                                           dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.NOsr[[as.character(f)]]$results,fold=f,ps="NOsr"))
  
  pcr.SUD.NOsu[[as.character(f)]]<-pcr.MLS(ps.NOsu[[f]]$train, ps.NOsu[[f]]$test,
                                           dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.NOsu[[as.character(f)]]$results,fold=f,ps="NOsu"))
  
  
  pcr.SUD.NOpsysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.NOpsysoc.nosr[[f]]$train, ps.NOpsysoc.nosr[[f]]$test,
                                                    dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD)
  pcr.SUD.results<-rbind(pcr.SUD.results,
                         data.frame(pcr.SUD.NOpsysoc.nosr[[as.character(f)]]$results,fold=f,ps="NOpsysoc.nosr"))

}

save.image("PCR_robust_SUD.RData")

# SUD prediction with a smaller number of components

pcr1.SUD.all<-list()
pcr1.SUD.cog<-list()
pcr1.SUD.sr<-list()
pcr1.SUD.su<-list()
pcr1.SUD.psysoc.nosr<-list()
pcr1.SUD.NOcog<-list()
pcr1.SUD.NOsr<-list()
pcr1.SUD.NOsu<-list()
pcr1.SUD.NOpsysoc.nosr<-list()

pcr1.SUD.results<-data.frame()

pcr2.SUD.all<-list()
pcr2.SUD.cog<-list()
pcr2.SUD.sr<-list()
pcr2.SUD.su<-list()
pcr2.SUD.psysoc.nosr<-list()
pcr2.SUD.NOcog<-list()
pcr2.SUD.NOsr<-list()
pcr2.SUD.NOsu<-list()
pcr2.SUD.NOpsysoc.nosr<-list()

pcr2.SUD.results<-data.frame()

for (f in 1:5){
  
  # PCR one component
  
  pcr1.SUD.all[[as.character(f)]]<-pcr.MLS(ps.all[[f]]$train, ps.all[[f]]$test,
                                          dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.all[[as.character(f)]]$results,fold=f,ps="all"))
  
  pcr1.SUD.cog[[as.character(f)]]<-pcr.MLS(ps.cog[[f]]$train, ps.cog[[f]]$test,
                                          dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.cog[[as.character(f)]]$results,fold=f,ps="cog"))
  
  pcr1.SUD.sr[[as.character(f)]]<-pcr.MLS(ps.sr[[f]]$train, ps.sr[[f]]$test,
                                         dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.sr[[as.character(f)]]$results,fold=f,ps="sr"))
  
  pcr1.SUD.su[[as.character(f)]]<-pcr.MLS(ps.su[[f]]$train, ps.su[[f]]$test,
                                          dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                          data.frame(pcr1.SUD.su[[as.character(f)]]$results,fold=f,ps="su"))
  
  pcr1.SUD.psysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.psysoc.nosr[[f]]$train, ps.psysoc.nosr[[f]]$test,
                                                  dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.psysoc.nosr[[as.character(f)]]$results,fold=f,ps="psysoc.nosr"))
  
  pcr1.SUD.NOcog[[as.character(f)]]<-pcr.MLS(ps.NOcog[[f]]$train, ps.NOcog[[f]]$test,
                                            dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.NOcog[[as.character(f)]]$results,fold=f,ps="NOcog"))
  
  pcr1.SUD.NOsr[[as.character(f)]]<-pcr.MLS(ps.NOsr[[f]]$train, ps.NOsr[[f]]$test,
                                           dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.NOsr[[as.character(f)]]$results,fold=f,ps="NOsr"))
  
  pcr1.SUD.NOsu[[as.character(f)]]<-pcr.MLS(ps.NOsu[[f]]$train, ps.NOsu[[f]]$test,
                                            dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                          data.frame(pcr1.SUD.NOsu[[as.character(f)]]$results,fold=f,ps="NOsu"))
  
  pcr1.SUD.NOpsysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.NOpsysoc.nosr[[f]]$train, ps.NOpsysoc.nosr[[f]]$test,
                                                    dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=1)
  pcr1.SUD.results<-rbind(pcr1.SUD.results,
                         data.frame(pcr1.SUD.NOpsysoc.nosr[[as.character(f)]]$results,fold=f,ps="NOpsysoc.nosr"))
  
  
  # PCR two components
  
  
  pcr2.SUD.all[[as.character(f)]]<-pcr.MLS(ps.all[[f]]$train, ps.all[[f]]$test,
                                            dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.all[[as.character(f)]]$results,fold=f,ps="all"))
  
  pcr2.SUD.cog[[as.character(f)]]<-pcr.MLS(ps.cog[[f]]$train, ps.cog[[f]]$test,
                                            dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.cog[[as.character(f)]]$results,fold=f,ps="cog"))
  
  pcr2.SUD.sr[[as.character(f)]]<-pcr.MLS(ps.sr[[f]]$train, ps.sr[[f]]$test,
                                           dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.sr[[as.character(f)]]$results,fold=f,ps="sr"))
  
  pcr2.SUD.su[[as.character(f)]]<-pcr.MLS(ps.su[[f]]$train, ps.su[[f]]$test,
                                          dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.su[[as.character(f)]]$results,fold=f,ps="su"))
  
  pcr2.SUD.psysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.psysoc.nosr[[f]]$train, ps.psysoc.nosr[[f]]$test,
                                                    dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.psysoc.nosr[[as.character(f)]]$results,fold=f,ps="psysoc.nosr"))
  
  pcr2.SUD.NOcog[[as.character(f)]]<-pcr.MLS(ps.NOcog[[f]]$train, ps.NOcog[[f]]$test,
                                              dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.NOcog[[as.character(f)]]$results,fold=f,ps="NOcog"))
  
  pcr2.SUD.NOsr[[as.character(f)]]<-pcr.MLS(ps.NOsr[[f]]$train, ps.NOsr[[f]]$test,
                                             dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.NOsr[[as.character(f)]]$results,fold=f,ps="NOsr"))
  
  pcr2.SUD.NOsu[[as.character(f)]]<-pcr.MLS(ps.NOsu[[f]]$train, ps.NOsu[[f]]$test,
                                            dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.NOsu[[as.character(f)]]$results,fold=f,ps="NOsu"))
  
  pcr2.SUD.NOpsysoc.nosr[[as.character(f)]]<-pcr.MLS(ps.NOpsysoc.nosr[[f]]$train, ps.NOpsysoc.nosr[[f]]$test,
                                                      dat[!dat$fold==f,]$SUD,dat[dat$fold==f,]$SUD,num.PCs=2)
  pcr2.SUD.results<-rbind(pcr2.SUD.results,
                          data.frame(pcr2.SUD.NOpsysoc.nosr[[as.character(f)]]$results,fold=f,ps="NOpsysoc.nosr"))
  
  
}

save.image("PCR_limited_pc_SUD.RData")


#####################################
#### plotting and report results ####
#####################################

# load results
load("PCR_robust_DUD.RData")
load("PCR_robust_AUD.RData")
load("PCR_robust_SUD.RData")
load("PCR_limited_pc_SUD.RData")

# custom functions for AUC and AUC differences adapted from the cvAUC
# package and cvAUC.plus extension to this package :
# https://rdrr.io/github/benkeser/cvAUC.plus/src/R/mainFunctions.R

# taken directly from from cvAUC.plus (modifies original cvAUC function to 
# save out the influence functions) except for modification allowing
# vector input for "folds"
ci.cvAUC_withIC <- function(predictions, labels, label.ordering = NULL, folds = NULL, confidence = 0.95) {
  
  # Pre-process the input
  clean <- cvAUC:::.process_input(predictions = predictions, labels = labels, 
                                  label.ordering = label.ordering, folds = folds,
                                  ids = NULL, confidence = confidence)
  
  predictions <- clean$predictions  # Length-V list of predicted values
  labels <- clean$labels  # Length-V list of true labels
  pos <- levels(labels[[1]])[[2]]  # Positive class label
  neg <- levels(labels[[1]])[[1]]  # Negative class label
  n_obs <- length(unlist(labels))  # Number of observations
  
  # Inverse probability weights across entire data set
  w1 <- 1/(sum(unlist(labels) == pos)/n_obs)  # Inverse weights for positive class
  w0 <- 1/(sum(unlist(labels) == neg)/n_obs)  # Inverse weights for negative class
  
  # This is required to cleanly get past R CMD CHECK
  # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  pred <- label <- NULL
  fracNegLabelsWithSmallerPreds <- fracPosLabelsWithLargerPreds <- icVal <- NULL
  
  .IC <- function(fold_preds, fold_labels, pos, neg, w1, w0) {
    n_rows <- length(fold_labels)
    n_pos <- sum(fold_labels == pos)
    n_neg <- n_rows - n_pos
    auc <- cvAUC:::AUC(fold_preds, fold_labels)
    DT <- data.table(pred = fold_preds, label = fold_labels)
    DT <- DT[order(pred, -xtfrm(label))]
    DT[, `:=`(fracNegLabelsWithSmallerPreds, cumsum(label == 
                                                      neg)/n_neg)]
    DT <- DT[order(-pred, label)]
    DT[, `:=`(fracPosLabelsWithLargerPreds, cumsum(label == 
                                                     pos)/n_pos)]
    DT[, `:=`(icVal, ifelse(label == pos, w1 * (fracNegLabelsWithSmallerPreds - 
                                                  auc), w0 * (fracPosLabelsWithLargerPreds - auc)))]
    return(DT$icVal)
  }
  
  icOut <- mapply(FUN = .IC, SIMPLIFY = FALSE, fold_preds = predictions, 
                  fold_labels = labels, MoreArgs = list(pos = pos, neg = neg, w1 = w1, w0 = w0))
  # ic <- rep(NA, n_obs)
  # ic[unlist(folds)] <- unlist(icOut)
  # changed to use vector input rather than list:
  ic <- unlist(icOut)
  # Estimated variance
  sighat2 <- mean(unlist(lapply(icOut, function(x){mean(x^2)})))
  se <- sqrt(sighat2/n_obs)  
  cvauc <- cvAUC::cvAUC(predictions, labels)$cvAUC
  z <- qnorm(confidence + (1 - confidence)/2)
  ci_cvauc <- c(cvauc - (z * se), cvauc + (z * se))
  ci_cvauc[1] <- ifelse(ci_cvauc[1] < 0, 0, ci_cvauc[1])  #Truncate CI at [0,1]
  ci_cvauc[2] <- ifelse(ci_cvauc[2] > 1, 1, ci_cvauc[2]) 
  
  return(list(cvAUC = cvauc, se = se, ci = ci_cvauc, confidence = confidence, ic = ic))
}

# modified cvAUC.plus function changed to directly accept outputs of ci.cvAUC_withIC
diff_cvAUC <- function(fit1, fit2, confidence = 0.95){
  diff <- fit1$cvAUC - fit2$cvAUC
  a <- matrix(c(1,-1))
  icMat <- rbind(fit1$ic, fit2$ic)
  seDiff <- sqrt( tcrossprod(crossprod(a,icMat))) / dim(icMat)[2]
  z <- qnorm(1- (1-confidence)/2)
  ci <- c(diff - z * seDiff, diff + z * seDiff)
  p <- 2*pnorm(-abs(diff/seDiff))
  out <- list(diff = diff, ci = ci, p = p, folds = length(fit1$folds), learner1 = fit1$learner, learner2 = fit2$learner, confidence = confidence)
  class(out) <- "diff_cvAUC"
  return(out)
}

# make structure for test AUCs

SUD.auc<-data.frame(preds=c("all","sr","su","psysoc.nosr","cog","NOsr","NOsu","NOpsysoc.nosr","NOcog"),
                    auc=NA,ci.lo=NA,ci.hi=NA,se=NA)

DUD.auc<-AUD.auc<-SUD.1pc.auc<-SUD.2pc.auc<-SUD.auc

DUD.auc.fit<-AUD.auc.fit<-SUD.1pc.auc.fit<-SUD.2pc.auc.fit<-SUD.auc.fit<-list()

# compute test AUCs by domain

for (p in SUD.auc$preds){
  tmp<-ci.cvAUC_withIC(pcr.SUD.results[pcr.SUD.results$type=="test" & pcr.SUD.results$ps==p,"prob"],
           pcr.SUD.results[pcr.SUD.results$type=="test" & pcr.SUD.results$ps==p,"act"],
           folds=pcr.SUD.results[pcr.SUD.results$type=="test" & pcr.SUD.results$ps==p,"fold"])
  SUD.auc[SUD.auc$preds==p,"auc"]<-tmp$cvAUC; SUD.auc[SUD.auc$preds==p,c("ci.lo","ci.hi")]<-tmp$ci
  SUD.auc[SUD.auc$preds==p,"se"]<-tmp$se; SUD.auc.fit[[p]]<-tmp}

for (p in DUD.auc$preds){
  tmp<-ci.cvAUC_withIC(pcr.DUD.results[pcr.DUD.results$type=="test" & pcr.DUD.results$ps==p,"prob"],
                pcr.DUD.results[pcr.DUD.results$type=="test" & pcr.DUD.results$ps==p,"act"],
                folds=pcr.DUD.results[pcr.DUD.results$type=="test" & pcr.DUD.results$ps==p,"fold"])
  DUD.auc[DUD.auc$preds==p,"auc"]<-tmp$cvAUC; DUD.auc[DUD.auc$preds==p,c("ci.lo","ci.hi")]<-tmp$ci
  DUD.auc[DUD.auc$preds==p,"se"]<-tmp$se; DUD.auc.fit[[p]]<-tmp}

for (p in AUD.auc$preds){
  tmp<-ci.cvAUC_withIC(pcr.AUD.results[pcr.AUD.results$type=="test" & pcr.AUD.results$ps==p,"prob"],
                pcr.AUD.results[pcr.AUD.results$type=="test" & pcr.AUD.results$ps==p,"act"],
                folds=pcr.AUD.results[pcr.AUD.results$type=="test" & pcr.AUD.results$ps==p,"fold"])
  AUD.auc[AUD.auc$preds==p,"auc"]<-tmp$cvAUC; AUD.auc[AUD.auc$preds==p,c("ci.lo","ci.hi")]<-tmp$ci
  AUD.auc[AUD.auc$preds==p,"se"]<-tmp$se; AUD.auc.fit[[p]]<-tmp}

for (p in SUD.1pc.auc$preds){
  tmp<-ci.cvAUC_withIC(pcr1.SUD.results[pcr1.SUD.results$type=="test" & pcr1.SUD.results$ps==p,"prob"],
                pcr1.SUD.results[pcr1.SUD.results$type=="test" & pcr1.SUD.results$ps==p,"act"],
                folds=pcr.SUD.results[pcr.SUD.results$type=="test" & pcr.SUD.results$ps==p,"fold"])
  SUD.1pc.auc[SUD.1pc.auc$preds==p,"auc"]<-tmp$cvAUC; SUD.1pc.auc[SUD.1pc.auc$preds==p,c("ci.lo","ci.hi")]<-tmp$ci
  SUD.1pc.auc[SUD.1pc.auc$preds==p,"se"]<-tmp$se; SUD.1pc.auc.fit[[p]]<-tmp}

for (p in SUD.2pc.auc$preds){
  tmp<-ci.cvAUC_withIC(pcr2.SUD.results[pcr2.SUD.results$type=="test" & pcr2.SUD.results$ps==p,"prob"],
                pcr2.SUD.results[pcr2.SUD.results$type=="test" & pcr2.SUD.results$ps==p,"act"],
                folds=pcr.SUD.results[pcr.SUD.results$type=="test" & pcr.SUD.results$ps==p,"fold"])
  SUD.2pc.auc[SUD.2pc.auc$preds==p,"auc"]<-tmp$cvAUC; SUD.2pc.auc[SUD.auc$preds==p,c("ci.lo","ci.hi")]<-tmp$ci
  SUD.2pc.auc[SUD.2pc.auc$preds==p,"se"]<-tmp$se; SUD.2pc.auc.fit[[p]]<-tmp}

# plots of AUCs by domain

labs<-c("all predictors","SR only","SR substance use only","non-SR psychosocial only",
       "cognitive only","without SR","without SR substance use","without non-SR psychosocial","without cognitive")

jpeg(filename = "SUD_prediction.jpeg",width = 10,height = 3,
     units = "in",res = 500)

par(mfrow=c(1,3),mar=c(5.1, 13, 3, 1.1))


plot(SUD.auc$auc,9:1,yaxt="n",xlim=c(0.4,0.85),main="SUD",
     ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=9:1,labels=labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(SUD.auc$preds)){
  lines(SUD.auc[r,c("ci.lo","ci.hi")],rep(10-r,2),lwd=3)}

plot(DUD.auc$auc,9:1,yaxt="n",xlim=c(0.4,0.85),main="DUD",
     ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=9:1,labels=labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(DUD.auc$preds)){
  lines(DUD.auc[r,c("ci.lo","ci.hi")],rep(10-r,2),lwd=3)}

plot(AUD.auc$auc,9:1,yaxt="n",xlim=c(0.4,0.85),main="AUD",
     ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=9:1,labels=labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(AUD.auc$preds)){
  lines(AUD.auc[r,c("ci.lo","ci.hi")],rep(10-r,2),lwd=3)}

dev.off()

### relevant equivalence tests for any SUD:

diff_cvAUC(SUD.auc.fit$all,SUD.auc.fit$NOsr)


diff_cvAUC(SUD.auc.fit$all,SUD.auc.fit$NOsu)


diff_cvAUC(SUD.auc.fit$all,SUD.auc.fit$NOpsysoc.nosr)


diff_cvAUC(SUD.auc.fit$all,SUD.auc.fit$NOcog)

### relevant equivalence tests for DUD:

diff_cvAUC(DUD.auc.fit$all,DUD.auc.fit$NOsr)


diff_cvAUC(DUD.auc.fit$all,DUD.auc.fit$NOsu)


diff_cvAUC(DUD.auc.fit$all,DUD.auc.fit$NOpsysoc.nosr)


diff_cvAUC(DUD.auc.fit$all,DUD.auc.fit$NOcog)


### relevant equivalence tests for DUD:

diff_cvAUC(AUD.auc.fit$all,AUD.auc.fit$NOsr)


diff_cvAUC(AUD.auc.fit$all,AUD.auc.fit$NOsu)


diff_cvAUC(AUD.auc.fit$all,AUD.auc.fit$NOpsysoc.nosr)


diff_cvAUC(AUD.auc.fit$all,AUD.auc.fit$NOcog)


# plots of AUC by number of components

all.cps<-rbind(SUD.auc[SUD.auc$preds=="all",],
               SUD.1pc.auc[SUD.1pc.auc$preds=="all",])
all.cps<-rbind(all.cps,
               SUD.2pc.auc[SUD.2pc.auc$preds=="all",])

sr.cps<-rbind(SUD.auc[SUD.auc$preds=="sr",],
               SUD.1pc.auc[SUD.1pc.auc$preds=="sr",])
sr.cps<-rbind(sr.cps,
               SUD.2pc.auc[SUD.2pc.auc$preds=="sr",])

su.cps<-rbind(SUD.auc[SUD.auc$preds=="su",],
              SUD.1pc.auc[SUD.1pc.auc$preds=="su",])
su.cps<-rbind(su.cps,
              SUD.2pc.auc[SUD.2pc.auc$preds=="su",])

psysoc.nosr.cps<-rbind(SUD.auc[SUD.auc$preds=="psysoc.nosr",],
              SUD.1pc.auc[SUD.1pc.auc$preds=="psysoc.nosr",])
psysoc.nosr.cps<-rbind(psysoc.nosr.cps,
              SUD.2pc.auc[SUD.2pc.auc$preds=="psysoc.nosr",])

cog.cps<-rbind(SUD.auc[SUD.auc$preds=="cog",],
              SUD.1pc.auc[SUD.1pc.auc$preds=="cog",])
cog.cps<-rbind(cog.cps,
              SUD.2pc.auc[SUD.2pc.auc$preds=="cog",])


jpeg(filename = "prediction_by_component.jpeg",width = 7,height = 5,
     units = "in",res = 500)

pc.labs<-c("full model","1 component","2 components")

par(mfrow=c(2,3),mar=c(5.1, 8, 3, 1.1))

plot(all.cps$auc,3:1,yaxt="n",xlim=c(0.4,0.85),ylim=c(.5,3.5),
     main="all predictors",ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=3:1,labels=pc.labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(all.cps$preds)){
  lines(all.cps[r,c("ci.lo","ci.hi")],rep(4-r,2),lwd=3)}

plot(sr.cps$auc,3:1,yaxt="n",xlim=c(0.4,0.85),ylim=c(.5,3.5),
     main="SR",ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=3:1,labels=pc.labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(sr.cps$preds)){
  lines(sr.cps[r,c("ci.lo","ci.hi")],rep(4-r,2),lwd=3)}

plot(su.cps$auc,3:1,yaxt="n",xlim=c(0.4,0.85),ylim=c(.5,3.5),
     main="SR substance use",ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=3:1,labels=pc.labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(su.cps$preds)){
  lines(su.cps[r,c("ci.lo","ci.hi")],rep(4-r,2),lwd=3)}

plot(psysoc.nosr.cps$auc,3:1,yaxt="n",xlim=c(0.4,0.85),ylim=c(.5,3.5),
     main="non-SR psychosocial",ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=3:1,labels=pc.labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(psysoc.nosr.cps$preds)){
  lines(psysoc.nosr.cps[r,c("ci.lo","ci.hi")],rep(4-r,2),lwd=3)}

plot(cog.cps$auc,3:1,yaxt="n",xlim=c(0.4,0.85),ylim=c(.5,3.5),
     main="cognitive",ylab="",xlab="AUC",pch=16,cex=1.5)
axis(2, at=3:1,labels=pc.labs, las=2)
# reference lines for effect sizes
for(l in c(.4,.6,.7,.8)){
  abline(v=l,col="gray70",lty=2,lwd=1)}
abline(v=.5,col="gray70",lty=1,lwd=2)
# add error bars
for (r in 1:length(cog.cps$preds)){
  lines(cog.cps[r,c("ci.lo","ci.hi")],rep(4-r,2),lwd=3)}


dev.off()

save.image("all_pred_results.RData")

### relevant equivalence tests for limited numbers of components 

diff_cvAUC(SUD.auc.fit$all,SUD.1pc.auc.fit$all)


diff_cvAUC(SUD.auc.fit$all,SUD.2pc.auc.fit$all)


diff_cvAUC(SUD.auc.fit$sr,SUD.1pc.auc.fit$sr)


diff_cvAUC(SUD.auc.fit$sr,SUD.2pc.auc.fit$sr)


diff_cvAUC(SUD.auc.fit$su,SUD.1pc.auc.fit$su)


diff_cvAUC(SUD.auc.fit$psysoc.nosr,SUD.1pc.auc.fit$psysoc.nosr)


diff_cvAUC(SUD.auc.fit$cog,SUD.1pc.auc.fit$cog)


##############################################################
### interpret components using average of empirical fits ####$
##############################################################

# how similar are the first and second component loadings across folds?

pc1.loadings<-as.data.frame(matrix(NA,300,5))
for(f in 1:5){pc1.loadings[,f]<-pcr.SUD.all[[f]]$fits$pca$var$coord[,1]}
colnames(pc1.loadings)<-c(paste0("f",1:5)); rownames(pc1.loadings)<-names(pcr.SUD.all[[f]]$fits$pca$var$coord[,1])

pc2.loadings<-as.data.frame(matrix(NA,300,5))
for(f in 1:5){pc2.loadings[,f]<-pcr.SUD.all[[f]]$fits$pca$var$coord[,2]}
colnames(pc2.loadings)<-c(paste0("f",1:5)); rownames(pc2.loadings)<-names(pcr.SUD.all[[f]]$fits$pca$var$coord[,1])

pc1.loadings$mean<-rowMeans(pc1.loadings[,1:5])
pc2.loadings$mean<-rowMeans(pc2.loadings[,1:5])

cor(pc1.loadings[,1:5])
#            f1        f2        f3        f4        f5
# f1 1.0000000 0.9939902 0.9942540 0.9904838 0.9927553
# f2 0.9939902 1.0000000 0.9926956 0.9909720 0.9931662
# f3 0.9942540 0.9926956 1.0000000 0.9918963 0.9927112
# f4 0.9904838 0.9909720 0.9918963 1.0000000 0.9927951
# f5 0.9927553 0.9931662 0.9927112 0.9927951 1.0000000

mean(cor(pc1.loadings[,1:5])[lower.tri(cor(pc1.loadings[,1:5]))])
#[1] 0.992572
range(cor(pc1.loadings[,1:5])[lower.tri(cor(pc1.loadings[,1:5]))])
#[1] 0.9904838 0.9942540

cor(pc2.loadings[,1:5])
# f1        f2        f3        f4        f5
# f1 1.0000000 0.9730440 0.9836273 0.9756926 0.9706930
# f2 0.9730440 1.0000000 0.9810241 0.9852357 0.9764701
# f3 0.9836273 0.9810241 1.0000000 0.9842454 0.9823956
# f4 0.9756926 0.9852357 0.9842454 1.0000000 0.9839972
# f5 0.9706930 0.9764701 0.9823956 0.9839972 1.0000000

mean(cor(pc2.loadings[,1:5])[lower.tri(cor(pc2.loadings[,1:5]))])
#[1] 0.9796425
range(cor(pc2.loadings[,1:5])[lower.tri(cor(pc2.loadings[,1:5]))])
#[1] 0.9706930 0.9852357

# what are the beta weights for these components?
comp.coefs<-as.data.frame(matrix(NA,2,5))
for(f in 1:5){comp.coefs[,f]<-pcr2.SUD.all[[f]]$fits$winning.model$coefficients[2:3]}
comp.coefs
#          V1        V2        V3        V4        V5
# 1 0.7405982 0.5577993 0.7256441 0.5869401 0.5911187
# 2 0.6118842 0.4916640 0.4690400 0.5349788 0.6241682
rowMeans(comp.coefs)
#[1] 0.6404201 0.5463470


################################################################################
### interpret components using overall group PCA and mean loadings ############
################################################################################

# missing data imputation
full.dat<-dat[,pred.cols]
full.dat<-missForest(xmis = full.dat)
save(full.dat,file="full_data_imputed.RData")

full.dat<-full.dat$ximp

s.means<-sapply(full.dat[,sapply(full.dat,is.numeric)],mean)
s.sds<-sapply(full.dat[,sapply(full.dat,is.numeric)],sd)
for (v in names(s.means)){
  full.dat[,v]<-(full.dat[,v]-s.means[v])/s.sds[v]}
full.dat<-dummy_cols(full.dat,remove_first_dummy = TRUE,
                      remove_selected_columns = TRUE)


pca.all<-PCA(full.dat,ncp = 2)
write.csv(pca.all$var$coord[,1:2],file="loadings_all.csv")

loadings<-data.frame(pca.all$var$coord[,1:2])

# these whole-group loadings are parctically identical to 
# the mean loadings from the fold-wise PCAs

cor(loadings$Dim.1,pc1.loadings$mean)
#[1] 0.9990891
cor(loadings$Dim.2,pc2.loadings$mean)
#[1] 0.9981581

# output all for supplemental

loadings.out<-round(loadings[,1:2],2)
write.csv(loadings.out,file="all_feature_loadings.csv")

# make plots of relevant features

loadings$inc<-(abs(loadings[,1])>.30 | abs(loadings[,2])>.30)

sum(loadings$inc)

save.image("all_pred_results.RData")

### look at DUD/AUD incidence across the different dimensions

scaled.comps<-pca.all$ind$coord
scaled.comps[,1]<-scale(scaled.comps[,1])
scaled.comps[,2]<-scale(scaled.comps[,2])

jpeg(filename = "quadrants.jpg",width = 12,height = 4,units = "in",res = 300)

par(mfrow=c(1,3))

plot(scaled.comps[dat$SUD==0,1],
     scaled.comps[dat$SUD==0,2],xlim=c(-3,5),ylim=c(-4,4),
     main="SUD = red",xlab="externalizing",ylab="social boldness")
abline(h=0);abline(v=0)
points(scaled.comps[dat$SUD==1,1],
       scaled.comps[dat$SUD==1,2],col="red")
mtext(round(mean(dat[scaled.comps[,1]<0 & scaled.comps[,2]>0,"SUD"]),3),
      at=-2.5,side=3,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]<0 & scaled.comps[,2]<0,"SUD"]),3),
      at=-2.5,side=1,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]>0 & scaled.comps[,2]>0,"SUD"]),3),
      at=4,side=3,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]>0 & scaled.comps[,2]<0,"SUD"]),3),
      at=4,side=1,line=-2,col="red")

plot(scaled.comps[dat$DUD==0,1],
     scaled.comps[dat$DUD==0,2],xlim=c(-3,5),ylim=c(-4,4),
     main="DUD = red",xlab="externalizing",ylab="social boldness")
abline(h=0);abline(v=0)
points(scaled.comps[dat$DUD==1,1],
       scaled.comps[dat$DUD==1,2],col="red")
mtext(round(mean(dat[scaled.comps[,1]<0 & scaled.comps[,2]>0,"DUD"]),3),
      at=-2.5,side=3,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]<0 & scaled.comps[,2]<0,"DUD"]),3),
      at=-2.5,side=1,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]>0 & scaled.comps[,2]>0,"DUD"]),3),
      at=4,side=3,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]>0 & scaled.comps[,2]<0,"DUD"]),3),
      at=4,side=1,line=-2,col="red")

plot(scaled.comps[dat$AUD==0,1],
     scaled.comps[dat$AUD==0,2],xlim=c(-3,5),ylim=c(-4,4),
     main="AUD = red",xlab="externalizing",ylab="social boldness")
abline(h=0);abline(v=0)
points(scaled.comps[dat$AUD==1,1],
       scaled.comps[dat$AUD==1,2],col="red")
mtext(round(mean(dat[scaled.comps[,1]<0 & scaled.comps[,2]>0,"AUD"]),3),
      at=-2.5,side=3,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]<0 & scaled.comps[,2]<0,"AUD"]),3),
      at=-2.5,side=1,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]>0 & scaled.comps[,2]>0,"AUD"]),3),
      at=4,side=3,line=-2,col="red")
mtext(round(mean(dat[scaled.comps[,1]>0 & scaled.comps[,2]<0,"AUD"]),3),
      at=4,side=1,line=-2,col="red")

dev.off()

# only included loadings
loadings.inc<-loadings[loadings$inc,1:2]

colnames(loadings.inc)<-c("PC1", "PC2")

#reverse the trail-making test loadings
loadings.inc["tmt_timeb_M" ,"PC1"]<-loadings.inc["tmt_timeb_M" ,"PC1"]*-1
loadings.inc["tmt_timeb_M" ,"PC2"]<-loadings.inc["tmt_timeb_M" ,"PC2"]*-1

# make groups of different loading types 

alc.vars<-c(bingedk_14_15="binge drinking days ages 14-15",
           bingedk_16_17="binge drinking days ages 16-17",
           bingedk_18_19="binge drinking days ages 18-19",
           drunk_14_15 = "got drunk ages 14-15",
           drunk_16_19 = "got drunk ages 16-19",
           maxamt_10_13 = "max drinks in one occasion, ages 10-13",
           maxamt_14_15 = "max drinks in one occasion, ages 14-15",
           maxamt_16_17 = "max drinks in one occasion, ages 16-17",
           maxamt_18_19 = "max drinks in one occasion, ages 18-19",
           daypermo_14_15 = "drinking days per month, ages 14-15",
           daypermo_16_17 = "drinking days per month, ages 16-17",
           daypermo_18_19 = "drinking days per month, ages 18-19",
           drinkday_14_15 = "drinks per drinking day, ages 14-15",
           drinkday_16_17 = "drinks per drinking day, ages 16-17",
           drinkday_18_19 = "drinks per drinking day, ages 18-19",
           alc_probs_14_15="alcohol problems ages 14-15",
           alc_probs_16_17="alcohol problems ages 16-17",
           alc_probs_18_19="alcohol problems ages 18-19",
           age_drink_final = "age first drink",
           age_drunk_final = "age first drunk")



tmp<-as.matrix(round(loadings.inc[names(alc.vars),1:2],2))
rownames(tmp)<-alc.vars
jpeg(filename = "alc_vars.jpeg",width = 12.5,height = 8.5,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()


SU.vars<-c(cig_10_13 = "cigarettes per day, ages 10-13",
           cig_14_15 = "cigarettes per day, ages 14-15",
           cig_16_17 = "cigarettes per day, ages 16-17",
           cig_18_19 = "cigarettes per day, ages 18-19",
           can_m_est_14_15 = "monthly cannabis use, ages 14-15",
           can_m_est_16_17 = "monthly cannabis use, ages 16-17",
           can_m_est_18_19 = "monthly cannabis use, ages 18-19",
           amph_TRUE = "tried amphetamines",
           psyche_TRUE = "tried psychedelics",
           cocaine_TRUE = "tried cocaine",
           narco_TRU = "tried narcotics",
           presc.narco_TRUE = "tried prescription narcotics",
           drug_probs_14_15="drug problems ages 14-15",
           drug_probs_16_17="drug problems ages 16-17",
           drug_probs_18_19="drug problems ages 18-19",
           age_can_final = "age first cannabis")


tmp<-as.matrix(round(loadings.inc[names(SU.vars),1:2],2))
rownames(tmp)<-SU.vars
jpeg(filename = "SU_vars.jpeg",width = 10,height = 7,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()


cog.vars<-c(overall.estM.IQ = "estimated IQ",
            hand_res.comp = "SDMT hand",
            oral_res.comp = "SDMT oral",
            tmt_timeb_M = "Trails B",
            stroop_colornam_M = "Stroop color naming",
            stroop_colorwor_M  = "Stroop incongruent condition",
            v_res = "DDM drift rate")

tmp<-as.matrix(round(loadings.inc[names(cog.vars),1:2],2))
rownames(tmp)<-cog.vars
jpeg(filename = "cog_vars.jpeg",width = 4.8,height = 3.2,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()


# parent reports of psychopathology

psychP.vars<-c(twthdrw_0_9 = "withdrawal, age <10",
                tanxdep_0_9 = "anxiety/depression, age <10",
               tsocpro_0_9 = "social problems, age <10",
               tthotpr_0_9 = "thought problems, age <10",
               tattent_0_9 = "attention problems, age <10",
               tdelinq_0_9 = "delinquency, age <10",
               taggres_0_9 = "aggression, age <10",
               twthdrw_10_13 = "withdrawal, ages 10-13",
               tanxdep_10_13 = "anxiety/depression, ages 10-13",
               tsocpro_10_13 = "social problems, ages 10-13",
               tthotpr_10_13 = "thought problems, ages 10-13",
               tattent_10_13 = "attention problems, ages 10-13",
               tdelinq_10_13 = "delinquency, ages 10-13",
               taggres_10_13 = "aggression, ages 10-13",
               twthdrw_14_18 = "withdrawal, ages 14-18",
               tanxdep_14_18 = "anxiety/depression, ages 14-18",
               tsocpro_14_18 = "social problems, ages 14-18",
               tthotpr_14_18 = "thought problems, ages 14-18",
               tattent_14_18 = "attention problems, ages 14-18",
               tdelinq_14_18 = "delinquency, ages 14-18",
               taggres_14_18 = "aggression, ages 14-18")

tmp<-as.matrix(round(loadings.inc[names(psychP.vars),1:2],2))
rownames(tmp)<-psychP.vars
jpeg(filename = "psychP_vars.jpeg",width = 13,height = 9,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()


# Teacher reports of psychopathology

psychT.vars<-c(twthdrw_teacher = "withdrawal",
               tsomati_teacher = "somatic problems",
               tsocpro_teacher = "social problems",
               tthotpr_teacher = "thought problems",
               tattent_teacher = "attention problems",
               tdelinq_teacher = "delinquency",
               taggres_teacher = "aggression")

tmp<-as.matrix(round(loadings.inc[names(psychT.vars),1:2],2))
rownames(tmp)<-psychT.vars
jpeg(filename = "psychT_vars.jpeg",width = 4.5,height = 3.2,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()

# self-reports of psychopathology

psychS.vars<-c(twthdrw_ysr_10_13 = "withdrawal, ages 10-13",
               tsomati_ysr_10_13  = "somatic, ages 10-13",
               tsocpro_ysr_10_13  = "social problems, ages 10-13",
               tanxdep_ysr_10_13 = "anxiety/depression, ages 10-13",
               tattent_ysr_10_13 = "attention problems, ages 10-13", 
               tdelinq_ysr_10_13 = "delinquency, ages 10-13",
               taggres_ysr_10_13  = "aggression, ages 10-13",
               twthdrw_ysr_14_18 = "withdrawal, ages 14-18",
               tsomati_ysr_14_18 = "somatic, ages 14-18",
               tsocpro_ysr_14_18 = "social problems, ages 14-18",
               tanxdep_ysr_14_18 = "anxiety/depression, ages 14-18",
               tthotpr_ysr_14_18 = "thought problems, ages 14-18",
               tattent_ysr_14_18 = "attention problems, ages 14-18",
               tdelinq_ysr_14_18 = "delinquency, ages 14-18",
               taggres_ysr_14_18 = "aggression, ages 14-18")


tmp<-as.matrix(round(loadings.inc[names(psychS.vars),1:2],2))
rownames(tmp)<-psychS.vars
jpeg(filename = "psychS_vars.jpeg",width = 8.5,height = 6.5,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()

# social and family context

socfam.vars<-c(peer_rel_10_13 = "peer relationship quality, ages 10-13",
             peer_rel_14_18 = "peer relationship quality, ages 14-18", 
             part_music = "participated in music",
             part_ath = "participated in athletics",
             part_other = "participated in other school activities",
             pmC_10_13 = "lack of parental monitoring, child report, ages 10-13", 
             pmC_14_18 = "lack of parental monitoring, child report, ages 14-18",
             pmP = "lack of parental monitoring, parent report",
             cohepar_childhood = "family cohesiveness, childhood",
             confpar_childhood = "family conflict, childhood",
             orgnpar_childhood = "family organization, childhood",
             cohepar_adolescence = "family cohesiveness, adolescence",
             confpar_adolescence = "family conflict, adolescence",
             actipar_adolescence = "family active/recreational orientation, adolescence",
             orgnpar_adolescence = "family organization, adolescence") 

tmp<-as.matrix(round(loadings.inc[names(socfam.vars),1:2],2))
rownames(tmp)<-socfam.vars
jpeg(filename = "socfam_vars.jpeg",width = 8.5,height = 6.5,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()

# personality 

pers.vars<-c(neurotys = "neuroticism, self-report",
             extravys = "extraversion, self-report",
             agreeys = "agreeableness, self-report",
             consys = "conscientiousness, self-report",
             q_resl = "Q-sort resiliency",
             q_nege = "Q-sort negative affect",
             activ = "activity level (DOTS), parent-report",
             appro = "behavioral approach (DOTS), parent-report",
             mood = "mood (DOTS), parent-report",
             rhymslp = "rhythmicity-sleep (DOTS), parent-report",
             rhymeat = "rhythmicity-eating (DOTS), parent-report",
             task = "task orientation (DOTS), parent-report",
             distrac = "lack of distactibility (DOTS), parent-report",
             persist = "persistence (DOTS), parent-report",
             self_activ = "activity level (DOTS), self-report",
             self_flex = "flexibility (DOTS), self-report",
             self_mood = "mood (DOTS), self-report") 

tmp<-as.matrix(round(loadings.inc[names(pers.vars),1:2],2))
rownames(tmp)<-pers.vars
jpeg(filename = "personality_vars.jpeg",width = 9,height = 7,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()


# self-concepts

selfc.vars<-c(sch = "school, childhood",
              soc = "social, childhood",
              ath = "athletic, childhood",
              phy = "physical attractiveness, childhood",
              beh = "behavioral conduct, childhood",
              glo = "global self-worth, childhood",
              school = "school, adolescence",
              social = "social, adolescence",
              athlet = "athletic, adolescence",
              romance = "romance, adolescence",
              conduct = "behavioral conduct, adolescence",
              friend = "making friends, adolescence",
              selfwor = "global self-worth, adolescence") 

tmp<-as.matrix(round(loadings.inc[names(selfc.vars),1:2],2))
rownames(tmp)<-selfc.vars
jpeg(filename = "self_concept_vars.jpeg",width = 7.5,height = 5.5,
     units = "in",res = 500)
corrplot(tmp,cl.pos = 'n',
         method="color",is.corr=TRUE,addCoef.col = 'black',number.cex=1)
dev.off()

### write out all loadings
write.csv(loadings,file="all_loadings.csv")


### proportions of missing data in final sample
final.miss<-colMeans(is.na(dat[,pred.cols]))
final.miss<-as.data.frame(final.miss)
write.csv(final.miss,file="final_missing_props.csv")

save.image("final_anl_for_paper.RData")
