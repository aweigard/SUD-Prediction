rm(list=ls())

# load necessary packages

library(foreign)
library(openxlsx)
library(rvest)
library(reshape)
library(lubridate)
library(lme4)
library(psych)

##########################################################
### define functions for pulling MLS data from archive ###
##########################################################

# transform factors to character variables by default
As.character<-function(x){
  if(is.factor(x)){as.character(x)}else{x}
}

# read MLS T-wave data
read.mls.t<-function(fnam,wave,sample,
                    path="../psych-SubAbuse-MLS/Current-MLS database/spss2000/",
                    scored=FALSE){
  
  if(sample==1){wave=paste0("T",wave,"/")}
  if(sample==2){wave=paste0("Sample II T",wave,"/")}
  if(sample==3){wave=paste0("Sample III T",wave,"/")}
  
  fpath=paste0(path,wave)
  
  if(scored){
    if(length(list.files(fpath,"Scored Files"))<1){fpath=paste0(fpath,"scored/")}else{
      fpath=paste0(fpath,"Scored Files/")}
  }
  
  lf<-list.files(path=fpath,pattern = fnam)
  
  lf<-lf[grep(".sav",lf)]
  lf<-lf[grep("Copy of",lf,invert=TRUE)]
  
  if(length(lf)==0){return("no file")}
  if(length(lf)>1){return("more than one file!")}
  
  dat<-suppressWarnings(read.spss(paste0(fpath,lf[1]),to.data.frame = TRUE))
  
  colnames(dat)<-tolower(colnames(dat))
  
  dat<-data.frame(lapply(dat, As.character), stringsAsFactors=FALSE)
  
  dat
  
}

# read MLS annual wave data
read.mls.a<-function(fnam,wave,sample,
                     path="//maize.umhsnas.med.umich.edu/psych-SubAbuse-MLS/Current-MLS database/spss2000/",
                     scored=FALSE){
  
  if(sample==1){wave=paste0("Annuals/A",wave,"/")}
  if(sample==2){wave=paste0("Annuals/Sample II A",wave,"/")}
  if(sample==3){wave=paste0("Annuals/Sample III A",wave,"/")}
  
  fpath=paste0(path,wave)
  
  if(scored){
    if(length(list.files(fpath,"Scored Files"))<1){fpath=paste0(fpath,"scored/")}else{
    fpath=paste0(fpath,"Scored Files/")}
    }
  
  lf<-list.files(path=fpath,pattern = fnam)
  
  lf<-lf[grep(".sav",lf)]
  lf<-lf[grep("Copy of",lf,invert=TRUE)]
  
  if(length(lf)==0){return("no file")}
  if(length(lf)>1){return("more than one file!")}
  
  dat<-suppressWarnings(read.spss(paste0(fpath,lf[1]),to.data.frame = TRUE))
  
  colnames(dat)<-tolower(colnames(dat))
  
  dat<-data.frame(lapply(dat, As.character), stringsAsFactors=FALSE)
  
  dat
  
}

# compile data across multiple waves into a single list structure
comp.sample<-function(fnam,sample,waves=13,type="T",scored=FALSE,TL=FALSE){
  
  dat<-list()
  
  if(length(waves)==1){waves=1:waves}
  
  if(TL){waves=c(waves,"L")}
  
  if(type=="T"){
  for (w in waves){
    dat[[w]]<-read.mls.t(fnam,w,sample,scored=scored)
  }} else {
    for (w in waves){
      dat[[w]]<-read.mls.a(fnam,w,sample,scored=scored)
    }}
  
  dat
  
}

##################################################
### Read in base data set and DIS SUD outcomes ###
##################################################

# base data set with DIS data and basic demographics

mls.dat<-read.spss("DIS_G2_04202023.sav",to.data.frame = TRUE)

# data set with birth dates of all MLS participants for family analyses

dob<-mls.dat[,c("target","family","dob","sex","race")]

dob$dob<-as.Date(dob$dob/86400,origin = "1582-10-14")

# exclude anyone without diagnostic data

for (c in c(paste0("dud.",20:26),paste0("aud.",20:26))){
  mls.dat[,c]<-as.character(mls.dat[,c])
  mls.dat[mls.dat[,c]%in%"Present",c]<-1
  mls.dat[,c]<-as.numeric(mls.dat[,c])
}

mls.dat$AUD<-as.numeric(rowMeans(mls.dat[,paste0("aud.",20:26)],na.rm=TRUE)>0)
mls.dat$DUD<-as.numeric(rowMeans(mls.dat[,paste0("dud.",20:26)],na.rm=TRUE)>0)
mls.dat$SUD<-as.numeric(rowMeans(mls.dat[,c("AUD","DUD")],na.rm=FALSE)>0)

mls.dat<-mls.dat[!is.na(mls.dat$SUD),]

# format basic demo variables

mls.dat$race<-as.character(mls.dat$race)
mls.dat[mls.dat$race%in%"Caucasian",]$race<-"White"
mls.dat[mls.dat$race%in%c("African American","Native American",
                          "Bi-Racial","Hispanic-caucasian race"),]$race<-"Other"
mls.dat$race<-as.factor(mls.dat$race)

# create master wide-format data frame

G2.master<-mls.dat[,c("target","family","dob")]
G2.master$dob<-as.Date(G2.master$dob/86400,origin = "1582-10-14")
G2.master<-G2.master[!duplicated(G2.master),]
G2.master<-G2.master[!is.na(G2.master$dob),]


########################################################
### G1 (parent) reported Demographics file processing ##
########################################################

# data file with relevant G1 (parent) reported demographics
# (income, highest education, marital status, religions)

Demo_G1.s1<-comp.sample("Demographic G1",sample=1)
Demo_G1.s2<-comp.sample("Demographic G1",sample=2)
Demo_G1.s3<-comp.sample("Demographic G1",sample=3)

save(Demo_G1.s1,Demo_G1.s2,Demo_G1.s3,
     file="Demographics_G1.RData")

# make long-format data file with the most relevant columns

sc<-c("target","family","tmember","tstep","t","tdate","income","selfed","selfdeg","relpres","servpres","howrel",
      paste0("ddob",1:14),paste0("mar",1:14))
demo.long<-do.call("rbind",
             lapply(Demo_G1.s1[Demo_G1.s1!="no file"],function(x){x[,sc]}))
demo.long<-rbind(demo.long,
                 do.call("rbind",
                         lapply(Demo_G1.s2[Demo_G1.s2!="no file"],function(x){x[,sc]})) )
demo.long<-rbind(demo.long,
                 do.call("rbind",
                         lapply(Demo_G1.s3[Demo_G1.s3!="no file"],function(x){x[,sc]})) )

for (d in 1:14){
demo.long[,paste0("ddob",d)]<-as.Date(as.numeric(demo.long[,paste0("ddob",d)])/86400,origin = "1582-10-14")}

demo.long[,"tdate"]<-as.Date(as.numeric(demo.long[,"tdate"])/86400,origin = "1582-10-14")

# compute highest parental education (ABCD categories)

demo.long$high.edu<-NA

c<-c("2. M.A. or M.S.", "2=M.A. or M.S.", 
     "3. Ph.D., M.D., or D.V.M.", "3. Ph.D., M.D., D.V.M., etc.",
     "3=Ph.D; M.D; D.V.M;etc.")
demo.long[demo.long$selfdeg%in%c,]$high.edu<-"Post. Grad."

c<-c("1. bachelors", "1=bachelors" )
demo.long[demo.long$selfdeg%in%c,]$high.edu<-"Bachelors"

c<-c("4. technical, vocational or associates degree",
     "4. vocational, technical, etc.",
     "4=vocational/technical/etc.")
demo.long[demo.long$selfdeg%in%c,]$high.edu<-"Some College"

c<-c("0. none", "0=none", "8. N.A.","8=n/a",NA)
y<-c("16", "14. two years beyond high school, etc",
     "18", "17", "13. one year beyond high school (post-grad, vocational, or c",
     "15", "20. 8 years beyond high school, etc",
     "19","16. 4 years beyond high school (college or voc-tech school)",
     "14. 2 years beyond high school (college or voc-tech school)" ,
     "19. 7 years beyond high school",
     "13. 1 year beyond high school (college or voc-tech school)",
     "15. 3 years beyond high school (college or voc-tech school)",
     "18. 6 years beyond high school",
     "20. 8 or more years beyond high school",
     "17. 5 years beyond high school",
     "1.","2.","3.","4.","5.","6.","7.","8+")
demo.long[demo.long$selfdeg%in%c & 
            demo.long$selfed%in%y,]$high.edu<-"Some College"


c<-c("0. none", "0=none", "8. N.A.","8=n/a",NA)
y<-c("12. twelfth grade (graduated high school)",
     "12. 12th grade","12")
demo.long[demo.long$selfdeg%in%c & 
            demo.long$selfed%in%y,]$high.edu<-"High School"


c<-c("0. none", "0=none", "8. N.A.","8=n/a",NA)
y<-c("0","1", "2", "3", "4", "5", "6", "7",
     "8", "9", "10", "11", "4. 4th grade",
     "9. 9th grade", "10. 10th grade" ,
     "11. 11th grade")
demo.long[demo.long$selfdeg%in%c & 
            demo.long$selfed%in%y,]$high.edu<-"< High School"


# compute ABCD income categories in 2018 dollars

# re-level income variables to be consistent

demo.long$income.clean<-NA

demo.long[demo.long$income%in%c("1. under $4,000",
                                "1=under $4,000"),]$income.clean<-"<4k"
demo.long[demo.long$income%in%c("2. $4000-$7000",
                                "2=$4,000-$7,000"),]$income.clean<-"4k-7k"
demo.long[demo.long$income%in%c("3. $7001-$10,000",
                                "3=$7,001-$10,000"),]$income.clean<-"7001-10k"
demo.long[demo.long$income%in%c("4. $10,001-$13,000",
                                "4=$10,001-$13,000"),]$income.clean<-"10001-13k"
demo.long[demo.long$income%in%c("5. $13,001-$16,000",
                                "5=$13,001-$16,000"),]$income.clean<-"13001-16k"
demo.long[demo.long$income%in%c("6. $16,001-$20, 000",
                                "6=$16,001-$20,000"),]$income.clean<-"16001-20k"
demo.long[demo.long$income%in%c("7. $20,001-$30,000",
                                "7=$20,001-$30,000"),]$income.clean<-"20001-30k"
demo.long[demo.long$income%in%c("8. $30,001-$50,000",
                                "8=$30,001-$50,000"),]$income.clean<-"30001-50k"
demo.long[demo.long$income%in%c("9. $50, 001-$75,000",
                                "9=$50,001-75,000"),]$income.clean<-"50001-75k"
demo.long[demo.long$income%in%c("10. $75,001-$100,000",
                                "10=$75,001-$100,000"),]$income.clean<-"75001-100k"
demo.long[demo.long$income%in%c("11. over $100,000",
                                "11=over $100,000",
                                "12","12=$100,001-$125,000",
                                "13","13=$125,001-$150,000",
                                "14","14=$150,001-$175,000",
                                "15","15=$175,001-$200,000",
                                "16", "16=Over $200,000"),]$income.clean<-">100k"
#only recorded in 1991 or earlier (2018 purchasing power >100k):
demo.long[demo.long$income%in%c("10. over $75,000"),]$income.clean<-">75k" 


# load historical CPI data from the Bureau of 
# Labor statistics

cpi<-read_html("https://data.bls.gov/timeseries/CUUR0000SA0?years_option=all_years")
cpi<-html_table(cpi)[[2]]
cpi<-data.frame(cpi[,1:13])
cpi<-melt(cpi,id = "Year",variable_name = "month")

cpi[cpi$Year==2018 & cpi$month=="Dec",]
# Year month   value
# 1316 2018   Dec 251.233

cpi$eq2018<-cpi$value/251.233

cpi$eq_50k<-cpi$eq2018*50000
cpi$eq_100k<-cpi$eq2018*100000

demo.long$Year<-as.numeric(format(demo.long$tdate,'%Y'))
demo.long$month<-as.numeric(format(demo.long$tdate,'%m'))

demo.long$month<-factor(demo.long$month)
levels(demo.long$month)<-c("Jan","Feb",
                           "Mar","Apr",
                           "May","Jun",
                           "Jul","Aug",
                           "Sep","Oct",
                           "Nov", "Dec")
demo.long<-merge(demo.long,cpi[,c("Year","month","eq_50k", "eq_100k")],
           all.x = TRUE)


# ABCD categories adjusted for inflation

demo.long$inc_ABCD<-NA

max(demo.long$eq_50k,na.rm = TRUE)
#[1] 48750.56
min(demo.long$eq_50k,na.rm = TRUE)
#[1] 18548.52

max(demo.long$eq_100k,na.rm = TRUE)
#[1] 97501.12
min(demo.long$eq_100k,na.rm = TRUE)
#[1] 37097.04

# category or category median always lower than 50k in 2018

c<-c("<4k","4k-7k","7001-10k","10001-13k","13001-16k","16001-20k")
demo.long[demo.long$income.clean%in%c,]$inc_ABCD<-"<50k"

# category or category median always higher than 100k in 2018

demo.long[demo.long$income.clean%in%c(">100k",">75k"),]$inc_ABCD<-">100k"

# classify those in other income categories

demo.long[demo.long$income.clean%in%c("20001-30k") & 
            demo.long$eq_50k>25000.5,]$inc_ABCD<-"<50k"
demo.long[demo.long$income.clean%in%c("20001-30k") & 
            demo.long$eq_50k<=25000.5,]$inc_ABCD<-"50k-100k"

demo.long[demo.long$income.clean%in%c("30001-50k" ) & 
            demo.long$eq_50k>40000.5,]$inc_ABCD<-"<50k"
demo.long[demo.long$income.clean%in%c("30001-50k" ) & 
            demo.long$eq_50k<=40000.5,]$inc_ABCD<-"50k-100k"
demo.long[demo.long$income.clean%in%c("30001-50k" ) & 
            demo.long$eq_100k<40000.5,]$inc_ABCD<-">100k"

demo.long[demo.long$income.clean%in%c("50001-75k"  ) & 
            demo.long$eq_100k>=62500.5,]$inc_ABCD<-"50k-100k"
demo.long[demo.long$income.clean%in%c("50001-75k"  ) & 
            demo.long$eq_100k<62500.5,]$inc_ABCD<-">100k"

demo.long[demo.long$income.clean%in%c("50001-75k"  ) & 
            demo.long$eq_100k>=62500.5,]$inc_ABCD<-"50k-100k"
demo.long[demo.long$income.clean%in%c("50001-75k"  ) & 
            demo.long$eq_100k<62500.5,]$inc_ABCD<-">100k"

demo.long[demo.long$income.clean%in%c("75001-100k") & 
            demo.long$eq_100k>=87500.5 & !is.na(demo.long$eq_100k),]$inc_ABCD<-"50k-100k"
demo.long[demo.long$income.clean%in%c("75001-100k") & 
            demo.long$eq_100k<87500.5 & !is.na(demo.long$eq_100k),]$inc_ABCD<-">100k"


#  Household income low/high based on statistics for 
# each year 

hi.quintiles<-read.xlsx("https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-income-households/h01ar.xlsx")

hi.quintiles<-hi.quintiles[9:65,]  
colnames(hi.quintiles)<-c("Year","n","lowest","second","third",
                          "fourth","top5_limit")
hi.quintiles<-hi.quintiles[!hi.quintiles$Year%in%c("2013 (39)",
                                                   "2017 (40)",
                                                   "2018",
                                                   "2019","2020 (41)","2021"),]
hi.quintiles$Year<-2017:1967

demo.long<-merge(demo.long,
                 hi.quintiles[,c("Year","lowest","fourth")],
                 all.x=TRUE)

demo.long$lowest<-as.numeric(demo.long$lowest)
demo.long$fourth<-as.numeric(demo.long$fourth)

demo.long$inc_rel<-"middle"

c<-c("<4k","4k-7k")
demo.long[demo.long$income.clean%in%c,]$inc_rel<-"low"

demo.long[demo.long$income.clean%in%c("7001-10k") & 
            demo.long$lowest>8500.5,]$inc_rel<-"low"

demo.long[demo.long$income.clean%in%c("10001-13k") & 
            demo.long$lowest>11500.5,]$inc_rel<-"low"

demo.long[demo.long$income.clean%in%c("13001-16k") & 
            demo.long$lowest>14500.5,]$inc_rel<-"low"

demo.long[demo.long$income.clean%in%c("16001-20k") & 
            demo.long$lowest>18000.5,]$inc_rel<-"low"

# no cases in which this is true:
# demo.long[demo.long$income.clean%in%c("30001-50k") & 
#             demo.long$fourth<40000.5,]$inc_rel<-"high"

demo.long[demo.long$income.clean%in%c("50001-75k") & 
            demo.long$fourth<62500.5,]$inc_rel<-"high"

demo.long[demo.long$income.clean%in%c("75001-100k") & 
            demo.long$fourth<87500.5 & 
            !is.na(demo.long$fourth),]$inc_rel<-"high"

# everyone with >75k is in high income tier
demo.long[demo.long$income.clean%in%c(">75k"),]$inc_rel<-"high"

# almost everyone with the vague ">100k" answer is clearly in
# high income tier. 
# those who are not would at least be close if their income was
# more than 100k, so we will estimate them as "high"
demo.long[demo.long$income%in%c("11. over $100,000",
                                "11=over $100,000"),]$inc_rel<-"high"

demo.long[demo.long$income%in%c("12","12=$100,001-$125,000") & 
            demo.long$fourth<112500.5,]$inc_rel<-"high"

demo.long[demo.long$income%in%c("13","13=$125,001-$150,000",
                                "14","14=$150,001-$175,000",
                                "15","15=$175,001-$200,000",
                                "16", "16=Over $200,000"),]$inc_rel<-"high"


# final variable in which everyone >100k is assumed to be 
# "high" income
demo.long[demo.long$income.clean%in%c(">100k"),]$inc_rel<-"high"

# religion

demo.long$rel_catholic<-demo.long$relpres%in%c("13=Roman Catholic","13",
                                               "13. Roman Catholic")
demo.long$rel_baptist<-demo.long$relpres%in%c("15=Baptist","15",
                                               "15. Baptist (all subdenominations)")
demo.long$rel_methodist<-demo.long$relpres%in%c("25=Methodist","25",
                                                "25. Methodist")
demo.long$rel_lutheran<-demo.long$relpres%in%c("24=Lutheran","24",
                                                "24. Lutheran")
demo.long$rel_none<-demo.long$relpres%in%c("97=None","97",
                                           "97. None, no church or religion")
demo.long$rel_other<-demo.long$relpres%in%c("98=Other","28=Pentecostal","29=Presbyterian",
                                            "17=Church of Christ","18=Church of God","14=Assembly of God",
                                            "30. Protestant (unspecified, nondenominational, or interdeno",
                                            "19=Congregational","98","29","7=Mormon (Latter Day Saint)",
                                            "21=Episcopalian","5=Jehovah's Witnes","22=Full Gospel (Tabernacle)",
                                            "27=Nazarene","28","8=Orthodox (Eastern, Greek, Russian, etc.)",
                                            "36=United Brethren","32=Reformed Church","17","7","35=Unitarian",
                                            "34=Seventh Day Adventists","29. Presbyterian","28. Pentecostal",
                                            "37=Wesleyan","2=Christian Scientist","27","14","17. Church of Christ",
                                            "Fundamentalist","18","98. other","30","7. Mormon (Latter Day Saints)",
                                            "32","21","1=Buddhist","34","36. United Brethren","6=Jewish","36",
                                            "27. Church of Nazarene","20=Disciples of Christ","35","22","8","37","5",
                                            "35. Unitarian-Universalist","5. Jehovahs Witness","33=Dutch Reformed Church",
                                            "16=Church of Brethren","19","20","34. Seventh Day Adventist",
                                            "21. Episcopalian","20. Disciples of Christ","18. Church of God","3=Hindu",
                                            "33","23","6","37. Wesleyan","22. Full Gospel (Tabernacle)","19. Congregational",
                                            "14. Assembly of God","11. Orthodox-other","9. Orthodox-Greek","8. Orthodox-Eastern",
                                            "11","4=Islam","12=Other Eastern (e.g Shinto, Taoism)","32. Reformed Church",
                                            "23. Fundamentalist (Unspecified Denomination)","10. Orthodox-Russian","6. Jewish",
                                            "2. Christian Scientist")

demo.long[is.na(demo.long$relpres),
                    grep("rel_",colnames(demo.long))]<-NA


demo.long$rel_how<-NA
demo.long[demo.long$howrel%in%c("1=not at all","1. not at all"),]$rel_how<-1
demo.long[demo.long$howrel%in%c("2=not very","2. not very"),]$rel_how<-2
demo.long[demo.long$howrel%in%c("3=fairly","3. fairly"),]$rel_how<-3
demo.long[demo.long$howrel%in%c("4=very religious","4. very religious",
                                "4=very","4. very"),]$rel_how<-4


demo.long$rel_regatt<-NA
demo.long[demo.long$servpres%in%c("1. several times a week",
                                  "2. about once a week","3. 2-3 times a month",
                                  "3=2-3 times a month","2=about once a week",
                                  "3. 2 to 3 times a month",
                                  "1=several times a week"),]$rel_regatt<-TRUE
demo.long[demo.long$servpres%in%c("5. never","5=never",
                                  "4. once a month or less",
                                  "4=once a month or less"),]$rel_regatt<-FALSE

write.csv(demo.long,row.names = FALSE,file="ABCD_g1_demographics.csv")


##################################################
####### G2 family-level variable processing ######
##################################################

# create data frame with one timepoint for each child and each G1 assessment timepoint

G2.fam<-merge(demo.long[,colnames(demo.long)!="target"],
              G2.master,
              all.x = TRUE)

# pair G1 (parent) measurement time point to G2 (child) age

G2.fam$g2.age<-interval(G2.fam$dob, G2.fam$tdate) / 
                               duration(num = 1, units = "years")

# qualitative labels for developmental periods
G2.fam$devp<-NA
G2.fam[G2.fam$g2.age<0 & !is.na(G2.fam$g2.age),]$devp<-"not born yet"
G2.fam[G2.fam$g2.age>=0 & G2.fam$g2.age<10 & !is.na(G2.fam$g2.age),]$devp<-"childhood"
G2.fam[G2.fam$g2.age>=10 & G2.fam$g2.age<19 & !is.na(G2.fam$g2.age),]$devp<-"adolescence"
G2.fam[G2.fam$g2.age>=19 & !is.na(G2.fam$g2.age),]$devp<-"adulthood"


# highest parental education any wave during childhood or adolescence

G2.master$edu.avail.child<-0
G2.master$edu.avail.adol<-0
G2.master$high.edu<-NA
G2.master$high.edu.child<-NA

educ.rank<-1:5
names(educ.rank)<-c("< High School", "High School",
                    "Some College", "Bachelors",
                    "Post. Grad.")
educ.rank[names(educ.rank)%in%"Bachelors"]

for(s in unique(G2.master$target)){
  if(length(G2.fam[G2.fam$target==s & 
                   G2.fam$devp%in%c("childhood","not born yet") & 
                   !is.na(G2.fam$high.edu),"target"])>0){
    edu<-G2.fam[G2.fam$target==s & G2.fam$devp%in%c("childhood","not born yet") &
                  !is.na(G2.fam$high.edu) & 
                  !is.na(G2.fam$target), "high.edu"]
    edu<-names(educ.rank[max(educ.rank[names(educ.rank)%in%edu])])
    G2.master[G2.master$target==s,]$high.edu.child<-edu
  }
  if(length(G2.fam[G2.fam$target==s & 
                   G2.fam$devp%in%c("adolescence","childhood","not born yet") & 
                   !is.na(G2.fam$high.edu),"target"])>0){
    edu<-G2.fam[G2.fam$target==s & G2.fam$devp%in%c("adolescence","childhood","not born yet") &
                  !is.na(G2.fam$high.edu) & 
                  !is.na(G2.fam$target), "high.edu"]
    edu<-names(educ.rank[max(educ.rank[names(educ.rank)%in%edu])])
    G2.master[G2.master$target==s,]$high.edu<-edu
  }
    G2.master[G2.master$target==s,]$edu.avail.child<-length(G2.fam[G2.fam$target==s & 
                                                              G2.fam$devp%in%c("childhood") &
                                                                !is.na(G2.fam$high.edu),"target"])
    G2.master[G2.master$target==s,]$edu.avail.adol<-length(G2.fam[G2.fam$target==s & 
                                                              G2.fam$devp%in%c("adolescence") &  
                                                                !is.na(G2.fam$high.edu),"target"])
}

# collapse <HS with HS due to low numbers of the former (<10)
table(G2.master$high.edu)

G2.master[G2.master$high.edu%in%c("< High School"),]$high.edu<-"High School"

# mostly stable between childhood and adolescence
table(G2.master$high.edu,G2.master$high.edu.child)


# categorical income variables for child, adolescence, overall

G2.master$inc.avail.child<-0
G2.master$inc.avail.adol<-0

G2.master$ABCD.inc.child<-NA
G2.master$ABCD.inc.adol<-NA
G2.master$ABCD.inc.overall<-NA
G2.master$rel.inc.child<-NA
G2.master$rel.inc.adol<-NA
G2.master$rel.inc.overall<-NA

G2.master$ABCD.nomode.child<-NA
G2.master$ABCD.nomode.adol<-NA
G2.master$ABCD.nomode.overall<-NA
G2.master$rel.nomode.child<-NA
G2.master$rel.nomode.adol<-NA
G2.master$rel.nomode.overall<-NA

G2.master$ever.50k.child<-NA
G2.master$ever.50k.adol<-NA
G2.master$ever.50k.overall<-NA

G2.master$ever.100k.child<-NA
G2.master$ever.100k.adol<-NA
G2.master$ever.100k.overall<-NA

G2.master$ever.low.child<-NA
G2.master$ever.low.adol<-NA
G2.master$ever.low.overall<-NA

G2.master$ever.high.child<-NA
G2.master$ever.high.adol<-NA
G2.master$ever.high.overall<-NA

modal.income<-function(x){
  ux <- unique(x)
  count<-tabulate(match(x, ux))
  if(length(count[count==max(count)])>1){4}
  else{ux[which.max(count)]}
}

for(s in unique(G2.master$target)){
  
  G2.master[G2.master$target==s,]$inc.avail.child<-length(G2.fam[G2.fam$target==s & 
                                                                    G2.fam$devp%in%c("childhood") &
                                                                    !is.na(G2.fam$income.clean),"target"])
  G2.master[G2.master$target==s,]$inc.avail.adol<-length(G2.fam[G2.fam$target==s & 
                                                                   G2.fam$devp%in%c("adolescence") &  
                                                                   !is.na(G2.fam$income.clean),"target"])
  if(G2.master[G2.master$target==s,]$inc.avail.child>0){
    
    c.inc<-G2.fam[G2.fam$target==s & 
                  G2.fam$devp%in%c("childhood") &
                  !is.na(G2.fam$income.clean),c("inc_ABCD","inc_rel")]
    G2.master[G2.master$target==s,]$ABCD.inc.child<-modal.income(c.inc$inc_ABCD)
    G2.master[G2.master$target==s,]$rel.inc.child<-modal.income(c.inc$inc_rel)
    
    if(G2.master[G2.master$target==s,]$ABCD.inc.child==4){
      G2.master[G2.master$target==s,]$ABCD.inc.child<-"50k-100k"
      G2.master[G2.master$target==s,]$ABCD.nomode.child<-TRUE
    } else{G2.master[G2.master$target==s,]$ABCD.nomode.child<-FALSE}
    
    if(G2.master[G2.master$target==s,]$rel.inc.child==4){
      G2.master[G2.master$target==s,]$rel.inc.child<-"middle"
      G2.master[G2.master$target==s,]$rel.nomode.child<-TRUE
    } else{G2.master[G2.master$target==s,]$rel.nomode.child<-FALSE}

    G2.master[G2.master$target==s,]$ever.50k.child<-sum(c.inc$inc_ABCD%in%c("<50k"))>0
    G2.master[G2.master$target==s,]$ever.100k.child<-sum(c.inc$inc_ABCD%in%c(">100k"))>0
    G2.master[G2.master$target==s,]$ever.low.child<-sum(c.inc$inc_rel%in%c("low"))>0
    G2.master[G2.master$target==s,]$ever.high.child<-sum(c.inc$inc_rel%in%c("high"))>0
    
  }
  
  if(G2.master[G2.master$target==s,]$inc.avail.adol>0){
    
    a.inc<-G2.fam[G2.fam$target==s & 
                    G2.fam$devp%in%c("adolescence") &
                    !is.na(G2.fam$income.clean),c("inc_ABCD","inc_rel")]
    G2.master[G2.master$target==s,]$ABCD.inc.adol<-modal.income(a.inc$inc_ABCD)
    G2.master[G2.master$target==s,]$rel.inc.adol<-modal.income(a.inc$inc_rel)
    
    if(G2.master[G2.master$target==s,]$ABCD.inc.adol==4){
      G2.master[G2.master$target==s,]$ABCD.inc.adol<-"50k-100k"
      G2.master[G2.master$target==s,]$ABCD.nomode.adol<-TRUE
    } else{G2.master[G2.master$target==s,]$ABCD.nomode.adol<-FALSE}
    
    if(G2.master[G2.master$target==s,]$rel.inc.adol==4){
      G2.master[G2.master$target==s,]$rel.inc.adol<-"middle"
      G2.master[G2.master$target==s,]$rel.nomode.adol<-TRUE
    } else{G2.master[G2.master$target==s,]$rel.nomode.adol<-FALSE}
    
    G2.master[G2.master$target==s,]$ever.50k.adol<-sum(a.inc$inc_ABCD%in%c("<50k"))>0
    G2.master[G2.master$target==s,]$ever.100k.adol<-sum(a.inc$inc_ABCD%in%c(">100k"))>0
    G2.master[G2.master$target==s,]$ever.low.adol<-sum(a.inc$inc_rel%in%c("low"))>0
    G2.master[G2.master$target==s,]$ever.high.adol<-sum(a.inc$inc_rel%in%c("high"))>0
    
  }
  
  if( sum(G2.master[G2.master$target==s,c("inc.avail.child","inc.avail.adol")])>0 ){
    
    o.inc<-G2.fam[G2.fam$target==s & 
                    G2.fam$devp%in%c("childhood","adolescence") &
                    !is.na(G2.fam$income.clean),c("inc_ABCD","inc_rel")]
    G2.master[G2.master$target==s,]$ABCD.inc.overall<-modal.income(o.inc$inc_ABCD)
    G2.master[G2.master$target==s,]$rel.inc.overall<-modal.income(o.inc$inc_rel)
    
    if(G2.master[G2.master$target==s,]$ABCD.inc.overall==4){
      G2.master[G2.master$target==s,]$ABCD.inc.overall<-"50k-100k"
      G2.master[G2.master$target==s,]$ABCD.nomode.overall<-TRUE
    } else{G2.master[G2.master$target==s,]$ABCD.nomode.overall<-FALSE}
    
    if(G2.master[G2.master$target==s,]$rel.inc.overall==4){
      G2.master[G2.master$target==s,]$rel.inc.overall<-"middle"
      G2.master[G2.master$target==s,]$rel.nomode.overall<-TRUE
    } else{G2.master[G2.master$target==s,]$rel.nomode.overall<-FALSE}
    
    G2.master[G2.master$target==s,]$ever.50k.overall<-sum(o.inc$inc_ABCD%in%c("<50k"))>0
    G2.master[G2.master$target==s,]$ever.100k.overall<-sum(o.inc$inc_ABCD%in%c(">100k"))>0
    G2.master[G2.master$target==s,]$ever.low.overall<-sum(o.inc$inc_rel%in%c("low"))>0
    G2.master[G2.master$target==s,]$ever.high.overall<-sum(o.inc$inc_rel%in%c("high"))>0
    
  }
  
}


### Religion variables #####

G2.master$reli_catholic<-NA
G2.master$reli_baptist<-NA
G2.master$reli_methodist<-NA
G2.master$reli_lutheran<-NA
G2.master$reli_none<-NA
G2.master$reli_other<-NA
G2.master$reli_regatt<-NA
G2.master$reli_how<-NA


for (s in unique(G2.master$target)){
  tmp<-G2.fam[G2.fam$target==s & !is.na(G2.fam$target) & 
                G2.fam$devp%in%c("childhood","adolescence"),]
  if(length(na.omit(tmp$relpres))>0){
  G2.master[G2.master$target==s,]$reli_catholic<-sum(tmp$rel_catholic,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_baptist<-sum(tmp$rel_baptist,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_methodist<-sum(tmp$rel_methodist,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_lutheran<-sum(tmp$rel_lutheran,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_none<-sum(tmp$rel_none,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_other<-sum(tmp$rel_other,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_regatt<-sum(tmp$rel_regatt,na.rm = TRUE)>0
  G2.master[G2.master$target==s,]$reli_how<-mean(tmp$rel_how,na.rm = TRUE)}
}

#### data file with relevant G2 (child_ reported demographics

Demo_G2.s1<-comp.sample("Demographic G2 T",sample=1)
Demo_G2.s2<-comp.sample("Demographic G2 II",sample=2)
Demo_G2.s3<-comp.sample("Demographic G2 III",sample=3)

save(Demo_G2.s1,Demo_G2.s2,Demo_G2.s3,
     file="Demographics_G2.RData")

# make long-format data file with the most relevant columns

sc<-c("target","family","tmember","tstep","t","tsex","tdate","age",
      "dem1a", "dem1b","dem4a","dem4b","dem4c",
      paste0("dem6",letters[1:7]),paste0("dem7",letters[1:4]),
      "dem8")

# fix inconsistently  labeled religion items
Demo_G2.s2[4][[1]]$dem4a<-as.character(Demo_G2.s2[4][[1]]$dem4a)
Demo_G2.s2[5][[1]]$dem4a<-as.character(Demo_G2.s2[5][[1]]$dem4a)

demo.adol.long<-do.call("rbind",
                   lapply(Demo_G2.s1[4:5],function(x){x[,sc]}))
demo.adol.long<-rbind(demo.adol.long,
                 do.call("rbind",
                         lapply(Demo_G2.s2[4:5],function(x){x[,sc]})) )
demo.adol.long<-rbind(demo.adol.long,
                 do.call("rbind",
                         lapply(Demo_G2.s3[3:5],function(x){x[,sc]})) )

demo.adol.long[,"tdate"]<-as.Date(as.numeric(demo.adol.long[,"tdate"])/86400,origin = "1582-10-14")



# ANNUAL data file with relevant G2 (adolescent) demographics

Demo_Annual_G2.s1<-comp.sample("Demographics G2 A",sample=1,waves = c(1:8,18:20),type = "A")
Demo_Annual_G2.s2<-comp.sample("Demographics G2 II A",sample=2,waves = c(1:8,18:20),type = "A")
Demo_Annual_G2.s3<-comp.sample("Demographics G2 III A",sample=3,waves = c(1:8,18:20),type = "A")

# need to manually pull a few in due to inconsistent naming 
Demo_Annual_G2.s3[[1]]<-read.mls.a("Demographic Youth",wave=1,sample=3)
Demo_Annual_G2.s3[[2]]<-read.mls.a("Demographic Youth",wave=2,sample=3)
Demo_Annual_G2.s3[[5]]<-read.mls.a("Demographic G2 III",wave=5,sample=3)
Demo_Annual_G2.s3[[7]]<-read.mls.a("Demographic G2 III",wave=7,sample=3)
Demo_Annual_G2.s3[[18]]<-read.mls.a("Demographic G2 III",wave=18,sample=3)
Demo_Annual_G2.s3[[19]]<-read.mls.a("Demographic G2 III",wave=19,sample=3)
Demo_Annual_G2.s3[[20]]<-read.mls.a("Demographic G2 III",wave=20,sample=3)

save(Demo_Annual_G2.s1,Demo_Annual_G2.s2,Demo_Annual_G2.s3,
     file="Demographics_Annual_G2.RData")

# make long-format data file with the most relevant columns

sc<-c("target","family","tmember","tstep","a","tsex","tdate","age",
      "dem1a", "dem1b","dem4a","dem4b","dem4c",
      paste0("dem6",letters[1:7]),paste0("dem7",letters[1:4]),
      "dem8")


demoA.adol.long<-do.call("rbind",
                        lapply(Demo_Annual_G2.s1[1:8],function(x){x[,sc]}))
demoA.adol.long<-rbind(demoA.adol.long,
                      do.call("rbind",
                              lapply(Demo_Annual_G2.s2[1:7],function(x){x[,sc]})) )
demoA.adol.long<-rbind(demoA.adol.long,
                      do.call("rbind",
                              lapply(Demo_Annual_G2.s3[1:7],function(x){x[,sc]})) )

demoA.adol.long[,"tdate"]<-as.Date(as.numeric(demoA.adol.long[,"tdate"])/86400,origin = "1582-10-14")


# merge annual and t-wave demographics forms 

demoA.adol.long$dem4a<-as.character(demoA.adol.long$dem4a)

demo.adol.long$ta<-paste0("t",demo.adol.long$t)
demoA.adol.long$ta<-paste0("a",demoA.adol.long$a)

demoMerge.adol.long<-rbind(demo.adol.long[,colnames(demo.adol.long)!="t"],
                           demoA.adol.long[,colnames(demoA.adol.long)!="a"])


# 1. religion (dem4a, dem4b, dem4c)

demoMerge.adol.long$rel_catholic<-demoMerge.adol.long$dem4a%in%c("Roman Catholic","13")
demoMerge.adol.long$rel_baptist<-demoMerge.adol.long$dem4a%in%c("Baptist","15")
demoMerge.adol.long$rel_methodist<-demoMerge.adol.long$dem4a%in%c("Methodist","25")
demoMerge.adol.long$rel_lutheran<-demoMerge.adol.long$dem4a%in%c("Lutheran","24")
demoMerge.adol.long$rel_none<-demoMerge.adol.long$dem4a%in%c("none","88")
demoMerge.adol.long$rel_other<-demoMerge.adol.long$dem4a%in%c("98","17","18","29",
                                                              "other","2","14","27",
                                                              "7","28","22","36",
                                                              "Church of Christ",
                                                              "Church of God","34",
                                                              "5","21","37","8",
                                                              "Presbyterian","35",
                                                              "Christian Scientist",
                                                              "20","32","1","16",
                                                              "Pentecostal","19",
                                                              "Mormon (Latter Day Saints)",
                                                              "Nazarene","Assembly of God",
                                                              "Unitarian","6",
                                                              "Seventh Day Adventist",
                                                              "Reformed Church",
                                                              "Full Gospel (Tabernacle)",
                                                              "Congregational",
                                                              "United Brethren",
                                                              "Dutch Reformed Church",
                                                              "23","12","Wesleyan",
                                                              "Disciples of Christ",
                                                              "Church of Brethren",
                                                              "Orthodox (Eastern, greek, Russian, etc.)")

# label missing with NAs
demoMerge.adol.long[is.na(demoMerge.adol.long$dem4a),
                    grep("rel_",colnames(demoMerge.adol.long))]<-NA


demoMerge.adol.long$rel_how<-NA
demoMerge.adol.long[demoMerge.adol.long$dem4c%in%c("1. Not religious at all","not religious at all"),]$rel_how<-1
demoMerge.adol.long[demoMerge.adol.long$dem4c%in%c("2. Not very religious","not very religious"),]$rel_how<-2
demoMerge.adol.long[demoMerge.adol.long$dem4c%in%c("3. Fairly religious","fairly religious"),]$rel_how<-3
demoMerge.adol.long[demoMerge.adol.long$dem4c%in%c("4. Very religious","very religious"),]$rel_how<-4
demoMerge.adol.long[demoMerge.adol.long$dem4c%in%c("Missing","0"),]$rel_how<-NA

demoMerge.adol.long$rel_regatt<-NA
demoMerge.adol.long[demoMerge.adol.long$dem4b%in%c("1. Several times a week","several times a week",
                                  "3. 2-3 times per month","2-3 times a month",
                                  "2. About once a week","about once a week"),]$rel_regatt<-TRUE
demoMerge.adol.long[demoMerge.adol.long$dem4b%in%c("never","5. Never",
                                  "4. Once a month or less","once a month or less"),]$rel_regatt<-FALSE
demoMerge.adol.long[demoMerge.adol.long$dem4b%in%c("Missing",NA),]$rel_how<-NA



### add self-reported religion variables to to master file

G2.master$reli_self_catholic<-NA
G2.master$reli_self_baptist<-NA
G2.master$reli_self_methodist<-NA
G2.master$reli_self_lutheran<-NA
G2.master$reli_self_none<-NA
G2.master$reli_self_other<-NA
G2.master$reli_self_regatt<-NA
G2.master$reli_self_how<-NA


for (s in unique(G2.master$target)){
  tmp<-demoMerge.adol.long[demoMerge.adol.long$target==s,]
  if(length(na.omit(tmp$dem4a))>0){
    G2.master[G2.master$target==s,]$reli_self_catholic<-sum(tmp$rel_catholic,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_baptist<-sum(tmp$rel_baptist,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_methodist<-sum(tmp$rel_methodist,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_lutheran<-sum(tmp$rel_lutheran,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_none<-sum(tmp$rel_none,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_other<-sum(tmp$rel_other,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_regatt<-sum(tmp$rel_regatt,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_how<-mean(tmp$rel_how,na.rm = TRUE)}
}



# school engagement and extracurriculars (dem6, dem7)


demoMerge.adol.long$school_enjoy<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6a%in%c("1","1, never","never"),]$school_enjoy<-1
demoMerge.adol.long[demoMerge.adol.long$dem6a%in%c("2","2, seldom","seldom"),]$school_enjoy<-2
demoMerge.adol.long[demoMerge.adol.long$dem6a%in%c("3","3, sometime","sometimes"),]$school_enjoy<-3
demoMerge.adol.long[demoMerge.adol.long$dem6a%in%c("4","4, often","often"),]$school_enjoy<-4
demoMerge.adol.long[demoMerge.adol.long$dem6a%in%c("5","5, almost always","almost always"),]$school_enjoy<-5


demoMerge.adol.long$school_hate<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6b%in%c("1","1, never","never"),]$school_hate<-1
demoMerge.adol.long[demoMerge.adol.long$dem6b%in%c("2","2, seldom","seldom"),]$school_hate<-2
demoMerge.adol.long[demoMerge.adol.long$dem6b%in%c("3","3, sometime","sometimes"),]$school_hate<-3
demoMerge.adol.long[demoMerge.adol.long$dem6b%in%c("4","4, often","often"),]$school_hate<-4
demoMerge.adol.long[demoMerge.adol.long$dem6b%in%c("5","5, almost always","almost always"),]$school_hate<-5

demoMerge.adol.long$school_dobest<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6c%in%c("1","1, never","never"),]$school_dobest<-1
demoMerge.adol.long[demoMerge.adol.long$dem6c%in%c("2","2, seldom","seldom"),]$school_dobest<-2
demoMerge.adol.long[demoMerge.adol.long$dem6c%in%c("3","3, sometime","sometimes"),]$school_dobest<-3
demoMerge.adol.long[demoMerge.adol.long$dem6c%in%c("4","4, often","often"),]$school_dobest<-4
demoMerge.adol.long[demoMerge.adol.long$dem6c%in%c("5","5, almost always","almost always"),]$school_dobest<-5

demoMerge.adol.long$school_hard<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6d%in%c("1","1, never","never"),]$school_hard<-1
demoMerge.adol.long[demoMerge.adol.long$dem6d%in%c("2","2, seldom","seldom"),]$school_hard<-2
demoMerge.adol.long[demoMerge.adol.long$dem6d%in%c("3","3, sometime","sometimes"),]$school_hard<-3
demoMerge.adol.long[demoMerge.adol.long$dem6d%in%c("4","4, often","often"),]$school_hard<-4
demoMerge.adol.long[demoMerge.adol.long$dem6d%in%c("5","5, almost always","almost always"),]$school_hard<-5

demoMerge.adol.long$school_interesting<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6e%in%c("1","1, never","never"),]$school_interesting<-1
demoMerge.adol.long[demoMerge.adol.long$dem6e%in%c("2","2, seldom","seldom"),]$school_interesting<-2
demoMerge.adol.long[demoMerge.adol.long$dem6e%in%c("3","3, sometime","sometimes"),]$school_interesting<-3
demoMerge.adol.long[demoMerge.adol.long$dem6e%in%c("4","4, often","often"),]$school_interesting<-4
demoMerge.adol.long[demoMerge.adol.long$dem6e%in%c("5","5, almost always","almost always"),]$school_interesting<-5

demoMerge.adol.long$school_failontime<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6f%in%c("1","1, never","never"),]$school_failontime<-1
demoMerge.adol.long[demoMerge.adol.long$dem6f%in%c("2","2, seldom","seldom"),]$school_failontime<-2
demoMerge.adol.long[demoMerge.adol.long$dem6f%in%c("3","3, sometime","sometimes"),]$school_failontime<-3
demoMerge.adol.long[demoMerge.adol.long$dem6f%in%c("4","4, often","often"),]$school_failontime<-4
demoMerge.adol.long[demoMerge.adol.long$dem6f%in%c("5","5, almost always","almost always"),]$school_failontime<-5

demoMerge.adol.long$school_office<-NA
demoMerge.adol.long[demoMerge.adol.long$dem6g%in%c("1","1, never","never"),]$school_office<-1
demoMerge.adol.long[demoMerge.adol.long$dem6g%in%c("2","2, seldom","seldom"),]$school_office<-2
demoMerge.adol.long[demoMerge.adol.long$dem6g%in%c("3","3, sometime","sometimes"),]$school_office<-3
demoMerge.adol.long[demoMerge.adol.long$dem6g%in%c("4","4, often","often"),]$school_office<-4
demoMerge.adol.long[demoMerge.adol.long$dem6g%in%c("5","5, almost always","almost always"),]$school_office<-5

demoMerge.adol.long$part_paper<-NA
demoMerge.adol.long[demoMerge.adol.long$dem7a%in%c("1","1, not at all","not at all"),]$part_paper<-1
demoMerge.adol.long[demoMerge.adol.long$dem7a%in%c("2","2, slight","slight"),]$part_paper<-2
demoMerge.adol.long[demoMerge.adol.long$dem7a%in%c("3","3, moderate","moderate"),]$part_paper<-3
demoMerge.adol.long[demoMerge.adol.long$dem7a%in%c("4","4, considerable","considerable"),]$part_paper<-4
demoMerge.adol.long[demoMerge.adol.long$dem7a%in%c("5","5, great","great"),]$part_paper<-5

demoMerge.adol.long$part_music<-NA
demoMerge.adol.long[demoMerge.adol.long$dem7b%in%c("1","1, not at all","not at all"),]$part_music<-1
demoMerge.adol.long[demoMerge.adol.long$dem7b%in%c("2","2, slight","slight"),]$part_music<-2
demoMerge.adol.long[demoMerge.adol.long$dem7b%in%c("3","3, moderate","moderate"),]$part_music<-3
demoMerge.adol.long[demoMerge.adol.long$dem7b%in%c("4","4, considerable","considerable"),]$part_music<-4
demoMerge.adol.long[demoMerge.adol.long$dem7b%in%c("5","5, great","great"),]$part_music<-5

demoMerge.adol.long$part_ath<-NA
demoMerge.adol.long[demoMerge.adol.long$dem7c%in%c("1","1, not at all","not at all"),]$part_ath<-1
demoMerge.adol.long[demoMerge.adol.long$dem7c%in%c("2","2, slight","slight"),]$part_ath<-2
demoMerge.adol.long[demoMerge.adol.long$dem7c%in%c("3","3, moderate","moderate"),]$part_ath<-3
demoMerge.adol.long[demoMerge.adol.long$dem7c%in%c("4","4, considerable","considerable"),]$part_ath<-4
demoMerge.adol.long[demoMerge.adol.long$dem7c%in%c("5","5, great","great"),]$part_ath<-5

demoMerge.adol.long$part_other<-NA
demoMerge.adol.long[demoMerge.adol.long$dem7d%in%c("1","1, not at all","not at all"),]$part_other<-1
demoMerge.adol.long[demoMerge.adol.long$dem7d%in%c("2","2, slight","slight"),]$part_other<-2
demoMerge.adol.long[demoMerge.adol.long$dem7d%in%c("3","3, moderate","moderate"),]$part_other<-3
demoMerge.adol.long[demoMerge.adol.long$dem7d%in%c("4","4, considerable","considerable"),]$part_other<-4
demoMerge.adol.long[demoMerge.adol.long$dem7d%in%c("5","5, great","great"),]$part_other<-5


demoMerge.adol.long$work<-NA
tmp<-c("5 or less hours","6-10 hours","11-15 hours","16-20 hours","21-35 hours",
       "26-30 hours","more than 30 hours","b. 5 or less hours","c. 6 to 10 hours",
       "d. 11 to 15 hours","e. 16 to 20 hours","f.  21 to 25 hours",
       "g. 26 to 30 hours","g. 26 to 30 hours","h.  More than 30 hours")
demoMerge.adol.long[demoMerge.adol.long$dem8%in%tmp,]$work<-TRUE
demoMerge.adol.long[demoMerge.adol.long$dem8%in%c("never","a. Never"),]$work<-FALSE

  
### add work and school variables to master data frame

cols<-c(colnames(demoMerge.adol.long)[grep("school",colnames(demoMerge.adol.long))],
        colnames(demoMerge.adol.long)[grep("part",colnames(demoMerge.adol.long))],"work")
G2.master[,cols]<-NA


for (s in unique(G2.master$target)){
  tmp<-demoMerge.adol.long[demoMerge.adol.long$target==s,]
  if(length(na.omit(tmp$dem6a))>0){G2.master[G2.master$target==s,]$school_enjoy<-mean(tmp$school_enjoy,na.rm = TRUE)}
  if(length(na.omit(tmp$dem6b))>0){G2.master[G2.master$target==s,]$school_hate<-mean(tmp$school_hate,na.rm = TRUE)}
  if(length(na.omit(tmp$dem6c))>0){G2.master[G2.master$target==s,]$school_dobest<-mean(tmp$school_dobest,na.rm = TRUE)}
  if(length(na.omit(tmp$dem6d))>0){G2.master[G2.master$target==s,]$school_hard<-mean(tmp$school_hard,na.rm = TRUE)}
  if(length(na.omit(tmp$dem6e))>0){G2.master[G2.master$target==s,]$school_interesting<-mean(tmp$school_interesting,na.rm = TRUE)}
  if(length(na.omit(tmp$dem6f))>0){G2.master[G2.master$target==s,]$school_failontime<-mean(tmp$school_failontime,na.rm = TRUE)}
  if(length(na.omit(tmp$dem6g))>0){G2.master[G2.master$target==s,]$school_office<-mean(tmp$school_office,na.rm = TRUE)}
  if(length(na.omit(tmp$dem7a))>0){G2.master[G2.master$target==s,]$part_paper<-mean(tmp$part_paper,na.rm = TRUE)}
  if(length(na.omit(tmp$dem7b))>0){G2.master[G2.master$target==s,]$part_music<-mean(tmp$part_music,na.rm = TRUE)}
  if(length(na.omit(tmp$dem7c))>0){G2.master[G2.master$target==s,]$part_ath<-mean(tmp$part_ath,na.rm = TRUE)}
  if(length(na.omit(tmp$dem7c))>0){G2.master[G2.master$target==s,]$part_other<-mean(tmp$part_other,na.rm = TRUE)}
  if(length(na.omit(tmp$dem8))>0){G2.master[G2.master$target==s,]$work<-sum(tmp$work,na.rm = TRUE)>0}
}




for (s in unique(G2.master$target)){
  tmp<-demoMerge.adol.long[demoMerge.adol.long$target==s,]
  if(length(na.omit(tmp$dem4a))>0){
    G2.master[G2.master$target==s,]$reli_self_catholic<-sum(tmp$rel_catholic,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_baptist<-sum(tmp$rel_baptist,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_methodist<-sum(tmp$rel_methodist,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_lutheran<-sum(tmp$rel_lutheran,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_none<-sum(tmp$rel_none,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_other<-sum(tmp$rel_other,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_regatt<-sum(tmp$rel_regatt,na.rm = TRUE)>0
    G2.master[G2.master$target==s,]$reli_self_how<-mean(tmp$rel_how,na.rm = TRUE)}
}


############################################
### estimated IQ ###########################
############################################

# Stanford-Binet

sb<-read.mls.t("Stanford",1,1)[,c("target","tdate","age","iq")]
sb<-rbind(sb,read.mls.t("Stanford",1,2)[,c("target","tdate","age","iq")])
sb<-rbind(sb,read.mls.t("Stanford",1,3)[,c("target","tdate","age","iq")])
sb[,"tdate"]<-as.Date(as.numeric(sb[,"tdate"])/86400,origin = "1582-10-14")
sb[,"sb.tdate"]<-sb[,"tdate"]
sb$sb.iq<-as.numeric(as.character(sb$iq))
sb<-sb[,c("target","sb.tdate","sb.iq")]

G2.master<-merge(G2.master,sb,all.x = TRUE)

# WISC

wisc3.t2<-read.mls.t("WISC-III G3",2,1)[,c("target","tdate","age","veiq","peiq","fsiq")]
wisc3.t2<-rbind(wisc3.t2,read.mls.t("WISC-III G3",2,2)[,c("target","tdate","age","veiq","peiq","fsiq")])
wisc3.t2<-rbind(wisc3.t2,read.mls.t("WISC-III G2",2,2)[,c("target","tdate","age","veiq","peiq","fsiq")])
wisc3.t2<-rbind(wisc3.t2,read.mls.t("WISC-III G2",2,3)[,c("target","tdate","age","veiq","peiq","fsiq")])

wiscR.t2<-read.mls.t("WISC-R G2",2,1)[,c("target","tdate","age","verbiq","perfiq","fulliq")]
wiscR.t2<-rbind(wiscR.t2,read.mls.t("WISC-R G2",2,2)[,c("target","tdate","age","verbiq","perfiq","fulliq")])


wisc3p.t4<-read.mls.t("WISC-III.+G2",4,1)[,c("target","tdate","age","bdscal","vocscal","symscal","infoscal")]
wisc3p.t4<-rbind(wisc3p.t4,read.mls.t("WISC-III.+G3",4,1)[,c("target","tdate","age","bdscal","vocscal","symscal","infoscal")])
wisc3p.t4<-rbind(wisc3p.t4,read.mls.t("WISC-III.+4 items.+G2",4,2)[,c("target","tdate","age","bdscal","vocscal","symscal","infoscal")])
wisc3p.t4<-rbind(wisc3p.t4,read.mls.t("WISCIII.+sav",4,3)[,c("target","tdate","age","bdscal","vocscal","symscal","infoscal")])
wisc3p.t4[wisc3p.t4$symscal==888 & !is.na(wisc3p.t4$symscal),]$symscal<-NA


wisc3.t4<-read.mls.t("WISC-III G2",4,2)[,c("target","tdate","age","bdscal","vocscal","symscal","infoscal","veiq","peiq","fsiq")]
wisc3.t4<-rbind(wisc3.t4,read.mls.t("WISC-III G2",4,3)[,c("target","tdate","age","bdscal","vocscal",
                                                                    "symscal","infoscal","veiq","peiq","fsiq")])

wiscR.t4<-read.mls.t("WISC-R G2",4,1)[,c("target","tdate","age","verbiq","perfiq","fulliq")]
wiscR.t4<-rbind(wiscR.t4,read.mls.t("WISC-R G2",4,2)[,c("target","tdate","age","verbiq","perfiq","fulliq")])

colnames(wisc3.t2)[-1]<-paste0("wisc3.t2.",colnames(wisc3.t2)[-1])
colnames(wiscR.t2)[-1]<-paste0("wiscR.t2.",colnames(wiscR.t2)[-1])
colnames(wisc3p.t4)[-1]<-paste0("wisc3p.t4.",colnames(wisc3p.t4)[-1])
colnames(wisc3.t4)[-1]<-paste0("wisc3.t4.",colnames(wisc3.t4)[-1])
colnames(wiscR.t4)[-1]<-paste0("wiscR.t4.",colnames(wiscR.t4)[-1])

G2.master<-merge(G2.master,wisc3.t2,all.x = TRUE)
G2.master<-merge(G2.master,wiscR.t2,all.x = TRUE)
G2.master<-merge(G2.master,wisc3p.t4,all.x = TRUE)
G2.master<-merge(G2.master,wisc3.t4,all.x = TRUE)
G2.master<-merge(G2.master,wiscR.t4,all.x = TRUE)


# estimate "short form" iq measure
G2.master$wisc3p.t4.Efsiq<-(rowMeans(G2.master[,c("wisc3p.t4.bdscal","wisc3p.t4.vocscal",
                      "wisc3p.t4.symscal","wisc3p.t4.infoscal")])-10)/3
G2.master$wisc3p.t4.Efsiq<-(G2.master$wisc3p.t4.Efsiq*15)+100

G2.master$wisc3.t4.Efsiq<-(rowMeans(G2.master[,c("wisc3.t4.bdscal","wisc3.t4.vocscal",
                                                  "wisc3.t4.symscal","wisc3.t4.infoscal")])-10)/3
G2.master$wisc3.t4.Efsiq<-(G2.master$wisc3.t4.Efsiq*15)+100

# seems a good estimate judging by the complete scores we have!
plot(G2.master$wisc3.t4.fsiq,G2.master$wisc3.t4.Efsiq)
cor(G2.master$wisc3.t4.fsiq,G2.master$wisc3.t4.Efsiq,use="complete")
#[1] 0.9368979

# how related are all of the disperate iq measures?
iqs<-c("sb.iq","wisc3.t2.fsiq","wiscR.t2.fulliq",
       "wisc3.t4.fsiq","wiscR.t4.fulliq","wisc3p.t4.Efsiq")
cor(G2.master[,iqs],use="pairwise.complete.obs")

plot(G2.master$sb.iq,G2.master$wisc3.t2.fsiq)
plot(G2.master$sb.iq,G2.master$wiscR.t2.fulliq)
plot(G2.master$sb.iq,G2.master$wiscR.t4.fulliq)
plot(G2.master$sb.iq,G2.master$wisc3p.t4.Efsiq)

# ages for each type of administration

G2.master$sb.age<-interval(G2.master$dob, G2.master$sb.tdate) / 
  duration(num = 1, units = "years")

G2.master$wisc3.t2.age<-interval(G2.master$dob, as.Date(G2.master$wisc3.t2.tdate/86400,origin = "1582-10-14")) / 
  duration(num = 1, units = "years")

G2.master$wiscR.t2.age<-interval(G2.master$dob, as.Date(G2.master$wiscR.t2.tdate/86400,origin = "1582-10-14")) / 
  duration(num = 1, units = "years")

G2.master$wisc3p.t4.age<-interval(G2.master$dob, as.Date(G2.master$wisc3p.t4.tdate/86400,origin = "1582-10-14")) / 
  duration(num = 1, units = "years")

G2.master$wisc3.t4.age<-interval(G2.master$dob, as.Date(G2.master$wisc3.t4.tdate/86400,origin = "1582-10-14")) / 
  duration(num = 1, units = "years")

G2.master$wiscR.t4.age<-interval(G2.master$dob, as.Date(G2.master$wiscR.t4.tdate/86400,origin = "1582-10-14")) / 
  duration(num = 1, units = "years")

mean(G2.master$sb.age,na.rm=TRUE)
#[1] 4.479424
mean(G2.master$wisc3.t2.age,na.rm=TRUE)
#[1]  7.622505
mean(G2.master$wiscR.t2.age,na.rm=TRUE)
#[1] 7.536786
mean(G2.master$wisc3p.t4.age,na.rm=TRUE)
#[1] 13.62385
mean(G2.master$wiscR.t4.age,na.rm=TRUE)
#[1] 13.52124

min(G2.master$sb.age,na.rm=TRUE); max(G2.master$sb.age,na.rm=TRUE)
# [1] 2.469541
# [1] 7.129363
min(G2.master$wisc3.t2.age,na.rm=TRUE); max(G2.master$wisc3.t2.age,na.rm=TRUE)
hist(G2.master$wisc3.t2.age,breaks="fd")
# [1] 0
# [1] 18.32991
min(G2.master$wiscR.t2.age,na.rm=TRUE);max(G2.master$wiscR.t2.age,na.rm=TRUE)
hist(G2.master$wiscR.t2.age,breaks="fd")
# [1] 5.853525
# [1] 10.0835
min(G2.master$wisc3p.t4.age,na.rm=TRUE); max(G2.master$wisc3p.t4.age,na.rm=TRUE)
# [1] 11.47159
# [1] 17.53867
min(G2.master$wisc3.t4.age,na.rm=TRUE); max(G2.master$wisc3.t4.age,na.rm=TRUE)
# [1] 12.03833
# [1] 15.19233
min(G2.master$wiscR.t4.age,na.rm=TRUE); max(G2.master$wiscR.t4.age,na.rm=TRUE)
# [1] 11.80014
# [1] 15.72621

# exclude a few cases with test dates that are not in the
# required age range

G2.master[G2.master$wisc3.t2.age>10 & !is.na(G2.master$wisc3.t2.age),
          grep("wisc3.t2",colnames(G2.master))]<-NA
G2.master[G2.master$wisc3.t2.age==0 & !is.na(G2.master$wisc3.t2.age),
          grep("wisc3.t2",colnames(G2.master))]<-NA
G2.master[G2.master$wiscR.t2.age>10 & !is.na(G2.master$wiscR.t2.age),
          grep("wiscR.t2",colnames(G2.master))]<-NA

# try some averages and/or composites for "child IQ" and "adolescent IQ"
# or could have a decent "adolescent IQ" and input most of the early estimates

G2.master$child.estM.IQ<-rowMeans(G2.master[,c("sb.iq",
                                               "wisc3.t2.fsiq",
                                               "wiscR.t2.fulliq")],na.rm=TRUE)
# still not great coverage
mean(is.na(G2.master$child.estM.IQ))
#[1] 0.359045

G2.master$adol.estM.IQ<-rowMeans(G2.master[,c( "wisc3.t4.fsiq" ,
                                               "wisc3p.t4.Efsiq",
                                               "wiscR.t4.fulliq")],na.rm=TRUE)

# a bit better
mean(is.na(G2.master$adol.estM.IQ))
#[1] 0.2506887

cor(G2.master$child.estM.IQ,G2.master$adol.estM.IQ,use="complete")
#[1] 0.7392427

G2.master$overall.estM.IQ<-rowMeans(G2.master[,c("sb.iq",
                                                 "wisc3.t2.fsiq",
                                                 "wiscR.t2.fulliq",
                                              "wisc3.t4.fsiq" ,
                                               "wisc3p.t4.Efsiq",
                                               "wiscR.t4.fulliq")],na.rm=TRUE)

mean(is.na(G2.master$overall.estM.IQ))
#[1] 0.1781451

# also try randomly picking one:

G2.master$child.estR.IQ<-NA
G2.master$adol.estR.IQ<-NA
G2.master$overall.estR.IQ<-NA

for (r in 1:length(G2.master$target)){
  
  if(sum(!is.na(as.numeric(
    G2.master[r,c("sb.iq",
                  "wisc3.t2.fsiq",
                  "wiscR.t2.fulliq")])))>0){
  G2.master$child.estR.IQ[r]<-sample(rep(na.omit(as.numeric(
    G2.master[r,c("sb.iq",
                   "wisc3.t2.fsiq",
                   "wiscR.t2.fulliq")])),2), 1)}
  
  if(sum(!is.na(as.numeric(
    G2.master[r,c("wisc3.t4.fsiq" ,
                  "wisc3p.t4.Efsiq",
                  "wiscR.t4.fulliq")])))>0){
    G2.master$adol.estR.IQ[r]<-sample(rep(na.omit(as.numeric(
      G2.master[r,c("wisc3.t4.fsiq" ,
                    "wisc3p.t4.Efsiq",
                    "wiscR.t4.fulliq")])),2), 1)}
  
  if(sum(!is.na(as.numeric(
    G2.master[r,c("sb.iq",
                  "wisc3.t2.fsiq",
                  "wiscR.t2.fulliq",
                  "wisc3.t4.fsiq" ,
                  "wisc3p.t4.Efsiq",
                  "wiscR.t4.fulliq")])))>0){
    G2.master$overall.estR.IQ[r]<-sample(rep(na.omit(as.numeric(
      G2.master[r,c("sb.iq",
                    "wisc3.t2.fsiq",
                    "wiscR.t2.fulliq",
                    "wisc3.t4.fsiq" ,
                    "wisc3p.t4.Efsiq",
                    "wiscR.t4.fulliq")])),2), 1)}
}

cor(G2.master$child.estR.IQ,G2.master$adol.estR.IQ,use="complete")
#[1] 0.6817284

tapply(G2.master$overall.estM.IQ,G2.master$high.edu,mean,na.rm=TRUE)
# < High School     Bachelors   High School   Post. Grad.  Some College 
# 98.31944     108.89988      96.09449     113.14426     101.34427 

tapply(G2.master$overall.estR.IQ,G2.master$high.edu,mean,na.rm=TRUE)
# < High School     Bachelors   High School   Post. Grad.  Some College 
# 98.88889     109.18159      96.14509     113.29245     101.51301 



################################
### symbol-digit modalities ####
################################

#read in

sdmt.t3<-read.mls.t("Symbol Digit G2",3,1)[,c("target","tdate","age","handscr","oralscr")]
sdmt.t3<-rbind(sdmt.t3,read.mls.t("Symbol Digit G2",3,2)[,c("target","tdate","age","handscr","oralscr")])
sdmt.t3<-rbind(sdmt.t3,read.mls.t("Symbol Digit G2",3,3)[,c("target","tdate","age","handscr","oralscr")])
sdmt.t3$wave<-"t3"
sdmt.t3[sdmt.t3$age<0 & !is.na(sdmt.t3$age),]$age<-NA # one person with a negative age?

sdmt.t4<-read.mls.t("Symbol Digit G2",4,1)[,c("target","tdate","age","handscr","oralscr")]
sdmt.t4[,c("handscr","oralscr")]<-apply(sdmt.t4[,c("handscr","oralscr")],2,as.numeric) # was factor for some reason?
sdmt.t4<-rbind(sdmt.t4,read.mls.t("Symbol Digit G2",4,2)[,c("target","tdate","age","handscr","oralscr")])
sdmt.t4<-rbind(sdmt.t4,read.mls.t("Symbol Digit G2",4,3)[,c("target","tdate","age","handscr","oralscr")])
sdmt.t4$wave<-"t4"

sdmt.all<-rbind(sdmt.t3,sdmt.t4)

sdmt_G2<-sdmt.all[sdmt.all$target%in%G2.master$target,]

#make sure age is correct by comparing to master DOB
sdmt_G2$sdmt.age<-NA
for (t in unique(sdmt_G2$target)){
  sdmt_G2[sdmt_G2$target==t,]$sdmt.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                   as.Date(sdmt_G2[sdmt_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
                                                    duration(num = 1, units = "years")
}

sdmt_G2$age_sq<-sdmt_G2$sdmt.age^2
sdmt_G2$oralscr<-as.numeric(sdmt_G2$oralscr)
sdmt_G2$handscr<-as.numeric(sdmt_G2$handscr)

plot(sdmt_G2$sdmt.age,sdmt_G2$oralscr)
points(sdmt.t3$age,sdmt.t3$oralscr,col="red")
points(sdmt.t4$age,sdmt.t4$oralscr,col="blue")
abline(lm(sdmt_G2$oralscr~sdmt_G2$sdmt.age))

plot(sdmt_G2$sdmt.age,sdmt_G2$handscr)
points(sdmt.t3$age,sdmt.t3$handscr,col="red")
points(sdmt.t4$age,sdmt.t4$handscr,col="blue")
abline(lm(sdmt_G2$handscr~sdmt_G2$sdmt.age))

summary(lm(handscr~sdmt.age,data = sdmt_G2))
summary(lm(handscr~sdmt.age+age_sq,data = sdmt_G2))

summary(lm(oralscr~sdmt.age,data = sdmt_G2))
summary(lm(oralscr~sdmt.age+age_sq,data = sdmt_G2))

sdmt_G2$hand_res<-c(scale(resid(lm(handscr~sdmt.age+age_sq,data = sdmt_G2,na.action=na.exclude))))
sdmt_G2$oral_res<-c(scale(resid(lm(oralscr~sdmt.age+age_sq,data = sdmt_G2,na.action=na.exclude))))

sdmt.cols<-c("target","sdmt.age","handscr","oralscr","hand_res","oral_res")

G2.master<-merge(G2.master,sdmt_G2[sdmt_G2$wave=="t3",sdmt.cols],all.x = TRUE)
colnames(G2.master)[colnames(G2.master)%in%sdmt.cols[-1]]<-paste0(sdmt.cols[-1],".t3")

G2.master<-merge(G2.master,sdmt_G2[sdmt_G2$wave=="t4",sdmt.cols],all.x = TRUE)
colnames(G2.master)[colnames(G2.master)%in%sdmt.cols[-1]]<-paste0(sdmt.cols[-1],".t4")

G2.master$handscr.comp<-rowMeans(G2.master[,c("handscr.t3","handscr.t4")],na.rm = TRUE)
G2.master$oralscr.comp<-rowMeans(G2.master[,c("oralscr.t3","oralscr.t4")],na.rm = TRUE)
G2.master$hand_res.comp<-rowMeans(G2.master[,c("hand_res.t3","hand_res.t4")],na.rm = TRUE)
G2.master$oral_res.comp<-rowMeans(G2.master[,c("oral_res.t3","oral_res.t4")],na.rm = TRUE)

################################
### trail making ###############
################################

tmt.t4<-read.mls.t("Trail Making Test G2",4,1)[,c("target","tdate","age","timea","timeb")]
tmt.t4<-rbind(tmt.t4,read.mls.t("Trail Making Test G2",4,2)[,c("target","tdate","age","timea","timeb")])
tmt.t4<-rbind(tmt.t4,read.mls.t("Trail Making Test G2",4,3)[,c("target","tdate","age","timea","timeb")])
tmt.t4$wave<-"t4"

tmt.t5<-read.mls.t("Trail Making Test G2",5,1)[,c("target","tdate","age","timea","timeb")]
tmt.t5<-rbind(tmt.t5,read.mls.t("Trail Making Test G2",5,2)[,c("target","tdate","age","timea","timeb")])
tmt.t5<-rbind(tmt.t5,read.mls.t("Trail Making Test G2",5,3)[,c("target","tdate","age","timea","timeb")])
tmt.t5$wave<-"t5"

tmt.t6<-read.mls.t("Trail Making Test G2 T",6,1)[,c("target","tdate","age","timea","timeb")]
tmt.t6<-rbind(tmt.t6,read.mls.t("Trail Making Test G2",6,2)[,c("target","tdate","age","timea","timeb")])
tmt.t6<-rbind(tmt.t6,read.mls.t("Trail Making Test G2",6,3)[,c("target","tdate","age","timea","timeb")])
tmt.t6$wave<-"t6"

tmt.t7<-read.mls.t("Trail Making Test G2 T",7,1)[,c("target","tdate","age","timea","timeb")]
tmt.t7<-rbind(tmt.t7,read.mls.t("Trail Making Test G2 II",7,2)[,c("target","tdate","age","timea","timeb")])
tmt.t7<-rbind(tmt.t7,read.mls.t("Trail Making Test G2",7,3)[,c("target","tdate","age","timea","timeb")])
tmt.t7$wave<-"t7"

tmt_G2<-rbind(tmt.t4,tmt.t5)
tmt_G2<-rbind(tmt_G2,tmt.t6)
tmt_G2<-rbind(tmt_G2,tmt.t7)

tmt_G2<-tmt_G2[tmt_G2$target%in%G2.master$target,]

# log transform scores
tmt_G2$log_timea<-log(tmt_G2$timea)
tmt_G2$log_timeb<-log(tmt_G2$timeb)


#make sure age is correct by comparing to master DOB
tmt_G2$tmt.age<-NA
for (t in unique(tmt_G2$target)){
  tmt_G2[tmt_G2$target==t,]$tmt.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                 as.Date(tmt_G2[tmt_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
}

tmtc_G2<-tmt_G2[tmt_G2$wave=="t4" & tmt_G2$tmt.age<20 & !is.na(tmt_G2$tmt.age),]
tmt_G2<-tmt_G2[tmt_G2$wave!="t4" & tmt_G2$tmt.age<20 & !is.na(tmt_G2$tmt.age),]

#trim two implausible times for timeb (a few seconds)
tmt_G2<-tmt_G2[tmt_G2$timeb>2 | is.na(tmt_G2$timeb),]

# remove duplicates from the first wave:
dup<-names(table(tmtc_G2$target))[table(tmtc_G2$target)>1]
tmtc_G2<-tmtc_G2[!tmtc_G2$target%in%dup,]

plot(tmt_G2$tmt.age,tmt_G2$timea)
points(tmt.t5$age,tmt.t5$timea,col="blue")
points(tmt.t6$age,tmt.t6$timea,col="red")
points(tmt.t7$age,tmt.t7$timea,col="green")

plot(tmtc_G2$tmt.age,tmtc_G2$log_timea)
plot(tmt_G2$tmt.age,tmt_G2$log_timea)
plot(tmtc_G2$tmt.age,tmtc_G2$log_timeb)
plot(tmt_G2$tmt.age,tmt_G2$log_timeb)


tmtc_G2$age_sq<-tmtc_G2$tmt.age^2
tmt_G2$age_sq<-tmt_G2$tmt.age^2

# more subtle age effects
summary(lm(log_timea~tmt.age+age_sq,data = tmtc_G2))
summary(lm(log_timeb~tmt.age+age_sq,data = tmtc_G2))
summary(lm(log_timea~tmt.age+age_sq,data = tmt_G2))
summary(lm(log_timeb~tmt.age+age_sq,data = tmt_G2))

tmtc_G2$timea_res<-c(scale(resid(lm(log_timea~tmt.age+age_sq,data = tmtc_G2,na.action=na.exclude))))
tmtc_G2$timeb_res<-c(scale(resid(lm(log_timeb~tmt.age+age_sq,data = tmtc_G2,na.action=na.exclude))))
tmt_G2$timea_res<-c(scale(resid(lm(log_timea~tmt.age+age_sq,data = tmt_G2,na.action=na.exclude))))
tmt_G2$timeb_res<-c(scale(resid(lm(log_timeb~tmt.age+age_sq,data = tmt_G2,na.action=na.exclude))))


G2.master$tmtc_timea<-NA
G2.master$tmtc_timeb<-NA
G2.master$tmt_timea_M<-NA
G2.master$tmt_timeb_M<-NA


for (s in unique(G2.master$target)){
  if(length(tmtc_G2[tmtc_G2$target==s,]$timea)>0){
  G2.master[G2.master$target==s,]$tmtc_timea<-tmtc_G2[tmtc_G2$target==s,]$timea_res
  G2.master[G2.master$target==s,]$tmtc_timeb<-tmtc_G2[tmtc_G2$target==s,]$timeb_res}
  if(length(tmt_G2[tmt_G2$target==s,]$timea)>0){
    tmp<-tmt_G2[tmt_G2$target==s,]
    G2.master[G2.master$target==s,]$tmt_timea_M<-mean(tmp$timea_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$tmt_timeb_M<-mean(tmp$timeb_res,na.rm=TRUE)}
}

################################
### stroop ###############
################################

stroop.t4<-read.mls.t("Stroop G2",4,1)[,c("target","tdate","age","wordread","colornam","colorwor")]
stroop.t4<-rbind(stroop.t4,read.mls.t("Stroop G2",4,2)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t4<-rbind(stroop.t4,read.mls.t("Stroop G2",4,3)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t4$wave<-"t4"

stroop.t5<-read.mls.t("Stroop G2",5,1)[,c("target","tdate","age","wordread","colornam","colorwor")]
stroop.t5<-rbind(stroop.t5,read.mls.t("Stroop G2",5,2)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t5<-rbind(stroop.t5,read.mls.t("Stroop G2",5,3)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t5$wave<-"t5"

stroop.t6<-read.mls.t("Stroop G2 T",6,1)[,c("target","tdate","age","wordread","colornam","colorwor")]
stroop.t6<-rbind(stroop.t6,read.mls.t("Stroop G2 I",6,2)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t6<-rbind(stroop.t6,read.mls.t("Stroop G2",6,3)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t6$wave<-"t6"

stroop.t7<-read.mls.t("Stroop G2 T",7,1)[,c("target","tdate","age","wordread","colornam","colorwor")]
stroop.t7<-rbind(stroop.t7,read.mls.t("Stroop G2 II",7,2)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t7<-rbind(stroop.t7,read.mls.t("Stroop C",7,3)[,c("target","tdate","age","wordread","colornam","colorwor")])
stroop.t7$wave<-"t7"

stroop_G2<-rbind(stroop.t4,stroop.t5)
stroop_G2<-rbind(stroop_G2,stroop.t6)
stroop_G2<-rbind(stroop_G2,stroop.t7)


stroop_G2<-stroop_G2[stroop_G2$target%in%G2.master$target,]

#make sure age is correct by comparing to master DOB
stroop_G2$stroop.age<-NA
for (t in unique(stroop_G2$target)){
  stroop_G2[stroop_G2$target==t,]$stroop.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                              as.Date(stroop_G2[stroop_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
}

stroop_G2<-stroop_G2[ stroop_G2$stroop.age<20 & !is.na(stroop_G2$stroop.age),]


plot(stroop_G2$age,stroop_G2$colornam)
points(stroop.t4$age,stroop.t4$colornam,col="purple")
points(stroop.t5$age,stroop.t5$colornam,col="blue")
points(stroop.t6$age,stroop.t6$colornam,col="red")
points(stroop.t7$age,stroop.t7$colornam,col="green")

stroop_G2$age_sq<-stroop_G2$stroop.age^2

stroop_G2$wordread_res<-c(scale(resid(lm(wordread~stroop.age+age_sq,data = stroop_G2,na.action=na.exclude))))
stroop_G2$colornam_res<-c(scale(resid(lm(colornam~stroop.age+age_sq,data = stroop_G2,na.action=na.exclude))))
stroop_G2$colorwor_res<-c(scale(resid(lm(colorwor~stroop.age+age_sq,data = stroop_G2,na.action=na.exclude))))


G2.master$stroop_wordread_M<-NA
G2.master$stroop_colornam_M<-NA
G2.master$stroop_colorwor_M<-NA


for (s in unique(G2.master$target)){
  if(length(stroop_G2[stroop_G2$target==s,]$target)>0){
    tmp<-stroop_G2[stroop_G2$target==s,]
    G2.master[G2.master$target==s,]$stroop_wordread_M<-mean(tmp$wordread_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$stroop_colornam_M<-mean(tmp$colornam_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$stroop_colorwor_M<-mean(tmp$colorwor_res,na.rm=TRUE)}
}


####################################
### DDM parameters #################
####################################

sst<-read.csv("sst_DDM_old_task.csv")
sst<-rbind(sst,read.csv("sst_DDM_new_task.csv"))
sst$target<-sst$Target


sst$tdate<-as.Date(sst$Date,format = "%m/%d/%Y")

sst_G2<-sst[sst$target%in%G2.master$target,]

# add ages 

sst_G2$sst.age<-NA
for (t in unique(sst_G2$target)){
  sst_G2[sst_G2$target==t,]$sst.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                 sst_G2[sst_G2$target==t,]$tdate)/duration(num = 1, units = "years")
}

sst_G2<-sst_G2[sst_G2$sst.age<20,]

# lots of variability in how many people have, but ~2 on average
table(sst_G2$Target)
mean(table(sst_G2$Target))

sst_G2$age_sq<-sst_G2$sst.age^2


# clear age effects of age on v
summary(lm(v~sst.age+age_sq+version,data = sst_G2))
summary(lm(a~sst.age+age_sq+version,data = sst_G2))
summary(lm(Ter~sst.age+age_sq+version,data = sst_G2))
summary(lm(gf~sst.age+age_sq+version,data = sst_G2))


sst_G2$v_res<-c(scale(resid(lm(v~sst.age+age_sq+version,data = sst_G2,na.action=na.exclude))))
sst_G2$a_res<-c(scale(resid(lm(a~sst.age+age_sq+version,data = sst_G2,na.action=na.exclude))))
sst_G2$Ter_res<-c(scale(resid(lm(Ter~sst.age+age_sq+version,data = sst_G2,na.action=na.exclude))))
sst_G2$gf_res<-c(scale(resid(lm(gf~sst.age+age_sq+version,data = sst_G2,na.action=na.exclude))))


G2.master$v_res<-NA
G2.master$a_res<-NA
G2.master$Ter_res<-NA
G2.master$gf_res<-NA


for (s in unique(G2.master$target)){
  if(length(sst_G2[sst_G2$target==s,]$target)>0){
    tmp<-sst_G2[sst_G2$target==s,]
    G2.master[G2.master$target==s,]$v_res<-mean(tmp$v_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$a_res<-mean(tmp$a_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$Ter_res<-mean(tmp$Ter_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$gf_res<-mean(tmp$gf_res,na.rm=TRUE)
    }
}

mean(!is.na(G2.master$v_res))


#########################
### Q-sort ##############
#########################

Eisen.c<-c("resl","nege","reacont")

qsort.t1<-read.mls.t("Cal_Qsort G2 Eisenberg",1,1,scored = TRUE)[,c("target","tdate","age",Eisen.c)]
qsort.t1<-rbind(qsort.t1,read.mls.t("Cal_Qsort G2 Eisenberg",1,2,scored = TRUE)[,c("target","tdate","age",Eisen.c)])
tmp<-read.mls.t("Cal_Qsort G2 Eisenberg",1,3,scored = TRUE)[,c("target","tdate","age","resil","negem","rxcont")]
colnames(tmp)<-c("target","tdate","age",Eisen.c)
qsort.t1<-rbind(qsort.t1,tmp)
qsort.t1$wave<-"t1"

qsort.t2<-read.mls.t("Cal_Qsort G2 Eisenberg",2,1,scored = TRUE)[,c("target","tdate","age",Eisen.c)]
qsort.t2<-rbind(qsort.t2,read.mls.t("Cal_Qsort G2 Eisenberg",2,2,scored = TRUE)[,c("target","tdate","age",Eisen.c)])
tmp<-read.mls.t("Cal_Qsort G2 Eisenberg",2,3,scored = TRUE)[,c("target","tdate","age","resil","negem","rxcont")]
colnames(tmp)<-c("target","tdate","age",Eisen.c)
qsort.t2<-rbind(qsort.t2,tmp)
qsort.t2$wave<-"t2"

qsort.t3<-read.mls.t("Cal_Qsort G2 Eisenberg",3,1,scored = TRUE)[,c("target","tdate","age",Eisen.c)]
qsort.t3$wave<-"t3"

qsort.t4<-read.mls.t("Cal_Qsort G2 Eisenberg",4,1,scored = TRUE)[,c("target","tdate","age",Eisen.c)]
qsort.t4$wave<-"t4"

qsort_G2<-rbind(qsort.t1,qsort.t2)
qsort_G2<-rbind(qsort_G2,qsort.t3)
qsort_G2<-rbind(qsort_G2,qsort.t4)

# add ages 

qsort_G2$qsort.age<-NA
for (t in unique(qsort_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  qsort_G2[qsort_G2$target==t,]$qsort.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                  as.Date(qsort_G2[qsort_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
                                                  duration(num = 1, units = "years")
 }
}

qsort_G2<-qsort_G2[qsort_G2$qsort.age<20 & !is.na(qsort_G2$qsort.age),]

# lots of variability in how many people have, but ~2.5 on average
table(qsort_G2$target)
mean(table(qsort_G2$target))

qsort_G2$age_sq<-qsort_G2$qsort.age^2

# no substantial effects of age other than for reactive control
summary(lm(resl~qsort.age+age_sq,data = qsort_G2))
summary(lm(nege~qsort.age+age_sq,data = qsort_G2))
summary(lm(reacont~qsort.age+age_sq,data = qsort_G2))

plot(qsort_G2$qsort.age,qsort_G2$reacont)

qsort_G2$resl_res<-c(scale(resid(lm(resl~qsort.age+age_sq,data = qsort_G2,na.action=na.exclude))))
qsort_G2$nege_res<-c(scale(resid(lm(nege~qsort.age+age_sq,data = qsort_G2,na.action=na.exclude))))
qsort_G2$reacont_res<-c(scale(resid(lm(reacont~qsort.age+age_sq,data = qsort_G2,na.action=na.exclude))))

G2.master$q_resl<-NA
G2.master$q_nege<-NA
G2.master$q_reacont<-NA

for (s in unique(G2.master$target)){
  if(length(qsort_G2[qsort_G2$target==s,]$target)>0){
    tmp<-qsort_G2[qsort_G2$target==s,]
    G2.master[G2.master$target==s,]$q_resl<-mean(tmp$resl_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$q_nege<-mean(tmp$nege_res,na.rm=TRUE)
    G2.master[G2.master$target==s,]$q_reacont<-mean(tmp$reacont_res,na.rm=TRUE)
  }
}


cor(G2.master[,c("q_resl","q_nege","q_reacont")],use="pairwise.complete")


#########################
### DOTS ################
#########################

#### parent on child

dotsr.c<-c("activ", "sleep", "appro", "flex", "mood", "rhymslp","rhymeat","rhymhab","task","distrac","persist")

dotsR.PonC.t1<-read.mls.t("DOTS-R G1 on G2",1,1,scored = TRUE)[,c("target","tdate","age",dotsr.c)]
dotsR.PonC.t1<-rbind(dotsR.PonC.t1,read.mls.t("DOTS-R G1 on G2",1,2,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t1<-rbind(dotsR.PonC.t1,read.mls.t("DOTS-R G1 on G2",1,3,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t1$wave<-"t1"

dotsR.PonC.t2<-read.mls.t("DOTS-R G1 on G2",2,1,scored = TRUE)[,c("target","tdate","age",dotsr.c)]
dotsR.PonC.t2<-rbind(dotsR.PonC.t2,read.mls.t("DOTS-R G1 on G2",2,2,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t2<-rbind(dotsR.PonC.t2,read.mls.t("DOTS-R G1 on G2",2,3,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t2$wave<-"t2"

dotsR.PonC.t3<-read.mls.t("DOTS-R G1 on G2",3,1,scored = TRUE)[,c("target","tdate","age",dotsr.c)]
dotsR.PonC.t3<-rbind(dotsR.PonC.t3,read.mls.t("DOTS-R G1 on G2",3,2,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t3<-rbind(dotsR.PonC.t3,read.mls.t("DOTS-R G1 on G2",3,3,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t3$wave<-"t3"

dotsR.PonC.t5<-read.mls.t("DOTS-R G1 on G2",5,1,scored = TRUE)[,c("target","tdate","age",dotsr.c)]
dotsR.PonC.t5<-rbind(dotsR.PonC.t5,read.mls.t("DOTS-R G1 on G2",5,2,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t5<-rbind(dotsR.PonC.t5,read.mls.t("DOTS-R G1 on G2",5,3,scored = TRUE)[,c("target","tdate","age",dotsr.c)])
dotsR.PonC.t5$wave<-"t5"


dotsR_PonC_G2<-rbind(dotsR.PonC.t1,dotsR.PonC.t2)
dotsR_PonC_G2<-rbind(dotsR_PonC_G2,dotsR.PonC.t3)
dotsR_PonC_G2<-rbind(dotsR_PonC_G2,dotsR.PonC.t5)

# add ages 

dotsR_PonC_G2$dotsPonC.age<-NA
for (t in unique(dotsR_PonC_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  dotsR_PonC_G2[dotsR_PonC_G2$target==t,]$dotsPonC.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                         as.Date(dotsR_PonC_G2[dotsR_PonC_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}


# some apparent age-related trends
plot(dotsR_PonC_G2$dotsPonC.age,dotsR_PonC_G2$distrac)
plot(dotsR_PonC_G2$dotsPonC.age,dotsR_PonC_G2$flex)
plot(dotsR_PonC_G2$dotsPonC.age,dotsR_PonC_G2$mood)
plot(dotsR_PonC_G2$dotsPonC.age,dotsR_PonC_G2$persist)

cor(dotsR_PonC_G2[,dotsr.c],use="pairwise.complete")

# create residuals and average

dotsR_PonC_G2$age_sq<-dotsR_PonC_G2$dotsPonC.age^2

for (c in dotsr.c){ 
  dotsR_PonC_G2[,paste0(c,"_res")]<-c(scale(resid(lm(paste0(c,"~dotsPonC.age+age_sq"),
                                                     data = dotsR_PonC_G2,na.action=na.exclude))))
}

G2.master[,dotsr.c]<-NA

for (s in unique(G2.master$target)){
  if(length(dotsR_PonC_G2[dotsR_PonC_G2$target==s,]$target)>0){
    tmp<-dotsR_PonC_G2[dotsR_PonC_G2$target==s,]
    for (c in dotsr.c){
      G2.master[G2.master$target==s,c]<-mean(tmp[,paste0(c,"_res")],na.rm=TRUE)}
  }
}


#### child on self

dotsrC.c<-paste0("dotsr",1:9)
names(dotsrC.c)<-c("activ", "sleep", "appro", "flex", "mood", "rhymslp","rhymeat","rhymhab","task")

dotsR.ConC.t3<-read.mls.t("DOTS-R G2 Self",3,1,scored = TRUE)[,c("target","tdate","age",dotsrC.c)]
dotsR.ConC.t3<-rbind(dotsR.ConC.t3,read.mls.t("DOTS-R G2 Self",3,2,scored = TRUE)[,c("target","tdate","age",dotsrC.c)])
dotsR.ConC.t3<-rbind(dotsR.ConC.t3,read.mls.t("DOTS-R G2 Self",3,3,scored = TRUE)[,c("target","tdate","age",dotsrC.c)])
dotsR.ConC.t3$wave<-"t3"

dotsR.ConC.t5<-read.mls.t("DOTS-R G2 Self",5,1,scored = TRUE)[,c("target","tdate","age",dotsrC.c)]
dotsR.ConC.t5<-rbind(dotsR.ConC.t5,read.mls.t("DOTS-R G2 Self",5,2,scored = TRUE)[,c("target","tdate","age",dotsrC.c)])
dotsR.ConC.t5<-rbind(dotsR.ConC.t5,read.mls.t("DOTS-R G2 Self",5,3,scored = TRUE)[,c("target","tdate","age",dotsrC.c)])
dotsR.ConC.t5$wave<-"t5"


dotsR_ConC_G2<-rbind(dotsR.ConC.t3,dotsR.ConC.t5)
colnames(dotsR_ConC_G2)<-c("target","tdate","age",names(dotsrC.c),"wave")

# get ages

dotsR_ConC_G2$dotsR_ConC.age<-NA
for (t in unique(dotsR_ConC_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  dotsR_ConC_G2[dotsR_ConC_G2$target==t,]$dotsR_ConC.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                                      as.Date(dotsR_ConC_G2[dotsR_ConC_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

# a few apparent age-related trends
plot(dotsR_ConC_G2$dotsR_ConC.age,dotsR_ConC_G2$activ)
plot(dotsR_ConC_G2$dotsR_ConC.age,dotsR_ConC_G2$flex)
plot(dotsR_ConC_G2$dotsR_ConC.age,dotsR_ConC_G2$mood)

# all under 20, everyone has one or two
max(dotsR_ConC_G2$dotsR_ConC.age,na.rm=TRUE)
table(table(dotsR_ConC_G2$target))

# create residuals and average

dotsR_ConC_G2$age_sq<-dotsR_ConC_G2$dotsR_ConC.age^2

for (c in names(dotsrC.c)){ 
  dotsR_ConC_G2[,paste0(c,"_res")]<-c(scale(resid(lm(paste0(c,"~dotsR_ConC.age+age_sq"),
                                                     data = dotsR_ConC_G2,na.action=na.exclude))))
}

G2.master[,paste0("self_",names(dotsrC.c))]<-NA

for (s in unique(G2.master$target)){
  if(length(dotsR_ConC_G2[dotsR_ConC_G2$target==s,]$target)>0){
    tmp<-dotsR_ConC_G2[dotsR_ConC_G2$target==s,]
    for (c in names(dotsrC.c)){
      G2.master[G2.master$target==s,paste0("self_",c)]<-mean(tmp[,paste0(c,"_res")],na.rm=TRUE)}
  }
}

########################################
##### NEO ##############################
########################################

big5<-c("neurotys","extravys","openys","agreeys","consys")

neoFFI.t5<-read.mls.t("NEO G2",5,1,scored = TRUE)[,c("target","tdate","age",big5)]
neoFFI.t5<-rbind(neoFFI.t5,read.mls.t("NEO G2",5,2,scored = TRUE)[,c("target","tdate","age",big5)])
neoFFI.t5<-rbind(neoFFI.t5,read.mls.t("NEO G2",5,3,scored = TRUE)[,c("target","tdate","age",big5)])
neoFFI.t5$wave<-"t5"

neoPIR.t5<-read.mls.t("NEO-PI-R G2",5,1,scored = TRUE) [,c("target","tdate","age",big5)]
neoPIR.t5<-rbind(neoPIR.t5,read.mls.t("NEO-PI-R G2",5,2,scored = TRUE)[,c("target","tdate","age",big5)])
neoPIR.t5<-rbind(neoPIR.t5,read.mls.t("NEO-PI-R G2",5,3,scored = TRUE)[,c("target","tdate","age",big5)])
neoPIR.t5$wave<-"t5"

neoPIR.t6<-read.mls.t("NEO-PI-R G2 T",6,1,scored = TRUE) [,c("target","tdate","age",big5)]
neoPIR.t6<-rbind(neoPIR.t6,read.mls.t("NEO-PI-R G2 II",6,2,scored = TRUE)[,c("target","tdate","age",big5)])
neoPIR.t6<-rbind(neoPIR.t6,read.mls.t("NEO-PI-R G2 III",6,3,scored = TRUE)[,c("target","tdate","age",big5)])
neoPIR.t6$wave<-"t6"

neoFFI.t7<-read.mls.t("NEO G2 T7.",7,1,scored = TRUE)[,c("target","tdate","age",big5)]
neoFFI.t7<-rbind(neoFFI.t7,read.mls.t("NEO G2 II T7.",7,2,scored = TRUE)[,c("target","tdate","age",big5)])
neoFFI.t7<-rbind(neoFFI.t7,read.mls.t("NEO G2 III T7.",7,3,scored = TRUE)[,c("target","tdate","age",big5)])
neoFFI.t7$wave<-"t7"

neoPIR.t7<-read.mls.t("NEO-PI-R G2 T",7,1,scored = TRUE) [,c("target","tdate","age",big5)]
neoPIR.t7<-rbind(neoPIR.t7,read.mls.t("NEO-PI-R G2 II",6,2,scored = TRUE)[,c("target","tdate","age",big5)])
neoPIR.t7$wave<-"t7"

neoFFI_G2<-rbind(neoFFI.t5,neoFFI.t7)
neoFFI_G2$version<-"ffi"

neoPIR_G2<-rbind(neoPIR.t5,neoPIR.t6)
neoPIR_G2<-rbind(neoPIR_G2,neoPIR.t7)
neoPIR_G2$version<-"pir"

neo_G2<-rbind(neoFFI_G2,neoPIR_G2)

# get ages

neo_G2$neo.age<-NA
for (t in unique(neo_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  neo_G2[neo_G2$target==t,]$neo.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                                        as.Date(neo_G2[neo_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

plot(neo_G2[neo_G2$version=="ffi",]$neo.age,neo_G2[neo_G2$version=="ffi",]$consys)
points(neo_G2[neo_G2$version=="pir",]$neo.age,neo_G2[neo_G2$version=="pir",]$consys,col="red")
cor(neo_G2$neo.age,neo_G2$consys,use="complete")

plot(neo_G2[neo_G2$version=="ffi",]$neo.age,neo_G2[neo_G2$version=="ffi",]$extravys)
points(neo_G2[neo_G2$version=="pir",]$neo.age,neo_G2[neo_G2$version=="pir",]$extravys,col="red")
cor(neo_G2$neo.age,neo_G2$consys,use="complete")


neo_G2<-neo_G2[neo_G2$neo.age<20 & !is.na(neo_G2$neo.age),]

# create residuals and average

neo_G2$age_sq<-neo_G2$neo.age^2

# pretty weak age effects, just a couple effects of version
summary(lm(neurotys~neo.age+age_sq+version,data=neo_G2))
summary(lm(consys~neo.age+age_sq+version,data=neo_G2))
summary(lm(extravys~neo.age+age_sq+version,data=neo_G2))
summary(lm(openys~neo.age+age_sq+version,data=neo_G2))
summary(lm(agreeys~neo.age+age_sq+version,data=neo_G2))

for (c in big5){ 
  neo_G2[,paste0(c,"_res")]<-c(scale(resid(lm(paste0(c,"~neo.age+age_sq+version"),
                                                     data = neo_G2,na.action=na.exclude))))
}

G2.master[,big5]<-NA

for (s in unique(G2.master$target)){
  if(length(neo_G2[neo_G2$target==s,]$target)>0){
    tmp<-neo_G2[neo_G2$target==s,]
    for (c in big5){
      G2.master[G2.master$target==s,c]<-mean(tmp[,paste0(c,"_res")],na.rm=TRUE)}
  }
}


##########################################
##### delay of gratification #############
##########################################

DoG.t1<-read.mls.t("Delay",1,1)[,c("target","tdate","age","totw")]
DoG.t1<-rbind(DoG.t1,read.mls.t("Delay",1,2)[,c("target","tdate","age","totw")])
DoG.t1<-rbind(DoG.t1,read.mls.t("Delay",1,3)[,c("target","tdate","age","totw")])
DoG.t1$wave<-"t1"

DoG.t2<-read.mls.t("Delay",2,1)[,c("target","tdate","age","totw")]
DoG.t2<-rbind(DoG.t2,read.mls.t("Delay",2,2)[,c("target","tdate","age","totw")])
#none for sample 3
DoG.t2$wave<-"t2"

DoG_G2<-rbind(DoG.t1,DoG.t2)

# add ages 

DoG_G2$DoG.age<-NA
for (t in unique(DoG_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  DoG_G2[DoG_G2$target==t,]$DoG.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                                      as.Date(DoG_G2[DoG_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

#clear age effects
plot(DoG_G2$age,DoG_G2$totw)

# some people have 2
table(table(DoG_G2$target))

# residuals, flip for interpretability

DoG_G2$age_sq<-DoG_G2$DoG.age^2
DoG_G2$DoG<-c(scale(resid(lm(totw~DoG.age+age_sq,data = DoG_G2,na.action=na.exclude))))*-1

G2.master$DoG<-NA

for (s in unique(G2.master$target)){
  if(length(DoG_G2[DoG_G2$target==s,]$target)>0){
    tmp<-DoG_G2[DoG_G2$target==s,]
    G2.master[G2.master$target==s,]$DoG<-mean(tmp$DoG)
  }
}

# coverage is not great, but can be helped with imputation
mean(!is.na(G2.master$DoG))

############################################
#### CBCL (not annual like YSR) ############
############################################,

cbcl.rcols<-c("wth","som","anx","soc" ,"tho","att","del","agg","intrr","extrr","total")
cbcl.tcols<-c("twthdrw","tsomati","tanxdep","tsocpro" ,"tthotpr","tattent","tdelinq","taggres",
              "inttscr","exttscr","tottscr")

cbcl.t1<-read.mls.t("Achenbach Parent",1,1,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)]
cbcl.t1<-rbind(cbcl.t1,read.mls.t("Achenbach Parent",1,2,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])
cbcl.t1<-rbind(cbcl.t1,read.mls.t("Achenbach Parent",1,3,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])

cbcl.t2<-read.mls.t("Achenbach Parent Report G2",2,1,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)]
cbcl.t2<-rbind(cbcl.t2,read.mls.t("Achenbach Parent",2,2,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])
cbcl.t2<-rbind(cbcl.t2,read.mls.t("Achenbach Parent",2,3,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])

cbcl.t3<-read.mls.t("Achenbach Parent Report G2 T3",3,1,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)]
cbcl.t3<-rbind(cbcl.t3,read.mls.t("Achenbach Parent Report G2 I",3,2,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])
cbcl.t3<-rbind(cbcl.t3,read.mls.t("Achenbach Parent Report G2 I",3,3,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])

cbcl.t4<-read.mls.t("Achenbach Parent Report G2 T4",4,1,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)]
cbcl.t4<-rbind(cbcl.t4,read.mls.t("Achenbach Parent Report G2 I",4,2,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])
# sample 3 t-scores missing, EMAIL RYAN

cbcl.t5<-read.mls.t("Achenbach Parent Report G2 T5",5,1,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)]
cbcl.t5<-rbind(cbcl.t5,read.mls.t("Achenbach Parent Report G2 I",5,2,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])
cbcl.t5<-rbind(cbcl.t5,read.mls.t("Achenbach Parent Report G2 I",5,3,scored=T)[,c("target","tdate","age",cbcl.rcols,cbcl.tcols)])

cbcl_G2<-rbind(cbcl.t1,cbcl.t2)
cbcl_G2<-rbind(cbcl_G2,cbcl.t3)
cbcl_G2<-rbind(cbcl_G2,cbcl.t4)
cbcl_G2<-rbind(cbcl_G2,cbcl.t5)

# add ages 

cbcl_G2$cbcl.age<-NA
for (t in unique(cbcl_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  cbcl_G2[cbcl_G2$target==t,]$cbcl.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                   as.Date(cbcl_G2[cbcl_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

# can probably be broken up
hist(cbcl_G2$cbcl.age,breaks="fd")
table(table(cbcl_G2$target))
mean(table(cbcl_G2$target))

cbcl_0_9<-cbcl_G2[cbcl_G2$cbcl.age<10 & !is.na(cbcl_G2$cbcl.age),]
cbcl_10_13<-cbcl_G2[cbcl_G2$cbcl.age<14 & cbcl_G2$cbcl.age>=10 & !is.na(cbcl_G2$cbcl.age),]
cbcl_14_18<-cbcl_G2[cbcl_G2$cbcl.age<19 & cbcl_G2$cbcl.age>=14 & !is.na(cbcl_G2$cbcl.age),]


# average t-scores

G2.master[,paste0(cbcl.tcols,"_0_9")]<-NA
G2.master[,paste0(cbcl.tcols,"_10_13")]<-NA
G2.master[,paste0(cbcl.tcols,"_14_18")]<-NA

for (s in unique(G2.master$target)){
  if(length(cbcl_0_9[cbcl_0_9$target==s,]$target)>0){
    tmp<-cbcl_0_9[cbcl_0_9$target==s,]
    for (c in cbcl.tcols){
      G2.master[G2.master$target==s,paste0(c,"_0_9")]<-mean(tmp[,c],na.rm=TRUE)}
  }
  if(length(cbcl_10_13[cbcl_10_13$target==s,]$target)>0){
    tmp<-cbcl_10_13[cbcl_10_13$target==s,]
    for (c in cbcl.tcols){
      G2.master[G2.master$target==s,paste0(c,"_10_13")]<-mean(tmp[,c],na.rm=TRUE)}
  }
  if(length(cbcl_14_18[cbcl_14_18$target==s,]$target)>0){
    tmp<-cbcl_14_18[cbcl_14_18$target==s,]
    for (c in cbcl.tcols){
      G2.master[G2.master$target==s,paste0(c,"_14_18")]<-mean(tmp[,c],na.rm=TRUE)}
  }
}

# decent coverage for all 
mean(!is.na(G2.master$tottscr_0_9))
mean(!is.na(G2.master$tottscr_10_13))
mean(!is.na(G2.master$tottscr_14_18))

# relevant correlations
cor(G2.master[,c("v_res","overall.estM.IQ" ,"DoG","task",paste0(cbcl.tcols,"_0_9"))],use="pairwise.complete")
cor(G2.master[,c("v_res","overall.estM.IQ" ,"DoG","task",paste0(cbcl.tcols,"_10_13"))],use="pairwise.complete")
cor(G2.master[,c("v_res","overall.estM.IQ" ,"DoG","task",paste0(cbcl.tcols,"_14_18"))],use="pairwise.complete")

#########################################################
#### CBCL teacher report ################################
#########################################################

cbclT.t2<-read.mls.t("Achenbach Teacher Report G2",2,1,scored=T)[,c("target","tdate","age",cbcl.tcols)]
cbclT.t2<-rbind(cbclT.t2,read.mls.t("Achenbach Teacher Report G2",2,2,scored=T)[,c("target","tdate","age",cbcl.tcols)])
cbclT.t2<-rbind(cbclT.t2,read.mls.t("Achenbach Teacher Report G2",2,3,scored=T)[,c("target","tdate","age",cbcl.tcols)])

cbclT.t3<-read.mls.t("Achenbach Teacher Report G2",3,1,scored=T)[,c("target","tdate","age",cbcl.tcols)]
cbclT.t3<-rbind(cbclT.t3,read.mls.t("Achenbach Teacher Report G2",3,2,scored=T)[,c("target","tdate","age",cbcl.tcols)])
cbclT.t3<-rbind(cbclT.t3,read.mls.t("Achenbach Teacher Report G2",3,3,scored=T)[,c("target","tdate","age",cbcl.tcols)])

cbclT.t4<-read.mls.t("Achenbach Teacher Report G2",4,1,scored=T)[,c("target","tdate","age",cbcl.tcols)]
cbclT.t4<-rbind(cbclT.t4,read.mls.t("Achenbach Teacher Report G2",4,2,scored=T)[,c("target","tdate","age",cbcl.tcols)])
cbclT.t4<-rbind(cbclT.t4,read.mls.t("Achenbach Teacher Report G2",4,3,scored=T)[,c("target","tdate","age",cbcl.tcols)])

cbclT.t5<-read.mls.t("Achenbach Teacher Report G2",5,1,scored=T)[,c("target","tdate","age",cbcl.tcols)]
cbclT.t5<-rbind(cbclT.t5,read.mls.t("Achenbach Teacher Report G2",5,2,scored=T)[,c("target","tdate","age",cbcl.tcols)])
cbclT.t5<-rbind(cbclT.t5,read.mls.t("Achenbach Teacher Report G2",5,3,scored=T)[,c("target","tdate","age",cbcl.tcols)])

cbclT_G2<-rbind(cbclT.t2,cbclT.t3)
cbclT_G2<-rbind(cbclT_G2,cbclT.t4)
cbclT_G2<-rbind(cbclT_G2,cbclT.t5)

# add ages 

cbclT_G2$cbclT.age<-NA
for (t in unique(cbclT_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  cbclT_G2[cbclT_G2$target==t,]$cbclT.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                      as.Date(cbclT_G2[cbclT_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

# can probably be broken up
hist(cbclT_G2$cbclT.age,breaks="fd")
table(table(cbclT_G2$target))
mean(table(cbclT_G2$target))

# average scores

G2.master[,paste0(cbcl.tcols,"_teacher")]<-NA

for (s in unique(G2.master$target)){
  if(length(cbclT_G2[cbclT_G2$target==s,]$target)>0){
    tmp<-cbclT_G2[cbclT_G2$target==s,]
    for (c in cbcl.tcols){
      G2.master[G2.master$target==s,paste0(c,"_teacher")]<-mean(tmp[,c],na.rm=TRUE)}
  }
}

# decent coverage for all 
mean(!is.na(G2.master$tottscr_teacher))

# relevant correlations
cor(G2.master[,c("v_res","overall.estM.IQ" ,"DoG","task",paste0(cbcl.tcols,"_teacher"))],use="pairwise.complete")


########################################################
#### YSR ###############################################
########################################################

# data file with relevant G2 demographics

YSR_G2.s1<-comp.sample("Achenbach Self Report G2 T",sample=1,scored=TRUE)
YSR_G2.s2<-comp.sample("Achenbach Self Report G2 II",sample=2,scored=TRUE)
YSR_G2.s3<-comp.sample("Achenbach Self Report G2 III",sample=3,scored=TRUE)

# make long-format data file with the most relevant columns

ysr.c<-c("twthdrw","tsomati","tsocpro","tanxdep","tthotpr","tattent",
         "tdelinq","taggres","inttscr","exttscr","tottscr")

YSR.long<-do.call("rbind",
                        lapply(YSR_G2.s1[3:5],function(x){x[,c("target","tdate","age",ysr.c)]}))
YSR.long<-rbind(YSR.long,
                      do.call("rbind",
                              lapply(YSR_G2.s2[3:5],function(x){x[,c("target","tdate","age",ysr.c)]})) )
YSR.long<-rbind(YSR.long,
                      do.call("rbind",
                              lapply(YSR_G2.s3[4:5],function(x){x[,c("target","tdate","age",ysr.c)]})) )


# ANNUAL data file with relevant G2 (adolescent) demographics

YSR_Annual_G2.s1<-comp.sample("Achenbach Self Report G2",sample=1,waves = c(1:8),type = "A",scored=TRUE)
YSR_Annual_G2.s2<-comp.sample("Achenbach Self Report G2",sample=2,waves = c(1:8),type = "A",scored=TRUE)
YSR_Annual_G2.s3<-comp.sample("Achenbach Self Report G2",sample=3,waves = c(1:8),type = "A",scored=TRUE)


YSRA.long<-do.call("rbind",
                  lapply(YSR_Annual_G2.s1[1:8],function(x){x[,c("target","tdate","age",ysr.c)]}))
YSRA.long<-rbind(YSRA.long,
                do.call("rbind",
                        lapply(YSR_Annual_G2.s2[1:7],function(x){x[,c("target","tdate","age",ysr.c)]})) )
YSRA.long<-rbind(YSRA.long,
                do.call("rbind",
                        lapply(YSR_Annual_G2.s3[1:7],function(x){x[,c("target","tdate","age",ysr.c)]})) )

YSR_G2<-rbind(YSR.long,YSRA.long)

# add ages

YSR_G2$YSR.age<-NA
for (t in unique(YSR_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  YSR_G2[YSR_G2$target==t,]$YSR.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                         as.Date(YSR_G2[YSR_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

# get rid of clearly wrong ages

YSR_G2<-YSR_G2[YSR_G2$YSR.age>6 & YSR_G2$YSR.age<19 & !is.na(YSR_G2$YSR.age),]

# can probably be broken up in adolescent years
hist(YSR_G2$YSR.age,breaks="fd")
table(table(YSR_G2$target))
mean(table(YSR_G2$target))

ysr_10_13<-YSR_G2[YSR_G2$YSR.age<14 & YSR_G2$YSR.age>=10 ,]
ysr_14_18<-YSR_G2[YSR_G2$YSR.age<19 & YSR_G2$YSR.age>=14 ,]

# average t-scores

G2.master[,paste0(ysr.c,"_ysr_10_13")]<-NA
G2.master[,paste0(ysr.c,"_ysr_14_18")]<-NA

for (s in unique(G2.master$target)){
  if(length(ysr_10_13[ysr_10_13$target==s,]$target)>0){
    tmp<-ysr_10_13[ysr_10_13$target==s,]
    for (c in ysr.c){
      G2.master[G2.master$target==s,paste0(c,"_ysr_10_13")]<-mean(tmp[,c],na.rm=TRUE)}
  }
  if(length(ysr_14_18[ysr_14_18$target==s,]$target)>0){
    tmp<-ysr_14_18[ysr_14_18$target==s,]
    for (c in ysr.c){
      G2.master[G2.master$target==s,paste0(c,"_ysr_14_18")]<-mean(tmp[,c],na.rm=TRUE)}
  }
}


########################################################
#### drinking and drug history #########################
########################################################

# data file with relevant G2 demographics

dd_G2.s1<-comp.sample("Drinking & Other Drug Use G2 T",sample=1,scored=TRUE)
dd_G2.s2<-comp.sample("Drinking & Other Drug Use G2 II",sample=2,scored=TRUE)
dd_G2.s3<-comp.sample("Drinking & Other Drug Use G2 III",sample=3,scored=TRUE)

# ANNUAL data file with relevant G2 (adolescent) demographics

dd_Annual_G2.s1<-comp.sample("Drinking & Other Drug Use G2 A",sample=1,waves = c(1:26),type = "A",scored=TRUE)
dd_Annual_G2.s2<-comp.sample("Drinking & Other Drug Use G2 II",sample=2,waves = c(1:26),type = "A",scored=TRUE)
dd_Annual_G2.s3<-comp.sample("Drinking & Other Drug Use G2 III",sample=3,waves = c(1:26),type = "A",scored=TRUE)

# "Health & Daily Living" (same form, basically) with relevant G2 demographics

hdl_G2.s1<-comp.sample("Health & Daily Living G2 T",sample=1,scored=TRUE)
hdl_G2.s2<-comp.sample("Health & Daily Living G2 II",sample=2,scored=TRUE)
hdl_G2.s3<-comp.sample("Health & Daily Living G2 III",sample=3,scored=TRUE)


# name columns of interest

dd.c<-c("target","tdate","age", # basic
        "missing1","dkpbm1y", # past year drinking problems, take raw second variable OR binary (low prevalence!)
        "missing2","dgpbm1y", # past year drug problems, take raw second variable OR binary (low prevalence!)
        "missing3","binge", # past year binge drinking,  second variable categorical
        "bingedk", # binge drinking days
        "maxamt", # max amount in 24 hours, ...
        "nodrk1", "nodrk2", # no drinking in past 6 months and 6 before that, redundant with maxamt
        "fstdrink", # age at first drink, find minimum across all reports; ALSO: create binary for lifetime use?
        "daypermo", # days used alcohol per per month for past 6 months, numeric or cat?
        "drinkday", # average drinks per day
        "drunk", # how often drunk in past year
        "agedrunk", # age at first drunk, numeric
        "behv1", # friends suggested drinking, likert
        "behv2", # father drinks, make binary
        "behv3", # mother drinks, make binary
        "behv4","behv5", # parent attitudes about girls and buys drinking, likert (combine)
        "behv6", # how do kids feel about drinking, likert        
        "behv7a", # how many kids at school drink, likert
        "behv7b", # how many kids you hang around with drink, likert
        "behv8", # can get access to alc if you want, weird wording and lack of endorsement
        "behv9", # where can you get alcohol? unsure how to parse
        "behv10", # school programs about alc, make binary but this may be mostly "yes" (check)
        "cig1", #ever smoked 
        "dof1", #smoked in last 3 years 
        "cig1a1", # age at first smoke, find first report
        "cig2", #smoking in past 12 months (make binary for time period)
        "cig3", #smoking amount past 30 days (recode as numeric)
        "mar4", # age at first cannabis, find minimum across all reports; ALSO: create binary for lifetime use?
        "mar1","mar2","mar3", # past-year and past-month cannabis use, recode as numeric, also binary, also combine?
        "dog1a","dog1b","dog1c", # alternate scale past-year and past-month cannabis use, same as above ^^
        "inhal1","inhal2","inhal3",# past-year and past-month inhalent use, make binary for time point (low rates)
        "nitr1","nitr2","nitr3",# past-year and past-month nitrate use, make binary for time point (low rates)
        "amph1","amph2","amph3", "rita1","rita2","rita3", 
        "meth1","meth2","meth3", # stimulant use, make binary for time point (low rates)
        "lsd1","lsd2","lsd3","psyc1","psyc2","psyc3", #psychedelics, make binary for time point
        "crack1","crack2","crack3","coke1","coke2","coke3", #cocaine, make binary for time point
        "quaa1","quaa2","quaa3","tran1","tran2","tran3","barb1","barb2","barb3", #sedatives, make binary for time point
        "hero1","hero2","hero3","narc1","narc2","narc3",
        "oxy1","oxy2","oxy3","vico1","vico2","vico3", #opioids, make binary for time point
        "club1","club2","club3", # "club drugs", make binary for time point
        "coc1", "coc2", "coc3", #cocaine in HDL form
        "othdrug1","othdrug2","drug1","drug2","drug3" #other drugs in HDL form
        )  

# variables not present on all forms
miss.vars<-c("fstdrink","drunk","behv1","behv2", "behv3","behv4","behv5","behv6",
             "behv7a","behv7b","behv8","behv9","behv10","cig1","dof1","cig1a1",
             "mar1","mar4",
             "mar2","mar3","dog1a","dog1b","dog1c","nitr1","nitr2","nitr3",
             "crack1","crack2","crack3","oxy1","oxy2","oxy3","rita1","rita2","rita3",
             "meth1","meth2","meth3",
             "vico1","vico2","vico3","agedrunk","maxamt","club1","club2","club3","inhal1",
             "missing1","dkpbm1y","missing2","dgpbm1y","missing3","daypermo", 
             "drinkday","drunk", "agedrunk","coc1", "coc2", "coc3", 
             "othdrug1","othdrug2","drug1","drug2","drug3",
             "nodrk1", "nodrk2", "amph1","amph2","amph3","lsd1","lsd2","lsd3",
             "psyc1","psyc2","psyc3","coke1","coke2","coke3",
             "quaa1","quaa2","quaa3","tran1","tran2","tran3",
             "barb1","barb2","barb3","hero1","hero2","hero3",
             "narc1","narc2","narc3")


# alias variables
dd.alias<-c(daypermo="doa1",drinkday="doa2",maxamt="dod1",
            cig2="dof2",cig3="dof3",
            amph1="dog5a",amph2="dog5b", amph3="dog5c",rita1="dog15a",rita2="dog15b", rita3="dog15c",
            meth1="dog13a",meth2="dog13b", meth3="dog13c",
            lsd1="dog2a",lsd2="dog2b", lsd3="dog2c",
            psyc1="dog3a",psyc2="dog3b", psyc3="dog3c",coke1="dog4a",coke2="dog4b", coke3="dog4c",
            quaa1="dog6a",quaa2="dog6b", quaa3="dog6c",tran1="dog8a",tran2="dog8b", tran3="dog8c",
            barb1="dog7a",barb2="dog7b", barb3="dog7c",hero1="dog9a",hero2="dog9b", hero3="dog9c",
            narc1="dog10a",narc2="dog10b", narc3="dog10c",oxy1="dog14a",oxy2="dog14b", oxy3="dog14c",
            vico1="dog16a",vico2="dog16b", vico3="dog16c",
            club1="dog12a",club2="dog12b", club3="dog12c",
            inhal1="dog11a",inhal2="dog11b",inhal3="dog11c")


# function to clean and standardize D&DH form
process.dd<-function(dat,c,miss,alias){
  ws<-1:length(dat)
  ws<-ws[!unlist(lapply(dat,is.character)) & !unlist(lapply(dat,is.null))]
  for (w in ws){
    # rename "alias" variables
    av<-alias[alias%in%colnames(dat[[w]])]
    if(length(av)>0){
    for (a in 1:length(av)){
      dat[[w]][,names(av[a])]<-dat[[w]][,av[a]]
    }}
    # add NAs for missing variables
    for (mi in miss[!miss%in%colnames(dat[[w]])]){
      dat[[w]][,mi]<-NA
    }
    cv<-c[!c%in%c("target","tdate","age")]
    dat[[w]][,cv]<-apply(dat[[w]][,cv],2,as.character)
  }
  # bind together
  out<-do.call("rbind",
          lapply(dat[ws],function(x){x[,c]}))
  out
}

dd.long<-process.dd(dd_G2.s1,dd.c,miss.vars,dd.alias)
dd.long<-rbind(dd.long,process.dd(dd_G2.s2,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(dd_G2.s3,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(dd_Annual_G2.s1,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(dd_Annual_G2.s2,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(dd_Annual_G2.s3,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(hdl_G2.s1,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(hdl_G2.s2,dd.c,miss.vars,dd.alias))
dd.long<-rbind(dd.long,process.dd(hdl_G2.s3,dd.c,miss.vars,dd.alias))

# get ages

dd.long$dd.age<-NA
for (t in unique(dd.long$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  dd.long[dd.long$target==t,]$dd.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                   as.Date(dd.long[dd.long$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

#  observations by time
hist(dd.long[dd.long$dd.age<20,]$dd.age,breaks="fd")

# transformations to address inconsistent text

dd.long$dkpbm1y<-as.numeric(dd.long$dkpbm1y)
dd.long$dgpbm1y<-as.numeric(dd.long$dgpbm1y)
dd.long$bingedk<-as.numeric(dd.long$bingedk)
dd.long[dd.long$maxamt%in%c("99","Missing"),]$maxamt<-NA
dd.long[dd.long$maxamt%in%c(" 0","0","None (did not drink)"),]$maxamt<-0
dd.long[dd.long$maxamt%in%c(" 2","2","1-2 drinks"),]$maxamt<-2
dd.long[dd.long$maxamt%in%c(" 4","4","3-4 drinks" ),]$maxamt<-4
dd.long[dd.long$maxamt%in%c(" 6","6","5-6 drinks" ),]$maxamt<-6
dd.long[dd.long$maxamt%in%c(" 8","8","7-9 drinks" ),]$maxamt<-8
dd.long[dd.long$maxamt%in%c(" 12","12","10-14 drinks" ),]$maxamt<-12
dd.long[dd.long$maxamt%in%c(" 17","17","15-19 drinks" ),]$maxamt<-17
dd.long[dd.long$maxamt%in%c(" 22","22","20-24 drinks" ),]$maxamt<-22
dd.long[dd.long$maxamt%in%c(" 27","27","25-29 drinks" ),]$maxamt<-27
dd.long[dd.long$maxamt%in%c(" 30","30","30 or more drinks" ),]$maxamt<-30
dd.long$maxamt<-as.numeric(dd.long$maxamt)
dd.long[dd.long$drunk%in%c("99"),]$drunk<-NA
dd.long[dd.long$drunk%in%c(" 0","None","88","NA"),]$drunk<-0
dd.long[dd.long$drunk%in%c(" 1","1 time in past year"),]$drunk<-1
dd.long[dd.long$drunk%in%c(" 2","2-3 times in past year"),]$drunk<-3
dd.long[dd.long$drunk%in%c(" 3","4-5 times in past year"),]$drunk<-5
dd.long[dd.long$drunk%in%c(" 4","6-10 times in past year"),]$drunk<-8
dd.long[dd.long$drunk%in%c(" 5","Once a month"),]$drunk<-12
dd.long[dd.long$drunk%in%c(" 6","Twice a month"),]$drunk<-24
dd.long[dd.long$drunk%in%c(" 7","Once a week"),]$drunk<-52
dd.long[dd.long$drunk%in%c(" 8","Twice a week"),]$drunk<-104
dd.long[dd.long$drunk%in%c(" 9","More than twice a week"),]$drunk<-156
dd.long$drunk<-as.numeric(dd.long$drunk)
dd.long[dd.long$fstdrink%in%c("88"),]$fstdrink<-20
dd.long[dd.long$fstdrink%in%c("99","N/A","NA"),]$fstdrink<-NA
dd.long$fstdrink<-as.numeric(dd.long$fstdrink)
dd.long[dd.long$agedrunk%in%c("88"),]$agedrunk<-20
dd.long[dd.long$agedrunk%in%c("99","N/A","NA"),]$agedrunk<-NA
dd.long$agedrunk<-as.numeric(dd.long$agedrunk)
dd.long[dd.long$daypermo%in%c("NA","N/A","99"),]$daypermo<-NA
dd.long[dd.long$daypermo%in%c("88"),]$daypermo<-"0"
dd.long[dd.long$daypermo%in%c("0.5"),]$daypermo<-"1"
dd.long$daypermo<-as.numeric(dd.long$daypermo)
dd.long[dd.long$drinkday%in%c("NA","N/A","99"),]$drinkday<-NA
dd.long[dd.long$drinkday%in%c("88"),]$drinkday<-"0"
dd.long[dd.long$drinkday%in%c("0.2"),]$drinkday<-"1"
dd.long$drinkday<-as.numeric(dd.long$drinkday)
dd.long[dd.long$behv1%in%c(9,"9","Missing"),"behv1"]<-NA
dd.long[dd.long$behv1%in%c(0,"0","Never","never"),"behv1"]<-0
dd.long[dd.long$behv1%in%c(1,"1","Once or twice","once or twice"),"behv1"]<-1
dd.long[dd.long$behv1%in%c(2,"2","Several times","several times"),"behv1"]<-2
dd.long[dd.long$behv1%in%c(3,"3","Often","often"),"behv1"]<-3
dd.long[dd.long$behv2%in%c(9,"9","Missing"),"behv2"]<-NA
dd.long[dd.long$behv2%in%c(0,"0","No","no"),"behv2"]<-0
dd.long[dd.long$behv2%in%c(7,"7","I don't know","don't know"),"behv2"]<-0
dd.long[dd.long$behv2%in%c(1,"1","Yes, fairly regularly","yes, fairly regularly"),"behv2"]<-1
dd.long[dd.long$behv2%in%c(2,"2","Yes, sometimes","yes, sometimes"),"behv2"]<-1 
dd.long[dd.long$behv3%in%c(9,"9"," 9","Missing","27"),"behv3"]<-NA
dd.long[dd.long$behv3%in%c(0,"0"," 0","No","no"),"behv3"]<-0
dd.long[dd.long$behv3%in%c(7,"7"," 7","I don't know","don't know" ),"behv3"]<-0
dd.long[dd.long$behv3%in%c(1,"1"," 1","Yes, fairly regularly","yes, fairly regularly"),"behv3"]<-1
dd.long[dd.long$behv3%in%c(2,"2"," 2","Yes, sometimes","yes, sometimes"),"behv3"]<-1 
dd.long[dd.long$behv4%in%c(9,"9","Missing",7,"7","I don't know","don't know"),"behv4"]<-NA
dd.long[dd.long$behv4%in%c(1,"1","Strongly approve","strongly approve"),"behv4"]<-1
dd.long[dd.long$behv4%in%c(2,"2","Approve","approve"),"behv4"]<-2
dd.long[dd.long$behv4%in%c(3,"3","Don't care one way or other","don't care"),"behv4"]<-3
dd.long[dd.long$behv4%in%c(4,"4","Disapprove","disapprove"),"behv4"]<-4
dd.long[dd.long$behv4%in%c(5,"5","Strongly disapprove","strongly disapprove"),"behv4"]<-5
dd.long[dd.long$behv5%in%c(9,"9","Missing",7,"7","I don't know","don't know" ),"behv5"]<-NA
dd.long[dd.long$behv5%in%c(1,"1","Strongly approve","strongly approve"),"behv5"]<-1
dd.long[dd.long$behv5%in%c(2,"2","Approve","approve"),"behv5"]<-2
dd.long[dd.long$behv5%in%c(3,"3","Don't care one way or other","don't care"),"behv5"]<-3
dd.long[dd.long$behv5%in%c(4,"4","Disapprove","disapprove"),"behv5"]<-4
dd.long[dd.long$behv5%in%c(5,"5","Strongly disapprove","strongly disapprove"),"behv5"]<-5
dd.long[dd.long$behv6%in%c(9,"9","Missing",7,"7",
                           "I don't know",8,"8","N/A","don't know","does not apply"),"behv6"]<-NA
dd.long[dd.long$behv6%in%c(1,"1","Strongly approve","strongly approve"),"behv6"]<-1
dd.long[dd.long$behv6%in%c(2,"2","Approve","approve" ),"behv6"]<-2
dd.long[dd.long$behv6%in%c(3,"3","Neither approve nor disapprove","don't care"),"behv6"]<-3
dd.long[dd.long$behv6%in%c(4,"4","Disapprove","disapprove"),"behv6"]<-4
dd.long[dd.long$behv6%in%c(5,"5","Strongly disapprove","strongly disapprove"),"behv6"]<-5
dd.long[dd.long$behv7a%in%c(9,"9","Missing"),"behv7a"]<-NA
dd.long[dd.long$behv7a%in%c(0,"0","none"),"behv7a"]<-0
dd.long[dd.long$behv7a%in%c(1,"1","1-2"),"behv7a"]<-1
dd.long[dd.long$behv7a%in%c(2,"2","several"),"behv7a"]<-2
dd.long[dd.long$behv7a%in%c(3,"3","less than half"),"behv7a"]<-3
dd.long[dd.long$behv7a%in%c(4,"4","more than half"),"behv7a"]<-4
dd.long[dd.long$behv7a%in%c(5,"5","all"),"behv7a"]<-5
dd.long[dd.long$behv7b%in%c(9,"9","Missing"),"behv7b"]<-NA
dd.long[dd.long$behv7b%in%c(0,"0","none"),"behv7b"]<-0
dd.long[dd.long$behv7b%in%c(1,"1","1-2"),"behv7b"]<-1
dd.long[dd.long$behv7b%in%c(2,"2","several"),"behv7b"]<-2
dd.long[dd.long$behv7b%in%c(3,"3","less than half"),"behv7b"]<-3
dd.long[dd.long$behv7b%in%c(4,"4","more than half"),"behv7b"]<-4
dd.long[dd.long$behv7b%in%c(5,"5","all"),"behv7b"]<-5
dd.long[dd.long$behv10%in%c(9,"9","Missing","NA"),"behv10"]<-NA 
dd.long[dd.long$behv10%in%c(0,"0","No","no"),"behv10"]<-0
dd.long[dd.long$behv10%in%c(1,"1","Yes","yes"),"behv10"]<-1
dd.long[,paste0("behv",c(1:6,"7a","7b",10))] <-sapply(dd.long[,paste0("behv",c(1:6,"7a","7b",10))],as.numeric)
dd.long$behv45<-rowMeans(dd.long[,c("behv4","behv5")])

dd.long[dd.long$cig1%in%c(9,"9","NA","N/A"),"cig1"]<-NA
dd.long[dd.long$cig1%in%c(0,"0","Never",
                          "never (code rest of cigarette questions as N/A)"),"cig1"]<-0
dd.long[!is.na(dd.long$cig1) & dd.long$cig1!=0,"cig1"]<-1
dd.long$cig1<-as.numeric(dd.long$cig1)

dd.long[dd.long$dof1%in%c("NA"),"dof1"]<-NA
dd.long[dd.long$dof1%in%c("Never (go to section G)"),"dof1"]<-0
dd.long[!is.na(dd.long$dof1) & dd.long$dof1!=0,"dof1"]<-1
dd.long$dof1<-as.numeric(dd.long$dof1)

dd.long[dd.long$cig2%in%c(9,"9","Missing","NA"),"cig2"]<-NA
dd.long[dd.long$cig2%in%c(0,"0","Never",
                          8,"8","never (code rest of cigarette questions as N/A)","N/A"),"cig2"]<-0
dd.long[dd.long$cig2%in%c("1","2","3","4",
                          "Once or twice","Occasionally but not regularly",
                          "Regularly in the past","Regularly now",
                          "Regularly for a while during this year, but not now",
                          "once or twice"),"cig2"]<-1
dd.long[is.na(dd.long$cig2) & dd.long$cig1%in%c(0),"cig2"]<-0
dd.long[is.na(dd.long$cig2) & dd.long$dof1%in%c(0),"cig2"]<-0
dd.long$cig2<-as.numeric(dd.long$cig2)


dd.long[dd.long$cig3%in%c("9","Missing","NA","N/A"),]$cig3<-NA
dd.long[dd.long$cig3%in%c("8","0","Not at all","not at all"),]$cig3<-0
dd.long[dd.long$cig3%in%c("1","Less than one per day","Less than one cigarettes per day"),]$cig3<-.5
dd.long[dd.long$cig3%in%c("3","About one-half pack per day","About one half pack per day"),]$cig3<-10
dd.long[dd.long$cig3%in%c("4","About one pack per day"),]$cig3<-20
dd.long[dd.long$cig3%in%c("5","About one and one-half packs per day"),]$cig3<-30
dd.long[dd.long$cig3%in%c("6","Two packs or more per day"),]$cig3<-40
dd.long[dd.long$cig3%in%c("2","One to five cigarettes per day","one to five cigarettes per day"),]$cig3<-3
dd.long[dd.long$cig2%in%c(0),]$cig3<-0
dd.long[is.na(dd.long$cig3) & dd.long$cig1%in%c("0"),"cig3"]<-0
dd.long[is.na(dd.long$cig3) & dd.long$dof1%in%c("0"),"cig3"]<-0
dd.long$cig3<-as.numeric(dd.long$cig3)

dd.long[dd.long$mar1%in%c(9,"9","09","99"),"mar1"]<-NA
dd.long[dd.long$mar1%in%c(0,"0"," 0","0 occasions"),"mar1"]<-0
dd.long[!is.na(dd.long$mar1) & dd.long$mar1!=0,"mar1"]<-1
dd.long[dd.long$dog1a%in%c("Never"),"dog1a"]<-0
dd.long[!is.na(dd.long$dog1a) & dd.long$dog1a!=0,"dog1a"]<-1
dd.long[dd.long$mar4%in%c("88"),]$mar4<-20
dd.long[dd.long$mar4%in%c("99","N/A","NA","Missing"),]$mar4<-NA
dd.long$mar4<-as.numeric(dd.long$mar4)
dd.long[dd.long$mar2%in%c("99","9"," 9","Missing"),]$mar2<-NA
dd.long[dd.long$mar2%in%c(" 0","0","0 occasions","88","never"),]$mar2<-0
dd.long[dd.long$mar2%in%c(" 8","8","1000+"),]$mar2<-1000
dd.long[dd.long$mar2%in%c(" 7","7","100-1000"),]$mar2<-550
dd.long[dd.long$mar2%in%c(" 6","6","40-99"),]$mar2<-69.5
dd.long[dd.long$mar2%in%c(" 5","5","20-39"),]$mar2<-29.5
dd.long[dd.long$mar2%in%c(" 4","4","10-19"),]$mar2<-14.5
dd.long[dd.long$mar2%in%c(" 3","3","6-9"),]$mar2<-7.5
dd.long[dd.long$mar2%in%c(" 2","2","3-5"),]$mar2<-4
dd.long[dd.long$mar2%in%c(" 1","1","1-2"),]$mar2<-1.5
dd.long[is.na(dd.long$mar2) & dd.long$mar1%in%c(0),"mar2"]<-0
dd.long[is.na(dd.long$mar2) & dd.long$dog1a%in%c(0),"mar2"]<-0
dd.long$mar2<-as.numeric(dd.long$mar2)
dd.long[dd.long$mar3%in%c("99","9"," 9","Missing"),]$mar3<-NA
dd.long[dd.long$mar3%in%c(" 0","0","0 occasions","88","never"),]$mar3<-0
dd.long[dd.long$mar3%in%c(" 8","8","1000+"),]$mar3<-1000
dd.long[dd.long$mar3%in%c(" 7","7","100-1000"),]$mar3<-550
dd.long[dd.long$mar3%in%c(" 6","6","40-99"),]$mar3<-69.5
dd.long[dd.long$mar3%in%c(" 5","5","20-39"),]$mar3<-29.5
dd.long[dd.long$mar3%in%c(" 4","4","10-19"),]$mar3<-14.5
dd.long[dd.long$mar3%in%c(" 3","3","6-9"),]$mar3<-7.5
dd.long[dd.long$mar3%in%c(" 2","2","3-5"),]$mar3<-4
dd.long[dd.long$mar3%in%c(" 1","1","1-2"),]$mar3<-1.5
dd.long[is.na(dd.long$mar3) & dd.long$mar1%in%c(0),"mar3"]<-0
dd.long[is.na(dd.long$mar3) & dd.long$dog1a%in%c(0),"mar3"]<-0
dd.long$mar3<-as.numeric(dd.long$mar3)
dd.long[dd.long$dog1b%in%c("NA","Missing"),]$dog1b<-NA
dd.long[dd.long$dog1b%in%c("Never"),]$dog1b<-0
dd.long[dd.long$dog1b%in%c("500 and above"),]$dog1b<-500
dd.long[dd.long$dog1b%in%c("250-499 occasions"),]$dog1b<-374.5
dd.long[dd.long$dog1b%in%c("100-249 occasions"),]$dog1b<-174.5
dd.long[dd.long$dog1b%in%c("40-99 occasions"),]$dog1b<-69.5
dd.long[dd.long$dog1b%in%c("20-39 occasions"),]$dog1b<-29.5
dd.long[dd.long$dog1b%in%c("10-19 occasions"),]$dog1b<-14.5
dd.long[dd.long$dog1b%in%c("6-9 occasions"),]$dog1b<-7.5
dd.long[dd.long$dog1b%in%c("3-5 occasions"),]$dog1b<-4
dd.long[dd.long$dog1b%in%c("1-2 occasions"),]$dog1b<-1.5
dd.long[is.na(dd.long$dog1b) & dd.long$mar1%in%c(0),"dog1b"]<-0
dd.long[is.na(dd.long$dog1b) & dd.long$dog1a%in%c(0),"dog1b"]<-0
dd.long$dog1b<-as.numeric(dd.long$dog1b)
dd.long[dd.long$dog1c%in%c("NA","Missing"),]$dog1c<-NA
dd.long[dd.long$dog1c%in%c("Never"),]$dog1c<-0
dd.long[dd.long$dog1c%in%c("500 and above"),]$dog1c<-500
dd.long[dd.long$dog1c%in%c("250-499 occasions"),]$dog1c<-374.5
dd.long[dd.long$dog1c%in%c("100-249 occasions"),]$dog1c<-174.5
dd.long[dd.long$dog1c%in%c("40-99 occasions"),]$dog1c<-69.5
dd.long[dd.long$dog1c%in%c("20-39 occasions"),]$dog1c<-29.5
dd.long[dd.long$dog1c%in%c("10-19 occasions"),]$dog1c<-14.5
dd.long[dd.long$dog1c%in%c("6-9 occasions"),]$dog1c<-7.5
dd.long[dd.long$dog1c%in%c("3-5 occasions"),]$dog1c<-4
dd.long[dd.long$dog1c%in%c("1-2 occasions"),]$dog1c<-1.5
dd.long[is.na(dd.long$dog1c) & dd.long$mar1%in%c(0),"dog1c"]<-0
dd.long[is.na(dd.long$dog1c) & dd.long$dog1a%in%c(0),"dog1c"]<-0
dd.long$dog1c<-as.numeric(dd.long$dog1c)
# winsorize at 500 to equate yearly cannabis scales
dd.long[dd.long$mar2%in%c(550,1000),]$mar2<-500
# winsorize at 100 (>3x daily and the lower bound of the lowest option) for monthly
dd.long[dd.long$mar3%in%c(550,1000),]$mar3<-100
dd.long[dd.long$dog1c%in%c(174.5,374.5,500),]$dog1c<-100
# make sure 0s in 30 days aren't incorrectly coded as missing:
dd.long[dd.long$mar2%in%c(0),]$mar3<-0
dd.long[dd.long$dog1b%in%c(0),]$dog1c<-0
dd.long$can_monthlyA<-dd.long$mar3
dd.long[is.na(dd.long$mar3),]$can_monthlyA<-dd.long[is.na(dd.long$mar3),]$dog1c
dd.long$can_monthlyB<-dd.long$mar2/12
dd.long[is.na(dd.long$mar2),]$can_monthlyB<-dd.long[is.na(dd.long$mar2),]$dog1b/12
dd.long$can_monthly_est<-rowMeans(dd.long[,c("can_monthlyA","can_monthlyB")])

dd.long[dd.long$inhal1%in%c(9,"9","09","99","NA","N/A"),"inhal1"]<-NA
dd.long[dd.long$inhal1%in%c(0,"0"," 0","0 occasions","Never"),"inhal1"]<-0
dd.long[!is.na(dd.long$inhal1) & dd.long$inhal1!=0,"inhal1"]<-1
dd.long[dd.long$inhal2%in%c("99","NA","9"," 9","N/A"),]$inhal2<-NA
dd.long[dd.long$inhal2%in%c("88"," 0","0","0 occasions","Never"),]$inhal2<-0
dd.long[!is.na(dd.long$inhal2) & dd.long$inhal2!=0,]$inhal2<-1
dd.long[is.na(dd.long$inhal2) & dd.long$inhal1%in%c(0),"inhal2"]<-0
dd.long$inhal2<-as.numeric(dd.long$inhal2)
dd.long[dd.long$inhal3%in%c("99","NA","9"," 9","N/A"),]$inhal3<-NA
dd.long[dd.long$inhal3%in%c("88"," 0","0","0 occasions","Never"),]$inhal3<-0
dd.long[!is.na(dd.long$inhal3) & dd.long$inhal3!=0,]$inhal3<-1
dd.long[is.na(dd.long$inhal3) & dd.long$inhal1%in%c(0),"inhal3"]<-0
dd.long$inhal3<-as.numeric(dd.long$inhal3)
dd.long$inhal<-dd.long$inhal2
dd.long[dd.long$inhal3%in%c(1),]$inhal<-1

dd.long[dd.long$nitr1%in%c(9,"9","09","99","NA","N/A"),"nitr1"]<-NA
dd.long[dd.long$nitr1%in%c(0,"0"," 0","0 occasions","Never"),"nitr1"]<-0
dd.long[!is.na(dd.long$nitr1) & dd.long$nitr1!=0,"nitr1"]<-1
dd.long[dd.long$nitr2%in%c("99","NA","9"," 9","N/A"),]$nitr2<-NA
dd.long[dd.long$nitr2%in%c("88"," 0","0","0 occasions","Never"),]$nitr2<-0
dd.long[!is.na(dd.long$nitr2) & dd.long$nitr2!=0,]$nitr2<-1
dd.long[is.na(dd.long$nitr2) & dd.long$nitr1%in%c(0),"nitr2"]<-0
dd.long$nitr2<-as.numeric(dd.long$nitr2)
dd.long[dd.long$nitr3%in%c("99","NA","9"," 9","N/A"),]$nitr3<-NA
dd.long[dd.long$nitr3%in%c("88"," 0","0","0 occasions","Never"),]$nitr3<-0
dd.long[!is.na(dd.long$nitr3) & dd.long$nitr3!=0,]$nitr3<-1
dd.long[is.na(dd.long$nitr3) & dd.long$nitr1%in%c(0),"nitr3"]<-0
dd.long$nitr3<-as.numeric(dd.long$nitr3)
dd.long$nitr<-dd.long$nitr2
dd.long[dd.long$nitr3%in%c(1),]$nitr<-1

dd.long[dd.long$amph1%in%c(9,"9","09","99","NA","N/A"),"amph1"]<-NA
dd.long[dd.long$amph1%in%c(0,"0"," 0","0 occasions","Never"),"amph1"]<-0
dd.long[!is.na(dd.long$amph1) & dd.long$amph1!=0,"amph1"]<-1
dd.long[dd.long$amph2%in%c("99","NA","9"," 9","N/A"),]$amph2<-NA
dd.long[dd.long$amph2%in%c("88"," 0","0","0 occasions","Never"),]$amph2<-0
dd.long[!is.na(dd.long$amph2) & dd.long$amph2!=0,]$amph2<-1
dd.long[is.na(dd.long$amph2) & dd.long$amph1%in%c(0),"amph2"]<-0
dd.long$amph2<-as.numeric(dd.long$amph2)
dd.long[dd.long$amph3%in%c("99","NA","9"," 9","N/A"),]$amph3<-NA
dd.long[dd.long$amph3%in%c("88"," 0","0","0 occasions","Never"),]$amph3<-0
dd.long[!is.na(dd.long$amph3) & dd.long$amph3!=0,]$amph3<-1
dd.long[is.na(dd.long$amph3) & dd.long$amph1%in%c(0),"amph3"]<-0
dd.long$amph3<-as.numeric(dd.long$amph3)
dd.long$amph<-dd.long$amph2
dd.long[dd.long$amph3%in%c(1),]$amph<-1

dd.long[dd.long$rita1%in%c(9,"9","09","99","NA","N/A"),"rita1"]<-NA
dd.long[dd.long$rita1%in%c(0,"0"," 0","0 occasions","Never"),"rita1"]<-0
dd.long[!is.na(dd.long$rita1) & dd.long$rita1!=0,"rita1"]<-1
dd.long[dd.long$rita2%in%c("99","NA","9"," 9","N/A"),]$rita2<-NA
dd.long[dd.long$rita2%in%c("88"," 0","0","0 occasions","Never"),]$rita2<-0
dd.long[!is.na(dd.long$rita2) & dd.long$rita2!=0,]$rita2<-1
dd.long[is.na(dd.long$rita2) & dd.long$rita1%in%c(0),"rita2"]<-0
dd.long$rita2<-as.numeric(dd.long$rita2)
dd.long[dd.long$rita3%in%c("99","NA","9"," 9","N/A"),]$rita3<-NA
dd.long[dd.long$rita3%in%c("88"," 0","0","0 occasions","Never"),]$rita3<-0
dd.long[!is.na(dd.long$rita3) & dd.long$rita3!=0,]$rita3<-1
dd.long[is.na(dd.long$rita3) & dd.long$rita1%in%c(0),"rita3"]<-0
dd.long$rita3<-as.numeric(dd.long$rita3)
dd.long$rita<-dd.long$rita2
dd.long[dd.long$rita3%in%c(1),]$rita<-1

dd.long[dd.long$meth1%in%c(9,"9","09","99","N/A","NA"),"meth1"]<-NA
dd.long[dd.long$meth1%in%c(0,"0"," 0","0 occasions","Never"),"meth1"]<-0
dd.long[!is.na(dd.long$meth1) & dd.long$meth1!=0,"meth1"]<-1
dd.long[dd.long$meth2%in%c("99","NA","9"," 9","N/A"),]$meth2<-NA
dd.long[dd.long$meth2%in%c("88"," 0","0","0 occasions","Never"),]$meth2<-0
dd.long[!is.na(dd.long$meth2) & dd.long$meth2!=0,]$meth2<-1
dd.long[is.na(dd.long$meth2) & dd.long$meth1%in%c(0),"meth2"]<-0
dd.long$meth2<-as.numeric(dd.long$meth2)
dd.long[dd.long$meth3%in%c("99","NA","9"," 9","N/A"),]$meth3<-NA
dd.long[dd.long$meth3%in%c("88"," 0","0","0 occasions","Never"),]$meth3<-0
dd.long[!is.na(dd.long$meth3) & dd.long$meth3!=0,]$meth3<-1
dd.long[is.na(dd.long$meth3) & dd.long$meth1%in%c(0),"meth3"]<-0
dd.long$meth3<-as.numeric(dd.long$meth3)
dd.long$meth<-dd.long$meth2
dd.long[dd.long$meth3%in%c(1),]$meth<-1

dd.long[dd.long$lsd1%in%c(9,"9","09","99","NA","N/A"),"lsd1"]<-NA
dd.long[dd.long$lsd1%in%c(0,"0"," 0","0 occasions","Never"),"lsd1"]<-0
dd.long[!is.na(dd.long$lsd1) & dd.long$lsd1!=0,"lsd1"]<-1
dd.long[dd.long$lsd2%in%c("99","NA","9"," 9","N/A"),]$lsd2<-NA
dd.long[dd.long$lsd2%in%c("88"," 0","0","0 occasions","Never"),]$lsd2<-0
dd.long[!is.na(dd.long$lsd2) & dd.long$lsd2!=0,]$lsd2<-1
dd.long[is.na(dd.long$lsd2) & dd.long$lsd1%in%c(0),"lsd2"]<-0
dd.long$lsd2<-as.numeric(dd.long$lsd2)
dd.long[dd.long$lsd3%in%c("99","NA","9"," 9","N/A"),]$lsd3<-NA
dd.long[dd.long$lsd3%in%c("88"," 0","0","0 occasions","Never"),]$lsd3<-0
dd.long[!is.na(dd.long$lsd3) & dd.long$lsd3!=0,]$lsd3<-1
dd.long[is.na(dd.long$lsd3) & dd.long$lsd1%in%c(0),"lsd3"]<-0
dd.long$lsd3<-as.numeric(dd.long$lsd3)
dd.long$lsd<-dd.long$lsd2
dd.long[dd.long$lsd3%in%c(1),]$lsd<-1

dd.long[dd.long$psyc1%in%c(9,"9","09","99","NA","N/A"),"psyc1"]<-NA
dd.long[dd.long$psyc1%in%c(0,"0"," 0","0 occasions","Never"),"psyc1"]<-0
dd.long[!is.na(dd.long$psyc1) & dd.long$psyc1!=0,"psyc1"]<-1
dd.long[dd.long$psyc2%in%c("99","NA","9"," 9","N/A"),]$psyc2<-NA
dd.long[dd.long$psyc2%in%c("88"," 0","0","0 occasions","Never"),]$psyc2<-0
dd.long[!is.na(dd.long$psyc2) & dd.long$psyc2!=0,]$psyc2<-1
dd.long[is.na(dd.long$psyc2) & dd.long$psyc1%in%c(0),"psyc2"]<-0
dd.long$psyc2<-as.numeric(dd.long$psyc2)
dd.long[dd.long$psyc3%in%c("99","NA","9"," 9","N/A"),]$psyc3<-NA
dd.long[dd.long$psyc3%in%c("88"," 0","0","0 occasions","Never"),]$psyc3<-0
dd.long[!is.na(dd.long$psyc3) & dd.long$psyc3!=0,]$psyc3<-1
dd.long[is.na(dd.long$psyc3) & dd.long$psyc1%in%c(0),"psyc3"]<-0
dd.long$psyc3<-as.numeric(dd.long$psyc3)
dd.long$psyc<-dd.long$psyc2
dd.long[dd.long$psyc3%in%c(1),]$psyc<-1

dd.long[dd.long$crack1%in%c(9,"9","09","99","NA","N/A"),"crack1"]<-NA
dd.long[dd.long$crack1%in%c(0,"0"," 0","0 occasions","Never"),"crack1"]<-0
dd.long[!is.na(dd.long$crack1) & dd.long$crack1!=0,"crack1"]<-1
dd.long[dd.long$crack2%in%c("99","NA","9"," 9","N/A"),]$crack2<-NA
dd.long[dd.long$crack2%in%c("88"," 0","0","0 occasions","Never"),]$crack2<-0
dd.long[!is.na(dd.long$crack2) & dd.long$crack2!=0,]$crack2<-1
dd.long[is.na(dd.long$crack2) & dd.long$crack1%in%c(0),"crack2"]<-0
dd.long$crack2<-as.numeric(dd.long$crack2)
dd.long[dd.long$crack3%in%c("99","NA","9"," 9","N/A"),]$crack3<-NA
dd.long[dd.long$crack3%in%c("88"," 0","0","0 occasions","Never"),]$crack3<-0
dd.long[!is.na(dd.long$crack3) & dd.long$crack3!=0,]$crack3<-1
dd.long[is.na(dd.long$crack3) & dd.long$crack1%in%c(0),"crack3"]<-0
dd.long$crack3<-as.numeric(dd.long$crack3)
dd.long$crack<-dd.long$crack2
dd.long[dd.long$crack3%in%c(1),]$crack<-1

dd.long[dd.long$coke1%in%c(9,"9","09","99","NA","N/A"),"coke1"]<-NA
dd.long[dd.long$coke1%in%c(0,"0"," 0","0 occasions","Never"),"coke1"]<-0
dd.long[!is.na(dd.long$coke1) & dd.long$coke1!=0,"coke1"]<-1
dd.long[dd.long$coke2%in%c("99","NA","9"," 9","N/A"),]$coke2<-NA
dd.long[dd.long$coke2%in%c("88"," 0","0","0 occasions","Never"),]$coke2<-0
dd.long[!is.na(dd.long$coke2) & dd.long$coke2!=0,]$coke2<-1
dd.long[is.na(dd.long$coke2) & dd.long$coke1%in%c(0),"coke2"]<-0
dd.long$coke2<-as.numeric(dd.long$coke2)
dd.long[dd.long$coke3%in%c("99","NA","9"," 9","N/A"),]$coke3<-NA
dd.long[dd.long$coke3%in%c("88"," 0","0","0 occasions","Never"),]$coke3<-0
dd.long[!is.na(dd.long$coke3) & dd.long$coke3!=0,]$coke3<-1
dd.long[is.na(dd.long$coke3) & dd.long$coke1%in%c(0),"coke3"]<-0
dd.long$coke3<-as.numeric(dd.long$coke3)
dd.long$coke<-dd.long$coke2
dd.long[dd.long$coke3%in%c(1),]$coke<-1 
# (nobody reported cocaine on H&DL form "coc" variables)

dd.long[dd.long$quaa1%in%c(9,"9","09","99","NA","N/A"),"quaa1"]<-NA
dd.long[dd.long$quaa1%in%c(0,"0"," 0","0 occasions","Never"),"quaa1"]<-0
dd.long[!is.na(dd.long$quaa1) & dd.long$quaa1!=0,"quaa1"]<-1
dd.long[dd.long$quaa2%in%c("99","NA","9"," 9","N/A"),]$quaa2<-NA
dd.long[dd.long$quaa2%in%c("88"," 0","0","0 occasions","Never"),]$quaa2<-0
dd.long[!is.na(dd.long$quaa2) & dd.long$quaa2!=0,]$quaa2<-1
dd.long[is.na(dd.long$quaa2) & dd.long$quaa1%in%c(0),"quaa2"]<-0
dd.long$quaa2<-as.numeric(dd.long$quaa2)
dd.long[dd.long$quaa3%in%c("99","NA","9"," 9","N/A"),]$quaa3<-NA
dd.long[dd.long$quaa3%in%c("88"," 0","0","0 occasions","Never"),]$quaa3<-0
dd.long[!is.na(dd.long$quaa3) & dd.long$quaa3!=0,]$quaa3<-1
dd.long[is.na(dd.long$quaa3) & dd.long$quaa1%in%c(0),"quaa3"]<-0
dd.long$quaa3<-as.numeric(dd.long$quaa3)
dd.long$quaa<-dd.long$quaa2
dd.long[dd.long$quaa3%in%c(1),]$quaa<-1 

dd.long[dd.long$tran1%in%c(9,"9","09","99","NA","N/A"),"tran1"]<-NA
dd.long[dd.long$tran1%in%c(0,"0"," 0","0 occasions","Never"),"tran1"]<-0
dd.long[!is.na(dd.long$tran1) & dd.long$tran1!=0,"tran1"]<-1
dd.long[dd.long$tran2%in%c("99","NA","9"," 9","N/A"),]$tran2<-NA
dd.long[dd.long$tran2%in%c("88"," 0","0","0 occasions","Never"),]$tran2<-0
dd.long[!is.na(dd.long$tran2) & dd.long$tran2!=0,]$tran2<-1
dd.long[is.na(dd.long$tran2) & dd.long$tran1%in%c(0),"tran2"]<-0
dd.long$tran2<-as.numeric(dd.long$tran2)
dd.long[dd.long$tran3%in%c("99","NA","9"," 9","N/A"),]$tran3<-NA
dd.long[dd.long$tran3%in%c("88"," 0","0","0 occasions","Never"),]$tran3<-0
dd.long[!is.na(dd.long$tran3) & dd.long$tran3!=0,]$tran3<-1
dd.long[is.na(dd.long$tran3) & dd.long$tran1%in%c(0),"tran3"]<-0
dd.long$tran3<-as.numeric(dd.long$tran3)
dd.long$tran<-dd.long$tran2
dd.long[dd.long$tran3%in%c(1),]$tran<-1 

dd.long[dd.long$barb1%in%c(9,"9","09","99","NA","N/A"),"barb1"]<-NA
dd.long[dd.long$barb1%in%c(0,"0"," 0","0 occasions","Never"),"barb1"]<-0
dd.long[!is.na(dd.long$barb1) & dd.long$barb1!=0,"barb1"]<-1
dd.long[dd.long$barb2%in%c("99","NA","9"," 9","N/A"),]$barb2<-NA
dd.long[dd.long$barb2%in%c("88"," 0","0","0 occasions","Never"),]$barb2<-0
dd.long[!is.na(dd.long$barb2) & dd.long$barb2!=0,]$barb2<-1
dd.long[is.na(dd.long$barb2) & dd.long$barb1%in%c(0),"barb2"]<-0
dd.long$barb2<-as.numeric(dd.long$barb2)
dd.long[dd.long$barb3%in%c("99","NA","9"," 9","N/A"),]$barb3<-NA
dd.long[dd.long$barb3%in%c("88"," 0","0","0 occasions","Never"),]$barb3<-0
dd.long[!is.na(dd.long$barb3) & dd.long$barb3!=0,]$barb3<-1
dd.long[is.na(dd.long$barb3) & dd.long$barb1%in%c(0),"barb3"]<-0
dd.long$barb3<-as.numeric(dd.long$barb3)
dd.long$barb<-dd.long$barb2
dd.long[dd.long$barb3%in%c(1),]$barb<-1 

dd.long[dd.long$hero1%in%c(9,"9","09","99","NA","N/A"),"hero1"]<-NA
dd.long[dd.long$hero1%in%c(0,"0"," 0","0 occasions","Never"),"hero1"]<-0
dd.long[!is.na(dd.long$hero1) & dd.long$hero1!=0,"hero1"]<-1
dd.long[dd.long$hero2%in%c("99","NA","9"," 9","N/A"),]$hero2<-NA
dd.long[dd.long$hero2%in%c("88"," 0","0","0 occasions","Never"),]$hero2<-0
dd.long[!is.na(dd.long$hero2) & dd.long$hero2!=0,]$hero2<-1
dd.long[is.na(dd.long$hero2) & dd.long$hero1%in%c(0),"hero2"]<-0
dd.long$hero2<-as.numeric(dd.long$hero2)
dd.long[dd.long$hero3%in%c("99","NA","9"," 9","N/A"),]$hero3<-NA
dd.long[dd.long$hero3%in%c("88"," 0","0","0 occasions","Never"),]$hero3<-0
dd.long[!is.na(dd.long$hero3) & dd.long$hero3!=0,]$hero3<-1
dd.long[is.na(dd.long$hero3) & dd.long$hero1%in%c(0),"hero3"]<-0
dd.long$hero3<-as.numeric(dd.long$hero3)
dd.long$hero<-dd.long$hero2
dd.long[dd.long$hero3%in%c(1),]$hero<-1 
# one person reports heroin on H&DL survey (only example of other drug use):
dd.long[dd.long$drug1%in%c("heroin"),]$hero<-1 

dd.long[dd.long$narc1%in%c(9,"9","09","99","NA","N/A"),"narc1"]<-NA
dd.long[dd.long$narc1%in%c(0,"0"," 0","0 occasions","Never"),"narc1"]<-0
dd.long[!is.na(dd.long$narc1) & dd.long$narc1!=0,"narc1"]<-1
dd.long[dd.long$narc2%in%c("99","NA","9"," 9","N/A"),]$narc2<-NA
dd.long[dd.long$narc2%in%c("88"," 0","0","0 occasions","Never"),]$narc2<-0
dd.long[!is.na(dd.long$narc2) & dd.long$narc2!=0,]$narc2<-1
dd.long[is.na(dd.long$narc2) & dd.long$narc1%in%c(0),"narc2"]<-0
dd.long$narc2<-as.numeric(dd.long$narc2)
dd.long[dd.long$narc3%in%c("99","NA","9"," 9","N/A"),]$narc3<-NA
dd.long[dd.long$narc3%in%c("88"," 0","0","0 occasions","Never"),]$narc3<-0
dd.long[!is.na(dd.long$narc3) & dd.long$narc3!=0,]$narc3<-1
dd.long[is.na(dd.long$narc3) & dd.long$narc1%in%c(0),"narc3"]<-0
dd.long$narc3<-as.numeric(dd.long$narc3)
dd.long$narc<-dd.long$narc2
dd.long[dd.long$narc3%in%c(1),]$narc<-1 

dd.long[dd.long$oxy1%in%c(9,"9","09","99","NA","N/A"),"oxy1"]<-NA
dd.long[dd.long$oxy1%in%c(0,"0"," 0","0 occasions","Never"),"oxy1"]<-0
dd.long[!is.na(dd.long$oxy1) & dd.long$oxy1!=0,"oxy1"]<-1
dd.long[dd.long$oxy2%in%c("99","NA","9"," 9","N/A"),]$oxy2<-NA
dd.long[dd.long$oxy2%in%c("88"," 0","0","0 occasions","Never"),]$oxy2<-0
dd.long[!is.na(dd.long$oxy2) & dd.long$oxy2!=0,]$oxy2<-1
dd.long[is.na(dd.long$oxy2) & dd.long$oxy1%in%c(0),"oxy2"]<-0
dd.long$oxy2<-as.numeric(dd.long$oxy2)
dd.long[dd.long$oxy3%in%c("99","NA","9"," 9","N/A"),]$oxy3<-NA
dd.long[dd.long$oxy3%in%c("88"," 0","0","0 occasions","Never"),]$oxy3<-0
dd.long[!is.na(dd.long$oxy3) & dd.long$oxy3!=0,]$oxy3<-1
dd.long[is.na(dd.long$oxy3) & dd.long$oxy1%in%c(0),"oxy3"]<-0
dd.long$oxy3<-as.numeric(dd.long$oxy3)
dd.long$oxy<-dd.long$oxy2
dd.long[dd.long$oxy3%in%c(1),]$oxy<-1 

dd.long[dd.long$vico1%in%c(9,"9","09","99","NA","N/A"),"vico1"]<-NA
dd.long[dd.long$vico1%in%c(0,"0"," 0","0 occasions","Never"),"vico1"]<-0
dd.long[!is.na(dd.long$vico1) & dd.long$vico1!=0,"vico1"]<-1
dd.long[dd.long$vico2%in%c("99","NA","9"," 9","N/A"),]$vico2<-NA
dd.long[dd.long$vico2%in%c("88"," 0","0","0 occasions","Never"),]$vico2<-0
dd.long[!is.na(dd.long$vico2) & dd.long$vico2!=0,]$vico2<-1
dd.long[is.na(dd.long$vico2) & dd.long$vico1%in%c(0),"vico2"]<-0
dd.long$vico2<-as.numeric(dd.long$vico2)
dd.long[dd.long$vico3%in%c("99","NA","9"," 9","N/A"),]$vico3<-NA
dd.long[dd.long$vico3%in%c("88"," 0","0","0 occasions","Never"),]$vico3<-0
dd.long[!is.na(dd.long$vico3) & dd.long$vico3!=0,]$vico3<-1
dd.long[is.na(dd.long$vico3) & dd.long$vico1%in%c(0),"vico3"]<-0
dd.long$vico3<-as.numeric(dd.long$vico3)
dd.long$vico<-dd.long$vico2
dd.long[dd.long$vico3%in%c(1),]$vico<-1 

dd.long[dd.long$club1%in%c(9,"9","09","99","NA","N/A"),"club1"]<-NA
dd.long[dd.long$club1%in%c(0,"0"," 0","0 occasions","Never"),"club1"]<-0
dd.long[!is.na(dd.long$club1) & dd.long$club1!=0,"club1"]<-1
dd.long[dd.long$club2%in%c("99","NA","9"," 9","N/A"),]$club2<-NA
dd.long[dd.long$club2%in%c("88"," 0","0","0 occasions","Never"),]$club2<-0
dd.long[!is.na(dd.long$club2) & dd.long$club2!=0,]$club2<-1
dd.long[is.na(dd.long$club2) & dd.long$club1%in%c(0),"club2"]<-0
dd.long$club2<-as.numeric(dd.long$club2)
dd.long[dd.long$club3%in%c("99","NA","9"," 9","N/A"),]$club3<-NA
dd.long[dd.long$club3%in%c("88"," 0","0","0 occasions","Never"),]$club3<-0
dd.long[!is.na(dd.long$club3) & dd.long$club3!=0,]$club3<-1
dd.long[is.na(dd.long$club3) & dd.long$club1%in%c(0),"club3"]<-0
dd.long$club3<-as.numeric(dd.long$club3)
dd.long$club<-dd.long$club2
dd.long[dd.long$club3%in%c(1),]$club<-1 

# only those with age calculated from master file
dd.long<-dd.long[!is.na(dd.long$dd.age),]

# summaries for main data object

G2.master$alc_probs_10_13<-NA
G2.master$alc_probs_14_15<-NA
G2.master$alc_probs_16_17<-NA
G2.master$alc_probs_18_19<-NA
G2.master$alc_probs_20_26<-NA

G2.master$drug_probs_10_13<-NA
G2.master$drug_probs_14_15<-NA
G2.master$drug_probs_16_17<-NA
G2.master$drug_probs_18_19<-NA
G2.master$drug_probs_20_26<-NA

G2.master$binge_10_13<-NA
G2.master$binge_14_15<-NA
G2.master$binge_16_17<-NA
G2.master$binge_18_19<-NA
G2.master$binge_20_26<-NA

G2.master$bingedk_10_13<-NA
G2.master$bingedk_14_15<-NA
G2.master$bingedk_16_17<-NA
G2.master$bingedk_18_19<-NA
G2.master$bingedk_20_26<-NA

G2.master$drunk_10_13<-NA
G2.master$drunk_14_15<-NA
G2.master$drunk_16_19<-NA
G2.master$drunk_20_26<-NA

G2.master$maxamt_10_13<-NA
G2.master$maxamt_14_15<-NA
G2.master$maxamt_16_17<-NA
G2.master$maxamt_18_19<-NA
G2.master$maxamt_20_26<-NA

G2.master$daypermo_10_13<-NA
G2.master$daypermo_14_15<-NA
G2.master$daypermo_16_17<-NA
G2.master$daypermo_18_19<-NA
G2.master$daypermo_20_26<-NA

G2.master$drinkday_10_13<-NA
G2.master$drinkday_14_15<-NA
G2.master$drinkday_16_17<-NA
G2.master$drinkday_18_19<-NA
G2.master$drinkday_20_26<-NA

G2.master$age_drink_min<-NA
G2.master$age_drink_min_rage<-NA
G2.master$age_drink_first<-NA
G2.master$age_drink_first_rage<-NA
G2.master$age_drunk_min<-NA
G2.master$age_drunk_min_rage<-NA
G2.master$age_drunk_first<-NA
G2.master$age_drunk_first_rage<-NA
G2.master$age_drink_est<-NA
G2.master$age_drink_final<-NA
G2.master$drink.discr.flag<-NA
G2.master$drunk.discr.flag<-NA
G2.master$drink.age.flag<-NA
G2.master$drunk.age.flag<-NA
G2.master$drink.maxamt.flag<-NA

G2.master[,paste0("behv",c(1:3,"45",6,"7a","7b",8:10),"_10_13")]<-NA 
G2.master[,paste0("behv",c(1:3,"45",6,"7a","7b",8:10),"_14_15")]<-NA
G2.master[,paste0("behv",c(1:3,"45",6,"7a","7b",8:10),"_16_19")]<-NA


G2.master$age_cig_est<-NA
G2.master$cig_10_13<-NA
G2.master$cig_14_15<-NA
G2.master$cig_16_17<-NA
G2.master$cig_18_19<-NA
G2.master$cig_20_26<-NA

G2.master$can_m_est_10_13<-NA
G2.master$can_m_est_14_15<-NA
G2.master$can_m_est_16_17<-NA
G2.master$can_m_est_18_19<-NA
G2.master$can_m_est_20_26<-NA

G2.master$age_can_min<-NA 
G2.master$age_can_min_rage<-NA
G2.master$age_can_first<-NA 
G2.master$age_can_first_rage<-NA
G2.master$age_can_est<-NA 
G2.master$age_can_final<-NA 
G2.master$can.discr.flag<-NA 
G2.master$can.age.flag<-NA 
G2.master$can.est.flag<-NA 

G2.master$inhal<-NA
G2.master$amph<-NA
G2.master$rita<-NA 
G2.master$meth<-NA 
G2.master$psyche<-NA  
G2.master$cocaine<-NA  
G2.master$sedat<-NA 
G2.master$narco<-NA 
G2.master$presc.narco<-NA
G2.master$club<-NA 

G2.master$inhal_20_26<-NA
G2.master$amph_20_26<-NA
G2.master$rita_20_26<-NA
G2.master$meth_20_26<-NA 
G2.master$psyche_20_26<-NA 
G2.master$cocaine_20_26<-NA 
G2.master$sedat_20_26<-NA 
G2.master$narco_20_26<-NA 
G2.master$presc.narco_20_26<-NA 
G2.master$club_20_26<-NA 

for (s in unique(G2.master$target)){
  if(length(dd.long[dd.long$target==s,]$target)>0){
    
    tmp<-dd.long[dd.long$target==s,]
    
      G2.master[G2.master$target==s,]$alc_probs_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$dkpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$alc_probs_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$dkpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$alc_probs_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$dkpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$alc_probs_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$dkpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$alc_probs_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$dkpbm1y,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$drug_probs_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$dgpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drug_probs_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$dgpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drug_probs_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$dgpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drug_probs_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$dgpbm1y,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drug_probs_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$dgpbm1y,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$binge_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$binge=="1.yes",na.rm=TRUE)>0
      G2.master[G2.master$target==s,]$binge_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$binge=="1.yes",na.rm=TRUE)>0
      G2.master[G2.master$target==s,]$binge_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$binge=="1.yes",na.rm=TRUE)>0
      G2.master[G2.master$target==s,]$binge_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$binge=="1.yes",na.rm=TRUE)>0
      G2.master[G2.master$target==s,]$binge_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$binge=="1.yes",na.rm=TRUE)>0
      
      G2.master[G2.master$target==s,]$bingedk_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$bingedk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$bingedk_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$bingedk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$bingedk_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$bingedk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$bingedk_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$bingedk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$bingedk_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$bingedk,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$drunk_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$drunk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drunk_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$drunk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drunk_16_19<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$drunk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drunk_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$drunk,na.rm=TRUE)

      G2.master[G2.master$target==s,]$maxamt_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$maxamt,na.rm=TRUE)
      G2.master[G2.master$target==s,]$maxamt_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$maxamt,na.rm=TRUE)
      G2.master[G2.master$target==s,]$maxamt_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$maxamt,na.rm=TRUE)
      G2.master[G2.master$target==s,]$maxamt_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$maxamt,na.rm=TRUE)
      G2.master[G2.master$target==s,]$maxamt_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$maxamt,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$daypermo_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$daypermo,na.rm=TRUE)
      G2.master[G2.master$target==s,]$daypermo_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$daypermo,na.rm=TRUE)
      G2.master[G2.master$target==s,]$daypermo_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$daypermo,na.rm=TRUE)
      G2.master[G2.master$target==s,]$daypermo_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$daypermo,na.rm=TRUE)
      G2.master[G2.master$target==s,]$daypermo_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$daypermo,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$drinkday_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$drinkday,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drinkday_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$drinkday,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drinkday_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$drinkday,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drinkday_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$drinkday,na.rm=TRUE)
      G2.master[G2.master$target==s,]$drinkday_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$drinkday,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$cig_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$cig3,na.rm=TRUE)
      G2.master[G2.master$target==s,]$cig_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$cig3,na.rm=TRUE)
      G2.master[G2.master$target==s,]$cig_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$cig3,na.rm=TRUE)
      G2.master[G2.master$target==s,]$cig_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$cig3,na.rm=TRUE)
      G2.master[G2.master$target==s,]$cig_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$cig3,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$can_m_est_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$can_monthly_est,na.rm=TRUE)
      G2.master[G2.master$target==s,]$can_m_est_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$can_monthly_est,na.rm=TRUE)
      G2.master[G2.master$target==s,]$can_m_est_16_17<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<18,]$can_monthly_est,na.rm=TRUE)
      G2.master[G2.master$target==s,]$can_m_est_18_19<-mean(tmp[tmp$dd.age>=18 & tmp$dd.age<20,]$can_monthly_est,na.rm=TRUE)
      G2.master[G2.master$target==s,]$can_m_est_20_26<-mean(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$can_monthly_est,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$behv1_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv1,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv1_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv1,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv1_16_19<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv1,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$behv2_10_13<-as.numeric(mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv2,na.rm=TRUE)>0)
      G2.master[G2.master$target==s,]$behv2_14_15<-as.numeric(mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv2,na.rm=TRUE)>0)
      G2.master[G2.master$target==s,]$behv2_16_19<-as.numeric(mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv2,na.rm=TRUE)>0)
      
      G2.master[G2.master$target==s,]$behv3_10_13<-as.numeric(mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv3,na.rm=TRUE)>0)
      G2.master[G2.master$target==s,]$behv3_14_15<-as.numeric(mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv3,na.rm=TRUE)>0)
      G2.master[G2.master$target==s,]$behv3_16_19<-as.numeric(mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv3,na.rm=TRUE)>0)
      
      G2.master[G2.master$target==s,]$behv45_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv45,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv45_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv45,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv45_16_19<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv45,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$behv6_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv6,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv6_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv6,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv6_16_19<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv6,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$behv7a_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv7a,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv7a_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv7a,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv7a_16_19<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv7a,na.rm=TRUE)

      G2.master[G2.master$target==s,]$behv7b_10_13<-mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv7b,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv7b_14_15<-mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv7b,na.rm=TRUE)
      G2.master[G2.master$target==s,]$behv7b_16_19<-mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv7b,na.rm=TRUE)
      
      G2.master[G2.master$target==s,]$behv10_10_13<-as.numeric(mean(tmp[tmp$dd.age>=10 & tmp$dd.age<14,]$behv10,na.rm=TRUE)>0)
      G2.master[G2.master$target==s,]$behv10_14_15<-as.numeric(mean(tmp[tmp$dd.age>=14 & tmp$dd.age<16,]$behv10,na.rm=TRUE)>0)
      G2.master[G2.master$target==s,]$behv10_16_19<-as.numeric(mean(tmp[tmp$dd.age>=16 & tmp$dd.age<20,]$behv10,na.rm=TRUE)>0)
      
      if(sum(!is.na(tmp[tmp$dd.age<20,]$inhal))>0){
        G2.master[G2.master$target==s,]$inhal<-sum(tmp[tmp$dd.age<20,]$inhal)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age<20,]$amph))>0){
        G2.master[G2.master$target==s,]$amph<-sum(tmp[tmp$dd.age<20,]$amph,na.rm=TRUE)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age<20,]$rita))>0){
        G2.master[G2.master$target==s,]$rita<-sum(tmp[tmp$dd.age<20,]$rita,na.rm=TRUE)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age<20,]$meth))>0){
        G2.master[G2.master$target==s,]$meth<-sum(tmp[tmp$dd.age<20,]$meth,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age<20,]$lsd,tmp[tmp$dd.age<20,]$psyc)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$psyche<-sum(tf,na.rm=TRUE)>0}
    
      tf<-c(tmp[tmp$dd.age<20,]$coke,tmp[tmp$dd.age<20,]$crack)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$cocaine<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age<20,]$coke,tmp[tmp$dd.age<20,]$crack)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$cocaine<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age<20,]$quaa,tmp[tmp$dd.age<20,]$tran,tmp[tmp$dd.age<20,]$barb)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$sedat<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age<20,]$hero,tmp[tmp$dd.age<20,]$narc)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$narco<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age<20,]$oxy,tmp[tmp$dd.age<20,]$vico)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$presc.narco<-sum(tf,na.rm=TRUE)>0}

      if(sum(!is.na(tmp[tmp$dd.age<20,]$club))>0){
        G2.master[G2.master$target==s,]$club<-sum(tmp[tmp$dd.age<20,]$club,na.rm=TRUE)>0}
      
      # same for ages 20-26
      
      if(sum(!is.na(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$inhal))>0){
        G2.master[G2.master$target==s,]$inhal_20_26<-sum(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$inhal)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$amph))>0){
        G2.master[G2.master$target==s,]$amph_20_26<-sum(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$amph,na.rm=TRUE)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$rita))>0){
        G2.master[G2.master$target==s,]$rita_20_26<-sum(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$rita,na.rm=TRUE)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$meth))>0){
        G2.master[G2.master$target==s,]$meth_20_26<-sum(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$meth,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$lsd,tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$psyc)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$psyche_20_26<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$coke,tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$crack)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$cocaine_20_26<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$quaa,tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$tran,tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$barb)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$sedat_20_26<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$hero,tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$narc)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$narco_20_26<-sum(tf,na.rm=TRUE)>0}
      
      tf<-c(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$oxy,tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$vico)
      if(sum(!is.na(tf))>0){G2.master[G2.master$target==s,]$presc.narco_20_26<-sum(tf,na.rm=TRUE)>0}
      
      if(sum(!is.na(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$club))>0){
        G2.master[G2.master$target==s,]$club_20_26<-sum(tmp[tmp$dd.age>=20 & tmp$dd.age<27,]$club,na.rm=TRUE)>0}
      
      # ages
      
      if(sum(!is.na(tmp$fstdrink))>0){
      G2.master[G2.master$target==s,]$age_drink_min<-min(tmp$fstdrink,na.rm=TRUE)
      G2.master[G2.master$target==s,]$age_drink_min_rage<-min(tmp[tmp$fstdrink==min(tmp$fstdrink,na.rm=TRUE),]$dd.age,na.rm=TRUE)
      tf<-tmp[!is.na(tmp$fstdrink) & tmp$fstdrink!=20,]
      if(length(tf$target)>0){
      G2.master[G2.master$target==s,]$age_drink_first<-tf[tf$dd.age==min(tf$dd.age,na.rm=TRUE),]$fstdrink
      G2.master[G2.master$target==s,]$age_drink_first_rage<-min(tf$dd.age,na.rm=TRUE)}
      # flag for discrepancy with earlier age 
      G2.master[G2.master$target==s,]$drink.discr.flag<-(max(floor(c(tmp[!is.na(tmp$fstdrink) & tmp$fstdrink==20,"age"],0)))>G2.master[G2.master$target==s,]$age_drink_first)
      }
      if(sum(!is.na(tmp$agedrunk))>0){
      G2.master[G2.master$target==s,]$age_drunk_min<-min(tmp$agedrunk,na.rm=TRUE)
      G2.master[G2.master$target==s,]$age_drunk_min_rage<-min(tmp[tmp$agedrunk==min(tmp$agedrunk,na.rm=TRUE),]$dd.age,na.rm=TRUE)
      tf<-tmp[!is.na(tmp$agedrunk) & tmp$agedrunk!=20,]
      if(length(tf$target)>0){
      G2.master[G2.master$target==s,]$age_drunk_first<-tf[tf$dd.age==min(tf$dd.age,na.rm=TRUE),]$agedrunk
      G2.master[G2.master$target==s,]$age_drunk_first_rage<-min(tf$dd.age,na.rm=TRUE)}
      # flag for discrepancy with earlier age 
      G2.master[G2.master$target==s,]$drunk.discr.flag<-(max(floor(c(tmp[!is.na(tmp$agedrunk) & tmp$agedrunk==20,"dd.age"],0)))>G2.master[G2.master$target==s,]$age_drunk_first)
      }
      #infer form maxamt
      tf<-tmp[tmp$maxamt>0,]
      if(length(na.omit(tf$maxamt))>0){
        G2.master[G2.master$target==s,]$age_drink_est<-floor(min(tf$dd.age,na.rm=TRUE))}
      
      if(length(tmp[!is.na(tmp$cig2) & tmp$dd.age<20,]$cig2)>0){
        if(length(tmp[!is.na(tmp$cig2) & tmp$cig2==1 & tmp$dd.age<20,]$cig2)>0){
        G2.master[G2.master$target==s,]$age_cig_est<-min(tmp[!is.na(tmp$cig2) & tmp$cig2==1,]$dd.age, na.rm=TRUE)}
        if(length(tmp[!is.na(tmp$cig2) & tmp$cig2==1 & tmp$dd.age<20,]$cig2)==0){G2.master[G2.master$target==s,]$age_cig_est<-20}
      } 
      
      if(sum(!is.na(tmp$mar4))>0){
        G2.master[G2.master$target==s,]$age_can_min<-min(tmp$mar4,na.rm=TRUE)
        G2.master[G2.master$target==s,]$age_can_min_rage<-min(tmp[tmp$mar4==min(tmp$mar4,na.rm=TRUE),]$dd.age,na.rm=TRUE)}
      if(sum(!is.na(tmp$mar4))>0){
        tf<-tmp[!is.na(tmp$mar4) & tmp$mar4!=20,]
        if(length(tf$target)>0){
          G2.master[G2.master$target==s,]$age_can_first<-tf[tf$dd.age==min(tf$dd.age,na.rm=TRUE),]$mar4
          G2.master[G2.master$target==s,]$age_can_first_rage<-min(tf$dd.age,na.rm=TRUE)}
        # flag for discrepancy with earlier age 
        G2.master[G2.master$target==s,]$can.discr.flag<-(max(floor(c(tmp[!is.na(tmp$mar4) & tmp$mar4==20,"dd.age"],0)))>G2.master[G2.master$target==s,]$age_can_first)
      }
      #infer form mar2
      tf<-tmp[tmp$mar2>0,]
      if(length(na.omit(tf$mar2))>0){
        G2.master[G2.master$target==s,]$age_can_est<-floor(min(tf$dd.age,na.rm=TRUE))}
    
  }
}


### add overall age estimates and additional drink age flags

# infer missing ages with maxamt 
G2.master$age_drink_final<-G2.master$age_drink_first
G2.master[G2.master$age_drink_min==20 & !is.na(G2.master$age_drink_min),]$age_drink_final<-20

G2.master$age_drunk_final<-G2.master$age_drunk_first
G2.master[G2.master$age_drunk_min==20 & !is.na(G2.master$age_drunk_min),]$age_drunk_final<-20


tmp<-G2.master[is.na(G2.master$age_drink_min) & G2.master$age_drink_est<20 & !is.na(G2.master$age_drink_est),]$age_drink_est
G2.master[is.na(G2.master$age_drink_min) & G2.master$age_drink_est<20 & !is.na(G2.master$age_drink_est),]$age_drink_final<-tmp
G2.master[is.na(G2.master$age_drink_min) & G2.master$age_drink_est<20 & !is.na(G2.master$age_drink_est),]$can.est.flag<-TRUE

tmp<-G2.master[G2.master$age_drink_min==20 & !is.na(G2.master$age_drink_min) & G2.master$age_drink_est<20 & !is.na(G2.master$age_drink_est),]$age_drink_est
G2.master[G2.master$age_drink_min==20 & !is.na(G2.master$age_drink_min) & G2.master$age_drink_est<20 & !is.na(G2.master$age_drink_est),]$age_drink_final<-tmp
G2.master[G2.master$age_drink_min==20 & !is.na(G2.master$age_drink_min) & G2.master$age_drink_est<20 & !is.na(G2.master$age_drink_est),]$drink.maxamt.flag<-TRUE

G2.master[!is.na(G2.master$age_drink_final) & is.na(G2.master$drink.maxamt.flag),]$drink.maxamt.flag<-FALSE

# implausible ages (<10) flag
G2.master$drink.age.flag<-G2.master$age_drink_first<10 & !is.na(G2.master$age_drink_first)
G2.master$drunk.age.flag<-G2.master$age_drunk_first<10 & !is.na(G2.master$age_drunk_first)


### add overall age estimates and additional cannabis age flags

# infer missing ages with mar2
G2.master$age_can_final<-G2.master$age_can_first
G2.master[G2.master$age_can_min==20 & !is.na(G2.master$age_can_min),]$age_can_final<-20

tmp<-G2.master[is.na(G2.master$age_can_min) & G2.master$age_can_est<20 & !is.na(G2.master$age_can_est),]$age_can_est
G2.master[is.na(G2.master$age_can_min) & G2.master$age_can_est<20 & !is.na(G2.master$age_can_est),]$age_can_final<-tmp
G2.master[is.na(G2.master$age_can_min) & G2.master$age_can_est<20 & !is.na(G2.master$age_can_est),]$can.est.flag<-TRUE

tmp<-G2.master[G2.master$age_can_min==20 & !is.na(G2.master$age_can_min) & G2.master$age_can_est<20 & !is.na(G2.master$age_can_est),]$age_can_est
G2.master[G2.master$age_can_min==20 & !is.na(G2.master$age_can_min) & G2.master$age_can_est<20 & !is.na(G2.master$age_can_est),]$age_can_final<-tmp
G2.master[G2.master$age_can_min==20 & !is.na(G2.master$age_can_min) & G2.master$age_can_est<20 & !is.na(G2.master$age_can_est),]$can.est.flag<-TRUE

G2.master[!is.na(G2.master$age_can_final) & is.na(G2.master$can.est.flag),]$can.est.flag<-FALSE

# implausible ages (<10) flag
G2.master$can.age.flag<-G2.master$age_can_first<10 & !is.na(G2.master$age_can_first)

##############################################################
#### Parental monitoring #####################################
##############################################################

### youth (including annual)

pmC_G2.s1<-comp.sample("Parent Monitoring G2 T",sample=1,scored=TRUE)
pmC_G2.s2<-comp.sample("Parent Monitoring G2 II T",sample=2,scored=TRUE)
pmC_G2.s3<-comp.sample("Parent Monitoring G2 III T",sample=3,scored=TRUE)

# ANNUAL data file with relevant G2 (adolescent) demographics

pmC_Annual_G2.s1<-comp.sample("Parent Monitoring G2 A",sample=1,waves = c(1:26),type = "A",scored=TRUE)
pmC_Annual_G2.s2<-comp.sample("Parent Monitoring G2 II",sample=2,waves = c(1:26),type = "A",scored=TRUE)
pmC_Annual_G2.s3<-comp.sample("Parent Monitoring G2 III",sample=3,waves = c(1:26),type = "A",scored=TRUE)

pmC.long<-do.call("rbind",
                  lapply(pmC_G2.s1[4:5],function(x){x[,c("target","tdate","age","pmself1","pmself2")]}))
pmC.long<-rbind(pmC.long,
                do.call("rbind",
                        lapply(pmC_G2.s2[4:5],function(x){x[,c("target","tdate","age","pmself1","pmself2")]})) )
pmC.long<-rbind(pmC.long,
                do.call("rbind",
                        lapply(pmC_G2.s3[3:5],function(x){x[,c("target","tdate","age","pmself1","pmself2")]})) )


pmCA.long<-do.call("rbind",
                   lapply(pmC_Annual_G2.s1[1:8],function(x){x[,c("target","tdate","age","pmself1","pmself2")]}))
pmCA.long<-rbind(pmCA.long,
                 do.call("rbind",
                         lapply(pmC_Annual_G2.s2[1:7],function(x){x[,c("target","tdate","age","pmself1","pmself2")]})) )
pmCA.long<-rbind(pmCA.long,
                 do.call("rbind",
                         lapply(pmC_Annual_G2.s3[1:7],function(x){x[,c("target","tdate","age","pmself1","pmself2")]})) )

pmC_G2<-rbind(pmC.long,pmCA.long)


### parent (T-waves only)
pmP_G2.s1<-comp.sample("Parent Monitoring G1 on G2 T",sample=1,scored=TRUE)
pmP_G2.s2<-comp.sample("Parent Monitoring G1 on G2 II T",sample=2,scored=TRUE)
pmP_G2.s3<-comp.sample("Parent Monitoring G1 on G2 III T",sample=3,scored=TRUE)

pmP.long<-do.call("rbind",
                  lapply(pmP_G2.s1[4:5],function(x){x[,c("target","tdate","age","pmpar1")]}))
pmP.long<-rbind(pmP.long,
                do.call("rbind",
                        lapply(pmP_G2.s2[4:5],function(x){x[,c("target","tdate","age","pmpar1")]})) )
pmP.long<-rbind(pmP.long,
                do.call("rbind",
                        lapply(pmP_G2.s3[3:5],function(x){x[,c("target","tdate","age","pmpar1")]})) )


# add ages
pmP.long$pmP.age<-NA
for (t in unique(pmP.long$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  pmP.long[pmP.long$target==t,]$pmP.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                    as.Date(pmP.long[pmP.long$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

pmC_G2$pmC.age<-NA
for (t in unique(pmC_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  pmC_G2[pmC_G2$target==t,]$pmC.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                   as.Date(pmC_G2[pmC_G2$target==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

pmC_10_13<-pmC_G2[pmC_G2$pmC.age<14 & pmC_G2$pmC.age>=10 ,]
pmC_14_18<-pmC_G2[pmC_G2$pmC.age<19 & pmC_G2$pmC.age>=14 ,]

G2.master[,"pmC_10_13"]<-NA
G2.master[,"pmC_14_18"]<-NA
G2.master[,"pmP"]<-NA

for (s in unique(G2.master$target)){
  if(length(pmC_10_13[pmC_10_13$target==s,]$target)>0){
      G2.master[G2.master$target==s,"pmC_10_13"]<-mean(pmC_10_13[pmC_10_13$target==s,"pmself1"],na.rm=TRUE)}
  if(length(pmC_14_18[pmC_14_18$target==s,]$target)>0){
      G2.master[G2.master$target==s,"pmC_14_18"]<-mean(pmC_14_18[pmC_14_18$target==s,"pmself1"],na.rm=TRUE)}
    if(length(pmP.long[pmP.long$target==s,]$target)>0){
      G2.master[G2.master$target==s,"pmP"]<-mean(pmP.long[pmP.long$target==s,"pmpar1"],na.rm=TRUE)}
}

########################################################
# Family environment (MOOS), child and adult report ####
########################################################

MOOS_G2.s1<-comp.sample("MOOS G2 T",sample=1,scored=TRUE)
MOOS_G2.s2<-comp.sample("MOOS G2 II T",sample=2,scored=TRUE)
MOOS_G2.s3<-comp.sample("MOOS G2 III T",sample=3,scored=TRUE)

MOOS_G1.s1<-comp.sample("MOOS G1 T",sample=1,scored=TRUE)
MOOS_G1.s2<-comp.sample("MOOS G1 II T",sample=2,scored=TRUE)
MOOS_G1.s3<-comp.sample("MOOS G1 III T",sample=3,scored=TRUE)


# take all MOOS subscales but exclude individuals with missing>0
moos_i<-c("missing","cohechl","expschl","confchl","indechl" ,
             "achichl", "intechl", "actichl", "morachl", "orgnchl", "cntlchl" )

MOOS_G2<-do.call("rbind",
                  lapply(MOOS_G2.s1[3:13],function(x){x[,c("rater","target","tdate","age",moos_i)]}))
MOOS_G2<-rbind(MOOS_G2,
                do.call("rbind",
                        lapply(MOOS_G2.s2[3:8],function(x){x[,c("rater","target","tdate","age",moos_i)]})) )

# age is missing in a subset for s3
MOOS_G2.s3[[6]]$age<-NA
MOOS_G2.s3[[7]]$age<-NA

MOOS_G2<-rbind(MOOS_G2,
               do.call("rbind",
                       lapply(MOOS_G2.s3[3:7],function(x){x[,c("rater","target","tdate","age",moos_i)]})) )

moos_p<-c("missing","cohepar","expspar","confpar","indepar" ,
          "achipar", "intepar", "actipar", "morapar", "orgnpar", "cntlpar")

MOOS_G1<-do.call("rbind",
                 lapply(MOOS_G1.s1[1:11],function(x){x[,c("family","target","tdate","age",moos_p)]}))
MOOS_G1<-rbind(MOOS_G1,
               do.call("rbind",
                       lapply(MOOS_G1.s2[1:7],function(x){x[,c("family","target","tdate","age",moos_p)]})) )
MOOS_G1<-rbind(MOOS_G1,
               do.call("rbind",
                       lapply(MOOS_G1.s3[2:6],function(x){x[,c("family","target","tdate","age",moos_p)]})) )

# eliminate entries with missing values

MOOS_G1<-MOOS_G1[MOOS_G1$missing==0,]

MOOS_G2<-MOOS_G2[MOOS_G2$missing==0,]

# add ages for G2 ratings

MOOS_G2$MOOS.age<-NA
for (t in unique(MOOS_G2$rater)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  MOOS_G2[MOOS_G2$rater==t,]$MOOS.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                       as.Date(MOOS_G2[MOOS_G2$rater==t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}


# for G2 ratings, break into adolescent developmental periods
hist(MOOS_G2$age,xlim=c(0,30),breaks="fd")

MOOS_G2_10_13<-MOOS_G2[MOOS_G2$MOOS.age<14 & MOOS_G2$MOOS.age>=10 ,]
MOOS_G2_14_18<-MOOS_G2[MOOS_G2$MOOS.age<19 & MOOS_G2$MOOS.age>=14 ,]

# average scores

G2.master[,paste0(moos_i,"_10_13")]<-NA
G2.master[,paste0(moos_i,"_14_18")]<-NA

for (s in unique(G2.master$target)){
  
  if(length(MOOS_G2_10_13[MOOS_G2_10_13$rater==s,]$rater)>0){
    tmp<-MOOS_G2_10_13[MOOS_G2_10_13$rater==s,]
    for (c in moos_i){
      G2.master[G2.master$target==s,paste0(c,"_10_13")]<-mean(tmp[,c],na.rm=TRUE)}
  }
  
  if(length(MOOS_G2_14_18[MOOS_G2_14_18$rater==s,]$rater)>0){
    tmp<-MOOS_G2_14_18[MOOS_G2_14_18$rater==s,]
    for (c in moos_i){
      G2.master[G2.master$target==s,paste0(c,"_14_18")]<-mean(tmp[,c],na.rm=TRUE)}
  }
  
}


# For G1 ratings, make similar structure to income

# merge with one timepoint for each child and G1 assessment timepoint
G2.fam.MOOS<-merge(MOOS_G1[,colnames(MOOS_G1)!="target"],
              G2.master[,c("target","family","dob")],
              all.x = TRUE)

G2.fam.MOOS$tdate<-as.Date(G2.fam.MOOS$tdate/86400,origin = "1582-10-14")

# lock G1 measurement to child age
G2.fam.MOOS$g2.age<-interval(G2.fam.MOOS$dob, G2.fam.MOOS$tdate) / 
  duration(num = 1, units = "years")

# qualitative labels for developmental periods
G2.fam.MOOS$devp<-NA
G2.fam.MOOS[G2.fam.MOOS$g2.age<0 & !is.na(G2.fam.MOOS$g2.age),]$devp<-"not born yet"
G2.fam.MOOS[G2.fam.MOOS$g2.age>=0 & G2.fam.MOOS$g2.age<10 & !is.na(G2.fam.MOOS$g2.age),]$devp<-"childhood"
G2.fam.MOOS[G2.fam.MOOS$g2.age>=10 & G2.fam.MOOS$g2.age<19 & !is.na(G2.fam.MOOS$g2.age),]$devp<-"adolescence"
G2.fam.MOOS[G2.fam.MOOS$g2.age>=19 & !is.na(G2.fam.MOOS$g2.age),]$devp<-"adulthood"

G2.master[,paste0(moos_p[2:11],"_childhood")]<-NA
G2.master[,paste0(moos_p[2:11],"_adolescence")]<-NA

for(s in unique(G2.master$target)){
  if(length(G2.fam.MOOS[G2.fam.MOOS$target==s & 
                        G2.fam.MOOS$devp%in%c("childhood") & 
                   !is.na(G2.fam.MOOS$cohepar),"target"])>0){
    tmp<-G2.fam.MOOS[G2.fam.MOOS$target==s & 
                       G2.fam.MOOS$devp%in%c("childhood") & 
                       !is.na(G2.fam.MOOS$cohepar),moos_p[2:11]]
    G2.master[G2.master$target==s,paste0(moos_p[2:11],"_childhood")]<-colMeans(tmp)
  }
  if(length(G2.fam.MOOS[G2.fam.MOOS$target==s & 
                        G2.fam.MOOS$devp%in%c("adolescence") & 
                        !is.na(G2.fam.MOOS$cohepar),"target"])>0){
    tmp<-G2.fam.MOOS[G2.fam.MOOS$target==s & 
                       G2.fam.MOOS$devp%in%c("adolescence") & 
                       !is.na(G2.fam.MOOS$cohepar),moos_p[2:11]]
    G2.master[G2.master$target==s,paste0(moos_p[2:11],"_adolescence")]<-colMeans(tmp)
  }
}


#############################################
### Parental Drinking and Drug History ######
#############################################

# read in

dd_G1.s1<-comp.sample("Drinking & Other Drug Use G1 ",sample=1,scored=TRUE)
dd_G1.s2<-comp.sample("Drinking & Other Drug Use G1 ",sample=2,scored=TRUE)
dd_G1.s3<-comp.sample("Drinking & Other Drug Use G1 ",sample=3,scored=TRUE)

# fix inconsistent naming
dd_G1.s1[[7]]<-comp.sample("Drinking & Other Drug Use G1 T7 Form B scored.sav",sample=1,scored=TRUE)[[7]]

# columns fo interest

ddp.c<-c("family","rmember","rstep","target","tdate",
         "dog1a","dog1b","dog1c",#cannabis
         "dof1", "dof2","dof3", # cigarettes
         "doa1", # days per month
         "doa2", # drinks per day
         "binge","bingedk")

# rename inconsistently names variables at T1

dd_G1.s1[[1]]$dog1a<-dd_G1.s1[[1]]$doe3a
dd_G1.s1[[1]]$dog1b<-dd_G1.s1[[1]]$doe3b
dd_G1.s1[[1]]$dog1c<-dd_G1.s1[[1]]$doe3c
dd_G1.s1[[1]]$dof1<-dd_G1.s1[[1]]$dod1a
dd_G1.s1[[1]]$dof2<-dd_G1.s1[[1]]$dod1b
dd_G1.s1[[1]]$dof3<-dd_G1.s1[[1]]$dod2

dd_G1.s2[[1]]$dog1a<-dd_G1.s2[[1]]$doe3a98
dd_G1.s2[[1]]$dog1b<-dd_G1.s2[[1]]$doe3b98
dd_G1.s2[[1]]$dog1c<-dd_G1.s2[[1]]$doe3c98
dd_G1.s2[[1]]$dof1<-dd_G1.s2[[1]]$dod1a
dd_G1.s2[[1]]$dof2<-dd_G1.s2[[1]]$dod1b
dd_G1.s2[[1]]$dof3<-dd_G1.s2[[1]]$dod2


dd_G1<-do.call("rbind",
                 lapply(dd_G1.s1[1:10],function(x){x[,c(ddp.c)]}))
dd_G1<-rbind(dd_G1,
               do.call("rbind",
                       lapply(dd_G1.s2[1:7],function(x){x[,c(ddp.c)]})) )
dd_G1<-rbind(dd_G1,
               do.call("rbind",
                       lapply(dd_G1.s3[2:6],function(x){x[,c(ddp.c)]})) )

# summary variables

# cannabis
dd_G1[dd_G1$dog1a%in%c("NA"," ",paste0(" _duplicated_",c(1:6,8,9))),]$dog1a<-NA
dd_G1[dd_G1$dog1a%in%c("0. 0 occasions", "Never", "0. Never"),]$dog1a<-0
dd_G1[!dd_G1$dog1a%in%c(0) & !is.na(dd_G1$dog1a),]$dog1a<-1
dd_G1$dog1a<-as.numeric(dd_G1$dog1a)
dd_G1[dd_G1$dog1b%in%c("NA"," ","88",paste0(" _duplicated_",c(1:6,8,9,88))),]$dog1b<-NA
dd_G1[dd_G1$dog1b%in%c("0. 0 occasions", "Never", "0. Never"),]$dog1b<-0
dd_G1[dd_G1$dog1b%in%c("8. More than 1000 occasions"),]$dog1b<-1000
dd_G1[dd_G1$dog1b%in%c("7. 100-1000 occasions"),]$dog1b<-550
dd_G1[dd_G1$dog1b%in%c("9. 500 or more times","500 and above"),]$dog1b<-500
dd_G1[dd_G1$dog1b%in%c("8. 250-499 times","250-499 occasions"),]$dog1b<-374.5
dd_G1[dd_G1$dog1b%in%c("7. 100-249 times","100-249 occasions"),]$dog1b<-174.5
dd_G1[dd_G1$dog1b%in%c("6. 40-99 times","40-99 occasions","6. 40-99 occasions"),]$dog1b<-69.5
dd_G1[dd_G1$dog1b%in%c("5. 20-39 times","20-39 occasions","5. 20-39 occasions"),]$dog1b<-29.5
dd_G1[dd_G1$dog1b%in%c("4. 10-19 times","10-19 occasions","4. 10-19 occasions"),]$dog1b<-14.5
dd_G1[dd_G1$dog1b%in%c("3. 6-9 occasions","6-9 occasions","3. 6-9 times"),]$dog1b<-7.5
dd_G1[dd_G1$dog1b%in%c("2. 3-5 occasions","3-5 occasions","2. 3-5 times"),]$dog1b<-4
dd_G1[dd_G1$dog1b%in%c("1. 1-2 times" ,"1-2 occasions","1. 1-2 occasions"),]$dog1b<-1.5
dd_G1$dog1b<-as.numeric(dd_G1$dog1b)
dd_G1[dd_G1$dog1c%in%c("NA"," ","88",paste0(" _duplicated_",c(1:9,88))),]$dog1c<-NA
dd_G1[dd_G1$dog1c%in%c("0. 0 occasions", "Never", "0. Never"),]$dog1c<-0
dd_G1[dd_G1$dog1c%in%c("7. 100-1000 occasions"),]$dog1c<-550
dd_G1[dd_G1$dog1c%in%c("9. 500 or more times","500 and above"),]$dog1c<-500
#dd_G1[dd_G1$dog1c%in%c("8. 250-499 times","250-499 occasions"),]$dog1c<-374.5
dd_G1[dd_G1$dog1c%in%c("7. 100-249 times","100-249 occasions"),]$dog1c<-174.5
dd_G1[dd_G1$dog1c%in%c("6. 40-99 times","40-99 occasions","6. 40-99 occasions"),]$dog1c<-69.5
dd_G1[dd_G1$dog1c%in%c("5. 20-39 times","20-39 occasions","5. 20-39 occasions"),]$dog1c<-29.5
dd_G1[dd_G1$dog1c%in%c("4. 10-19 times","10-19 occasions","4. 10-19 occasions"),]$dog1c<-14.5
dd_G1[dd_G1$dog1c%in%c("3. 6-9 occasions","6-9 occasions","3. 6-9 times"),]$dog1c<-7.5
dd_G1[dd_G1$dog1c%in%c("2. 3-5 occasions","3-5 occasions","2. 3-5 times"),]$dog1c<-4
dd_G1[dd_G1$dog1c%in%c("1. 1-2 times" ,"1-2 occasions","1. 1-2 occasions"),]$dog1c<-1.5
dd_G1$dog1c<-as.numeric(dd_G1$dog1c)
# winsorize at 500 to equate yearly cannabis scales
dd_G1[dd_G1$dog1b%in%c(550,1000),]$dog1b<-500
# winsorize at 100 (>3x daily and the lower bound of the lowest option) for monthly
dd_G1[dd_G1$dog1c%in%c(174.5,374.5,500,550,1000),]$dog1c<-100
# make sure 0s aren't incorrectly coded as missing:
dd_G1[dd_G1$dog1a%in%c(0),c("dog1b","dog1c")]<-0
dd_G1[dd_G1$dog1b%in%c(0),]$dog1c<-0
dd_G1$can_monthlyA<-dd_G1$dog1c
dd_G1$can_monthlyB<-dd_G1$dog1b/12
dd_G1$can_monthly_est<-rowMeans(dd_G1[,c("can_monthlyA","can_monthlyB")])

# cigarettes
dd_G1[dd_G1$dof1%in%c(" ","NA",paste0(" _duplicated_",c(1:9,88))),"dof1"]<-NA
dd_G1[dd_G1$dof1%in%c("1. never", "1. Never", "Never (Go to section G)","0. Never",
                      "Never (go to section G)", "Never (go to section G on page 12)"),"dof1"]<-0
dd_G1[!is.na(dd_G1$dof1) & dd_G1$dof1!=0,"dof1"]<-1
dd_G1[dd_G1$dof2%in%c(" ","NA",paste0(" _duplicated_",c(1:9,88)),
                      "8. N/A (answered never to F1)","8. N/A (answered NEVER to DOF1)",
                      "8. N/A (DOF1 was 0 for NEVER)"),"dof2"]<-NA
dd_G1[dd_G1$dof2%in%c("1. never", "1. Never", "0. Never","Never"),"dof2"]<-0
dd_G1[!is.na(dd_G1$dof2) & dd_G1$dof2!=0,"dof2"]<-1
dd_G1[dd_G1$dof3%in%c(" ","NA"," 9. missing",paste0(" _duplicated_",c(1:9,88))),"dof3"]<-NA
dd_G1[dd_G1$dof3%in%c("1. not at all", "8. N/A (answered never to F1 and/or F2)", 
                      "8. N/A" ,"Not at all","0","0. Not at all"),"dof3"]<-0
dd_G1[dd_G1$dof3%in%c("1. Less than one per day","2. less than one cigarette per day",
                          "2. less than 1 per day","Less than one per day"),]$dof3<-.5
dd_G1[dd_G1$dof3%in%c("3. one to five cigarettes per day","3. 1-5 per day",
                      "2. One to five cigarettes per day"  ,"One to five cigarettes per day"),]$dof3<-3
dd_G1[dd_G1$dof3%in%c("4. about one-half pack per day","4. 1/2 pack per day",
                      "3. About one half pack per day","About one half pack per day"),]$dof3<-10
dd_G1[dd_G1$dof3%in%c("5. about one pack per day" ,"5. 1 pack per day" ,
                      "4. About one pack per day" ,"About one pack per day"),]$dof3<-20
dd_G1[dd_G1$dof3%in%c("6. about one and one-half packs per day" ,"6. 1-1/2 packs per day" ,
                      "5. About one and one-half packs per day" ,"About one and one-half packs per day"),]$dof3<-30
dd_G1[dd_G1$dof3%in%c("7. two packs or more per day"  ,"7. 2+ packs per day"  ,
                      "6. Two packs or more per day"  ,"Two packs or more per day"),]$dof3<-40
# make sure 0s aren't incorrectly coded as missing:
dd_G1[dd_G1$dof1%in%c(0),c("dof3")]<-0
dd_G1[dd_G1$dof2%in%c(0),c("dof3")]<-0
dd_G1$dof3<-as.numeric(dd_G1$dof3)

dd_G1[dd_G1$doa1%in%c("na"),]$doa1<-NA
dd_G1[dd_G1$doa1%in%c("88. N/A (did not drink during 6 mo. period)","88. N/A (did not drink)","88"),]$doa1<-0
dd_G1$doa1<-as.numeric(dd_G1$doa1)

dd_G1[dd_G1$doa2%in%c("na","N/A"),]$doa2<-NA
dd_G1[dd_G1$doa2%in%c("88. N/A (did not drink during period in question--6 months)","88. N/A (did not drink during 6 mo. period)","88"),]$doa2<-0
dd_G1[dd_G1$doa2%in%c("0.5"),]$doa2<-1
dd_G1$doa2<-as.numeric(dd_G1$doa2)

dd_G1[dd_G1$binge%in%c("0.no"),]$binge<-0
dd_G1[dd_G1$binge%in%c("1.yes"),]$binge<-1
dd_G1$binge<-as.numeric(dd_G1$binge)

dd_G1$bingedk<-as.numeric(dd_G1$bingedk)

# For G1 ratings, make similar structure to income

# only bio parents
dd_G1.bio<-dd_G1[dd_G1$rstep%in%c(1),]

# merge with one timepoint for each child and G1 assessment timepoint
G2.fam.dd<-merge(dd_G1.bio[,colnames(dd_G1.bio)!="target"],
                   G2.master[,c("target","family","dob")],
                   all.x = TRUE)

G2.fam.dd$tdate<-as.Date(as.numeric(G2.fam.dd$tdate)/86400,origin = "1582-10-14")


# lock G1 measurement to child age
G2.fam.dd$g2.age<-interval(G2.fam.dd$dob, G2.fam.dd$tdate) / 
  duration(num = 1, units = "years")

# qualitative labels for developmental periods
G2.fam.dd$devp<-NA
G2.fam.dd[G2.fam.dd$g2.age<0 & !is.na(G2.fam.dd$g2.age),]$devp<-"not born yet"
G2.fam.dd[G2.fam.dd$g2.age>=0 & G2.fam.dd$g2.age<10 & !is.na(G2.fam.dd$g2.age),]$devp<-"childhood"
G2.fam.dd[G2.fam.dd$g2.age>=10 & G2.fam.dd$g2.age<19 & !is.na(G2.fam.dd$g2.age),]$devp<-"adolescence"
G2.fam.dd[G2.fam.dd$g2.age>=19 & !is.na(G2.fam.dd$g2.age),]$devp<-"adulthood"

# sum cols
g1.dd.c<-c("can_est_","cig_","drink_days_m_","drinks_day_","binge_","bingedk_")

G2.master[,paste0(g1.dd.c,"mom_childhood")]<-NA
G2.master[,paste0(g1.dd.c,"mom_adolescence")]<-NA
G2.master[,paste0(g1.dd.c,"dad_childhood")]<-NA
G2.master[,paste0(g1.dd.c,"dad_adolescence")]<-NA


for(s in unique(G2.master$target)){

  if(length(G2.fam.dd[G2.fam.dd$target==s & 
                      G2.fam.dd$devp%in%c("childhood") & 
                      G2.fam.dd$rmember%in%c(1),"target"])>0){
    tmp<-G2.fam.dd[G2.fam.dd$target==s & 
                     G2.fam.dd$devp%in%c("childhood") & 
                     G2.fam.dd$rmember%in%c(1),]
    G2.master[G2.master$target==s,"can_est_mom_childhood"]<-mean(tmp$can_monthly_est,na.rm=TRUE)
    G2.master[G2.master$target==s,"cig_mom_childhood"]<-mean(tmp$dof3,na.rm=TRUE)
    G2.master[G2.master$target==s,"drink_days_m_mom_childhood"]<-mean(tmp$doa1,na.rm=TRUE)
    G2.master[G2.master$target==s,"drinks_day_mom_childhood"]<-mean(tmp$doa2,na.rm=TRUE)
    G2.master[G2.master$target==s,"binge_mom_childhood"]<-mean(tmp$binge,na.rm=TRUE)>0
    G2.master[G2.master$target==s,"bingedk_mom_childhood"]<-mean(tmp$bingedk,na.rm=TRUE)
  }
  
  if(length(G2.fam.dd[G2.fam.dd$target==s & 
                      G2.fam.dd$devp%in%c("adolescence") & 
                      G2.fam.dd$rmember%in%c(1),"target"])>0){
    tmp<-G2.fam.dd[G2.fam.dd$target==s & 
                     G2.fam.dd$devp%in%c("adolescence") & 
                     G2.fam.dd$rmember%in%c(1),]
    G2.master[G2.master$target==s,"can_est_mom_adolescence"]<-mean(tmp$can_monthly_est,na.rm=TRUE)
    G2.master[G2.master$target==s,"cig_mom_adolescence"]<-mean(tmp$dof3,na.rm=TRUE)
    G2.master[G2.master$target==s,"drink_days_m_mom_adolescence"]<-mean(tmp$doa1,na.rm=TRUE)
    G2.master[G2.master$target==s,"drinks_day_mom_adolescence"]<-mean(tmp$doa2,na.rm=TRUE)
    G2.master[G2.master$target==s,"binge_mom_adolescence"]<-mean(tmp$binge,na.rm=TRUE)>0
    G2.master[G2.master$target==s,"bingedk_mom_adolescence"]<-mean(tmp$bingedk,na.rm=TRUE)
  }
  
  if(length(G2.fam.dd[G2.fam.dd$target==s & 
                      G2.fam.dd$devp%in%c("childhood") & 
                      G2.fam.dd$rmember%in%c(2),"target"])>0){
    tmp<-G2.fam.dd[G2.fam.dd$target==s & 
                     G2.fam.dd$devp%in%c("childhood") & 
                     G2.fam.dd$rmember%in%c(2),]
    G2.master[G2.master$target==s,"can_est_dad_childhood"]<-mean(tmp$can_monthly_est,na.rm=TRUE)
    G2.master[G2.master$target==s,"cig_dad_childhood"]<-mean(tmp$dof3,na.rm=TRUE)
    G2.master[G2.master$target==s,"drink_days_m_dad_childhood"]<-mean(tmp$doa1,na.rm=TRUE)
    G2.master[G2.master$target==s,"drinks_day_dad_childhood"]<-mean(tmp$doa2,na.rm=TRUE)
    G2.master[G2.master$target==s,"binge_dad_childhood"]<-mean(tmp$binge,na.rm=TRUE)>0
    G2.master[G2.master$target==s,"bingedk_dad_childhood"]<-mean(tmp$bingedk,na.rm=TRUE)
  }
  
  if(length(G2.fam.dd[G2.fam.dd$target==s & 
                      G2.fam.dd$devp%in%c("adolescence") & 
                      G2.fam.dd$rmember%in%c(2),"target"])>0){
    tmp<-G2.fam.dd[G2.fam.dd$target==s & 
                     G2.fam.dd$devp%in%c("adolescence") & 
                     G2.fam.dd$rmember%in%c(2),]
    G2.master[G2.master$target==s,"can_est_dad_adolescence"]<-mean(tmp$can_monthly_est,na.rm=TRUE)
    G2.master[G2.master$target==s,"cig_dad_adolescence"]<-mean(tmp$dof3,na.rm=TRUE)
    G2.master[G2.master$target==s,"drink_days_m_dad_adolescence"]<-mean(tmp$doa1,na.rm=TRUE)
    G2.master[G2.master$target==s,"drinks_day_dad_adolescence"]<-mean(tmp$doa2,na.rm=TRUE)
    G2.master[G2.master$target==s,"binge_dad_adolescence"]<-mean(tmp$binge,na.rm=TRUE)>0
    G2.master[G2.master$target==s,"bingedk_dad_adolescence"]<-mean(tmp$bingedk,na.rm=TRUE)
  }
  

}

#############################################
### Hassles and Uplifts #####################
#############################################

# read in

HU_G1.s1<-comp.sample("Hassles and Uplifts G1 ",sample=1,scored=FALSE)
HU_G1.s2<-comp.sample("Hassles and Uplifts G1 ",sample=2,scored=FALSE)
HU_G1.s3<-comp.sample("Hassles and Uplifts G1 ",sample=3,scored=FALSE)

HU.new.cols<-c("target","tdate","age","family",paste0("h",1:35),paste0("u",1:35))

h.old<-c(5,6,7,10,17,18,19,26,27,30,35,42,48,51,52,56,67,71,74,76,79,83,85,90,94,99,102,
         103,108,111,112,113,114,115,116)
names(h.old)<-paste0("h",1:35)

u.old<-c(2,6,11,17,18,21,24,25,26,31,37,40,42,43,48,49,50,51,55,59,60,67,71,73,77,81,84,
         85,87,91,106,109,114,120,132)
names(u.old)<-paste0("u",1:35)

HU.old.cols<-c("target","tdate","age","family",paste0("h",h.old),paste0("u",u.old))

# compile old and re-label 

HU_G1<-do.call("rbind",
               lapply(HU_G1.s1[1:6],function(x){x[,c(HU.old.cols)]}))
HU_G1<-rbind(HU_G1,
             do.call("rbind",
                     lapply(HU_G1.s2[c(1,5:6)],function(x){x[,c(HU.old.cols)]})) )
HU_G1<-rbind(HU_G1,
             do.call("rbind",
                     lapply(HU_G1.s3[6],function(x){x[,c(HU.old.cols)]})) )

colnames(HU_G1)<-c("target","tdate","age","family",names(h.old),names(u.old))

# add new

HU_G1<-rbind(HU_G1,
             do.call("rbind",
                     lapply(HU_G1.s1[c(7:10)],function(x){x[,c(HU.new.cols)]})) )
HU_G1<-rbind(HU_G1,
             do.call("rbind",
                     lapply(HU_G1.s2[c(2:4,7)],function(x){x[,c(HU.new.cols)]})) )
HU_G1<-rbind(HU_G1,
             do.call("rbind",
                     lapply(HU_G1.s3[2:5],function(x){x[,c(HU.new.cols)]})) )

# re-label column entries 

unique(unlist(c(HU_G1[,HU.new.cols[5:73]])))

r0<-c("0. not selected","Not selected","0. Not selected","not selected","not often"  )
r1<-c("1. somewhat severe","Somewhat severe","1. Somewhat severe","somewhat severe","somewhat often" )
r2<-c("2. moderately severe","Moderately severe","2. Moderately severe","moderately severe","moderately often" )
r3<-c("3. extremely severe","Extremely severe","3. Extremely severe","extremely severe","extremely often"  )

for (c in HU.new.cols[4:74]){
  HU_G1[HU_G1[,c]%in%r0,c]<-0
  HU_G1[HU_G1[,c]%in%r1,c]<-1
  HU_G1[HU_G1[,c]%in%r2,c]<-2
  HU_G1[HU_G1[,c]%in%r3,c]<-3
  HU_G1[,c]<-as.numeric(HU_G1[,c])
}

HU_G1$hassles<-rowSums(HU_G1[,paste0("h",1:35)])
HU_G1$uplifts<-rowSums(HU_G1[,paste0("u",1:35)])


# For G1 ratings, make similar structure to income

# merge with one timepoint for each child and G1 assessment timepoint
G2.fam.HU<-merge(HU_G1[,colnames(HU_G1)!="target"],
                   G2.master[,c("target","family","dob")],
                   all.x = TRUE)

G2.fam.HU$tdate<-as.Date(as.numeric(G2.fam.HU$tdate)/86400,origin = "1582-10-14")

# lock G1 measurement to child age
G2.fam.HU$g2.age<-interval(G2.fam.HU$dob, G2.fam.HU$tdate) / 
  duration(num = 1, units = "years")

# qualitative labels for developmental periods
G2.fam.HU$devp<-NA
G2.fam.HU[G2.fam.HU$g2.age<0 & !is.na(G2.fam.HU$g2.age),]$devp<-"not born yet"
G2.fam.HU[G2.fam.HU$g2.age>=0 & G2.fam.HU$g2.age<10 & !is.na(G2.fam.HU$g2.age),]$devp<-"childhood"
G2.fam.HU[G2.fam.HU$g2.age>=10 & G2.fam.HU$g2.age<19 & !is.na(G2.fam.HU$g2.age),]$devp<-"adolescence"
G2.fam.HU[G2.fam.HU$g2.age>=19 & !is.na(G2.fam.HU$g2.age),]$devp<-"adulthood"

G2.master[,c("hassles_childhood","uplifts_childhood",
             "hassles_adolescence","uplifts_adolescence")]<-NA


for(s in unique(G2.master$target)){
  if(length(G2.fam.HU[G2.fam.HU$target==s & 
                      G2.fam.HU$devp%in%c("childhood") & 
                        !is.na(G2.fam.HU$hassles),"target"])>0){
    tmp<-G2.fam.HU[G2.fam.HU$target==s & 
                     G2.fam.HU$devp%in%c("childhood") & 
                     !is.na(G2.fam.HU$hassles),]
    G2.master[G2.master$target==s,"hassles_childhood"]<-mean(tmp$hassles,na.rm=TRUE)
    G2.master[G2.master$target==s,"uplifts_childhood"]<-mean(tmp$uplifts,na.rm=TRUE)
  }
  if(length(G2.fam.HU[G2.fam.HU$target==s & 
                      G2.fam.HU$devp%in%c("adolescence") & 
                      !is.na(G2.fam.HU$hassles),"target"])>0){
    tmp<-G2.fam.HU[G2.fam.HU$target==s & 
                     G2.fam.HU$devp%in%c("adolescence") & 
                     !is.na(G2.fam.HU$hassles),]
    G2.master[G2.master$target==s,"hassles_adolescence"]<-mean(tmp$hassles,na.rm=TRUE)
    G2.master[G2.master$target==s,"uplifts_adolescence"]<-mean(tmp$uplifts,na.rm=TRUE)
  }
}


#######################################
#### Adverse Childhood Experiences ####
#######################################


### 1. Family Crisis List

FC_G1.s1<-comp.sample("Family Crisis List G1 T",sample=1,scored=TRUE)
FC_G1.s2<-comp.sample("Family Crisis List G1 II T",sample=2,scored=TRUE)
FC_G1.s3<-comp.sample("Family Crisis List G1 III T",sample=3,scored=TRUE)

FC.c<-c("target","tdate","family",
        "housc2a","housc7a","heal2i","econc7a","econc3a")


FC_G1<-rbind(do.call("rbind",
                     lapply(FC_G1.s1[c(1:11)],function(x){x[,c(FC.c)]})) )
FC_G1<-rbind(FC_G1,
             do.call("rbind",
                     lapply(FC_G1.s2[c(1:7)],function(x){x[,c(FC.c)]})) )
FC_G1<-rbind(FC_G1,
             do.call("rbind",
                     lapply(FC_G1.s3[2:6],function(x){x[,c(FC.c)]})) )

# re-label column entries 

unique(unlist(c(FC_G1[,FC.c[4:8]])))

r0<-c("0. no, not circled","not checked","0=not checked")
r1<-c("1.yes, circled","1. yes, circled","checked","1=checked")

for (c in FC.c[4:8]){
  FC_G1[,c]<-as.character(FC_G1[,c])
  FC_G1[FC_G1[,c]%in%r0,c]<-0
  FC_G1[FC_G1[,c]%in%r1,c]<-1
  FC_G1[,c]<-as.numeric(FC_G1[,c])
}

FC_G1$ACEs<-rowSums(FC_G1[,FC.c[4:8]])

# For G1 ratings, make similar structure to income

# merge with one timepoint for each child and G1 assessment timepoint
G2.fam.FC<-merge(FC_G1[,colnames(FC_G1)!="target"],
                 G2.master[,c("target","family","dob")],
                 all.x = TRUE)

G2.fam.FC$tdate<-as.Date(as.numeric(G2.fam.FC$tdate)/86400,origin = "1582-10-14")

# lock G1 measurement to child age
G2.fam.FC$g2.age<-interval(G2.fam.FC$dob, G2.fam.FC$tdate) / 
  duration(num = 1, units = "years")

# qualitative labels for developmental periods
G2.fam.FC$devp<-NA
G2.fam.FC[G2.fam.FC$g2.age<0 & !is.na(G2.fam.FC$g2.age),]$devp<-"not born yet"
G2.fam.FC[G2.fam.FC$g2.age>=0 & G2.fam.FC$g2.age<10 & !is.na(G2.fam.FC$g2.age),]$devp<-"childhood"
G2.fam.FC[G2.fam.FC$g2.age>=10 & G2.fam.FC$g2.age<19 & !is.na(G2.fam.FC$g2.age),]$devp<-"adolescence"
G2.fam.FC[G2.fam.FC$g2.age>=19 & !is.na(G2.fam.FC$g2.age),]$devp<-"adulthood"



### 2. Conflict Tactics Scale, parent to parent 

Int_G1.s1<-comp.sample("Interactions G1 T",sample=1,scored=TRUE)
Int_G1.s2<-comp.sample("Interactions G1 II T",sample=2,scored=TRUE)
Int_G1.s3<-comp.sample("Interactions G1 III T",sample=3,scored=TRUE)

Int_G1.c<-c("target","tdate","family",
        "v4","iv4", #insult
        "v9","iv9", #threaten to hit or throw
        "v11","iv11", #actually threw
        "v12","iv12") #actually hit

Int_G1<-rbind(do.call("rbind",
                     lapply(Int_G1.s1[c(2:10)],function(x){x[,c(Int_G1.c)]})) )

Int_G1<-rbind(Int_G1,
             do.call("rbind",
                     lapply(Int_G1.s2[c(1:7)],function(x){x[,c(Int_G1.c)]})) )
Int_G1<-rbind(Int_G1,
             do.call("rbind",
                     lapply(Int_G1.s3[2:6],function(x){x[,c(Int_G1.c)]})) )


# inconsistently named and organized files for s1:
CTS.s1<-comp.sample("Conflict Tactics G1 T",sample=1,scored=TRUE)

CTS.s1[[1]]$v4<-CTS.s1[[1]]$ct5d
CTS.s1[[1]]$iv4<-CTS.s1[[1]]$ct6d
CTS.s1[[1]]$v9<-CTS.s1[[1]]$ct5i
CTS.s1[[1]]$iv9<-CTS.s1[[1]]$ct6i
CTS.s1[[1]]$v11<-CTS.s1[[1]]$ct5k
CTS.s1[[1]]$iv11<-CTS.s1[[1]]$ct6k
CTS.s1[[1]]$v12<-CTS.s1[[1]]$ct5l
CTS.s1[[1]]$iv12<-CTS.s1[[1]]$ct6l


CTS.s1[[2]]$v4<-CTS.s1[[2]]$vda
CTS.s1[[2]]$iv4<-CTS.s1[[2]]$ivda
CTS.s1[[2]]$v9<-CTS.s1[[2]]$via
CTS.s1[[2]]$iv9<-CTS.s1[[2]]$ivia
CTS.s1[[2]]$v11<-CTS.s1[[2]]$vka
CTS.s1[[2]]$iv11<-CTS.s1[[2]]$ivka
CTS.s1[[2]]$v12<-CTS.s1[[2]]$vla
CTS.s1[[2]]$iv12<-CTS.s1[[2]]$ivla


Int_G1<-rbind(Int_G1,CTS.s1[[1]][,Int_G1.c])
Int_G1<-rbind(Int_G1,CTS.s1[[2]][,Int_G1.c])

# re-label column entries 

unique(unlist(c(Int_G1[,Int_G1.c[4:11]])))

r0<-c("A = Never","A = NEVER","never","0")

for (c in Int_G1.c[4:11]){
  Int_G1[,c]<-as.character(Int_G1[,c])
  Int_G1[Int_G1[,c]%in%r0,c]<-0
  Int_G1[Int_G1[,c]%in%c("N/A","n/a"),c]<-NA
  Int_G1[Int_G1[,c]!=0 & !is.na(Int_G1[,c]),c]<-1
  Int_G1[,c]<-as.numeric(Int_G1[,c])
}




# For G1 ratings, make similar structure to income

# merge with one timepoint for each child and G1 assessment timepoint
G2.fam.Int_G1<-merge(Int_G1[,colnames(Int_G1)!="target"],
                 G2.master[,c("target","family","dob")],
                 all.x = TRUE)

G2.fam.Int_G1$tdate<-as.Date(as.numeric(G2.fam.Int_G1$tdate)/86400,origin = "1582-10-14")

# lock G1 measurement to child age
G2.fam.Int_G1$g2.age<-interval(G2.fam.Int_G1$dob, G2.fam.Int_G1$tdate) / 
  duration(num = 1, units = "years")


### 2. Conflict Tactics Scale, parent to target child

Int_G1onG2.s1<-comp.sample("Interactions G1 on G2 T",sample=1,scored=TRUE)
Int_G1onG2.s2<-comp.sample("Interactions G1 on G2 II T",sample=2,scored=TRUE)
Int_G1onG2.s3<-comp.sample("Interactions G1 on G2 III T",sample=3,scored=TRUE)

Int_G1onG2.c<-c("target","tdate","family",
            "iii4", #insult
            "iii9", #threaten to hit or throw
            "iii11", #actually threw
            "iii12", #actually hit
            "vioong2") #violence composite


Int_G1onG2<-rbind(do.call("rbind",
                      lapply(Int_G1onG2.s1[c(2:6)],function(x){x[,c(Int_G1onG2.c)]})) )
Int_G1onG2<-rbind(Int_G1onG2,
              do.call("rbind",
                      lapply(Int_G1onG2.s2[c(1:5)],function(x){x[,c(Int_G1onG2.c)]})) )
Int_G1onG2<-rbind(Int_G1onG2,
              do.call("rbind",
                      lapply(Int_G1onG2.s3[c(1,3,5)],function(x){x[,c(Int_G1onG2.c)]})) )


# inconsistently named and organized files for s1:
CTS.s1<-comp.sample("Conflict Tactics G1 T",sample=1,scored=TRUE)


CTS.s1[[1]]$target<-CTS.s1[[1]]$target1
CTS.s1[[1]]$iii4<-CTS.s1[[1]]$ct2d
CTS.s1[[1]]$iii9<-CTS.s1[[1]]$ct2i
CTS.s1[[1]]$iii11<-CTS.s1[[1]]$ct2k
CTS.s1[[1]]$iii12<-CTS.s1[[1]]$ct2l
CTS.s1[[1]]$vioong2<-NA


# several items missing in this one, just forget it
# CTS.s1[[2]]$target<-CTS.s1[[2]]$target1
# CTS.s1[[2]]$iii4<-CTS.s1[[2]]$iiida
# CTS.s1[[2]]$iii9<-CTS.s1[[2]]$iiiia
# CTS.s1[[2]]$iii11<-CTS.s1[[2]]$iiika
# CTS.s1[[2]]$iii12<-CTS.s1[[2]]$ct2l
# CTS.s1[[2]]$vioong2<-NA

# female target child data from early waves

FTC.s1<-comp.sample("Conflict Tactics G1 on FTC T",sample=1,scored=FALSE)

FTC.s1[[1]]$target<-FTC.s1[[1]]$target1
FTC.s1[[1]]$iii4<-FTC.s1[[1]]$ct2d
FTC.s1[[1]]$iii9<-FTC.s1[[1]]$ct2i
FTC.s1[[1]]$iii11<-FTC.s1[[1]]$ct2k
FTC.s1[[1]]$iii12<-FTC.s1[[1]]$ct2l
FTC.s1[[1]]$vioong2<-NA
 

Int_G1onG2<-rbind(Int_G1onG2,CTS.s1[[1]][,Int_G1onG2.c])
Int_G1onG2<-rbind(Int_G1onG2,FTC.s1[[1]][,Int_G1onG2.c])


# re-label column entries 

unique(unlist(c(Int_G1onG2[,Int_G1onG2.c[4:7]])))

r0<-c("A=Never","never")

for (c in Int_G1onG2.c[4:7]){
  Int_G1onG2[,c]<-as.character(Int_G1onG2[,c])
  Int_G1onG2[Int_G1onG2[,c]%in%r0,c]<-0
  Int_G1onG2[Int_G1onG2[,c]%in%c("N/A","n/a"),c]<-NA
  Int_G1onG2[Int_G1onG2[,c]!=0 & !is.na(Int_G1onG2[,c]),c]<-1
  Int_G1onG2[,c]<-as.numeric(Int_G1onG2[,c])
}

Int_G1onG2$vioong2<-as.numeric(Int_G1onG2$vioong2)

# add ages

Int_G1onG2$tdate<-as.numeric(Int_G1onG2$tdate)

Int_G1onG2$int.age<-NA
for (t in unique(Int_G1onG2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  Int_G1onG2[Int_G1onG2$target%in%t,]$int.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                         as.Date(Int_G1onG2[Int_G1onG2$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}


# Retrospective reports were collected after age 20 and are sparse, so leave out

# Coddington
FE.s1<-comp.sample("Family Events G1 on G2",sample=1,scored=FALSE)
FE.s2<-comp.sample("Family Events G1 on G2",sample=2,scored=FALSE)
FE.s3<-comp.sample("Family Events G1 on G2",sample=3,scored=FALSE)

# inconsistent naming in s2
FE.s2[[4]]<-comp.sample("Family Events G1 on G2 II T4",sample=2,scored=FALSE)[[4]]

# insert "old" versions
FE.s1[[3]]<-comp.sample("Family Events G1 on G2 T3 old",sample=1,scored=FALSE)[[3]]
FE.s1[[4]]<-comp.sample("Family Events G1 on G2 T4 old",sample=1,scored=FALSE)[[4]]

FE.s2[[2]]<-comp.sample("Family Events G1 on G2 II T2 old",sample=2,scored=FALSE)[[2]]
FE.s2[[3]]<-comp.sample("Family Events G1 on G2 II T3 old",sample=2,scored=FALSE)[[3]]

FE.s3[[2]]<-comp.sample("Family Events Questionnaire G1 on G2 - Old",sample=3,scored=FALSE)[[2]]

# new structures for "new" versions

FEn.s1<-comp.sample("Family Events G1 on G2 T3 new",sample=1,scored=FALSE)
FEn.s1[[4]]<-comp.sample("Family Events G1 on G2 T4 new",sample=1,scored=FALSE)[[4]]

FEn.s2<-comp.sample("Family Events G1 on G2 II T2 new",sample=2,scored=FALSE)
FEn.s2[[3]]<-comp.sample("Family Events G1 on G2 II T3 new",sample=2,scored=FALSE)[[3]]

FEn.s3<-comp.sample("Family Events Questionnaire G1 on G2 - New",sample=3,scored=FALSE)

# bullying item not included in early forms
FE.s1[[1]]$cod33<-NA
FE.s1[[2]]$cod33<-NA

FE.s2[[1]]$cod33<-NA
FE.s2[[2]]$cod33<-NA

FE.s2[[1]]$cod33<-NA
FE.s2[[2]]$cod33<-NA

FE.s3[[2]]$cod33<-NA


FE.c<-c("target","tdate","family","age",
        "cod14", # sibling death
        "cod28","cod29", #parent incarcerated
        "cod31", # sibling involved in drug/alcohol
        "cod33") # child bullied (too much missing data, though, so leave out)


FE<-rbind(do.call("rbind",
                     lapply(FE.s1[c(1:5)],function(x){x[,c(FE.c)]})) )
FE<-rbind(FE,
             do.call("rbind",
                     lapply(FE.s2[c(1:5)],function(x){x[,c(FE.c)]})) )
FE<-rbind(FE,
             do.call("rbind",
                     lapply(FE.s3[2:3],function(x){x[,c(FE.c)]})) )

FE<-rbind(FE,
          do.call("rbind",
                  lapply(FEn.s1[c(3:4)],function(x){x[,c(FE.c)]})) )
FE<-rbind(FE,
          do.call("rbind",
                  lapply(FEn.s2[2:3],function(x){x[,c(FE.c)]})) )
FE<-rbind(FE,
          do.call("rbind",
                  lapply(FEn.s3[2],function(x){x[,c(FE.c)]})) )

# convert to 0/1

unique(unlist(c(FE[,FE.c[5:9]])))

r0<-c("0. no, not checked","No")

for (c in FE.c[5:9]){
  FE[,c]<-as.character(FE[,c])
  FE[FE[,c]%in%r0,c]<-0
  FE[FE[,c]%in%c("N/A","n/a"),c]<-NA
  FE[FE[,c]!=0 & !is.na(FE[,c]),c]<-1
  FE[,c]<-as.numeric(FE[,c])
}

# combine incarceration items

FE$cod28_29<-as.numeric(FE$cod28+FE$cod29)

# add ages 

FE$tdate<-as.numeric(FE$tdate)

FE$fe.age<-NA
for (t in unique(FE$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  FE[FE$target%in%t,]$fe.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                             as.Date(FE[FE$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}



#### Compile all ACEs together

ACEs<-G2.master[,c("target","family","dob")]


ACEs$housc2a<-NA
ACEs$housc7a<-NA
ACEs$heal2i<-NA
ACEs$econc7a<-NA
ACEs$econc3a<-NA
ACEs$parv4<-NA
ACEs$parv9<-NA
ACEs$parv11<-NA
ACEs$parv12<-NA
ACEs$chiv4<-NA
ACEs$chiv9<-NA
ACEs$chiv11<-NA
ACEs$chiv12<-NA
ACEs$cod14<-NA
ACEs$cod28_29<-NA
ACEs$cod31<-NA

for (s in ACEs$target){
  
  fam<-ACEs[ACEs$target==s,"family"]
  
  if(length(G2.fam.FC[G2.fam.FC$family==fam & G2.fam.FC$target==s & 
               G2.fam.FC$g2.age<18 & G2.fam.FC$g2.age>0,])>0){
    tmp<-G2.fam.FC[G2.fam.FC$family==fam & G2.fam.FC$target==s & 
                     G2.fam.FC$g2.age<18 & G2.fam.FC$g2.age>0,]
    tmp<-colSums(tmp[,c("housc2a","housc7a","heal2i","econc7a","econc3a")])>0
    ACEs[ACEs$target==s,c("housc2a","housc7a","heal2i","econc7a","econc3a")]<-tmp
  }
  
  
  if(length(G2.fam.Int_G1[G2.fam.Int_G1$family==fam & G2.fam.Int_G1$target==s & 
                          G2.fam.Int_G1$g2.age<18 & G2.fam.Int_G1$g2.age>0,])>0){
    tmp<-G2.fam.Int_G1[G2.fam.Int_G1$family==fam & G2.fam.Int_G1$target==s & 
                         G2.fam.Int_G1$g2.age<18 & G2.fam.Int_G1$g2.age>0,]
    t1<-colSums(tmp[,c("v4","v9","v11","v12")])>0
    t2<-colSums(tmp[,c("iv4","iv9","iv11","iv12")])>0
    tmp<-colSums(rbind(t1,t2))>0
    ACEs[ACEs$target==s,c("parv4","parv9","parv11","parv12")]<-tmp
  }
  
  if(length(Int_G1onG2[Int_G1onG2$target%in%s & 
                       Int_G1onG2$int.age<18 & Int_G1onG2$int.age>0,])>0){
    tmp<-Int_G1onG2[Int_G1onG2$target%in%s & 
                      Int_G1onG2$int.age<18 & Int_G1onG2$int.age>0,]
    tmp<-colSums(tmp[,c("iii4","iii9","iii11","iii12")])>0
    ACEs[ACEs$target==s,c("chiv4","chiv9","chiv11","chiv12")]<-tmp
  }

  if(length(FE[FE$target%in%s & 
               FE$fe.age<18 & FE$fe.age>0,])>0){
    tmp<-FE[FE$target%in%s & 
              FE$fe.age<18 & FE$fe.age>0,]
    tmp<-colSums(tmp[,c("cod14","cod28_29","cod31")])>0
    ACEs[ACEs$target==s,c("cod14","cod28_29","cod31")]<-tmp
  }
}


ACEs$ACEs<-rowSums(ACEs[,4:19])

G2.master<-merge(G2.master,ACEs[,c("target","ACEs")])

# add separate violence composites from G1 on G2 CTC 

G2.master$vioong2<-NA

for (s in G2.master$target){
  
  if(length(Int_G1onG2[Int_G1onG2$target%in%s & 
                       Int_G1onG2$int.age<18 & Int_G1onG2$int.age>0,])>0){
    G2.master[G2.master$target==s,]$vioong2<-mean(Int_G1onG2[Int_G1onG2$target%in%s & 
                      Int_G1onG2$int.age<18 & Int_G1onG2$int.age>0,"vioong2"],na.rm=TRUE)
  }
}

##########################
### Peer relationships ###
##########################

### youth (including annual)

pb_G2.s1<-comp.sample("Peer Behavior Profile G2 T",sample=1,scored=FALSE)
pb_G2.s2<-comp.sample("Peer Behavior Profile G2 II T",sample=2,scored=FALSE)
pb_G2.s3<-comp.sample("Peer Behavior Profile G2 III T",sample=3,scored=FALSE)

# ANNUAL data file 

pb_Annual_G2.s1<-comp.sample("Peer Behavior Profile G2 A",sample=1,waves = c(1:26),type = "A",scored=FALSE)
pb_Annual_G2.s2<-comp.sample("Peer Behavior Profile G2 II",sample=2,waves = c(1:26),type = "A",scored=FALSE)
pb_Annual_G2.s3<-comp.sample("Peer Behavior Profile G2 III",sample=3,waves = c(1:26),type = "A",scored=FALSE)

pb.long<-do.call("rbind",
                  lapply(pb_G2.s1[4:5],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]}))
pb.long<-rbind(pb.long,
                do.call("rbind",
                        lapply(pb_G2.s2[4:5],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]})) )
pb.long<-rbind(pb.long,
                do.call("rbind",
                        lapply(pb_G2.s3[3:5],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]})) )


pbA.long<-do.call("rbind",
                   lapply(pb_Annual_G2.s1[1:8],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]}))
pbA.long<-rbind(pbA.long,
                 do.call("rbind",
                         lapply(pb_Annual_G2.s2[1:7],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]})) )
pbA.long<-rbind(pbA.long,
                 do.call("rbind",
                         lapply(pb_Annual_G2.s3[1:5],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]})) )
pbA.long<-rbind(pbA.long,
                do.call("rbind",
                        lapply(pb_Annual_G2.s3[7],function(x){x[,c("target","tdate","age",paste0("peer",1:55))]})) )


pb_G2<-rbind(pb.long,pbA.long)


# convert to numeric

unique(unlist(c(pb_G2[,paste0("peer",1:45)])))

for (c in paste0("peer",1:45)){
  pb_G2[,c]<-as.character(pb_G2[,c])
  pb_G2[pb_G2[,c]%in%c("ALMOST NONE","Almost none"),c]<-1
  pb_G2[pb_G2[,c]%in%c("A FEW","A few"),c]<-2
  pb_G2[pb_G2[,c]%in%c("HALF","Half"),c]<-3
  pb_G2[pb_G2[,c]%in%c("MOST","Most"),c]<-4
  pb_G2[pb_G2[,c]%in%c("ALMOST ALL","Almost all"),c]<-5
  pb_G2[,c]<-as.numeric(pb_G2[,c])
}

unique(unlist(c(pb_G2[,paste0("peer",46:55)])))

for (c in paste0("peer",46:55)){
  pb_G2[,c]<-as.character(pb_G2[,c])
  pb_G2[pb_G2[,c]%in%c("VERY STRONGLY AGREE","Very strongly agree"),c]<-1
  pb_G2[pb_G2[,c]%in%c("STRONGLY AGREE","Strongly agree"),c]<-2
  pb_G2[pb_G2[,c]%in%c("AGREE","Agree"),c]<-3
  pb_G2[pb_G2[,c]%in%c("DISAGREE","Disagree"),c]<-4
  pb_G2[pb_G2[,c]%in%c("STRONGLY DISAGREE","Strongly disagree"),c]<-5
  pb_G2[pb_G2[,c]%in%c("VERY STRONGLY DISAGREE","Very strongly disagree"),c]<-6
  pb_G2[,c]<-as.numeric(pb_G2[,c])
}

# reverse code for peer relations scale

pb_G2$peer46<-(pb_G2$peer46*-1)+7
pb_G2$peer51<-(pb_G2$peer51*-1)+7
pb_G2$peer53<-(pb_G2$peer53*-1)+7
pb_G2$peer55<-(pb_G2$peer55*-1)+7

# summary score for peer relations scale

pb_G2$peer_rel_s<-rowMeans(pb_G2[,paste0("peer",46:55)])

# add ages

pb_G2$tdate<-as.numeric(pb_G2$tdate)

pb_G2$pb.age<-NA
for (t in unique(pb_G2$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  pb_G2[pb_G2$target%in%t,]$pb.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                            as.Date(pb_G2[pb_G2$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

hist(pb_G2$pb.age)
mean(table(pb_G2$target))
median(table(pb_G2$target))

# can be broken up into early and late
pb_10_13<-pb_G2[pb_G2$pb.age<14 & pb_G2$pb.age>=10 & !is.na(pb_G2$pb.age),]
pb_14_18<-pb_G2[pb_G2$pb.age<19 & pb_G2$pb.age>=14 & !is.na(pb_G2$pb.age),]


# add averages to master file 

G2.master[,c(paste0("peer",1:45,"_10_13"),"peer_rel_10_13")]<-NA
G2.master[,c(paste0("peer",1:45,"_14_18"),"peer_rel_14_18")]<-NA

for(s in unique(G2.master$target)){
  if(length(pb_10_13[pb_10_13$target==s,"target"])>0){
    tmp<-pb_10_13[pb_10_13$target%in%s,]
    G2.master[G2.master$target%in%s,c(paste0("peer",1:45,"_10_13"),"peer_rel_10_13")]<-colMeans(tmp[,c(paste0("peer",1:45),"peer_rel_s")],na.rm=TRUE)
  }
  if(length(pb_14_18[pb_14_18$target==s,"target"])>0){
    tmp<-pb_14_18[pb_14_18$target%in%s,]
    G2.master[G2.master$target%in%s,c(paste0("peer",1:45,"_14_18"),"peer_rel_14_18")]<-colMeans(tmp[,c(paste0("peer",1:45),"peer_rel_s")],na.rm=TRUE)
  }
}

#########################
### pubertal timing #####
#########################

# read in males

pd_M.s1<-comp.sample("Physical Development G2 Male",sample=1,scored=FALSE)
pd_M.s2<-comp.sample("Physical Development G2 Male",sample=2,scored=FALSE)
pd_M.s3<-comp.sample("Physical Development G2 Male",sample=3,scored=FALSE)

# read in females

pd_F.s1<-comp.sample("Physical Development G2 Female",sample=1,scored=FALSE)
pd_F.s2<-comp.sample("Physical Development G2 Female",sample=2,scored=FALSE)
pd_F.s3<-comp.sample("Physical Development G2 Female",sample=3,scored=FALSE)

# merge in with relevant columns

pd_M.c<-c("target","tdate","family","tmember","penis","pubic")
pd_F.c<-c("target","tdate","family","tmember","breast1","pubic")


pd_M<-rbind(do.call("rbind",
                     lapply(pd_M.s1[c(3:5)],function(x){x[,c(pd_M.c)]})) )
pd_M<-rbind(pd_M,
             do.call("rbind",
                     lapply(pd_M.s2[c(3:5)],function(x){x[,c(pd_M.c)]})) )
pd_M<-rbind(pd_M,
             do.call("rbind",
                     lapply(pd_M.s3[3:5],function(x){x[,c(pd_M.c)]})) )

pd_F<-rbind(do.call("rbind",
                    lapply(pd_F.s1[c(3:5)],function(x){x[,c(pd_F.c)]})) )
pd_F<-rbind(pd_F,
            do.call("rbind",
                    lapply(pd_F.s2[c(3:5)],function(x){x[,c(pd_F.c)]})) )
pd_F<-rbind(pd_F,
            do.call("rbind",
                    lapply(pd_F.s3[3:5],function(x){x[,c(pd_F.c)]})) )

# compute relevant variables

pd_M$penis<-as.numeric(pd_M$penis)
pd_M$pubic<-as.numeric(pd_M$pubic)

pd_F$breast1<-as.numeric(pd_F$breast1)
pd_F$pubic<-as.numeric(pd_F$pubic)

pd_M$tanner<-rowMeans(pd_M[,c("penis","pubic")],na.rm = T)

pd_F$tanner<-rowMeans(pd_F[,c("breast1","pubic")],na.rm = T)

# compute age

pd_M$tdate<-as.numeric(pd_M$tdate)
pd_M$pd.age<-NA
for (t in unique(pd_M$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  pd_M[pd_M$target%in%t,]$pd.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                  as.Date(pd_M[pd_M$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

hist(pd_M$pd.age)

pd_F$tdate<-as.numeric(pd_F$tdate)
pd_F$pd.age<-NA
for (t in unique(pd_F$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  pd_F[pd_F$target%in%t,]$pd.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                as.Date(pd_F[pd_F$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

hist(pd_F$pd.age)


# include only complete data points 
pd_M<-pd_M[!is.na(pd_M$pd.age) & !is.na(pd_M$tanner),]
pd_F<-pd_F[!is.na(pd_F$pd.age) & !is.na(pd_F$tanner),]

# z-score average functionally equivalent to random intercept?

model_M <- lmer(tanner ~ pd.age +  (1 | target),
              data = pd_M, REML = T)
summary(model_M)
int_M<-ranef(model_M)$target

model_F <- lmer(tanner ~ pd.age +  (1 | target),
                data = pd_F, REML = T)
summary(model_F)
int_F<-ranef(model_F)$target

pd_M$resid<-scale(resid(lm(tanner ~ pd.age, data = pd_M)))
pd_F$resid<-scale(resid(lm(tanner ~ pd.age, data = pd_F)))

int_M$avg<-NA
for (r in rownames(int_M)){
  int_M[r,"avg"]<-mean(pd_M[pd_M$target==r,]$resid)}

int_F$avg<-NA
for (r in rownames(int_F)){
  int_F[r,"avg"]<-mean(pd_F[pd_F$target==r,]$resid)}

plot(int_M$`(Intercept)`,int_M$avg);cor(int_M$`(Intercept)`,int_M$avg)

plot(int_F$`(Intercept)`,int_F$avg);cor(int_F$`(Intercept)`,int_F$avg)

G2.master$pub_timing<-NA

for (r in rownames(int_M)){
  G2.master[G2.master$target==r,]$pub_timing<-mean(pd_M[pd_M$target==r,]$resid)}

for (r in rownames(int_F)){
  G2.master[G2.master$target==r,]$pub_timing<-mean(pd_F[pd_F$target==r,]$resid)}

################################
### prenatal health history#####
################################

pn.cols<-c("target","rater","tdate","family","prehh1","prehh2","prehh3",
           "prehh5","prehh8","prehh10","prehh11","prehh16","prehh18")

prenatal.t1<-read.mls.t("Prenatal History G2 T1.sav",1,1,scored=F)[,pn.cols]
prenatal.t1<-rbind(prenatal.t1,read.mls.t("Prenatal",1,2,scored=F)[,pn.cols])
prenatal.t1<-rbind(prenatal.t1,read.mls.t("Prenatal",1,3,scored=F)[,pn.cols])

prenatal.t1$tdate<-as.Date(prenatal.t1$tdate/86400,origin = "1582-10-14")

# several people have multiple entries from different dates (mostly consistent answers)
sort(table(prenatal.t1$target))

# retain first entry for each child
dups<-table(prenatal.t1$target)
dups<-names(dups[dups>1])

keep<-rep(TRUE,length(prenatal.t1$target))
keep[prenatal.t1$target%in%dups]<-FALSE

for (d in dups){
  dates<-prenatal.t1[prenatal.t1$target==d,]$tdate
  dates<-min(dates)
  keep[prenatal.t1$target==d & prenatal.t1$tdate==dates]<-TRUE
}

# one more exact duplicate to address
keep[prenatal.t1$target==679320 ]<-c(TRUE,FALSE)

prenatal.t1<-prenatal.t1[keep,]

# address text inconsistencies
prenatal.t1[prenatal.t1$prehh1%in%c("yes","1. yes"),]$prehh1<-1
prenatal.t1[prenatal.t1$prehh1%in%c("no","0. no"),]$prehh1<-0
prenatal.t1[prenatal.t1$prehh1%in%c("don't know"),]$prehh1<-NA
prenatal.t1$prehh1<-as.logical(as.numeric(prenatal.t1$prehh1))

prenatal.t1[prenatal.t1$prehh2%in%c("yes","1. yes"),]$prehh2<-1
prenatal.t1[prenatal.t1$prehh2%in%c("no","0. no"),]$prehh2<-0
prenatal.t1$prehh2<-as.logical(as.numeric(prenatal.t1$prehh2))

prenatal.t1[prenatal.t1$prehh3%in%c("yes","1. yes"),]$prehh3<-1
prenatal.t1[prenatal.t1$prehh3%in%c("no","0. no"),]$prehh3<-0
prenatal.t1$prehh3<-as.logical(as.numeric(prenatal.t1$prehh3))

prenatal.t1[prenatal.t1$prehh5%in%c("yes","1. yes"),]$prehh5<-1
prenatal.t1[prenatal.t1$prehh5%in%c("no","0. no"),]$prehh5<-0
prenatal.t1$prehh5<-as.logical(as.numeric(prenatal.t1$prehh5))

prenatal.t1[prenatal.t1$prehh8%in%c("yes","1. yes"),]$prehh8<-1
prenatal.t1[prenatal.t1$prehh8%in%c("no","0. no"),]$prehh8<-0
prenatal.t1$prehh8<-as.logical(as.numeric(prenatal.t1$prehh8))

prenatal.t1[prenatal.t1$prehh10%in%c("yes","1. yes"),]$prehh10<-1
prenatal.t1[prenatal.t1$prehh10%in%c("no","0. no"),]$prehh10<-0
prenatal.t1$prehh10<-as.logical(as.numeric(prenatal.t1$prehh10))

prenatal.t1[prenatal.t1$prehh11%in%c("yes","1. yes"),]$prehh11<-1
prenatal.t1[prenatal.t1$prehh11%in%c("no","0. no"),]$prehh11<-0
prenatal.t1$prehh11<-as.logical(as.numeric(prenatal.t1$prehh11))

prenatal.t1[prenatal.t1$prehh16%in%c("32","35","30","34","36","26","33",
                                     "7","8","6"),]$prehh16<-1
prenatal.t1[prenatal.t1$prehh16%in%c("39","37","40","41","43","38","45","44","42","52","46",
                                     "9","10","9.5"),]$prehh16<-0
prenatal.t1[prenatal.t1$prehh16%in%c("12"),]$prehh16<-NA
prenatal.t1$prehh16<-as.logical(as.numeric(prenatal.t1$prehh16))

prenatal.t1$prehh18<-as.numeric(prenatal.t1$prehh18)
prenatal.t1$prehh18<-prenatal.t1$prehh18<508

G2.master<-merge(G2.master,prenatal.t1[,c("target","prehh1","prehh2","prehh3",
                                          "prehh5","prehh8","prehh10","prehh11","prehh16","prehh18")],
                 all.x=TRUE)

##################################################
### Health history (parent and child report) #####
##################################################

# for ever had counseling and hyperactivity meds

# read in lifetime

chhl_M.s1<-comp.sample("Child Health History Lifetime G2",sample=1,scored=FALSE)
chhl_M.s2<-comp.sample("Child Health History Lifetime G2",sample=2,scored=FALSE)
chhl_M.s3<-comp.sample("Child Health History Lifetime G2",sample=3,scored=FALSE)

chhf_M.s1<-comp.sample("Child Health History Followup G2",sample=1,scored=FALSE)
chhf_M.s2<-comp.sample("Child Health History Followup G2",sample=2,scored=FALSE)
chhf_M.s3<-comp.sample("Child Health History Followup G2",sample=3,scored=FALSE)


# merge in with relevant columns

ch.c<-c("target","tdate","family","counsel","hyper")

ch<-rbind(do.call("rbind",
                    lapply(chhl_M.s1[c(2:4)],function(x){x[,c(ch.c)]})) )
ch<-rbind(ch,do.call("rbind",
                  lapply(chhl_M.s2[c(2:4)],function(x){x[,c(ch.c)]})) )
ch<-rbind(ch,do.call("rbind",
                  lapply(chhl_M.s3[c(2:3)],function(x){x[,c(ch.c)]})) )
ch<-rbind(ch,do.call("rbind",
                  lapply(chhf_M.s1[c(3:4)],function(x){x[,c(ch.c)]})) )
ch<-rbind(ch,do.call("rbind",
                     lapply(chhf_M.s2[c(2:5)],function(x){x[,c(ch.c)]})) )
ch<-rbind(ch,do.call("rbind",
                     lapply(chhf_M.s3[c(3:4)],function(x){x[,c(ch.c)]})) )

# address variability

ch[ch$counsel%in%c("yes","Yes"),]$counsel<-TRUE
ch[ch$counsel%in%c("no","No"),]$counsel<-FALSE
ch$counsel<-as.logical(ch$counsel)

ch[ch$hyper%in%c("yes","Yes"),]$hyper<-TRUE
ch[ch$hyper%in%c("no","No"),]$hyper<-FALSE
ch$hyper<-as.logical(ch$hyper)

# add ages

ch$tdate<-as.numeric(ch$tdate)
ch$ch.age<-NA
for (t in unique(ch$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  ch[ch$target%in%t,]$ch.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                as.Date(ch[ch$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

G2.master$counsel<-NA
G2.master$hyper<-NA

for(s in unique(G2.master$target)){
  if(length(ch[ch$target==s,"target"])>0){
    tmp<-ch[ch$target%in%s,]
    G2.master[G2.master$target==s,]$counsel<-sum(tmp$counsel)>0
    G2.master[G2.master$target==s,]$hyper<-sum(tmp$hyper)>0
    }
}


#  use the counseling one
table(G2.master$counsel)
table(G2.master$hyper)

#############################
#### harter self-concepts ###
#############################

harter.s1<-comp.sample("Harter Self Perception G2",sample=1,scored=TRUE)
harter.s2<-comp.sample("Harter Self Perception G2",sample=2,scored=TRUE)
harter.s3<-comp.sample("Harter Self Perception G2",sample=3,scored=TRUE)

har.ch.c<-c("target","tdate","family","sch","soc","ath","phy","beh","glo")

har.ch<-rbind(do.call("rbind",
                  lapply(harter.s1[c(2:3)],function(x){x[,c(har.ch.c)]})) )
har.ch<-rbind(har.ch,do.call("rbind",
                     lapply(harter.s2[c(2:3)],function(x){x[,c(har.ch.c)]})) )
har.ch<-rbind(har.ch,do.call("rbind",
                     lapply(harter.s3[c(2:3)],function(x){x[,c(har.ch.c)]})) )

har.ad.c<-c("target","tdate","family","school","social","athlet",
            "appear","job","romance","conduct","friend","selfwor" )

har.ad<-rbind(do.call("rbind",
                      lapply(harter.s1[c(4:5)],function(x){x[,c(har.ad.c)]})) )
har.ad<-rbind(har.ad,do.call("rbind",
                             lapply(harter.s2[c(4:5)],function(x){x[,c(har.ad.c)]})) )
har.ad<-rbind(har.ad,do.call("rbind",
                             lapply(harter.s3[c(4:5)],function(x){x[,c(har.ad.c)]})) )


# add ages  

har.ch$tdate<-as.numeric(har.ch$tdate)
har.ch$har.age<-NA
for (t in unique(har.ch$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  har.ch[har.ch$target%in%t,]$har.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                            as.Date(har.ch[har.ch$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

har.ad$tdate<-as.numeric(har.ad$tdate)
har.ad$har.age<-NA
for (t in unique(har.ad$target)){
  if(length(G2.master[G2.master$target==t,]$dob)==1){  har.ad[har.ad$target%in%t,]$har.age<-interval(G2.master[G2.master$target==t,]$dob, 
                                                                                                     as.Date(har.ad[har.ad$target%in%t,]$tdate/86400,origin = "1582-10-14")) / 
    duration(num = 1, units = "years")
  }
}

# add to master

G2.master[,c("sch","soc","ath","phy","beh","glo")]<-NA
G2.master[,c("school","social","athlet","appear","job","romance","conduct","friend","selfwor")]<-NA

for (s in unique(G2.master$target)){
  
  if(length(har.ch[har.ch$target==s,]$target)>0){
    tmp<-har.ch[har.ch$target==s,]
    for (c in c("sch","soc","ath","phy","beh","glo")){
      G2.master[G2.master$target==s,c]<-mean(tmp[,c],na.rm=TRUE)}
  }
  
  if(length(har.ad[har.ad$target==s,]$target)>0){
    tmp<-har.ad[har.ad$target==s,]
    for (c in c("school","social","athlet","appear","job","romance","conduct","friend","selfwor")){
      G2.master[G2.master$target==s,c]<-mean(tmp[,c],na.rm=TRUE)}
  }
  
}

################################
### save out data ##############
################################

### save entire master file

save(G2.master,file="G2_master.RData")

### save out all structures relevant to computing standardized variables

save(pd_M,pd_F,sdmt_G2,tmt_G2,tmtc_G2, 
     stroop_G2, sst_G2, qsort_G2, dotsR_ConC_G2, 
     dotsR_PonC_G2, neo_G2, DoG_G2,
     file="MLS_long_files.RData")

