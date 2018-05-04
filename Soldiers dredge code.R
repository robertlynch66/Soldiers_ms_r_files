# soldiers project
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
p$offspring_sex_ratio <- p$sons/p$kids

# make husband served and husband injured 0 for unmarrieds
p$servedduringwar_husband_complete<- p$servedduringwar_husband
p$servedduringwar_husband_complete[p$never_married==1]<- 0

p$injuredinwar_husband_complete<- p$injuredinwar_husband
p$injuredinwar_husband_complete[p$never_married==1]<- 0



# make never_married category
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# get ww2 lottas only
p1 <- p %>% filter (never_married == 1 | weddingyear> 1944 | first_child_yob>1944 & birthyear<1930)
p1 <- p %>% filter (last_child_yob<1925 & birthyear_spouse > 1896)
lottas <- p %>% filter (lotta==1)
non_lottas <- p %>% filter (lotta==0)
both <- p1 %>% filter (lotta==1 | lotta==0)

# make function for SE
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}
#martta
# make summary table
means <- aggregate (sisters~ martta, p1, FUN=mean, na.action = na.omit)
SE <- aggregate (sisters~ martta, p1, st.err, na.action = na.omit)
N <-  aggregate(sisters~ martta, p1,FUN=length, na.action = na.omit)
table <- cbind(means, SE[,2], N[,2])
table
#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
maw <- p1 %>% select("kids","lotta","birthyear",
                     "education","agriculture","martta")


maw<- maw[complete.cases(maw),]
cor_mat <- cor(maw)

# correlation threshold
cor_threshold <- 0.5

# set correlations to 0 if absolute value bigger than threshold
cor_mat <- ifelse(abs(cor_mat) > cor_threshold, 0 , 1)

# generate the lower-triangular matrix with correlations
cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA



#  Married and had kids after the war
################## MODEL RANK ####################DV's InAlt", "HostOut", "ParAlt###
options(na.action = "na.fail") 
#model<-glm(HostOut ~ M1F2+RFA96+RFA2006+RelDev+IntRB+ExtRB+Christian+Church+baptize, data=RL_Data, family = binomial)
model<-glm(kids ~  birthyear+ lotta +martta+birthyear*lotta +birthyear*martta+    
             education + agriculture , data=maw,
           family = poisson)
modelset<-dredge(model, rank = AICc, trace=FALSE) #subset = (!RelDev | !IntRB))# find out from Dylan how to exclude more correlated  variables from being entered into the same model
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
write.table(modelset,"C:/Users/rofrly/Desktop/AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")
