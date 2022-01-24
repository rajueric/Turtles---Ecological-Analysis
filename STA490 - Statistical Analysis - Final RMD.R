install.packages("pandas")
library(tidyverse)
library(here)
library(marked)
library(dplyr)
library(readxl)
library(pandas)

# Special user-defined functions
index <- function(lst,item,k,all=FALSE) {
  # Return the first index of item in lst. If k = -1, return the last one. Return
  # only the one index is all is false, else return all.
  indices <- c()
  n <- length(lst)
  for (i in 1:n) {
    if (item==lst[i]) {
      indices <- append(indices,i,length(indices))
    }
  }
  if ((k!=-1)&&(!all)) {
    indices[k]
  }
  else if (k==-1) {
    n <- length(indices)
    indices[n]
  }
  else {
    indices
  }
}

unique <- function(lst) {
  # Create a list of each unique element of lst.
  unit <- c()
  for (item in lst) {
    if (!(item%in%unit)) {
      unit <- append(unit,item,length(unit))
    }
  }
  unit
}
#

turtles_after_EDA <- read_excel("C:/Users/ericr/OneDrive/Documents/UNIVERSITY/U OF T/COURSES/2020-2021/STA490Y1/turtles_after_EDA.xlsx")

turtles_raw = turtles_after_EDA
##
turtles_forModel = tibble(notch = turtles_raw$notch,
                          site = turtles_raw$site,
                          sex = turtles_raw$sex,
                          midpl = turtles_raw$midpl,
                          dead = turtles_raw$dead,
                          year = turtles_raw$Year)

## Selecting the data
turtles_forModel = turtles_forModel[-which(turtles_forModel$dead==TRUE),]
turtles_forModel = turtles_forModel[which(turtles_forModel$year%in%
                                            c(1991:2017)),]
turtles_forModel = turtles_forModel[-which(is.na(turtles_forModel$sex)),]
turtles_forModel = turtles_forModel[-which(turtles_forModel$sex=='-'),]

## Imputing unknown sex -> does site affect it
unknowns = which(turtles_forModel$sex=='Unknown')
sites_with_unknown = turtles_forModel$site[unknowns]
sites_check = unique(sites_with_unknown)
counts_all = c()
counts_unknown = c()
counts_male = c()
counts_female = c()
for (site in sites_check) {
  at_site = which(turtles_forModel$site==site)
  at_site_unknown = which(turtles_forModel[at_site,]$sex=='Unknown')
  counts_all = append(counts_all,length(at_site),length(counts_all))
  counts_unknown = append(counts_unknown,length(at_site_unknown),
                          length(counts_unknown))
  turtle_site = turtles_forModel[at_site,]
  turtle_male = which(turtle_site$sex=='Male')
  nmale = length(turtle_male)
  counts_male = append(counts_male,nmale,length(counts_male))
  turtle_female = which(turtle_site$sex=='Female')
  nfemale = length(turtle_female)
  counts_female = append(counts_female,nfemale,length(counts_female))
}
frac_unknown_bysite = counts_unknown/counts_all
frac_unknown_bysite = round(frac_unknown_bysite,3)
frac_male = counts_male/(counts_all-counts_unknown)
frac_female = counts_female/(counts_all-counts_unknown)
sites_check_unknown = cbind(sites_check,counts_all,counts_unknown,
                            frac_unknown_bysite,counts_male,counts_female,
                            frac_male,frac_female)

remove = c("Blanding's Bog")
nosite = which(turtles_forModel$site%in%remove)
unknown_both = intersect(nosite,unknowns)
turtles_forModel = turtles_forModel[-unknown_both,]

# for the remaining turtles with unknown sex, split them up by the proportion of
# male/female in the other respective sites
# left_sites = setdiff(sites_check,remove)
# for (row in 1:nrow(turtles_forModel)) {
#   if (turtles_forModel$site[row]%in%left_sites) {
#     if (turtles_forModel$sex[row]=='Unknown') {
#       site = turtles_forModel$site[row]
#       sites_ind = index(left_sites,site,k=1,all=FALSE)
#       fMale = sites_check_unknown[sites_ind,7] # fMale = prob, ignoring unknowns
#       fFemale = sites_check_unknown[sites_ind,8] #fFemale = 1-prob, ignoring unknowns
#       newsex = sample(c("Male","Female"),size=1,replace=TRUE,prob=c(fMale,fFemale))
#       turtles_forModel[row,3] = newsex
#     }
#   }
# }

## Imputing midpl as the average value for that turtle
# using normal drift
notches = turtles_forModel$notch
notch.means.num = c()
notch.inits= c()
N = nrow(turtles_forModel)
for (notch in notches) {
  notch_inds = which(turtles_forModel$notch==notch)
  this.turtle = turtles_forModel[notch_inds,]
  known.turtle = this.turtle[which(!(is.na(this.turtle$midpl))),]
  Ni = nrow(known.turtle)
  mu.i = mean(known.turtle$midpl)
  Xi.0 = known.turtle$midpl[1]
  val = (1/Ni)*(mu.i*Xi.0)
  notch.means.num = append(notch.means.num,val,length(notch.means.num))
  notch.inits = append(notch.inits,Xi.0,length(notch.inits))
}
notch.means.denom = mean(notch.inits)
notch.means = notch.means.num/notch.means.denom
notch.means = cbind(notches,notch.means)
colnames(notch.means) = c("notches","means")

for (i in 1:nrow(turtles_forModel)) {
  notch = turtles_forModel$notch[i]
  notch_inds = which(turtles_forModel$notch==notch)
  this.turtle = turtles_forModel[notch_inds,]
  known.turtle = this.turtle[which(!(is.na(this.turtle$midpl))),]
  midpl = known.turtle$midpl
  Ni = nrow(known.turtle)
  mu.i = mean(known.turtle$midpl)
  Xi.0 = known.turtle$midpl[1]
  midavg = as.numeric(notch.means[which(notch.means[,1]==notch),2])
  sigma = ((1/Ni)^2)*((midpl-mu.i)^2)*((Xi.0/notch.means.denom)^2)
  sigma = (1/(N-1))*sum(sigma)
  for (ind in notch_inds) {
    if (is.na(turtles_forModel$midpl[ind])) {
      turtles_forModel[ind,4] = rnorm(n=1,mean=midavg,sd=sigma)
    }
  }
}

turtles3 <- turtles_forModel %>%
  select(notch, year) %>%
  mutate(capture = "1") %>%
  complete(notch, year, fill = list(capture = "0")) %>%
  group_by(notch) %>%
  summarise(capture_history = str_c(capture, collapse = ""))

## Combining sex and sites
notches = turtles3$notch
sex = c()
sites = c()
for (notch in notches) { # all the unique appearances of sex
  this.turtle =
    turtles_forModel[which(turtles_forModel$notch==notch),]
  this.sex = this.turtle$sex[1]
  this.site = this.turtle$site[1]
  sex = append(sex,this.sex,length(sex))
  sites = append(sites,this.site,length(sites))
}
turtles3$sex = sex

# define variable newsite and combine the following--
newsite = vector(mode="list",length=6)
newsite[[1]] = c("Arowhon Rd.")
newsite[[2]] = c("Sims Creek")
newsite[[3]] = c("Maiden Lake")
newsite[[4]] = c("Road to WHP","Blanding's Bog")
newsite[[5]] = c("Wolf Howl Pond","Wolf Howl Pond E.","Wolf Howl Pond e.")
newsite[[6]] = c("Road to Wrose","West Rose","March Hare Lake")
newsites = c()
for (site in sites) {
  newsite.add = NULL
  for (i in 1:6) {
    if (site%in%newsite[[i]]) {
      newsite.add = i
    }
  }
  newsites = append(newsites,newsite.add,length(newsites))
}
turtles3$newsite = newsites

# for mid-pl
midplTable = notches
for (year in 1991:2017) {
  this.year = turtles_forModel[which(turtles_forModel$year
                                     ==year),]
  yearTurtle = c()
  for (notch in 1:nrow(this.year)) {
    yearTurtle = append(yearTurtle,this.year$midpl[notch],length(yearTurtle))
  }
  midplTable = cbind(midplTable,yearTurtle)
}
midplTable = midplTable[,-1]
colnames(midplTable) = c(1991:2017)
rownames(midplTable) = notches
midplTibble = tibble(midplTable)
nyears = 2017-1990
yearTable = c(1991:2017)
for (year in 1:nyears) {
  name = paste("midpl",year)
  name = gsub(" ","",name)
  colnames(midplTable)[year] = name
}
turtles3 = cbind(turtles3,midplTable)
turtles3$ch = turtles3$capture_history

# Constructing Size Classes
first_appearance = c()
first_size = c()
for (notch in notches) {
  notch_inds = which(turtles_forModel$notch==notch)
  this.turtle = turtles_forModel[notch_inds,]
  appears = this.turtle$year[1]
  size = this.turtle$midpl[1]
  first_appearance = append(first_appearance,appears,length(first_appearance))
  first_size = append(first_size,size,length(first_size))
}
turtles3$first_appearance = first_appearance
turtles3$first_size = first_size

size_classes = unname(quantile(first_size))
notch_lists = vector("list",length=4)
vals0 = which(first_size<=size_classes[1])
vals1 = which(first_size<=size_classes[2])
vals1 = setdiff(vals1,vals0)
vals2 = which(first_size<size_classes[3])
vals2 = setdiff(setdiff(vals2,vals1),vals0)
vals3 = which(first_size<size_classes[4])
vals3 = setdiff(setdiff(setdiff(vals3,vals2),vals1),vals0)
vals4 = which(first_size<size_classes[5])
vals4 = setdiff(setdiff(setdiff(setdiff(vals4,vals3),vals2),vals1),vals0)
vals1 = c(1,vals0,vals1)
notch_lists[[1]] = as.data.frame(rbind(notches[vals1],
                                       first_appearance[vals1],
                                       first_size[vals1]))
notch_lists[[2]] = as.data.frame(rbind(notches[vals2],
                                       first_appearance[vals2],
                                       first_size[vals2]))
notch_lists[[3]] = as.data.frame(rbind(notches[vals3],
                                       first_appearance[vals3],
                                       first_size[vals3]))
notch_lists[[4]] = as.data.frame(rbind(notches[vals4],
                                       first_appearance[vals4],
                                       first_size[vals4]))
captures = c(1991:2017)
for (row in 1:nrow(turtles3)) {
  captures = rbind(captures,as.numeric(strsplit(turtles3$ch[row],"")[[1]]))
}
years = captures[1,]
captures = captures[-1,]
colnames(captures) = years

turtles3 = cbind(turtles3,captures)

#df <- data.frame(as.character(size_classes))
#kable(df, col.names = c("","0%","25%","50%","75%","100%"), escape = F, caption = "Quantiles #for size classes of turtles") %>%
#  kable_styling(latex_options = "hold_position")

turtles3 = turtles3[notches,]
turtles3.temp = turtles3[,6:ncol(turtles3)]
for (row in 1:nrow(turtles3.temp)) {
  for (col in 1:ncol(turtles3.temp)) {
    temp = as.numeric(as.character(turtles3.temp[row,col]))
    turtles3.temp[row,col] = temp
  }
}
turtles3[,6:ncol(turtles3)] = turtles3.temp

turtlesMale = turtles3[which(turtles3$sex=="Male"),]
turtlesFemale = turtles3[which(turtles3$sex=="Female"),]
turtlesJuv = turtles3[which(turtles3$sex=="Unknown"),]

years = c(1991:2017)

turtlesMale = turtles3[which(turtles3$sex=="Male"),]
turtlesFemale = turtles3[which(turtles3$sex=="Female"),]
turtlesJuv = turtles3[which(turtles3$sex=="Unknown"),]

male.sites = c()
female.sites = c()
juv.sites = c()
for (i in 1:6) {
  site.lengthM = length(which(turtlesMale$newsite==i))
  male.sites = append(male.sites,site.lengthM,length(male.sites))
  site.lengthF = length(which(turtlesFemale$newsite==i))
  female.sites = append(female.sites,site.lengthF,length(female.sites))
  site.lengthJ = length(which(turtlesJuv$newsite==i))
  juv.sites = append(juv.sites,site.lengthJ,length(juv.sites))
}
sites.by.sex = cbind(male.sites,female.sites,juv.sites)
matplot(x=c(1:6),y=log(sites.by.sex),type='l',col=c("red","blue","green"))

male.year = c()
female.year = c()
juv.year = c()
for (year in 1:26) {
  year.lengthM = length(which(turtlesMale[,year+31]==1))
  male.year = append(male.year,year.lengthM,length(male.year))
  year.lengthF = length(which(turtlesFemale[,year+31]==1))
  female.year = append(female.year,year.lengthF,length(female.year))
  year.lengthJ = length(which(turtlesJuv[,year+31]==1))
  juv.year = append(juv.year,year.lengthJ,length(juv.year))
}
year.by.sex = cbind(male.year,female.year,juv.year)
matplot(x=c(1:26)+1991,y=year.by.sex,type='l',col=c("red","blue","green"))

a = ggplot(data=NULL,aes(x=c(1991:2017))) +
  geom_density(aes(turtlesMale$first_size),col="blue") +
  geom_density(aes(turtlesFemale$first_size),col="red") +
  geom_density(aes(turtlesJuv$first_size),col="green")

all.midpl = turtles3[,5:30]
male.midpl = turtlesMale[,5:30]
female.midpl = turtlesFemale[,5:30]
juv.midpl = turtlesJuv[,5:30]

#matplot(x=c(1991:2017),density(t(male.midpl)),type="l")

time.midpl = ggplot(data=NULL,aes(x=c(1991:2017)))
for (year in 5:30) {
  time.midpl = time.midpl +
    geom_point(aes(y=log(mean(as.numeric(as.character(turtlesMale[,year]))))),col="blue")
  #time.midpl = time.midpl +
  #  geom_point(aes(y=log(mean(as.numeric(as.character(turtlesFemale[,year]))))),col="red")
  #time.midpl = time.midpl +
  #  geom_point(aes(y=log(mean(as.numeric(as.character(turtlesJuv[,year]))))),col="green")
}

turtles4 = turtles3[,1:5]
turtles4$ch = turtles3$capture_history
for (year in 1991:2017) {
  names = colnames(turtles4)
  colnames(turtles4)[index(names,year,k=1,all=FALSE)] = paste("time",year-1990,sep="")
}

model1 = vector("list",length=2)
model2 = vector("list",length=2)
model3 = vector("list",length=2)
model4 = vector("list",length=2)
model1[[1]] = turtles4[vals1,]
model1[[2]] = c("phi^","p^","AIC","-2lnl")
model2[[1]] = turtles4[vals2,]
model2[[2]] = c("phi^","p^","AIC","-2lnl")
model3[[1]] = turtles4[vals3,]
model3[[2]] = c("phi^","p^","AIC","-2lnl")
model4[[1]] = turtles4[vals4,]
model4[[2]] = c("phi^","p^","AIC","-2lnl")
model_turtles = list("model class 1"=model1,
                     "model class 2"=model2,
                     "model class 3"=model3,
                     "model class 4"=model4
)

# Re-creating the midpl data
turtles4 = turtles3[,-c(1:4)]
midpl.df = turtles3[,c(5:31)]
colnames(midpl.df) = paste("midpl",1:27,sep="")
turtles4 = cbind(turtles3[,c(1:4)],midpl.df)
for (row in 1:nrow(midpl.df)) {
  for (col in 5:ncol(midpl.df)) {
    midpl.df[row,col] = as.numeric(as.character(midpl.df[row,col]))
  }
}
turtles4 = cbind(turtles4,turtles3[,c(32:61)])

###
colnames(turtles4) = colnames(model_turtles[[1]][[1]])
# Time-varying models
turtles.proc = process.data(turtles4)
design.Phi = list(static=c("notch","sex","newsite"),
                  time.varying=c("midpl","time"))
design.p = list(static=c("notch","sex","newsite"),
                time.varying=c("midpl","time"))
design.parameters = list(Phi=design.Phi,p=design.p)
turtles.ddl = make.design.data(turtles.proc,parameters=design.parameters)

#fit.models=function() # Simple models: 1, 1+time, 1+time+site, 1+time+site+sex
#fit.models2=function() # Simple models: 1+sex
fit.modelsNew=function() # Time-varying model: 1+sex+midpl
#fit.modelsNew2=function()
{
  #Phi.dot = list(formula=~1)
  #p.dot = list(formula=~1)
  #Phi.sex = list(formula=~1+sex)
  #p.sex = list(formula=~1+sex)
  # Phi.time=list(formula=~1+time)
  # p.time=list(formula=~1+time)
  # Phi.time.newsite = list(formula=~1+time+newsite)
  # p.time.newsite = list(formula=~1+time+newsite)
  # Phi.time.newsite.sex = list(formula=~1+time+newsite+sex)
  # p.time.newsite.sex = list(formula=~1+time+newsite+sex)
  Phi.sex.midpl = list(formula=~1+sex+as.numeric(as.character(midpl)))
  p.sex.midpl = list(formula=~1+sex+as.numeric(as.character(midpl)))
  cml=create.model.list(c("Phi","p"))
  results=crm.wrapper(cml,data=turtles.proc, ddl=turtles.ddl, hessian=TRUE,
                      external=FALSE,accumulate=FALSE)
  return(results)
}
#turtles.models=fit.models()
#turtles.models2=fit.models2()
turtles.modelsNew=fit.modelsNew()
turtles.modelsNew2=fit.modelsNew2()

#
models_midplClass = vector("list",length=4)
for (i in 1:4) {
  class_models = vector("list",length=4)
  models_midplClass[[i]] = class_models
}

### ignore this
# For the models of the form ~1+time+newsite+(sex), by midpl quantile
for (i in 1:4) {
  turtles.proc = process.data(model_turtles[[i]][[1]])
  design.Phi = list(static=c("notch","sex","newsite"),time.varying=c("time"))
  design.p = list(static=c("notch","sex","newsite"),time.varying=c("time"))
  design.parameters = list(Phi=design.Phi,p=design.p)
  turtles.ddl = make.design.data(turtles.proc,parameters=design.parameters)
  
  #if (i<=4) {
    #print(i)
    #fit.models3=function()
  fitModel3=function()
    {
    Phi.time.newsite = list(formula=~1+time+newsite)
    p.time.newsite = list(formula=~1+time+newsite)
    cml=create.model.list(c("Phi","p"))
    results=crm.wrapper(cml,data=turtles.proc, ddl=turtles.ddl,
                        external=FALSE,accumulate=FALSE,hessian=TRUE)
    return(results)
  }
  turtles.models3=fitModel3()
  model = turtles.models3$Phi.time.newsite.p.time.newsite$results
  models_Phis = vector("list",length=2)
  models_Phis[[1]] = model$reals$Phi
  site.ind.Phi = which(model$beta$Phi=="newsite")
  models_Phis[[2]] = model$beta$Phi[-27] # just the time effects
  models_Phis[[3]] = model$beta$Phi[27] # site effect
  models_ps = vector("list",length=2)
  models_ps[[1]] = model$reals$p
  site.ind.p = which(names(model$beta$p)=="newsite")
  models_ps[[2]] = model$beta$p[-27] # just the time effects
  models_ps[[3]] = model$beta$p[27] # site effect
  models_midplClass[[i]][[1]] = models_Phis
  models_midplClass[[i]][[2]] = models_ps
  models_midplClass[[i]][[3]] = model$neg2lnl
  models_midplClass[[i]][[4]] = model$AIC
  #}
  # else if (i==4) { # nearly all of the turtles here are female, by coincidence
  #   # which interferes with the model
  #   fit.models3=function()
  #   {
  #     Phi.time.newsite = list(formula=~1+time+newsite)
  #     p.time.newsite = list(formula=~1+time+newsite)
  #     cml=create.model.list(c("Phi","p"))
  #     results=crm.wrapper(cml,data=turtles.proc, ddl=turtles.ddl,hessian=TRUE,
  #                         external=FALSE,accumulate=FALSE)
  #     return(results)
  #   }
  #   turtles.models3=fit.models3()
  #   model5 = turtles.models2$Phi.time.newsite.p.time.newsite
  #   results5 = model5$results
  #   model_turtles[[i]][[2]] = results5
  # }
}

report = vector("list",length=4)
lnL = c(models_midplClass[[1]][[3]],models_midplClass[[2]][[3]],
        models_midplClass[[3]][[3]],models_midplClass[[4]][[3]])
lnL.avg = mean(lnL)
lnL.sd = sqrt(var(lnL))
c(lnL.avg,lnL.sd)
AIC = c(models_midplClass[[1]][[4]],models_midplClass[[2]][[4]],
        models_midplClass[[3]][[4]],models_midplClass[[4]][[4]])
AIC.avg = mean(AIC)
AIC.sd = sqrt(var(AIC))
c(AIC.avg,AIC.sd)

for (i in 1:4) {
  model.Phi.sites = unique(models_midplClass[[i]][[1]][[1]][,"newsite"])
  time.effects.Phi = models_midplClass[[i]][[1]][[2]]
  time.PhiAvg = mean(time.effects.Phi)
  time.PhiSd = sqrt(var(time.effects.Phi))
  time.Phi = c(time.PhiAvg,time.PhiSd)
  site.Phi = models_midplClass[[i]][[2]][[3]]
  Phi.effects = c(time.Phi,site.Phi)
  
  model.p.sites = unique(models_midplClass[[i]][[2]][[1]][,"newsite"])
  time.effects.p = models_midplClass[[i]][[2]][[2]]
  time.pAvg = mean(time.effects.p)
  time.pSd = sqrt(var(time.effects.p))
  time.p = c(time.pAvg,time.pSd)
  site.p = models_midplClass[[i]][[2]][[3]]
  p.effects = c(time.p,site.p)
  
  all.class = rbind(Phi.effects,p.effects)
  colnames(all.class) = c("mean time effect","sd time effect","site effect")
  rownames(all.class) = c("Phi","p")
  report[[i]] = all.class
}

class.reports = rbind(report[[1]],report[[2]],report[[3]],report[[4]])
class.reports = cbind(c(rep("Size Class 1",2),rep("Size Class 2",2),
                      rep("Size Class 3",2),rep("Size Class 4",2)),
                      class.reports)

sites1.Phi = sort(unique(class1.effects.Phi[,2]))
sites2.Phi = sort(unique(class2.effects.Phi[,2]))
sites3.Phi = sort(unique(class3.effects.Phi[,2]))
sites4.Phi = sort(unique(class4.effects.Phi[,2]))
class1.effects.Phi = models_midplClass[[1]][[1]][[1]]
class2.effects.Phi = models_midplClass[[2]][[1]][[1]]
class3.effects.Phi = models_midplClass[[3]][[1]][[1]]
class4.effects.Phi = models_midplClass[[4]][[1]][[1]]
sites1.p = sort(unique(class1.effects.p[,2]))
sites2.p = sort(unique(class2.effects.p[,2]))
sites3.p = sort(unique(class3.effects.p[,2]))
sites4.p = sort(unique(class4.effects.p[,2]))
class1.effects.p = models_midplClass[[1]][[2]][[1]]
class2.effects.p = models_midplClass[[2]][[2]][[1]]
class3.effects.p = models_midplClass[[3]][[2]][[1]]
class4.effects.p = models_midplClass[[4]][[2]][[1]]

classplots.Phi = vector("list", length=4)
classplots.p = vector("list", length=4)

for (c in 1:4) {
  sites.Phi = NULL
  sites.p = NULL
  if (c==1) {
    sites.Phi = sites1.Phi
    sites.p = sites1.p
  } else if (c==2) {
      sites.Phi = sites2.Phi
      sites.p = sites2.p
  } else if (c==3) {
    sites.Phi = sites3.Phi
    sites.p = sites3.p
  } else if (c==4) {
    sites.Phi = sites4.Phi
    sites.p = sites4.p
  }
  this.plot.Phi = vector("list", length=length(sites.Phi))
  d.Phi = as.data.frame(models_midplClass[[c]][[1]][[1]])
  this.plot.p = vector("list", length=length(sites.p))
  d.p = as.data.frame(models_midplClass[[c]][[2]][[1]])
  for (s in sites.Phi) {
    times = c(2:27)
    site.inds.Phi = which(d.Phi$newsite==s)
    d.Phi.site = d.Phi[which(d.Phi$newsite==s),]
    d.Phi.site = d.Phi.site[order(d.Phi.site$time),]
    times.Phi = d.Phi.site$time
    smallest.PhiTime = min(c(times.Phi))
    smallest.PhiYear = smallest.PhiTime + 1991
    largest.PhiTime = max(c(times.Phi))
    largest.PhiYear = largest.PhiTime + 1991
    Phi.years = c(smallest.PhiYear:largest.PhiYear)
    times.Phi = c(times.Phi)
    
    times = c(2:27)
    site.inds.p = which(d.p$newsite==s)
    d.p.site = d.p[which(d.p$newsite==s),]
    d.p.site = d.p.site[order(d.p.site$time),]
    times.p = d.p.site$time
    smallest.pTime = min(c(times.p))
    smallest.pYear = smallest.pTime + 1991
    largest.pTime = max(c(times.p))
    largest.pYear = largest.pTime + 1991
    p.years = c(smallest.pYear:largest.pYear)
    s.new = s
    
    message(s)
    this.plot.Phi[[which(sites.Phi==s)]] = local({
      s = s
      PhiPlot = ggplot(data=NULL, aes(x = Phi.years)) +
        geom_line(aes(y = d.Phi.site$estimate),col="black") +
        geom_line(aes(y = d.Phi.site$lcl),col="blue") +
        geom_line(aes(y = d.Phi.site$ucl),col="blue") +
        xlab("Year") + ylab("effect") +
          ggtitle(paste("Size class",c,"effects on Phi at site",s))
      print(PhiPlot)
    })
    
    message(s.new)
    this.plot.p[[which(sites.p==s.new)]] <- local({
      s = s.new
      p.years = c
      pPlot = ggplot(data=NULL, aes(x = p.years)) +
        geom_line(aes(y = d.p.site$estimate),col="black") +
        geom_line(aes(y = d.p.site$lcl),col="blue") +
        geom_line(aes(y = d.p.site$ucl),col="blue") +
        xlab("Year") + ylab("effect") +
          ggtitle(paste("Size class",c,"effects at on p at site",s))
      print(pPlot)
    })
  }
  #grid.arrange(grobs=this.plot.Phi[[1]],nrow=1)
  #grid.arrange(grobs=this.plot.p,nrow=1)
  classplots.Phi[[c]] = this.plot.Phi
  classplots.p[[c]] = this.plot.p
}
grid.arrange(grobs=list(classplots.Phi[[1]][[1]],classplots.Phi[[2]][[1]],
                        classplots.Phi[[3]][[1]],classplots.Phi[[4]][[1]]),
             nrow=1)

# We may evaluate the coefficients as follows:
# Model 1:
model1 = turtles.models$Phi.dot.p.dot
results1 = model1$results$reals
ncolsPhi1 = length(results1$Phi)
bPhi1 = ncolsPhi1
aPhi1 = ncolsPhi1 - 3
odds.Phi1 = cbind(results1$Phi[,-c(aPhi1:bPhi1)],exp(results1$Phi[,c(aPhi1:bPhi1)]))
ncolsp1 = length(results1$p)
bp1 = ncolsp1
ap1 = ncolsp1 - 3
odds.p1 = cbind(results1$p[,-c(ap1:bp1)],exp(results1$p[,c(ap1:bp1)]))

est.1Phi = results1$Phi
est.1p = results1$p
ests.1 = rbind(est.1Phi,est.1p)
colnames(ests.1) = colnames(results1$Phi)

ests.1[,1] = c("Phi","p")
colnames(ests.1)[1] = "parameter"
odds.1 = cbind(ests.1[,1],exp(ests.1[,2:5]))
odds.1[,1] = c("Phi","p")
colnames(odds.1)[1] = "parameter"

ests.1 = cbind(ests.1[,1],round(ests.1[,2:5],4))
colnames(ests.1)[1] = "parameter"
odds.1 = cbind(odds.1[,1],round(odds.1[,2:5],4))
colnames(odds.1)[1] = "parameter"


# Model 1a:
model1a = turtles.modelsNew$Phi.sex.p.sex
results1a = model1a$results$reals
ncolsPhi1a = length(results1a$Phi)
bPhi1a = ncolsPhi1a
aPhi1a = ncolsPhi1a - 3
odds.Phi1a = cbind(results1a$Phi[,-c(aPhi1a:bPhi1a)],exp(results1a$Phi[,c(aPhi1a:bPhi1a)]))
ncolsp1a = length(results1a$p)
bp1a = ncolsp1a
ap1a = ncolsp1a - 3
odds.p1a = cbind(results1a$p[,-c(ap1a:bp1a)],exp(results1a$p[,c(ap1a:bp1a)]))

est.1aPhi = results1a$Phi
est.1ap = results1a$p
ests.1a = rbind(est.1aPhi,est.1ap)
colnames(ests.1a) = colnames(results1a$Phi)
ests.1a = cbind(ests.1a[,1],ests.1a[,3:6])
ests.1a = cbind(c(rep("Phi",3),rep("p",3)),ests.1a)
ests.1a[,2] = rep(c("Female","Male","Unknown"))
colnames(ests.1a)[c(1,2)] = c("parameter","sex")
params.1a = colnames(ests.1a)


odds.1a = cbind(ests.1a[,1],exp(ests.1a[,2:5]))
odds.1a = cbind(c(rep("Phi",3),rep("p",3)),odds.1a)
odds.1a[,2] = rep(c("Female","Male","Unknown"))
colnames(odds.1a)[c(1,2)] = c("parameter","sex")

# Model 2:
model2 = turtles.models$Phi.time.p.time
results2 = model2$results$reals
ncolsPhi2 = length(results2$Phi)
bPhi2 = ncolsPhi2
aPhi2 = ncolsPhi2 - 3
odds.Phi2 = cbind(results2$Phi[,-c(aPhi2:bPhi2)],exp(results2$Phi[,c(aPhi2:bPhi2)]))
ncolsp2 = length(results2$p)
bp2 = ncolsp2
ap2 = ncolsp2 - 3
odds.p2 = cbind(results2$p[,-c(ap2:bp2)],exp(results2$p[,c(ap2:bp2)]))

ntime = c(2:25)
newsort2.Phi = colnames(results2$Phi)
for (i in ntime) {
  inds.i = which(results2$Phi[,1]==i)
  newsort2.Phi = rbind(newsort2.Phi,results2$Phi[inds.i,])
}
newsort2.Phi = newsort2.Phi[-1,]
newsort2.Phi = newsort2.Phi[,-which(colnames(newsort2.Phi)=="occ")]
j = which(colnames(newsort2.Phi)=="estimate")
for (col in j:ncol(newsort2.Phi)) {
  for (row in 1:nrow(newsort2.Phi)) {
    temp = as.numeric(as.character(newsort2.Phi[row,col]))
    newsort2.Phi[row,col] = round(temp,4)
  }
}

ntime = c(2:25)
newsort2.p = colnames(results2$p)
for (i in ntime) {
  inds.i = which(results2$p[,1]==i)
  newsort2.p = rbind(newsort2.p,results2$p[inds.i,])
}
newsort2.p = newsort2.p[-1,]
newsort2.p = newsort2.p[,-which(colnames(newsort2.p)=="occ")]
j = which(colnames(newsort2.p)=="estimate")
for (col in j:ncol(newsort2.p)) {
  for (row in 1:nrow(newsort2.p)) {
    temp = as.numeric(as.character(newsort2.p[row,col]))
    newsort2.p[row,col] = round(temp,4)
  }
}

est.2Phi = cbind(rep("Phi",nrow(results2$Phi)),results2$Phi)
colnames(est.2Phi)[1] = "Est. name"
est.2p = cbind(rep("p",nrow(results2$p)),results2$p)
colnames(est.2p)[1] = "Est. name"
ests.2 = rbind(est.2Phi,est.2p)
colnames(ests.2) = c("Est. name",colnames(results2$Phi))

# Model 2a:
model2a = turtles.modelsNew2$Phi.sex.midpl.p.sex.midpl
results2a = model2a$results$reals
ncolsPhi2a = length(results2a$Phi)
bPhi2a = ncolsPhi2a
aPhi2a = ncolsPhi2a - 3
odds.Phi2a = cbind(results2a$Phi[,-c(aPhi2a:bPhi2a)],exp(results2a$Phi[,c(aPhi2a:bPhi2a)]))
ncolsp2a = length(results2a$p)
bp2a = ncolsp2a
ap2a = ncolsp2a - 3
odds.p2a = cbind(results2a$p[,-c(ap2a:bp2a)],exp(results2a$p[,c(ap2a:bp2a)]))

ntime = c(2:25)
newsort2a.Phi = colnames(results2a$Phi)
for (i in ntime) {
  inds.i = which(results2a$Phi[,1]==i)
  newsort2a.Phi = rbind(newsort2a.Phi,results2a$Phi[inds.i,])
}
newsort2a.Phi = newsort2a.Phi[-1,]
newsort2a.Phi = newsort2a.Phi[,-which(colnames(newsort2a.Phi)=="occ")]
j = which(colnames(newsort2a.Phi)=="estimate")
for (col in j:ncol(newsort2a.Phi)) {
  for (row in 1:nrow(newsort2a.Phi)) {
    temp = as.numeric(as.character(newsort2a.Phi[row,col]))
    newsort2a.Phi[row,col] = round(temp,4)
  }
}

ntime = c(2:25)
newsort2a.p = colnames(results2a$p)
for (i in ntime) {
  inds.i = which(results2a$p[,1]==i)
  newsort2a.p = rbind(newsort2a.p,results2a$p[inds.i,])
}
newsort2a.p = newsort2a.p[-1,]
newsort2a.p = newsort2a.p[,-which(colnames(newsort2a.p)=="occ")]
j = which(colnames(newsort2a.p)=="estimate")
for (col in j:ncol(newsort2a.p)) {
  for (row in 1:nrow(newsort2a.p)) {
    temp = as.numeric(as.character(newsort2a.p[row,col]))
    newsort2a.p[row,col] = round(temp,4)
  }
}

est.2aPhi = cbind(rep("Phi",nrow(results2a$Phi)),results2a$Phi)
colnames(est.2aPhi)[1] = "Est. name"
est.2ap = cbind(rep("p",nrow(results2a$p)),results2a$p)
colnames(est.2ap)[1] = "Est. name"
ests.2a = rbind(est.2aPhi,est.2ap)
colnames(ests.2a) = c("Est. name",colnames(results2a$Phi))

# Model 3:
model3 = turtles.models$Phi.time.newsite.p.time.newsite
results3 = model3$results$reals
ncolsPhi3 = length(results3$Phi)
bPhi3 = ncolsPhi3
aPhi3 = ncolsPhi3 - 3
odds.Phi3 = cbind(results3$Phi[,-c(aPhi3:bPhi3)],exp(results3$Phi[,c(aPhi3:bPhi3)]))
odds.avg.Phi3 = vector("list",length=6)
for (site in 1:6) {
  this.odds = colnames(odds.Phi3)
  for (row in 1:nrow(odds.Phi3)) {
    if (odds.Phi3$newsite==site) {
      this.odds = rbind(this.odds,odds.Phi3[row,])
    }
  }
  odds.avg.Phi3[[site]] = this.odds
}

ncolsp3 = length(results3$p)
bp3 = ncolsp3
ap3 = ncolsp3 - 3
odds.p3 = cbind(results3$p[,-c(ap3:bp3)],exp(results3$p[,c(ap3:bp3)]))
odds.avg.p3 = vector("list",length=6)
for (site in 1:6) {
  this.odds = colnames(odds.p3)
  for (row in 1:nrow(odds.p3)) {
    if (odds.p3$newsite==site) {
      this.odds = rbind(this.odds,odds.p3[row,])
    }
  }
  odds.avg.p3[[site]] = this.odds
}

ntime = c(2:25)
newsort3.Phi = colnames(results3$Phi)
for (i in ntime) {
  inds.i.Phi = which(results3$Phi[,1]==i)
  for (site in 1:6) {
    inds.i.site.Phi = which(results3$Phi[inds.i.Phi,][,2]==site)
    newsort3.Phi = rbind(newsort3.Phi,results3$Phi[inds.i.site.Phi,])
  }
}
newsort3.Phi = newsort3.Phi[-1,]
newsort3.Phi = newsort3.Phi[,-which(colnames(newsort3.Phi)=="occ")]
j = which(colnames(newsort3.Phi)=="estimate")
for (col in j:ncol(newsort3.Phi)) {
  for (row in 1:nrow(newsort3.Phi)) {
    temp = as.numeric(as.character(newsort3.Phi[row,col]))
    newsort3.Phi[row,col] = round(temp,4)
  }
}

ntime = c(2:25)
newsort3.p = colnames(results3$p)
for (i in ntime) {
  inds.i.p = which(results3$p[,1]==i)
  for (site in 1:6) {
    inds.i.site.p = which(results3$p[inds.i.p,][,2]==site)
    newsort3.p = rbind(newsort3.p,results3$p[inds.i.site.p,])
  }
}
newsort3.p = newsort3.p[-1,]
newsort3.p = newsort3.p[,-which(colnames(newsort3.p)=="occ")]
j = which(colnames(newsort3.p)=="estimate")
for (col in j:ncol(newsort3.p)) {
  for (row in 1:nrow(newsort3.p)) {
    temp = as.numeric(as.character(newsort3.p[row,col]))
    newsort3.p[row,col] = round(temp,4)
  }
}

est.Phi3 = cbind(rep("Phi",nrow(results3$Phi)),results3$Phi[,-c(aPhi3:bPhi3)],
               exp(results3$Phi[,c(aPhi3:bPhi3)]))
colnames(est.Phi3)[1] = "Est. name"
est.avg.Phi3 = vector("list",length=6)
for (site in 1:6) {
  this.est = colnames(est.Phi3)
  for (row in 1:nrow(est.Phi3)) {
    if (est.Phi3$newsite==site) {
      this.est = rbind(this.est,est.Phi3[row,])
    }
  }
  est.avg.Phi3[[site]] = this.est
}

est.p3 = cbind(rep("p",nrow(results3$p)),results3$p[,-c(ap3:bp3)],
               exp(results3$p[,c(ap3:bp3)]))
colnames(est.p3)[1] = "Est. name"
est.avg.p3 = vector("list",length=6)
for (site in 1:6) {
  this.est = colnames(est.p3)
  for (row in 1:nrow(est.p3)) {
    this.est = rbind(this.est,est.p3[which(est.p3$newsite==site),])
  }
  est.avg.p3[[site]] = this.est
}

ests.3 = rbind(est.3Phi,est.3p)
colnames(ests.3) = c("Est. name",colnames(results3$Phi))

# Model 4:
model4 = turtles.models2$Phi.time.newsite.sex.p.time.newsite.sex
results4 = model4$results$reals
ncolsPhi4 = length(results4$Phi)
bPhi4 = ncolsPhi4
aPhi4 = ncolsPhi4 - 3
odds.Phi4 = cbind(results4$Phi[,-c(aPhi4:bPhi4)],exp(results4$Phi[,c(aPhi4:bPhi4)]))
odds.avgSex.Phi4 = vector("list",length=3)
for (sex in 1:3) { # 1 = male, 2 = female, 3 = unknown (juv.)
  odds.sex.Phi4 = vector("list",length=6)
  for (site in 1:6) {
    this.odds = colnames(odds.Phi4)
    for (row in 1:nrow(odds.Phi4)) {
      if (odds.Phi4$newsite==site) {
        this.odds = rbind(this.odds,odds.Phi4[row,])
      }
    }
    colnames(this.odds) = this.odds[1,]
    this.odds = this.odds[-1,]
    odds.sex.Phi4[[site]] = this.odds
  }
  odds.avgSex.Phi4[[sex]] = odds.sex.Phi4
}

ncolsp4 = length(results4$p)
bp4 = ncolsp4
ap4 = ncolsp4 - 3
odds.p4 = cbind(results4$p[,-c(ap4:bp4)],exp(results4$p[,c(ap4:bp4)]))
odds.avgSex.p4 = vector("list",length=3)
for (sex in 1:3) { # 1 = male, 2 = female, 3 = unknown (juv.)
  odds.sex.p4 = vector("list",length=6)
  for (site in 1:6) {
    this.odds = colnames(odds.p4)
    for (row in 1:nrow(odds.p4)) {
      if (odds.p4$newsite==site) {
        this.odds = rbind(this.odds,odds.p4[row,])
      }
    }
    odds.sex.p4[[site]] = this.odds
  }
  odds.avgSex.p4[[sex]] = odds.sex.p4
}

ntime = c(2:25)
newsort4.Phi = colnames(results4$Phi)
for (i in ntime) {
  inds.i.Phi = which(results4$Phi[,1]==i)
  for (site in 1:6) {
    inds.i.site.Phi = which(results4$Phi[inds.i.Phi,][,2]==site)
    for (sex in c("Male","Female","Unknown")) {
      inds.i.site.sex.Phi = which(results4$Phi[inds.i.site.Phi,][,3]==sex)
      newsort4.Phi = rbind(newsort4.Phi,results4$Phi[inds.i.site.Phi,])
    }
  }
}
newsort4.Phi = newsort4.Phi[-1,]
newsort4.Phi = newsort4.Phi[,-which(colnames(newsort4.Phi)=="occ")]
j = which(colnames(newsort4.Phi)=="estimate")
for (col in j:ncol(newsort4.Phi)) {
  for (row in 1:nrow(newsort4.Phi)) {
    temp = as.numeric(as.character(newsort4.Phi[row,col]))
    newsort4.Phi[row,col] = round(temp,4)
  }
}

ntime = c(2:25)
newsort4.p = colnames(results4$p)
for (i in ntime) {
  inds.i.p = which(results4$p[,1]==i)
  for (site in 1:6) {
    inds.i.site.p = which(results4$p[inds.i.p,][,2]==site)
    newsort4.p = rbind(newsort4.p,results4$p[inds.i.site.p,])
  }
}
newsort4.p = newsort4.p[-1,]
newsort4.p = newsort4.p[,-which(colnames(newsort4.p)=="occ")]
j = which(colnames(newsort4.p)=="estimate")
for (col in j:ncol(newsort4.p)) {
  for (row in 1:nrow(newsort4.p)) {
    temp = as.numeric(as.character(newsort4.p[row,col]))
    newsort4.p[row,col] = round(temp,4)
  }
}

est.Phi4 = cbind(rep("Phi",nrow(results4$Phi)),results4$Phi[,-c(aPhi4:bPhi4)],
               exp(results4$Phi[,c(aPhi4:bPhi4)]))
colnames(est.Phi4)[1] = "Est. name"
est.avgSex.Phi4 = vector("list",length=3)
for (sex in 1:3) { # 1 = male, 2 = female, 3 = unknown (juv.)
  est.sex.Phi4 = vector("list",length=6)
  for (site in 1:6) {
    this.est = colnames(est.Phi4)
    for (row in 1:nrow(est.Phi4)) {
      if (est.Phi4$newsite==site) {
        this.est = rbind(this.est,est.Phi4[row,])
      }
    }
    est.sex.Phi4[[site]] = this.est
  }
  est.avgSex.Phi4[[sex]] = est.sex.Phi4
}

est.p4 = cbind(rep("p",nrow(results3$p)),results3$p[,-c(ap4:bp4)],
               exp(results3$p[,c(ap4:bp4)]))
colnames(est.p4)[1] = "Est. name"
est.avgSex.p4 = vector("list",length=3)
for (sex in 1:3) { # 1 = male, 2 = female, 3 = unknown (juv.)
  est.sex.p4 = vector("list",length=6)
  for (site in 1:6) {
    this.est = colnames(est.p4)
    for (row in 1:nrow(est.p4)) {
      if (est.p4$newsite==site) {
        this.est = rbind(this.est,est.p4[row,])
      }
    }
    est.sex.p4[[site]] = this.est
  }
  est.avgSex.p4[[sex]] = est.sex.p4
}

##

timevals.Phi = c(1991:2017)
timevals.p = c(1991:2017)
phi2 = c()
p2 = c()
phi3 = c()
p3 = c()
phi4 = c()
p4 = c()
phi5 = c()
p5 = c()
for (year in 1:25) {
  Phi2.year = sum(as.numeric(results2$Phi[1:year])) 
  p2.year = sum(as.numeric(results2$p[1:year]))
  timevals.Phi = rbind(timevals.Phi,Phi2.year)
  timevals.p = rbind(timevals.p,p2.year)
  Phi3.year = sum(as.numeric(results3$Phi[1:year])) 
  p3.year = sum(as.numeric(results3$p[1:year]))
  timevals.Phi = rbind(timevals.Phi,Phi3.year)
  timevals.p = rbind(timevals.p,p3.year)
  Phi4.year = sum(as.numeric(results4$Phi[1:year])) 
  p4.year = sum(as.numeric(results4$p[1:year]))
  timevals.Phi = rbind(timevals.Phi,Phi4.year)
  timevals.p = rbind(timevals.p,p4.year)
  Phi5.year = sum(as.numeric(results5$Phi[1:year])) 
  p5.year = sum(as.numeric(results5$p[1:year]))
  timevals.Phi = rbind(timevals.Phi,Phi5.year)
  timevals.p = rbind(timevals.Phi,p5.year)
}

colnames(timevals.Phi) = timevals.Phi[1,]
timevals.Phi = timevals.Phi[-1,]
colnames(timevals.p) = timevals.p[1,]
timevals.p = timevals.p[-1,]

likel = c(model1$results$neg2lnl,model2$results$neg2lnl,model3$results$neg2lnl,
          model4$results$neg2lnl,model5$results$neg2lnl)
AIC = c(model1$results$AIC,model2$results$AIC,model3$results$AIC,
        model4$results$AIC,model5$results$AIC)

timevals.Phi = c("beta_0",as.character(c(2:25)),"newsite",as.character(c(2:25)),
                 "sexmale")
timevals.p = c("beta_0",as.character(c(2:25)),"newsite",as.character(c(2:25)))
for (j in 1:4) { # the first for the year is the coefficient, the second is the term
  model = model_turtles[[j]][[2]]
  coefs.beta.phi = unname(model$reals$Phi)
  vals.phi = c()
  coefs.beta.p = unname(model_turtles[[j]][[2]]$reals$p)
  vals.p = c()
  for (y in 2:25) {
    beta.phi.y = coefs.beta.phi[1] + sum(coefs.beta.phi[2:y])
    vals.phi = append(vals.phi,beta.phi.y,length(vals.phi))
    beta.p.y = coefs.beta.p[1] + sum(coefs.beta.p[2:y])
    vals.p = append(vals.p,beta.p.y,length(vals.p))
  }
  coef.sexM.phi = unname(model_turtles[[j]][[2]]$reals$Phi[27])
  coefs.phi = c(coefs.beta.phi,vals.phi)
  timevals.Phi = rbind(timevals.Phi,coefs.phi)
  coefs.p = c(coefs.beta.phi,vals.p)
  timevals.p = rbind(timevals.p,coefs.p)
}

timevals.p = timevals.p[,-51]

for (col in 27:50) {
  timevals.Phi[1,col] = paste("Phi^ value for year",col-25)
  timevals.p[1,col] = paste("p^ value for year",col)
}
colnames(timevals.Phi) = timevals.Phi[1,]
colnames(timevals.p) = timevals.p[1,]
timevals.Phi = timevals.Phi[-1,]
timevals.p = timevals.p[-1,]

timevals.Phi = as.matrix(timevals.Phi)
timevals.p = as.matrix(timevals.p)

class(timevals.Phi) = 'numeric'
class(timevals.p) = 'numeric'
rownames(timevals.both) = c("Model 1","Model 2","Model 3","Model 4")
colnames(timevals.both) = c("Phi","p","-2 log L.","AIC")
