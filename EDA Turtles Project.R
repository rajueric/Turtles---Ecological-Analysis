library(readxl)
install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(ggplot2)
install.packages("pandas")
library(pandas)

# Helper functions
cleanTable <- function(table) {
  # A function to transform a csv file into a table.
  clean.table <- c()
  for (row in 1:length(table)) {
    this.row <- strsplit(table[row],"\t")
    clean.table <- rbind(clean.table,as.character(this.row[[1]]))
  }
  clean.table
}

cleanChars <- function(table=clean.table) {
  # Convert all the numerical values to numeric type.
  n < nrow(clean.table)
  m <- ncol(clean.table)
  for (i in 1:n) {
    for (j in 1:m) {
      if (!(is.na(clean.table[i,j]))) {
        num <- as.numeric(clean.table[i,j])
        clean.table[i,j] <- num
      }
    }
  }
  clean.table
}

countUnique <- function(lst) {
  #Return each element in lst exactly once
  unique <- c()
  for (item in lst) {
    if (!(item%in%unique)) {
      unique <- append(unique,item,length(unique))
    }
  }
  unique
} 

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


my.file <- readxl::read_xlsx(path="C:/Users/ericr/OneDrive/Documents/UNIVERSITY/U OF T/COURSES/2020-2021/Fall 2020/STA490Y1/turtles_site.xlsx")

notch.corrected <- my.file$notch
n <- nrow(my.file)
notch.flags <- rep(FALSE,n)
for (i in 1:n) {
  any <- FALSE
  for (letter in c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S",
                   "T","U","V","W","X","Y","Z")) {
    if (stringr::str_count(my.file$notch[i],letter)>=1) {
      any <- TRUE
    }
  }
  if (stringr::str_count(my.file$notch[i],letter)==1) {
    temp <- notch.corrected[i]
    notch.corrected[i] <- substr(temp,start=1,stop=nchar(temp)-1) # remove the letter
  }
  notch.flags[i] <- any
}

sites <- countUnique(my.file$site)
goodsites <- c("Arowhon Rd.","Sims Creek","Maiden Lake","Road to WHP","Blanding's Bog",
"Wolf Howl Pond","Wolf Howl Pond E.","Road to Wrose","West Rose","March Hare Lake")

# Strange indices and naming
my.file$'notch-correction' <- notch.corrected
my.file$notch.flags <- notch.flags

#####

strange1 <- my.file[index(my.file$notch.flags,TRUE,k=1,all=TRUE),]
strange1 <- rbind(strange1[1,],strange1[4,])
nrow1 <- nrow(strange1)
str.rows1 <- index(my.file$notch.flags,TRUE,k=1,all=TRUE)

strange1.notches <- list(rep(0,10))
for (i in 1:length(goodsites)) {
  this.notch <- strange1$`notch-correction`[index(strange2$site,goodsites[i],k=1,all=TRUE)]
  strange1.notches[i][[1]] <- this.notch
}

strange1.notch2 <- list(rep(0,6))
for (j in 1:8) {
  if (j%in%c(1,2,3)) {
    strange1.notch2[j][[1]] <- strange1.notches[j][[1]]
  }
  else if (j==4) {
    strange1.notch2[4][[1]] <- c(strange1.notches[4][[1]],strange1.notches[5][[1]])
  }
  else if (j==6) {
    strange1.notch2[5][[1]] <- c(strange1.notches[6][[1]],strange1.notches[7][[1]])
  }
  else if (j==8) {
    strange1.notch2[6][[1]] <- c(strange1.notches[8][[1]],strange1.notches[9][[1]],strange1.notches[10][[1]])
  }
}

#####

strange2 <- my.file[index(my.file$'notch-correction',"0000",k=1,all=TRUE),]
nrow2 <- nrow(strange2)
str.rows2 <- index(my.file$'notch-correction',"0000",k=1,all=TRUE)

strange2.notches <- list(rep(0,10))
for (i in 1:length(goodsites)) {
  this.notch <- strange2$`notch-correction`[index(strange2$site,goodsites[i],k=1,all=TRUE)]
  strange2.notches[i][[1]] <- this.notch
}

strange2.notch2 <- list(rep(0,6))
for (j in 1:8) {
  if (j%in%c(1,2,3)) {
    strange2.notch2[j][[1]] <- strange2.notches[j][[1]]
  }
  else if (j==4) {
    strange2.notch2[4][[1]] <- c(strange2.notches[4][[1]],strange2.notches[5][[1]])
  }
  else if (j==6) {
    strange2.notch2[5][[1]] <- c(strange2.notches[6][[1]],strange2.notches[7][[1]])
  }
  else if (j==8) {
    strange2.notch2[6][[1]] <- c(strange2.notches[8][[1]],strange2.notches[9][[1]],strange2.notches[10][[1]])
  }
}

#####

stranged <- my.file[index(my.file$'notch-correction',"DEAD",k=1,all=TRUE),]
nrowd <- nrow(stranged)
str.rowsd <- index(my.file$'notch-correction',"DEAD",k=1,all=TRUE)

stranged.notches <- list(rep(0,10))
for (i in 1:length(goodsites)) {
  this.notch <- stranged$`notch-correction`[index(stranged$site,goodsites[i],k=1,all=TRUE)]
  stranged.notches[i][[1]] <- this.notch
}

stranged.notch2 <- list(rep(0,6))
for (j in 1:8) {
  if (j%in%c(1,2,3)) {
    stranged.notch2[j][[1]] <- stranged.notches[j][[1]]
  }
  else if (j==4) {
    stranged.notch2[4][[1]] <- c(stranged.notches[4][[1]],stranged.notches[5][[1]])
  }
  else if (j==6) {
    stranged.notch2[5][[1]] <- c(stranged.notches[6][[1]],stranged.notches[7][[1]])
  }
  else if (j==8) {
    stranged.notch2[6][[1]] <- c(stranged.notches[8][[1]],stranged.notches[9][[1]],stranged.notches[10][[1]])
  }
}

#####

good <- my.file[-c(str.rows1,str.rows2,str.rowsd),] # the "good" data

good.notches <- list(rep(0,10))
for (i in 1:length(goodsites)) {
  this.notch <- good$`notch-correction`[index(good$site,goodsites[i],k=1,all=TRUE)]
  good.notches[i][[1]] <- this.notch
}

good.notch2 <- list(rep(0,6))
for (j in 1:8) {
  if (j%in%c(1,2,3)) {
    good.notch2[j][[1]] <- good.notches[j][[1]]
  }
  else if (j==4) {
    good.notch2[4][[1]] <- c(good.notches[4][[1]],good.notches[5][[1]])
  }
  else if (j==6) {
    good.notch2[5][[1]] <- c(good.notches[6][[1]],good.notches[7][[1]])
  }
  else if (j==8) {
    good.notch2[6][[1]] <- c(good.notches[8][[1]],good.notches[9][[1]],good.notches[10][[1]])
  }
}

flag.logic <- c()
nall <- nrow(my.file)
for (row in 1:nall) {
  flag.logic <- append(flag.logic,identity(my.file$notch.flags[row],val=TRUE),length(flag.logic))
}

good.nums <- c()
for (k in 1:6) {
  good.nums <- append(good.nums,good.notch2[k][[1]],length(good.nums))
}
good.nums <- as.numeric(good.nums)
good.all <- my.file[index(my.file$notch.flags,item=FALSE,k=1,all=TRUE),]
strange.all <- my.file[-index(my.file$notch,good.nums,k=1,all=TRUE),]

# Editing:
for (k in 1:3) {
  strange1.notch2[k][[1]] <- NA
  strange2.notch2[k][[1]] <- NA
  stranged.notch2[k][[1]] <- NA
}
strange1.notch2[4][[1]] <- "OOOA"
strange2.notch2[4][[1]] <- rep("0000",4)
stranged.notch2[4][[1]] <- NA
for (k in 5:6) {
  strange1.notch2[k][[1]] <- strange1.notch2[k][[1]][1]
  stranged.notch2[k][[1]] <- stranged.notch2[k][[1]][1]
}
strange2.notch2[5][[1]] <- rep("0000",10)
strange2.notch2[6][[1]] <- NA

sites.matrix <- matrix(0,ncol=6,nrow=4)
colnames(sites.matrix) <- c(goodsites[1:3],paste(goodsites[4],goodsites[5]),
                                 paste(goodsites[6],goodsites[7]),
                                 paste(goodsites[8],goodsites[9],goodsites[10]))
rownames(sites.matrix) <- c("Good","OOOA or OOOB","OOOO","DEAD")
for (snum in 1:6) {
  sites.matrix[1,snum] <- length(good.notch2[snum][[1]])
  scond <- identity(is.na(strange2.notch2[snum][[1]][1]),TRUE)
  sites.matrix[2,snum] <- length(strange1.notch2[snum][[1]])*scond
  sites.matrix[3,snum] <- length(strange2.notch2[snum][[1]])*scond
  sites.matrix[4,snum] <- length(stranged.notch2[snum][[1]])*scond
}

typeofstrange <- sites.matrix[1,]
colsum <- c()
for (snum in 1:6) {
  this.col <- sites.matrix[2,snum]+sites.matrix[3,snum]+sites.matrix[4,snum]
  colsum <- append(colsum,this.col,length(colsum))
}
typeofstrange <- rbind(typeofstrange,colsum)
typeofstrange <- rbind(typeofstrange,(typeofstrange[2,]/typeofstrange[1,]))
colnames(typeofstrange) <- colnames(sites.matrix)
rownames(typeofstrange) <- c("Good","Strange","Fraction strange")

fract <- table(typeofstrange[3,])

# Plotting

# 1. Where are the weird data?

shortsites <- colnames(typeofstrange)
strange.data <- as.data.frame(t(typeofstrange))

ggplot(data=strange.data,aes(x=shortsites,y=`Fraction strange`))+geom_point()+ggtitle("Weird cases by site")

# 2. Where are the weird data?
# 3. Proportion of weird data
# 4. Probability of first capture by year--was there a bias?

ggplot(data=my.file,aes(x=`Year of birth`,y=firstcap))+geom_point()+geom_line()+ggtitle("First capture vs. year of birth")

# 5. How many were captured for a given date with a certain year of birth?

all.years <- sort(countUnique(my.file$Year))
year.to.birth <- c()
var.birth <- c()
for (year in all.years) {
  temp <- my.file[index(my.file$Year,year,k=1,all=TRUE),]$`Year of birth`
  if (index(all.years,year,k=1,all=FALSE)==3) {
    print(temp[length(temp)])
  }
  year.data <- c()
  for (item in temp) {
    if (!(is.na(item))) {
      year.data <- append(year.data,item,length(year.data))
    }
  }
  year.mean <- mean(year.data)
  this.birth <- max(year.data)-min(year.data)
  year.to.birth <- append(year.to.birth,year.mean,length(year.to.birth))
  var.birth <- append(var.birth,this.birth,length(var.birth))
}

# Note, the value for 1989 has NA so use the previous years' last entry.
# This is 1968 -->
year.to.birth[4] <- 1968

births <- sort(countUnique(my.file$`Year of birth`))

ggplot(data=NULL,aes(x=all.years,y=year.to.birth))+geom_point()+geom_line()+
  ggplot("Birth ranges per given year")

# Excluding the 1984 data, which seems to be an outlier, as well as the 1989 data
ggplot(data=NULL,aes(x=all.years[-c(2,4)],y=year.to.birth[-c(2,4)]))+geom_point()+geom_line()+
  ggplot("Birth ranges per given year, excluding possible outliers")


# 6. year of birth vs. age --> is the age estimate good?

ggplot(data=my.file,aes(x=`Year of birth`,y=age))+geom_point()+geom_line()+ggtitle("Age vs. year of birth")

# Comparing known versus unknowns:
known <- my.file[index(my.file$`known age`,TRUE,k=1,all=TRUE),]
unknown <- my.file[index(my.file$`known age`,FALSE,k=1,all=TRUE),]

ggplot(data=known,aes(x=`Year of birth`,y=age))+geom_point(col="green")+geom_line(col="green")+
  geom_point(data=unknown,aes(x=`Year of birth`,y=age),col="yellow")+
  geom_line(data=unknown,aes(x=`Year of birth`,y=age),col="yellow")+ggtitle("Age vs. year of birth, partitioned")

# 7. What are the sources of missing values?
nas <- c()
for (col in 1:ncol(my.file)) {
  nas <- append(nas,sum(is.na(my.file[,col]),length(nas)))
}

# The two columns with the largest number of missing values, both over 5000, are the year of first and second nest dates.
# The second nest date column is emptier because by definition one needs a first nest date beforehand.

# The next largest NA columns are those corresponding to year of birth and age, with both over 3000 counts.

# The other columns with significant (i.e. over 100) missing data are the columns 18-28 which contain measurements.

# 8. To see the impact of missing values, consider the year of birth and age versus first and second nest date.

ggplot(data=known,aes(x=`Year of birth`,y=nestdate))+geom_point(col="green")+geom_line(col="green")+
  geom_point(data=unknown,aes(x=`Year of birth`,y=nestdate),col="yellow")+
  geom_line(data=unknown,aes(x=`Year of birth`,y=nestdate),col="yellow")+ggtitle("First nest date vs. year of birth")

ggplot(data=known,aes(x=`Year of birth`,y=nest2dat))+geom_point(col="green")+geom_line(col="green")+
  geom_point(data=unknown,aes(x=`Year of birth`,y=nest2dat),col="yellow")+
  geom_line(data=unknown,aes(x=`Year of birth`,y=nest2dat),col="yellow")+ggtitle("Second nest date vs. year of birth")

# 9. The measurements:
# (a):

ggplot(data=my.file,aes(x=Year,y=mass))+geom_point()+geom_line()+ggtitle("Mass vs. year")
ggplot(data=my.file,aes(x=Year,y=cl))+geom_point()+geom_line()+ggtitle("Cl vs. year")
ggplot(data=my.file,aes(x=Year,y=pl))+geom_point()+geom_line()+ggtitle("Midcl vs. year")
ggplot(data=my.file,aes(x=Year,y=midcl))+geom_point()+geom_line()+ggtitle("Pl vs. year")
ggplot(data=my.file,aes(x=Year,y=midpl))+geom_point()+geom_line()+ggtitle("Midpl vs. year")
ggplot(data=my.file,aes(x=cl,y=midcl))+geom_point()+geom_line()+ggtitle("Cl vs. midcl")
ggplot(data=my.file,aes(x=pl,y=midpl))+geom_point()+geom_line()+ggtitle("Pl vs. midpl")
ggplot(data=my.file,aes(x=cl,y=pl))+geom_point()+geom_line()+ggtitle("Cl vs. pl")
ggplot(data=my.file,aes(x=Year,y=lclaw))+geom_point(col="red")+geom_line(col="red")+ggtitle("Lclaw vs. year")+
  geom_point(data=my.file,aes(x=Year,y=rclaw),col="blue")+geom_line(data=my.file,aes(x=Year,y=rclaw),col="blue")+
  ggtitle("Rclaw vs. year")
ggplot(data=my.file,aes(x=Year,y=cw))+geom_point()+geom_line()+ggtitle("Cw vs. year")
ggplot(data=my.file,aes(x=Year,y=ch))+geom_point()+geom_line()+ggtitle("Ch vs. year")
ggplot(data=my.file,aes(x=Year,y=dead))+geom_point()+geom_line()+ggtitle("Deaths vs. year")
ggplot(data=my.file,aes(x=name,y=site))+geom_point()+geom_line()+ggtitle("Researchers by site")

deathssite <- c()
for (this.site in goodsites) {
  deathssite <- append(deathssite,sum(my.file[index(my.file$site,this.site,k=1,all=TRUE),]$dead))
}
ggplot(data=my.file[which(my.file$dead==TRUE),],aes(site))+geom_bar()+ggtitle("Deaths by site")

## So... what to do with the missing measurement data? Most of the missing values occured in the first four data points,
# so perhaps we want to eliminate everything that occured during this period.

clean.file <- my.file[-which(my.file$Year%in%c(1978,1984,1985,1989)),]

# Furthermore, we should eliminate any data with missing cl, pl, midcl or midpl as this will bias any estimate to age.
# Consider which years, sites and researchers are more likely to have this missing data.

clean.file <- my.file[-which(my.file$Year%in%c(1978,1984,1985,1989)),]
clpl.out <- c()
bad.clpl <- c()
for (row in 1:nrow(my.file)) {
  if (!((is.na(clean.file$cl[row]))||(is.na(clean.file$pl[row]))||
        (is.na(clean.file$midcl[row]))||(is.na(clean.file$midpl[row])))) {
    clpl.out <- rbind(clpl.out,my.file[row,])
  }
  else {
    bad.clpl <- rbind(bad.clpl,my.file[row,])
  }
}

clean.file <- clpl.out

ggplot(data=bad.clpl,aes(site))+geom_histogram(stat="count")+
  ggtitle("Missing measurements, preventing a good age estimate")

# What does this do to the probability of capture?

ggplot(data=clean.file,aes(x=`Year of birth`,y=firstcap))+geom_point()+geom_line()+
  ggtitle("First capture vs. year of birth, revised")

# Let's go back to the estimates of age.

known2 <- clean.file[index(my.file$`known age`,TRUE,k=1,all=TRUE),]
unknown2 <- clean.file[index(my.file$`known age`,FALSE,k=1,all=TRUE),]

ggplot(data=known2,aes(x=`Year of birth`,y=nestdate))+geom_point(col="green")+geom_line(col="green")+
  geom_point(data=unknown2,aes(x=`Year of birth`,y=nestdate),col="yellow")+
  geom_line(data=unknown2,aes(x=`Year of birth`,y=nestdate),col="yellow")+
  ggtitle("First nest date vs. year of birth, revised")

ggplot(data=known2,aes(x=`Year of birth`,y=nest2dat))+geom_point(col="green")+geom_line(col="green")+
  geom_point(data=unknown2,aes(x=`Year of birth`,y=nest2dat),col="yellow")+
  geom_line(data=unknown2,aes(x=`Year of birth`,y=nest2dat),col="yellow")+ggtitle("Second nest date vs. year of birth")

