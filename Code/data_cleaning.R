#Analysis 2:
#data cleaning process

#meaningful column names
dat <- read.table("../data/hurdat2022.txt", header = FALSE, sep = ",",  dec = ".", fill=TRUE)
cnames <- read.table("../data/col_names.txt", sep = ",", fill=TRUE)
colnames(dat) <- cnames

#ID column
#name column
ID <- rep(NA, nrow(dat))
Name<-rep(NA, nrow(dat))
a <- c()

for (j in 1:length(ID)) {
  if (grepl("AL", dat$Date[j])) {
    hur_code <- dat$Date[j]
    hur_name<-dat$` Time`[j]
    a <- c(a, j)
  } else {
    ID[j] <- hur_code
    Name[j]<- hur_name
  }
}
dat <- data.frame(ID,Name,dat)

colnames(dat[1]) <- "ID"
colnames(dat[2]) <- "Name"
dat <- dat[-a, ]

#remove the index column
rownames(dat) <- NULL

#change longitudes into direction E, latitudes into direction N
for (j in 1:length(dat$ID)) {
  new_long<-substr(dat$X.Longitude[j], 1, nchar(dat$X.Longitude[j])-1)
  num_long<-as.numeric(new_long)
  if (grepl("W", dat$X.Longitude[j])) {
    dat$X.Longitude[j]<--num_long
  }else{
    dat$X.Longitude[j]<-num_long
  }
  new_lai<-substr(dat$X.Latitude[j], 1, nchar(dat$X.Latitude[j])-1)
  num_lai<-as.numeric(new_lai)
  if (grepl("S", dat$X.Latitude[j])) {
    dat$X.Latitude[j]<--num_lai
  }else{
    new_lai<-substr(dat$X.Latitude[j], 1, nchar(dat$X.Latitude[j])-1)
    num_lai<-as.numeric(new_lai)
    dat$X.Latitude[j]<-num_lai
  }

}

#change coordinates into numeric columns
dat$X.Longitude <- as.double(dat$X.Longitude)
dat$X.Latitude <- as.double(dat$X.Latitude)
dat

#get rid of NAs
dat[dat==-999]<-NA

#we found this typos in the dataset which would affect interpolation
dat[28291,3]<-"19661006"
dat[28292,3]<-"19661006"
dat[28293,3]<-"19661006"
dat[28294,3]<-"19661006"
#ID="AL121969"
dat[29392:29395,3]<-"19690827"
dat[29396:29397,3]<-"19690828"
save(dat, file = "dat.Rdata")
