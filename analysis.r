readUsers <- function(path="base2/arabiaSaudita/profilesArabia.dat") {
  fullPath <- paste("~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/datasets/", path, sep="")
  saU <- read.csv(fullPath, header=F, sep="\t")
  colnames(saU) <- c("idUserFoursquare", "user", "userLocal", "gender")
  return(saU)
}

readCheckIns <- function(path="base2/arabiaSaudita/Saudi-Arabia.txt") {
  fullPath <- paste("~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/datasets/", path, sep="")
  sa <- read.csv(fullPath, header=F, sep="\t")
  colnames(sa) <- c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                     "subcategory", "category", "city", "country")
  return(sa)
}

cleanUsers <- function(users) {
  return(users[grep("Saudi|Mecca|Medina|Riya|Jedda", ignore.case=T, users$userLocal), ])
}

joinCheckInsWithProfiles <- function(checkIns, profiles) {
  query <- paste("Select * From", checkIns, "JOIN", profiles, "using(idUserFoursquare)")
  return(sqldf(query))
}

categoriesByGender <- function(table, gender, uniqueUsers=FALSE, subcategory=FALSE) {
  category <- "category"
  if(subcategory==TRUE) {
    category <- "subcategory"
  }
  queryString <- paste("Select *, count(*) as count from ", table,
                     " where gender='", gender, "' group by ", category, sep="")
  if(uniqueUsers==TRUE) {
    queryString <- paste("Select *, count(*) as CCount from  (select * from ", table,
                      " where gender='", gender, "' group by user, ",
                       category, " ) group by ",  category, sep="")
  }
  return(sqldf(queryString))
}


##########
# run
##########

sa <- readCheckIns()
cSaU <- cleanUsers(readUsers())

joined <- joinCheckInsWithProfiles("cSaU", "sa")
tableString <- deparse(substitute(joined))
saMC <- categoriesByGender(tableString, "male")
saFC <- categoriesByGender(tableString, "female")
saUMC <- categoriesByGender(tableString, "male", uniqueUsers=TRUE)
saFMC <- categoriesByGender(tableString, "female", uniqueUsers=TRUE)

saMS <- categoriesByGender(tableString, "male", subcategory=TRUE)
saFS <- categoriesByGender(tableString, "female", subcategory=TRUE)
saUMS <- categoriesByGender(tableString, "male", subcategory=TRUE, uniqueUsers=TRUE)
saUFS <- categoriesByGender(tableString, "female", subcategory=TRUE, uniqueUsers=TRUE)


## normalization
saMC$count <- saMC$count/sum(saMC$count)
saFC$count <- saFC$count/sum(saFC$count)

saMS$count <- saMS$count/sum(saMS$count)
saFS$count <- saFS$count/sum(saFS$count)

### normalization – unique users
saUMC$CCount <- saUMC$CCount/sum(saUMC$CCount)
saUFC$CCount <- saUFC$CCount/sum(saUFC$CCount)

saUMS$CCount <- saUMS$CCount/sum(saUMS$CCount)
saUFS$CCount <- saUFS$CCount/sum(saUFS$CCount)

## correlation categories
### counting check-ins
plot(saMC$count, saFC$count, main="Correlation Categories counting check-ins", xlab="Male", ylab="Female")
abline(0, 1, col="red")
text(saMC$count, saFC$count, labels=saMC$category, pos=3)
cor.test(saFC$count, saMC$count)

### counting check-ins by unique-users (counting every user once)
plot(saUMC$CCount, saUFC$CCount, main="Correlation Categories counting unique users", xlab="Male", ylab="Female")
abline(0, 1, col="red")
text(saUMC$CCount, saUFC$CCount, labels=saUMC$category, pos=3)
cor.test(saUMC$CCount, saUFC$CCount)

chisq.test(saUMC$CCount, saUFC$CCount)

## complete subcategories
temp <- sqldf("Select * from saMS where subcategory not in (Select subcategory from saFS)")
temp$count=0
saFS <- rbind(saFS, temp)

temp <- sqldf("Select * from saFS where subcategory not in (Select subcategory from saMS)")
temp$count=0
saMS <- rbind(saMS, temp)

saFS <- saFS[ order(saFS$subcategory), ]
saMS <- saMS[ order(saMS$subcategory), ]

### with unique users
temp <- sqldf("Select * from saUMS where subcategory not in (Select subcategory from saUFS)")
temp$CCount=0
saUFS <- rbind(saUFS, temp)

temp <- sqldf("Select * from saUFS where subcategory not in (Select subcategory from saUMS)")
temp$CCount=0
saUMS <- rbind(saUMS, temp)

saUFS <- saUFS[ order(saUFS$subcategory), ]
saUMS <- saUMS[ order(saUMS$subcategory), ]

## normalize subcategories
saFSR <- saFS; saMSR <- saMS
saFSR$count <- saFSR$count/sum(saFSR$count)
saMSR$count <- saMSR$count/sum(saMSR$count)

### with unique users
saUFSR <- saUFS; saUMSR <- saUMS
saUMSR$CCount <- saUMSR$CCount/sum(saUMSR$CCount)
saUFSR$CCount <- saUFSR$CCount/sum(saUFSR$CCount)

## correlate subcategories
plot(saMSR$count, saFSR$count, main="Correlation Subcategories by Gender, Counting check-ins", xlab="Male", ylab="Female")
abline(0,1, col="red")
text(saMSR$count, saFSR$count, labels=saMSR$subcategory, pos=3)
cor.test(saMC$count, saFC$count)


### with unique users
plot(saUMSR$CCount, saUFSR$CCount, main="Correlation Subcategories by Gender, Counting Unique Users", xlab="Male", ylab="Female")
abline(0,1, col="red")
text(saUMSR$CCount, saUFSR$CCount, labels=saUMSR$subcategory, pos=3)
cor.test(saUMSR$CCount, saUFSR$CCount)
