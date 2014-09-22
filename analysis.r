library(sqldf)

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

cleanUsers <- function(users, filter="Saudi|Mecca|Medina|Riya|Jedda") {
  return(users[grep(filter, ignore.case=T, users$userLocal), ])
}

joinCheckInsWithProfiles <- function(checkIns, profileTable) {
  query <- paste("Select * from", checkIns, "JOIN", profileTable, "using(idUserFoursquare)")
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
    queryString <- paste("Select *, count(*) as count from  (select * from ", table,
                      " where gender='", gender, "' group by user, ",
                       category, " ) group by ",  category, sep="")
  }
  return(sqldf(queryString))
}

normalizeByAbsolutePercentage <- function(attribute){
  return(attribute/sum(attribute))
}

normalizeByPercentageOfMax <- function(attribute) {
  return(attribute/max(attribute))
}

correlateCategories <- function(x, y, labels, country="Saudi Arabia", countMethod="check-ins", categories="Categories") {
  plot(x, y, main=paste("Correlation of", categories, "counting", countMethod, "in", country),
       xlab="Male", ylab="Female")
  abline(0, 1, col="red")
  text(x, y, labels=labels, pos=3)
  print(cor.test(x, y))
  print(chisq.test(x, y))
}

selectNotPresentFromOtherGender <- function(x, y) {
  return(sqldf(paste("Select * from", x, "where subcategory not in (Select subcategory from", y,")")))
}

completeSubcategories <- function(gender1, gender2, gender1String, gender2String) {
  temp <- selectNotPresentFromOtherGender(gender1String, gender2String)
  temp$count <- 0
  gender2<-rbind(gender2, temp)
  return( gender2[ order(gender2$subcategory), ] )
}

##########
# run
##########

ci <- readCheckIns(path="base2/Sweden.txt")
franceFilter <- "Paris|France|Metz|Bordeaux|Marseille|Midi-Py|Strasbourg|Lyon"
swedenFilter <- "Sverige|Sweden|Stockholm|Malmö"
users <- readUsers(path="base2/profileFiltredGermanyFranceEmiratesSweden.dat")
profiles <- cleanUsers(users, filter=swedenFilter)

joined <- joinCheckInsWithProfiles("ci", "profiles")
tableString <- "joined"

#categories
maleC <- categoriesByGender(tableString, "male")
femaleC <- categoriesByGender(tableString, "female")
maleUniqueC <- categoriesByGender(tableString, "male", uniqueUsers=TRUE)
femaleUniqueC <- categoriesByGender(tableString, "female", uniqueUsers=TRUE)

#subcategories
maleSubC <- categoriesByGender(tableString, "male", subcategory=TRUE)
femaleSubC <- categoriesByGender(tableString, "female", subcategory=TRUE)
maleUniqueSubC <- categoriesByGender(tableString, "male", subcategory=TRUE, uniqueUsers=TRUE)
femaleUniqueSubC <- categoriesByGender(tableString, "female", subcategory=TRUE, uniqueUsers=TRUE)

femaleSubC <- completeSubcategories(maleSubC, femaleSubC, "maleSubC", "femaleSubC")
maleSubC <- completeSubcategories(femaleSubC, maleSubC, "femaleSubC", "maleSubC")
femaleUniqueSubC <- completeSubcategories(maleUniqueSubC, femaleUniqueSubC, "maleUniqueSubC", "femaleUniqueSubC")
maleUniqueSubC <- completeSubcategories(femaleUniqueSubC, maleUniqueSubC, "femaleUniqueSubC", "maleUniqueSubC")

## normalization - counting check-ins
maleC$count <- normalizeByAbsolutePercentage(maleC$count)
femaleC$count <- normalizeByAbsolutePercentage(femaleC$count)
maleSubC$count <- normalizeByAbsolutePercentage(maleSubC$count)
femaleSubC$count <- normalizeByAbsolutePercentage(femaleSubC$count)

### normalization – unique users
maleUniqueC$count <- normalizeByAbsolutePercentage(maleUniqueC$count)
femaleUniqueC$count <- normalizeByAbsolutePercentage(femaleUniqueC$count)
maleUniqueSubC$count <- normalizeByAbsolutePercentage(maleUniqueSubC$count)
femaleUniqueSubC$count <- normalizeByAbsolutePercentage(femaleUniqueSubC$count)

## correlation categories
country <- "Sweden"
correlateCategories(maleC$count, femaleC$count, maleC$category, country=country)
correlateCategories(maleUniqueC$count, femaleUniqueC$count, maleC$category, country=country,
                    countMethod="unique users")

correlateCategories(maleSubC$count, femaleSubC$count, maleSubC$subcategory, country=country,
                    categories="Subcategories")
correlateCategories(maleUniqueSubC$count, femaleUniqueSubC$count, maleUniqueSubC$subcategory, country=country,
                    categories="Subcategories", countMethod="unique users")
