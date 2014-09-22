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

correlateCategories <- function(x, y, labels, countMethod="check-ins", categories="Categories") {
  plot(x, y, main=paste("Correlation of", categories, "counting", countMethod), xlab="Male", ylab="Female")
  abline(0, 1, col="red")
  text(x, y, labels=labels, pos=3)
  cor.test(x, y)
  chisq.test(x, y)
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

sa <- readCheckIns()
cSaU <- cleanUsers(readUsers())

joined <- joinCheckInsWithProfiles("cSaU", "sa")
tableString <- "joined"
saMC <- categoriesByGender(tableString, "male")
saFC <- categoriesByGender(tableString, "female")
saUMC <- categoriesByGender(tableString, "male", uniqueUsers=TRUE)
saUFC <- categoriesByGender(tableString, "female", uniqueUsers=TRUE)

saMS <- categoriesByGender(tableString, "male", subcategory=TRUE)
saFS <- categoriesByGender(tableString, "female", subcategory=TRUE)
saUMS <- categoriesByGender(tableString, "male", subcategory=TRUE, uniqueUsers=TRUE)
saUFS <- categoriesByGender(tableString, "female", subcategory=TRUE, uniqueUsers=TRUE)

## normalization
saMC$count <- normalizeByAbsolutePercentage(saMC$count)
saFC$count <- normalizeByAbsolutePercentage(saFC$count)
saMS$count <- normalizeByAbsolutePercentage(saMS$count)
saFS$count <- normalizeByAbsolutePercentage(saFS$count)

### normalization – unique users
saUMC$count <- normalizeByAbsolutePercentage(saUMC$count)
saUFC$count <- normalizeByAbsolutePercentage(saUFC$count)
saUMS$count <- normalizeByAbsolutePercentage(saUMS$count)
saUFS$count <- normalizeByAbsolutePercentage(saUFS$count)

## correlation categories
correlateCategories(saMC$count, saFC$count, saMC$category)
correlateCategories(saUMC$count, saUFC$count, saMC$category, countMethod="unique users")

saFS <- completeSubcategories(saMS, saFS, "saMS", "saFS")
saMS <- completeSubcategories(saFS, saMS, "saFS", "saMS")
saUFS <- completeSubcategories(saUMS, saUFS, "saUMS", "saUFS")
saUMS <- completeSubcategories(saUFS, saUMS, "saUFS", "saUMS")

stopifnot(length(saUFS$count) == length(saUMS$count))

correlateCategories(saMS$count, saFS$count, saMS$subcategory, categories="Subcategories")
correlateCategories(saUMS$count, saUFS$count, saUMS$subcategory, categories="Subcategories")

## normalize subcategories
saFSR <- saFS; saMSR <- saMS
saFSR$count <- normalizeByAbsolutePercentage(saFSR$count)
saMSR$count <- normalizeByAbsolutePercentage(saMSR$count)

saUFSR <- saUFS; saUMSR <- saUMS
saUMSR$count <- normalizeByAbsolutePercentage(saUMSR$count)
saUFSR$count <- normalizeByAbsolutePercentage(saUFSR$count)

## test
stopifnot(length(saUFSR$count) == length(saUMSR$count))

correlateCategories(saMSR$count, saFSR$count, saMSR$subcategory, categories="Subcategories")
correlateCategories(saUMSR$count, saUFSR$count, saUMSR$subcategory, categories="Subcategories",
                    countMethod="uniqueUsers")
