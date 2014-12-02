library(sqldf)
library(Hmisc) # Ecdf
library(parallel)
library(data.table)

subcategoryPreferencesByGender <- function(checkIns) {
  joined <- checkIns

  #categories
  maleC <- categoriesByGender(joined, "male")
  femaleC <- categoriesByGender(joined, "female")
  maleUniqueC <- categoriesByGender(joined, "male", uniqueUsers=TRUE)
  femaleUniqueC <- categoriesByGender(joined, "female", uniqueUsers=TRUE)

  #subcategories
  maleSubC <- categoriesByGender(joined, "male", subcategory=TRUE)
  femaleSubC <- categoriesByGender(joined, "female", subcategory=TRUE)
  maleUniqueSubC <- categoriesByGender(joined, "male", subcategory=TRUE, uniqueUsers=TRUE)
  femaleUniqueSubC <- categoriesByGender(joined, "female", subcategory=TRUE, uniqueUsers=TRUE)

  femaleSubC <- completeSubcategories(maleSubC, femaleSubC)
  maleSubC <- completeSubcategories(femaleSubC, maleSubC)
  femaleUniqueSubC <- completeSubcategories(maleUniqueSubC, femaleUniqueSubC)
  maleUniqueSubC <- completeSubcategories(femaleUniqueSubC, maleUniqueSubC)

  # aggregate equivalent subcategories
  maleSubC <- aggregateEquivalentSubC(substitutionRules, maleSubC)
  femaleSubC <- aggregateEquivalentSubC(substitutionRules, femaleSubC)
  maleUniqueSubC <- aggregateEquivalentSubC(substitutionRules, maleUniqueSubC)
  femaleUniqueSubC <- aggregateEquivalentSubC(substitutionRules, femaleUniqueSubC)

  # normalization - counting check-ins
  maleC$count <- normalizeByAbsolutePercentage(maleC$count)
  femaleC$count <- normalizeByAbsolutePercentage(femaleC$count)
  maleSubC$count <- normalizeByAbsolutePercentage(maleSubC$count)
  femaleSubC$count <- normalizeByAbsolutePercentage(femaleSubC$count)

  # normalization – unique users
  maleUniqueC$count <- normalizeByAbsolutePercentage(maleUniqueC$count)
  femaleUniqueC$count <- normalizeByAbsolutePercentage(femaleUniqueC$count)
  maleUniqueSubC$count <- normalizeByAbsolutePercentage(maleUniqueSubC$count)
  femaleUniqueSubC$count <- normalizeByAbsolutePercentage(femaleUniqueSubC$count)

  return(list(maleCategories=maleC, femaleCategories=femaleC,
              maleUniqueCategories=maleUniqueC, femaleUniqueCategories=femaleUniqueC,
              maleSubcategories=maleSubC, femaleSubcategories=femaleSubC,
              maleUniqueSubcategories=maleUniqueSubC, femaleUniqueSubcategories=femaleUniqueSubC))
}

correlateTopCategories <- function(group1, group2, country, categories, countMethod, popularityMeasure, topN=10) {
  top <- getTopNCategories(group1, group2, topN, method=popularityMeasure)

  correlateCategories(top$group1$count,
                      top$group2$count,
                      labels=top$group1$subcategory,
                      country=country,
                      categories=categories,
                      countMethod=countMethod)
}

getTopNCategories <- function(group1, group2, N, method="most popular"){
  topN <- group1[1:nrow(group1),] # select all
  filterFun <- function(){}
  if(method=="most popular") {
    filterFun <- function(x, y) { x + y }
  } else if(method=="most different") {
    filterFun <- function(x , y){ abs(x - y) }
  } else {stop("Select a filter method = {'most popular'|'most different'}")}
  topN <- filterTopNSubcategories(group2, group1, N, filterFun)
  group2Top <- group2[topN,]
  group1Top <- group1[topN,]
  return(list(group1=group1Top, group2=group2Top))
}

getCheckInsInCountry <- function(countryCheckIns, countryUsers, userLocalFilter, substitutionRules) {
  ci <- readCheckIns(countryCheckIns)
  if(!missing(countryUsers)) {
    users <- readUsers(countryUsers)
    profiles <- discardNonResidents(users, filter=userLocalFilter)
    ci <- joinCheckInsWithProfiles(ci, profiles)
  }
  if(!missing(userLocalFilter)) {
    ci <- discardNonResidents(ci, filter=userLocalFilter)
  }
  if(!missing(substitutionRules)) {
    ci <- combineEquivalentSubCategories(ci, substitutionRules)
  }
  ## for second data collection
  if(grepl("paises", countryCheckIns)) {
    colnames(ci) <- c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                     "subcategory", "category", "country", "city", "district", "gender", "timeOffset")
  }
  ci <- as.data.table(ci)
  return(oneCheckInForUserAndLocation(ci))
}

oneCheckInForUserAndLocation <- function(checkIns) {
  setkey(checkIns, idLocal, idUserFoursquare)
  checkIns <- unique(checkIns)
  setkey(checkIns, NULL)
  return(checkIns)
}

getCheckInsInRegion <- function(regionFilters, countryCheckIns, countryUsers, userLocalFilter, substitutionRules, checkIns) {
  if(missing(checkIns)) {
    checkIns <- getCheckInsInCountry(countryCheckIns, countryUsers, userLocalFilter, substitutionRules)
  }

  checkInsInRegion <- c()
  for(filter in regionFilters) {
    checkInsInRegion <- rbind(checkInsInRegion, sqldf(sprintf("Select * from checkIns where city LIKE %s", shQuote(filter))))
  }
  #return(subset(checkInsInRegion, !duplicated(checkInsInRegion)))
  stopifnot(haskey(checkInsInRegion)==FALSE) # unique only works without keys!
  return(unique(checkInsInRegion))
}

combineEquivalentSubCategories <- function(checkIns, substitutionRules) {
  for(pair in substitutionRules) {
    for(equivalent in pair$equivalents) {
      if(equivalent %in% checkIns$subcategory) {
        checkIns[checkIns$subcategory==equivalent, ]$subcategory <- pair$original
      }
    }
  }
  return(checkIns)
}

segregation <- function(checkIns, location="<location>", sub=NULL, axeslim=SEGREGATION_AXES) {
  totalMaleSum <- checkIns[, list(maleCount=sum(gender=='male'))]$maleCount
  totalFemaleSum <- checkIns[, list(femaleCount=sum(gender=='female'))]$femaleCount

  checkIns[, maleCount:=sum(gender=='male')/totalMaleSum, by=idLocal]
  checkIns[, femaleCount:=sum(gender=='female')/totalFemaleSum, by=idLocal]

  print(cor.test(checkIns$maleCount, checkIns$femaleCount))

  print(chisq.test(checkIns$maleCount, checkIns$femaleCount))

  plot(checkIns$maleCount, checkIns$femaleCount,
      main=paste("Gender separation in", location), sub=sub, xlab="Male Popularity", ylab="Female Popularity",
      xlim=axeslim, ylim=axeslim)
  abline(0, 1, col="red")

  print("Top male")

  topMale <- sqldf("Select maleCount, subcategory, idLocal, latitude, longitude from checkIns
                   group by idLocal
                   order by maleCount desc limit 10")
  print(topMale)
  print("Top female")
  topFemale <- sqldf("Select femaleCount, subcategory, idLocal, latitude, longitude from checkIns
                     group by idLocal
                     order by femaleCount desc limit 10")
  print(topFemale)

  return(checkIns)
}

aggregateSegregationForRegion <- function(regionCheckIns, region) {
  seg <- segregation(regionCheckIns, region)
  distances <- euclideanDistance(seg$maleCIR$count, seg$femaleCIR$count)
  boxplot(distances, names=c(region))
  return(summary(distances))
}

completeCheckInsByGenderForRegion <- function(checkIns) {
  maleLocations <- countLocationsByGender(copy(checkIns), 'male')
  femaleLocations <- countLocationsByGender(copy(checkIns), 'female')

  notInF <- locationsNotCheckedInByGender(femaleLocations, maleLocations)
  notInM <- locationsNotCheckedInByGender(maleLocations, femaleLocations)
  completeFemale <- completeLocationsWithOtherGender(femaleLocations, notInF)
  completeMale <- completeLocationsWithOtherGender(maleLocations, notInM)

  stopifnot( length(completeMale$count) == length(completeFemale$count) )
  stopifnot( completeMale[order(completeMale$idLocal), ]$idLocal ==
             completeFemale[order(completeFemale$idLocal), ]$idLocal )
  return(list(female=completeFemale, male=completeMale))
}

completeLocationsWithOtherGender <- function(checkIns, checkInsNotMade){
    checkInsNotMade$count <- 0
    complete <- rbind(checkIns, checkInsNotMade)
    # sort by popularity
    return(complete[order(complete$count, decreasing=T), ])
}

locationsNotCheckedInByGender <- function(checkIns, checkInsOfOtherGender) {
    return(sqldf("Select * from checkInsOfOtherGender where idLocal not in (Select idLocal from checkIns)"))
}

countLocationsByGender <- function(checkIns, genderString) {
    checkIns[, list(count=length(unique(idUserFoursquare[gender==genderString]))), by=idLocal]
}

topLocations <- function(checkIns, n=7) {
    return(checkIns[order(checkIns$count, decreasing=T),][1:n,])
}

readUsers <- function(path) {
  fullPath <- paste("~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/datasets/", path, sep="")
  users <- read.csv(fullPath, header=F, sep="\t")
  colnames(users) <- c("idUserFoursquare", "user", "userLocal", "gender")
  return(users)
}

readCheckIns <- function(path) {
  fullPath <- paste("~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/datasets/", path, sep="")
  ci <- read.csv(fullPath, header=F, sep="\t")
  colnames(ci) <- c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                     "subcategory", "category", "city", "country")
  return(ci)
}

discardNonResidents <- function(users, filter) {
  return(users[grep(filter, ignore.case=T, users$userLocal), ])
}

joinCheckInsWithProfiles <- function(checkIns, profiles) {
  return(sqldf("Select * from checkIns JOIN profiles using(idUserFoursquare)"))
}

categoriesByGender <- function(joinedTable, genderString, uniqueUsers=FALSE, subcategory=FALSE) {
  categoryString <- "category"
  if(subcategory==TRUE) {
    categoryString <- "subcategory"
  }
  if(uniqueUsers==TRUE) {
    return(checkIns[, count:=length(unique(idUserFoursquare[gender==genderString]))), by=categoryString])
  } else {
    return(checkIns[, count:=length(idUserFoursquare[gender==genderString])), by=categoryString])
  }
}

normalizeByAbsolutePercentage <- function(attribute){
  return(attribute/sum(attribute))
}

normalizeByPercentageOfMax <- function(attribute) {
  return(attribute/max(attribute))
}

correlateCategories <- function(x, y, labels, country, file, countMethod="check-ins",
                                categories="Categories",
                                xlim=0.1, ylim=0.1) {
  if(!missing(file)) {
    png(file)
  }
  plot(x, y, main=sprintf("Correlation of %s counting %s in %s",categories, countMethod, country),
       xlab="Male Popularity", ylab="Female Popularity", xlim=c(0, xlim), ylim=c(0, ylim))
  abline(0, 1, col="red")
  text(x, y, labels=labels, pos=3)
  print(cor.test(x, y))
  print(chisq.test(x, y))
  if(!missing(file)) {
    dev.off()
  }
}

selectNotPresentFromOtherGender <- function(gender1, gender2) {
  return(sqldf(paste("Select * from gender1 where subcategory not in (Select subcategory from gender2)")))
}

completeSubcategories <- function(gender1, gender2) {
  temp <- selectNotPresentFromOtherGender(gender1, gender2)
  temp$count <- 0
  #temp$gender <- gender2$gender
  gender2<-rbind(gender2, temp)
  return( gender2[ order(gender2$subcategory), ] )
}

sumEquivalentSubC <- function(subC, equivalents, table) {
  for(equivalent in equivalents) {
    if(equivalent %in% table$subcategory & subC %in% table$subcategory) {
      equivalentIdx <- match(equivalent, table$subcategory)
      idx <- match(subC, table$subcategory)
      table[idx, ]$count <- table[idx, ]$count + table[equivalentIdx, ]$count
      table <- table[-equivalentIdx, ] # delete equivalent
    }
  }
  return(table)
}

aggregateEquivalentSubC <- function(substitutionRules, table) {
  for(pair in substitutionRules) {
    table <- sumEquivalentSubC(pair$original, pair$equivalents, table)
  }
  return(table)
}

filterTopNSubcategories <- function(x, y, n, fun=`+`) {
  uniqueSubC <- merge(x, y, by="subcategory")
  uniqueSubC$sumCount <- fun(uniqueSubC$count.x, uniqueSubC$count.y)
  return(order(uniqueSubC$sumCount, decreasing=TRUE)[1:n])
}

plot_pdf <- function(x, main="Probability density of …", xlab="% of unique users checked in", ylab="Frequency") {
  # probability density function
  h <- hist(x, breaks = 100, plot=FALSE)
  h$counts=h$counts/sum(h$counts)
  plot(h, xlab=xlab, ylab=ylab, main=main)
}

plotDensityOfDistanceInSubcategory <- function(distances, subcategory) {
  stopifnot(subcategory %in% distances$subcategory)
  plot(density(distances[distances$subcategory==subcategory][[1]]),
       main=sprintf("Density of gender distance in %s in %s", subcategory,city))
}

plotProbabilityDensityOfDistanceInSubcategory <- function(distances, subcategory) {
  stopifnot(subcategory %in% distances$subcategory)
  plot_pdf(distances[distances$subcategory==subcategory][[1]],
           main=sprintf("Probability density of %s in %s", subcategory, city), xlab="gender distance")
}

distances <- function(masculine, feminine) {
  stopifnot(nrow(masculine) == nrow(feminine))
  dists <- list()
  for(s in unique(masculine$subcategory)){
    m <- masculine[masculine$subcategory==s, ]$count
    f <- feminine[feminine$subcategory==s, ]$count
    dists <- c(dists, list(euclideanDistance(m, f)))
  }
  dists$subcategory <- unique(masculine$subcategory)
  return(dists)
}

euclideanDistance <- function(male, female) {
  dist <- sqrt((0.5*(male+female) - female)^2 + (0.5*(male + female)-male)^2)

  # determine side of the diagonale
  for(i in seq(length(male))) {
    if(female[i]>male[i]) {
      dist[i] <- - dist[i]
    }
  }
  return(dist)
}

compareDistanceSegregationsECDFin <- function(checkInsInCategory1, checkInsInCategory2, regionName1, regionName2, name) {
  g <- c( rep(regionName1, length(checkInsInCategory1)),
          rep(regionName2, length(checkInsInCategory2)))
  Ecdf(c(checkInsInCategory1, checkInsInCategory2), group=g,
       col=c('blue', 'orange'), xlab="Gender distance", main=sprintf("ECDF for %s", name))
  abline(v=0:1, untf=FALSE, col='red')
  ####
  ksTest <- ks.test(checkInsInCategory1, checkInsInCategory2) # ksTest.statistic holds difference
  message("Largest difference: ", signif(ksTest$statistic, 3), " with p-value: ", signif(ksTest$p.value, 3))
  return(ksTest)
}

compareSegregationBoxplot <- function(segregationInRegion1, segregationInRegion2, regionName1, regionName2, main="Distribution of Gender Difference") {
  seg1 <- segregationInRegion1$femaleCIR$count-segregationInRegion1$maleCIR$count
  seg2 <- segregationInRegion2$femaleCIR$count-segregationInRegion2$maleCIR$count
  boxplot(seg1, seg2, names=c(regionName1, regionName2), main=main)
  print(t.test(seg1, seg2))
  print(paste("SD region1: ", sd(seg1)))
  print(paste("SD region2: ", sd(seg2)))
  message("Summary region1")
  print(summary(seg1))
  message("Summary region2")
  print(summary(seg2))
}

genderDistanceForCountry <- function(countries, substitutionRules, main){
  distances <- list()
  for(country in countries) {
    message(country$name)
    ci <- getCheckInsInCountry(country$checkIns, country$users, country$filter,substitutionRules)
    seg <- segregation(ci, country$name)
    distances <- c(distances, list(euclideanDistance(seg$maleCIR$count, seg$femaleCIR$count)))
  }
  names <- list()
  for(c in countries) {
    names <- c(names, c$name)
  }

  do.call(boxplot, list(distances, names=names, main=main))
}

generateCheckIns <- function(checkIns, UNIFORM_LOCATION_PROBABILITY=FALSE, UNIFORM_GENDER_PROBABILITY=TRUE) {
  message("Generating…")
  locations <- sqldf("Select idLocal, latitude,longitude, subcategory, category, city, country, gender, count(gender) as genderCount
                      From checkIns
                      Group By city, idLocal, gender")
  userIds <- sqldf("Select distinct(idUserFoursquare) from checkIns")
  date <- Sys.time()

  generated <- data.frame(
    idUserFoursquare=NaN
    ,date=NaN
    ,latitude=NaN
    ,longitude=NaN
    ,idLocal=NaN
    ,subcategory=NaN
    ,category=NaN
    ,city=NaN
    ,country=NaN
    ,user=NaN
    ,userLocal=NaN
    ,gender=NaN
  )
  return(generateForCities(checkIns, generated, locations, userIds, date,
                            UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY))
}

generateForCities <- function(checkIns, generated, locations, userIds, date,
                              UNIFORM_LOCATION_PROBABILITY=TRUE,
                              UNIFORM_GENDER_PROBABILITY=TRUE) {
  country <- locations$country[1]
  for(c in unique(locations$city)) {
    message("City: ", c)
    locationsInCity <- locations[locations$city==c, ]

    nCheckIns <- countCheckInsWithSpecifiedGender(locationsInCity)
    message("#checkIns male/female: ", nCheckIns)

    randomIdLocal <- sampleLocationIds(locationsInCity, nCheckIns, UNIFORM_LOCATION_PROBABILITY)
    randomGender <- sampleGender(nCheckIns, countMaleCheckIns(locationsInCity), UNIFORM_GENDER_PROBABILITY)

    userIdVec <- buildUserIdColumn(checkIns, randomGender, nCheckIns)

    longitudeVec <- vector(mode="double", length=nCheckIns)
    latitudeVec <- vector(mode="double", length=nCheckIns)
    subcategoryVec <- vector(mode="character", length=nCheckIns)
    categoryVec <- vector(mode="character", length=nCheckIns)

    locationsInCity$subcategory <- as.matrix(locationsInCity$subcategory)
    locationsInCity$category <- as.matrix(locationsInCity$category)

    for(i in seq(nCheckIns)) {
      # performance bottleneck is this loop
      idLocal <- randomIdLocal[i]
      localAttrs <- locationsInCity[locationsInCity$idLocal==idLocal, ][1,]
      longitudeVec[i] <- localAttrs$longitude
      latitudeVec[i] <- localAttrs$latitude
      subcategoryVec[i] <- localAttrs$subcategory
      categoryVec[i] <- localAttrs$category
    }

    checkInsForCity <- as.matrix(data.frame(idUserFoursquare=userIdVec, date=date,
        latitude=latitudeVec, longitude=longitudeVec,
        idLocal=randomIdLocal,
        subcategory=subcategoryVec, category=categoryVec,
        city=c, country=country,
        user=userIdVec, userLocal=c,
        gender=randomGender
    ))
    generated <- rbind(generated, checkInsForCity)
    generated <- generated[2:nCheckIns,] # delete NaN row
  }
  return(generated)
}

buildUserIdColumn <- function(checkIns, randomGender, nCheckIns) {
  maleUserIds <- unique(checkIns[checkIns$gender=="male", ]$idUserFoursquare)
  femaleUserIds <- unique(checkIns[checkIns$gender=="female", ]$idUserFoursquare)
  randomMaleIds <- sample(maleUserIds, nCheckIns, replace=T)
  randomFemaleIds <- sample(femaleUserIds, nCheckIns, replace=T)

  n <- length(randomGender)
  userIdVec <- vector(mode="character", length=n)
  maleCount <- 1
  femaleCount <- 1
  id <- NULL
  for(i in seq(n)) {
    gender <- randomGender[i]
    if(gender=="male") {
      id <- randomMaleIds[maleCount]
      maleCount <- maleCount +1
    } else {
      id <- randomFemaleIds[femaleCount]
      femaleCount <- femaleCount +1
    }
    userIdVec[i] <- id
  }
  return(userIdVec)
}

sampleLocationIds <- function(locationsInCity, nCheckIns, UNIFORM_LOCATION_PROBABILITY) {
  locationIDs <- c()
  if(UNIFORM_LOCATION_PROBABILITY==TRUE) {
    locationIDs <- unique(locationsInCity$idLocal)
  } else {
    locationIDs <- locationsInCity$idLocal
  }
  return(sample(locationIDs, nCheckIns, replace=TRUE))
}

sampleGender <- function(nCheckIns, nMaleCheckIns, UNIFORM_GENDER_PROBABILITY) {
 if(UNIFORM_GENDER_PROBABILITY==TRUE) {
    return(sample(c("male", "female"), nCheckIns, replace=TRUE))
  } else {
    malePercentage <- nMaleCheckIns/nCheckIns
    genderDistribution <- c(malePercentage, 1 - malePercentage)
    return(sample(c("male", "female"), nCheckIns, prob=genderDistribution, replace=TRUE) )
  }
}

countCheckInsWithSpecifiedGender <- function(locationsInCity) {
    # discard 'other' gender
    nMale <- countMaleCheckIns(locationsInCity)
    nFemale <- countFemaleCheckIns(locationsInCity)
    return(nMale + nFemale)
}

countMaleCheckIns <- function(checkIns) {
  sum(checkIns[checkIns$gender=="male", ]$genderCount)
}

countFemaleCheckIns <- function(checkIns) {
  sum(checkIns[checkIns$gender=="female", ]$genderCount)
}

empiricalAttributeDistribution <- function(checkIns, attribute) {
  ## attribute = {"category", "subcategory", "idLocal", "latitude", "gender", …}
  distribution <- sqldf(sprintf("select %s as attribute, count(*) as percentage from checkIns group by %s", attribute, attribute))
  distribution$percentage <- distribution$percentage / sum(distribution$percentage)
  return(distribution)
}

####################
# Null Model generation
####################

######## Permutation #############
runPermutate <- function(checkIns, segregation, folderName, plotName, regionName, k=100) {
  gen.segregation <- c()
  checkIns <- rbind(checkIns[checkIns$gender=="male", ], checkIns[checkIns$gender=="female", ])
  nCheckIns <- nrow(checkIns)
  for(i in seq(k)) {
      gen.checkIns <- permutateGender(checkIns)
      s <- segregation(gen.checkIns, regionName,
                          sub="permutating gender")

      gen.segregation$maleCIR <- rbind(gen.segregation$maleCIR, s$maleCIR)
      gen.segregation$femaleCIR <- rbind(gen.segregation$femaleCIR, s$femaleCIR)
  }

  femaleSegregationFile <- sprintf("%s/generated-%s-%s-pop-female.csv", folderName, regionName, plotName)
  write.csv(gen.segregation$femaleCIR, femaleSegregationFile)
  message("Wrote generated segregation to %s", femaleSegregationFile)
  maleSegregationFile <- sprintf("%s/generated-%s-%s-pop-male.csv", folderName, regionName, plotName)
  write.csv(gen.segregation$maleCIR, maleSegregationFile)
  message("Wrote generated segregation to %s", maleSegregationFile)
  return(c(maleSegregationFile, femaleSegregationFile))
}

permutateGender <- function(checkIns) {
  # TODO use built-in grouping to parallelize
  uniqueUserCI <- sqldf("Select * from checkIns Group By idUserFoursquare, idLocal")
  uniqueUserCI$gender <- sample(uniqueUserCI$gender)
  return(uniqueUserCI)
}

######## Check-in generation #############

runGenerate <- function(checkIns, segregation, UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY,
                        folderName, plotName, k=100) {
  checkIns <- rbind(checkIns[checkIns$gender=="male", ], checkIns[checkIns$gender=="female", ])
  nCheckIns <- nrow(checkIns)

  gen.segregation <- c()
  x <- data.frame(idUserFoursquare=NaN ,date=NaN ,latitude=NaN ,longitude=NaN ,idLocal=NaN
      ,subcategory=NaN ,category=NaN ,city=NaN ,country=NaN ,user=NaN ,userLocal=NaN ,gender=NaN)
  for(i in seq(k)) {
      gen.checkIns <- generateCheckIns(checkIns, UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY)
      stopifnot(!any(is.na(gen.checkIns)))
      # don't know where the <NA> come from
      #gen.checkIns <- gen.checkIns[!is.na(gen.checkIns$idUserFoursquare), ]
      x <- rbind(gen.checkIns, x)

      # segregation with all values crashes :(
      s <- segregation(gen.checkIns, "Riyadh generated",
                          sub=sprintf("uniform location: %s, uniform gender: %s",
                              UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY))
      gen.segregation$maleCIR <- rbind(gen.segregation$maleCIR, s$maleCIR)
      gen.segregation$femaleCIR <- rbind(gen.segregation$femaleCIR, s$femaleCIR)
  }

  x <- x[2:(k*nCheckIns),] # discard first NaN row
  checkInFile <- sprintf("%s/generated-riyadh-%s.csv", folderName, plotName)
  write.csv(x, checkInFile)
  message("Wrote generated check-ins to %s", checkInFile)

  femaleSegregationFile <- sprintf("%s/generated-riyadh-%s-pop-female.csv", folderName, plotName)
  write.csv(gen.segregation$femaleCIR, femaleSegregationFile)
  message("Wrote generated segregation to %s", femaleSegregationFile)
  maleSegregationFile <- sprintf("%s/generated-riyadh-%s-pop-male.csv", folderName, plotName)
  write.csv(gen.segregation$maleCIR, maleSegregationFile)
  message("Wrote generated segregation to %s", maleSegregationFile)
  return(c(checkInFile, maleSegregationFile, femaleSegregationFile))
}

testObservationWithNullModel <- function(gen.segregation, folderName, regionName,
                                         UNIFORM_LOCATION_PROBABILITY,
                                         UNIFORM_GENDER_PROBABILITY,
                                         SEARCH_ANOMALOUS_LOCATIONS=FALSE, alpha=0.05, PLOT_ALL_DISTS=F) {
  meanMalePopularities <- c()
  meanFemalePopularities <- c()
  uniqueLocations <- unique(gen.segregation$maleCIR$idLocal)
  anomalyCount <- 0
  for(location in uniqueLocations) {
    malePopularity <- gen.segregation$maleCIR[gen.segregation$maleCIR$idLocal==location, ]$count
    femalePopularity <- gen.segregation$femaleCIR[gen.segregation$femaleCIR$idLocal==location, ]$count
    meanMalePopularities <- c(meanMalePopularities, mean(malePopularity))
    meanFemalePopularities <- c(meanFemalePopularities, mean(femalePopularity))

    if (SEARCH_ANOMALOUS_LOCATIONS) {
      empiricalDist <- euclideanDistance(malePopularity, femalePopularity)
      percentile <- quantile(empiricalDist, c(alpha/2, 1-alpha/2))

      # unnecessary because ids are ordered and r.segregation$maleCIR$idLocal == r.segregation$femaleCIR$idLocal
      observedMale <- r.segregation$maleCIR[r.segregation$maleCIR$idLocal==location, ]$count
      observedFemale <- r.segregation$femaleCIR[r.segregation$femaleCIR$idLocal==location, ]$count
      observedDist <- observedFemale-observedMale
      if((observedDist & percentile[1] & percentile[2]) & observedDist < percentile[1] | observedDist > percentile[2]) {
        anomalyCount <- anomalyCount +1
        # message(sprintf("Anomalous location: %s, subcategory: %s, distance: %s\n",
        #               location, gen.segregation$maleCIR[gen.segregation$maleCIR$idLocal==location, ]$subcategory, signif(observedDist)))
        if(PLOT_ALL_DISTS | sample(c(T, F), 1, prob=c(0.05, 0.95))) {
          filename <- sprintf("%s/anomalous-%s.pdf", folderName, location)

          pdf(filename)
          hist(c(empiricalDist, observedDist), main="Histogram of gender distance", xlab="gender distance",
               sub=sprintf("Location: %s, subcategory: %s, distance: %s, anomalous with alpha=%s, k=%s",
                        location, gen.segregation$maleCIR[gen.segregation$maleCIR$idLocal==location, ]$subcategory, signif(observedDist), alpha, k))
          abline(v=percentile[1], col="green")
          abline(v=percentile[2], col="green")
          abline(v=observedDist, col="blue")
          dev.off()
        }
      }
    message(sprintf("%s of %s (%s%%) locations with observed anomalous segregation",
              anomalyCount, length(uniqueLocations),
              100*round(anomalyCount/length(uniqueLocations),3)))
    }
  }

  pdf(sprintf("%s/avg-segregation-generated-%s.pdf", folderName, regionName))
  plot(meanMalePopularities, meanFemalePopularities,
        main=sprintf("Gender separation in Generated %s", regionName),
        sub=sprintf("uniform location: %s, uniform gender: %s, k=%s",
            UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY, k),
        xlim=SEGREGATION_AXES, ylim=SEGREGATION_AXES,
        xlab="Male Popularity", ylab="Female Popularity")
  abline(0, 1, col="red")
  dev.off()
}

readGeneratedDataAndPlot <- function(malePopularityFile, femalePopularityFile, folderName, regionName,
                                     UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY) {
  gen.segregation <-c()
  gen.segregation$maleCIR <- read.csv(malePopularityFile)
  gen.segregation$femaleCIR <- read.csv(femalePopularityFile)
  testObservationWithNullModel(gen.segregation, folderName, regionName,
                               UNIFORM_GENDER_PROBABILITY, UNIFORM_LOCATION_PROBABILITY,
                               SEARCH_ANOMALOUS_LOCATIONS=F, PLOT_ALL_DISTS=F)
}

checkInsInlocationsWithMinimumCheckIns <- function(checkIns, n=7) {
  x <- checkIns[, list(hasMore=length(unique(idUserFoursquare))>=7), by=idLocal]
  locations <- x[hasMore==TRUE]$idLocal
  return(checkIns[idLocal %in% locations, ])
}


##########
# Constants
##########
SEGREGATION_AXES = c(0, 0.05)

saudiCheckIns <- "base2/arabiaSaudita/Saudi-Arabia.txt"
franceCheckIns <- "base2/France.txt"
swedenCheckIns <- "base2/Sweden.txt"
uaeCheckIns <- "base2/United-Arab-Emirates.txt"
germanyCheckIns <- "base2/Germany.txt"

franceFilter <- "Paris|France|Metz|Bordeaux|Marseille|Midi-Py|Strasbourg|Lyon"
swedenFilter <- "Sverige|Sweden|Stockholm|Malmö"
saudiFilter <- "Saudi|Mecca|Medina|Riyadh|Ar Riyad|الرياض|Jedda"  # الرياض = Riyadh
uaeFilter <- "Dubai|United Arab Emirates|Abu Dhabi|Sharjah|Al Ain|Ras Al-Khaimah"
germanyFilter <- "Deutschland|Berlin|Germany|München|Munich|Frankfurt|Hamburg|Stuttgart|Mainz|Düsseldorf|Köln|Cologne|Thüringen|Hessen|Sachsen|Bremen|Schleswig|Mecklenburg|Saarbrücken|Saarland|Bayern|Bavaria|Nordrhein-Westfalen"
franceUsers=germanyUsers=swedenUsers=uaeUsers <- "base2/profileFiltredGermanyFranceEmiratesSweden.dat"
saudiUsers <- "base2/arabiaSaudita/profilesArabia.dat"

substitutionRules <- list(
    list(original="Café", equivalents=c("Coffee Shop", "College Cafeteria")),
    list(original="Airport", equivalents=c("Airport Lounge", "Airport Gate", "Airport Terminal")),
    list(original="Train Station", equivalents=c("Train", "Platform", "Light Rail")),
    list(original="Bus Station", equivalents=c("Bus Line", "Bus Terminal")),
    list(original="Library", equivalents=c("College Library")),
    list(original="Movie Theater", equivalents=c("Indie Movie Theater", "Multiplex")),
    list(original="University", equivalents=c("General College & University", "College & University")),
    list(original="Gym", equivalents=c("Gym / Fitness Center", "College Gym")))



##################
# Run
##################
country <- "UAE"
checkInsInCity <- getCheckInsInRegion("Abu Dhabi", uaeCheckIns, uaeUsers, uaeFilter)
segregation <- segregation(checkInsInCity, "Abu Dhabi")


# correlation categories
uae.ci <- getCheckInsInCountry(uaeCheckIns, uaeUsers, uaeFilter)
data <- subcategoryPreferencesByGender(uae.ci)
country <- "Saudi Arabia"
saudi.ci <- getCheckInsInCountry(saudiCheckIns, saudiUsers, saudiFilter)
data <- subcategoryPreferencesByGender(saudi.ci)

correlateCategories(data$maleCategories$count, data$femaleCategories$count,
                    data$maleCategories$category,
                    countMethod="check-ins",
                    country=country,
                    xlim=0.3, ylim=0.3)

correlateCategories(data$maleUniqueCategories$count, data$femaleUniqueCategories$count,
                    data$maleUniqueCategories$category,
                    country=country,
                    countMethod="unique users",
                    xlim=0.3, ylim=0.3)

correlateCategories(data$maleSubcategories$count, data$femaleSubcategories$count,
                    data$maleSubcategories$subcategory,
                    country=country,
                    countMethod="check-ins",
                    categories="Subcategories",
                    xlim=0.1, ylim=0.1)

correlateCategories(data$maleSubcategories$count, data$femaleSubcategories$count,
                    data$maleSubcategories$subcategory,
                    country=country,
                    countMethod="unique users",
                    categories="Subcategories",
                    xlim=0.1, ylim=0.1)
topN <- 10
correlateTopCategories(data$maleUniqueSubcategories, data$femaleUniqueSubcategories,
                       country,
                       countMethod="unique users",
                       sprintf("%s most popular subcategories", topN),
                       "most popular",
                       topN)

correlateTopCategories(data$maleUniqueSubcategories, data$femaleUniqueSubcategories,
                       country,
                       "unique users",
                       sprintf("%s most different subcategories", topN),
                       "most different",
                       topN)

############
# Correlation Locations & Segregation
############

ad.checkIns <- getCheckInsInRegion(c("Abu Dhabi"), uaeCheckIns, uaeUsers, uaeFilter, substitutionRules)
ad.segregation <- segregation(ad.checkIns, "Abu Dhabi")
ad.dists <- distances(ad.segregation$maleCIR, ad.segregation$femaleCIR)

r.checkIns <- getCheckInsInRegion(c("Riyadh"), saudiCheckIns, saudiUsers, saudiFilter, substitutionRules)
r.segregation <- segregation(r.checkIns, "Riyadh")
r.dists <- distances(r.segregation$maleCIR, r.segregation$femaleCIR)

seg <- segregation(getCheckInsInRegion(c("Paris"), franceCheckIns, franceUsers, franceFilter, substitutionRules), "Paris")
dists <- distances(seg$maleCIR, seg$femaleCIR)
####
# for copy'n paste into console
####
dists <- ad.dists
city <- "Abu Dhabi"

dists <- r.dists
city <- "Riyadh"

plotDensityOfDistanceInSubcategory(dists, "University")
plotDensityOfDistanceInSubcategory(dists, "Mall")
plotDensityOfDistanceInSubcategory(dists, "Café")

plotProbabilityDensityOfDistanceInSubcategory(dists, "Café")

r.cafe <- r.dists[r.dists$subcategory=="Café"][[1]]
ad.cafe <- ad.dists[ad.dists$subcategory=="Café"][[1]]

compareDistanceSegregationsECDFin(r.cafe, ad.cafe, "Riyadh", "Abu Dhabi", "Café")

############

countries <- list(
  list(checkIns=franceCheckIns, users=franceUsers, filter=franceFilter, name="France")
  ,list(checkIns=germanyCheckIns, users=germanyUsers, filter=germanyFilter, name="Germany")
  ,list(checkIns=swedenCheckIns, users=swedenUsers, filter=swedenFilter, name="Sweden")
  ,list(checkIns=saudiCheckIns, users=saudiUsers, filter=saudiFilter, name="Saudi Arabia")
  ,list(checkIns=uaeCheckIns, users=uaeUsers, filter=uaeFilter, name="UAE")
  )

genderDistanceForCountry(countries, substitutionRules, "Distribution of gender distance")

#########################################
# Null Model Generation
#########################################

r.checkIns <- getCheckInsInRegion(c("Riyadh"), saudiCheckIns, saudiUsers, saudiFilter, substitutionRules)
r.checkIns <- rbind(r.checkIns[r.checkIns$gender=="male", ], r.checkIns[r.checkIns$gender=="female", ])
r.segregation <- segregation(r.checkIns, "Riyadh")

k=10
fileNames <- c()
folderPrefix <- "results/null-model"
folderNames <- c(sprintf("%s/uniform-location-uniform-gender", folderPrefix),
                 sprintf("%s/observed-location-uniform-gender", folderPrefix),
                 sprintf("%s/uniform-location-observed-gender", folderPrefix),
                 sprintf("%s/observed-location-observed-gender", folderPrefix))
plotNames <- c("uniform-location-uniform-gender", "observed-location-uniform-gender",
               "uniform-location-observed-gender", "observed-location-observed-gender")

uniformLocationProbability <- c(TRUE, FALSE, TRUE, FALSE)
uniformGenderProbability <- c(TRUE, TRUE, FALSE, FALSE)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[1],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[1],
            folderName=folderNames[1],
            plotName=plotNames[1], k=k)
fileNames <- rbind(fileNames, f)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[2],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[2],
            folderName=folderNames[2],
            plotName=plotNames[2], k=k)
fileNames <- rbind(fileNames, f)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[3],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[3],
            folderName=folderNames[3],
            plotName=plotNames[3], k=k)
fileNames <- rbind(fileNames, f)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[4],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[4],
            folderName=folderNames[4],
            plotName=plotNames[4], k=k)
fileNames <- rbind(fileNames, f)

print(fileNames)

mapply(readGeneratedDataAndPlot, fileNames[,2 ], fileNames[,3 ],
         folderNames, uniformLocationProbability, uniformGenderProbability)


########################
# Run Permutation Test
########################
f <- runPermutate(r.checkIns, r.segregation, "results/null-model/gender-permutation", "permutate-gender", k=100)
gen.segregation <-c()
gen.segregation$maleCIR <- read.csv(f[1])
gen.segregation$femaleCIR <- read.csv(f[2])
testObservationWithNullModel(gen.segregation, "results/null-model/gender-permutation", F, F, T)
