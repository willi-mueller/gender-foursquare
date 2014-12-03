library(sqldf)
library(Hmisc) # Ecdf
library(parallel)
library(data.table)

subcategoryPreferencesByGender <- function(checkIns) {
  joined <- checkIns
  nOfUsersByGenderAndCategory <- checkIns[, list(count=length(unique(idUserFoursquare))),
                                             by=list(category, gender)]
  nOfUsersByGenderAndSubcategory <- checkIns[, list(count=length(unique(idUserFoursquare))),
                                               by=list(subcategory, gender)]

  femaleUniqueC <- nOfUsersByGenderAndCategory[gender=='female']
  maleUniqueC <- nOfUsersByGenderAndCategory[gender=='male']

  femaleUniqueC$count <- femaleUniqueC$count/sum(femaleUniqueC$count)
  maleUniqueC$count <- maleUniqueC$count/sum(maleUniqueC$count)

  femaleUniqueSubC <- nOfUsersByGenderAndSubcategory[gender=='female']
  maleUniqueSubC <- nOfUsersByGenderAndSubcategory[gender=='male']

  femaleUniqueSubC <- completeSubcategories(maleUniqueSubC, femaleUniqueSubC)
  maleUniqueSubC <- completeSubcategories(femaleUniqueSubC, maleUniqueSubC)

  femaleUniqueSubC$count <- femaleUniqueSubC$count/sum(femaleUniqueSubC$count)
  maleUniqueSubC$count <- maleUniqueSubC$count/sum(maleUniqueSubC$count)
  # # aggregate equivalent subcategories
  maleUniqueSubC <- aggregateEquivalentSubC(substitutionRules, maleUniqueSubC)
  femaleUniqueSubC <- aggregateEquivalentSubC(substitutionRules, femaleUniqueSubC)


  return(list(maleUniqueCategories=maleUniqueC, femaleUniqueCategories=femaleUniqueC,
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
  # given that we have 1 checkin for user and location

  nMaleUsers <- length(unique(checkIns[gender=='male', ]$idUserFoursquare))
  nFemaleUsers <- length(unique(checkIns[gender=='female', ]$idUserFoursquare))

  checkIns[, maleCount:=sum(gender=='male')/nMaleUsers, by=idLocal]
  checkIns[, femaleCount:=sum(gender=='female')/nFemaleUsers, by=idLocal]

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
    return(checkIns[, count:=length(unique(idUserFoursquare[gender==genderString])), by=categoryString])
  } else {
    return(checkIns[, count:=length(idUserFoursquare[gender==genderString]), by=categoryString])
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
                                axeslim=SEGREGATION_AXES) {
  if(!missing(file)) {
    pdf(file)
  }
  plot(x, y, main=sprintf("Correlation of %s counting %s in %s",categories, countMethod, country),
       xlab="Male Popularity", ylab="Female Popularity", xlim=SEGREGATION_AXES, ylim=SEGREGATION_AXES)
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
  #TODO check count(gender) group by city, idLocal, gender
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
  checkIns <- checkIns[gender=="male" || gender=="female", ]
  nCheckIns <- nrow(checkIns)
  for(i in seq(k)) {
      gen.checkIns <- permutateGender(checkIns)
      s <- segregation(gen.checkIns, regionName,
                          sub="permutating gender")

      gen.segregation <- rbind(gen.segregation, s)
  }

  segregationFile <- sprintf("%s/generated-%s-%s-pop.csv", folderName, regionName, plotName)
  message("Wrote generated segregation to %s", segregationFile)

  return(gen.segregation)
}

permutateGender <- function(checkIns) {
  # Expects only a single check-in per user per local
  checkIns <- copy(checkIns)
  # TODO: shuffle gender of USERS not check-ins
  checkIns$gender <- sample(checkIns$gender)
  return(checkIns)
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

testObservationWithNullModel <- function(observedSegregation, gen.segregation, folderName, regionName,
                                         UNIFORM_LOCATION_PROBABILITY=FALSE,
                                         UNIFORM_GENDER_PROBABILITY=FALSE,
                                         SEARCH_ANOMALOUS_LOCATIONS=TRUE,
                                         k, alpha=0.01, PLOT_ALL_DISTS=F, SEGREGATION_AXES=SEGREGATION_AXES) {
  meanMalePopularities <- c()
  meanFemalePopularities <- c()
  uniqueLocations <- unique(gen.segregation$idLocal)
  anomalyCount <- 0
  for(location in uniqueLocations) {
    iterLocation <- gen.segregation[idLocal==location, ]
    malePopularity <- iterLocation$maleCount
    femalePopularity <- iterLocation$femaleCount
    meanMalePopularities <- c(meanMalePopularities, mean(malePopularity))
    meanFemalePopularities <- c(meanFemalePopularities, mean(femalePopularity))

    if (SEARCH_ANOMALOUS_LOCATIONS) {
      empiricalDist <- euclideanDistance(malePopularity, femalePopularity)
      percentile <- quantile(empiricalDist, c(alpha/2, 1-alpha/2))
      observedMale <- observedSegregation[idLocal==location, ]$maleCount[[1]] # same value for each check-in
      observedFemale <- observedSegregation[idLocal==location, ]$femaleCount[[1]]
      observedDist <- euclideanDistance(observedMale, observedFemale)
      if((observedDist & percentile[1] & percentile[2]) & observedDist < percentile[1] | observedDist > percentile[2]) {
        anomalyCount <- anomalyCount +1
        # message(sprintf("Anomalous location: %s, subcategory: %s, distance: %s\n",
        #               location, iterLocation$subcategory, signif(observedDist)))
        if(PLOT_ALL_DISTS | sample(c(T, F), 1, prob=c(0.05, 0.95))) {
          filename <- sprintf("%s/anomalous-%s.pdf", folderName, location)

          pdf(filename)
          hist(c(empiricalDist, observedDist), main="Histogram of gender distance", xlab="gender distance",
               sub=sprintf("Location: %s, subcategory: %s, distance: %s, anomalous with alpha=%s, k=%s",
                        location, iterLocation$subcategory, signif(observedDist), alpha, k))
          abline(v=percentile[1], col="green")
          abline(v=percentile[2], col="green")
          abline(v=observedDist, col="blue")
          dev.off()
        }
        message(sprintf("%s of %s (%s%%) locations with observed anomalous segregation",
              anomalyCount, length(uniqueLocations),
              100*round(anomalyCount/length(uniqueLocations),3)))
      }
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

testObservationWithNullModelForCategories<-function(observedSegregation, gen.segregation,
                                                    folderName, regionName,
                                                    UNIFORM_LOCATION_PROBABILITY=FALSE,
                                                    UNIFORM_GENDER_PROBABILITY=FALSE,
                                                    SEARCH_ANOMALOUS_CATEGORIES=TRUE,
                                                    k, alpha=0.01, PLOT_ALL_DISTS=TRUE){
  maleCategoryPopularities <- c()
  femaleCategoryPopularities <- c()
  for (i in seq(k)) {
    generationRange <- seq((i-1)*nrow(gen.segregation)/k+1, (i * nrow(gen.segregation)/k))
    iter <- gen.segregation[generationRange]
    gen.male <- iter[,list(pop=mean(maleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
    gen.male <- gen.male[order(rank(category))]$pop
    gen.female <- iter[,list(pop=mean(femaleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
    gen.female <- gen.female[order(rank(category))]$pop

    maleCategoryPopularities <- c(maleCategoryPopularities, gen.male)
    femaleCategoryPopularities <- c(femaleCategoryPopularities, gen.female)
  }

  # mean category popularity over all generations for plotting
  gen.male.mean <- gen.segregation[,list(pop=mean(maleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
  gen.male.mean <- gen.male.mean[order(rank(category))]
  gen.female.mean <- gen.segregation[,list(pop=mean(femaleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
  gen.female.mean <- gen.female.mean[order(rank(category))]

  if (SEARCH_ANOMALOUS_CATEGORIES) {
    empiricalDist <- euclideanDistance(maleCategoryPopularities, femaleCategoryPopularities)
    nCategories <- length(unique(observedSegregation$category))

    stopifnot(nCategories == length(empiricalDist)/k)

    categoryDistDistribution <- list()
    for (i in seq(nCategories)) {
      allOfACategory <- empiricalDist[seq(i, length(empiricalDist), nCategories)]
      categoryDistDistribution[i] <- list(allOfACategory)
    }
    percentiles <- lapply(categoryDistDistribution,quantile, c(alpha/2, 1-alpha/2))

    observedMale <- observedSegregation[,list(pop=mean(maleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
    observedMale <- observedMale[order(rank(category))]$pop
    observedFemale <- observedSegregation[,list(pop=mean(femaleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
    observedFemale <- observedFemale[order(rank(category))]$pop
    observedDist <- euclideanDistance(observedMale, observedFemale)

    sortedCategories <- gen.male.mean$category

    write.csv(percentiles, sprintf("%s-category-percentiles-%s.csv", folderName, regionName))
    write.csv(observedDist, sprintf("%s-category-observed-dist-%s.csv", folderName, regionName))
    write.csv(sortedCategories, sprintf("%s-category-sorted-categories-%s.csv", folderName, regionName))

    anomalyCount <- 0
    for(i in seq(nCategories)) {
      if(observedDist[i] < percentiles[[i]][1] | observedDist[i] > percentiles[[i]][2]) {
          anomalyCount <- anomalyCount + 1
          message(sprintf("Anomalous category: %s, distance: %s\n",
                        sortedCategories[i], signif(observedDist[i])))
          if(PLOT_ALL_DISTS | sample(c(T, F), 1, prob=c(0.05, 0.95))) {
            filename <- sprintf("%s/anomalous-category-%s.pdf", folderName, sortedCategories[i])
            pdf(filename)
            hist(c(categoryDistDistribution[[i]], observedDist[i]), main="Histogram of gender distance", xlab="gender distance",
                 sub=sprintf("Category: %s, distance: %s, anomalous with alpha=%s, k=%s",
                          sortedCategories[i], signif(observedDist[i]), alpha, k))
            abline(v=percentiles[[i]][1], col="green")
            abline(v=percentiles[[i]][2], col="green")
            abline(v=observedDist[i], col="blue")
            dev.off()
          }
          message(sprintf("%s of %s (%s%%) categories with observed anomalous segregation",
                anomalyCount, nCategories,
                100*round(anomalyCount/nCategories, 3)))
        }
      }
  }
  pdf(sprintf("%s/avg-segregation-generated-category-%s.pdf", folderName, regionName))
  plot(gen.male.mean$pop, gen.female.mean$pop,
        main=sprintf("Gender separation in Generated %s", regionName),
        sub=sprintf("uniform location: %s, uniform gender: %s, k=%s",
            UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY, k),
        xlim=c(0,0.4), ylim=c(0,0.4),
        xlab="Male Popularity", ylab="Female Popularity")
  abline(0, 1, col="red")
  text(gen.male.mean$pop, gen.female.mean$pop, labels=sortedCategories, pos=3)
  dev.off()
}

readGeneratedDataAndPlot <- function(segregationFile, folderName, regionName,
                                     UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY) {
  gen.segregation <-c()
  gen.segregation$maleCIR <- read.csv(malePopularityFile)
  gen.segregation$femaleCIR <- read.csv(femalePopularityFile)
  testObservationWithNullModel(gen.segregation, folderName, regionName,
                               UNIFORM_GENDER_PROBABILITY, UNIFORM_LOCATION_PROBABILITY,
                               SEARCH_ANOMALOUS_LOCATIONS=F, PLOT_ALL_DISTS=F)
}

checkInsInlocationsWithMinimumCheckIns <- function(checkIns, n=5) {
  locations <- checkIns[, list(hasMore=length(unique(idUserFoursquare))>=n), by=idLocal][hasMore==TRUE]$idLocal
  return(checkIns[idLocal %in% locations, ])
}


##########
# Constants
##########
SEGREGATION_AXES = c(0, 0.20)

# FOR DATA BASE 2
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
checkInsInCity <- getCheckInsInRegion(c("Abu Dhabi"), uaeCheckIns, uaeUsers, uaeFilter)
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
         folderNames, rep("NY", nrow(fileNames), uniformLocationProbability, uniformGenderProbability)


########################
# Run Permutation Test
########################
k=100
ny <- usa[usa$city=="New York City",]
ny <- checkInsInlocationsWithMinimumCheckIns(ny, n=5)
ny.segregation <- segregation(ny, "New York City")
gen.segregation <- runPermutate(ny, ny.segregation, "results/null-model/ny/gender-permutation", "permutate-gender", "New York City", k=k)
testObservationWithNullModel(ny.segregation, gen.segregation,
                             "results/null-model/ny/gender-permutation", "New York City",
                             F, F, SEARCH_ANOMALOUS_LOCATIONS=T, k=k)

########################
# Plots correlation
########################

usa <- getCheckInsInCountry("paises/United-States.dat", substitutionRules=substitutionRules)
sa <- getCheckInsInCountry("paises/Saudi-Arabia.dat", substitutionRules=substitutionRules)
uae <- getCheckInsInCountry("paises/United-Arab-Emirates.dat", substitutionRules=substitutionRules)
japan <- getCheckInsInCountry("paises/Japan.dat", substitutionRules=substitutionRules)
brazil <- getCheckInsInCountry("paises/Brazil.dat", substitutionRules=substitutionRules)
france <- getCheckInsInCountry("paises/France.dat", substitutionRules=substitutionRules)
# china <- getCheckInsInCountry("paises/China.dat", substitutionRules=substitutionRules)
# saf <- getCheckInsInCountry("paises/South-Africa.dat", substitutionRules=substitutionRules)
# ger <- getCheckInsInCountry("paises/Germany.dat", substitutionRules=substitutionRules)
indonesia <- getCheckInsInCountry("paises/Indonesia.dat", substitutionRules=substitutionRules)

usa <- checkInsInlocationsWithMinimumCheckIns(usa, 5)
sa <- checkInsInlocationsWithMinimumCheckIns(sa, 5)
uae <- checkInsInlocationsWithMinimumCheckIns(uae, 5)
japan <- checkInsInlocationsWithMinimumCheckIns(japan, 5)
brazil <- checkInsInlocationsWithMinimumCheckIns(brazil, 5)
france <- checkInsInlocationsWithMinimumCheckIns(france, 5)
indonesia <- checkInsInlocationsWithMinimumCheckIns(indonesia, 5)

ny <- usa[city=="New York City",] # 6647
riyadh <- sa[city=="Riyadh",] # 6278
ad <- uae[city=="Abu Dhabi",] # 667
tokyo <- japan[city=="Tokyo",] # 32834
sp <- brazil[city=="Sao Paulo", ] # 12569
paris <- france[city=="Paris",] # 1767
jakarta <- indonesia[city=="Jakarta"] # 6245

beijing <- china[city=="Beijing",] # 474 checkins
jb <- saf[city=="Johannesburg",] #294 checkins
berlin <- ger[city=="Berlin",] # 183 checkins

usa.segregation <- segregation(usa, "USA")
sa.segregation <- segregation(sa, "Saudi Arabia")
uae.segregation <- segregation(uae, "United Arab Emirates")
japan.segregation <- segregation(japan, "Japan")
brazil.segregation <- segregation(brazil, "Brazil")
france.segregation <- segregation(france, "France")
indonesia.segregation <- segregation(indonesia, "Indonesia")

ny.segregation <- segregation(ny, "New York City")
sp.segregation <- segregation(sp, "São Paulo")
riyadh.segregation <- segregation(riyadh, "Riyadh")
ad.segregation <- segregation(ad, "Abu Dhabi")
tokyo.segregation <- segregation(tokyo, "Tokyo")
paris.segregation <- segregation(paris, "Paris")
jakarta.segregation <- segregation(jakarta, "Jakarta")

# china.segregation <- segregation(china, "China")
# saf.segregation <- segregation(china, "South Africa")

countries <- ("USA", "UAE", "Saudi Arabia", "Japan", "Brazil", "France", "Indonesia")
countryVars <- list(usa, uae, sa, japan, brazil, france, indonesia)
for(i in 1:length(countries)) {
  axesOld <- SEGREGATION_AXES
  data <- subcategoryPreferencesByGender(countryVars[[i]])
  pdf(sprintf("results/segregation-subcategory-country/%s.pdf", countries[i]))
  SEGREGATION_AXES <- c(0,0.15)
  correlateCategories(data$maleUniqueSubcategories$count, data$femaleUniqueSubcategories$count,
                    data$maleUniqueSubcategories$subcategory,
                    country=countries[i],
                    countMethod="unique users",
                    categories="Subcategories",
                    axeslim=c(0, 0.15))
  dev.off()
  SEGREGATION_AXES <- c(0, 0.3)
  pdf(sprintf("results/segregation-category-country/%s.pdf", countries[i]))
  correlateCategories(data$maleUniqueCategories$count, data$femaleUniqueCategories$count,
                    data$maleUniqueCategories$category,
                    country=countries[i],
                    countMethod="unique users",
                    categories="Categories",
                    axeslim=c(0, 0.3))
  dev.off()
  SEGREGATION_AXES <- axesOld
}


###################
# Permutation Test
###################

ny.gen.segregation <- runPermutate(ny, ny.segregation,
                                   "results/null-model/ny/gender-permutation",
                                   "permutate-gender", "New York City", k=k)
testObservationWithNullModel(ny.segregation, ny.gen.segregation,
                             "results/null-model/ny/gender-permutation",
                              "New York City", F, F, T, k=k)

sp.gen.segregation <- runPermutate(sp, sp.segregation,
                                  "results/null-model/sao-paulo/gender-permutation",
                                  "permutate-gender", "São Paulo", k=k)
testObservationWithNullModel(sp.segregation, sp.gen.segregation,
                            "results/null-model/sao-paulo/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(sp.segregation, sp.gen.segregation,
                                 "results/null-model/sao-paulo/gender-permutation",
                                 "São Paulo",
                                 k=k, alpha=0.01)

tokyo.gen.segregation <- runPermutate(tokyo, tokyo.segregation,
                                     "results/null-model/tokyo/gender-permutation",
                                    "permutate-gender", "Tokyo", k=k)
testObservationWithNullModel(tokyo.segregation, tokyo.gen.segregation,
                            "results/null-model/tokyo/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(tokyo.segregation, tokyo.gen.segregation,
                                 "results/null-model/tokyo/gender-permutation",
                                 "Tokyo",
                                 k=k, alpha=0.01)

riyadh.gen.segregation <- runPermutate(riyadh, riyadh.segregation,
                                       "results/null-model/riyadh/gender-permutation",
                                       "permutate-gender", "Riyadh", k=k)
testObservationWithNullModel(riyadh.segregation, riyadh.gen.segregation,
                            "results/null-model/riyadh/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(riyadh.segregation, riyadh.gen.segregation,
                                 "results/null-model/riyadh/gender-permutation",
                                 "Riyadh",
                                 k=k, alpha=0.01)

ad.gen.segregation <- runPermutate(ad, ad.segregation,
                                       "results/null-model/abu-dhabi/gender-permutation",
                                       "permutate-gender", "Abu Dhabi", k=k)
testObservationWithNullModel(ad.segregation, ad.gen.segregation,
                            "results/null-model/abu-dhabi/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(ad.segregation, ad.gen.segregation,
                                 "results/null-model/abu-dhabi/gender-permutation",
                                 "Abu Dhabi",
                                 k=k, alpha=0.01)


paris.gen.segregation <- runPermutate(paris, paris.segregation,
                                       "results/null-model/Paris/gender-permutation",
                                       "permutate-gender", "Paris", k=k)
testObservationWithNullModel(paris.segregation, paris.gen.segregation,
                            "results/null-model/paris/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(paris.segregation, paris.gen.segregation,
                                 "results/null-model/paris/gender-permutation",
                                 "Paris",
                                 k=k, alpha=0.01)


jakarta.gen.segregation <- runPermutate(jakarta, jakarta.segregation,
                                       "results/null-model/jakarta/gender-permutation",
                                       "permutate-gender", "Jakarta", k=k)
testObservationWithNullModel(jakarta.segregation, jakarta.gen.segregation,
                            "results/null-model/jakarta/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(jakarta.segregation, jakarta.gen.segregation,
                                 "results/null-model/jakarta/gender-permutation",
                                 "jakarta",
                                 k=k, alpha=0.01)
