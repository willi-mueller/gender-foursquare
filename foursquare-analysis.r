# library(sqldf)
library(Hmisc) # Ecdf
library(data.table)
library(moments) # skewness
library(parallel) #mclapply

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
#################
# Read Data
################

readAndFilterCheckIns <- function(f, thresh=THRESH) {
  cc <- list(integer=c(1, 12), character=c(2, seq(5, 11)), numeric=c(3, 4))
  ci <- try(fread(f, header=F, sep="\t", stringsAsFactors=FALSE, colClasses=cc))
  if(length(ci)>2) {
    if(nrow(ci) < thresh) {
      message("< ", thresh, " check-ins")
    } else {
      setnames(ci, 1:12, c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                  "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))

      filtered <- cleanData(ci, substitutionRules)
      stopifnot(length(unique(filtered$gender)) == 2 ) # only male and female
      if(nrow(filtered) > thresh) {
        return(combineEquivalentSubCategories(filtered, substitutionRules))
      }
    }
  }
  return(data.table())
}

getCheckInsInCountry <- function(countryCheckIns, substitutionRules, countryUsers, userLocalFilter) {
  ci <- readCheckIns(countryCheckIns)
  if(!missing(countryUsers)) {
    users <- readUsers(countryUsers)
    profiles <- discardNonResidents(users, filter=userLocalFilter)
    ci <- joinCheckInsWithProfiles(ci, profiles)
  }
  if(!missing(userLocalFilter)) {
    ci <- discardNonResidents(ci, filter=userLocalFilter)
  }
  return(cleanData(ci, substitutionRules))
}

cleanData <- function(ci, substitutionRules) {
  ci <- ci[gender== "male" | gender=="female", ]
  ci<- oneCheckInForUserAndLocation(ci)
  ci <- combineEquivalentSubCategories(ci, substitutionRules)
  return(checkInsInlocationsWithMinimumCheckIns(ci, n=5))
}

oneCheckInForUserAndLocation <- function(checkIns) {
  stopifnot(haskey(checkIns)==FALSE)
  setkey(checkIns, idLocal, idUserFoursquare)
  checkIns <- unique(checkIns)
  setkey(checkIns, NULL)
  return(checkIns)
}

checkInsInlocationsWithMinimumCheckIns <- function(checkIns, n=5) {
  locations <- checkIns[, list(hasMore=length(unique(idUserFoursquare))>=n), by=idLocal][hasMore==TRUE]$idLocal
  return(checkIns[idLocal %in% locations, ])
}

filterSelectedCategories <- function(ci, allowed=c("Arts", "Food", "Education", "Nightlife", "Work")) {
  inAllowedCategories <- ci[category %in% allowed]
  sufficientlyPopularSubc <- inAllowedCategories[, subcategory[length(unique(idLocal))>=2], by=subcategory]$subcategory
  return( ci[subcategory %in% sufficientlyPopularSubc] )
}

getCheckInsInRegion <- function(regionFilters, countryCheckIns, countryUsers, userLocalFilter, substitutionRules, checkIns) {
  if(missing(checkIns)) {
    checkIns <- getCheckInsInCountry(countryCheckIns, substitutionRules, countryUsers, userLocalFilter)
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

segregationSubcategories <- function(checkIns, axeslim=c(0,1)) {
  malePop <- checkIns[, list(maleSum=sum(maleCount)), by=subcategory]
  femalePop <- checkIns[, list(femaleSum=sum(femaleCount)), by=subcategory]
  normFactor <- max(malePop$maleSum, femalePop$femaleSum)

  joined <- cbind(malePop, femalePop)
  joined$maleSum <- joined$maleSum/normFactor
  joined$femaleSum <- joined$femaleSum/normFactor

  plot(joined$maleSum, joined$femaleSum,
      main=NULL, xlab="Sum of male popularity of locations in subcategory", ylab="Sum of female popularity of locations in subcategory",
      xlim=axeslim, ylim=axeslim)
  abline(0, 1, col="red")

  top <- joined[, diff:=abs(maleSum)+abs(femaleSum)][order(-rank(diff))][1:5]
  top <- rbindlist(list( top, joined[order(-femaleSum)][1:5]) )
  top <- rbindlist(list (top, joined[order(-maleSum)][1:5]) )
  top <- unique(top)
  x <- c(); y<-c()
  for(subc in top$subcategory) {
    x <- c(x, joined[subcategory==subc]$maleSum)
    y <- c(y, joined[subcategory==subc]$femaleSum)
  }
  text(x,y, label=top$subcategory, pos=2)
  return(joined)
}

segregation <- function(checkIns, location="<location>", sub=NULL, axeslim=SEGREGATION_AXES, log=TRUE) {
  # given that we have 1 checkin for user and location

  nMaleUsers <- length(unique(checkIns[, idUserFoursquare[gender=="male"]]))
  nFemaleUsers <- length(unique(checkIns[, idUserFoursquare[gender=="female"]]))

  checkIns[, maleCount:=sum(gender=='male')/nGenderCheckIns(checkIns, "male"), by=idLocal]
  checkIns[, femaleCount:=sum(gender=='female')/nGenderCheckIns(checkIns, "female"), by=idLocal]

  checkIns <- replace(checkIns, is.na(checkIns), 0)

  popularityByLocation <- checkIns[, .SD[1], by=idLocal]

  if(log==TRUE) {
    plot(popularityByLocation$maleCount, popularityByLocation$femaleCount,
      main=paste("Gender separation in", location), sub=sub, xlab="Male Popularity", ylab="Female Popularity",
      xlim=axeslim, ylim=axeslim)
    abline(0, 1, col="red")
    print(cor.test(popularityByLocation$maleCount, popularityByLocation$femaleCount))

    print("Top male")
    print(popularityByLocation[, list(maleCount, subcategory, category, idLocal), by=idLocal][order(-rank(maleCount))][1:10])
    print("Top female")
    print(popularityByLocation[, list(femaleCount, subcategory, category, idLocal), by=idLocal][order(-rank(femaleCount))][1:10])
  }
  return(checkIns)
}

aggregateSegregationForRegion <- function(regionCheckIns, region) {
  seg <- segregation(regionCheckIns, region)
  distances <- euclideanDistance(seg$maleCount, seg$femaleCount)
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
  ci <- fread(path, header=F, sep="\t", stringsAsFactors=FALSE)
  setnames(ci, 1:12,c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                      "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))
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

correlateCategories <- function(countrySegregation, country, file,
                                categories="Categories",
                                axeslim=SEGREGATION_AXES) {
  x <- countrySegregation[,list(pop=mean(maleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
  y <- countrySegregation[,list(pop=mean(femaleCount)), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
  labels <- x$category
  if(!missing(file)) {
    pdf(file)
  }
  plot(x$pop, y$pop, main=sprintf("Correlation of %s in %s",categories, country),
       xlab="Male Popularity", ylab="Female Popularity", xlim=axeslim, ylim=axeslim)
  abline(0, 1, col="red")
  text(x$pop, y$pop, labels=labels, pos=3)
  print(cor.test(x$pop, y$pop))
  print(chisq.test(x$pop, y$pop))
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
  stopifnot(length(male) == length(female))
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
    ci <- getCheckInsInCountry(country$checkIns, substitutionRules, country$users, country$filter)
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
runPermutate <- function(checkIns, folderName, plotName, regionName, k=100, log=FALSE, forceGenerate=FALSE) {
  generatedFile <- sprintf("%s/generated-%s-%s-pop.csv", folderName, regionName, plotName)
  if(file.exists(generatedFile) & !forceGenerate) {
    message("Already randomized check-ins for ", regionName)
    cc <- list(integer=c("idUserFoursquare", "timeOffset"),
        caracter=c("date","idLocal", "subcategory", "category", "country", "city", "district", "gender"),
        numeric=c("maleCount", "femaleCount"))
    return(fread(generatedFile, header=T, sep="\t", stringsAsFactors=FALSE, colClasses=cc))
  } else {
    if( !file.exists(folderName) | forceGenerate ) {
      dir.create(folderName, recursive=TRUE)
    }
    checkIns <- checkIns[gender=="male" || gender=="female", ]
    randomizedCheckIns <- copy(checkIns)
    calc <- function(i) {
      gen.checkIns <- permutateGender(randomizedCheckIns)
      gen.checkIns[,iterPermutation:=i]
      segregation(gen.checkIns, regionName,
                  sub="permutating gender", log=log)
      return(gen.checkIns)
    }

    message("Generating check-ins for ", regionName)
    gen.segregation <- rbindlist( mclapply(seq(k), calc, mc.cores=N_CORES), use.names=TRUE)
    stopifnot(length(unique(gen.segregation$iterPermutation))==k)
    message("Generated ", regionName, ". Writing…")
    write.table(gen.segregation, generatedFile, sep="\t", row.names=FALSE)
    message("Wrote generated segregation of ", regionName, " to ", generatedFile)

    return(gen.segregation)
  }
}

permutateGender <- function(checkIns) {
  # Expects only a single check-in per user per local
  # TODO: shuffle gender of USERS not check-ins

# this copy() is important!! If removed,
# iterPermutation will have N_CORE distinct values
  copy(checkIns)[, gender:=sample(checkIns[, gender])]

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
                                         k,
                                         SEARCH_ANOMALOUS_LOCATIONS=TRUE,
                                         PLOT_ANOM_DIST=F, axeslim=SEGREGATION_AXES, alpha=0.01) {
  meanMalePopularities <- c()
  meanFemalePopularities <- c()
  uniqueLocations <- unique(observedSegregation$idLocal)
  nUniqueLocations <- length(uniqueLocations)

  locationStats <- function(i) {
    location <- uniqueLocations[i]

    iterLocation <- gen.segregation[idLocal==location, ]
    malePopularity <- iterLocation[, .SD[1], by=iterPermutation]$maleCount
    femalePopularity <- iterLocation[, .SD[1], by=iterPermutation]$femaleCount
    meanMalePopularities <- c(meanMalePopularities, mean(malePopularity))
    meanFemalePopularities <- c(meanFemalePopularities, mean(femalePopularity))

    if (SEARCH_ANOMALOUS_LOCATIONS) {
      empiricalDifference <- malePopularity - femalePopularity
      percentile <- quantile(empiricalDifference, c(alpha/2, 1-alpha/2))
      observedMale <- observedSegregation[idLocal==location, ]$maleCount[[1]] # same value for each check-in
      observedFemale <- observedSegregation[idLocal==location, ]$femaleCount[[1]]
      observedDifference <- observedMale - observedFemale
      isAnomalous <- ( observedDifference < percentile[1] | observedDifference > percentile[2] )
      if(isAnomalous) {
        if(PLOT_ANOM_DIST) {
          filename <- sprintf("%s/location-%s-anomalous.csv", folderName, location)
          write.table(data.table(idLocal=uniqueLocations[i],
                                 empiricalDifference=empiricalDifference,
                                 observedDifference=observedDifference), filename)
          filename <- sprintf("%s/location-%s-anomalous.pdf", folderName, location)

          pdf(filename)
          hist(c(empiricalDifference, observedDifference), xlab=NULL, main=NULL)
          abline(v=percentile[1], col="green")
          abline(v=percentile[2], col="green")
          abline(v=observedDifference, col="blue")
          dev.off()
        }
      }
    }
    return(data.table(idLocal=location, subcategory=iterLocation$subcategory[[1]],
                      category=iterLocation$category[[1]], city=iterLocation$city[[1]],
                      country=iterLocation$country[[1]], malePopularity=observedMale, femalePopularity=observedFemale,
                      meanMalePopularity=mean(malePopularity), meanFemalePopularity=mean(femalePopularity),
                      difference=observedDifference, lowerPercentile=percentile[1], upperPercentile=percentile[2],
                      isAnomalous=isAnomalous))

  }
  allLocationStats <- rbindlist( mclapply(seq(nUniqueLocations), locationStats, mc.cores=N_CORES) )

  summary_ <- allLocationStats[, list(nAnomalous=sum(isAnomalous),
                                     nLocations=length(idLocal),
                                     percAnomalous=length(isAnomalous[isAnomalous==T])/length(isAnomalous))]
  message(sprintf("Distance to diagonal: %s of %s (%s%%) locations with observed anomalous segregation",
                  summary_$nAnomalous, summary_$nLocations,
                  round(100*summary_$percAnomalous, 3)))
  f <- sprintf("%s/location-stats-generated-%s.csv", folderName, regionName)
  write.table(allLocationStats, f)
  f2 <- sprintf("%s/location-stats-generated-%s-summary.csv", folderName, regionName)
  write.table(summary_, f2)

  pdf(sprintf("%s/avg-segregation-generated-%s.pdf", folderName, regionName))

  plot(allLocationStats$meanMalePopularity, allLocationStats$meanFemalePopularity,
        xlim=axeslim, ylim=axeslim,
        xlab="Mean male popularity", ylab="Mean female popularity")
  abline(0, 1, col="red")
  dev.off()
  return(allLocationStats)
}

getBootstrappedStatistics <- function(plotFolder, observed, generated, k, region, alpha=0.01) {
  observedStats <- calculateCategoryStats(observed)
  calc <- function(i) {
    stat <- calculateCategoryStats(generated[iterPermutation==i])
    stat$bootstrapIter <- i
    return(stat)
  }
  genStats <- rbindlist( mclapply(seq(k), calc, mc.cores=N_CORES) )
  bootstrapStats <- flagAnomalousSubcategories(observedStats, genStats, k, alpha, plotFolder, region)
  plotGeneratedMeans(plotFolder, region,
                    bootstrapStats$meanMaleSubcPop, bootstrapStats$meanFemaleSubcPop,
                    bootstrapStats$subcategory, axeslim=c(0, 1))
  # plotGeneratedMeans(plotFolder, bootstrapStats$meanMaleSubcPop, bootstrapStats$meanFemaleSubcPop, bootstrapStats$category))
  return(list(observedStats=observedStats, bootstrapStats=bootstrapStats))
}

calculateCategoryStats <- function(checkIns) {
  checkIns <- percentagesOfGenderForCategory(checkIns)
  checkIns <- percentagesForCategory(checkIns)
  checkIns <- percentagesForSubcategory(checkIns)
  checkIns <- euclideanDistanceForCategory(checkIns)
  checkIns <- euclideanDistanceForSubcategory(checkIns)
  checkIns <- popularityOfGenderForSubcategory(checkIns)
  checkIns <- euclideanDistanceForSubcategoryPopularity(checkIns)
  checkIns[, .SD[1], by=subcategory ][, list(
    country, category, subcategory, percMaleCat, percFemaleCat,
    percMaleSubc, percFemaleSubc,
    malePopCat, femalePopCat, eucDistCat, eucDistSubc,
    malePopSubC, femalePopSubC, eucDistSubcPop)]
}

flagAnomalousSubcategories <- function(observedStats, genStats, k, alpha, plotFolder, region) {
  nSubcategories <- nrow(genStats)/k

  calc <- function(i) {
    statsForSubc <- genStats[seq(i, nSubcategories*k, nSubcategories)]
    percentiles.eucDistSubc <- quantile(statsForSubc$eucDistSubc, c(alpha/2, 1-alpha/2))
    observed.eucDistSubc <- observedStats[ subcategory==statsForSubc$subcategory[1], ]$eucDistSubc

    percentiles.eucDistSubcPop <- quantile(statsForSubc$eucDistSubcPop, c(alpha/2, 1-alpha/2))
    observed.eucDistSubcPop <- observedStats[ subcategory==statsForSubc$subcategory[1], ]$eucDistSubcPop
    stats <- list(
      eucDistSubc = ( observed.eucDistSubc < percentiles.eucDistSubc[[1]] | observed.eucDistSubc > percentiles.eucDistSubc[[2]] ),
      eucDistSubcLowerQuantile = percentiles.eucDistSubc[[1]],
      eucDistSubcUpperQuantile = percentiles.eucDistSubc[[2]],
      eucDistSubcGenMean = mean(statsForSubc$eucDistSubc),
      eucDistSubcGenMedian = median(statsForSubc$eucDistSubc),

      meanMaleSubcPop = mean(statsForSubc$malePopSubC),
      meanFemaleSubcPop  = mean(statsForSubc$femalePopSubC),
      eucDistSubcPop = ( observed.eucDistSubcPop < percentiles.eucDistSubcPop[[1]] | observed.eucDistSubcPop > percentiles.eucDistSubcPop[[2]] ),
      eucDistSubcPopLowerQuantile = percentiles.eucDistSubcPop[[1]],
      eucDistSubcPopUpperQuantile = percentiles.eucDistSubcPop[[2]],
      eucDistSubcPopGenMean = mean(statsForSubc$eucDistSubcPop),
      eucDistSubcPopGenMedian = median(statsForSubc$eucDistSubcPop)
    )
    plotCategoryDist(plotFolder, region, statsForSubc$subcategory[[1]], isAnomalous=stats$eucDistSubc,
                              statsForSubc$eucDistSubcPop, observed.eucDistSubcPop,
                              stats$eucDistSubcPopLowerQuantile,
                              stats$eucDistSubcPopUpperQuantile)
    return(stats)
  }
  statsPerSubc <- writeObservedValues(genStats, observedStats)
  isAnomalous <- mclapply(seq(nSubcategories), calc, mc.cores=N_CORES)

  isAnomalous.eucDistSubc <- unlist(lapply(isAnomalous, function(x)x$eucDistSubc))

  statsPerSubc$eucDistSubcLowerQuantile <- unlist(lapply(isAnomalous, function(x)x$eucDistSubcLowerQuantile))
  statsPerSubc$eucDistSubcUpperQuantile <- unlist(lapply(isAnomalous, function(x)x$eucDistSubcUpperQuantile))
  statsPerSubc$eucDistSubcGenMean <-  unlist(lapply(isAnomalous, function(x)x$eucDistSubcGenMean))
  statsPerSubc$eucDistSubcGenMedian <-  unlist(lapply(isAnomalous, function(x)x$eucDistSubcGenMedian))
  statsPerSubc$eucDistSubcIsAnomalous <- isAnomalous.eucDistSubc

  nAnomalous <- length(isAnomalous.eucDistSubc[isAnomalous.eucDistSubc==TRUE])
  percOfAnomalousSubc <- nAnomalous/nSubcategories
  statsPerSubc$percAnomalousEucDistSubc <- percOfAnomalousSubc

  ############
  isAnomalous.eucDistSubcPop <- unlist(lapply(isAnomalous, function(x)x$eucDistSubcPop))

  statsPerSubc$eucDistSubcPopLowerQuantile <- unlist(lapply(isAnomalous, function(x)x$eucDistSubcPopLowerQuantile))
  statsPerSubc$eucDistSubcPopUpperQuantile <- unlist(lapply(isAnomalous, function(x)x$eucDistSubcPopUpperQuantile))
  statsPerSubc$eucDistSubcPopGenMean <-  unlist(lapply(isAnomalous, function(x)x$eucDistSubcPopGenMean))
  statsPerSubc$eucDistSubcPopGenMedian <-  unlist(lapply(isAnomalous, function(x)x$eucDistSubcPopGenMedian))
  statsPerSubc$eucDistSubcPopIsAnomalous <- isAnomalous.eucDistSubcPop
  statsPerSubc$meanMaleSubcPop <- unlist(lapply(isAnomalous, function(x) x$meanMaleSubcPop ))
  statsPerSubc$meanFemaleSubcPop <- unlist(lapply(isAnomalous, function(x) x$meanFemaleSubcPop))

  nAnomalous2 <- length(isAnomalous.eucDistSubcPop[isAnomalous.eucDistSubcPop==TRUE])
  percOfAnomalousSubcPop <- nAnomalous2/nSubcategories
  statsPerSubc$percAnomalousEucDistSubcPop <- percOfAnomalousSubcPop

  statsPerSubc$alpha <- alpha
  statsPerSubc$bootstrapIter <- NULL

  return(statsPerSubc)
}

OUTDATED_testObservationWithNullModelForCategories <- function(observedSegregation, gen.segregation, folderName, regionName,
                                                    k,
                                                    UNIFORM_LOCATION_PROBABILITY=FALSE,
                                                    UNIFORM_GENDER_PROBABILITY=FALSE,
                                                    SEARCH_ANOMALOUS_CATEGORIES=TRUE,
                                                    PLOT_ALL_DISTS=TRUE, axeslim=c(0, 0.4), alpha=0.01){
  ########### OUTDATED? #############
  stopifnot(c(catOrSubCat) %in% c(quote(category), quote(subcategory)))
  stopifnot(nrow(observedSegregation) * k == nrow(gen.segregation))

  if (SEARCH_ANOMALOUS_CATEGORIES) {
    empiricalDist <- euclideanDistance(maleCategoryPopularities, femaleCategoryPopularities)
    nCategories <- length(observedSegregation[,unique(eval(catOrSubCat))])
    stopifnot(nCategories == length(empiricalDist)/k)

    categoryDistDistribution <- list()
    for (i in seq(nCategories)) {
      allOfACategory <- empiricalDist[seq(i, length(empiricalDist), by=nCategories)]
      categoryDistDistribution[i] <- list(allOfACategory)
    }
    percentiles <- lapply(categoryDistDistribution, FUN=quantile, c(alpha/2, 1-alpha/2))

    observedMale <- categoryPopularity(observedSegregation, quote(maleCount))
    observedFemale <- categoryPopularity(observedSegregation, quote(femaleCount))
    observedDist <- euclideanDistance(observedMale, observedFemale)

    sortedCategories <- c()
    sortedCategories <- gen.male.mean[, category]

    categoryStats <- data.table(region=regionName,
                                category=sortedCategories,
                                observedDistance=observedDist,
                                isAnomalous=F, alpha=alpha,
                                lowerPercentile=lapply(percentiles, function(x) x[[1]]),
                                upperPercentile=lapply(percentiles, function(x) x[[2]]),
                                variance=var(observedDist),
                                skewness=skewness(observedDist))
    categoryStats[, lowerPercentile:=as.numeric(lowerPercentile)]
    categoryStats[, upperPercentile:=as.numeric(upperPercentile)]

    anomalyCount <- 0
    for(i in seq(nCategories)) {
      if(observedDist[i] < percentiles[[i]][1] | observedDist[i] > percentiles[[i]][2]) {
          anomalyCount <- anomalyCount + 1
          categoryStats[i]$isAnomalous <- T
          message(sprintf("Anomalous category: %s, distance: %s\n",
                        sortedCategories[i], signif(observedDist[i])))
          message(sprintf("%s of %s (%s%%) categories with observed anomalous segregation",
                anomalyCount, nCategories,
                100*round(anomalyCount/nCategories, 3)))
          plotCategoryDist(folderName, sortedCategories,
                            categoryDistDistribution, observedDist, percentiles, i)
      } else {
            if(PLOT_ALL_DISTS) {
              plotCategoryDist(folderName, sortedCategories,
                            categoryDistDistribution, observedDist, percentiles, i)
            }
      }
    }
  }
  pdf(sprintf("%s/avg-segregation-generated-categories-%s.pdf", folderName, regionName))
  plot(gen.male.mean$pop, gen.female.mean$pop,
        main=sprintf("Gender separation in generated %s", regionName),
        sub=sprintf("uniform location: %s, uniform gender: %s, k=%s",
            UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY, k),
        xlim=axeslim, ylim=axeslim,
        xlab="Male Popularity", ylab="Female Popularity")
  abline(0, 1, col="red")
  text(gen.male.mean$pop, gen.female.mean$pop, labels=sortedCategories, pos=3)
  dev.off()
  return(categoryStats)
writeObservedValues <- function(genStats, observedStats) {
  # get data format from first bootstrap iteration of each subcategory
  statsPerSubc <- genStats[, .SD[1], by=subcategory]
  # verify order
  stopifnot(observedStats$subcategory == genStats[, .SD[1], by=subcategory]$subcategory)
  statsPerSubc$eucDistSubcPop <- observedStats$eucDistSubcPop
  statsPerSubc$malePopSubC <- observedStats$malePopSubC
  statsPerSubc$femalePopSubC <- observedStats$femalePopSubC
  statsPerSubc$percMaleCat <- observedStats$percMaleCat
  statsPerSubc$percFemaleCat <- observedStats$percFemaleCat
  statsPerSubc$percMaleSubc <- observedStats$percMaleSubc
  statsPerSubc$percFemaleSubc <- observedStats$percFemaleSubc
  statsPerSubc$malePopCat  <- observedStats$malePopCat
  statsPerSubc$femalePopCat <- observedStats$femalePopCat
  statsPerSubc$eucDistCat <- observedStats$eucDistCat
  statsPerSubc$eucDistSubc <- observedStats$eucDistSubc
  return(statsPerSubc)
}

plotCategoryDist <- function(folderName, categoryName, isAnomalous,
                             categoryDistDistribution, observedDist,
                             lowerPercentile, upperPercentile) {
  filename <- ""
  if(isAnomalous) {
    filename <- sprintf("%s/%s-category-%s-anomalous", folderName, region, categoryName)
  } else {
    filename <- sprintf("%s/%s-category-%s", folderName, region, categoryName)
  }
  filename <- gsub(" / ", "--", filename) # for Monument / Landmark
  pdf(sprintf("%s.pdf", filename))
  hist(c(categoryDistDistribution, observedDist), xlab="gender distance")
  abline(v=lowerPercentile, col="green")
  abline(v=upperPercentile, col="green")
  abline(v=observedDist, col="blue")
  dev.off()
  write.table(data.table(category=categoryName,
                          observed=observedDist, generated=categoryDistDistribution,
                          lowerPercentile=lowerPercentile, upperPercentile=upperPercentile,
                          isAnomalous=isAnomalous),
              file=sprintf("%s.csv", filename),
              row.names=FALSE, sep="\t")
}

plotGeneratedMeans <- function(folderName, regionName, meanMale, meanFemale, categories, axeslim=SEGREGATION_AXES) {
  write.table(data.table(region=regionName, meanMalePop=meanMale,
                        meanFemalePop=meanFemale, subcategory=categories),
             row.names=F, sep="\t", file=sprintf("%s/mean-segregation-generated-categories-%s.csv", folderName, regionName))
  pdf(sprintf("%s/mean-segregation-generated-categories-%s.pdf", folderName, regionName))
  plot(meanMale, meanFemale,
        main=NULL, #sprintf("Gender separation in generated %s", regionName),
        xlim=axeslim, ylim=axeslim,
        xlab="Percentage of check-ins by male users", ylab="Percentage of check-ins by female users")
  abline(0, 1, col="red")
  text(meanMale, meanFemale, labels=categories, pos=3)
  dev.off()
}

sortByCategory <- function(ci) {
  ci[order(rank(category))]
}

sortBySubcategory <- function(ci) {
  ci[order(rank(subcategory))]
}


###################### Analysing Null Model ######

nGenderUsers <- function(checkIns, genderStr) {
  stopifnot(genderStr %in% c("male", "female"))
  as.numeric(checkIns[, list(n=length(unique(idUserFoursquare[ gender==genderStr ]))) ]$n)
}

nGenderCheckIns <- function(checkIns, genderStr) {
  stopifnot(genderStr %in% c("male", "female"))
  as.numeric(checkIns[, list(n=nrow( checkIns[ gender==genderStr ] )) ]$n)
}


############# Percentage #####
percentagesOfGenderForCategory <- function(checkIns) {
  nMaleCheckIns <- nGenderCheckIns(checkIns, "male")
  nFemaleCheckIns <- nGenderCheckIns(checkIns, "female")
  sortByCategory( checkIns[, `:=`(
                malePopCat=as.numeric(length(gender[gender=='male'])/nMaleCheckIns),
                femalePopCat=as.numeric(length(gender[gender=='female'])/nFemaleCheckIns)),
            by=category] )
  # deal with case 0/0 == NaN
  replace(checkIns, is.na(checkIns), 0)
}

popularityOfGenderForSubcategory <- function(checkIns) {
  nMaleCheckIns <- nGenderCheckIns(checkIns, "male")
  nFemaleCheckIns <- nGenderCheckIns(checkIns, "female")
   sortBySubcategory( checkIns[, `:=`(
                malePopSubC=as.numeric(length(gender[gender=='male'])/nMaleCheckIns),
                femalePopSubC=as.numeric(length(gender[gender=='female'])/nFemaleCheckIns)),
            by=subcategory] )
  replace(checkIns, is.na(checkIns), 0)
}

percentagesForCategory <- function(checkIns) {
  sortByCategory( checkIns[, `:=`(
                percMaleCat=as.numeric(length(idUserFoursquare[gender=='male'])/length(idUserFoursquare)),
                percFemaleCat=as.numeric(length(idUserFoursquare[gender=='female'])/length(idUserFoursquare))),
            by=category] )
  replace(checkIns, is.na(checkIns), 0)
}

percentagesForSubcategory <- function(checkIns) {
   sortBySubcategory( checkIns[, `:=`(
                percMaleSubc=as.numeric(length(idUserFoursquare[gender=='male'])/length(idUserFoursquare)),
                percFemaleSubc=as.numeric(length(idUserFoursquare[gender=='female'])/length(idUserFoursquare))),
            by=subcategory] )
  replace(checkIns, is.na(checkIns), 0)
}

############# Euclidean Distance #####
euclideanDistanceForCategory <- function(checkIns) {
   sortByCategory( checkIns[,eucDistCat:=euclideanDistance(percMaleCat, percFemaleCat), by=category] )
}

euclideanDistanceForSubcategory <- function(checkIns) {
   sortBySubcategory( checkIns[,eucDistSubc:=euclideanDistance(percMaleSubc, percFemaleSubc), by=subcategory] )
}

euclideanDistanceForSubcategoryPopularity <- function(checkIns) {
   sortBySubcategory( checkIns[,eucDistSubcPop:=euclideanDistance(malePopSubC, femalePopSubC), by=subcategory] )
}

euclideanDistanceForLocation <- function(checkIns) {
   sortByCategory( checkIns[ ,eucDistLoc:=euclideanDistance(percMaleLoc, percFemaleLoc), by=idLocal] )
}

meanEuclidDist <- function(checkIns) {
   sortByCategory( checkIns[ ,meanEucDistLoc:=mean(eucDistLoc), by=category] )
}

medianEuclidDist <- function(checkIns) {
   sortByCategory( checkIns[ ,list(medianEucDistLoc=median(eucDistLoc)), by=category] )
}

varEuclidDist <- function(checkIns) {
   sortByCategory( checkIns[ ,list(varEucDistLoc=var(eucDistLoc)), by=category] )
}

sdEuclidDist <- function(checkIns) {
   sortByCategory( checkIns[ ,list(sdEucDistLoc=sd(eucDistLoc)), by=category] )
}

skewnessEuclidDist <- function(checkIns) {
   sortByCategory( checkIns[ ,list(skewnessEucDistLoc=skewness(eucDistLoc)), by=category] )
}


############# Difference of percentages #####
differenceOfLocation <- function(checkIns) {
   sortByCategory( checkIns[, list(diffLoc=percMaleLoc - percFemaleLoc), by=idLocal] )
}

differenceOfCategory <- function(checkIns) {
   sortByCategory( checkIns[, list(diffCat=percMaleLoc - percFemaleLoc), by=category] )
}

meanDifferenceOfCategory <- function(checkIns){
   sortByCategory( checkIns[, list(meanDiff=mean(diffLoc)), by=category] )
}

medianDifferenceOfCategory <- function(checkIns){
   sortByCategory( checkIns[, list(medianDiff=median(diffLoc)), by=category] )
}

varDifferenceOfCategory <- function(checkIns){
   sortByCategory( checkIns[, list(varDiff=var(diffLoc)), by=category] )
}

sdDifferenceOfCategory <- function(checkIns){
   sortByCategory( checkIns[, list(sdDiff=sd(diffLoc)), by=category] )
}

skewnessDifferenceOfCategory <- function(checkIns){
   sortByCategory( checkIns[, list(skewnessDiff=skewness(diffLoc)), by=category] )
}

############# Accumulated popularity #####
categoryPopularity <- function(ci, genderCount) {
  # old stuff: describe difference and popularity in one number
  ci <- ci[,list(pop=mean(eval(genderCount))), by=list(idLocal, category)][,list(pop=sum(pop)), by=category]
  return(ci[order(rank(category))])
}

readGeneratedDataAndPlot <- function(segregationFile, folderName, regionName,
                                     UNIFORM_LOCATION_PROBABILITY, UNIFORM_GENDER_PROBABILITY) {
  gen.segregation <-c()
  gen.segregation$maleCIR <- read.csv(malePopularityFile)
  gen.segregation$femaleCIR <- read.csv(femalePopularityFile)
  testObservationWithNullModel(gen.segregation, folderName, regionName,
                               UNIFORM_GENDER_PROBABILITY, UNIFORM_LOCATION_PROBABILITY,
                               SEARCH_ANOMALOUS_LOCATIONS=F, PLOT_ANOM_DISTS=F)
}

checkInsInlocationsWithMinimumCheckIns <- function(checkIns, n=5) {
  locations <- checkIns[, list(hasMore=length(unique(idUserFoursquare))>=n), by=idLocal][hasMore==TRUE]$idLocal
  return(checkIns[idLocal %in% locations, ])
}


##########
# Constants
##########
N_CORES <- detectCores()
SEGREGATION_AXES <- c(0, 0.20)

substitutionRules <- list(
    list(original="Café", equivalents=c("Coffee Shop", "College Cafeteria")),
    list(original="Airport", equivalents=c("Airport Lounge", "Airport Gate", "Airport Terminal")),
    list(original="Train Station", equivalents=c("Train", "Platform", "Light Rail")),
    list(original="Bus Station", equivalents=c("Bus Line", "Bus Terminal")),
    list(original="Library", equivalents=c("College Library")),
    list(original="Movie Theater", equivalents=c("Indie Movie Theater", "Multiplex")),
    list(original="University", equivalents=c("General College & University", "College & University")),
    list(original="Gym", equivalents=c("Gym / Fitness Center", "College Gym")))

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