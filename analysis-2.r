library(sqldf)
library(Hmisc) # Ecdf

subcategoryPreferencesByGender <- function(countryCheckIns, countryUsers, countryFilter, country) {
  joined <- getCheckInsInCountry(countryCheckIns, countryUsers, countryFilter)

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

correlateTopCategories <- function(group1, group2, country, categories, countMethod) {
  top <- getTopNCategories(group1, group2)

  correlateCategories(top$group1$count,
                      top$group2$count,
                      labels=top$group1$subcategory,
                      country=country,
                      categories=categories,
                      countMethod=countMethod)
}

getTopNCategories <- function(group1, group2){
  topN <- group1[1:nrow(group1),] # select all
  # disable following line
  topN <- filterTopNSubcategories(group2, group1, 10,
                                 function(x,y){abs(x+y)})

  group2Top <- group2[topN,]
  group1Top <- group1[topN,]
  return(list(group1=group1Top, group2=group2Top))
}

getCheckInsInCountry <- function(countryCheckIns, countryUsers, countryFilter, substitutionRules) {
  ci <- readCheckIns(countryCheckIns)
  users <- readUsers(countryUsers)
  profiles <- cleanUsers(users, filter=countryFilter)
  joined <- joinCheckInsWithProfiles(ci, profiles)
  if(!missing(substitutionRules)) {
    joined <- combineEquivalentSubCategories(joined, substitutionRules)
  }
  return(joined)
}

getCheckInsInRegion <- function(regionFilters, countryCheckIns, countryUsers, countryFilter, substitutionRules) {
  joined <- getCheckInsInCountry(countryCheckIns, countryUsers, countryFilter, substitutionRules)

  checkInsInRegion <- c()
  for(filter in regionFilters) {
    checkInsInRegion <- rbind(checkInsInRegion, sqldf(sprintf("Select * from joined where city LIKE %s", shQuote(filter))))
  }
  return(subset(checkInsInRegion, !duplicated(checkInsInRegion)))
}

combineEquivalentSubCategories <- function(checkIns, substitutionRules) {
  for(pair in substitutionRules) {
    for(equivalent in pair$equivalents) {
      if(equivalent %in% checkIns$subcategory) {
        tmp <- checkIns[checkIns$subcategory==equivalent, ]
        tmp$subcategory <- pair$original
      }
    }
  }
  return(checkIns)
}

segregation <- function(checkIns, location) {
  checkIns <- completeCheckInsByGenderForRegion(checkIns)
  completeFemale <- checkIns$female
  completeMale <- checkIns$male

  completeMaleR <- relativeCount(completeMale)
  completeFemaleR <- relativeCount(completeFemale)

  # check if the same subcategories exist
  # currently violated in Paris, where idLocal==4ba12ba3f964a520849e37e3 has different subcategories
  if(identical(unique(completeMale$subcategory[order(completeMale$subcategory)]),
               unique(completeFemale$subcategory[order(completeFemale$subcategory)]))) {
    print("Subcategories differ between genders")
  }

  print(cor.test(completeMaleR$count, completeFemaleR$count))

  print(chisq.test(completeMaleR[order(completeMaleR$count, decreasing=T),]$count,
             completeFemaleR[order(completeFemaleR$count, decreasing=T),]$count))

  plot(completeMaleR$count, completeFemaleR$count,
      main=paste("Gender separation in", location), xlab="male", ylab="female",
      xlim=c(0,0.05), ylim=c(0, 0.05))
  abline(0, 1, col="red")

  printTopLocations(completeMaleR, "male")
  printTopLocations(completeFemaleR, "female")

  return(list(maleCIR=completeMaleR, femaleCIR=completeFemaleR))
}

aggregateSegregationForRegion <- function(regionCheckIns, region) {
  seg <- segregation(regionCheckIns, region)
  distances <- euclideanDistance(seg$maleCIR$count, seg$femaleCIR$count)
  boxplot(distances)
  return(summary(distances))
}

completeCheckInsByGenderForRegion <- function(checkIns) {
  femaleCI <- checkIns[checkIns$gender=='female', ]
  maleCI <- checkIns[checkIns$gender=='male', ]

  maleLocations <- countLocationsByGender(maleCI)
  femaleLocations <- countLocationsByGender(femaleCI)

  notInF <- locationsNotCheckedInByGender(femaleLocations, maleLocations)
  notInM <- locationsNotCheckedInByGender(maleLocations, femaleLocations)

  completeFemale <- completeLocationsWithOtherGender(femaleLocations, notInF)
  completeMale <- completeLocationsWithOtherGender(maleLocations, notInM)

  stopifnot( length(completeMale$count) == length(completeFemale$count) )
  stopifnot( completeMale[order(completeMale$idLocal), ]$idLocal ==
             completeFemale[order(completeFemale$idLocal), ]$idLocal )
  return(list(female=completeFemale, male=completeMale))
}

relativeCount <- function(checkIns) {
  checkInsRelative <- checkIns
  checkInsRelative$count <- checkInsRelative$count/sum(checkInsRelative$count)
  # sort
  return(checkInsRelative[order(checkInsRelative$idLocal, decreasing=T), ])
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

countLocationsByGender <- function(checkInsOfGender) {
    mCityLocations <- sqldf("Select *, count(*) as count from
      (select * from checkInsOfGender group by user, idLocal) group by idLocal")
}

topLocations <- function(checkIns, n=7) {
    return(checkIns[order(checkIns$count, decreasing=T),][1:n,])
}

printTopLocations <- function(checkIns, gender) {
  top <- topLocations(checkIns)
  print(sprintf("Top %s:", gender))
  print(top[, c("gender", "count", "subcategory", "idLocal", "latitude", "longitude")])
}

readUsers <- function(path) {
  fullPath <- paste("~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/datasets/", path, sep="")
  saU <- read.csv(fullPath, header=F, sep="\t")
  colnames(saU) <- c("idUserFoursquare", "user", "userLocal", "gender")
  return(saU)
}

readCheckIns <- function(path) {
  fullPath <- paste("~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/datasets/", path, sep="")
  sa <- read.csv(fullPath, header=F, sep="\t")
  colnames(sa) <- c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                     "subcategory", "category", "city", "country")
  return(sa)
}

cleanUsers <- function(users, filter) {
  return(users[grep(filter, ignore.case=T, users$userLocal), ])
}

joinCheckInsWithProfiles <- function(checkIns, profiles) {
  return(sqldf("Select * from checkIns JOIN profiles using(idUserFoursquare)"))
}

categoriesByGender <- function(joinedTable, gender, uniqueUsers=FALSE, subcategory=FALSE) {
  category <- "category"
  if(subcategory==TRUE) {
    category <- "subcategory"
  }
  queryString <- paste("Select *, count(*) as count from joinedTable where gender='",
    gender, "' group by ", category, sep="")
  if(uniqueUsers==TRUE) {
    queryString <- paste("Select *, count(*) as count from  (select * from joinedTable where gender='",
      gender, "' group by user, ",
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

correlateCategories <- function(x, y, labels, country, countMethod="check-ins",
                                categories="Categories",
                                xlim=0.1, ylim=0.1) {
  plot(x, y, main=sprintf("Correlation of %s counting %s in %s",categories, countMethod, country),
       xlab="Male", ylab="Female", xlim=c(0, xlim), ylim=c(0, ylim))
  abline(0, 1, col="red")
  text(x, y, labels=labels, pos=3)
  print(cor.test(x, y))
  print(chisq.test(x, y))
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
  stopifnot(length(masculine[,1]) == length(feminine[,1]))
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

compareDistanceSegregationsIn <- function(checkInsInCategory1, checkInsInCategory2, regionName1, regionName2, name) {
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

generateCheckIn <- function(checkIns) {
  n <- length(checkIns[,1])
  distributions <- list(
    idUserFoursquare=empiricalAttributeDistribution(checkIns, "idUserFoursquare")
    ,date=empiricalAttributeDistribution(checkIns, "date")
    ,latitude=empiricalAttributeDistribution(checkIns, "latitude")
    ,longitude=empiricalAttributeDistribution(checkIns, "longitude")
    ,idLocal=empiricalAttributeDistribution(checkIns, "idLocal")
    ,subcategory=empiricalAttributeDistribution(checkIns, "subcategory")
    ,category=empiricalAttributeDistribution(checkIns, "category")
    ,city=empiricalAttributeDistribution(checkIns, "city")
    ,country=empiricalAttributeDistribution(checkIns, "country")
    ,user=empiricalAttributeDistribution(checkIns, "user")
    ,userLocal=empiricalAttributeDistribution(checkIns, "userLocal")
    ,gender=empiricalAttributeDistribution(checkIns, "gender")
  )
  checkIns <- data.frame(
    idUserFoursquare=sample(distributions$idUserFoursquare$attribute, n, prob=distributions$percentage, replace=TRUE)
    ,date=sample(distributions$date$attribute, n, prob=distributions$date$percentage, replace=TRUE)
    ,latitude=sample(distributions$latitude$attribute, n, prob=distributions$latitude$percentage, replace=TRUE)
    ,longitude=sample(distributions$longitude$attribute, n, prob=distributions$longitude$percentage, replace=TRUE)
    ,idLocal=sample(distributions$idLocal$attribute, n, prob=distributions$idLocal$percentage, replace=TRUE)
    ,subcategory=sample(distributions$subcategory$attribute, n, prob=distributions$subcategory$percentage, replace=TRUE)
    ,category=sample(distributions$category$attribute, n, prob=distributions$category$percentage, replace=TRUE)
    ,city=sample(distributions$city$attribute, n, prob=distributions$city$percentage, replace=TRUE)
    ,country=sample(distributions$country$attribute, n, prob=distributions$country$percentage, replace=TRUE)
    ,user=sample(distributions$user$attribute, n, prob=distributions$user$percentage, replace=TRUE)
    ,userLocal=sample(distributions$userLocal$attribute, n, prob=distributions$userLocal$percentage, replace=TRUE)
    ,gender=sample(distributions$gender$attribute, n, prob=distributions$gender$percentage, replace=TRUE)
  )
}

empiricalAttributeDistribution <- function(checkIns, attribute) {
  ## attribute = {"category", "subcategory", "idLocal", "latitude", "gender", …}
  distribution <- sqldf(sprintf("select %s as attribute, count(*) as percentage from checkIns group by %s", attribute, attribute))
  distribution$percentage <- distribution$percentage / sum(distribution$percentage)
  return(distribution)
}

##########
# Constants
##########

saudiCheckIns <- "base2/arabiaSaudita/Saudi-Arabia.txt"
franceCheckIns <- "base2/France.txt"
swedenCheckIns <- "base2/Sweden.txt"
uaeCheckIns <- "base2/United-Arab-Emirates.txt"
germanyCheckIns <- "base2/Germany.txt"

franceFilter <- "Paris|France|Metz|Bordeaux|Marseille|Midi-Py|Strasbourg|Lyon"
swedenFilter <- "Sverige|Sweden|Stockholm|Malmö"
saudiFilter <- "Saudi|Mecca|Medina|Riya|الرياض|Jedda|"  # الرياض = Riyadh
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
data <- subcategoryPreferencesByGender(uaeCheckIns, uaeUsers, uaeFilter, country)

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

correlateTopCategories(data$maleUniqueSubcategories, data$femaleUniqueSubcategories,
                       country,
                       "unique users",
                       "10 most popular subcategories")


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

compareDistanceSegregationsIn(r.cafe, ad.cafe, "Riyadh", "Abu Dhabi", "Café")

############

countries <- list(
  list(checkIns=franceCheckIns, users=franceUsers, filter=franceFilter, name="France")
  ,list(checkIns=germanyCheckIns, users=germanyUsers, filter=germanyFilter, name="Germany")
  ,list(checkIns=swedenCheckIns, users=swedenUsers, filter=swedenFilter, name="Sweden")
  ,list(checkIns=saudiCheckIns, users=saudiUsers, filter=saudiFilter, name="Saudi Arabia")
  ,list(checkIns=uaeCheckIns, users=uaeUsers, filter=uaeFilter, name="UAE")
  )

genderDistanceForCountry(countries, substitutionRules, "Distribution of gender distance")

s <- segregation(generateCheckIn(getCheckInsInCountry(saudiCheckIns, saudiUsers, saudiFilter, substitutionRules)), "Saudi Arabia")
