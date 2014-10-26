library(sqldf)

getCheckInsInCity <- function(cityName, countryCheckIns, countryUsers, countryFilter) {
  ci <- readCheckIns(countryCheckIns)
  users <- readUsers(germanyUsers)
  profiles <- cleanUsers(users, filter=germanyFilter)

  joined <- joinCheckInsWithProfiles(ci, profiles)
  checkInsInCity <- sqldf(sprintf("Select * from joined where city LIKE %s", shQuote(cityName)))
  return(checkInsInCity)
}


citySegregation <- function(checkInsInCity, cityName) {
  fCity <- checkInsInCity[checkInsInCity$gender=='female', ]
  mCity <- checkInsInCity[checkInsInCity$gender=='male', ]

  countLocationsByGender <- function(checkInsOfGender) {
    mCityLocations <- sqldf("Select *, count(*) as count from
      (select * from checkInsOfGender group by user, idLocal) group by idLocal")
  }

  mCityLocations <- countLocationsByGender(mCity)
  fCityLocations <- countLocationsByGender(fCity)

  notInF <- sqldf("Select * from mCityLocations where idLocal not in (Select idLocal from fCityLocations)")
  notInM <- sqldf("Select * from fCityLocations where idLocal not in (Select idLocal from mCityLocations)")


  temp <- notInM
  temp$count<-0
  completeMale <- rbind(mCityLocations, temp)
  completeMale <- completeMale[order(completeMale$count, decreasing=T), ]

  temp <- notInF
  temp$count<-0
  completeFemale <- rbind(fCityLocations, temp)
  completeFemale <- completeFemale[order(completeFemale$count, decreasing=T), ]

  completeMaleR <- completeMale; completeFemaleR <- completeFemale
  completeMaleR$count <- completeMaleR$count/sum(completeMaleR$count)
  completeFemaleR$count <- completeFemaleR$count/sum(completeFemaleR$count)

  completeMaleR <- completeMaleR[order(completeMaleR$idLocal, decreasing=T), ]
  completeFemaleR <- completeFemaleR[order(completeFemaleR$idLocal, decreasing=T), ]

  print(cor.test(completeMaleR$count, completeFemaleR$count))

  print(chisq.test(completeMaleR[order(completeMaleR$count, decreasing=T),]$count,
             completeFemaleR[order(completeFemaleR$count, decreasing=T),]$count))

  plot(completeMaleR$count, completeFemaleR$count,
      main=paste("Gender separation in", cityName), xlab="male", ylab="female",
      xlim=c(0,0.05), ylim=c(0, 0.05))
  abline(0, 1, col="red")

  printTopLocations(completeMaleR, "male")
  printTopLocations(completeFemaleR, "female")
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
       xlab="Male", ylab="Female", xlim=c(0,0.1), ylim=c(0, 0.1))
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

plot_pdf <- function(x, xlab="% of unique users checked in", ylab="Frequency") {
  # probability density function
  h <- hist(completeFemaleR$count, breaks = 100, plot=FALSE)
  h$counts=h$counts/sum(h$counts)
  plot(h, xlab=xlab, ylab=ylab)
}

plot_density <-function(x) {
  plot(density(x))
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
country <- "Germany"
checkInsInCity <- getCheckInsInCity("Berlin", germanyCheckIns, germanyUsers, germanyFilter)
citySegregation(checkInsInCity, "Berlin")