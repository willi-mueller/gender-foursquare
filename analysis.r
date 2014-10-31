library(sqldf)

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

joinCheckInsWithProfiles <- function(checkIns, profileTable) {
  sqldf("Select * from ci")
  query <- sprintf("Select * from %s JOIN %s using(idUserFoursquare)", checkIns, profileTable)
  print(query)
  return(fn$sqldf("Select * from $checkIns JOIN $profileTable using(idUserFoursquare)"))
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

# xfit <- seq(min(completeMale$count), max(completeMale$count), length=100)
# yfit <- dnorm(xfit, mean=summary(completeMale$count)[4], sd=sd(completeMale$count))
# lines(xfit, yfit, col="red")

##########
# run
##########
country <- "Saudi Arabia"

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

ci <- readCheckIns(franceCheckIns)

users <- readUsers(franceUsers)
profiles <- cleanUsers(users, filter=franceFilter)

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

topN <- femaleUniqueSubC[1:nrow(femaleUniqueSubC),] # select all
# disable following line
topN <- filterTopNSubcategories(maleUniqueSubC, femaleUniqueSubC, 10,
                               function(x,y){abs(x+y)})

maleUniqueSubCTop <- maleUniqueSubC[topN,]
femaleUniqueSubCTop <- femaleUniqueSubC[topN,]

# correlation categories
# correlateCategories(maleC$count, femaleC$count, maleC$category, country=country)
# correlateCategories(maleUniqueC$count, femaleUniqueC$count, maleC$category, country=country,
#                     countMethod="unique users")

# correlateCategories(maleSubC$count, femaleSubC$count, maleSubC$subcategory, country=country,
#                     categories="Subcategories")
correlateCategories(maleUniqueSubCTop$count, femaleUniqueSubCTop$count, maleUniqueSubCTop$subcategory, country=country,
                    categories="10 most popular subcategories", countMethod="unique users")

#################################
# Analyze gender separation
#################################

ci <- readCheckIns(saudiCheckIns)

users <- readUsers(saudiUsers)
profiles <- cleanUsers(users, filter=saudiFilter)

joined <- joinCheckInsWithProfiles("ci", "profiles")

riyadh <- sqldf("Select * from joined where city LIKE 'Riyadh'")
friyadh <- riyadh[riyadh$gender=='female', ]
mriyadh <- riyadh[riyadh$gender=='male', ]
 # paste("Select *, count(*) as count from  (select * from ", table,
 #                      " where gender='", gender, "' group by user, ",
 #                       category, " ) group by ",  category, sep="")

xmriyadh <- sqldf("Select *, count(*) as count from
    (select * from mriyadh group by user, idLocal) group by idLocal")
xfriyadh <- sqldf("Select *, count(*) as count from
    (select * from friyadh group by user, idLocal) group by idLocal")
notInF <- sqldf("Select * from xmriyadh where idLocal not in (Select idLocal from xfriyadh)")
notInM <- sqldf("Select * from xfriyadh where idLocal not in (Select idLocal from xmriyadh)")

temp <- notInM
temp$count<-0
completeMale <- rbind(xmriyadh, temp)
completeMale <- completeMale[order(completeMale$count, decreasing=T), ]

temp <- notInF
temp$count<-0
completeFemale <- rbind(xfriyadh, temp)
completeFemale <- completeFemale[order(completeFemale$count, decreasing=T), ]

completeMaleR <- completeMale; completeFemaleR <- completeFemale
completeMaleR$count <- completeMaleR$count/sum(completeMaleR$count)
completeFemaleR$count <- completeFemaleR$count/sum(completeFemaleR$count)

completeMaleR <- completeMaleR[order(completeMaleR$idLocal, decreasing=T), ]
completeFemaleR <- completeFemaleR[order(completeFemaleR$idLocal, decreasing=T), ]

cor.test(completeMaleR$count, completeFemaleR$count)
#   Pearson's product-moment correlation
# t = 3.041, df = 844, p-value = 0.002431
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.03696506 0.17031534
# sample estimates:
#       cor
# 0.1041081
chisq.test(completeMaleR[order(completeMaleR$count, decreasing=T),]$count,
           completeFemaleR[order(completeFemaleR$count, decreasing=T),]$count)
#   Pearson's Chi-squared test
# X-squared = 3733.106, df = 80, p-value < 2.2e-16
plot(completeMaleR$count, completeFemaleR$count,
    main="Gender separation in Riyadh", xlab="male", ylab="female",
    xlim=c(0,0.05), ylim=c(0, 0.05))
abline(0, 1, col="red")

# check top locations
completeMaleR[order(completeMaleR$count, decreasing=T),][1:7,]
completeFemaleR[order(completeFemaleR$count, decreasing=T),][1:7,]


########################

ci <- readCheckIns(franceCheckIns)

users <- readUsers(franceUsers)
profiles <- cleanUsers(users, filter=franceFilter)

joined <- joinCheckInsWithProfiles("ci", "profiles")
riyadh <- sqldf("Select * from joined where city LIKE 'Paris'")
friyadh <- riyadh[riyadh$gender=='female', ]
mriyadh <- riyadh[riyadh$gender=='male', ]
xmriyadh <- sqldf("Select *, count(*) as count from
    (select * from mriyadh group by user, idLocal) group by idLocal")
xfriyadh <- sqldf("Select *, count(*) as count from
    (select * from friyadh group by user, idLocal) group by idLocal")
notInF <- sqldf("Select * from xmriyadh where idLocal not in (Select idLocal from xfriyadh)")
notInM <- sqldf("Select * from xfriyadh where idLocal not in (Select idLocal from xmriyadh)")

temp <- notInM
temp$count<-0
completeMale <- rbind(xmriyadh, temp)
completeMale <- completeMale[order(completeMale$count, decreasing=T), ]

temp <- notInF
temp$count<-0
completeFemale <- rbind(xfriyadh, temp)
completeFemale <- completeFemale[order(completeFemale$count, decreasing=T), ]

completeMaleR <- completeMale; completeFemaleR <- completeFemale
completeMaleR$count <- completeMale$count/sum(completeMale$count)
completeFemaleR$count <- completeFemale$count/sum(completeFemale$count)

completeMaleR <- completeMaleR[order(completeMaleR$idLocal, decreasing=T), ]
completeFemaleR <- completeFemaleR[order(completeFemaleR$idLocal, decreasing=T), ]

cor.test(completeMaleR$count, completeFemaleR$count)
# Pearson's product-moment correlation

# data:  completeMaleR$count and completeFemaleR$count
# t = 19.5576, df = 1008, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4782692 0.5677967
# sample estimates:
#       cor
# 0.5244812
chisq.test(completeMaleR[order(completeMaleR$count, decreasing=T),]$count,
           completeFemaleR[order(completeFemaleR$count, decreasing=T),]$count)
#X-squared = 8595.182, df = 140, p-value < 2.2e-16

plot(completeMaleR$count, completeFemaleR$count,
    main="Gender separation in Paris", xlab="male", ylab="female",
    xlim=c(0,0.05), ylim=c(0, 0.05))
abline(0, 1, col="red")

# check top locations
completeMaleR[order(completeMaleR$count, decreasing=T),][1:7,]
completeFemaleR[order(completeFemaleR$count, decreasing=T),][1:7,]



##################################

ci <- readCheckIns(uaeCheckIns)

users <- readUsers(uaeUsers)
profiles <- cleanUsers(users, filter=uaeFilter)

joined <- joinCheckInsWithProfiles("ci", "profiles")

riyadh <- sqldf("Select * from joined where city LIKE 'Abu Dhabi'")
friyadh <- riyadh[riyadh$gender=='female', ]
mriyadh <- riyadh[riyadh$gender=='male', ]
 # paste("Select *, count(*) as count from  (select * from ", table,
 #                      " where gender='", gender, "' group by user, ",
 #                       category, " ) group by ",  category, sep="")

xmriyadh <- sqldf("Select *, count(*) as count from
    (select * from mriyadh group by user, idLocal) group by idLocal")
xfriyadh <- sqldf("Select *, count(*) as count from
    (select * from friyadh group by user, idLocal) group by idLocal")
notInF <- sqldf("Select * from xmriyadh where idLocal not in (Select idLocal from xfriyadh)")
notInM <- sqldf("Select * from xfriyadh where idLocal not in (Select idLocal from xmriyadh)")

temp <- notInM
temp$count<-0
completeMale <- rbind(xmriyadh, temp)
completeMale <- completeMale[order(completeMale$count, decreasing=T), ]

temp <- notInF
temp$count<-0
completeFemale <- rbind(xfriyadh, temp)
completeFemale <- completeFemale[order(completeFemale$count, decreasing=T), ]

completeMaleR <- completeMale; completeFemaleR <- completeFemale
completeMaleR$count <- completeMaleR$count/sum(completeMaleR$count)
completeFemaleR$count <- completeFemaleR$count/sum(completeFemaleR$count)

completeMaleR <- completeMaleR[order(completeMaleR$idLocal, decreasing=T), ]
completeFemaleR <- completeFemaleR[order(completeFemaleR$idLocal, decreasing=T), ]

cor.test(completeMaleR$count, completeFemaleR$count)

#   Pearson's product-moment correlation

# data:  completeMaleR$count and completeFemaleR$count
# t = -3.8159, df = 365, p-value = 0.0001593
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.29237214 -0.09540704
# sample estimates:
#        cor
# -0.1958642

chisq.test(completeMaleR[order(completeMaleR$count, decreasing=T),]$count,
           completeFemaleR[order(completeFemaleR$count, decreasing=T),]$count)
# X-squared = 989.6645, df = 24, p-value < 2.2e-16

plot(completeMaleR$count, completeFemaleR$count,
    main="Gender separation in Abu Dhabi", xlab="male", ylab="female",
    xlim=c(0,0.05), ylim=c(0, 0.05))
abline(0, 1, col="red")

# check top locations
completeMaleR[order(completeMaleR$count, decreasing=T),][1:7,]
completeFemaleR[order(completeFemaleR$count, decreasing=T),][1:7,]





##################################

getCheckInsInCity <- function(cityName, countryCheckIns, countryUsers, countryFilter) {
  ci <- readCheckIns(countryCheckIns)

  sqldf("Select * from ci")
  users <- readUsers(germanyUsers)
  profiles <- cleanUsers(users, filter=germanyFilter)

  joined <- joinCheckInsWithProfiles("ci", "profiles")
  print(joined)
  # string <- sprintf("Select * from joined where city LIKE %s", shQuote(cityName))
  # printf(string)
  # checkInsInCity <- sqldf(string)
  # return(checkInsInCity)
}


citySegregation <- function(checkInsInCity, cityName) {
  fCity <- checkInsInCity[checkInsInCity$gender=='female', ]
  m_city <- checkInsInCity[checkInsInCity$gender=='male', ]
   # paste("Select *, count(*) as count from  (select * from ", table,
   #                      " where gender='", gender, "' group by user, ",
   #                       category, " ) group by ",  category, sep="")

  mCityLocations <- sqldf("Select *, count(*) as count from
      (select * from m_city group by user, idLocal) group by idLocal")
  fCityLocations <- sqldf("Select *, count(*) as count from
      (select * from fCity group by user, idLocal) group by idLocal")
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

  #   Pearson's product-moment correlation

  # data:  completeMaleR$count and completeFemaleR$count
  # t = -3.8159, df = 365, p-value = 0.0001593
  # alternative hypothesis: true correlation is not equal to 0
  # 95 percent confidence interval:
  #  -0.29237214 -0.09540704
  # sample estimates:
  #        cor
  # -0.1958642

  print(chisq.test(completeMaleR[order(completeMaleR$count, decreasing=T),]$count,
             completeFemaleR[order(completeFemaleR$count, decreasing=T),]$count))
  # X-squared = 171.2095, df = 15, p-value < 2.2e-16

  plot(completeMaleR$count, completeFemaleR$count,
      main=paste("Gender separation in", cityName), xlab="male", ylab="female",
      xlim=c(0,0.05), ylim=c(0, 0.05))
  abline(0, 1, col="red")

  # check top locations
  completeMaleR[order(completeMaleR$count, decreasing=T),][1:7,]
  completeFemaleR[order(completeFemaleR$count, decreasing=T),][1:7,]
}

checkInsInCity <- getCheckInsInCity("Berlin", germanyCheckIns, germanyUsers, germanyFilter)
citySegregation(checkInsInCity, "Berlin")


#####################################
# does not work
inner <- function() {
  print(sqldf("select * from foo"))
}
outer <- function() {
  foo <- data.frame(1,2,3)
  inner()
}

outer()


# works
outer <- function() {
  foo <- data.frame(1,2,3)
  inner(foo)
}
inner <- function(bar) {
  print(sqldf("select * from bar"))
}

outer()

