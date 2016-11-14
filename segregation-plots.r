#
# Segregation plots for city and country
#

segregationLocationStats <- function(locationStats, normFactor, axeslim=c(0,1)) {
  malePop <- locationStats$malePopularity
  femalePop <- locationStats$femalePopularity
  if(missing(normFactor)) {
    normFactor <- max(malePop, femalePop)
  }

  plot(malePop/normFactor, femalePop/normFactor,
      main=NULL, xlab="Male popularity", ylab="Female popularity",
      xlim=axeslim, ylim=axeslim)
  abline(0, 1, col="red")
  print(locationStats[order(-femalePopularity)][1:10])
  print(locationStats[order(-malePopularity)][1:10])
  # top <- c() # locationStats[, sum:=abs(malePop)+abs(femalePop)][order(-rank(sum))][1:6]
  # top <- rbindlist(list( top, locationStats[order(-femalePopularity)][1:10]) )
  # top <- rbindlist(list (top, locationStats[order(-malePopularity)][1:10]) )
  # top <- unique(top)
  # x <- c(); y<-c()
  # for(subc in top$subcategory) {
  #   x <- c(x, locationStats[idLocal %in% top$idLocal]$malePopularity)
  #   y <- c(y, locationStats[idLocal %in% top$idLocal]$femalePopularity)
  # }


  # locationLabels<-c("University Anhembi Morumbi", "University UNINOVO", "Event Space", "Bakery",
  #            "Café", "University Mackenzie", "University", "University", "Pub", "Art Museum Bienal",
  #             "Movie Theater", "Gay Bar The Week", "Gay Bar Bubu Lounge Disco", "Starbucks R. Haddock Lobo",
  #             "Movie Theater", "Gay Bar Club Yacht", "Nightclub")
  # text(x/normFactor,y/normFactor, label=locationLabels, pos=1)
}



generatedSegregationLocationStats <- function(locationStats, normFactor, axeslim=c(0,1)) {
  malePop <- locationStats$meanMalePopularity
  femalePop <- locationStats$meanFemalePopularity
  if(missing(normFactor)) {
    normFactor <- max(malePop, femalePop)
  }

  plot(malePop/normFactor, femalePop/normFactor,
      main=NULL, xlab="Male popularity", ylab="Female popularity",
      xlim=axeslim, ylim=axeslim)
  abline(0, 1, col="red")

  # top <- c() # locationStats[, sum:=abs(malePop)+abs(femalePop)][order(-rank(sum))][1:6]
  # top <- rbindlist(list( top, locationStats[order(-meanFemalePopularity)][1:10]) )
  # top <- rbindlist(list (top, locationStats[order(-malePopularity)][1:10]) )
  # top <- unique(top)
  # x <- c(); y<-c()
  # for(subc in top$subcategory) {
  #   x <- c(x, locationStats[idLocal %in% top$idLocal]$meanFemalePopularity)
  #   y <- c(y, locationStats[idLocal %in% top$idLocal]$meanMalePopularity)
  # }


  # locationLabels<-c("Movie Theater", "Bakery", "University Anhembi Morumbi", "Café", "Gay Bar The Week",
  #             "University", "University Mackenzie", "University", "Starbucks R. Haddock Lobo", "Movie Theater", "University",
  #            "Gay Bar Bubu Lounge Disco",  "Gay Bar Club Yacht", "Nightclub")
  #text(x/normFactor,y/normFactor, label=locationLabels, pos=1)
}

# Example for São Paulo and São Paulo Nightclub
sp.stats <- fread("results/null-model-3/Sao Paulo/bootstrap/location-stats-generated-Sao Paulo.csv")
sp.night.stats <- fread("results/null-model-3/Sao Paulo Nightclub/bootstrap/location-stats-generated-Sao Paulo Nightclub.csv")
segregationLocationStats(sp.stats)
segregationLocationStats(sp.night.stats)
generatedSegregationLocationStats(sp.stats, normFactor=1) # to not move dots to the upper right corner
generatedSegregationLocationStats(sp.night.stats, normFactor=1) # to not move dots to the upper right corner


countrySegregationPlot <- function(countryStats) {
  # already summed popularity across subcategory and normalized
  plot(countryStats$maleSum, countryStats$femaleSum,
       main=NULL, xlab="Male popularity", ylab="Female popularity",
       xlim=c(0,1), ylim=c(0,1))
   abline(0, 1, col="red")
}

generatedCountrySegregationPlot <- function(countryStats) {
  plot(us.catstats$meanMaleSubcPop, us.catstats$meanFemaleSubcPop,
         main=NULL, xlab="Male popularity", ylab="Female popularity",
         xlim=c(0,1), ylim=c(0,1))
  abline(0, 1, col="red")
}

# Example for United States
allCatStats <- fread("results/null-model-3/server-2016-11-13/category-stats-15-countries-5-categories.csv")
us.allCatStats <- catstats[country=="United States"]
us.catStats <- fread("results/null-model-3/server-2016-11-04/United-States/segregation-subcategories.csv")
generatedCountrySegregationPlot(us.allCatStats)
countrySegregationPlot(us.catStats)
