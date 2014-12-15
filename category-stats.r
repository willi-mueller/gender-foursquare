#library(doParallel)
library(data.table)
source('analysis/foursquare-analysis.r')

THRESH <- 100
MAX_CI <- 4e+5
countryFiles <- dir("paises")
categoryStats <- list() # global to save it in the workspace image

generateNullModel <- function() {
	k <- 100
	# stats <- foreach( i=c(19, 24, 25), .combine=function(x,y)rbindlist(list(x,y)) ) %dopar% {
	for(i in c(19, 24, 25)) {
		f <- sprintf("paises/%s", countryFiles[i])
		country <- strsplit(countryFiles[i], ".", fixed=T)[[1]][[1]] # remove .dat
		message(country)
		ci <- readCheckIns(f)
		if(nrow(ci > 0)) {
			categoryStats[[i]] <<- calculateStats(ci, country)
		}
	}
	# global assignment
	categoryStats <<- rbindlist(categoryStats)
	save.image()
	print(categoryStats)
	write.table(categoryStats, "results/null-model/category-stats.csv", sep="\t", row.names=FALSE)
}

#################
# Clean Data
################

cleanData <- function(ci) {
	ci <- ci[gender== "male" | gender=="female", ]
	ci<- oneCheckInForUserAndLocation(ci)
	ci <- checkInsInlocationsWithMinimumCheckIns(ci, n=5)
}

checkInsInlocationsWithMinimumCheckIns <- function(checkIns, n=5) {
  locations <- checkIns[, list(hasMore=length(unique(idUserFoursquare))>=n), by=idLocal][hasMore==TRUE]$idLocal
  return(checkIns[idLocal %in% locations, ])
}

oneCheckInForUserAndLocation <- function(checkIns) {
  setkey(checkIns, idLocal, idUserFoursquare)
  checkIns <- unique(checkIns)
  setkey(checkIns, NULL)
  return(checkIns)
}

readCheckIns <- function(f) {
	cc <- list(integer=c(1, 12), character=c(2, seq(5, 11)), numeric=c(3, 4))
	ci <- try(fread(f, header=F, sep="\t", stringsAsFactors=FALSE, colClasses=cc))
	if(length(ci)>2) {
		if(nrow(ci) < THRESH) {
			message("<", THRESH, "check-ins")
		} else {
			setnames(ci, 1:12, c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
		              "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))

			ci <- cleanData(ci)
		}
	} else {
		return(data.table())
	}
}

##############
# Calculation
#############

calculateStats <- function(ci, country) {
	ci <- resampleIfTooMuchCheckIns(ci)
	gen.segregation <- runPermutate(ci,
									sprintf("results/null-model/%s/gender-permutation", country),
                          			"permutate-gender", country, k=k)
	ci.segregation <- segregation(ci, country, log=F)

	folderName <- sprintf("results/null-model/%s/gender-permutation", country)
	stats <- testObservationWithNullModelForCategories(ci.segregation,
														gen.segregation, folderName, country,
                                    					k, quote(category))
	return(stats)
}

resampleIfTooMuchCheckIns <- function(ci) {
	n <- nrow(ci)
	if(n > MAX_CI) {
		message("Too many check-ins")
		ci <- ci[sample(n, 3e+5, replace=FALSE)]
		message("resampled check-ins")
		stopifnot(nrow(ci) < MAX_CI)
	}
	return(ci)
}

##############
# Run
#############

generateNullModel()
