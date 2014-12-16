library(parallel)
library(data.table)
source('analysis/foursquare-analysis.r')

N_CORES <- detectCores()
THRESH <- 100
MAX_CI <- 4e+5
k <- 100
countryFiles <- rev(dir("paises"))
categoryStats <- list()
oneTable <- data.frame() # global to save it in the workspace image

collectStatisticsForRanking <- function() {
	for(i in 1:length(countryFiles)) {
	#readAndCalc <- function(i) {
		f <- sprintf("paises/%s", countryFiles[i])
		country <- strsplit(countryFiles[i], ".", fixed=T)[[1]][[1]] # remove .dat
		message(country)
		ci <- readCheckIns(f)
		if(nrow(ci) > 0) {
			oneTable <<- rbind(oneTable, calculateStats(ci, country))
		} else {
			oneTable <<- rbind(oneTable, data.table())
		}
	}
	# global assignment
	#categoryStats <<- mclapply(1:length(countryFiles), readAndCalc, mc.cores=N_CORES)
	save.image()
	#oneTable <<- rbindlist(categoryStats)
	#save.image()
	print(oneTable)
	write.table(oneTable, "results/null-model/category-stats.csv", sep="\t", row.names=FALSE)
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

			return(cleanData(ci))
		}
	}
	return(data.table())
}

##############
# Calculation
#############

calculateStats <- function(ci, country) {
	ci <- resampleIfTooMuchCheckIns(ci)
	folderName <- sprintf("results/null-model/%s/gender-permutation", country)
	generated <- runPermutate(ci, folderName, "permutate-gender", country, k=k)
	# ci.segregation <- segregation(ci, country, log=F)

	stats <- getBootstrappedStatistics(ci, generated, k, alpha=0.01)
	return(stats)
}

resampleIfTooMuchCheckIns <- function(ci) {
	n <- nrow(ci)
	if(n > MAX_CI) {
		message("Too many check-ins")
		ci <- ci[sample(n, 3e+4, replace=FALSE)]
		message("resampled check-ins")
		stopifnot(nrow(ci) < MAX_CI)
	}
	return(ci)
}

##############
# Run
#############

collectStatisticsForRanking()
