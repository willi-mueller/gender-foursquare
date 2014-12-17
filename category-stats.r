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
		country <- strsplit(countryFiles[i], ".dat", fixed=T)[[1]]
		message(country)
		ci <- readCheckIns(f, 10000)
		if(nrow(ci) > 0) {
			oneTable <<- rbindlist(list(oneTable, calculateStats(ci, country)))
		} else {
			oneTable <<- rbindlist(list(oneTable, data.table()))
		}
	}
	# global assignment
	#categoryStats <<- mclapply(1:length(countryFiles), readAndCalc, mc.cores=N_CORES)
	save.image()
	#oneTable <<- rbindlist(categoryStats) # filter not NA/NULL elements
	#save.image()
	print(oneTable)
	write.table(oneTable, "results/null-model/category-stats.csv", sep="\t", row.names=FALSE)
}

#################
# Read Data
################

readCheckIns <- function(f, thresh=THRESH) {
	cc <- list(integer=c(1, 12), character=c(2, seq(5, 11)), numeric=c(3, 4))
	ci <- try(fread(f, header=F, sep="\t", stringsAsFactors=FALSE, colClasses=cc))
	if(length(ci)>2) {
		if(nrow(ci) < thresh) {
			message("< ", thresh, " check-ins")
		} else {
			setnames(ci, 1:12, c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
		              "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))

			filtered <- cleanData(ci)
			stopifnot(length(unique(filtered$gender)) == 2 ) # only male and female
			if(nrow(filtered) > thresh) {
				return(filtered)
			}
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

	getBootstrappedStatistics(ci, generated, k, alpha=0.01)
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
