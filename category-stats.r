library(parallel)
library(data.table)
source('analysis/foursquare-analysis.r')

N_CORES <- detectCores()
THRESH <- 100
MAX_CI <- 4e+5
k <- 100
countryFiles <- rev(dir("paises"))
categoryStats <- list()
categoryStats <- data.frame() # global to save it in the workspace image
locationStats <- data.frame() # global to save it in the workspace image
allCheckIns <- data.frame()

collectStatisticsForRanking <- function() {
	for(i in 1:length(countryFiles)) {
	#readAndCalc <- function(i) {
		f <- sprintf("paises/%s", countryFiles[i])
		country <- strsplit(countryFiles[i], ".dat", fixed=T)[[1]]
		if(country %in% c("Brazil", "United-States", "Indonesia", "France", "Japan", "Saudi-Arabia", "Russia")) {
			message(country)

			ci <- readAndFilterCheckIns(f, 1000)
			if(nrow(ci) > 0) {
				allCheckIns <<- rbindlist(list(allCheckIns, ci))

				stats <- calculateStats(ci, country)
				print(stats)
				categoryStats <<- rbindlist( list(categoryStats, stats$categoryStats))
				locationStats <<- rbindlist( list(locationStats, stats$locationStats))
			}
		}
	}
	# global assignment
	#categoryStats <<- mclapply(1:length(countryFiles), readAndCalc, mc.cores=N_CORES)
	save.image()
	#categoryStats <<- rbindlist(categoryStats) # filter not NA/NULL elements
	#save.image()
	print(categoryStats)
	write.table(locationStats, "results/null-model/location-stats-all-countries-unified-subc.csv", sep="\t", row.names=FALSE)
	write.table(categoryStats, "results/null-model/category-stats-all-countries-unified-subc.csv", sep="\t", row.names=FALSE)
	write.table(allCheckIns, "results/cleaned-check-ins-1000.csv", sep="\t", row.names=FALSE)
}

##############
# Calculation
#############

calculateStats <- function(ci, region) {
	ci <- resampleIfTooMuchCheckIns(ci)
	folderName <- sprintf("results/null-model/%s/gender-permutation", region)
	generated <- runPermutate(ci, folderName, "permutate-gender", region, k=k, forceGenerate=F)
	# segregation() is crucial, the others need male and female popularity,
	ci <- segregation(ci, region, log=F)

	locationStats <- testObservationWithNullModel(ci, generated, folderName, country, k, PLOT_ANOM_DIST=T)
	categoryStats <- getBootstrappedStatistics(folderName, ci, generated, k, region, alpha=0.01)
	return(list(categoryStats=categoryStats$bootstrapStats, locationStats=locationStats))
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
