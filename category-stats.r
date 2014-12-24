library(parallel)
library(data.table)
source('analysis/foursquare-analysis.r')

baseFolder <- "results/null-model-3"
N_CORES <- detectCores()
THRESH <- 1000
MAX_CI <- 3e+5
k <- 100
countryFiles <- dir("paises")
categoryStats <- data.frame() # global to save it in the workspace image
locationStats <- data.frame() # global to save it in the workspace image
allCheckIns <- data.frame()

collectStatisticsForRanking <- function() {
	for(i in 1:length(countryFiles)) {
	#readAndCalc <- function(i) {
		f <- sprintf("paises/%s", countryFiles[i])
		country <- strsplit(countryFiles[i], ".dat", fixed=T)[[1]]
		if(country %in% c("Germany", "France", "Spain", "United-Kingdom",
				 "United-States", "Brazil", "Mexico",
				 "United-Arab-Emirates", "Saudi-Arabia", "Kuwait", # run Turkey manually in R shell
				 "South-Korea", "Malaysia", "Japan", "Thailand")) {
			# c("Brazil", "United States", "Indonesia", "France", "Singapore", "Saudi Arabia", "Russia")
			message(country)

			ci <- readAndFilterCheckIns(f, THRESH)
			ci <- filterSelectedCategories(ci)
			ci <- resampleIfTooMuchCheckIns(ci)
			message(nrow(ci), " check-ins are going to be analyzed")
			if(nrow(ci) > 0) {
				allCheckIns <<- rbindlist(list(allCheckIns, ci))

				stats <- calculateStats(ci, country)
				categoryStats <<- rbindlist( list(categoryStats, stats$categoryStats))
				locationStats <<- rbindlist( list(locationStats, stats$locationStats))
			}
		}
	}
	save.image()
	print(categoryStats)
	write.table(locationStats, sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
	write.table(categoryStats, sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
	write.table(allCheckIns, sprintf("%s/cleaned-check-ins-1000-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
}

##############
# Calculation
#############

calculateStats <- function(ci, region) {
	folderName <- sprintf("%s/%s/gender-permutation", baseFolder, region)
	generated <- runPermutate(ci, folderName, "permutate-gender", region, k=k, forceGenerate=T)
	# segregation() is crucial, the others need male and female popularity,
	ci <- segregation(ci, region, log=F)

	locationStats <- testObservationWithNullModel(ci, generated, folderName, region, k, PLOT_ANOM_DIST=T)
	categoryStats <- getBootstrappedStatistics(folderName, ci, generated, k, region, alpha=0.01)
	return(list(categoryStats=categoryStats$bootstrapStats, locationStats=locationStats))
}

resampleIfTooMuchCheckIns <- function(ci) {
	n <- nrow(ci)
	if(n > MAX_CI) {
		message("Too many check-ins")
		ci <- ci[sample(n, MAX_CI, replace=FALSE)]
		message("resampled check-ins")
		stopifnot(nrow(ci) <= MAX_CI)
	}
	return(ci)
}

##############
# Run
#############

collectStatisticsForRanking()
