#####################
# Analyses of categories for ranking
####################

library(parallel)
library(data.table)
source('analysis/foursquare-analysis.r')

baseFolder <- "results/null-model-5-dev"
N_CORES <- detectCores()
THRESH <- 100
MAX_CI <- Inf #Inf to disable filter for large data set
k <- 100
DATA_DIR <- "newData"
countryFiles <- dir(DATA_DIR) # "paises" for old data set
categoryStats <- data.frame() # global to save it in the workspace image
locationStats <- data.frame() # global to save it in the workspace image
allCheckIns <- data.frame()
ZCAT <- "zcat" # for OSX: `zcat < file.gz`, for linux: `zcat file.gz`

RUN_TURKEY = FALSE

collectStatisticsForRanking <- function(countries) {
	if(missing(countries)) {
		countries <- c("Germany", "Brazil", "France", "Spain", "United-Kingdom",
				 "United-States", "Brazil", "Mexico",
				 "United-Arab-Emirates", "Saudi-Arabia", "Kuwait", #"Turkey", # run Turkey manually in R shell
				 "South-Korea", "Malaysia", "Japan", "Thailand")
# 				# c("Brazil", "United States", "Indonesia", "France", "Singapore", "Saudi Arabia", "Russia")
	}
	for(i in 1:length(countryFiles)) {
	#readAndCalc <- function(i) {
		f <- sprintf("%s/%s", DATA_DIR, countryFiles[i])
		country <- strsplit(countryFiles[i], ".dat.gz", fixed=T)[[1]]
		if(country %in% countries) {
			start <- Sys.time()
			message(country)

			ci <- readAndFilterCheckIns(f, THRESH)
			ci <- filterSelectedCategories(ci)
			ci <- resampleIfTooMuchCheckIns(ci)
			message("#### Reading took: ", Sys.time()-start)
			message("#### ", nrow(ci), " check-ins are going to be analyzed")
			start <- Sys.time()
			if(nrow(ci) > 0) {
				allCheckIns <<- rbindlist(list(allCheckIns, ci))

				stats <- calculateStats(ci, country)
				categoryStats <<- rbindlist( list(categoryStats, stats$categoryStats))
				locationStats <<- rbindlist( list(locationStats, stats$locationStats))
			}
			message("#### Analyses took: ", Sys.time()-start)
		}
	}
	save.image()
	print(categoryStats)
	write.table(locationStats, sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
	write.table(categoryStats, sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
	write.table(allCheckIns, gzfile(sprintf("%s/cleaned-check-ins-1000-15-countries-5-categories.csv.gz", baseFolder)),
				sep="\t", row.names=FALSE)
}

subcategorySegregationPlots <- function() {
	for(country in c("Germany", "France", "Spain", "United-Kingdom",
				 	"United-States", "Brazil", "Mexico",
			 		"United-Arab-Emirates", "Saudi-Arabia", "Kuwait", "Turkey",
			 		"South-Korea", "Malaysia", "Japan", "Thailand")) {
		message(country)
		f <- sprintf("%s/%s.dat.gz", DATA_DIR, country)
		ci <- readAndFilterCheckIns(f, THRESH)
		ci <- filterSelectedCategories(ci)
		ci <- resampleIfTooMuchCheckIns(ci)
		message(nrow(ci), " check-ins are going to be analyzed")

		segregation(ci)
		pdf(sprintf("%s/%s/gender-permutation/segregation-subcategories.pdf", baseFolder, country))
		segregationData <- segregationSubcategories(ci)
		dev.off()
		write.table(locationStats, sprintf("%s/%s/gender-permutation/segregation-subcategories.csv",
											baseFolder, country), sep="\t", row.names=FALSE)

	}
}

##############
# Calculation
#############

calculateStats <- function(ci, region) {
	folderName <- sprintf("%s/%s/gender-permutation", baseFolder, region)
	generated <- runPermutate(ci, folderName, "permutate-gender", region, k=k, forceGenerate=F)
	# segregation() is crucial, the others need male and female popularity,
	ci <- segregation(ci, region, log=F)

	locationStats <- testObservationWithNullModel(ci, generated, folderName, region, k, PLOT_ANOM_DIST=T)
	categoryStats <- getBootstrappedStatistics(folderName, ci, generated, k, region)
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


#####################################
# Terminal for Turkey
#####################################
if(RUN_TURKEY) {
	country <- "Turkey"
	ci <- readAndFilterCheckIns("%s/Turkey.dat.gz", DATA_DIR, THRESH)
	ci <- filterSelectedCategories(ci)
	ci <- resampleIfTooMuchCheckIns(ci)
	allCheckIns <- fread(sprintf("%s %s/cleaned-check-ins-1000-15-countries-5-categories.csv.gz", ZCAT, baseFolder))
	allCheckIns <<- rbindlist(list(allCheckIns, ci))

	stats <- calculateStats(ci, country)

	categoryStats <- fread(sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder))
	categoryStats <<- rbindlist(list(categoryStats, stats$categoryStats))

	locationStats <- fread(sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder))
	locationStats <<- rbindlist( list(locationStats, stats$locationStats))



	save.image()
	write.table(locationStats, sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
	write.table(categoryStats, sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder),
				sep="\t", row.names=FALSE)
	write.table(allCheckIns, gzfile(sprintf("%s/cleaned-check-ins-1000-15-countries-5-categories.csv.gz"), baseFolder),
				sep="\t", row.names=FALSE)

}
