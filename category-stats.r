#####################
# Analyses of categories for ranking
####################

library(parallel)
library(data.table)
source('analysis/foursquare-analysis.r')

baseFolder <- "results/null-model-3"
N_CORES <- detectCores()
MIN_CI <- 100
MAX_CI <- 30000 #Inf to disable filter for large data set
k <- 100
DATA_DIR <- "paises"
countryFiles <- dir(DATA_DIR) # "paises" for old data set
categoryStats <- data.frame() # global to save it in the workspace image
locationStats <- data.frame() # global to save it in the workspace image
allCheckIns <- data.frame()
ZCAT <- "zcat" # for OSX: `zcat < file.gz`, for linux: `zcat file.gz`

RUN_TURKEY = TRUE

collectStatisticsForRanking <- function(countries,
										allCheckIns=allCheckIns,
										categoryStats=categoryStats,
										locationStats=locationStats) {
	if(missing(countries)) {
		countries <- c("Germany", "Brazil", "France", "Spain", "United-Kingdom",
				 "United-States", "Brazil", "Mexico",
				 "United-Arab-Emirates", "Saudi-Arabia", "Kuwait", #"Turkey", # run Turkey manually in R shell
				 "South-Korea", "Malaysia", "Japan", "Thailand")
	}
	for(i in 1:length(countryFiles)) {
		f <- sprintf("%s/%s", DATA_DIR, countryFiles[i])
		country <- strsplit(countryFiles[i], ".dat", fixed=T)[[1]]
		if(country %in% countries) {
			start <- Sys.time()
			message(country)

			ci <- readAndFilterCheckIns(f, MIN_CI)
			ci <- filterSelectedCategories(ci)
			ci <- resampleIfTooMuchCheckIns(ci, country)
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
	write.table(allCheckIns, gzfile(sprintf("%s/cleaned-check-ins-15-countries-5-categories.csv.gz", baseFolder)),
				sep="\t", row.names=FALSE)
}

subcategorySegregationPlots <- function(allci) {
	for(cntry in c("Germany", "France", "Spain", "United-Kingdom",
				 	"United-States", "Brazil", "Mexico",
			 		"United-Arab-Emirates", "Saudi-Arabia", "Kuwait", "Turkey",
			 		"South-Korea", "Malaysia", "Japan", "Thailand")) {
		message(cntry)
		ci <- allci[country==cntry]
		message(nrow(ci), " check-ins are going to be analyzed")

		ci <- segregation(ci) # has nasty side effect of adding columns, don't know if this works
		pdf(sprintf("%s/%s/segregation-subcategories.pdf", baseFolder, cntry))
		segregationData <- segregationSubcategories(ci)
		#browser()
		segPlotData <- segregationData[, c("country", "subcategory", "meanMaleSubcPop", "meanFemaleSubcPop", "eucDistSubcPopIsAnomalous"), with=F]
		dev.off()
		write.table(segPlotData, sprintf("%s/%s/segregation-subcategories.csv",
											baseFolder, cntry), sep="\t", row.names=FALSE)
	}
}

##############
# Calculation
#############

calculateStats <- function(ci, region) {
	folderName <- sprintf("%s/%s/bootstrap", baseFolder, region)
	generated <- runPermutate(ci, folderName, "bootstrap", region, k=k, forceGenerate=T)
	# segregation() is crucial, the others need male and female popularity
	ci <- segregation(ci, region, log=F)

	locationStats <- testObservationWithNullModel(ci, generated, folderName, region, k, PLOT_ANOM_DIST=T)
	categoryStats <- getBootstrappedStatistics(folderName, ci, generated, k, region)
	return(list(categoryStats=categoryStats$bootstrapStats, locationStats=locationStats))
}

resampleIfTooMuchCheckIns <- function(ci, country) {
	n <- nrow(ci)
	if(n > MAX_CI) {
		message(sprintf("Too many check-ins in %s (%s)", country, n))
		countrySample <- sprintf("%s/%s_sample.dat", DATA_DIR, country)
		# for repeatability keep the sample
		if( !file.exists(countrySample)) {
			ci <- ci[sample(n, MAX_CI, replace=FALSE)]
			message("resampled check-ins to ", nrow(ci))
			stopifnot(nrow(ci) <= MAX_CI)
			write.table(ci, countrySample, sep="\t", row.names=F)
		} else {
			message("Read existing sample of ", country)
			ci <- readAndFilterCheckIns(countrySample, MIN_CI)
		}

	}
	return(ci)
}

##############
# Run
#############

collectStatisticsForRanking() # all countries

allCheckIns <- fread(sprintf("%s %s/cleaned-check-ins-15-countries-5-categories.csv.gz", ZCAT, baseFolder))
categoryStats <- fread(sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder))
locationStats <- fread(sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder))

if(RUN_TURKEY) {
	collectStatisticsForRanking(c("Turkey"), allCheckIns, categoryStats, locationStats)
}
allci <- fread(sprintf("%s %s/cleaned-check-ins-15-countries-5-categories.csv.gz", ZCAT, baseFolder))

subcategorySegregationPlots(allci)
