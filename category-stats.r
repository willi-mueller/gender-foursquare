#####################
# Analyses of categories for ranking
####################

library(parallel)
library(data.table)
source('analysis/foursquare-analysis.r')

baseFolder <- "results/null-model-3"
N_CORES <- detectCores()
MIN_CI <- 1000
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
	for(cntry in c("Germany", "France", "Spain", "United Kingdom",
				 	"United States", "Brazil", "Mexico",
			 		"United Arab Emirates", "Saudi Arabia", "Kuwait", "Turkey",
			 		"South Korea", "Malaysia", "Japan", "Thailand")) {
		message(cntry)
		ci <- allci[country==cntry]
		message(nrow(ci), " check-ins are going to be analyzed")

		ci <- segregation(ci) # has nasty side effect of adding columns, don't know if this works
		cntryFolder <- gsub(" ", "-", cntry)
		pdf(sprintf("%s/%s/segregation-subcategories.pdf", baseFolder, cntryFolder))
		segregationData <- segregationSubcategories(ci)
		segPlotData <- segregationData[, c("subcategory", "maleSum", "femaleSum", "diff"), with=F]
		dev.off()
		write.table(segPlotData, sprintf("%s/%s/segregation-subcategories.csv",
											baseFolder, cntryFolder), sep="\t", row.names=FALSE)
	}
}

##############
# Calculation
#############

calculateStats <- function(ci, region) {
	folderName <- sprintf("%s/%s/bootstrap", baseFolder, region)
	generated <- generateNullModel(ci, folderName, "bootstrap", region, k=k, forceGenerate=T)
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
		countrySample <- sprintf("%s/%s_sample.csv.gz", DATA_DIR, country)
		# for repeatability keep the sample
		if( !file.exists(countrySample)) {
			message("Please locate a subsample at the path: ", countrySample)
			# turkey: ci <- turk[turk$idLocal %in% c(sample(unique(turk$idLocal), 1499), "4b69625ef964a520a9a02be3"), ]
			# malaysia: ci <- malaysia[malaysia$idLocal %in% sample(unique(malaysia$idLocal), 2700), ]

			# message("resampled check-ins to ", nrow(ci))
			# stopifnot(nrow(ci) <= MAX_CI)
			# write.table(ci, countrySample, sep="\t", row.names=F)
		} else {
			message(sprintf("Read existing sample of %s with %s check-ins", country, nrow(ci)))
			ci <- fread(sprintf("%s %s", ZCAT, countrySample))	# read directly because readAndFilterCheckIns() assumes different data format
		}

	}
	return(ci)
}

runResampledCountry <- function(cntry, allCheckIns, categoryStats, locationStats) {
	allCheckIns   <- allCheckIns  [country != cntry]
	categoryStats <- categoryStats[country != cntry]
	locationStats <- locationStats[country != cntry]

	start <- Sys.time()
	ci <- fread(sprintf("%s %s/%s_sample.csv.gz", ZCAT, DATA_DIR, cntry))
	message(sprintf("Read %s check-ins of %s", nrow(ci), cntry))

	allCheckIns <<- rbindlist(list(allCheckIns, ci))

	stats <- calculateStats(ci, cntry)

	categoryStats <<- rbindlist( list(categoryStats, stats$categoryStats))
	locationStats <<- rbindlist( list(locationStats, stats$locationStats))
	message("#### Analyses took: ", Sys.time()-start)
	write.table(locationStats, sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder),
		sep="\t", row.names=FALSE)
	write.table(categoryStats, sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder),
		sep="\t", row.names=FALSE)
	write.table(allCheckIns, gzfile(sprintf("%s/cleaned-check-ins-15-countries-5-categories.csv.gz", baseFolder)),
		sep="\t", row.names=FALSE)
	warnings()
}

##############
# Run
#############

#collectStatisticsForRanking() # all countries

allCheckIns <- fread(sprintf("%s %s/cleaned-check-ins-15-countries-5-categories.csv.gz", ZCAT, baseFolder))
categoryStats <- fread(sprintf("%s/category-stats-15-countries-5-categories.csv", baseFolder))
locationStats <- fread(sprintf("%s/location-stats-15-countries-5-categories.csv", baseFolder))

#collectStatisticsForRanking(c("Turkey", "Malaysia"), allCheckIns, categoryStats, locationStats)

runResampledCountry("Turkey", allCheckIns, categoryStats, locationStats)
runResampledCountry("Malaysia", allCheckIns, categoryStats, locationStats)

#subcategorySegregationPlots(allci)

warnings()

