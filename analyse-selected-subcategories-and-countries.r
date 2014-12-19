source('analysis/foursquare-analysis.r')

# subcstats <- catstats[, list(subc=unique(subcategory)), by=country]
# allSubc <- list()
# for(i in seq(length(unique(subcstats$country)))) {
# 	allSubc[[i]] <- subcstats[country==unique(subcstats$country)[i]]$subc
# }
# Reduce(intersect, allSubc[1:3])

# for(i in seq(length(unique(allSubcStats$country)))) {
# 	allSubcList[[i]] <- foo[country==unique(allSubcStats$country)[i]]$subcategory
# }


# allci <- fread("results/cleaned-check-ins-1000.csv")


pickCIinTopLocations <- function(ci, topN) {
	topCI <- data.table()
	subc <- unique(ci$subcategory)
	for(s in subc) {
		x <- ci[subcategory==s]
		locations <- x[, list(n=.N), by=idLocal][order(-n)][1:topN]$idLocal
		topCI <- rbindlist( list(topCI, x[idLocal %in% locations]) )
	}
	return(topCI)
}

AXES = c(0, 0.5)
MAIN_FOLDER <- "results/null-model/selected-subcategories-and-countries/difference"
TOP_N <- c(5, 10)
k <- c(1000, 2000)

run <- function(allci, TOP_N, k, AXES, MAIN_FOLDER) {

	countries <- c("Brazil", "United States", "Indonesia", "Turkey", "Singapore", "Japan", "Saudi Arabia", "Russia")

	stopifnot( all(allci[, list(n=.N), by=idLocal]$n >= 5) )

	selectedCI <- allci[country %in% countries]
	subc <- Reduce(intersect, selectedCI[, .(list(unique(subcategory))), country]$V1)
	selectedCI <- selectedCI[subcategory %in% subc]

	nCheckInsStats <- selectedCI[, list(n=length(idUserFoursquare)), by=list(country, idLocal, subcategory)][order(subcategory, country)]
	write.table(nCheckInsStats, sprintf("%s/n-checkIns-for-selected-subc-locations-top-%s.csv", MAIN_FOLDER, TOP_N), row.names=F, sep="\t")

	nLocationsStats <- selectedCI[, list(nLocations=length(unique(idLocal))),by=list(subcategory, country)][order(subcategory, country)]
	write.table(nLocationsStats, sprintf("%s/n-locations-for-selected-subc-top%s.csv", MAIN_FOLDER, TOP_N), row.names=F, sep="\t")

	# chosenSubcStats <- nLocationsStats[, .SD[all(nLocations>20)],by=subcategory]
	# chosenSubc <- unique(chosenSubcStats$subcategory)
	chosenSubc <- c("Mall", "University", "Café")
	message("Chose subcategories: ", chosenSubc)

	chosenSubcCI <- selectedCI[subcategory %in% chosenSubc]
	message("Analysing ", nrow(chosenSubcCI), " check-ins")


	topCI <- lapply(countries, function(countryStr) {
									countryCI <- chosenSubcCI[country==countryStr]
									pickCIinTopLocations(countryCI,  TOP_N)
								})
	topCI <- rbindlist(topCI)

	nGenderCheckIns <- topCI[,list(nCheckIns=.N), by=list(country, gender)]
	write.table(nGenderCheckIns, sprintf("%s/gender-check-ins-for-selected-subcategories-and-countries-top-%s.csv", MAIN_FOLDER, TOP_N))
	message("In top locations ", nrow(topCI), " check-ins")
	print(nGenderCheckIns)

	stats <- rbindlist( lapply( countries, function(countryStr){
		folder <- sprintf("%s/%s", MAIN_FOLDER, countryStr)
		dir.create(folder)
		countryCI <- topCI[country==countryStr]
		seg <- segregation(countryCI, countryStr, log=T, axeslim=AXES)
		gen <- 	runPermutate(countryCI, folder,
							 sprintf("%s-selected-permutated-top", countryStr),
							 countryStr,
							 k=k, forceGenerate=T)
		permutationStats <- testObservationWithNullModel(seg, gen, folder , countryStr, k=k,
														 PLOT_ANOM_DIST=T, axeslim=AXES)
		return( permutationStats )
	}) )

	write.table(stats, sprintf("%s/selected-locations-stats-top-%s.csv",MAIN_FOLDER, TOP_N), sep="\t")

	anomalyRanking <- stats[, list( nAnomalous= length(isAnomalous[isAnomalous==T]),
									percAnomalous= length(isAnomalous[isAnomalous==T]) / length(unique(idLocal)) ),
	 						by=list(country)][order(percAnomalous)]
	write.table(anomalyRanking, sprintf("%s/ranking-select-top-%s-locations.csv", MAIN_FOLDER, TOP_N), sep="\t")
	print(anomalyRanking)
}

countryFileNames <- c("Brazil", "United-States", "Indonesia", "Turkey", "Japan", "Singapore", "Saudi-Arabia", "Russia")

allci <- rbindlist( mclapply(countryFileNames, function(x) {
	readAndFilterCheckIns(sprintf("paises/%s.dat", x))
	}, mc.cores=N_CORES))

for(n in c(10, 7, 5)) {
	for(k in c(2000) {
		run(allci, n, k, AXES, sprintf("%s/n%s-k%s", MAIN_FOLDER, n, k))
	}
}
######## TODO
# Pub == Bar?
# Cafeteria == Café?
