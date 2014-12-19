source('analysis/foursquare-analysis.r')
source("analysis/category-stats.r")
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

countryFileNames <- c("Brazil", "United-States", "Indonesia", "Turkey", "Japan", "Saudi-Arabia", "Russia")
allci <- rbindlist( mclapply(countryFileNames, function(x) {
	readCheckIns(sprintf("paises/%s.dat", x))
	}, mc.cores=N_CORES))
countries <- c("Brazil", "United States", "Indonesia", "Turkey", "Japan", "Saudi Arabia", "Russia")

stopifnot( all(allci[, list(n=.N), by=idLocal]$n >= 5) )

selectedCI <- allci[country %in% countries]
subc <- Reduce(intersect, selectedCI[, .(list(unique(subcategory))), country]$V1)
selectedCI <- selectedCI[subcategory %in% subc]

nCheckInsStats <- selectedCI[, list(n=length(idUserFoursquare)), by=list(country, idLocal, subcategory)][order(country, subcategory)]
write.table(nCheckInsStats, "results/n-checkIns-for-selected-subc-locations.csv", row.names=F, sep="\t")

nLocationsStats <- selectedCI[, list(nLocations=length(unique(idLocal))),by=list(subcategory, country)][order(subcategory, country)]
write.table(nLocationsStats, "results/n-locations-for-selected-subc.csv", row.names=F, sep="\t")

chosenSubcStats <- nLocationsStats[, .SD[all(nLocations>20)],by=subcategory]
chosenSubc <- unique(chosenSubcStats$subcategory)

chosenSubcCI <- selectedCI[subcategory %in% chosenSubc]
nrow(chosenSubcCI) #== 187851

TOP_N <- 5
k <- 100

topCI <- rbindlist( lapply(countries, function(countryStr) {
												countryCI <- chosenSubcCI[country==countryStr]
												pickCIinTopLocations(countryCI,  TOP_N)
											}) )
nrow(topCI) # = 14316

topCI[, .N,by=country]
#          country    N
# 1:        Brazil 1450
# 2: United States  251
# 3:     Indonesia  834
# 4:        Turkey 9801
# 5:         Japan  448
# 6:  Saudi Arabia  863
# 7:        Russia  669

mainFolder <- "results/null-model/selected-subcategories-and-countries"

stats <- rbindlist( lapply( countries, function(countryStr){
	folder <- sprintf("%s/%s", mainFolder, countryStr)
	dir.create(folder)
	countryCI <- topCI[country==countryStr]
	seg <- segregation(countryCI, countryStr, log=T)
	gen <- 	runPermutate(countryCI, folder,
				 sprintf("%s-selected-permutated", countryStr),
				 countryStr,
				 k=k, forceGenerate=T)

	return( testObservationWithNullModel(seg, gen, folder , countryStr, k=k, PLOT_ANOM_DIST=T ) )
} ) )

write.table(stats, sprintf("%s/selected-stats.csv",mainFolder))


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

######## TODO
# Pub == Bar?
# Cafeteria == Café?









