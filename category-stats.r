library(data.table)
source('foursquare-analysis.r')

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

countries <- dir("paises")

generateNullModel <- function() {
	k <- 100
	categoryStats <- list()
	for(i in seq(countries)) {
		country <- countries[i]
		f <- sprintf("paises/%s", countries[i])
		ci <- try(fread(f, header=F, sep="\t", stringsAsFactors=FALSE))
		country <- strsplit(country, ".", fixed=T)[[1]][[1]] # remove .dat
		message(country)
		if(length(ci)>2) {
			setnames(ci, 1:12, c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                      "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))

			ci <- ci[gender== "male" | gender=="female", ]
			ci<- oneCheckInForUserAndLocation(ci)
			ci <- checkInsInlocationsWithMinimumCheckIns(ci, n=5)
			if(nrow(ci) > 0) {
				gen.segregation <- runPermutate(ci, sprintf("results/null-model/%s/gender-permutation", country),
	                                  "permutate-gender", country, k=k)
				ci.segregation <- segregation(ci, country, log=F)

				folderName <- sprintf("results/null-model/%s/gender-permutation", country)
				stats <- testObservationWithNullModelForCategories(ci.segregation,
																			gen.segregation, folderName, country,
	                                                    					k, quote(category))
				print(stats)
				categoryStats[[i]] <- stats
			}
		}
	}
	write.table(categoryStats, "results/null-modell/category-stats.csv", sep="\t", row.names=FALSE)
}

generateNullModel()