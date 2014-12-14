library(data.table)
source('analysis/foursquare-analysis.r')

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

THRESH <- 100
MAX_CI <- 4e+5
countries <- dir("paises")


generateNullModel <- function() {
	k <- 100
	categoryStats <- list()
	for(i in seq(countries)) {
		country <- countries[i]
		f <- sprintf("paises/%s", countries[i])
		country <- strsplit(country, ".", fixed=T)[[1]][[1]] # remove .dat
		message(country)
		# cc <- list(integer=c("idUserFoursquare", "timeOffset"),
  #       			caracter=c("date","idLocal", "subcategory", "category", "country", "city", "district", "gender"))
		cc <- list(integer=c(1, 12), character=c(2, seq(5, 11)), numeric=c(3, 4))
		ci <- try(fread(f, header=F, sep="\t", stringsAsFactors=FALSE, colClasses=cc))
		if(length(ci)>2) {
			if(nrow(ci) < THRESH) {
				message("<", THRESH, "check-ins")
			} else {
				setnames(ci, 1:12, c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
	                      "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))

				ci <- ci[gender== "male" | gender=="female", ]
				ci<- oneCheckInForUserAndLocation(ci)
				ci <- checkInsInlocationsWithMinimumCheckIns(ci, n=5)
				n <- nrow(ci)
				if(n > 0) {
					if(n > MAX_CI) {
						message("Too many check-ins")
						ci <- ci[sample(n, 3e+5, replace=FALSE)]
						message("resampled check-ins")
					}
					stopifnot(nrow(ci) < MAX_CI)

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
	}
	oneTable <- rbindlist(categoryStats)
	print(oneTable)
	save.image()
	write.table(oneTable, "results/null-model/category-stats.csv", sep="\t", row.names=FALSE)
}

generateNullModel()
