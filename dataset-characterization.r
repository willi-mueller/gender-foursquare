library(Hmisc) # Ecdf
library(data.table)

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

THRESH=100

countries <- dir("paises")
n <- length(countries)
maleCI <- vector(mode="double", length=n)
femaleCI <- vector(mode="double", length=n)
nCheckIns <-  vector(mode="double", length=n)

allCheckIns <- list()
filteredMaleCI <- vector(mode="double", length=n)
filteredFemaleCI <- vector(mode="double", length=n)
filteredNCheckIns <-  vector(mode="double", length=n)

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

countriesWithoutCheckIns <- 0
for(i in seq(n)) {
	message(countries[i])
	f <- sprintf("paises/%s", countries[i])
	if(file.exists(f)){
		ci <- try(fread(f, header=F, sep="\t", stringsAsFactors=FALSE))

		if(length(ci)>2) {
			setnames(ci, 1:12,c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
                      "subcategory", "category", "country", "city", "district", "gender", "timeOffset"))
			allCheckIns[[i]] <- ci
			maleCI[i] <- nrow(ci[ci$gender=="male", ]) # [, length(idUserFoursquare[gender=='male'])]
			femaleCI[i] <- nrow(ci[ci$gender=="female", ])
			nCheckIns[i] <- nrow(ci)

			onlyLocationsWithSufficientCheckIns <- checkInsInlocationsWithMinimumCheckIns(ci, n=5)
			filteredMaleCI[i] <- nrow(onlyLocationsWithSufficientCheckIns[gender=='male'])
			filteredFemaleCI[i] <- nrow(onlyLocationsWithSufficientCheckIns[gender=='female'])
			filteredNCheckIns[i] <- nrow(onlyLocationsWithSufficientCheckIns)
		}
		else{
			countriesWithoutCheckIns <- countriesWithoutCheckIns +1
		}
	}
}

otherCI <- nCheckIns-maleCI-femaleCI
filteredOtherCI <- filteredNCheckIns-filteredMaleCI-filteredFemaleCI

filter <- nCheckIns>THRESH
nFilter <- length(nCheckIns[filter])
nFilter2 <- length(filteredNCheckIns[filteredNCheckIns>0])

png("results/characterize-dataset/gender-proportion-boxplot.png")
boxplot(maleCI/nCheckIns, femaleCI/nCheckIns, otherCI/nCheckIns,
		names=c("male", "female", "other")
		#,main="Proportion of genders of check-ins in across all 187 countries"
		)
dev.off()

png("results/characterize-dataset/gender-proportion-filter2-boxplot.png")
boxplot(filteredMaleCI/filteredNCheckIns, filteredFemaleCI/filteredNCheckIns, filteredOtherCI/filteredNCheckIns,
		names=c("male", "female", "other")
		#,main=sprintf("Proportion of genders of check-ins in locations with > 5 check-ins across %s countries",nFilter2)
		)
dev.off()

png("results/characterize-dataset/gender-proportion-filtered-boxplot.png")
boxplot(maleCI[filter]/nCheckIns[filter],
		femaleCI[filter]/nCheckIns[filter],
		(otherCI[filter])/nCheckIns[filter],
		names=c("male", "female", "other")
		#,main=sprintf("Proportion of genders of check-ins across 175 countries with >%s check-ins", THRESH)
		)
dev.off()

png("results/characterize-dataset/gender-proportion-ecdf.png")
g <- c( rep("male", n),
		rep("female", n),
        rep("other", n))
Ecdf(c(maleCI/nCheckIns, femaleCI/nCheckIns, otherCI/nCheckIns), group=g,
       col=c('blue', 'orange', "black"),
       xlim=c(0, 1.0),
       xlab="Gender proportions"
       #,main="ECDF Gender Proportions of check-ins across all 187 countries"
       )
dev.off()

png("results/characterize-dataset/gender-proportion-filter2-ecdf.png")
g <- c( rep("male", n),
		rep("female", n),
        rep("other", n))
Ecdf(c(filteredMaleCI/filteredNCheckIns, filteredFemaleCI/filteredNCheckIns, filteredOtherCI/filteredNCheckIns), group=g,
       col=c('blue', 'orange', "black"),
       xlim=c(0, 1.0),
       xlab="Gender proportions"
       #,main=sprintf("ECDF Gender Proportions of check-ins in locations with >5 check-ins across %s countries",n)
       )
dev.off()

png("results/characterize-dataset/gender-proportion-filtered-ecdf.png")
g <- c( rep("male", nFilter),
		rep("female", nFilter),
        rep("other", nFilter))
Ecdf(c(maleCI[filter]/nCheckIns[filter], femaleCI[filter]/nCheckIns[filter], otherCI[filter]/nCheckIns[filter]), group=g,
       col=c('blue', 'orange', "black"),
       xlim=c(0, 1.0),
       xlab="Gender proportions"
       #,main=sprintf("ECDF Gender Proportions of check-ins across 120 countries with >%s check-ins", THRESH)
       )
dev.off()
