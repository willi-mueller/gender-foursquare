library(Hmisc) # Ecdf
library(data.table)

checkInsInlocationsWithMinimumCheckIns <- function(checkIns, n=5) {
  locations <- checkIns[, list(hasMore=length(unique(idUserFoursquare))>=n), by=idLocal][hasMore==TRUE]$idLocal
  return(checkIns[idLocal %in% locations, ])
}

THRESH=100

countries <- dir("paises")
n <- length(countries)
maleCI <- vector(mode="double", length=n)
femaleCI <- vector(mode="double", length=n)
nCheckIns <-  vector(mode="double", length=n)

allCheckIns <- list()
# filter: removing locations with less than 5 check-ins
filteredMaleCI <- vector(mode="double", length=n)
filteredFemaleCI <- vector(mode="double", length=n)
filteredNCheckIns <-  vector(mode="double", length=n)

countriesWithoutCheckIns <- 0
for(i in seq(n)) {
	message(countries[i])
	f <- paste("paises", countries[i], sep="/")
	if(file.exists(f)){
		ci <- try(read.csv(f, sep="\t"))
		if(length(ci)>2) {
			colnames(ci) <- c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
			                 "subcategory", "category", "country", "city", "district", "gender", "timeOffset")
			ci <- as.data.table(ci)
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





country <- c(france, turkey, brazil, uae)
countryName <- c("France", "Turkey", "Brazil", "UAE")
for(i in seq(length(countryName))) {
	sc <- subcategoryPreferencesByGender(country[i])
	correlateCategories(sc$maleSubcategories$count,
					sc$femaleSubcategories$count,
                     sc$maleSubcategories$subcategory,
                     file=paste("results/characterize-dataset/2014-11-27-subcategories", countryName[i], sep="-"),
                     country=countryName[i],
                     countMethod="unique users",
                     categories="Subcategories",
                     xlim=0.1, ylim=0.1)
}
