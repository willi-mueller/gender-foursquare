library(Hmisc) # Ecdf

countries <- dir("paises")
n <- length(countries)
maleCI <- vector(mode="double", length=n)
femaleCI <- vector(mode="double", length=n)
nCheckIns <-  vector(mode="double", length=n)

countriesWithoutCheckIns <- 0
for(i in seq(n)) {
	message(countries[i])
	f <- paste("paises", countries[i], sep="/")
	if(file.exists(f)){
		ci <- try(read.csv(f, sep="\t"))
		if(length(ci)>2) {
			colnames(ci) <- c("idUserFoursquare", "date", "latitude", "longitude", "idLocal",
			                 "subcategory", "category", "country", "city", "district", "gender", "timeOffset")
			maleCI[i] <- nrow(ci[ci$gender=="male", ])
			femaleCI[i] <- nrow(ci[ci$gender=="female", ])
			nCheckIns[i] <- nrow(ci)
		}
		else{
			countriesWithoutCheckIns <- countriesWithoutCheckIns +1
		}
	}
}

otherCI <- nCheckIns-maleCI-femaleCI

filter <- nCheckIns>100
nFilter <- length(nCheckIns[filter])

png("results/gender-proportion-boxplot.png")
boxplot(maleCI/nCheckIns, femaleCI/nCheckIns, otherCI/nCheckIns,
		names=c("male", "female", "other"),
		main="Proportion of genders of check-ins in across all 187 countries")
dev.off()

png("results/gender-proportion-filtered-boxplot.png")
boxplot(maleCI[filter]/nCheckIns[filter],
		femaleCI[filter]/nCheckIns[filter],
		(otherCI[filter])/nCheckIns[filter],
		names=c("male", "female", "other"),
		main="Proportion of genders of check-ins across 175 countries with >100 check-ins")
dev.off()

png("results/gender-proportion-ecdf.png")
g <- c( rep("male", n),
		rep("female", n),
        rep("other", n))
Ecdf(c(maleCI/nCheckIns, femaleCI/nCheckIns, otherCI/nCheckIns), group=g,
       col=c('blue', 'orange', "black"),
       xlim=c(0, 1.0),
       xlab="Gender proportions",
       main="ECDF Gender Proportions of check-ins across all 187 countries")
dev.off()

png("results/gender-proportion-filtered-ecdf.png")
g <- c( rep("male", nFilter),
		rep("female", nFilter),
        rep("other", nFilter))
Ecdf(c(maleCI[filter]/nCheckIns[filter], femaleCI[filter]/nCheckIns[filter], otherCI[filter]/nCheckIns[filter]), group=g,
       col=c('blue', 'orange', "black"),
       xlim=c(0, 1.0),
       xlab="Gender proportions",
       main="ECDF Gender Proportions of check-ins across 175 countries with >100 check-ins")
dev.off()





country <- c(france, turkey, brazil, uae)
countryName <- c("France", "Turkey", "Brazil", "UAE")
for(i in seq(length(countryName))) {
	sc <- subcategoryPreferencesByGender(country[i])
	correlateCategories(sc$maleSubcategories$count,
					sc$femaleSubcategories$count,
                     sc$maleSubcategories$subcategory,
                     file=paste("results/2014-11-27-subcategories", countryName[i], sep="-"),
                     country=countryName[i],
                     countMethod="unique users",
                     categories="Subcategories",
                     xlim=0.1, ylim=0.1)
}
