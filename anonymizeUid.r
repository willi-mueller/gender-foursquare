# This script anonymizes data submitted for publishing (PlosOne 2016)
# and generates a dataset description
library(hash)

allci <- fread("results/null-model-4/cleaned-check-ins-1000-15-countries-5-categories.csv")

anonymizeUid <- function(ci) {
	ids <- unique(ci$idUserFoursquare)
	mapping <- hash(keys=sort(ids), values=1:length(ids))

	ci <- ci[, idUserFoursquare:=mapping[[ toString(idUserFoursquare) ]], by=idUserFoursquare]
}

removeColumns <- function(ci) {
	ci <- ci[, timeOffset:=NULL]
}

renameColumns <- function(ci) {
	setnames(ci, "idLocal", "idLocation")
}

allci <- removeColumns(allci)
allci <- renameColumns(allci)
allci <- anonymizeUid(allci)
#write.table(allCheckIns, gzfile("results/null-model-4/anonymized-check-ins-15-countries-5-categories.csv.gz"),
