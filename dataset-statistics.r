allci <- fread("zcat < results/null-model-3/server-2016-11-13/cleaned-check-ins-15-countries-5-categories.csv.gz")

datasetStats <- allci

datasetStats[, list(
				nCI=length(idUserFoursquare),
				nFemaleCI=length(gender[gender=="female"]),
				nMaleCI=length(gender[gender=="male"]),
				percMaleCI=sprintf( "%s%%", round(100*length(gender[gender=="male"])/(length(gender[gender=="female"])+length(gender[gender=="male"]))) ),
				nLocations=length(unique(idLocal)), nFemaleU=length(unique(idUserFoursquare[gender=="female"])),
				nUsers=length(unique(idUserFoursquare)),
				nMaleUsers=length(unique(idUserFoursquare[gender=="male"])),
				percMaleUser=sprintf( "%s%%", round(100*length(unique(idUserFoursquare[gender=="male"])) / (
					 length(unique(idUserFoursquare[gender=="male"])) + length(unique(idUserFoursquare[gender=="female"])) )) )
		),
		by=country]

write.table(datasetStats, "dataset-stats.csv", sep="\t", row.names=F)
		sep="\t", row.names=FALSE)

message("total")
message("check-ins: ", nrow(allci))
message("users: ", length(unique(allci$idUserFoursquare)))
message(sprintf("total male users: %.0f%% ", 100*length(unique(allci[gender=="male"]$idUserFoursquare))/length(unique(allci$idUserFoursquare))))
message("venues: ", length(unique(allci$idLocal)))
message("countries: ", length(unique(allci$country)))

countryStat <- function(cntry) {
	message("country: ", cntry)
	country_ci <- allci[country==cntry]
	n_ci <- nrow(country_ci)
	message("check-ins: ", n_ci)
	message("male check-ins ", nrow(country_ci[gender=="male"]))
	message(sprintf("male check-ins %.0f%%", 100*nrow(country_ci[gender=="male"])/n_ci))
	message("venues: ", length(unique(country_ci$idLocal)))
	n_male_users <- length(unique(country_ci[gender=="male"]$idUserFoursquare))
	message("male users: ", n_male_users)
	message(sprintf("male users %.0f%%", 100*n_male_users/length(unique(country_ci$idUserFoursquare))))
	message("\n")
}


