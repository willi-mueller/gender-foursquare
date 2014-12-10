##################
# Run
##################
country <- "UAE"
checkInsInCity <- getCheckInsInRegion(c("Abu Dhabi"), uaeCheckIns, uaeUsers, uaeFilter)
segregation <- segregation(checkInsInCity, "Abu Dhabi")


# correlation categories
uae.ci <- getCheckInsInCountry("paises/United-Arab-Emirates.dat", substitutionRules)
data <- subcategoryPreferencesByGender(uae.ci)
country <- "Saudi Arabia"
saudi.ci <- getCheckInsInCountry("paises/Saudi-Arabia.dat", substitutionRules)
data <- subcategoryPreferencesByGender(saudi.ci)

correlateCategories(data$maleCategories$count, data$femaleCategories$count,
                    data$maleCategories$category,
                    countMethod="check-ins",
                    country=country,
                    xlim=0.3, ylim=0.3)

correlateCategories(data$maleUniqueCategories$count, data$femaleUniqueCategories$count,
                    data$maleUniqueCategories$category,
                    country=country,
                    countMethod="unique users",
                    xlim=0.3, ylim=0.3)

correlateCategories(data$maleSubcategories$count, data$femaleSubcategories$count,
                    data$maleSubcategories$subcategory,
                    country=country,
                    countMethod="check-ins",
                    categories="Subcategories",
                    xlim=0.1, ylim=0.1)

correlateCategories(data$maleSubcategories$count, data$femaleSubcategories$count,
                    data$maleSubcategories$subcategory,
                    country=country,
                    countMethod="unique users",
                    categories="Subcategories",
                    xlim=0.1, ylim=0.1)
topN <- 10
correlateTopCategories(data$maleUniqueSubcategories, data$femaleUniqueSubcategories,
                       country,
                       countMethod="unique users",
                       sprintf("%s most popular subcategories", topN),
                       "most popular",
                       topN)

correlateTopCategories(data$maleUniqueSubcategories, data$femaleUniqueSubcategories,
                       country,
                       "unique users",
                       sprintf("%s most different subcategories", topN),
                       "most different",
                       topN)

############
# Correlation Locations & Segregation
############

ad.checkIns <- getCheckInsInRegion(c("Abu Dhabi"), uaeCheckIns, uaeUsers, uaeFilter, substitutionRules)
ad.segregation <- segregation(ad.checkIns, "Abu Dhabi")
ad.dists <- distances(ad.segregation$maleCIR, ad.segregation$femaleCIR)

r.checkIns <- getCheckInsInRegion(c("Riyadh"), saudiCheckIns, saudiUsers, saudiFilter, substitutionRules)
r.segregation <- segregation(r.checkIns, "Riyadh")
r.dists <- distances(r.segregation$maleCIR, r.segregation$femaleCIR)

seg <- segregation(getCheckInsInRegion(c("Paris"), franceCheckIns, franceUsers, franceFilter, substitutionRules), "Paris")
dists <- distances(seg$maleCIR, seg$femaleCIR)
####
# for copy'n paste into console
####
dists <- ad.dists
city <- "Abu Dhabi"

dists <- r.dists
city <- "Riyadh"

plotDensityOfDistanceInSubcategory(dists, "University")
plotDensityOfDistanceInSubcategory(dists, "Mall")
plotDensityOfDistanceInSubcategory(dists, "Café")

plotProbabilityDensityOfDistanceInSubcategory(dists, "Café")

r.cafe <- r.dists[r.dists$subcategory=="Café"][[1]]
ad.cafe <- ad.dists[ad.dists$subcategory=="Café"][[1]]

compareDistanceSegregationsECDFin(r.cafe, ad.cafe, "Riyadh", "Abu Dhabi", "Café")

############

countries <- list(
  list(checkIns=franceCheckIns, users=franceUsers, filter=franceFilter, name="France")
  ,list(checkIns=germanyCheckIns, users=germanyUsers, filter=germanyFilter, name="Germany")
  ,list(checkIns=swedenCheckIns, users=swedenUsers, filter=swedenFilter, name="Sweden")
  ,list(checkIns=saudiCheckIns, users=saudiUsers, filter=saudiFilter, name="Saudi Arabia")
  ,list(checkIns=uaeCheckIns, users=uaeUsers, filter=uaeFilter, name="UAE")
  )

genderDistanceForCountry(countries, substitutionRules, "Distribution of gender distance")

#########################################
# Null Model Generation
#########################################

r.checkIns <- getCheckInsInRegion(c("Riyadh"), saudiCheckIns, saudiUsers, saudiFilter, substitutionRules)
r.checkIns <- rbind(r.checkIns[r.checkIns$gender=="male", ], r.checkIns[r.checkIns$gender=="female", ])
r.segregation <- segregation(r.checkIns, "Riyadh")

k<-10
fileNames <- c()
folderPrefix <- "results/null-model"
folderNames <- c(sprintf("%s/uniform-location-uniform-gender", folderPrefix),
                 sprintf("%s/observed-location-uniform-gender", folderPrefix),
                 sprintf("%s/uniform-location-observed-gender", folderPrefix),
                 sprintf("%s/observed-location-observed-gender", folderPrefix))
plotNames <- c("uniform-location-uniform-gender", "observed-location-uniform-gender",
               "uniform-location-observed-gender", "observed-location-observed-gender")

uniformLocationProbability <- c(TRUE, FALSE, TRUE, FALSE)
uniformGenderProbability <- c(TRUE, TRUE, FALSE, FALSE)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[1],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[1],
            folderName=folderNames[1],
            plotName=plotNames[1], k=k)
fileNames <- rbind(fileNames, f)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[2],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[2],
            folderName=folderNames[2],
            plotName=plotNames[2], k=k)
fileNames <- rbind(fileNames, f)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[3],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[3],
            folderName=folderNames[3],
            plotName=plotNames[3], k=k)
fileNames <- rbind(fileNames, f)

f <- runGenerate(r.checkIns, r.segregation,
            UNIFORM_LOCATION_PROBABILITY=uniformLocationProbability[4],
            UNIFORM_GENDER_PROBABILITY=uniformGenderProbability[4],
            folderName=folderNames[4],
            plotName=plotNames[4], k=k)
fileNames <- rbind(fileNames, f)

print(fileNames)

mapply(readGeneratedDataAndPlot, fileNames[,2 ], fileNames[,3 ],
         folderNames, rep("NY", nrow(fileNames), uniformLocationProbability, uniformGenderProbability)


########################
# Run Permutation Test
########################
k <- 100
ny <- usa[usa$city=="New York City",]
ny <- checkInsInlocationsWithMinimumCheckIns(ny, n=5)
ny.segregation <- segregation(ny, "New York City")
gen.segregation <- runPermutate(ny, ny.segregation, "results/null-model/ny/gender-permutation", "permutate-gender", "New York City", k=k)
testObservationWithNullModel(ny.segregation, gen.segregation,
                             "results/null-model/ny/gender-permutation", "New York City", k=k,
                             F, F, SEARCH_ANOMALOUS_LOCATIONS=T)

########################
# Plots correlation
########################

usa <- getCheckInsInCountry("paises/United-States.dat", substitutionRules=substitutionRules)
sa <- getCheckInsInCountry("paises/Saudi-Arabia.dat", substitutionRules=substitutionRules)
uae <- getCheckInsInCountry("paises/United-Arab-Emirates.dat", substitutionRules=substitutionRules)
japan <- getCheckInsInCountry("paises/Japan.dat", substitutionRules=substitutionRules)
brazil <- getCheckInsInCountry("paises/Brazil.dat", substitutionRules=substitutionRules)
france <- getCheckInsInCountry("paises/France.dat", substitutionRules=substitutionRules)
# china <- getCheckInsInCountry("paises/China.dat", substitutionRules=substitutionRules)
# saf <- getCheckInsInCountry("paises/South-Africa.dat", substitutionRules=substitutionRules)
# ger <- getCheckInsInCountry("paises/Germany.dat", substitutionRules=substitutionRules)
indonesia <- getCheckInsInCountry("paises/Indonesia.dat", substitutionRules=substitutionRules)

usa <- checkInsInlocationsWithMinimumCheckIns(usa, 5)
sa <- checkInsInlocationsWithMinimumCheckIns(sa, 5)
uae <- checkInsInlocationsWithMinimumCheckIns(uae, 5)
japan <- checkInsInlocationsWithMinimumCheckIns(japan, 5)
brazil <- checkInsInlocationsWithMinimumCheckIns(brazil, 5)
france <- checkInsInlocationsWithMinimumCheckIns(france, 5)
indonesia <- checkInsInlocationsWithMinimumCheckIns(indonesia, 5)

ny <- usa[city=="New York City",] # 6647
riyadh <- sa[city=="Riyadh",] # 6278
ad <- uae[city=="Abu Dhabi",] # 667
tokyo <- japan[city=="Tokyo",] # 32834
sp <- brazil[city=="Sao Paulo", ] # 12569
paris <- france[city=="Paris",] # 1767
jakarta <- indonesia[city=="Jakarta"] # 6245

beijing <- china[city=="Beijing",] # 474 checkins
jb <- saf[city=="Johannesburg",] #294 checkins
berlin <- ger[city=="Berlin",] # 183 checkins

usa.segregation <- segregation(usa, "the USA")
sa.segregation <- segregation(sa, "Saudi Arabia")
uae.segregation <- segregation(uae, "United Arab Emirates")
japan.segregation <- segregation(japan, "Japan")
brazil.segregation <- segregation(brazil, "Brazil")
france.segregation <- segregation(france, "France")
indonesia.segregation <- segregation(indonesia, "Indonesia")

ny.segregation <- segregation(ny, "New York City")
sp.segregation <- segregation(sp, "São Paulo")
riyadh.segregation <- segregation(riyadh, "Riyadh")
ad.segregation <- segregation(ad, "Abu Dhabi")
tokyo.segregation <- segregation(tokyo, "Tokyo")
paris.segregation <- segregation(paris, "Paris")
jakarta.segregation <- segregation(jakarta, "Jakarta")

# china.segregation <- segregation(china, "China")
# saf.segregation <- segregation(china, "South Africa")

countries <- ("USA", "UAE", "Saudi Arabia", "Japan", "Brazil", "France", "Indonesia")
countryVars <- list(usa, uae, sa, japan, brazil, france, indonesia)
for(i in 1:length(countries)) {
  axesOld <- SEGREGATION_AXES
  data <- subcategoryPreferencesByGender(countryVars[[i]])
  pdf(sprintf("results/segregation-subcategory-country/%s.pdf", countries[i]))
  SEGREGATION_AXES <- c(0,0.15)
  correlateCategories(data$maleUniqueSubcategories$count, data$femaleUniqueSubcategories$count,
                    data$maleUniqueSubcategories$subcategory,
                    country=countries[i],
                    countMethod="unique users",
                    categories="Subcategories",
                    axeslim=c(0, 0.15))
  dev.off()
  SEGREGATION_AXES <- c(0, 0.3)
  pdf(sprintf("results/segregation-category-country/%s.pdf", countries[i]))
  correlateCategories(data$maleUniqueCategories$count, data$femaleUniqueCategories$count,
                    data$maleUniqueCategories$category,
                    country=countries[i],
                    countMethod="unique users",
                    categories="Categories",
                    axeslim=c(0, 0.3))
  dev.off()
  SEGREGATION_AXES <- axesOld
}


###################
# Permutation Test
###################

########### Cities ###############
ny.gen.segregation <- runPermutate(ny, ny.segregation,
                                   "results/null-model/ny/gender-permutation",
                                   "permutate-gender", "New York City", k=k)
testObservationWithNullModel(ny.segregation, ny.gen.segregation,
                             "results/null-model/ny/gender-permutation",
                              "New York City", F, F, T, k=k)
testObservationWithNullModelForCategories(ny.segregation, ny.gen.segregation,
                                 "results/null-model/ny/gender-permutation",
                                 "New York City",
                                 k=k, alpha=0.01)

sp.gen.segregation <- runPermutate(sp, sp.segregation,
                                  "results/null-model/sao-paulo/gender-permutation",
                                  "permutate-gender", "São Paulo", k=k)
testObservationWithNullModel(sp.segregation, sp.gen.segregation,
                            "results/null-model/sao-paulo/gender-permutation","São Paulo", F, F, T, k, T)
testObservationWithNullModelForCategories(sp.segregation, sp.gen.segregation,
                                 "results/null-model/sao-paulo/gender-permutation",
                                 "São Paulo",
                                 k=k, alpha=0.01)

tokyo.gen.segregation <- runPermutate(tokyo, tokyo.segregation,
                                     "results/null-model/tokyo/gender-permutation",
                                    "permutate-gender", "Tokyo", k=k)
testObservationWithNullModel(tokyo.segregation, tokyo.gen.segregation,
                            "results/null-model/tokyo/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(tokyo.segregation, tokyo.gen.segregation,
                                 "results/null-model/tokyo/gender-permutation",
                                 "Tokyo",
                                 k=k, alpha=0.01)

riyadh.gen.segregation <- runPermutate(riyadh, riyadh.segregation,
                                       "results/null-model/riyadh/gender-permutation",
                                       "permutate-gender", "Riyadh", k=k)
testObservationWithNullModel(riyadh.segregation, riyadh.gen.segregation,
                            "results/null-model/riyadh/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(riyadh.segregation, riyadh.gen.segregation,
                                 "results/null-model/riyadh/gender-permutation",
                                 "Riyadh",
                                 k=k, alpha=0.01)

ad.gen.segregation <- runPermutate(ad, ad.segregation,
                                       "results/null-model/abu-dhabi/gender-permutation",
                                       "permutate-gender", "Abu Dhabi", k=k)
testObservationWithNullModel(ad.segregation, ad.gen.segregation,
                            "results/null-model/abu-dhabi/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(ad.segregation, ad.gen.segregation,
                                 "results/null-model/abu-dhabi/gender-permutation",
                                 "Abu Dhabi",
                                 k=k, alpha=0.01)

paris.gen.segregation <- runPermutate(paris, paris.segregation,
                                       "results/null-model/Paris/gender-permutation",
                                       "permutate-gender", "Paris", k=k)
testObservationWithNullModel(paris.segregation, paris.gen.segregation,
                            "results/null-model/paris/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(paris.segregation, paris.gen.segregation,
                                 "results/null-model/paris/gender-permutation",
                                 "Paris",
                                 k=k, alpha=0.01)

jakarta.gen.segregation <- runPermutate(jakarta, jakarta.segregation,
                                       "results/null-model/jakarta/gender-permutation",
                                       "permutate-gender", "Jakarta", k=k)
testObservationWithNullModel(jakarta.segregation, jakarta.gen.segregation,
                            "results/null-model/jakarta/gender-permutation", F, F, T, k, T)
testObservationWithNullModelForCategories(jakarta.segregation, jakarta.gen.segregation,
                                 "results/null-model/jakarta/gender-permutation",
                                 "jakarta",
                                 k=k, alpha=0.01)

########### Countries ###############

japan.gen.segregation <- runPermutate(japan, japan.segregation,
                                     "results/null-model/japan/gender-permutation",
                                     "permutate-gender", "Japan", k=k)
testObservationWithNullModelForCategories(japan.segregation, japan.gen.segregation,
                               "results/null-model/japan/gender-permutation",
                               "Japan",
                               k=k, alpha=0.01)

indonesia.gen.segregation <- runPermutate(indonesia, indonesia.segregation,
                                     "results/null-model/indonesia/gender-permutation",
                                     "permutate-gender", "Indonesia", k=k)
testObservationWithNullModelForCategories(indonesia.segregation, indonesia.gen.segregation,
                               "results/null-model/indonesia/gender-permutation",
                               "Indonesia",
                               k=k, alpha=0.01)

france.gen.segregation <- runPermutate(france, france.segregation,
                                     "results/null-model/france/gender-permutation",
                                     "permutate-gender", "France", k=k)
testObservationWithNullModelForCategories(france.segregation, france.gen.segregation,
                               "results/null-model/france/gender-permutation",
                               "France",
                               k=k, alpha=0.01)

uae.gen.segregation <- runPermutate(uae, uae.segregation,
                                     "results/null-model/uae/gender-permutation",
                                     "permutate-gender", "the United Arab Emirates", k=k)
testObservationWithNullModelForCategories(uae.segregation, uae.gen.segregation,
                               "results/null-model/uae/gender-permutation",
                               "the United Arab Emirates",
                               k=k, alpha=0.01)

sa.gen.segregation <- runPermutate(sa, sa.segregation,
                                     "results/null-model/sa/gender-permutation",
                                     "permutate-gender", "Saudi Arabia", k=k)
testObservationWithNullModelForCategories(sa.segregation, sa.gen.segregation,
                               "results/null-model/sa/gender-permutation",
                               "Saudi Arabia",
                               k=k, alpha=0.01)

brazil.gen.segregation <- runPermutate(brazil, brazil.segregation,
                                       "results/null-model/brazil/gender-permutation",
                                       "permutate-gender", "Brazil", k=k)
testObservationWithNullModelForCategories(brazil.segregation, brazil.gen.segregation,
                                 "results/null-model/brazil/gender-permutation",
                                 "Brazil",
                                 k=k, alpha=0.01)

usa.gen.segregation <- runPermutate(usa, usa.segregation,
                                       "results/null-model/usa/gender-permutation",
                                       "permutate-gender", "the USA", k=k)
testObservationWithNullModelForCategories(usa.segregation, usa.gen.segregation,
                                 "results/null-model/usa/gender-permutation",
                                 "the USA",
                                 k=k, alpha=0.01)
