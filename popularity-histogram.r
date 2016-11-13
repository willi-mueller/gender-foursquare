datafile <- "~/studium/Lehrveranstaltungen/informationRetrieval/GenderSocialMedia/paper/2016-epjDataScience/images/subcategory-hist/Saudi-Arabia-category-University-anomalous.csv"


popularityHistogramSubcategory <- function(datafile, filename, xlim=c(-0.05, 0.05), breaks=10) {
	#filename <- sprintf("%s/location-%s-anomalous.pdf", folderName, location)

	pdf(filename, pointsize=25)

	data <- read.table(datafile  ,sep ="\t", header=T)


	h <- hist(data$generated, breaks=breaks, plot=F) # for location: empiricalDifference, for subcategory: generated
	#h$counts=h$counts/sum(h$counts)
	plot(h, xlab="Popularity difference", main=NULL, ylab="Occurrences in %", xlim=xlim)
	abline(v=data$lowerLimit, lty=3, lwd=5)
	abline(v=data$upperLimit, lty=3, lwd=5)
	abline(v=data$observed[1], lwd=5)	# for location: observedDifference, for subcategory: observed
	dev.off()
}


popularityHistogramLocation <- function(datafile, filename, xlim= c(-0.006, 0.001), breaks=10) {
	#filename <- sprintf("%s/location-%s-anomalous.pdf", folderName, location)

	pdf(filename, pointsize=25)

	data <- read.table(datafile  ,sep ="\t", header=T)


	h <- hist(data$empiricalDifference, breaks=breaks, plot=F)
	#h$counts=h$counts/sum(h$counts)
	plot(h, xlab="Popularity difference", main=NULL, ylab="Occurrences in %", xlim=xlim)
	abline(v=data$lowerLimit, lty=3, lwd=5)
	abline(v=data$upperLimit, lty=3, lwd=5)
	abline(v=data$observedDifference[1], lwd=5)
	dev.off()
}
