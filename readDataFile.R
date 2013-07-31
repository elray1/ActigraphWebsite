# Function to read data in from files.  Submitted data file is preprocessed and split into 2 files:
#  - One which contains the start date for data collection from the file header
#  - One which contains the recorded counts

read.actigraph.GT3X <- function(header.file.name, data.file.name) {
# Takes file names for the preprocessed header info and data file.
# Returns a data frame with counts, time, min, and ten.sec.

	# read header and get start time
	start.time <- as.POSIXlt(strptime(readLines(header.file.name),"%H:%M:%S %m/%d/%Y", tz="GMT"))

	# read the data
	data <- read.csv(data.file.name, header=FALSE, col.names="counts")

	####### bug
	### also adjust for daylight savings time
	### some code outside this function must also be adjusted

	#### the code preserves the relationship between the times
	#### but replaces the actual times with ranks (indices)

	# build other columns with time information
	n <- length(data$counts)
	data$time <- start.time + (0:(n-1))

	data$day <- as.character(trunc(data$time, units="day"))
	# (as.character instead of number or list makes code for intervals of the day much faster.)

	data$day.as.factor <- factor(data$day, levels=unique(data$day))
	# this is used in tapply's, we specify the levels here to be sure we get the order of output to be the same as the order of what we get from unique()

	temp.start <- start.time
	temp.start$sec <- 0
	n.to.drop <- start.time$sec
	if(n.to.drop > 0) {
		data$min <- temp.start + (60*rep(0:(floor(n/60)),each=60,length=(n + n.to.drop)))[-(1:n.to.drop)]
	} else {
		data$min <- temp.start + (60*rep(0:(floor(n/60)),each=60,length=n))
	}

	data$min.as.factor <- factor(data$min, levels=as.character(unique(data$min)))
	# this is used in tapply's, we specify the levels here to be sure we get the order of output to be the same as the order of what we get from unique()

#  The following was used for Crouter
#	temp.start$sec <- 10*trunc(start.time$sec/10)
#	n.to.drop <- start.time$sec - 10*trunc(start.time$sec/10)
#	if(n.to.drop > 0) {
#		data$ten.sec <- temp.start + (10*rep(0:(floor(n/10)),each=10,length=(n + n.to.drop)))[-(1:n.to.drop)]
#	} else {
#		data$ten.sec <- temp.start + (10*rep(0:(floor(n/10)),each=10,length=n))
#	}

	return(list(data, start.time))
}
