
################################################################################

###  The locations of various files and directories

#  The location of "results" files
RESULTS_DIR = "/var/www/html/actigraph/results/"
#  The full path to the saved nnet fit to use (including file name)
NNET_PATH = "/var/www/cgi-bin/actigraph/nnet3ests.RData"
#  The path to the "status" file.  This is passed in from the R.cgi script
STATUS_FILE = Sys.getenv("CURRENT_STATUS_FILE");
#  The path to the "warnings" file.  This is passed in from the R.cgi script
WARNINGS_FILE = Sys.getenv("WARNINGS_FILE");
#  The path to the file with code related to wear time.
WEARTIME_CODE_FILE = "/var/www/cgi-bin/actigraph/weartimefuncts.R";
#  The path to the file with code for reading in the data file.
READDATA_CODE_FILE = "/var/www/cgi-bin/actigraph/readDataFile.R";
#  The path to the file with code for sojourn estimation.
SOJOURNESTIMATION_CODE_FILE = "/var/www/cgi-bin/actigraph/sojournEstimation.R";
#  The path to the file with code for computing/estimating number of bouts.
BOUTESTIMATION_CODE_FILE = "/var/www/cgi-bin/actigraph/boutsComputation.R";
#  The path to the file with code for interacting with the website.
WEBSITEINTERACT_CODE_FILE = "/var/www/cgi-bin/actigraph/websiteInteraction.R";



################################################################################

### Load functionality for interacting with the website
### This consists of the get.lock, unlock, status.update, and warnings.update functions.

source(WEBSITEINTERACT_CODE_FILE);


### Load functionality for reading Actigraph data files
### This consists of the read.actigraph.GT3X function.

source(READDATA_CODE_FILE);


################################################################################

###  Here's the actual statistics


acf.lag1 <- function(x) {
# computes lag one autocorrelation
	n <- length(x)
	a <- mean((x[-1]-mean(x[-1]))*(x[-n]-mean(x[-n])))
	v <- var(x)
	if ((v==0)|(is.na(v))) {
		val <- 0
	} else {
		val <- a/v
	}
	return(val)	
}



# update the status
status.update("Processing File... Loading data into R")



# read in the actigraph data

#data <- read.actigraph.GT3X(Sys.getenv("HEADER_FILE"), Sys.getenv("DATA_FILE"))
temp <- read.actigraph.GT3X(Sys.getenv("HEADER_FILE"), Sys.getenv("DATA_FILE"))
data <- temp[[1]]
start.time <- temp[[2]]
n <- nrow(data)

save.image("/var/www/html/actigraph/system/imageDR.RData")


# read in the options
options <- read.csv(Sys.getenv("OPTIONS_FILE"),colClasses="character")
optNames <- names(options);

parse.time <- function(time,date) 
{
	if(length(strsplit(time,":")[[1]]) == 2)
	{
		if(nchar(strsplit(date,"/")[[1]][3]) == 4) {
			return( strptime(paste(time,date),"%H:%M %m/%d/%Y", tz="GMT") )
		} else if(nchar(strsplit(date,"/")[[1]][3]) == 2) {
			return( strptime(paste(time,date),"%H:%M %m/%d/%y", tz="GMT") )
		} else {
			warnings.update("WARNING: Invalid entry in on/off record.  Year must be either 2 or 4 digits.")
			return( "ERROR" )
		}
	} else if(length(strsplit(time,":")[[1]]) == 3)
	{
		if(nchar(strsplit(date,"/")[[1]][3]) == 4) {
			return( strptime(paste(time,date),"%H:%M:%S %m/%d/%Y", tz="GMT") )
		} else if(nchar(strsplit(date,"/")[[1]][3]) == 2) {
			return( strptime(paste(time,date),"%H:%M:%S %m/%d/%y", tz="GMT") )
		} else {
			warnings.update("WARNING: Invalid entry in on/off record.  Year must be either 2 or 4 digits.")
			return( "ERROR" )
		}
	} else
	{
		warnings.update(paste("WARNING: Invalid entry in on/off record.", time, date, "does not follow a valid format.  Time must be entered in the format HH:MM:SS or HH:MM", sep=" "))
		return( "ERROR" )
	}
}

data.indices.when.on <- function(on.off.record, data) {

	n.on.off <- dim(on.off.record)[1]

	first.start <- as.numeric(data[1,"time"])
	n <- dim(data)[1]
	inds <- c()

	for (i in seq_len(n.on.off))
	{
		start <- as.numeric(parse.time(paste(on.off.record$Time.Start[i]),paste(on.off.record$Date.Start[i])))
		end <- 	as.numeric(parse.time(paste(on.off.record$Time.Stop[i]),paste(on.off.record$Date.Stop[i])))
					
		start.index <- as.numeric(start) - first.start
		end.index <- as.numeric(end) - first.start + 1
					
		if(end.index - start.index > 0)
		{
			inds <- c(inds,start.index:end.index)
			if(sum(duplicated(inds)) > 0) {
				warnings.update(paste("WARNING: On period", on.off.record$Time.Start[i], on.off.record$Date.Start[i], "to", on.off.record$Time.Stop[i], on.off.record$Date.Stop[i], "overlaps with one or more previous entries in the on/off record. Overlapping on/off entries were combined into one on period.", sep=" "))
				inds <- unique(inds)
			}
		} else {
			warnings.update(paste("WARNING: Invalid entry in on/off record.  Start time", on.off.record$Time.Start[i], on.off.record$Date.Start[i], "is not before stop time", on.off.record$Time.Stop[i], on.off.record$Date.Stop[i], ". This entry was ignored.", sep=" "))
		}
	}

	return(inds)
}


if((length(grep("NN",options$methods)) == 1) || (length(grep("Crouter",options$methods)) == 1) || (length(grep("Freedson",options$methods)) == 1) || (length(grep("Other",options$methods)) == 1)) {
	source(BOUTESTIMATION_CODE_FILE);
}

on.off.file <- Sys.getenv("ON_OFF_FILE")
if("generateOnOff" %in% optNames) {
	generate.on.off <- options$generateOnOff
}

# If an on/off record was supplied, read the information in from it.
# Compute indices of the data vector when the Actigraph was off.
# Do some validation of the times in the on/off record.
if(on.off.file != "")
{
	on.off.record <- read.csv(on.off.file)
	on.off.record <- on.off.record[on.off.record$Subject == options$subject,]
	on.off.record <- on.off.record[on.off.record$Visit == options$visit,]

	# If there were no entries in the on/off record for the given subject, emit a warning and use an auto-generated on/off record instead.
	if(dim(on.off.record)[1] == 0) {
		warnings.update("WARNING: There were no entries in the uploaded on/off record for the subject specified for this data file.  Treating all time in the data file as on.");
		on.off.file <- "";
	}

	inds.when.on <- data.indices.when.on(on.off.record, data)

	if(on.off.file != "") {
		if(max(inds.when.on) < 1) {
			warnings.update(paste("WARNING: All on times in the uploaded on/off record occur before the first time of data collection in the uploaded Actigraph data file, ", data[1,"time"], ". Treating all time in the data file as on.", sep=""))
			on.off.file <- "";
		} else if(min(inds.when.on) > n) {
			warnings.update(paste("WARNING: All on times in the uploaded on/off record occur after the last time of data collection in the uploaded Actigraph data file, ", data[n,"time"], ". Treating all time in the data file as on.", sep=""))
			on.off.file <- "";
		} else if(min(inds.when.on) < 1) {
			warnings.update("WARNING: The uploaded on/off record specifies on times before the first time of data collection in the uploaded Actigraph data file. These times have been ignored.")
			inds.when.on <- inds.when.on[inds.when.on >= 1]
		} else if(max(inds.when.on) > n) {
			warnings.update("WARNING: The uploaded on/off record specifies on times after the last time of data collection in the uploaded Actigraph data file. These times have been ignored.")
			inds.when.on <- inds.when.on[inds.when.on <= n]
		}
	}

	if (length(inds.when.on)==0 && on.off.file != "") {
		warnings.update("WARNING: The uploaded on/off record did not contain any valid on times during the period of data collection in the uploaded Actigraph data file. Treating all time in the data file as on.")
		on.off.file <- "";
	} else if(length(inds.when.on) > 0 && on.off.file != "") {
		temp.inds.when.on <- c(0,sort(inds.when.on),length(data[["counts"]]) + 1)
		gap.inds <- which( (temp.inds.when.on[-1] - temp.inds.when.on[-length(temp.inds.when.on)]) > 86400 )
		for(i in seq_along(gap.inds)) {
			warnings.update(paste("WARNING: The uploaded on/off record did not contain any on time for a period of 24 or more hours from ", data[temp.inds.when.on[gap.inds[i]] + 1, "time"], " to ", data[temp.inds.when.on[gap.inds[i] + 1], "time"], ".", sep=""))
		}
	}
}


# If the user selected that option, generate an on/off record
if(generate.on.off == "generateOnOff")
{
	# load functionality for estimating wear time.  This code implements
	# the default wear time estimation method in library(PhysicalActivity) to estimate an on off record.
	# (It doesn't require the library to be loaded though.)
	source(WEARTIME_CODE_FILE);

	if("generateOnOffParamFrame" %in% optNames && "generateOnOffParamAllowanceFrame" %in% optNames) {
		generate.on.off.Frame <- as.numeric(options$generateOnOffParamFrame);
		generate.on.off.AllowanceFrame <- as.numeric(options$generateOnOffParamAllowanceFrame);
	} else {
		warnings.update(paste("WARNING: One or both of the required parameters for generating an on/off record was not submitted.  Proceeding with the default values of Frame = 90 and Allowance Frame = 2."))
		generate.on.off.Frame <- 90;
		generate.on.off.AllowanceFrame <- 2;
	}

	junk <- est.on.off( data.frame(counts = data$counts, time = as.character(data$time)), generate.on.off.Frame, generate.on.off.AllowanceFrame )

	on.start.dates <- paste(substr(junk$on.start.times,6,7),substr(junk$on.start.times,9,10),substr(junk$on.start.times,3,4),sep="/")
	on.start.times <- substr(junk$on.start.times,12,19)

	on.end.dates <- paste(substr(junk$off.start.times,6,7),substr(junk$off.start.times,9,10),substr(junk$off.start.times,3,4),sep="/")
	on.end.times <- substr(junk$off.start.times,12,19)
	
	on.off.record <- data.frame(options$subject,options$visit,on.start.dates, on.start.times, on.end.dates, on.end.times)
	names(on.off.record) <- c("Subject", "Visit", "Date.Start", "Time.Start", "Date.Stop", "Time.Stop")

	inds.when.on <- data.indices.when.on(on.off.record, data)
}

if((generate.on.off == "" && on.off.file == "") || generate.on.off == "allOn") {
	on.start.dates <- paste(substr(data[1,"time"],6,7),substr(data[1,"time"],9,10),substr(data[1,"time"],3,4),sep="/")
	on.start.times <- substr(data[1,"time"],12,19)

	n <- length(data[["counts"]])
	on.end.dates <- paste(substr(data[n,"time"],6,7),substr(data[n,"time"],9,10),substr(data[n,"time"],3,4),sep="/")
	on.end.times <- substr(data[n,"time"],12,19)

	on.off.record <- data.frame(options$subject, options$visit, on.start.dates, on.start.times, on.end.dates, on.end.times)
	names(on.off.record) <- c("Subject", "Visit", "Date.Start", "Time.Start", "Date.Stop", "Time.Stop")

	inds.when.on <- data.indices.when.on(on.off.record, data)
}

if (length(inds.when.on )>0)
	data[-inds.when.on,"counts"] <- 0


save.image("/var/www/html/actigraph/system/image1.RData")




# set up data frames to contain summary information by minute and by day
temp <- tapply(data$counts,data$min.as.factor,mean)*60	#use mean*60 instead of sum to account for partial minutes
sum.by.min <- data.frame(time=names(temp),cpm=as.vector(temp))
sum.by.min$day <- as.character(strptime(as.character(sum.by.min$time),"%Y-%m-%d"))

sum.by.min$day.as.factor <- factor(sum.by.min$day, levels=unique(sum.by.min$day))
# this is used in tapply's, we specify the levels here to be sure we get the order of output to be the same as the order of what we get from unique()


sum.by.day <- data.frame(subject=options$subject,visit=options$visit,day=unique(sum.by.min$day))
day.of.week <- as.character(format(strptime(sum.by.day$day, format="%Y-%m-%d"), format="%A"))
sum.by.day$weekday <- "weekday"
sum.by.day$weekday[(day.of.week == "Saturday") | (day.of.week == "Sunday")] <- "weekend"


save.image("/var/www/html/actigraph/system/image2.RData")


# build infrastructure to handle sub-intervals of the day, if the user wants them
# This code assumes that all seconds after the start time are present in the data.

interval.times <- as.character(unlist(options[optNames[grep("time+", optNames, perl = TRUE)]]))
interval.times.hours <- as.integer(substr(interval.times, start=1, stop=(nchar(interval.times) - 3)))
interval.times.mins <- as.integer(substr(interval.times, start=(nchar(interval.times) - 1), stop=(nchar(interval.times))))
interval.times.in.mins <- 60*interval.times.hours + interval.times.mins

# do some validation - make sure the times are strictly increasing.
if(length(interval.times) > 0) {
	# the sorted list should be equal to the original list and all entries should be unique
	# (so the list of unique entries should be of the same length as the original list)
	if(!((interval.times.in.mins == sort(interval.times.in.mins)) && (length(interval.times.in.mins) == length(unique(interval.times.in.mins)))))
	{
		# if there were any problems, remove the list of interval times.
		# (there should't be any problems - the times were validated with javascript on the site too)
		# but we should still think about returning an error to the user...
		rm(interval.times)
	}
}

if(length(interval.times) > 0) {
	#build two vectors of length 24 hours with info on which time interval each second and minute falls into
	#note that the element in position 1 of this vector represents second (or minute) 0 of the day (0:00:00, or 12:00:00 AM)
	day.breakdown.by.sec <- rep("not specified", 86400)
	day.breakdown.by.min <- rep("not specified", 1440)

	for(i in 1:(length(interval.times) - 1)) {
		start.time.index <- 60*interval.times.in.mins[i] + 1
		end.time.index <- 60*interval.times.in.mins[i + 1]
		day.breakdown.by.sec[start.time.index:end.time.index] <- paste(interval.times[i], interval.times[i + 1], sep="-")

		start.time.index <- interval.times.in.mins[i] + 1
		end.time.index <- interval.times.in.mins[i + 1]
		day.breakdown.by.min[start.time.index:end.time.index] <- paste(interval.times[i], interval.times[i + 1], sep="-")
	}

	#align the day breakdown so that it starts at the same second/minute in the day as the collected data
	first.sec.of.data <- strsplit(strsplit(as.character(data[["time"]][1]), ' ')[[1]][2], ':')
	first.sec.of.data <- 3600*as.integer(first.sec.of.data[[1]][1]) + 60*as.integer(first.sec.of.data[[1]][2]) + as.integer(first.sec.of.data[[1]][3]) + 1
	day.breakdown.by.sec <- c(day.breakdown.by.sec[first.sec.of.data:86400], day.breakdown.by.sec[1:(first.sec.of.data - 1)])

	first.min.of.data <- strsplit(strsplit(as.character(data[["time"]][1]), ' ')[[1]][2], ':')
	first.min.of.data <- 60*as.integer(first.min.of.data[[1]][1]) + as.integer(first.min.of.data[[1]][2]) + 1
	day.breakdown.by.min <- c(day.breakdown.by.min[first.min.of.data:1440], day.breakdown.by.min[1:(first.min.of.data - 1)])

	#add a column to the data and sum.by.min data frames containing this breakdown information.
	data$interval <- paste(data$day, "-", day.breakdown.by.sec)[1:(length(data$day))]

	data$interval.as.factor <- factor(data$interval, levels=unique(data$interval))
	# this is used in tapply's, we specify the levels here to be sure we get the order of output to be the same as the order of what we get from unique()

	sum.by.min$interval <- paste(sum.by.min$day, "-", day.breakdown.by.min)[1:(length(sum.by.min$day))]

	sum.by.min$interval.as.factor <- factor(sum.by.min$interval, levels=unique(sum.by.min$interval))
	# this is used in tapply's, we specify the levels here to be sure we get the order of output to be the same as the order of what we get from unique()

	#create a new data frame to contain summary information by sub interval of the day
	sum.by.interval <- data.frame(subject=options$subject,visit=options$visit,day=unique(data$interval))
	sum.by.interval$interval <- substr(sum.by.interval$day, start=14, stop=nchar(as.character(sum.by.interval$day)))
	sum.by.interval$day <- substr(sum.by.interval$day, start=1, stop=10)
	day.of.week <- as.character(format(strptime(sum.by.interval$day, format="%Y-%m-%d"), format="%A"))
	sum.by.interval$weekday <- "weekday";
	sum.by.interval$weekday[day.of.week == "Saturday" | day.of.week == "Sunday"] <- "weekend";
}

save.image("/var/www/html/actigraph/system/image3.RData")


# get the time worn in each day and, if applicable, interval.
# the variable "inds.when.on" is left over from the on/off record handling.

# onoffbysec = 0 when off and 1 when on
onoffbysec <- rep(0, length(data$counts))
onoffbysec[inds.when.on] <- 1


sum.by.day$hours.on <- tapply(onoffbysec, data$day.as.factor, sum)/3600

if(length(interval.times) > 0) {
	sum.by.interval$hours.on <- tapply(onoffbysec, data$interval.as.factor, sum)/3600
}


onoffbymin <- tapply(onoffbysec,data$min.as.factor,max)


# vectors containing the day for each second and minute the actigraph was on.  This is used in obtaining summary by day info. below
day.when.on.by.sec <- data[(onoffbysec == 1), "day.as.factor"]
day.when.on.by.min <- sum.by.min[(onoffbymin == 1), "day.as.factor"]

# a vector containing the interval for each second and minute the actigraph was on.  This is used in obtaining summary by interval info. below
if(length(interval.times) > 0) {
	interval.when.on.by.sec <- data[(onoffbysec == 1), "interval.as.factor"]
	interval.when.on.by.min <- sum.by.min[(onoffbymin == 1), "interval.as.factor"]
}


save.image("/var/www/html/actigraph/system/image4.RData")


# Do whatever statistical methods were requested

# update the status
status.update("Processing File... Performing statistical analysis")



#Neural Network
if(length(grep("NN",options$methods)) == 1) {

	# Load the fitted nnets and the nnet library.
	load(NNET_PATH)
	library(nnet)

	# Load functionality for identifying sojourns
	# This consists of the sojourn function.
	source(SOJOURNESTIMATION_CODE_FILE);


	est <- sojourn(data[["counts"]], perc.cut=0.05, perc.cut.2=0.12, perc.cut.3=0.55, too.short=10, sit.cut=90, long.soj=120)
	data$nnet.METs <- est$METs.2

	# update summary

	nnet.METs.when.on <- data[(onoffbysec == 1), "nnet.METs"]
#	nnet.acts.when.on <- data[(onoffbysec == 1), "nnet.acts"]


	sum.by.day$nnet.METhrs <- as.vector(tapply(nnet.METs.when.on,day.when.on.by.sec,sum)/3600)

	sum.by.day$nnet.sedentary.min <- as.vector(tapply((nnet.METs.when.on<1.5),day.when.on.by.sec,sum)/60)
	sum.by.day$nnet.light.min <- as.vector(tapply((nnet.METs.when.on<3)&(nnet.METs.when.on>=1.5),day.when.on.by.sec,sum)/60)
	sum.by.day$nnet.moderate.min <- as.vector(tapply((nnet.METs.when.on<6)&(nnet.METs.when.on>=3),day.when.on.by.sec,sum)/60)
	sum.by.day$nnet.vigorous.min <- as.vector(tapply((nnet.METs.when.on>=6),day.when.on.by.sec,sum)/60)

#	sum.by.day$nnet.min.act.min <- as.vector(tapply((nnet.acts.when.on=="minimal"),day.when.on.by.sec,sum)/60)
#	sum.by.day$nnet.locomot.act.min <- as.vector(tapply((nnet.acts.when.on=="locomotion"),day.when.on.by.sec,sum)/60)
#	sum.by.day$nnet.vigsp.act.min <- as.vector(tapply((nnet.acts.when.on=="vig sport"),day.when.on.by.sec,sum)/60)
#	sum.by.day$nnet.house.act.min <- as.vector(tapply((nnet.acts.when.on=="household/other"),day.when.on.by.sec,sum)/60)


	temp <- get.bouts.info(data$nnet.METs, data$day)
	sum.by.day$nnet.num.bouts <- temp[,"num.bouts"]
	sum.by.day$nnet.bout.hours <- temp[,"bout.hours"]
	sum.by.day$nnet.bout.MET.hours <- temp[,"bout.MET.hours"]


	#if applicable, get MET hours for each sub interval of the day
	if(length(interval.times) > 0) {
		sum.by.interval$nnet.METhrs <- as.vector(tapply(nnet.METs.when.on,interval.when.on.by.sec,sum)/3600)

		sum.by.interval$nnet.sedentary.min <- as.vector(tapply((nnet.METs.when.on<1.5),interval.when.on.by.sec,sum)/60)
		sum.by.interval$nnet.light.min <- as.vector(tapply((nnet.METs.when.on<3)&(nnet.METs.when.on>=1.5),interval.when.on.by.sec,sum)/60)
		sum.by.interval$nnet.moderate.min <- as.vector(tapply((nnet.METs.when.on<6)&(nnet.METs.when.on>=3),interval.when.on.by.sec,sum)/60)
		sum.by.interval$nnet.vigorous.min <- as.vector(tapply((nnet.METs.when.on>=6),interval.when.on.by.sec,sum)/60)

#		sum.by.interval$nnet.min.act.min <- as.vector(tapply((nnet.acts.when.on=="minimal"),interval.when.on.by.sec,sum)/60)
#		sum.by.interval$nnet.locomot.act.min <- as.vector(tapply((nnet.acts.when.on=="locomotion"),interval.when.on.by.sec,sum)/60)
#		sum.by.interval$nnet.vigsp.act.min <- as.vector(tapply((nnet.acts.when.on=="vig sport"),interval.when.on.by.sec,sum)/60)
#		sum.by.interval$nnet.house.act.min <- as.vector(tapply((nnet.acts.when.on=="household/other"),interval.when.on.by.sec,sum)/60)

		temp <- get.bouts.info(data$nnet.METs, data$interval)
		sum.by.interval$nnet.num.bouts <- temp[,"num.bouts"]
		sum.by.interval$nnet.bout.hours <- temp[,"bout.hours"]
		sum.by.interval$nnet.bout.MET.hours <- temp[,"bout.MET.hours"]
	}

	# update the status
	status.update("Processing File... Done with NNet")
}



#Crouter et al 2-regression model
if(length(grep("Crouter",options$methods)) == 1) {
	sum.by.10sec <- list()
	#get total counts for each 10 second interval
	sum.by.10sec$total.count <- tapply(data$counts, data$ten.sec, sum, na.rm = T)
	# adjust for partial 10 second intervals
	sum.by.10sec$total.count <- sum.by.10sec$total.count*10/table(data$ten.sec)

	#get the minute corresponding to each 10 sec interval
	sum.by.10sec$minute <- as.character(strptime(as.character(names(sum.by.10sec$total.count)),"%Y-%m-%d %H:%M"))

	#compute the CV of the counts for the 10 sec intervals in each minute
	sum.by.min$CV.of.counts.per.10sec <- tapply(sum.by.10sec$total.count, sum.by.10sec$minute, function(x){ifelse(mean(x)==0|length(x)==1,0,100*(sd(x)/mean(x)))})
	

	###compute the METs for each minute
	# "default" to 1 - for resting
	sum.by.min$Crouter.METs <- 1
	n <- length(sum.by.min[,1])
	#formula for non-resting counts and low CV
	inds <- (1:n)[(sum.by.min$cpm > 50) & (sum.by.min$CV.of.counts.per.10sec > 0) & (sum.by.min$CV.of.counts.per.10sec <= 10)]
	sum.by.min$Crouter.METs[inds] <- 2.379833*exp(0.00013529*sum.by.min$cpm[inds])
	#formula for non-resting counts and high CV (or CV = 0)
	inds <- (1:n)[(sum.by.min$cpm > 50) & ((sum.by.min$CV.of.counts.per.10sec == 0) | (sum.by.min$CV.of.counts.per.10sec > 10))]
	#status.update(sum.by.min$cpm)
	#status.update(sum.by.min$CV.of.counts.per.10sec)
	sum.by.min$Crouter.METs[inds] <- 2.330519 +
		(0.001646*sum.by.min$cpm[inds]) -
		((1.2017e-7)*(sum.by.min$cpm[inds]^2)) +
		((3.3779e-12)*(sum.by.min$cpm[inds]^3))


	#get MET hours for each day
	Crouter.METs.when.on <- sum.by.min[(onoffbymin == 1), "Crouter.METs"]

	sum.by.day$Crouter.METhrs <- as.vector(tapply(Crouter.METs.when.on,day.when.on.by.min,sum)/60)
	sum.by.day$Crouter.sedentary.min <- as.vector(tapply((Crouter.METs.when.on<1.5),day.when.on.by.min,sum))
	sum.by.day$Crouter.light.min <- as.vector(tapply((Crouter.METs.when.on<3)&(Crouter.METs.when.on>=1.5),day.when.on.by.min,sum))
	sum.by.day$Crouter.moderate.min <- as.vector(tapply((Crouter.METs.when.on<6)&(Crouter.METs.when.on>=3),day.when.on.by.min,sum))
	sum.by.day$Crouter.vigorous.min <- as.vector(tapply((Crouter.METs.when.on>=6),day.when.on.by.min,sum))

	temp <- get.bouts.info(sum.by.min$Crouter.METs, sum.by.min$day, units="min")
	sum.by.day$Crouter.num.bouts <- temp[,"num.bouts"]
	sum.by.day$Crouter.bout.hours <- temp[,"bout.hours"]
	sum.by.day$Crouter.bout.MET.hours <- temp[,"bout.MET.hours"]

	#if applicable, get MET hours for each sub interval of the day
	if(length(interval.times) > 0) {
		sum.by.interval$Crouter.METhrs <- as.vector(tapply(Crouter.METs.when.on,interval.when.on.by.min,sum)/60)
		sum.by.interval$Crouter.sedentary.min <- as.vector(tapply((Crouter.METs.when.on<1.5),interval.when.on.by.min,sum))
		sum.by.interval$Crouter.light.min <- as.vector(tapply((Crouter.METs.when.on<3)&(Crouter.METs.when.on>=1.5),interval.when.on.by.min, sum))
		sum.by.interval$Crouter.moderate.min <- as.vector(tapply((Crouter.METs.when.on<6)&(Crouter.METs.when.on>=3),interval.when.on.by.min,sum))
		sum.by.interval$Crouter.vigorous.min <- as.vector(tapply((Crouter.METs.when.on>=6),interval.when.on.by.min,sum))

		temp <- get.bouts.info(sum.by.min$Crouter.METs, sum.by.min$interval, units="min")
		sum.by.interval$Crouter.num.bouts <- temp[,"num.bouts"]
		sum.by.interval$Crouter.bout.hours <- temp[,"bout.hours"]
		sum.by.interval$Crouter.bout.MET.hours <- temp[,"bout.MET.hours"]
	}

	# update the status
	status.update("Processing File... Done with Crouter")
}



# Freedson linear regression
if(length(grep("Freedson",options$methods)) == 1) {

	est.mets.by.sec <- 1.439008 + (60*0.000795 * data$counts)

	sum.by.min$Freedson.METs <- 1.439008 + (0.000795 * sum.by.min$cpm)

	Freedson.METs.when.on <- sum.by.min[(onoffbymin == 1), "Freedson.METs"]

	sum.by.day$Freedson.METhrs <- as.vector(tapply(Freedson.METs.when.on,day.when.on.by.min,sum)/60)
	sum.by.day$Freedson.sedentary.min <- as.vector(tapply((Freedson.METs.when.on<1.5),day.when.on.by.min,sum))
	sum.by.day$Freedson.light.min <- as.vector(tapply((Freedson.METs.when.on<3)&(Freedson.METs.when.on>=1.5),day.when.on.by.min,sum))
	sum.by.day$Freedson.moderate.min <- as.vector(tapply((Freedson.METs.when.on<6)&(Freedson.METs.when.on>=3),day.when.on.by.min,sum))
	sum.by.day$Freedson.vigorous.min <- as.vector(tapply((Freedson.METs.when.on>=6),day.when.on.by.min,sum))

	sum.by.day$Freedson.sedentary.min.by.sec <- as.vector(tapply((est.mets.by.sec<1.5),data$day,sum))/60
	sum.by.day$Freedson.light.min.by.sec <- as.vector(tapply((est.mets.by.sec<3)&(est.mets.by.sec>=1.5),data$day,sum))/60
	sum.by.day$Freedson.moderate.min.by.sec <- as.vector(tapply((est.mets.by.sec<6)&(est.mets.by.sec>=3),data$day,sum))/60
	sum.by.day$Freedson.vigorous.min.by.sec <- as.vector(tapply((est.mets.by.sec>=6),data$day,sum))/60


	temp <- get.bouts.info(sum.by.min$Freedson.METs, sum.by.min$day, units="min")
	sum.by.day$Freedson.num.bouts <- temp[,"num.bouts"]
	sum.by.day$Freedson.bout.hours <- temp[,"bout.hours"]
	sum.by.day$Freedson.bout.MET.hours <- temp[,"bout.MET.hours"]

	#if applicable, get MET hours for each sub interval of the day
	if(length(interval.times) > 0) {
		sum.by.interval$Freedson.METhrs <- as.vector(tapply(Freedson.METs.when.on,interval.when.on.by.min,sum)/60)
		sum.by.interval$Freedson.sedentary.min <- as.vector(tapply((Freedson.METs.when.on<1.5),interval.when.on.by.min,sum))
		sum.by.interval$Freedson.light.min <- as.vector(tapply((Freedson.METs.when.on<3)&(Freedson.METs.when.on>=1.5),interval.when.on.by.min,sum))
		sum.by.interval$Freedson.moderate.min <- as.vector(tapply((Freedson.METs.when.on<6)&(Freedson.METs.when.on>=3),interval.when.on.by.min,sum))
		sum.by.interval$Freedson.vigorous.min <- as.vector(tapply((Freedson.METs.when.on>=6),interval.when.on.by.min,sum))

		temp <- get.bouts.info(sum.by.min$Freedson.METs, sum.by.min$interval, units="min")
		sum.by.interval$Freedson.num.bouts <- temp[,"num.bouts"]
		sum.by.interval$Freedson.bout.hours <- temp[,"bout.hours"]
		sum.by.interval$Freedson.bout.MET.hours <- temp[,"bout.MET.hours"]
		sum.by.interval$Freedson.sed.to.gt.sed.trans <- temp[,"sed.to.gt.sed.trans"]
	}

	# update the status
	status.update("Processing File... Done with Freedson")
}


# "Other" linear regression - cutpoints specified
if(length(grep("Other",options$methods)) == 1) {
	SedentaryCutpoint <- as.numeric(options[["SedentaryCutpoint"]])
	X3METCutpoint <- as.numeric(options[["X3METCutpoint"]])
	X6METCutpoint <- as.numeric(options[["X6METCutpoint"]])

	beta.0 <- (3*X6METCutpoint - 6*X3METCutpoint)/(X6METCutpoint - X3METCutpoint)
	beta.1 <- 3/(X6METCutpoint - X3METCutpoint)

	sum.by.min$OtherLM.METs <- apply(as.matrix(beta.0 + (beta.1 * sum.by.min$cpm)), 1, function(x) { return(max(x, 1.1)) })
	sum.by.min[sum.by.min$cpm <= SedentaryCutpoint, "OtherLM.METs"] <- 1.1

	OtherLM.METs.when.on <- sum.by.min[(onoffbymin == 1), "OtherLM.METs"]
	sum.by.day$OtherLM.METhrs <- as.vector(tapply(OtherLM.METs.when.on,day.when.on.by.min,sum)/60)
	sum.by.day$OtherLM.sedentary.min <- as.vector(tapply((OtherLM.METs.when.on<1.5),day.when.on.by.min,sum))
	sum.by.day$OtherLM.light.min <- as.vector(tapply((OtherLM.METs.when.on<3)&(OtherLM.METs.when.on>=1.5),day.when.on.by.min,sum))
	sum.by.day$OtherLM.moderate.min <- as.vector(tapply((OtherLM.METs.when.on<6)&(OtherLM.METs.when.on>=3),day.when.on.by.min,sum))
	sum.by.day$OtherLM.vigorous.min <- as.vector(tapply((OtherLM.METs.when.on>=6),day.when.on.by.min,sum))

	temp <- get.bouts.info(sum.by.min$OtherLM.METs, sum.by.min$day, units="min")
	sum.by.day$OtherLM.num.bouts <- temp[,"num.bouts"]
	sum.by.day$OtherLM.bout.hours <- temp[,"bout.hours"]
	sum.by.day$OtherLM.bout.MET.hours <- temp[,"bout.MET.hours"]
	sum.by.day$OtherLM.sed.to.gt.sed.trans <- temp[,"sed.to.gt.sed.trans"]

	#if applicable, get MET hours for each sub interval of the day
	if(length(interval.times) > 0) {
		sum.by.interval$OtherLM.METhrs <- as.vector(tapply(OtherLM.METs.when.on,interval.when.on.by.min,sum)/60)
		sum.by.interval$OtherLM.sedentary.min <- as.vector(tapply((OtherLM.METs.when.on<1.5),interval.when.on.by.min,sum))
		sum.by.interval$OtherLM.light.min <- as.vector(tapply((OtherLM.METs.when.on<3)&(OtherLM.METs.when.on>=1.5),interval.when.on.by.min,sum))
		sum.by.interval$OtherLM.moderate.min <- as.vector(tapply((OtherLM.METs.when.on<6)&(OtherLM.METs.when.on>=3),interval.when.on.by.min,sum))
		sum.by.interval$OtherLM.vigorous.min <- as.vector(tapply((OtherLM.METs.when.on>=6),interval.when.on.by.min,sum))

		temp <- get.bouts.info(sum.by.min$OtherLM.METs, sum.by.min$interval, units="min")
		sum.by.interval$OtherLM.num.bouts <- temp[,"num.bouts"]
		sum.by.interval$OtherLM.bout.hours <- temp[,"bout.hours"]
		sum.by.interval$OtherLM.bout.MET.hours <- temp[,"bout.MET.hours"]
		sum.by.interval$OtherLM.sed.to.gt.sed.trans <- temp[,"sed.to.gt.sed.trans"]
	}

	# update the status
	status.update("Processing File... Done with Other Linear Model")
}



# Write results to a file
if(options$newFile == "true") {
	# if it's the first entry in the file, include column headers
	write.table(sum.by.day,file=paste(RESULTS_DIR,"session",options$sessionID,"-results.csv",sep=""),sep=",",row.names=F, qmethod="double")
	if(length(interval.times) > 0) {
		write.table(sum.by.interval,file=paste(RESULTS_DIR,"session",options$sessionID,"-intervalresults.csv",sep=""),sep=",",row.names=F, qmethod="double")
	}
	write.table(on.off.record,file=paste(RESULTS_DIR,"session",options$sessionID,"-onrecord.csv",sep=""),sep=",",row.names=F, qmethod="double")
} else {
	# if it's not the first entry, don't include column headers
	write.table(sum.by.day,file=paste(RESULTS_DIR,"session",options$sessionID,"-results.csv",sep=""),append=TRUE,sep=",",row.names=F, col.names=F, qmethod="double")
	if(length(interval.times) > 0) {
		write.table(sum.by.interval,file=paste(RESULTS_DIR,"session",options$sessionID,"-intervalresults.csv",sep=""),append=TRUE,sep=",",row.names=F, col.names=F, qmethod="double")
	}
	write.table(on.off.record,file=paste(RESULTS_DIR,"session",options$sessionID,"-onrecord.csv",sep=""),append=TRUE,sep=",",row.names=F,col.names=F,qmethod="double")
}



# output a "success" message to be sent to the browser
cat("\n<status>success</status>\n<message> File processed successfully </message>\n</root>")

# update the status
status.update("Complete")
