get.lock <- function(file, try.count=0) {
	got.lock <- FALSE
	temp.lock.file <- paste(file,".lock.temp",sep="",collapse="")
	real.lock.file <- paste(file,".lock",sep="",collapse="")
	while(!got.lock && try.count < 5) {
		try.count <- try.count + 1
		tryCatch({
			temp <- file.create(temp.lock.file)
			cat("applynn",file=temp.lock.file)
			got.lock <- file.copy(from = temp.lock.file, to=real.lock.file, overwrite=FALSE)
		}, error = function (e) {
			got.lock <- FALSE
		}, finally = {
			temp <- file.remove(temp.lock.file)
			if(!got.lock) {
				Sys.sleep(0.5)
			}
		})
	}
	return(got.lock)
}

unlock <- function(file) {
	temp <- file.remove(paste(file,".lock",sep="",collapse=""))
}

status.update <- function(status) {
	#write new status to the status file
	if(get.lock(STATUS_FILE)) {
		cat("<status>",status,"</status>",file=STATUS_FILE)
		unlock(STATUS_FILE)
	}
}

warnings.update <- function(warning) {
	#write new warning to the warnings file
	if(get.lock(WARNINGS_FILE)) { #try really hard to get the lock - we don't want our warning message to fail to get through.
		cat("<warning>",warning,"</warning>",file=WARNINGS_FILE, append=TRUE)
		unlock(WARNINGS_FILE)
	}
}
