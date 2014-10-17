complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
		cc <- data.frame(id=integer(0),nobs=integer(0),stringsAsFactors = FALSE)
		if((min(id) < 0) || (max(id) > 332)) return(cc)
		x <- data.frame(Date=as.Date(character()),sulfate=numeric(0),nitrate=numeric(0),ID=integer(0),stringsAsFactors = FALSE)
		files <- paste(paste(directory,formatC(x=id,width = 3,flag = "0"), sep = "/"),".csv", sep = "")
		for (f in files) x <- rbind(x,read.csv(f))
		x[complete.cases(x),] -> x
		for (i in id) {cc <- rbind(cc,data.frame(id=i,nobs=nrow(x[x$ID==i,])))}
		return(cc)
}
