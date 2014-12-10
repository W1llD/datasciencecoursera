pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
		pm <- 0
		if((min(id) < 0) || (max(id) > 332)) return(pm)
		x <- data.frame(Date=as.Date(character()),sulfate=numeric(0),nitrate=numeric(0),ID=integer(0),stringsAsFactors = FALSE)
		files <- paste(paste(directory,formatC(x=id,width = 3,flag = "0"), sep = "/"),".csv", sep = "")
		for (f in files) x <- rbind(x,read.csv(f))
		##x[complete.cases(x),] -> x
		pm <- if (pollutant == "sulfate") 	   {
						mean(x$sulfate, na.rm = TRUE) 
									        }
		      else if (pollutant == "nitrate") {
						mean(x$nitrate, na.rm = TRUE) 
											}
			  else {0}
		return(pm)
		}