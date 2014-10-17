corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
		id = 1:332
		x <- c()
		t <- data.frame(Date=as.Date(character()),sulfate=numeric(0),nitrate=numeric(0),ID=integer(0),stringsAsFactors = FALSE)
		for (i in id) {
		f <- paste(paste(directory,formatC(i,width = 3,flag = "0"), sep = "/"),".csv", sep = "")
		t <- read.csv(f)
		t[complete.cases(t),] -> t
		if (nrow(t) > threshold )  x <- c(x,cor(t$sulfate,t$nitrate))
		}
		return(x)
}
