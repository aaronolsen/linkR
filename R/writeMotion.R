writeMotion <- function(x, file){

	if(length(dim(x)) == 3){
		
		if(dim(x)[2] == 3){
		
			xmat <- arr2mat(x)

		}else if(dim(x)[3] == 4){
		}

	}else if(length(dim(x)) == 4){

	}

	if(grepl('[.]csv$', file)) write.csv(x=xmat, file=file)
}