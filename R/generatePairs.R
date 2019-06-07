generatePairs <- function(n1, n2 = NULL, incl.self = FALSE, incl.rev = FALSE){

	# n1: VALUES FOR FIRST VARIABLE
	# n2: VALUES FOR SECOND VARIABLE
	# incl.rev will only work with input of just n1 (no n2 input)

	# IF n2 IS NULL MAKE SAME AS n1
	m <- matrix(NA, nrow=0, ncol=2)
	if(is.null(n2)){

		for(i in n1){
			for(j in n1){

				# SKIP IF OTHER ORDER
				if(!incl.rev && j < i) next

				# SKIP IDENTICAL NUMBERS, IF SPECIFIED
				if(!incl.self && j == i) next

				m <- rbind(m, c(i, j))
			}
		}

	}else{

		for(i in n1){
			for(j in n2){

				# SKIP IDENTICAL NUMBERS, IF SPECIFIED
				if(!incl.self && j == i) next

				m <- rbind(m, c(i, j))
			}
		}
	}

	m
}