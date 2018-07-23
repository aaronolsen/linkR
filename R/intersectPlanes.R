intersectPlanes <- function(p1, n1, p2, n2){

	## Find line at the intersection of two planes
	# https://math.stackexchange.com/questions/475953/how-to-calculate-the-intersection-of-two-planes

	# Return NULL if planes are parallel
	if(avec(n1, n2) < 1e-6) return(NULL)

	# Find direction of line by cross-product
	l_v <- uvector(cprod(n1, n2))

	# Find line constants
	c1 <- sum(p1*n1)
	c2 <- sum(p2*n2)


	# Find point on line
	pl <- rep(NA, 3)
	if(n1[1] != 0){
		
		# Assume z = 0
		if(n1[2] == 0){

			# Solve for x
			pl[1] <- c1 / n1[1]

			# Solve for y
			if(n2[2] == 0){
				pl[2] <- 0		# Can be any value - just set to 0
			}else{
				pl[2] <- (c2 - n2[1]*pl[1]) / n2[2]
			}

		}else{
			
			if(n2[1] == 0 && n2[2] == 0){
				pl[2] <- 0
				pl[1] <- (c1 - n1[3]*(c2 / n2[3])) / n1[1]
			}else{
				pl[2] <- -(c2 - ((n2[1]*c1) / n1[1])) / (((n2[1]*n1[2]) / n1[1]) - n2[2])
				pl[1] <- (c1 - n1[2]*pl[2]) / n1[1]
			}
		}

		if(n1[3] != 0) pl[3] <- (c1 - n1[1]*pl[1] - n1[2]*pl[2]) / n1[3]
		if(n2[3] != 0) pl[3] <- (c2 - n2[1]*pl[1] - n2[2]*pl[2]) / n2[3]
		if(n1[3] == 0 && n2[3] == 0) pl[3] <- 0		# Can be any value

	}else if(n1[3] != 0){

		# Assume y = 0
		if(n1[1] == 0){

			# Solve for z
			pl[3] <- c1 / n1[3]

			# Solve for x
			if(n2[1] == 0){
				pl[1] <- 0		# Can be any value - just set to 0
			}else{
				pl[1] <- (c2 - n2[3]*pl[3]) / n2[1]
			}

		}else{
			
			if(n2[1] == 0 && n2[3] == 0){
				pl[1] <- 0
				pl[3] <- (c1 - n1[2]*(c2 / n2[2])) / n1[3]
			}else{
				pl[1] <- (c2 - ((n2[3]*c1) / n1[3])) / (n2[1] - ((n2[3]*n1[1])/n1[3]))
				pl[3] <- (c1 - n1[1]*pl[1]) / n1[3]
			}
		}

		if(n1[2] != 0) pl[2] <- (c1 - n1[1]*pl[1] - n1[3]*pl[3]) / n1[2]
		if(n2[2] != 0) pl[2] <- (c2 - n2[1]*pl[1] - n2[3]*pl[3]) / n2[2]
		if(n1[2] == 0 && n2[2] == 0) pl[2] <- 0		# Can be any value

	}else{

		# Assume x = 0
		if(n1[3] == 0){

			# Solve for y
			pl[2] <- c1 / n1[2]

			# Solve for z
			if(n2[3] == 0){
				pl[3] <- 0		# Can be any value - just set to 0
			}else{
				pl[3] <- (c2 - n2[2]*pl[2]) / n2[3]
			}

		}else{
			
			if(n2[3] == 0 && n2[2] == 0){
				pl[3] <- 0
				pl[2] <- (c1 - n1[1]*(c2 / n2[1])) / n1[2]
			}else{
				pl[3] <- (c2 - ((n2[2]*c1) / n1[2])) / (n2[3] - ((n2[2]*n1[3])/n1[2]))
				pl[2] <- (c1 - (n1[3]*pl[3])) / n1[2]
			}
		}

		if(n1[1] != 0) pl[1] <- (c1 - n1[2]*pl[2] - n1[3]*pl[3]) / n1[1]
		if(n2[1] != 0) pl[1] <- (c2 - n2[2]*pl[2] - n2[3]*pl[3]) / n2[1]
		if(n1[1] == 0 && n2[1] == 0) pl[1] <- 0		# Can be any value
	}

	# Check solution
	check1 <- sum(pl*n1) - c1
	check2 <- sum(pl*n2) - c2
	if(abs(check1) > 1e-7 || abs(check2) > 1e-7) stop('One of checks exceeds 0')

	#cat(paste0('Check: ', check1, '\n'))
	#cat(paste0('Check: ', check2, '\n'))

	list('p'=pl, 'v'=l_v, 'l1'=pl, 'l2'=pl+l_v)
}