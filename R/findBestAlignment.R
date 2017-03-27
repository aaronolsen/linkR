findBestAlignment <- function(m1, m2, m3 = NULL, sign = NULL){

	## See fit_joint_model for revised svd code that fixes bug
	
	# IF INPUTTING A MATRIX WITH ALL ZEROS IN ONE DIMENSION, MAKE IT M2, NOT M1

	if(is.null(m3)) m3r <- NULL

	# SET INITIAL COMMON POINT MATRIX VALUES
	m1o <- m1
	m2o <- m2

	# USE ROWNAMES, IF GIVEN, TO REMOVE NON-CORRESPONDING POINTS
	if(!is.null(rownames(m1)) && !is.null(rownames(m2))){

		m1o <- m1o[sort(rownames(m1o)), ]
		m2o <- m2o[sort(rownames(m2o)), ]

		# REMOVE NA VALUES
		m1o <- m1o[!is.na(m1o[, 1]), ]
		m2o <- m2o[!is.na(m2o[, 1]), ]

		m1o[!rownames(m1o) %in% rownames(m2o), ] <- NA
		m2o[!rownames(m2o) %in% rownames(m1o), ] <- NA

	}else{

		# REPLACE NON-COMMON LANDMARKS BETWEEN TWO MATRICES WITH NA
		m1o[which(is.na(m2o))] <- NA
		m2o[which(is.na(m1o))] <- NA
	}

	# REMOVE NA VALUES
	m1o <- m1o[!is.na(m1o[, 1]), ]
	m2o <- m2o[!is.na(m2o[, 1]), ]

	# CREATE FIRST TRANSLATION TRANSFORMATION MATRIX
	tmat1 <- diag(4);tmat1[1:3, 4] <- -colMeans(m2o, na.rm=TRUE)
	
	# CENTER M2 ABOUT CENTROID OF COMMON POINTS
	m2c <- mtransform(m2, tmat1)

	# APPLY TRANSLATION TO EXTRA MATRIX, IF PROVIDED
	if(!is.null(m3)) m3c <- mtransform(m3, tmat1)
	
	# CENTER COMMON POINTS
	m1oc <- scale(m1o, center=TRUE, scale=FALSE)
	m2oc <- scale(m2o, center=TRUE, scale=FALSE)

	# FIND ROTATION MATRIX TO APPLY TO M2 THAT MINIMIZES DISTANCE BETWEEN M1 AND M2
	SVD <- svd(t(na.omit(m1oc)) %*% na.omit(m2oc))
	L <- diag(SVD$d)
	
	S <- ifelse(L<0, -1, L)
	S <- ifelse(L>0, 1, L)
	
	## NEW ADDITION - WAS GIVING ZERO AT 3,3 BEFORE
	if(!is.null(sign)){
		if(S[3,3] == 0) S[3,3] <- sign
		if(S[3,3] == 1 && sign == -1) S[3,3] <- sign
	}

	# GET ROTATION MATRIX
	# MIGHT CHANGE POINTS RELATIVE TO ONE ANOTHER (VERY SLIGHTLY)
	# I THINK IT ONLY HAPPENS WHEN ONE DIMENSION OF M1 IS ALL ZEROS
	# CAUSES PROBLEM IN DETERMINING FIT PERHAPS
	RM <- SVD$v %*% S %*% t(SVD$u)

	# CREATE ROTATION TRANSFORMATION MATRIX
	tmat2 <- diag(4);tmat2[1:3, 1:3] <- t(RM)

	# TEST ALIGNMENT
	#t2 <- m2c %*% RM
	#print(m1oc[!is.na(m1oc[, 1]), ])
	#print(t2[!is.na(t2[, 1]), ])

	# ROTATE ALL CENTER LANDMARKS IN M2
	m2r <- mtransform(m2c, tmat2)
	if(!is.null(m3)) m3r <- mtransform(m3c, tmat2)

	# TEST WHETHER CHIRALITY OF POINT SET HAS FLIPPED
	if(nrow(m1o) == 3 && nrow(m2) > 3){
		
		# IF THE ALIGNMENT FLIPPED THE SET TO ITS MIRROR IMAGE, THE CROSS PRODUCT OF THE 
		#	FIRST THREE POINTS WILL MAINTAIN THE SAME ORIENTATION. BUT ANY OTHER POINTS WILL
		#	BE FLIPPED RELATIVE TO THIS VECTOR. SO IF THE DISTANCE BETWEEN THE CROSS PRODUCT
		#	AND THESE POINTS CHANGES, IT INDICATES THE CHIRALITY HAS BEEN FLIPPED

		# FIND NORMAL VECTORS FOR PRE AND POST ROTATED SETS
		m2c_cprod <- uvector(cprod(m2c[2, ]-m2c[1, ], m2c[3, ]-m2c[1, ]))
		m2r_cprod <- uvector(cprod(m2r[2, ]-m2r[1, ], m2r[3, ]-m2r[1, ]))
		
		# FIND DISTANCE FROM CPROD VECTOR TO OTHER POINTS
		dpp <- distPointToPoint(m2c_cprod, m2c[4:min(7,nrow(m2c)), ])
		dpp_r <- distPointToPoint(m2r_cprod, m2r[4:min(7,nrow(m2r)), ])
		
		# CHIRALITY HAS FLIPPED, FLIP 3RD COLUMN OF SVD$v AND RE-TRANSFORM
		if(sum(round(abs(dpp - dpp_r), 7)) > 0.001){
			SVD$v[, 3] <- -SVD$v[, 3]
			RM <- SVD$v %*% S %*% t(SVD$u)
			tmat2[1:3, 1:3] <- t(RM)
			m2r <- mtransform(m2c, tmat2)
			if(!is.null(m3)) m3r <- mtransform(m3c, tmat2)
		}else{
		}

		#print(SVD$v)
		#print(S)
		#print(t(SVD$u))
		#cat('\n')
	}

	m2or <- mtransform(m2oc, tmat2)

	# CREATE SECOND TRANSLATION TRANSFORMATION MATRIX
	tmat3 <- diag(4);tmat3[1:3, 4] <- colMeans(m1o, na.rm=TRUE)

	# APPLY TRANSLATION PARAMETERS
	m2r <- mtransform(m2r, tmat3)
#	m2r <- m2r + matrix(colMeans(m1o, na.rm=TRUE), nrow=nrow(m2r), ncol=ncol(m2r), byrow=TRUE)
	if(!is.null(m3)) m3r <- mtransform(m3r, tmat3)

	# GET ALIGNMENT ERROR
	errors <- m1oc - m2or
	attr(errors, "scaled:center") <- NULL
	
	dist.errors <- matrix(sqrt(rowSums(errors^2)), ncol=1, dimnames=list(rownames(errors), NULL))

	# GET FINAL TRANSFORMATION MATRIX
	tmat <- tmat3 %*% tmat2 %*% tmat1
	
	list(
		mat=m2r,
		pos.errors=errors,
		dist.errors=dist.errors,
		mc=m3r,
		tmat=tmat
	)
}