fitShape <- function(mat, shape, centroid.align = NULL){

	## Fits a shape to a set of points
	
	# Remove NA values
	mat <- mat[!is.na(mat[,1]),]

	# Align coor by centroid, if not input
	if(shape %in% c('circle', 'plane')){
		if(is.null(centroid.align)) centroid.align <- mat  - matrix(colMeans(mat), nrow(mat), ncol(mat), byrow=TRUE)
	}
	
	if(shape == 'vector'){

		# Fit line to 3D points
		fit_line <- fitLine3D(mat)
		
		shape_obj <- list('V'=fit_line$v)
		class(shape_obj) <- 'vector'
		return(shape_obj)
	}

	if(shape == 'sphere') return(fitSphere(mat))

	if(shape == 'elliptic cylinder') return(fitEllipticCylinder(mat))

	if(shape == 'cylinder') return(fitCylinder(mat))

	if(shape == 'cylinders') return(fitCylinders(mat))

	if(shape == 'circle'){
		
		# https://meshlogic.github.io/posts/jupyter/curve-fitting/fitting-a-circle-to-cluster-of-3d-points/
		
		# Fit plane
		fit_plane <- fitShape(mat, 'plane', centroid.align=centroid.align)
		
		# If plane fit failed, return NULL
		if(is.null(fit_plane)) return(NULL)

		# Find angle between current normal and new projected plane normal
		n_avec <- avec(fit_plane$N, c(0,0,1))
		
		# Check that points are not already in plane
		if(n_avec > 1e-13){
			# Find axis of rotation for projecting points into xy-plane
			k <- cprod(fit_plane$N, c(0,0,1))
		}else{
			k <- c(1,0,0)
		}

		# Rotate points to align with xy-plane
		project <- centroid.align %*% tMatrixEP(k, n_avec)

		# Fit 2D circle to points projected into xy-plane to find best-fit radius
		fit_circle_2d <- fitCircle2D(project[, 1:2])
		
		# Rotate circle center back into original 3D plane and translate by original centroid
		center <- c(fit_circle_2d$C, 0) %*% tMatrixEP(k, -n_avec) + colMeans(mat)

		# Create circle
		shape_obj <- defineCircle(center=center, nvector=fit_plane$N, radius=fit_circle_2d$R, 
			redefine_center=FALSE)

		return(shape_obj)
	}

	if(shape == 'plane'){

		# Check for differences in point position
		if(mean(apply(centroid.align, 2, 'sd')) < 1e-10) return(NULL)

		# SVD of centroid-aligned points
		if(FALSE){

			svd_res <- svd(centroid.align)

			# SVD theory
			# Given input matrix m x n
			# U and V are unitary matrices, so w conjugate transpose of U (U*), U* %*% U is an 
			# nxn identity matrix
			#	print(round(Conj(t(svd_res$u)) %*% svd_res$u, 7))
			# and V* %*% V is also an nxn identity matrix
			#	print(round(Conj(t(svd_res$v)) %*% svd_res$v, 7))
			# The original matrix can be recovered by
			#	sigma <- diag(svd_res$d)
			#	print(svd_res$u %*% sigma %*% t(svd_res$v))
		

			# Create plane
			# 	Normal vector to plane that minimizes mean orthogonal distances of points to plane
			# 	Use centroid as point in plane
			shape_obj <- list(N=svd_res$v[, 3], Q=colMeans(mat, na.rm=TRUE))
			class(shape_obj) <- 'plane'

		}else{

			meanX <- apply(mat, 2, mean) 
			pca <- prcomp(mat)

			v <- matrix(NA, nrow=3, ncol=3)
			endpts <- array(NA, dim=c(2,3,3))
			side_len <- rep(NA, 3)

			for(i in 1:3){

				t <- range(pca$x[, i])

				# Get end points along axis
				endpts[1,,i] <- meanX + t[1]*pca$rotation[, i]
				endpts[2,,i] <- meanX + t[2]*pca$rotation[, i]

				# Get vector corresponding to axis
				v[i, ] <- uvector(endpts[2,,i]-endpts[1,,i])
		
				# Get half of length
				side_len[i] <- distPointToPoint(endpts[1,,i], endpts[2,,i])
			}

			half_side_len <- side_len / 2
			corners_center <- colMeans(rbind(endpts[,,1], endpts[,,2], endpts[,,3]))
			corners <- matrix(NA, 4, 3, byrow=TRUE)
			corners[1,] <- corners_center + half_side_len[1]*v[1,] + half_side_len[2]*v[2,]
			corners[2,] <- corners_center - half_side_len[1]*v[1,] + half_side_len[2]*v[2,]
			corners[3,] <- corners_center - half_side_len[1]*v[1,] - half_side_len[2]*v[2,]
			corners[4,] <- corners_center + half_side_len[1]*v[1,] - half_side_len[2]*v[2,]

			shape_obj <- list('N'=v[3, ], 'Q'=corners_center, 'center'=corners_center, 
				'corners'=corners, 'vectors'=v, 'len'=side_len[1:2], 'halflen'=side_len[1:2]/2)
			class(shape_obj) <- 'plane'
		}

		return(shape_obj)
	}

}