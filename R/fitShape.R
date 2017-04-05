fitShape <- function(mat, shape, centroid.align = NULL){

	## Fits a shape to a set of points

	# Align coor by centroid, if not input
	if(shape %in% c('circle', 'plane')){
		if(is.null(centroid.align)) centroid.align <- mat  - matrix(colMeans(mat, na.rm=TRUE), nrow(mat), ncol(mat), byrow=TRUE)
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

		return(shape_obj)
	}

}