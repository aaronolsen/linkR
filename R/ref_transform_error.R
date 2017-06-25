ref_transform_error <- function(p, ref.points, fit.points, transform, fit.wts){

	# Create test transformation matrix from p
	tmat <- diag(4)
	tmat[1:3, 1:3] <- rotationMatrixZYX(p[1:3])
	tmat[1:3, 4] <- p[4:6]

	# Apply test transformation to reference points
	ref.points <- applyTransform(ref.points, tmat)	

	# Apply transformation array to test transformed reference points
	ref.points <- applyTransform(ref.points, transform)	

	# Return error
	return(sqrt(mean(fit.wts*(ref.points - fit.points)^2)))
}