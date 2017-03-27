alignByCentroid <- function(coor){

	## Aligns an array of body coordinates by their centroid

	# Get centroid of shape at each iteration
	centroids <- t(apply(coor, 3, colMeans, na.rm=TRUE))

	# Convert coordinate array into matrix
	cmat <- matrix(coor, nrow=dim(coor)[1], ncol=dim(coor)[2]*dim(coor)[3])

	# Align all iterations by centroid
	cmat_align <- cmat - matrix(t(centroids), nrow=nrow(cmat), ncol=ncol(cmat), byrow=TRUE)

	# Convert back to array
	carr_align <- array(cmat_align, dim=dim(coor))

	carr_align
}