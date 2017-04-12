tmmat2arr <- function(mat){

	## Converts transformation matrix into array of transformation matrices

	# Get number of iterations
	n_iter <- nrow(mat)
	
	# Get number of bodies
	n_bodies <- ncol(mat) / 16
	
	# Get body names
	body_names <- unique(gsub('_(R[0-3]{2}|[0-3]{2}|TX|TY|TZ|1)$', '', colnames(mat)))
	
	# Convert to array
	tarr <- array(t(mat), dim=c(4,4,n_bodies,n_iter), dimnames=list(NULL, NULL, body_names, NULL))
	
	tarr
}
