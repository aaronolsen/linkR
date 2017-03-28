body_pose_error <- function(p, type, cons, param, coor, coor.compare, lcs = NULL, 
	check.joint.cons = FALSE, print.progress = FALSE){

	# Rotate about centroid 
	centroid_mat <- matrix(colMeans(coor), nrow(coor), ncol(coor), byrow=TRUE)
	coor <- coor - centroid_mat
	coor <- coor %*% rotationMatrixZYX(p[1:3])
	coor <- coor + centroid_mat
	
	# Translate
	coor <- coor + matrix(p[4:6], nrow(coor), ncol(coor), byrow=TRUE)

	# Run joint model
	anim <- animate_joint(type=type, cons=cons, param=param, coor=coor, lcs=lcs, 
		check.joint.cons=check.joint.cons, print.progress=print.progress)

	# Compare simulated coordinates to ideal
	sqrt(mean((anim$coor - coor.compare)^2))
}