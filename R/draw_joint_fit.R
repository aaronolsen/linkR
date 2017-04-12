draw_joint_fit <- function(file, ref.coor, model.coor, model.cons, joint.type, path.connect, 
	window.title, cex.model=4, cex.ref=2){

	# Set number of iterations
	n_iter <- dim(ref.coor)[3]

	# Get joint constraints
	jt_cons <- model.cons

	# Find centroid of model coordinates over time
	model_centroid <- array(apply(model.coor, 3, 'colMeans'), dim=c(1,3,n_iter))

	# Get centroid size of all points
	csize <- centroidSize(ref.coor[, , 1]) / 10

	low_opa <- 0.3
	high_opa <- 0.8

	svg.new(file, window.title=window.title)

	# Draw frame
	svg.frame(ref.coor)

	#svg.points(ref.coor, opacity.stroke=low_opa, opacity.fill=low_opa)

	diag_3 <- diag(3)
	if(joint.type %in% c('L', 'P', 'T')) svg.points(model_centroid, col='blue', cex=3)
	if(joint.type == 'T') for(i in 1:3) svg.arrows(rbind(model_centroid[, , 1]-csize*diag_3[i,], model_centroid[, , 1]+csize*diag_3[i,]), col='blue', lwd=2)
	if(joint.type == 'L'){
		svg.arrows(rbind(model_centroid[, , 1]-csize*jt_cons[1, 1:3], 
			model_centroid[, , 1]+csize*jt_cons[1, 1:3]), col='blue', lwd=2, len=csize/5)
	}
	if(joint.type == 'P'){
		svg.arrows(rbind(model_centroid[, , 1]-csize*jt_cons[1, 1:3], 
			model_centroid[, , 1]+csize*jt_cons[1, 1:3]), col='blue', lwd=2, len=csize/5)
		svg.arrows(rbind(model_centroid[, , 1]-csize*jt_cons[1, 4:6], 
			model_centroid[, , 1]+csize*jt_cons[1, 4:6]), col='blue', lwd=2, len=csize/5)
	}
	if(joint.type %in% c('R', 'U', 'S')) svg.points(jt_cons[1, 1:3], col='purple', cex=3)
	if(joint.type %in% c('R', 'U')){
		svg.arrows(rbind(jt_cons[1, 1:3]-csize*jt_cons[1, 4:6], 
			jt_cons[1, 1:3]+csize*jt_cons[1, 4:6]), col='purple', lwd=2)
	}
	if(joint.type == 'U'){
		arrow_arr <- array(NA, dim=c(2,3,n_iter))
		for(iter in 1:n_iter) arrow_arr[, , iter] <- rbind(jt_cons[iter, 1:3]-csize*uvector(jt_cons[iter, 7:9]), jt_cons[iter, 1:3]+csize*uvector(jt_cons[iter, 7:9]))
		svg.arrows(arrow_arr, col='MediumPurple', lwd=2)
	}
	if(joint.type %in% c('S')){
		for(i in 1:3) svg.arrows(rbind(jt_cons[1, 1:3]-csize*diag_3[i,], jt_cons[1, 1:3]+csize*diag_3[i,]), col='purple', lwd=2)
	}

	svg.points(model.coor, col.stroke='orange', col.fill='none', cex=cex.model)
	svg.points(ref.coor, col.stroke='black', col.fill='black', cex=cex.ref)

#	svg.pointsC(ref.coor, col.stroke.C='black', col.stroke='black', col.fill.C='none', close=TRUE, lwd.C=2)

	# Get paths to connect
	read_lines <- readLines(path.connect)
	plist_names <- list()
	for(line_num in 1:length(read_lines)) plist_names[[length(plist_names)+1]] <- strsplit(read_lines[line_num], ',[ ]?')[[1]]

	# Draw paths
	for(i in 1:length(plist_names)){
		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(ref.coor)[[1]]]
		if(length(path_names) > 0){

			if(sum(path_names %in% dimnames(model.coor)[[1]]) > 0){
				svg.pointsC(ref.coor[path_names, , ], cex=0, opacity.stroke.C=high_opa, lwd.C=2)
			}else{
				svg.pointsC(ref.coor[path_names, , ], cex=0, opacity.stroke.C=low_opa)
			}
		}

		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(model.coor)[[1]]]
		if(length(path_names) > 0){
			svg.pointsC(model.coor[path_names, , ], cex=0, col.stroke.C='orange', col.fill.C='none', 
				opacity.stroke.C=high_opa, col.stroke='orange', lwd.C=2)
		}
	}

	svg.close()
}