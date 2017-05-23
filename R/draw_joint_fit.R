draw_joint_fit <- function(file, ref.coor, model.coor, model.cons, joint.type, path.connect, 
	window.title, cex.model=4, cex.ref=2, arrow.len=NULL, path.close=FALSE){

	# Set number of iterations
	n_iter <- dim(ref.coor)[3]

	# Get joint constraints
	jt_cons <- model.cons

	# Find centroid of model coordinates over time
	model_centroid <- array(apply(model.coor, 3, 'colMeans'), dim=c(1,3,n_iter))

	# Get centroid size of all points
	if(is.null(arrow.len)){
		arrow_len <- centroidSize(ref.coor[, , 1]) / 10
	}else{
		arrow_len <- arrow.len
	}

	fixed_opa <- 0.1
	mobile_opa <- 0.3
	fixed_stroke_opa <- 0.01
	mobile_stroke_opa <- 0.05

	svg.new(file, window.title=window.title)

	# Get paths to connect
	read_lines <- readLines(path.connect)
	plist_names <- list()
	for(line_num in 1:length(read_lines)) plist_names[[length(plist_names)+1]] <- strsplit(read_lines[line_num], ',[ ]?')[[1]]

	svg.points(model.coor, col.stroke='gray', col.fill='gray', cex=0)

	# Draw model paths
	for(i in 1:length(plist_names)){

		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(model.coor)[[1]]]
		if(length(path_names) > 0){
		
			d <- c()
			for(j in 1:length(path_names)) d <- c(d, which(path_names[j] == dimnames(model.coor)[[1]]))

			svg.pathsC(path=d, col.stroke='red', opacity.fill=mobile_opa, col.fill='red', 
				opacity.stroke=mobile_stroke_opa, lwd=2)
		#	svg.pointsC(model.coor[path_names, , ], cex=0, col.stroke.C='red', 
		#		opacity.fill.C=mobile_opa, col.fill.C='red', 
		#		opacity.stroke.C=mobile_opa+0.2, col.stroke='red', lwd.C=2, close=path.close)
		}
	}
	
	# Points overlapping between ref and model
	ref_coor_model <- ref.coor[dimnames(model.coor)[[1]], , ]

	svg.points(ref_coor_model, col.stroke='gray', col.fill='gray', cex=0)

	# Draw ref coordinate paths
	d_add <- dim(model.coor)[1]
	for(i in 1:length(plist_names)){

		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(ref_coor_model)[[1]]]
		if(length(path_names) > 0){
		
			d <- c()
			for(j in 1:length(path_names)) d <- c(d, which(path_names[j] == dimnames(model.coor)[[1]]))

			svg.pathsC(path=d+d_add, col.stroke='blue', opacity.fill=mobile_opa, col.fill='blue', 
				opacity.stroke=mobile_stroke_opa, lwd=2)
		}
	}

	# Ref points only
	ref_coor_only <- ref.coor[!dimnames(ref.coor)[[1]] %in% dimnames(model.coor)[[1]], , ]

	svg.points(ref_coor_only, col.stroke='gray', col.fill='gray', cex=0)

	# Draw paths
	d_add <- d_add + dim(ref_coor_model)[1]
	for(i in 1:length(plist_names)){

		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(ref_coor_only)[[1]]]
		if(length(path_names) > 0){
		
			d <- c()
			for(j in 1:length(path_names)) d <- c(d, which(path_names[j] == dimnames(ref_coor_only)[[1]]))

			svg.pathsC(path=d+d_add, col.stroke='black', opacity.fill=fixed_opa, col.fill='black', 
				opacity.stroke=fixed_stroke_opa, lwd=2)
		}
	}

	diag_3 <- diag(3)
	if(FALSE){

		if(joint.type %in% c('L', 'P', 'T')) svg.points(model_centroid, col='orange', cex=3)
		if(joint.type == 'T') for(i in 1:3) svg.arrows(rbind(model_centroid[, , 1]-arrow_len*diag_3[i,], model_centroid[, , 1]+arrow_len*diag_3[i,]), col='orange', lwd=2)
		if(joint.type == 'L'){
			svg.arrows(rbind(model_centroid[, , 1]-arrow_len*jt_cons[1, 1:3], 
				model_centroid[, , 1]+arrow_len*jt_cons[1, 1:3]), col='orange', lwd=2, len=arrow_len/5)
		}
		if(joint.type == 'P'){
			svg.arrows(rbind(model_centroid[, , 1]-arrow_len*jt_cons[1, 1:3], 
				model_centroid[, , 1]+arrow_len*jt_cons[1, 1:3]), col='orange', lwd=2, len=arrow_len/5)
			svg.arrows(rbind(model_centroid[, , 1]-arrow_len*jt_cons[1, 4:6], 
				model_centroid[, , 1]+arrow_len*jt_cons[1, 4:6]), col='orange', lwd=2, len=arrow_len/5)
		}
	}

	if(joint.type %in% c('R', 'U', 'S')) svg.points(jt_cons[1, 1:3], col='green', cex=3)
	if(joint.type %in% c('R', 'U')){
		svg.arrows(rbind(jt_cons[1, 1:3]-arrow_len*jt_cons[1, 4:6], 
			jt_cons[1, 1:3]+arrow_len*jt_cons[1, 4:6]), col='green', lwd=2)
	}
	if(joint.type == 'U'){
		arrow_arr <- array(NA, dim=c(2,3,n_iter))
		for(iter in 1:n_iter) arrow_arr[, , iter] <- rbind(jt_cons[iter, 1:3]-arrow_len*uvector(jt_cons[iter, 7:9]), jt_cons[iter, 1:3]+arrow_len*uvector(jt_cons[iter, 7:9]))
		svg.arrows(arrow_arr, col='LimeGreen', lwd=2)
	}
	if(joint.type %in% c('S')){
		for(i in 1:3) svg.arrows(rbind(jt_cons[1, 1:3]-arrow_len*diag_3[i,], jt_cons[1, 1:3]+arrow_len*diag_3[i,]), col='green', lwd=2)
	}



#	svg.pointsC(t(model.coor['LowerJawL_symphysis', , ]), col.stroke.C='red', col.stroke='red', 
#		col.fill.C='none', cex=0, opacity.stroke.C=0.3, close=TRUE, lwd.C=2)

#	svg.pointsC(t(model.coor['LowerJawL_symphysis', , ]), col.stroke.C='blue', col.stroke='blue', 
#		col.fill.C='none', cex=0, opacity.stroke.C=0.9, close=TRUE, lwd.C=2)

#	svg.pointsC(ref.coor, col.stroke.C='black', col.stroke='black', col.fill.C='none', close=TRUE, lwd.C=2)

	#svg.points(model.coor, col.stroke='red', col.fill='none', cex=cex.model)
	#svg.points(ref.coor, col.stroke='black', col.fill='black', cex=cex.ref)

	# Draw frame
#	svg.frame(ref.coor, z.index=-1)
	svg.box(ref.coor, z.index=-1, sides=c(1,3,5), tick.axes=c(2,3,1), tick.labels=c(2,3,1))

	svg.close()
}