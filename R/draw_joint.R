draw_joint <- function(file, animate.joint, fixed, fixed.lcs, col=c('red', 'green', 'blue'), 
	cons.col.r=c('purple', 'MediumPurple', 'Magenta'), cons.col.t=c('blue', 'teal', 'aqua'), 
	cex=c(7,3), lwd=1, draw.lcs = TRUE, 
	animate = TRUE, arrow.len=1, head.len='auto', animate.duration=4, window.title='draw_joint', 
	animate.reverse = TRUE){

	# Standardize as list of animate.joint outputs
	if(!is.null(names(animate.joint))) animate.joint <- list(animate.joint)
	
	# Number of joint animations
	n_animate <- 0
	for(i in 1:length(animate.joint)) if(!is.null(animate.joint[[i]])) n_animate <- n_animate + 1

	# Create svg file
	svg.new(file, animate.reverse=animate.reverse, animate.duration=animate.duration, window.title=window.title)

	# Draw fixed body
	svg.pointsC(fixed, close=TRUE, col.fill=col, col.stroke='none', cex=cex[2])

	# Draw fixed body center
	svg.pointsC(colMeans(fixed), cex=cex[2])

	# Draw fixed body coordinate system
	if(draw.lcs) draw_lcs(fixed.lcs, arrow.len=arrow.len, head.len=head.len)

	# Is frame drawn?
	frame_drawn <- FALSE

	# Draw each animate joint output (e.g. actual and model)
	for(i in 1:length(animate.joint)){
		
		if(is.null(animate.joint[[i]])) next

		# Set draw properties
		if(n_animate > 1 && i == 1) {opacity <- 0.5; stroke_col <- 'black'; col_fill_center <- 'black'; col_fill <- col; cexi <- cex[2]}
		if((n_animate > 1 && i == 2)) {opacity <- 1; stroke_col <- 'orange'; col_fill <- 'none'; col_fill_center <- 'none'; cexi <- cex[1]}
		if(n_animate == 1) {opacity <- 1; stroke_col <- 'black'; col_fill <- 'none'; col_fill_center <- 'none'; cexi <- cex[1]}

		jt_cons <- animate.joint[[i]]$cons
		mobile <- animate.joint[[i]]$coor
		mobile_lcs <- animate.joint[[i]]$lcs
		dof <- animate.joint[[i]]$dof
		
		# Add center point to mobile
		mobile_n <- array(NA, dim=c(dim(mobile)[1]+1, dim(mobile)[2:3]))
		for(j in 1:dim(mobile)[3]){
			mobile_n[1, , j] <- colMeans(mobile[, , j], na.rm=TRUE)
			mobile_n[2:dim(mobile_n)[1], , j] <- mobile[, , j]
		}
		mobile <- mobile_n

		# Get number of iterations
		n_iter <- dim(mobile)[3]

		# Draw frame
		if(!frame_drawn){

			# All points
			all_points <- rbind(fixed, apply(mobile, 2, 'matrix', byrow=TRUE))

			# Draw frame
			svg.frame(all_points, z.index=-1)
			
			frame_drawn <- TRUE
		}

		if(animate){
			# Draw mobile body
			svg.pointsC(mobile[2:4, , ], close=TRUE, col.fill=col_fill, col.stroke=col, opacity.stroke=opacity, opacity.stroke.C=opacity, col.stroke.C=stroke_col, cex=cexi)

			# Draw mobile body centers
			svg.pointsC(array(mobile[1, , ], dim=c(1,3,dim(mobile)[3])), col.fill=col_fill_center, opacity.stroke=opacity, cex=cexi, label='Mobile')
		}else{
			for(iter in 1:n_iter){

				# Draw mobile body
				svg.pointsC(mobile[2:4, , iter], close=TRUE, col.fill=col_fill, col.stroke=col, opacity.stroke=opacity, opacity.stroke.C=opacity, col.stroke.C=stroke_col, cex=cexi)

				# Draw mobile body centers
				svg.pointsC(array(mobile[1, , iter], dim=c(1,3,dim(mobile)[3])), col.fill=col_fill_center, opacity.stroke=opacity, cex=cexi, label='Mobile')
			}
		}

		# Trace body center trajectory
		svg.lines(t(mobile[1, , ]), lwd=lwd, opacity=opacity, col=stroke_col)

		# Draw mobile body coordinate systems
		if(draw.lcs) draw_lcs(mobile_lcs, arrow.len=arrow.len, head.len=head.len)
	
		# Draw rotational constraints
		arrow_arr <- array(NA, dim=c(2,3,n_iter))
		if(dof[1] > 0){

			# Draw rotational axis
			for(j in 1:dof[1]){
				if(animate){
					for(iter in 1:n_iter) arrow_arr[, , iter] <- rbind(jt_cons[iter, 1:3], jt_cons[iter, 1:3]+0.75*arrow.len*uvector(jt_cons[iter, (j*3+1):(j*3+3)]))
					svg.arrows(arrow_arr, lwd=lwd, col=cons.col.r[j], len=head.len, opacity=opacity)
				}else{
					for(iter in 1:n_iter){
						svg.arrows(rbind(jt_cons[iter, 1:3], jt_cons[iter, 1:3]+0.75*arrow.len*uvector(jt_cons[iter, (j*3+1):(j*3+3)])), 
							lwd=lwd, col=cons.col.r[j], len=head.len, opacity=opacity)
					}
				}
			}

			# Draw point at center of rotation			
			svg.pointsC(array(t(jt_cons[, 1:3]), dim=c(1,3,nrow(jt_cons))), cex=cex[2], col=cons.col.r[1])

			# Draw path of center of rotation
			svg.lines(jt_cons[, 1:3], lwd=lwd, col=cons.col.r[1])
		}

		# Draw translational constraints
		if(dof[2] > 0){
			if(dof[1] == 0){j_add <- 0}else{j_add <- (dof[1]+1)*3}
			for(j in 1:dof[2]){
				for(iter in 1:n_iter) arrow_arr[, , iter] <- rbind(mobile_lcs[1, , iter], mobile_lcs[1, , iter]+0.75*arrow.len*uvector(jt_cons[iter, ((j-1)*3+1+j_add):((j-1)*3+3+j_add)]))
				svg.arrows(arrow_arr, lwd=1, col=cons.col.t[j], len=head.len, opacity=opacity)
			}
		}
	}

	svg.close()
}