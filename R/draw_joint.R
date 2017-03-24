draw_joint <- function(file, animate.joint, fixed, fixed.lcs, col=c('red', 'green', 'blue'), 
	cons.col=c('purple', 'MediumPurple', 'Magenta'), cex=3, lwd=1, arrow.len=1, head.len='auto', 
	animate.duration=4, window.title='draw_joint', animate.reverse = TRUE){

	jt_cons <- animate.joint$cons
	mobile <- animate.joint$coor[1:4, , ]
	mobile_lcs <- animate.joint$lcs
	dof <- animate.joint$dof
	
	# Get number of iterations
	n_iter <- dim(mobile)[3]

	# All points
	all_points <- rbind(fixed, apply(mobile, 2, 'matrix', byrow=TRUE))

	# Create svg file
	svg.new(file, animate.reverse=animate.reverse, animate.duration=animate.duration, window.title=window.title)

	# Draw frame
	svg.frame(all_points)

	# Draw triangles (bodies)
	svg.pointsC(fixed[2:4, ], close=TRUE, col.fill=col, col.stroke='none', cex=cex)
	svg.pointsC(mobile[2:4, , ], close=TRUE, col.fill=col, col.stroke='none', cex=cex)

	# Draw body centers
	svg.pointsC(fixed[1, ], cex=cex)
	svg.pointsC(array(mobile[1, , ], dim=c(1,3,dim(mobile)[3])), cex=cex)

	# Trace body center trajectory
	svg.lines(t(mobile[1, , ]), lwd=lwd)

	# Draw body coordinate systems
	draw_lcs(fixed.lcs, arrow.len=arrow.len, head.len=head.len)
	draw_lcs(mobile_lcs, arrow.len=arrow.len, head.len=head.len)
	
	# Draw rotational constraints
	arrow_arr <- array(NA, dim=c(2,3,n_iter))
	if(dof[1] > 0){

		# Draw rotational axis
		for(j in 1:dof[1]){
			for(iter in 1:n_iter) arrow_arr[, , iter] <- rbind(jt_cons[iter, 1:3], jt_cons[iter, 1:3]+0.75*arrow.len*uvector(jt_cons[iter, (j*3+1):(j*3+3)]))
			svg.arrows(arrow_arr, lwd=lwd, col=cons.col[j], len=head.len)
		}

		# Draw point at center of rotation			
		svg.pointsC(array(t(jt_cons[, 1:3]), dim=c(1,3,nrow(jt_cons))), cex=cex, col='purple')

		# Draw path of center of rotation
		svg.lines(jt_cons[, 1:3], lwd=lwd, col='purple')
	}

	if(dof[2] > 0){
		if(dof[1] == 0){j_add <- 0}else{j_add <- (dof[1]+1)*3}
		for(j in 1:dof[2]){
			for(iter in 1:n_iter) arrow_arr[, , iter] <- rbind(mobile_lcs[1, , iter], mobile_lcs[1, , iter]+0.75*arrow.len*uvector(jt_cons[iter, ((j-1)*3+1+j_add):((j-1)*3+3+j_add)]))
			svg.arrows(arrow_arr, lwd=1, col=cons.col[j], len=head.len)
		}
	}

	svg.close()
}