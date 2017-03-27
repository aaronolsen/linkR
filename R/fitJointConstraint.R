fitJointConstraint <- function(coor, type){

	# Fit single rotational axis constraint
	if(type == 'R'){

		# Circle parameters
		CoRs <- matrix(NA, dim(coor)[1], 3)
		AoRs <- matrix(NA, dim(coor)[1], 3)
		rads <- rep(NA, dim(coor)[1])

		# Fit a circle to the set of positions of each body point over time
		for(i in 1:dim(coor)[1]){

			# Align points by centroid
			centroid_align <- t(coor[i, , ]) - matrix(colMeans(t(coor[i, , ])), dim(coor)[3], dim(coor)[2], byrow=TRUE)

			# Fit circles to each point across iterations
			fit_circle_3d <- fitShape(t(coor[i, , ]), 'circle', centroid.align=centroid_align)

			# If point lies along axis of rotation, skip
			if(is.null(fit_circle_3d)) next
			
			# Save center, axis of rotation and radius
			CoRs[i, ] <- fit_circle_3d$C
			AoRs[i, ] <- fit_circle_3d$N
			rads[i] <- fit_circle_3d$R
		}
		
		# Remove NA rows
		AoRs <- AoRs[!is.na(AoRs[,1]), ]
		CoRs <- CoRs[!is.na(CoRs[,1]), ]
		rads <- rads[!is.na(rads)]

		# Find single CoR and AoR
		if(nrow(AoRs) == 1){
			CoR <- CoRs
			AoR <- AoRs
		}else{

			# Flip AoRs that are in opposite direction from first
			for(i in 2:nrow(AoRs)) if(avec(AoRs[1, ], AoRs[i, ]) > pi/2) AoRs[i, ] <- -AoRs[i, ]

			# Find average CoR and AoR, weighting each iteration by circle radius
			CoR <- colSums(CoRs*rads, na.rm=TRUE) / sum(rads, na.rm=TRUE)
			AoR <- uvector(colSums(AoRs*rads, na.rm=TRUE) / sum(rads, na.rm=TRUE))
		}

		# Find initial position as reference

		# 
		if(TRUE){
			svg.new(file='Test.html')

			svg.pointsC(t(coor[i, , ]), cex=3, col.stroke='green')
			svg.pointsC(circlePoint(fit_circle_3d, seq(0,2*pi,length=80)), cex=0, col.stroke='orange')
			svg.arrows(rbind(fit_circle_3d$C, fit_circle_3d$C+0.25*uvector(fit_circle_3d$N)), col='blue')

			svg.points(CoR, cex=3, col='orange')
			svg.arrows(rbind(CoR, CoR+AoR), col='orange')
		
			svg.arrows(rbind(c(1,0.2,0), c(1,0.2,0)+0.5*uvector(c(0.3,0.3,1))), col='purple')
			svg.close()
		}
	}

	# Align coordinates across all time points using generalized procrustes analysis
	cons_coor <- procAlign(coor)

	# Find mean shape scaled to mean centroid size
	print(cons_coor$mean.scaled)

}