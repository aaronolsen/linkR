intersectTriangles <- function(t1, t2, return.logical = FALSE){

	## Finds intersection of two triangles

	# Get normal vectors
	n1 <- cprod(t1[2,]-t1[1,], t1[3,]-t1[1,])
	n2 <- cprod(t2[2,]-t2[1,], t2[3,]-t2[1,])

	# Find intersection of two planes
	inp <- intersectPlanes(t1[1,], n1, t2[1,], n2)
	
	# TF whether line intersects each triangle
	intersecting <- rep(FALSE, 2)
	
	# Save intersecting points
	int_pts <- matrix(NA, nrow=4, 3)
	int_ct <- 1

	if(is.null(inp)){
		if(return.logical) return(FALSE)
		return(NULL)
	}

	#
	for(tri_num in 1:2){
		
		if(tri_num == 1){ tri <- t1 }else{ tri <- t2 }

		# For each edge of each triangle
		for(i in 1:3){

			#
			#print(tri[i,])
			
			# Set vertices of edge
			if(i == 1) edge <- c(1,2)
			if(i == 2) edge <- c(2,3)
			if(i == 3) edge <- c(1,3)
			
			# Find whether line intersects with any edge of triangle
			ei <- intersectLines(inp$p, inp$v, tri[edge[1],], tri[edge[2],]-tri[edge[1],])

			if(FALSE){
				svg.new(paste0('/Users/aaron/Downloads/', tri_num, '-', i, '.html'), mode='webgl')
				svg.triangle(t1, col='purple')
				svg.spheres(t1, col='purple', radius=0.5)
				svg.triangle(t2, col='orange')
				svg.spheres(t2, col='orange', radius=0.5)
				svg.lines(rbind(inp$p, inp$p + inp$v))
				svg.spheres(ei, col='red', radius=0.1)
				svg.close()
			}

			# Check whether intersection is on edge
			if(!is.null(ei)){

				# Check whether intersection is on edge line
				dpl <- distPointToLine(ei, tri[edge[1],], tri[edge[2],])
				#print(dpl)

				# Skip if point is not on line
				if(dpl > 1e-10) next
				
				# Check if intersection is at vertex
				if(sqrt(sum((ei - tri[edge[1],])^2)) < 1e-10 || sqrt(sum((ei - tri[edge[2],])^2)) < 1e-10){
				
					# Intersection point: ei
					intersecting[tri_num] <- TRUE
					
					#if(return.logical) next

					# Add intersecting point
					int_pts[int_ct, ] <- ei
					int_ct <- int_ct + 1
				
					# Update line point to intersection
					inp$p <- inp$l1 <- ei
					inp$l2 <- ei + inp$v
					
					next
				}
				
				# Check whether intersection point is between two vertices of edge
				if(ptBnPts(ei, tri[edge[1],], tri[edge[2],])){

					# Intersection point: ei
					intersecting[tri_num] <- TRUE
					
					#if(return.logical) next

					# Add intersecting point
					int_pts[int_ct, ] <- ei
					int_ct <- int_ct + 1
				
					# Update line point to intersection
					inp$p <- inp$l1 <- ei
					inp$l2 <- ei + inp$v
				}
			}
		}

		if(return.logical && !intersecting[tri_num]) return(FALSE)

		#if(!intersecting[tri_num]) return(list(lp=inp$p, lv=inp$v, ip=int_pts))
	}

	# 
	inter <- FALSE
	
	# Set whether intersecting
	#if(sum(intersecting) == 2){ inter <- TRUE }else{ inter <- FALSE }
	
	# Remove NAs
	int_pts <- int_pts[!is.na(int_pts[,1]), ]

	if(nrow(int_pts) == 0){
		int_pts <- NULL
	}else if(nrow(int_pts) == 2){
		inp$l1 <- int_pts[1,]
		inp$l2 <- int_pts[2,]
	}else if(nrow(int_pts) == 4){

		# Test whether either point from t2 is between t1 points
		if(ptBnPts(int_pts[3,], int_pts[1,], int_pts[2,]) && ptBnPts(int_pts[4,], int_pts[1,], int_pts[2,])){
			int_pts <- int_pts[1:2,]
			inter <- TRUE
		}else if(ptBnPts(int_pts[1,], int_pts[3,], int_pts[4,]) && ptBnPts(int_pts[2,], int_pts[3,], int_pts[4,])){
			int_pts <- int_pts[3:4,]
			inter <- TRUE
		}else if(ptBnPts(int_pts[3,], int_pts[1,], int_pts[2,]) && !ptBnPts(int_pts[4,], int_pts[1,], int_pts[2,])){
			d12o <- distPointToPoint(int_pts[1:2,], int_pts[4,])
			int_pts <- int_pts[c(3, c(1:2)[which.min(d12o)]), ]
			inter <- TRUE
		}else if(!ptBnPts(int_pts[3,], int_pts[1,], int_pts[2,]) && ptBnPts(int_pts[4,], int_pts[1,], int_pts[2,])){
			d12o <- distPointToPoint(int_pts[1:2,], int_pts[3,])
			int_pts <- int_pts[c(4, c(1:2)[which.min(d12o)]), ]
			inter <- TRUE
		}else if(ptBnPts(int_pts[1,], int_pts[3,], int_pts[4,]) && !ptBnPts(int_pts[2,], int_pts[3,], int_pts[4,])){
			d12o <- distPointToPoint(int_pts[3:4,], int_pts[2,])
			int_pts <- int_pts[c(1, c(3:4)[which.min(d12o)]), ]
			inter <- TRUE
		}else if(!ptBnPts(int_pts[1,], int_pts[3,], int_pts[4,]) && ptBnPts(int_pts[2,], int_pts[3,], int_pts[4,])){
			d12o <- distPointToPoint(int_pts[3:4,], int_pts[1,])
			int_pts <- int_pts[c(2, c(3:4)[which.min(d12o)]), ]
			inter <- TRUE
		}
	}
	
	if(return.logical) return(inter)

	return(list(i=inter, lp=inp$p, lv=inp$v, l1=inp$l1, l2=inp$l2, ip=int_pts))
}