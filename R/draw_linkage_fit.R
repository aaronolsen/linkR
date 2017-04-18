draw_linkage_fit <- function(file, ref.coor, model.coor, fit.markers, path.connect, window.title, 
	cex.model=4, cex.ref=2, path.close=FALSE){

	# Set number of iterations
	n_iter <- dim(ref.coor)[3]

	# Find centroid of model coordinates over time
	model_centroid <- array(apply(model.coor, 3, 'colMeans'), dim=c(1,3,n_iter))

	fixed_opa <- 0.1
	mobile_opa <- 0.3
	fixed_stroke_opa <- 0.01
	mobile_stroke_opa <- 0.05

	svg.new(file, window.title=window.title)

	# Get paths to connect
	read_lines <- readLines(path.connect)
	plist_names <- list()
	for(line_num in 1:length(read_lines)) plist_names[[length(plist_names)+1]] <- strsplit(read_lines[line_num], ',[ ]?')[[1]]

	# Bodies not fit
	model_coor <- model.coor[grepl('^(SuspensoriumL|CleithrumL|HyoidL)', dimnames(model.coor)[[1]]), , ]

	svg.points(model_coor, col.stroke='gray', col.fill='gray', cex=0)

	# Draw model paths
	for(i in 1:length(plist_names)){

		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(model_coor)[[1]]]
		if(length(path_names) > 0){
		
			d <- c()
			for(j in 1:length(path_names)) d <- c(d, which(path_names[j] == dimnames(model_coor)[[1]]))

			svg.pathsC(path=d, col.stroke='red', opacity.fill=mobile_opa, col.fill='red', 
				opacity.stroke=mobile_stroke_opa, lwd=2)
		}
	}
	
	# Bodies fit
	ref_coor_model <- ref.coor[grepl('^(SuspensoriumL|CleithrumL|HyoidL)', dimnames(ref.coor)[[1]]), , ]

	svg.points(ref_coor_model, col.stroke='gray', col.fill='gray', cex=0)

	# Draw ref coordinate paths
	d_add <- dim(model_coor)[1]
	for(i in 1:length(plist_names)){

		path_names <- plist_names[[i]][plist_names[[i]] %in% dimnames(ref_coor_model)[[1]]]
		if(length(path_names) > 0){
		
			d <- c()
			for(j in 1:length(path_names)) d <- c(d, which(path_names[j] == dimnames(ref_coor_model)[[1]]))

			svg.pathsC(path=d+d_add, col.stroke='blue', opacity.fill=mobile_opa, col.fill='blue', 
				opacity.stroke=mobile_stroke_opa, lwd=2)
		}
	}

	# Bodies not fit
	ref_coor_only <- ref.coor[grepl('^(Neurocranium|Urohyal|PostTemporalL|PostTemporalR)', dimnames(ref.coor)[[1]]), , ]

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
	
	svg.points(model.coor[fit.markers, , ], col.fill='none', col.stroke='black', cex=4)
	svg.points(ref.coor[fit.markers, , ], cex=2)

	# Draw in "sternohyoid"
	svg.pointsC(model.coor[c('HyoidL_hypohyal_midline_symphysis_cra', 'CleithrumL_ventral_midline_cra'), , ], 
		col.fill.C='none', opacity.stroke.C=mobile_opa, col.stroke.C='red', cex=0, lwd.C=10, z.index.C=-1)

	# Draw frame
	svg.frame(ref.coor, z.index=-2)

	svg.close()
}