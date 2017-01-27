drawLinkageFit <- function(file, fit, window.title = NULL, frame = TRUE, 
	animate=TRUE, animate.duration = 3, opacity.stroke.C = NULL, lwd.C = 2, cex = 2, 
	show.origin=TRUE){
	
	invivo <- fit$ref.coor
	model <- fit$fit.coor
	fixed_link <- fit$fixed.link
	invivo.params <- fit$ref.linkage
	model.params <- fit$fit.linkage

	# Set file name
	if(!grepl('[.]html$', file, ignore.case=TRUE)) file <- paste0(file, '.html')

	# Set window name
	if(is.null(window.title)) window.title <- fit$model

	model_col <- 'blue' #rgb(0.5,0.5,1)
	invivo_col <- 'black'

	opacity_stroke_C <- opacity.stroke.C
	lwd_C <- lwd.C

	# If multiple individual sets, put all together into single array
	if(length(dim(invivo)) == 4){

		# Number of sets
		num_sets <- dim(invivo)[4]

		# Increase animation time		
		animate.duration <- animate.duration*num_sets

		# Create arrays
		invivo_new <- array(NA, dim=c(dim(invivo)[1:2], dim(invivo)[3]*num_sets), dimnames=list(dimnames(invivo)[[1]], dimnames(invivo)[[2]], NULL))
		model_new <- array(NA, dim=c(dim(model)[1:2], dim(model)[3]*num_sets), dimnames=list(dimnames(invivo)[[1]], dimnames(model)[[2]], NULL))

		# Fill arrays
		for(i in 1:num_sets) invivo_new[, , ((i-1)*dim(invivo)[3]+1):((i-1)*dim(invivo)[3]+dim(invivo)[3])] <- invivo[, , , i]
		for(i in 1:num_sets) model_new[, , ((i-1)*dim(model)[3]+1):((i-1)*dim(model)[3]+dim(model)[3])] <- model[, , , i]

		invivo <- invivo_new
		model <- model_new

		if(nrow(invivo.params[[1]]$RA[[1]]) > 1){
			ip_new <- list('RA'=invivo.params[[1]]$RA)
			for(i in 2:num_sets) for(j in 1:2) ip_new[['RA']][[j]] <- rbind(ip_new[['RA']][[j]], invivo.params[[i]]$RA[[j]])
			invivo.params <- ip_new
		}else{
		}

		if(nrow(model.params[[1]]$RA[[1]]) > 1){
			mp_new <- list('RA'=model.params[[1]]$RA)
			for(i in 2:num_sets) for(j in 1:2) mp_new[['RA']][[j]] <- rbind(mp_new[['RA']][[j]], model.params[[i]]$RA[[j]])
		}else{
			
			if(is.na(model.params[[1]]$RA[[1]][1,1])){
				#print(model.params)

				mp_new <- list('AJ'=model.params[[1]]$AJ, 'RI'=model.params[[1]]$RI)
				for(i in 2:num_sets){
					for(j in 1:length(mp_new[['AJ']])) mp_new[['AJ']][[j]] <- rbind(mp_new[['AJ']][[j]], model.params[[i]]$AJ[[j]])
					for(j in 1:length(mp_new[['RI']])) mp_new[['RI']][[j]] <- rbind(mp_new[['RI']][[j]], model.params[[i]]$RI[[j]])
				}

				# Check that AJ and RI are the same
				for(i in 1:length(mp_new[['AJ']])){
					if(sum(apply(mp_new[['AJ']][[i]], 2, 'sd')) > 1e-10) stop('AJ vectors are not the same.')
					mp_new[['AJ']][[i]] <- matrix(mp_new[['AJ']][[i]][1, ], nrow=1, ncol=3)
				}

				for(i in 1:length(mp_new[['RI']])){
					if(sum(apply(mp_new[['RI']][[i]], 2, 'sd')) > 1e-10) stop('RI vectors are not the same.')
					mp_new[['RI']][[i]] <- matrix(mp_new[['RI']][[i]][1, ], nrow=1, ncol=3)
				}
				mp_new[['RA']] <- model.params[[1]]$RA

			}else{
				#
				mp_new <- list('RA'=model.params[[1]]$RA)
				for(i in 2:num_sets) for(j in 1:2) mp_new[['RA']][[j]] <- rbind(mp_new[['RA']][[j]], model.params[[i]]$RA[[j]])
				for(i in 1:length(mp_new[['RA']])){
					# Check that axes are the same
					if(sum(apply(mp_new[['RA']][[i]], 2, 'sd')) > 1e-10) stop('Rotational axes not the same.')
					mp_new[['RA']][[i]] <- matrix(mp_new[['RA']][[i]][1, ], nrow=1, ncol=3)
				}
			}
		}

		model.params <- mp_new
	}


	# Create new svg html file
	svg.new(file=file, window.title=window.title, animate.duration=animate.duration, animate.reverse=FALSE)

	# Add points
	if(animate){
		
		if(is.null(opacity.stroke.C)) opacity_stroke_C <- c(0.5,1)
		if(is.null(lwd.C)) lwd_C <- 2

		svg.pointsC(x=invivo, col.stroke=invivo_col, opacity.fill=opacity_stroke_C[1], 
			opacity.stroke=opacity_stroke_C[1], col.stroke.C=invivo_col, lwd.C=lwd_C, close=TRUE, 
			opacity.stroke.C=opacity_stroke_C[1], cex=cex)

		if(!is.null(model)) svg.pointsC(x=model, col.stroke=model_col, 
			opacity.fill=opacity_stroke_C[2], opacity.stroke=opacity_stroke_C[2], 
			col.stroke.C=model_col, lwd.C=lwd_C, close=TRUE, 
			opacity.stroke.C=opacity_stroke_C[2], cex=cex)
	}else{

		if(is.null(opacity.stroke.C)) opacity_stroke_C <- 0.2
		apply(model, 3, svg.pointsC, col.stroke=model_col, close=TRUE, 
			opacity.stroke.C=opacity_stroke_C[1], opacity.fill=0.2, opacity.stroke=opacity_stroke_C)
		apply(invivo, 3, svg.pointsC, col.stroke=invivo_col, close=TRUE, 
			opacity.stroke.C=opacity_stroke_C[1], opacity.fill=0.2, opacity.stroke=opacity_stroke_C)
	}

	# Get ranges
	ranges <- apply(invivo, 2, 'range')
	if(!is.null(model)) ranges <- rbind(ranges, apply(model, 2, 'range'))
	
	# Expand negative x by a bit
	ranges[1, 1] <- ranges[1, 1]-0.04*abs(ranges[2, 1]-ranges[1, 1])

	aor_list <- list()
	AOR <- list()
	for(params_type in c('invivo_params', 'model_params')){
		
		aor_list[[params_type]] <- list()
		AOR[[params_type]] <- list()
		params <- NULL
		if(params_type == 'invivo_params' && !is.null(invivo.params)){
			params <- invivo.params
			point_array <- invivo
		}
		if(params_type == 'model_params' && !is.null(model.params)){
			params <- model.params
			point_array <- model
		}

		if(is.null(params)) next

		for(i in 1:2){

			aor_list[[params_type]][[i]] <- list()

			if(is.na(params$RA[[i]][1, 1])){
				RA2D <- uvector(cprod(point_array[names(params$AJ), , 1], params$RI[[1]]))
				params$RA[[1]] <- matrix(RA2D, nrow=nrow(params$RA[[1]]), ncol=3, byrow=TRUE)
				params$RA[[2]] <- params$RA[[1]]
			}

			AOR[[params_type]][[i]] <- t(params$RA[[i]])

			if(nrow(AOR[[params_type]][[i]]) == 1) AOR[[params_type]][[i]] <- matrix(AOR[[params_type]][[i]], nrow=3, ncol=dim(point_array)[3])
			if(ncol(AOR[[params_type]][[i]]) == 1) AOR[[params_type]][[i]] <- matrix(AOR[[params_type]][[i]], nrow=3, ncol=dim(point_array)[3])

			aor_list[[params_type]][[i]] <- array(NA, dim=c(2,3,dim(point_array)[3]))
			aor_list[[params_type]][[i]][1, , ] <- point_array[names(params$RA)[i], , ]
			aor_list[[params_type]][[i]][2, , ] <- point_array[names(params$RA)[i], , ] + t(10*uvector(t(AOR[[params_type]][[i]])))

			ranges <- rbind(ranges, apply(aor_list[[params_type]][[i]], 2, 'range'))
		}
	}

	if(frame) svg.frame(x=ranges, grid.lwd=1.5, grid.opacity=0.3)

	# ADD ORIGIN VECTORS
	vec_len <- 0.8
	if(show.origin){
		svg.arrows(x=rbind(c(0,0,0), c(5,0,0)), opacity=0.5, len=vec_len, col='red')
		svg.arrows(x=rbind(c(0,0,0), c(0,5,0)), opacity=0.5, len=vec_len, col='green')
		svg.arrows(x=rbind(c(0,0,0), c(0,0,5)), opacity=0.5, len=vec_len, col='blue')
	}

	for(params_type in c('invivo_params', 'model_params')){
		
		params <- NULL
		if(params_type == 'invivo_params') opacity <- opacity_stroke_C[1]
		if(params_type == 'model_params') opacity <- opacity_stroke_C[2]

		if(params_type == 'invivo_params' && !is.null(invivo.params)){
			params <- invivo.params
			point_array <- invivo
			if(is.null(lwd.C)) lwd_C <- 2
		}
		if(params_type == 'model_params' && !is.null(model.params)){
			params <- model.params
			point_array <- model
		}

		if(is.null(params)) next

		for(i in 1:2){

			if(animate){
				svg.arrows(x=aor_list[[params_type]][[i]], len=vec_len, col='purple', lwd=lwd_C, 
					opacity=opacity)
			}else{

				if(ncol(AOR[[params_type]][[i]]) == 1){opacity <- 0.5}else{opacity <- 0.1}

				for(j in 1:ncol(AOR[[params_type]][[i]])){
					svg.arrows(x=rbind(point_array[names(params$RA)[i], , j], 
						point_array[names(params$RA)[i], , j] + uvector(AOR[[params_type]][[i]][, j])), 
						len=vec_len, col='purple', lwd=2, opacity=opacity)
				}
			}
		}
	}
	
	svg.close()
}