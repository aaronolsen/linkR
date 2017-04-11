unify_xromm_ct <- function(ct_mat, xr_arr, print.progress = TRUE){

	# Get body names
	body_names <- rownames(ct_mat)

	body_names <- gsub('_[A-Za-z0-9-]*', '', body_names)

	#body_names <- gsub('[0-9]', '', body_names)
	#for(i in 1:2) body_names <- gsub(paste0('_(ant|sup|mid|inf|pos)[_]?'), '_', body_names)
	#body_names <- gsub('_$', '', body_names)

	# Get virtual markers
	body_names_vm <- rep(NA, length(body_names))
	body_names_vm[grepl('-', body_names)] <- gsub('[A-Za-z]+-', '', body_names[grepl('-', body_names)])
	body_names <- gsub('-[A-Za-z]+', '', body_names)
	names(body_names_vm) <- body_names

	# Set body associations
	body_assoc <- body_names

	# Get unique body names
	body_names <- unique(body_names)
	
	# Create transformation matrix names
	tm_names <- body_names

	# Create transformation matrix array
	tm_arr <- array(NA, dim=c(4, 4, length(tm_names), dim(xr_arr)[3]), 
		dimnames=list(NULL, NULL, tm_names, NULL))

	# Create coordinate system names
	cs_names <- body_names

	# Create array for initial coordinate system positions and orientations
	cs_ini <- array(NA, dim=c(4, dim(xr_arr)[2], length(cs_names)), dimnames=list(NULL, NULL, cs_names))

	# Fill coordinate system matrix - initial configuration
	for(cs_name in dimnames(cs_ini)[[3]]){
	
		if(cs_name %in% body_names){

			# Find all markers associated with bodys
			ct_mat_sub <- ct_mat[grepl(paste0(cs_name, '[_|-]'), paste0(rownames(ct_mat), '_')), ]
			
			# Find body centroid
			if(is.matrix(ct_mat_sub)){
				cs_ini[1, , cs_name] <- colMeans(ct_mat_sub, na.rm=TRUE)
			}else{
				cs_ini[1, , cs_name] <- ct_mat_sub
			}
			
			# Add orientation points
			cs_ini[2:4, , cs_name] <- matrix(cs_ini[1, , cs_name], nrow=3, ncol=3, byrow=TRUE) + diag(3)
		}
	}

	# Create array for transformed CT markers
	ct_arr <- array(NA, dim=c(dim(ct_mat)[1], dim(xr_arr)[2:3]), dimnames=list(rownames(ct_mat), dimnames(xr_arr)[[3]]))

	# Order in which to align body points - do bodies with virtual markers first
	body_order <- setNames(rep(2, length(body_names)), body_names)
	body_order[unique(names(body_names_vm[!is.na(body_names_vm)]))] <- 1
	body_order <- sort(body_order)
	
	# Get names of virtual markers
	virtual_markers <- rownames(ct_mat)[!is.na(body_names_vm)]

	# Add virtual markers to xr_arr
	xr_arr_n <- array(NA, dim=c(dim(xr_arr)[1]+length(virtual_markers), dim(xr_arr)[2], dim(xr_arr)[3]), 
		dimnames=list(c(dimnames(xr_arr)[[1]], virtual_markers), dimnames(xr_arr)[[2]], dimnames(xr_arr)[[3]]))
	xr_arr_n[dimnames(xr_arr)[[1]], , ] <- xr_arr

	# Iterate through each frame of video
	min_iter <- 1
	max_iter <- dim(xr_arr_n)[3]
	for(iter in min_iter:max_iter){
	
		#if(print.progress) cat(iter, '\n')

		# Do unification of all common points to approximately orient bodies with single X-ray marker
		m1 <- xr_arr[rownames(ct_mat)[rownames(ct_mat) %in% dimnames(xr_arr)[[1]]], , iter]
		m2 <- ct_mat
		m3 <- apply(cs_ini, 2, as.matrix)
		align_ct_xr <- findBestAlignment(m1, m2, m3=m3, sign=1)
		ct_mat_align <- align_ct_xr$mat
		cs_ini_align <- cs_ini
		for(rowi in seq(1, nrow(m3), by=4)) cs_ini_align[, , ((rowi-1) / 4) + 1] <- align_ct_xr$mc[rowi:(rowi+3), ]
		
		# Unify markers
		for(body_name in names(body_order)){

			#if(!body_name %in% c('SuspensoriumL', 'Neurocranium')) next
			if(print.progress) cat(paste0(body_name, '\n'))

			# Get body landmarks
			#ct_mat_sub <- ct_mat[which(body_assoc == body_name), ]
			ct_row_sub <- grepl(paste0(body_name, '[_|-]'), paste0(rownames(ct_mat), '_'))
			ct_mat_sub <- ct_mat[ct_row_sub, ]
			#print(rownames(ct_mat)[grepl(paste0(body_name, '_|-'), rownames(ct_mat))])
		
			# Find which CT markers are in XR array
			ct_in_xr <- rownames(ct_mat_sub) %in% dimnames(xr_arr_n)[[1]]

			if(sum(ct_in_xr) == 0){
				if(print.progress) cat(paste0('\t0 common point(s) between CT and X-Ray sets for body \'', body_name, '\'\n'))
				next
			}

			# XROMM markers in CT
			xr_mat_sub <- matrix(xr_arr_n[rownames(ct_mat_sub)[ct_in_xr], , iter], nrow=sum(ct_in_xr), ncol=ncol(xr_arr_n), 
				dimnames=list(rownames(ct_mat_sub)[ct_in_xr], NULL))
			
			# Remove NA values
			xr_mat_sub <- matrix(xr_mat_sub[!is.na(xr_mat_sub[, 1]), ], nrow=sum(!is.na(xr_mat_sub[, 1])), ncol=ncol(xr_mat_sub), 
				dimnames=list(rownames(xr_mat_sub)[!is.na(xr_mat_sub[, 1])], NULL))
			
			# Translate bodies with single marker in common
			if(nrow(xr_mat_sub) == 1){

				# Find translation from CT coordinates to X-ray coordinates
				tmat <- align_ct_xr$tmat
				#tmat <- diag(4)
				common_marker <- rownames(xr_mat_sub)
				tmat[1:3, 4] <- tmat[1:3, 4] + (xr_mat_sub[common_marker, ] - ct_mat_align[common_marker, ])

				# Translate CT markers based on common point with X-ray markers
				ct_mat_sub_t <- ct_mat_align[rownames(ct_mat_sub), ] + matrix(xr_mat_sub[common_marker, ] - ct_mat_align[common_marker, ], nrow=nrow(ct_mat_sub), ncol=ncol(ct_mat_sub), byrow=TRUE)

				# Save transformation matrix
				tm_arr[, , body_name, iter] <- tmat

			}else if(nrow(xr_mat_sub) == 2){

				if(print.progress) cat(paste0('\t', sum(ct_in_xr), ' common point(s) between CT and X-Ray sets for body \'', body_name, '\'\n'))
				if(print.progress) cat(paste0('\t', sum(is.na(xr_arr_n[rownames(ct_mat_sub)[ct_in_xr], 1, 1])), ' point(s) are NA\n'))
				next

			}else{

				# Transform CT markers to correspond with XROMM markers
				align <- findBestAlignment(xr_mat_sub, ct_mat_sub, m3=cs_ini[, , body_name], sign=1)
				ct_mat_sub_t <- align$mat

				# Save transformation matrix
				tm_arr[, , body_name, iter] <- align$tmat
			}

			if(print.progress && !is.null(align)){
				print(align$dist.error)
				#cat(paste0('\tError range: ', paste(range(align$dist.error), collapse=', '), '\n'))
			}
			
			# Add new positions of any virtual markers to xr_arr_n
			if(sum(rownames(ct_mat_sub) %in% virtual_markers) > 0){
				
				# Virtual markers in subset
				virtual_markers_sub <- rownames(ct_mat_sub)[rownames(ct_mat_sub) %in% virtual_markers]

				# Remove virtual markers that are non-NA in xr_arr (previously positioned with first body)
				virtual_markers_sub <- virtual_markers_sub[is.na(xr_arr_n[virtual_markers_sub, 1, iter])]
				
				# Add new virtual markers
				if(length(virtual_markers_sub) > 0) xr_arr_n[virtual_markers_sub, , iter] <- ct_mat_sub_t[rownames(ct_mat_sub) %in% virtual_markers, ]
			}

			ct_arr[rownames(ct_mat_sub_t), , iter] <- ct_mat_sub_t

			#print(xr_mat_sub);print(ct_mat_sub_t)
		}
		
		#print(ct_arr[, , iter])
	}

	# If any values in CT array are NA, replace with X-ray markers that have the same name
	ct_is_na <- is.na(ct_arr[, 1, 1])
	if(sum(ct_is_na) > 0){
		ct_na_in_xr <- names(ct_is_na)[ct_is_na][names(ct_is_na)[ct_is_na] %in% dimnames(xr_arr)[[1]]]
		if(length(ct_na_in_xr) > 0) ct_arr[ct_na_in_xr, , ] <- xr_arr[ct_na_in_xr, , ]
	}

	list(
		'ct_arr'=ct_arr,
		'xr_arr'=xr_arr_n,
		'tm_arr'=tm_arr
	)
}