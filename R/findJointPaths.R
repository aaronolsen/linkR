findJointPaths <- function(body.conn, joint.types, solvable.paths, sole.joints){

	# IF A SINGLE JOINT, RETURN 0,1
	if(nrow(body.conn) == 1) return(list(
		'paths.closed'=NULL,
		'paths.open'=list(c(0,1)),
		'fixed.joints'=1,
		'joint.conn'=NULL
	))

	## CREATE MATRIX FOR CONNECTED JOINTS
	joint.conn <- matrix(NA, nrow=0, ncol=3, dimnames=list(NULL, c('body.idx', 'joint1', 'joint2')))

	# GET UNIQUE LINK INDICES
	link_idx_unique <- sort(unique(c(body.conn)))

	# NUMBER OF JOINTS
	num_joints <- nrow(body.conn)

	# FILL MATRIX OF CONNECTED JOINTS
	for(link_idx in link_idx_unique){

		# FIND ALL JOINTS CONNECTED TO LINK
		joints_conn <- (1:num_joints)[(rowSums(link_idx == body.conn) == 1)]
		
		# REMOVE NA VALUES
		#joints_conn <- joints_conn[!is.na(joints_conn)]

		# SKIP IF ONLY SINGLE JOINT CONNECTED
		if(length(joints_conn) == 1) next

		# GENERATE UNIQUE PAIRS
		for(i in 1:(length(joints_conn)-1)){
			for(j in (i+1):(length(joints_conn))){
				joint.conn <- rbind(joint.conn, c(link_idx, sort(c(joints_conn[i], joints_conn[j]))))
			}
		}
	}
	
	# FIND JOINTS CONNECTED TO FIXED LINK (INCLUDES OPEN CHAIN WITH SINGLE JOINT CONNECTED TO FIXED LINK)
	fixed.joints <- which(rowSums(body.conn == 1) > 0)
	#fixed.joints <- unique(c(joint.conn[joint.conn[, 1] == 1, 2:3]))

	# ADD ROWS FOR JOINTS CONNECTED TO THE FIXED LINK FOR EASIER PATH SEARCHING
	joint.conn <- rbind(cbind(rep(1,length(fixed.joints)), rep(0,length(fixed.joints)), fixed.joints), joint.conn)
	colnames(joint.conn) <- c('body.idx', 'joint1', 'joint2')
	rownames(joint.conn) <- NULL

	## FIND ALL JOINT PATHS
	# INITIAL PATHS
	paths <- list()
	for(i in 1:length(fixed.joints)) paths[[i]] <- c(0, fixed.joints[i])

	it <- 1
	no_change <- FALSE
	max_path_len <- 20

	# FIND ALL UNIQUE FIXED TO FIXED PATHS
	#while(it < 5){
	while(no_change == FALSE){

		#for(i in 1:length(paths)){cat('P');print(paths[[i]])}

		no_change <- TRUE

		for(i in 1:length(paths)){
		
			#cat('P');print(paths[[i]])
		
			# SKIP IF LAST JOINT IS NA
			if(is.na(paths[[i]][length(paths[[i]])])) next

			# SKIP IF LAST JOINT IS CONNECTED TO FIXED
			if(paths[[i]][length(paths[[i]])] == 0) next

			# SKIP IF PATH EXCEEDS MAXIMUM PATH LENGTH
			if(length(paths[[i]]) > max_path_len) next
			
			# GET LAST JOINT IN PATH
			last_joint <- paths[[i]][length(paths[[i]])]

			# FIND ALL CONNECTED JOINTS
			joints_conn_idx <- rowSums(joint.conn[, 2:3] == last_joint) > 0
			joints_conn <- unique(c(joint.conn[joints_conn_idx, 2:3]))
			
			# REMOVE JOINTS ALREADY IN PATH EXCEPT ZEROS
			joints_conn <- joints_conn[!joints_conn %in% paths[[i]][paths[[i]] > 0]]

			# IF LAST JOINT IS JOINT CONNECTED TO FIXED LINK, REMOVE OTHER FIXED LINK JOINTS - AVOIDS LOOPS WITHIN FIXED LINK
			if(last_joint %in% fixed.joints) joints_conn <- joints_conn[!joints_conn %in% fixed.joints]

			# CHECK IF NO CONNECTED JOINTS
			if(length(joints_conn) == 0) next

			# REFINE TO LIMIT NUMBER OF LOOPS WITHIN LINK
			if(TRUE){

				if(length(paths[[i]]) > 1 && paths[[i]][length(paths[[i]])] != 0){
				
					## IF LAST TWO JOINTS ARE WITHIN SAME BODY, REMOVE JOINTS IN THE SAME BODY - AVOIDS LOOPS WITHIN A LINK
					# GET LAST TWO JOINTS
					last_two <- paths[[i]][(length(paths[[i]])-1):length(paths[[i]])]
				
					# FIND ASSOCIATED BODIES
					body1 <- unique(body.conn[last_two[1], ])
					body2 <- unique(body.conn[last_two[2], ])
				
					# FIND SAME BODY BETWEEN TWO JOINTS
					shared_body <- body1[body1 %in% body2]
				
					# FIND JOINTS
					if(length(shared_body) > 0){

						# FIND JOINTS CONNECTED TO SHARED BODY
						shared_body_joints <- unique(c(joint.conn[joint.conn[, 1] == shared_body, 2:3]))
					
						# REMOVE
						joints_conn <- joints_conn[!joints_conn %in% shared_body_joints]
					}

					# IF ALL CONNECTING JOINTS WERE ELIMINATED BECAUSE THEY WERE IN THE SAME LINK, ADD NA TO FLAG PATH FOR REMOVAL
					if(length(joints_conn) == 0){
						#paths[[i]] <- c(paths[[i]], NA)
						next
					}
				}
				#cat('J2 ');print(joints_conn)
			}

			# REMOVE ZERO IF PATH IS ONLY TWO JOINTS LONG
			if(length(paths[[i]]) == 2) joints_conn <- joints_conn[joints_conn > 0]

			# CHECK IF NO CONNECTED JOINTS
			if(length(joints_conn) == 0) next

			#cat('P');print(paths[[i]])

			# SET CHANGE
			no_change <- FALSE

			# CREATE NEW PATHS IF MORE THAN ONE CONNECTING JOINT
			if(length(joints_conn) > 1) for(j in 2:length(joints_conn)) paths[[length(paths)+1]] <- c(paths[[i]], joints_conn[j])

			# ADD FIRST CONNECTING JOINT TO CURRENT PATH
			paths[[i]] <- c(paths[[i]], joints_conn[1])

			#cat('\n')
		}
		
		it <- it + 1
	}

	## BREAK UP CLOSED PATHS INTO FRAGMENTS
	# FIND MINIMUM AND MAXIMUM FRAGMENT LENGTH TO SAVE
	paths_nchar <- unlist(lapply(gsub('-', '', solvable.paths), 'nchar'))
	frag_len_min <- min(paths_nchar)
	frag_len_max <- max(paths_nchar)

	# ADD REVERSE SOLVABLE PATHS
	for(i in 1:length(solvable.paths)) solvable.paths <- c(solvable.paths, paste0(rev(strsplit(solvable.paths[i], '-')[[1]]), collapse='-'))
	solvable.paths <- unique(solvable.paths)
	
	# Remove paths with NA - may need to put this later?
	#paths <- paths[unlist(lapply(lapply(paths, 'is.na'), 'sum')) == 0]

	path_frags <- c()
	path_frag_list <- list()
	for(path_num in 1:length(paths)){
	
		path <- paths[[path_num]]

		# IF OPEN CHAIN SAVE BUT DO NOT BREAK INTO FRAGMENTS
		if(path[1] == 0 && tail(path, 1) != 0){
			path_frag_list[[length(path_frag_list) + 1]] <- path
			next
		}
		
		for(frag_len in frag_len_min:min(length(path), frag_len_max)){
			for(i in 1:(length(path)-frag_len+1)){
				
				# SAVE PATH AS VECTOR
				path_vec <- path[seq(i, i+frag_len-1)]

				# REMOVE ZEROS
				path_vec <- path_vec[path_vec != 0]
				
				# CHECK IF FRAGMENT ALREADY EXISTS
				if(paste(path_vec, collapse='-') %in% path_frags) next

				#print(path_vec)

				# CHECK IF FRAGMENT TYPE SEQUENCE IS SOLVABLE
				if(!paste0(joint.types[path_vec], collapse='-') %in% solvable.paths) next

				# MAKE SEQUENCE INTO STRING FOR EASY LOOK-UP
				path_string <- paste(path_vec, collapse='-')

				# ADD PATH TO VECTOR OF FRAGMENTS
				path_frags <- c(path_frags, path_string)
				
				# ADD PATH TO PATH LIST
				path_frag_list[[length(path_frag_list) + 1]] <- c(0, path_vec, 0)
			}
		}
	}
	
	paths <- path_frag_list

	# CONVERT PATHS TO STRINGS
	paths_str <- unlist(lapply(paths, 'paste', collapse='-'))

	for(i in 1:length(paths)){

		# REMOVE PATHS WITH NA VALUE
		#if(sum(is.na(paths[[i]])) > 0 && !is_open[i]){paths_str[i] <- NA; next}

		# GET END JOINT
		end_joint <- tail(paths[[i]], 1)

		# REMOVE PATHS THAT END IN NON-FIXED JOINT THAT IS CONNECTED TO TWO OR MORE OTHER JOINTS (FALSE OPEN CHAIN)
		if(end_joint > 0){
		
			# SKIP IF JOINT IS "SOLE JOINT" - JOINT CONNECTED TO OTHERWISE ISOLATED BODY
			if(end_joint %in% sole.joints) next
			
			# FIND CONNECTED JOINTS
			joints_conn_end <- unique(c(joint.conn[rowSums(end_joint == joint.conn[, 2:3]) > 0, 2:3]))

			# REMOVE FOCAL JOINT AND ZERO
			joints_conn_end <- joints_conn_end[!joints_conn_end %in% c(0, end_joint)]

			# REMOVE FIXED JOINTS
			joints_conn_end <- joints_conn_end[!joints_conn_end %in% fixed.joints]

			# IF JOINTS REMAINING, MARK AS NA
			if(length(joints_conn_end) > 1) paths_str[i] <- NA

			# COERCE TO MATRIX			
			#if(is.vector(joints_conn_end)) joints_conn_end <- matrix(joints_conn_end, nrow=1, ncol=2)
			#joints_conn_end <- joints_conn_end[rowSums(joints_conn_end == 0) == 0, ]
			# IF MORE THAN ONE ROW (LENGTH > 2) MARK AS NA			
			#if(length(joints_conn_end) > 2) paths_str[i] <- NA
			
			next
		}

		if(FALSE){

			# REMOVE PATHS THAT DOUBLE BACK ON THE SAME LINK
			bodies_in_path <- rep(NA, length(paths[[i]])-1)
			for(j in 1:(length(paths[[i]])-1)){
		
				# FIND BODY ASSOCIATED WITH PAIR OF JOINTS
				body_num <- joint.conn[rowSums((joint.conn[, 2:3] == paths[[i]][j])+(joint.conn[, 2:3] == paths[[i]][j+1])) == 2, 1]
			
				# PATH WITH ZERO AT ENDS TO INDICATE CLOSED PATH BUT ACTUALLY A FRAGMENT WITHIN CLOSED PATH
				if(length(body_num) == 0) next
			
				# SKIP IF 0
				if(body_num == 1) next
			
				# IF SAME BODY IS PRESENT IN PATH TWICE, MARK AS NA
				if(body_num %in% bodies_in_path){
					paths_str[i] <- NA
					next
				}

				# ADD BODY NUMBER TO VECTOR
				bodies_in_path[j] <- body_num
			}
		}
		
		# REMOVE PATHS THAT ARE REVERSE OF ANOTHER PATH - EVERY FIXED-TO-FIXED PATH SHOULD HAVE REVERSE
		#if(sum(paste(rev(paths[[i]]), collapse='-') == paths_str[-i], na.rm=TRUE) > 0) paths_str[i] <- NA
	}

	# REMOVE NA VALUES
	paths_str <- paths_str[!is.na(paths_str)]
	
	# SORT INTO CLOSED AND OPEN PATHS
	paths_closed_str <- paths_str[grepl('^0-', paths_str) * grepl('-0$', paths_str) == 1]
	paths_open_str <- paths_str[!grepl('-0$', paths_str)]

	# REMOVE FIRST AND LAST ZEROES
	paths_closed_str <- gsub('(^0-)|(-0$)', '', paths_closed_str)
	paths_open_str <- gsub('^0-', '', paths_open_str)

	# EXPLODE BACK INTO VECTOR
	paths_closed <- lapply(strsplit(paths_closed_str, '-'), 'as.numeric')
	paths_open <- lapply(strsplit(paths_open_str, '-'), 'as.numeric')

	# SET ZERO-LENGTH PATHS AS NULL
	if(length(paths_closed) == 0) paths_closed <- NULL
	if(length(paths_open) == 0) paths_open <- NULL
	
	list(
		'paths.closed'=paths_closed,
		'paths.open'=paths_open,
		'fixed.joints'=fixed.joints,
		'joint.conn'=joint.conn
	)
}