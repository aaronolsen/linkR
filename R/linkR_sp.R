# Set solvable paths
linkR_sp <- list()
linkR_sp[['str']] <- list()
linkR_sp[['exp']] <- list()
linkR_sp[['exp']][[1]] <- c('R', 'J', 'SN', '1', 'R', 'J', 'NN', '2', 'R', 'D', 'NS')
linkR_sp[['exp']][[length(linkR_sp[['exp']])+1]] <- c('S', 'J', 'SP', '1', 'S', 'J', 'PS', '1', 'S', 'J', 'PN', '2', 'S', 'D', 'NS')
linkR_sp[['exp']][[length(linkR_sp[['exp']])+1]] <- c('R', 'J', 'SN', '1', 'S', 'J', 'NN', '2', 'S', 'D', 'NS')
linkR_sp[['exp']][[length(linkR_sp[['exp']])+1]] <- c('R', 'J', 'SN', '1', 'L', 'J', 'NN', '2', 'R', 'D', 'NS')
linkR_sp[['exp']][[length(linkR_sp[['exp']])+1]] <- c('S', 'J', 'SN', '1', 'L', 'J', 'NN', '2', 'S', 'D', 'NS')

for(linkR_i in 1:length(linkR_sp[['exp']])){
	
	# Get separate components of string
	lvec <- linkR_sp[['exp']][[linkR_i]]
	lseq <- seq(1,length(lvec),by=4)

	# Create strings
	jt_str <- createJointPathString(joint.types=lvec[lseq], jointed=lvec[lseq+1], status=lvec[lseq+2], 
		bodies=lvec[seq(4,length(lvec),by=4)], mode = c('t', 'tj', 'ts', 'tjs'))

	# Save strings
	linkR_sp[['str']][['t']][linkR_i] <- jt_str[['t']]
	linkR_sp[['str']][['ts']][linkR_i] <- jt_str[['ts']]
	linkR_sp[['str']][['tj']][linkR_i] <- jt_str[['tj']]
	linkR_sp[['str']][['tjs']][linkR_i] <- jt_str[['tjs']]
}