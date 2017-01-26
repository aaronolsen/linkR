linkR_examples <- function(name, fdir=NULL){

	# RETURN LIST
	rlist <- list()

	joint.coor <- NULL
	joint.types <- NULL
	joint.cons <- NULL
	joint.conn <- NULL
	input.param <- NULL
	input.joint <- NULL
	link.points <- NULL
	link.assoc <- NULL
	path.connect <- NULL

	# Set path to package
	if(is.null(fdir)){
		fdir <- tryCatch({
			fdir <- paste0(path.package("linkR"), "/extdata/")
		}, warning = function(w) {
		}, error = function(e) {
			if(e[1]$message == 'none of the packages are loaded'){
				fdir_dev <- '/Users/aaron/Documents/Research/github/linkR/inst/extdata/'
				if(file.exists(fdir_dev)){
					return(fdir_dev)
				}else{
					stop(e)
				}
			}
		}, finally = {
		})
	}

	kinematic_data_list <- list.files(paste0(fdir, 'kinematic_data/'))
	
	test_configs_list <- list.files(paste0(fdir, 'test_configs/'))
	
	simulation_names <- paste0(c('Full parameter', paste0('Constant ', c('3D', '2D', 'axis', 'length'))), ' simulation')

	if(name %in% c(simulation_names, paste0(simulation_names, ' set'))){
		
		name_wo_set <- gsub(' set$', '', name)
		
		# Set number of iterations per set
		num_iter <- 10
		
		# Set linkage parameters
		linkage <- list()
		linkage[[1]] <- list(
			'FJ' = list('susp.oper'=matrix(c(0,0,0), nrow=1)),
			'AJ' = list('oper.inop'=matrix(c(0,-1,0), nrow=1)),
			'LL' = list('ljaw.susp-susp.oper'=39, 'oper.inop-susp.oper'=31, 'inop.ljaw-oper.inop'=31, 'inop.ljaw-ljaw.susp'=9),
			'RA' = list('susp.oper'=matrix(c(0.010,0.03,-0.094), nrow=1), 'ljaw.susp'=matrix(c(0.0054,0.30,-0.31), nrow=1)),
			'RI' = list('ljaw.susp'=matrix(cprod(c(0.010,0.03,-0.094), c(0,-1,0)) %*% tMatrixEP(v=c(0.010,0.03,-0.094), a=-0.7), nrow=1)),
			'JO' = list('inop.ljaw'=matrix(c(-30,-31,-6.2), nrow=1))
		)

		if(name_wo_set == 'Constant 2D simulation'){
			linkage[[1]][['RA']][['susp.oper']] <- linkage[[1]][['RA']][['susp.oper']]*NA
			linkage[[1]][['RA']][['ljaw.susp']] <- linkage[[1]][['RA']][['ljaw.susp']]*NA
		}

		# If constant axis, make link lengths variable
		if(name_wo_set %in% c('Constant axis simulation', 'Full parameter simulation')){
			linkage[[1]][['LL']][['inop.ljaw-oper.inop']] <- linkage[[1]][['LL']][['inop.ljaw-oper.inop']]*seq(1,1.02,length=num_iter)
			linkage[[1]][['LL']][['ljaw.susp-susp.oper']] <- linkage[[1]][['LL']][['inop.ljaw-oper.inop']]*seq(1,1.02,length=num_iter)
		}

		# If constant length, make axes of rotation variable
		if(name_wo_set %in% c('Constant length simulation', 'Full parameter simulation')){
			linkage[[1]][['RA']][['susp.oper']] <- matrix(linkage[[1]][['RA']][['susp.oper']], nrow=num_iter, ncol=3, byrow=TRUE)
			linkage[[1]][['RA']][['susp.oper']][, 1] <- linkage[[1]][['RA']][['susp.oper']][, 1]*seq(1,7,length=num_iter)
			linkage[[1]][['RA']][['susp.oper']][, 2] <- linkage[[1]][['RA']][['susp.oper']][, 2]*seq(1,9,length=num_iter)
			linkage[[1]][['RA']][['susp.oper']] <- uvector(linkage[[1]][['RA']][['susp.oper']])

			linkage[[1]][['RA']][['ljaw.susp']] <- matrix(linkage[[1]][['RA']][['ljaw.susp']], nrow=num_iter, ncol=3, byrow=TRUE)
			linkage[[1]][['RA']][['ljaw.susp']][, 1] <- linkage[[1]][['RA']][['ljaw.susp']][, 1]*seq(1,3,length=num_iter)
			linkage[[1]][['RA']][['ljaw.susp']] <- uvector(linkage[[1]][['RA']][['ljaw.susp']])
		}

		linkage[[2]] <- linkage[[1]]
		
		# Set input parameters
		input.param <- list(list('susp.oper'=seq(0, 0.05, length=num_iter)), list('susp.oper'=seq(0.15, 0.2, length=num_iter)))

		if(!grepl(' set$', name)){
			linkage <- linkage[[1]]
			input.param <- input.param[[1]]
		}

		# Set which links are connected by each joint
		joint.conn <- rbind(c("susp","oper"), c("oper","inop"), c("inop","ljaw"), c("ljaw","susp"))
		rownames(joint.conn) <- c('susp.oper', 'oper.inop', 'inop.ljaw', 'ljaw.susp')

		# Create simulated constant 3D linkage
		solve_linkage <- solveLinkage(linkage=linkage, input.param=input.param, joint.conn=joint.conn)
		dimnames(solve_linkage)[[2]] <- c('x', 'y', 'z')

		if(grepl(' set$', name)) dimnames(solve_linkage)[[4]] <- paste0('Set', 1:2)

		return(list('xyz'=solve_linkage))
	}

	#
	if(paste0(name, '.txt') %in% kinematic_data_list){

		# Read and return motion data
		return(readMotionData(paste0(fdir, 'kinematic_data/', name, '.txt')))
	}

	if(paste0(name, '.R') %in% test_configs_list)
		return(sourcePartial_linkR(paste0(fdir, 'test_configs/', name, '.R'), 
			startTag='(# Define joint coordinates)|(# Get specimen data)',endTag='(# Animate linkage)|(# Set number of animation iterations)'))

	# If input parameters defined, return linkage
	if(!is.null(joint.coor)) return(defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, link.assoc=link.assoc,
		path.connect=path.connect))
}
