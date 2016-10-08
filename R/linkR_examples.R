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
				fdir_dev <- '/Users/aaron/Documents/Research/R dev/linkR/inst/extdata/'
				if(file.exists(fdir_dev)){
					return(fdir_dev)
				}else{
					stop(e)
				}
			}
		}, finally = {
		})
	}
	
	test_configs_list <- list.files(paste0(fdir, 'test_configs/'))
	
	if(paste0(name, '.R') %in% test_configs_list)
		return(sourcePartial_linkR(paste0(fdir, 'test_configs/', name, '.R'), 
			startTag='(# Define joint coordinates)|(# Get specimen data)',endTag='(# Animate linkage)|(# Set number of animation iterations)'))

	# If input parameters defined, return linkage
	if(!is.null(joint.coor)) return(defineLinkage(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, joint.conn=joint.conn, link.points=link.points, link.assoc=link.assoc,
		path.connect=path.connect))
}
