print_joint_status <- function(mechanism, indent){

	dframe <- data.frame(
		'jointed'=mechanism[['status']][['jointed']],
		'solved-1'=mechanism[['status']][['solved']][,1],
		'solved-2'=mechanism[['status']][['solved']][,2],
		'transformed-1'=mechanism[['status']][['transformed']][,1],
		'transformed-2'=mechanism[['status']][['transformed']][,2]
		)

	indent_str1 <- paste0(rep(indent, 4), collapse='')
	indent_str2 <- paste0(rep(indent, 5), collapse='')
	cat(paste0(indent_str1, 'Joint status:\n'))
	cat(paste0(indent_str2, paste0(capture.output(print(dframe)), collapse=paste0('\n', indent_str2)), '\n'))

	#joint_status_print <- joint.status
	#joint_status_print[joint_status_print == ''] <- '_'
	#cat(paste0(paste0(rep(indent, indent.level-1), collapse=''), 'Joint status\n'))
	#for(k in 1:nrow(joint_status_print)) cat(paste0(paste0(rep(indent, indent.level), collapse=''), joint.names[k], ': ', paste0(joint_status_print[k,], collapse=','), '\n'))
}