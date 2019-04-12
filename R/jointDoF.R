jointDoF <- function(name){

	joint_dofs <- c('r'=1, 'u'=2, 's'=3, 'p'=2, 'ur'=2, 'ou'=3, 'our'=3)
	
	joint_dofs[tolower(name)]
}