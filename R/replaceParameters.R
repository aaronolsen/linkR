replaceParameters <- function(p, linkage){

	if(is.null(names(linkage))){
		for(ii in 1:length(linkage)) linkage[[ii]] <- replaceParameters(p=p, linkage=linkage[[ii]])
		return(linkage)
	}

	if('RA1x' %in% names(p)) linkage$RA[[1]] <- matrix(p[c('RA1x', 'RA1y', 'RA1z')], ncol=3)
	if('RA2x' %in% names(p)) linkage$RA[[2]] <- matrix(p[c('RA2x', 'RA2y', 'RA2z')], ncol=3)

	if('LL1' %in% names(p)) linkage$LL[[1]] <- p['LL1']
	if('LL2' %in% names(p)) linkage$LL[[2]] <- p['LL2']
	if('LL3' %in% names(p)) linkage$LL[[3]] <- p['LL3']
	if('LL4' %in% names(p)) linkage$LL[[4]] <- p['LL4']

	if('RI1x' %in% names(p)) linkage$RI[[1]] <- matrix(p[c('RI1x', 'RI1y', 'RI1z')], ncol=3)

	if('AJ1x' %in% names(p)) linkage$AJ[[1]] <- matrix(p[c('AJ1x', 'AJ1y', 'AJ1z')], ncol=3)

	linkage
}