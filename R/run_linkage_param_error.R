run_linkage_param_error <- function(p, params, coor.compare, fit.markers, on.error.return=100){

	# Create p as list
	p_list <- list()
	for(i in 1:length(params$other$param.names)) p_list[[params$other$param.names[i]]] <- p[i]

	# Run linkage
	run_r <- run_linkage(coor=params$coor_init, cons=params$jt_cons_list, input.params=p_list, 
		body_names=params$body_names, other=params$other)

	# If no solution, return large number
	if(is.null(run_r)) return(on.error.return)

#print(run_r$coor)
#print(coor.compare)
	# Compare simulated coordinates to ideal
	sqrt(mean((run_r$coor[fit.markers, , 1] - coor.compare[fit.markers, ])^2))
}
