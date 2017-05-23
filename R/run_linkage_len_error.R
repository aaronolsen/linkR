run_linkage_len_error <- function(p, params, input.params, coor.compare, fit.markers, on.error.return=100){

	# Set length
	params$other$hyoid.d <- p

	# Run linkage
	run_r <- run_linkage(coor=params$coor_init, cons=params$jt_cons_list, input.params=input.params, 
		body_names=params$body_names, other=params$other)

	# If no solution, return large number
	if(is.null(run_r)) return(on.error.return)

#print(run_r$coor)
#print(coor.compare)
	# Compare simulated coordinates to ideal
	sqrt(mean((run_r$coor[fit.markers, , ] - coor.compare[fit.markers, , ])^2))
}
