dist_point_to_line_error <- function(p, l1, l2, wts){
	n_lines <- nrow(l1)
	d <- rep(NA, n_lines)
	for(i in 1:n_lines) d[i] <- distPointToLine(p, l1[i,], l2[i,])
	mean(wts*d)
}