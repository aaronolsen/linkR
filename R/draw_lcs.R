draw_lcs <- function(lcs, file = NULL, arrow.len = 1, head.len = "auto", col = c('red', 'green', 'blue'), lwd = 1){

	# If file is null, set current connection
	if(is.null(file)){

		# Give error if svg.new has not been called
		if(is.null(getOption("svg_glo_con"))) stop("svg.new has not been called yet.")

		# Get current connection
		file <- getOption("svg_glo_con")
	}

	if(length(dim(lcs)) == 2){
		for(arrow in 1:3){
			slcs <- rbind(lcs[1, ], lcs[1, ]+arrow.len*uvector(lcs[arrow+1, ]-lcs[1, ]))
			svg.arrows(slcs, lwd=lwd, col=col[arrow], len=head.len)
		}
	}else if(length(dim(lcs)) == 3){
		for(arrow in 1:3){
			slcs <- lcs[c(1,arrow+1), , ]
			for(j in 1:dim(lcs)[3]) slcs[, , j] <- rbind(slcs[1, , j], slcs[1, , j]+arrow.len*uvector(slcs[2, , j]-slcs[1, , j]))
			svg.arrows(slcs, lwd=lwd, col=col[arrow], len=head.len)
		}
	}
}