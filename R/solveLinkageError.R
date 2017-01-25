solveLinkageError <- function(p, linkage, marker_array, NA.penalty=0, shift.input=TRUE, 
	return.error='rmse'){ 

	if(is.null(names(linkage))){
		errors <- c()
		for(ii in 1:length(linkage)){
			errors <- c(errors, solveLinkageError(p=p, linkage=linkage[[ii]], 
				marker_array=marker_array[, , , ii], NA.penalty=NA.penalty, shift.input=shift.input, 
				return.error='diff'))
		}
		rmse <- sqrt(mean(errors^2, na.rm=TRUE)) + NA.penalty*sum(is.na(errors))
		return(rmse)
	}

	# Replace linkage parameters with parameters to be optimized
	linkage <- replaceParameters(p=p, linkage=linkage)

	# Shift input parameters for constant axes to most closely match reference
	if(shift.input){
		shift_input <- shiftInputParameters(linkage=linkage, ref.coor=marker_array)
		input.param <- shift_input$RD
	}else{
		input.param <- linkage$RD
	}

	joint.conn <- linkage$joint.conn
	linkage$joint.conn <- NULL
	linkage$RD <- NULL

	# Solve linkage
	linkage_solve <- solveLinkage(linkage, input.param=input.param, joint.conn=joint.conn)
	
	#print(linkage_solve)

	# FIND DIFFERENCE IN COORDINATE POSITIONS
	diff <- linkage_solve - marker_array

	# RETURN ROOT MEAN SQUARE ERROR/DEVIATION
	if(return.error == 'diff') return(diff)
	if(return.error == 'rmse') return(sqrt(mean(diff^2, na.rm=TRUE)) + NA.penalty*sum(is.na(diff)))
}
