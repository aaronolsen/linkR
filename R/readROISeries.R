readROISeries <- function(file, name.min.char = 4){

	# Read lines
	read_lines <- readLines(file, warn=FALSE, skipNul=TRUE)

	# Create single string
	read_lines <- paste(read_lines, collapse='\n')
	
	#cat(read_lines);cat('\n')

	# Remove crazy characters
	cc <- paste0('\\', formatC(c(1:7,16:49,177,201:206,210,212,216,220,222:226,231,234:237,
		241,243,246,251,253:255,270:271,273,277,301:302,304:307,314,316:317,320:321,323,325,
		330,332,334,341,344,346,350,352:354,357,360:361,366:367,372:373,375:377),width=3,flag='0'))
	#cc_print <- paste0('c(\'', paste0(cc, collapse='\',\''), '\')');cat(cc_print); cat('\n')

	repl_char <- c('\001','\002','\003','\004','\005','\006','\007','\016','\017','\018','\019','\020','\021','\022','\023','\024','\025','\026','\027','\028','\029','\030','\031','\032','\033','\034','\035','\036','\037','\038','\039','\040','\041','\042','\043','\044','\045','\046','\047','\048','\049','\177','\201','\202','\203','\204','\205','\206','\210','\212','\216','\220','\222','\223','\224','\225','\226','\231','\234','\235','\236','\237','\241','\243','\246','\251','\253','\254','\255','\270','\271','\273','\277','\301','\302','\304','\305','\306','\307','\314','\316','\317','\320','\321','\323','\325','\330','\332','\334','\341','\344','\346','\350','\352','\353','\354','\357','\360','\361','\366','\367','\372','\373','\375','\376','\377', 'f88B')
	repl_char <- paste0(repl_char, collapse='|')
	#repl_char <- paste0('\b|\f|\n|\t|\v|', paste0(repl_char, collapse='|'))
	char_sub <- gsub(repl_char, '', read_lines)

	# Delete other things
	#char_sub <- gsub('E=?', '', char_sub, fixed=TRUE)

	# Split at 2DPos for names
	pos2d_split <- strsplit(char_sub, '2D[ ]?Pos:')[[1]]
	
	# Find string of letters, numbers and underscore more than 4 characters long
	greg_expr <- gregexpr(paste0('[A-Za-z0-9_-]+', '{', name.min.char, '}'), pos2d_split)

	# Create vectors and matrices
	read_names <- c()
	read_pos2d <- matrix(NA, nrow=0, ncol=2)
	read_pos3d <- matrix(NA, nrow=0, ncol=3)

	# Find name in each
	for(i in 1:length(pos2d_split)){

		# Default not found
		not_found <- TRUE

		# Find name
		for(j in length(greg_expr[[i]]):1){
			
			# Get potential name
			pot_name <- substr(pos2d_split[i], greg_expr[[i]][j], stop=greg_expr[[i]][j]+attr(greg_expr[[i]], "match.length")[j]-1)
			
			# Skip if only numeric
			if(!is.na(suppressWarnings(as.numeric(pot_name)))) next

			# Skip if number followed by px
			if(grepl('^[0-9]+px', pot_name, ignore.case=TRUE)) next
			if(grepl('^[0-9]+mm(0|1|2|x|y|z)?', pot_name, ignore.case=TRUE)) next

			if(grepl('Value$', pot_name)) next
			if(grepl('^JLN', pot_name)) next

			# Skip if in know roi file words
			if(grepl('^(Value|NSNumberNSValue|NSString|NSObjectiROI|NSMutableArrayNSArray|3DPos|streamtype)$', pot_name)) next

			# Add name
			read_names <- c(read_names, pot_name)
			
			# Set to found
			not_found <- FALSE

			break
		}
		
		#if(not_found) print(pos2d_split[i])
	}

	# Get number of markers
	num_markers <- length(gregexpr('2D[ ]?Pos:', char_sub, ignore.case=TRUE)[[1]])

	# Check for unnamed marker
	if('unnamed' %in% read_names){
		print(char_sub)
		warning("'Unnamed' marker present.")
	}

	# Check that marker names are unique (Neurocranium_bead_cauLE)
	if(length(unique(read_names)) != length(read_names)){

		# Find duplicates
		duplicates <- c()
		for(i in 1:length(read_names)) if(sum(read_names[i] == read_names) > 1) duplicates <- c(duplicates, read_names[i])
		
		stop(paste0("Marker names are not unique, duplicates: ", paste0(duplicates, collapse=', ')))
	}
	
	# Check that number of names and markers match	
	if(length(read_names) != num_markers) stop(paste0("Number of markers (", num_markers, ") does not match the number of unique names (", length(read_names), ")"))

	# Split at 3D for 3D
	pos3d_split <- strsplit(char_sub, '3D[ ]?Pos:')[[1]]

	# Read each line
	for(i in 2:length(pos2d_split)){

		# Split at 2D pos
		pos2d <- as.numeric(gsub('[A-Za-z: ]+', '', strsplit(pos2d_split[i], 'px')[[1]])[1:2])
	
		# Split at 3D pos
		pos3d <- as.numeric(gsub('[A-Za-z: ]+', '', strsplit(pos3d_split[i], 'mm')[[1]])[1:3])

		# Add elements
		read_pos2d <- rbind(read_pos2d, pos2d)
		read_pos3d <- rbind(read_pos3d, pos3d)
	}

	markers_2d <- read_pos2d
	rownames(markers_2d) <- read_names
	markers_2d <- markers_2d[sort(rownames(markers_2d)), ]

	markers_3d <- read_pos3d
	rownames(markers_3d) <- read_names
	markers_3d <- markers_3d[sort(rownames(markers_3d)), ]

	#print(markers_3d)

	return(markers_3d)

	# Find point names
	#regexpr('', '', read_lines)

	Sys.setlocale('LC_ALL','C')
	#read_lines <- gsub(paste('\001\231\f'), '\n', read_lines)
	#read_lines <- gsub(paste('\001\222\201\t\001\222\204\201\001\001\231(\020|\021|\v|\f|\a|\025|\023|\027|\017|\022|\016)'), '\n', read_lines)
	#read_lines <- gsub(paste('\201\t\001\222\204\201\001\001\231(\016|\017|\020|\021|\022|\023|\025|\026|\027|\031|\032|\033|\035|\036|\a|\f|\v| |#)'), '\n', read_lines)

	read_lines <- gsub('[$|!|#|\"|)|*|%]', ' ', read_lines)
	read_lines <- gsub(paste('\001\222\204\201\001\001\231(\016|\017|\020|\021|\022|\023|\024|\025|\026|\027|\028|\029|\030|\031|\032|\033|\034|\035|\036|\a|\f|\v| )'), '\n', read_lines)

	read_lines <- strsplit(read_lines, split='\n')[[1]]
	read_lines <- read_lines[read_lines != '']
	
	read_names <- c()
	read_pos2d <- matrix(NA, nrow=0, ncol=2)
	read_pos3d <- matrix(NA, nrow=0, ncol=3)

	prev_name <- FALSE
	
	for(line_num in 2:length(read_lines)){

		if(!grepl('\206', read_lines[line_num])) next

		# Split at marker name
		if(!prev_name) name <- strsplit(read_lines[line_num], '\206')[[1]][1]		

		prev_name <- FALSE
		if(!grepl('2D Pos: ', read_lines[line_num])){
			prev_name <- TRUE
			next
		}

		# Split at 2D pos
		pos2d_split <- strsplit(read_lines[line_num], '2D Pos: ')[[1]]
		pos2d <- as.numeric(gsub('[A-Za-z: ]+', '', strsplit(pos2d_split[2], 'px')[[1]])[1:2])

		# Split at 3D pos
		pos3d_split <- strsplit(read_lines[line_num], '3D Pos: ')[[1]]
		pos3d <- as.numeric(gsub('[A-Za-z: ]+', '', strsplit(pos3d_split[2], 'mm')[[1]])[1:3])

		read_names <- c(read_names, name)
		read_pos2d <- rbind(read_pos2d, pos2d)
		read_pos3d <- rbind(read_pos3d, pos3d)
	}
	
	markers_2d <- read_pos2d
	rownames(markers_2d) <- read_names
	markers_2d <- markers_2d[sort(rownames(markers_2d)), ]

	markers_3d <- read_pos3d
	rownames(markers_3d) <- read_names
	markers_3d <- markers_3d[sort(rownames(markers_3d)), ]

	markers_3d
}