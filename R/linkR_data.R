linkR_data <- function(name, fdir=NULL){

	# RETURN LIST
	rlist <- list()

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

	# CONVERT NAME TO LOWERCASE
	name <- tolower(name)

	# GET SPECIES NAME AND FILE NAME
	fname <- name

	if(name %in% c('salmon', 'fish skull')){
		fname <- 'salmo_salar_WLC2016001_all'
		species <- 'salmo salar'
	}
	if(name %in% c('owl', 'bird skull')){
		fname <- 'bubo_virginianus_FMNH488595_all'
		species <- 'bubo virginianus'
	}

	if(grepl('salmo_salar', name, ignore.case=TRUE)) species <- 'salmo salar'
	if(grepl('bubo_virginianus', name, ignore.case=TRUE)) species <- 'bubo virginianus'

	# Make filename lowercase
	fname <- tolower(fname)

	if(species == 'bubo virginianus'){

		# GET LANDMARKS
		landmarks <- as.matrix(read.table(file=paste0(fdir, 'specimens/', fname, '.txt'), row.names=1))

		# GET LANDMARK-LINK ASSOCIATIONS
		lm_assoc_ref <- as.matrix(read.table(file=paste0(fdir, 'specimens/owl_link_associations.txt')))

		path_connect <- list(
			c("upper_bill_culmen[0-9]+", "nc_ub_L", "ju_ub_L", "upper_bill_tomium_L[0-9]+", "ub_tip", "upper_bill_culmen[0]+[1]"),
			c("upper_bill_culmen[0-9]+", "nc_ub_R", "ju_ub_R", "upper_bill_tomium_R[0-9]+", "ub_tip", "upper_bill_culmen[0]+[1]"),
			c("nc_qd_m_R", "nc_qd_l_R", "ju_qd_R", "mc_qd_u_R", "mc_qd_l_R", "mc_qd_m_R", "pt_qd_R", "nc_qd_m_R"),
			c("nc_qd_m_R", "op_sup_R", "op_dist_R", "op_inf_R", "pt_qd_R"),
			c("nc_qd_l_R", "op_sup_R", "nc_qd_m_R"),
			c("pa_lc_a_R", "palatine_lat_crest_R[0-9]+", "pa_pt_R", paste0("palatine_med_crest_R00", 30:10), paste0("palatine_med_crest_R000", 9:1), "pa_mc_a_R"),
			c("pa_lc_a_L", "palatine_lat_crest_L[0-9]+", "pa_pt_L", paste0("palatine_med_crest_L00", 30:10), paste0("palatine_med_crest_L000", 9:1), "pa_mc_a_L"),
			c("nc_qd_m_L", "op_sup_L", "op_dist_L", "op_inf_L", "pt_qd_L"),
			c("nc_qd_l_L", "op_sup_L", "nc_qd_m_L"),
			c("nc_qd_m_L", "nc_qd_l_L", "ju_qd_L", "mc_qd_u_L", "mc_qd_l_L", "mc_qd_m_L", "pt_qd_L", "nc_qd_m_L"),
			c("op_inf_L", "ju_qd_L"),
			c("op_inf_R", "ju_qd_R", "mc_qd_u_R", "mc_qd_l_R", "mc_qd_m_R", "pt_qd_R"),
			c("op_inf_L", "ju_qd_L", "mc_qd_u_L", "mc_qd_l_L", "mc_qd_m_L", "pt_qd_L"),
			c("nc_ub_R", "ju_ub_R"),
			c("nc_ub_L", "ju_ub_L"),
			c("nc_ub_R", "nc_ub_L"),
			c("cranium_sagittal[0-9]+", "cranium_occipital"),
			c("pt_qd_R", "pa_pt_R", "pa_ub_R"),
			c("pt_qd_L", "pa_pt_L", "pa_ub_L"),
			c("ju_qd_R", "ju_ub_R"),
			c("ju_qd_L", "ju_ub_L")
		)
		
		if(fname %in% c('bubo_virginianus_fmnh488595', 'bubo_virginianus_fmnh492487', 'bubo_virginianus_fmnh493661')){
			path_connect[[length(path_connect)+1]] <- c("nc_ub_L", "ub_tip")
			path_connect[[length(path_connect)+1]] <- c("nc_ub_R", "ub_tip")
		}
	}

	if(species == 'salmo salar'){

		# GET LANDMARKS
		landmarks <- as.matrix(read.table(file=paste0(fdir, 'specimens/', fname, '.txt'), row.names=1))

		# GET LANDMARK-LINK ASSOCIATIONS
		lm_assoc_ref <- as.matrix(read.table(file=paste0(fdir, 'specimens/salmon_link_associations.txt')))

		# SET PATH CONNECTIONS
		path_connect <- list(
			c("ljaw_symph[0-9]+"),
			c("premax_midline[0-9]+"),
			c("cranium_sup[0-9]+"),
			c("cranium_pos[0-9]+"),

			c("ljaw_sup_L[0-9]+"),
			c("ljaw_inf_L[0-9]+"),
			c("hyoman_joint_L[0-9]+"),
			c("hyoman_ant_L[0-9]+"),
			c("hyoman_sup_L[0-9]+"),
			c("hyoman_inf_L[0-9]+"),
			c("hyoman_pos_L[0-9]+"),
			c("operc_pos_L[0-9]+"),
			c("cerhyal_ant_L[0-9]+"),
			c("cerhyal_inf_L[0-9]+"),
			c("cerhyal_pos_L[0-9]+"),
			c("cerhyal_sup_L[0-9]+"),
			c("epihyal_ant_L[0-9]+"),
			c("epihyal_inf_L[0-9]+"),
			c("epihyal_pos_L[0-9]+"),
			c("hypohyal_sup_L[0-9]+"),
			c("hypohyal_inf_L[0-9]+"),
			c("orb_ridge_L[0-9]+"),
			c("premax_inf_L[0-9]+"),
			c("premax_pos_L[0-9]+"),
			c("premax_orb_L[0-9]+"),

			c("ljaw_sup_R[0-9]+"),
			c("ljaw_inf_R[0-9]+"),
			c("hyoman_joint_R[0-9]+"),
			c("hyoman_ant_R[0-9]+"),
			c("hyoman_sup_R[0-9]+"),
			c("hyoman_inf_R[0-9]+"),
			c("hyoman_pos_R[0-9]+"),
			c("operc_pos_R[0-9]+"),
			c("cerhyal_ant_R[0-9]+"),
			c("cerhyal_inf_R[0-9]+"),
			c("cerhyal_pos_R[0-9]+"),
			c("cerhyal_sup_R[0-9]+"),
			c("epihyal_ant_R[0-9]+"),
			c("epihyal_inf_R[0-9]+"),
			c("epihyal_pos_R[0-9]+"),
			c("hypohyal_sup_R[0-9]+"),
			c("hypohyal_inf_R[0-9]+"),
			c("orb_ridge_R[0-9]+"),
			c("premax_inf_R[0-9]+"),
			c("premax_pos_R[0-9]+"),
			c("premax_orb_R[0-9]+"),
			c("ljaw_sup_L01", "lj_sy_sup", "ljaw_sup_R01"),
			c("ljaw_inf_L50", "lj_sy_inf", "ljaw_inf_R50")
		)

		if(fname %in% c('salmo_salar_wlc2016001', 'salmo_salar_wlc2016002', 'salmo_salar_wlc2016003')){
			path_connect[[length(path_connect)+1]] <- c("nc_su_p_R", "pc_su_R")
			path_connect[[length(path_connect)+1]] <- c("nc_su_p_L", "pc_su_L")
			path_connect[[length(path_connect)+1]] <- c("nc_su_a_R", "nc_su_p_R")
			path_connect[[length(path_connect)+1]] <- c("nc_su_a_L", "nc_su_p_L")
			path_connect[[length(path_connect)+1]] <- c("pc_su_L", "pc_ai_L", "pc_as_L", "pc_su_L")
			path_connect[[length(path_connect)+1]] <- c("pc_su_R", "pc_ai_R", "pc_as_R", "pc_su_R")
			path_connect[[length(path_connect)+1]] <- c("ac_hy_L", "ac_pi_L", "ac_ps_L", "ac_as_L", "ac_hy_L")
			path_connect[[length(path_connect)+1]] <- c("ac_hy_R", "ac_pi_R", "ac_ps_R", "ac_as_R", "ac_hy_R")
			path_connect[[length(path_connect)+1]] <- c("nc_su_a_L", "su_pt_L", "lj_qd_L")
			path_connect[[length(path_connect)+1]] <- c("nc_su_a_R", "su_pt_R", "lj_qd_R")
			path_connect[[length(path_connect)+1]] <- c("lj_qd_L", "lj_sy_sup", "lj_sy_inf", "lj_qd_L")
			path_connect[[length(path_connect)+1]] <- c("lj_qd_R", "lj_sy_sup", "lj_sy_inf", "lj_qd_R")
			path_connect[[length(path_connect)+1]] <- c("ac_as_L", "ac_as_R")
			path_connect[[length(path_connect)+1]] <- c("nc_su_a_L", "nc_su_a_R")
			path_connect[[length(path_connect)+1]] <- c("nc_su_p_L", "nc_su_p_R")
			path_connect[[length(path_connect)+1]] <- c("nc_vc", "nc_ep", "px_mid")
			path_connect[[length(path_connect)+1]] <- c("nc_su_a_L", "px_mid", "nc_su_a_R")
		}
	}

	# MATCH LANDMARK AND LINK ASSOCIATIONS
	lm_assoc <- rep(NA, nrow(landmarks))
	for(i in 1:nrow(landmarks)){
		for(j in 1:nrow(lm_assoc_ref)){
			if(grepl(lm_assoc_ref[j, 1], rownames(landmarks)[i])){
				lm_assoc[i] <- lm_assoc_ref[j, 2]
				break
			}
		}
	}

	rlist <- list(
		'landmarks'=landmarks,
		'path.connect'=path_connect,
		'lm.assoc'=lm_assoc
	)

	rlist
}
