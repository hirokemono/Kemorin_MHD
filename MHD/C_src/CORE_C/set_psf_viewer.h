/*
 *  set_psf_viewer.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/13.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef SET_PSF_VIEWER_
#define SET_PSF_VIEWER_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "skip_comment_c.h"
#include "set_new_patch_4_map_c.h"


/* prototypes */
void set_viewer_ucd_data(struct psf_data *viz_s, struct psf_data *viz_tmp);
void set_evolution_udt_data(struct psf_data *viz_s, struct psf_data *viz_tmp);

void set_ucd_with_mapping(struct psf_data *viz_s, struct psf_data *viz_tmp);
void set_evolution_map_udt_data(struct psf_data *viz_s, struct psf_data *viz_tmp);

#endif
