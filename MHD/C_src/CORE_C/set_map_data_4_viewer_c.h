
/*  set_map_data_4_viewer_c.h */
#ifndef SET_MAP_DATA_4_VIEWER_C_
#define SET_MAP_DATA_4_VIEWER_C_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_map_data_4_viewer_c.h"

/* prototypes */

void count_map_patch_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s);
void set_map_patch_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s);
void set_map_grid_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s);
void set_map_data_from_psf_c(struct psf_data *psf_s, struct psf_map_data *psf_map_s);

#endif
