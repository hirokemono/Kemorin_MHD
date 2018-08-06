/*
//  t_control_data_LIC_pvr_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/06.
*/

#ifndef t_control_data_LIC_pvr_c_h_
#define t_control_data_LIC_pvr_c_h_

#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_data_4_psf_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "t_control_data_4_pvr_c.h"
#include "t_control_data_LIC_c.h"

struct LIC_pvr_ctl_c{
	int maxlen;
	
	char *view_file_ctl;
	char *color_file_ctl;
	
	struct chara_ctl_item *updated_ctl;
	
	struct chara_ctl_item *file_head_ctl;
	struct chara_ctl_item *file_fmt_ctl;
	
	struct chara_ctl_item *monitoring_ctl;
	struct chara_ctl_item *transparent_ctl;
	struct chara_ctl_item *streo_ctl;
	struct chara_ctl_item *anaglyph_ctl;
	
	struct chara_ctl_item *pvr_field_ctl;
	struct chara_ctl_item *pvr_comp_ctl;
	
	int iflag_lic_ctl;
	struct lic_ctl_c *lic_c;
	
	int iflag_plot_area_ctl;
	struct pvr_plot_area_ctl_c *area_c;
	int iflag_modeview_ctl;
	struct modeview_ctl_c *mat_c;
	int iflag_colormap_ctl;
	struct colormap_ctl_c *color_c;
	int iflag_lighting_ctl;
	struct lighting_ctl_c *light_c;
	int iflag_pvr_colorbar_ctl;
	struct pvr_colorbar_ctl_c *cbar_c;
	int iflag_pvr_movie_ctl;
	struct pvr_movie_ctl_c *movie_c;
	
	int num_pvr_sect_ctl;
	struct pvr_section_ctl_c **pvr_sect_ctl;
	int num_pvr_iso_ctl;
	struct pvr_isosurf_ctl_c **pvr_iso_ctl;
};

/* prototypes */

void alloc_lic_pvr_sections_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c);
int read_lic_pvr_sections_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_pvr_ctl_c *lic_pvr_c);
int write_lic_pvr_sections_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_pvr_ctl_c *lic_pvr_c);

void alloc_lic_pvr_isosurfs_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c);
int read_lic_pvr_isosurfs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_pvr_ctl_c *lic_pvr_c);
int write_lic_pvr_isosurfs_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_pvr_ctl_c *lic_pvr_c);


void alloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *pvr_c);
void dealloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *pvr_c);
int read_LIC_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct LIC_pvr_ctl_c *pvr_c);
int write_LIC_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct LIC_pvr_ctl_c *pvr_c);


#endif /* t_control_data_LIC_pvr_c_h_ */
