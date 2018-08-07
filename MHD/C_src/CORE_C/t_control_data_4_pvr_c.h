/*
//  t_control_data_4_pvr_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_pvr_c_h_
#define t_control_data_4_pvr_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_data_4_psf_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"

struct pvr_plot_area_ctl_c{
	int maxlen;
	
	struct chara_ctl_array *pvr_area_ctl;
	struct chara2_real_ctl_array *surf_enhanse_ctl;
};

struct pvr_section_ctl_c{
	int maxlen;
	
	char *fname_sect_ctl;
	int iflag_psf_define_ctl;
	struct psf_define_ctl_c *psf_def_c;
	struct real_ctl_item *opacity_ctl;
};

struct pvr_isosurf_ctl_c{
	int maxlen;
	
	struct real_ctl_item *isosurf_value_ctl;
	struct real_ctl_item *opacity_ctl;
	struct chara_ctl_item *isosurf_type_ctl;
};

struct pvr_colorbar_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *colorbar_switch_ctl;
	struct chara_ctl_item *colorbar_scale_ctl;
	struct chara_ctl_item *zeromarker_flag_ctl;
	
	struct int_ctl_item *font_size_ctl;
	struct int_ctl_item *ngrid_cbar_ctl;
	
	struct real_ctl_item *cbar_min_ctl;
	struct real_ctl_item *cbar_max_ctl;
	
	struct chara_ctl_item *axis_switch_ctl;
};

struct pvr_movie_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *rotation_axis_ctl;
	struct int_ctl_item *num_frames_ctl;
};

struct pvr_ctl_c{
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
	
	int iflag_plot_area_ctl;
	struct pvr_plot_area_ctl_c *area_c;
	int iflag_modeview_ctl;
	char *pvr_modelview_file_name;
	struct modeview_ctl_c *mat_c;
	int iflag_colormap_ctl;
    char *pvr_colormap_file_name;
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

void alloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c);
void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c);
int read_pvr_plot_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_plot_area_ctl_c *area_c);
int write_pvr_plot_area_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_plot_area_ctl_c *area_c);

void alloc_pvr_section_ctl_c(struct pvr_section_ctl_c *pvr_sect_ctl);
void dealloc_pvr_section_ctl_c(struct pvr_section_ctl_c *pvr_sect_ctl);
int read_pvr_section_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_section_ctl_c *pvr_sect_ctl);
int write_pvr_section_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_section_ctl_c *pvr_sect_ctl);

void alloc_pvr_isosurf_ctl_c(struct pvr_isosurf_ctl_c *pvr_iso_ctl);
void dealloc_pvr_isosurf_ctl_c(struct pvr_isosurf_ctl_c *pvr_iso_ctl);
int read_pvr_isosurf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_isosurf_ctl_c *pvr_iso_ctl);
int write_pvr_isosurf_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_isosurf_ctl_c *pvr_iso_ctl);

void alloc_pvr_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c);
void dealloc_pvr_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c);
int read_pvr_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_colorbar_ctl_c *cbar_c);
int write_pvr_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colorbar_ctl_c *cbar_c);

void alloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c);
void dealloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c);
int read_pvr_movie_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_movie_ctl_c *movie_c);
int write_pvr_movie_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_movie_ctl_c *movie_c);

void alloc_pvr_sections_ctl_c(struct pvr_ctl_c *pvr_c);
int read_pvr_sections_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_ctl_c *pvr_c);
int write_pvr_sections_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_ctl_c *pvr_c);

void alloc_pvr_isosurfs_ctl_c(struct pvr_ctl_c *pvr_c);
int read_pvr_isosurfs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_ctl_c *pvr_c);
int write_pvr_isosurfs_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_ctl_c *pvr_c);

void alloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c);
void dealloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c);
void read_pvr_ctl_items(FILE *fp, char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_items(FILE *fp, int level, struct pvr_ctl_c *pvr_c);
int read_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_ctl_c *pvr_c);

int read_pvr_ctl_subfiles(char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c);
void write_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c);

int read_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_file_c(const char *file_name, struct pvr_ctl_c *pvr_c);

#endif /* t_control_data_4_pvr_c_h_ */
