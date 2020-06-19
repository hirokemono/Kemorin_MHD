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
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "t_control_data_4_pvr_movie_c.h"
#include "t_control_data_pvr_section_list.h"
#include "t_control_data_pvr_isosurf_list.h"
#include "m_PVR_control_labels_from_f.h"

#define NLBL_PVR_MOVIE_CTL      2
#define NLBL_PVR_CTL            19

struct pvr_plot_area_ctl_c{
    int iflag_use;
	struct control_labels_f *label_pvr_area;
	
	struct chara_clist *pvr_area_list;
	struct chara2_real_clist *surf_enhanse_ctl;
};

struct pvr_ctl_c{
    int iflag_use;
	int maxlen;
	struct control_labels_f *label_pvr_ctl_w_dpl;
	
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

    struct int_ctl_item *maxpe_composit_ctl;

	struct pvr_plot_area_ctl_c *area_c;
    
	char *pvr_modelview_file_name;
	struct modelview_ctl_c *mat_c;
    
	struct lighting_ctl_c *light_c;
	
	char *pvr_colormap_file_name;
	int iflag_cmap_cbar_ctl;
	struct pvr_colormap_bar_ctl_c *cmap_cbar_c;
	
	struct pvr_movie_ctl_c *movie_c;
	struct pvr_sect_ctl_list pvr_sect_c_list;
	struct pvr_iso_ctl_list  pvr_iso_c_list;
};

/* prototypes */
struct pvr_plot_area_ctl_c * init_pvr_plot_area_ctl_c();
void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c);
void read_pvr_plot_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_plot_area_ctl_c *area_c);
int write_pvr_plot_area_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_plot_area_ctl_c *area_c);

struct pvr_ctl_c * init_pvr_ctl_c();
void dealloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c);
void read_pvr_ctl_items(FILE *fp, char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_items(FILE *fp, int level, struct pvr_ctl_c *pvr_c);
void read_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_ctl_c *pvr_c);

void rename_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c);
void read_pvr_ctl_subfiles(char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c);
void write_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c);

void read_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct pvr_ctl_c *pvr_c);
int write_pvr_ctl_file_c(const char *file_name, struct pvr_ctl_c *pvr_c);

#endif /* t_control_data_4_pvr_c_h_ */
