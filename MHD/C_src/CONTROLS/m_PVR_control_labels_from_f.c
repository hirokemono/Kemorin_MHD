/*
//  m_PVR_control_labels_from_f.c
//
//  Created by Hiroaki Matsui on 06/15/20.
//
*/

#include "m_PVR_control_labels_from_f.h"

int num_label_psf_dirs_f();
void set_label_psf_dirs_f(char *name1);


int num_label_pvr_ctl_f();
int num_label_pvr_ctl_w_dup_f();
int num_label_pvr_modelview_f();
int num_label_pvr_pixels_f();
int num_label_pvr_streo_f();
int num_label_pvr_projection_f();
int num_label_pvr_area_f();
int num_label_pvr_light_f();
int num_label_pvr_colormap_f();
int num_label_pvr_colorbar_f();
int num_label_pvr_cmap_bar_f();
int num_label_pvr_section_f();
int num_label_pvr_isosurface_f();
int num_label_pvr_movie_f();
int num_flag_pvr_movie_mode_f();
int num_flag_pvr_isosurf_dir_f();

void set_label_pvr_ctl_w_dup_f(char *name1);
void set_label_pvr_modelview_f(char *name1);
void set_label_pvr_pixels_f(char *name1);
void set_label_pvr_streo_f(char *name1);
void set_label_pvr_projection_f(char *name1);
void set_label_pvr_area_f(char *name1);
void set_label_pvr_light_f(char *name1);
void set_label_pvr_colormap_f(char *name1);
void set_label_pvr_colorbar_f(char *name1);
void set_label_pvr_cmap_bar_f(char *name1);
void set_label_pvr_section_f(char *name1);
void set_label_pvr_isosurface_f(char *name1);
void set_label_pvr_movie_f(char *name1);
void set_flag_pvr_movie_mode_f(char *name1);
void set_flag_pvr_isosurf_dir_f(char *name1);

int num_label_LIC_movie_f();
int num_flag_LIC_movie_mode_f();
void set_label_LIC_movie_f(char *name1);
void set_flag_LIC_movie_mode_f(char *name1);

struct control_labels_f * init_label_pvr_ctl_w_dpl(){
	struct control_labels_f *label_pvr_ctl_w_dpl
			= init_control_labels_f(num_label_pvr_ctl_w_dup_f, 
									set_label_pvr_ctl_w_dup_f);
	return label_pvr_ctl_w_dpl;
};

struct control_labels_f * init_label_pvr_pixels(){
	struct control_labels_f *label_pvr_pixels
			= init_control_labels_f(num_label_pvr_pixels_f, 
									set_label_pvr_pixels_f);
	return label_pvr_pixels;
};
struct control_labels_f * init_label_pvr_project(){
	struct control_labels_f *label_pvr_project
			= init_control_labels_f(num_label_pvr_projection_f, 
									set_label_pvr_projection_f);
	return label_pvr_project;
};
struct control_labels_f * init_label_pvr_streo(){
	struct control_labels_f *label_pvr_streo
			= init_control_labels_f(num_label_pvr_streo_f, 
									set_label_pvr_streo_f);
	return label_pvr_streo;
};

struct control_labels_f * init_label_pvr_modelview(){
	struct control_labels_f *label_pvr_modelview
			= init_control_labels_f(num_label_pvr_modelview_f, 
									set_label_pvr_modelview_f);
	return label_pvr_modelview;
};

struct control_labels_f * init_label_pvr_area(){
	struct control_labels_f *label_pvr_area
			= init_control_labels_f(num_label_pvr_area_f, 
									set_label_pvr_area_f);
	return label_pvr_area;
};

struct control_labels_f * init_label_pvr_light(){
	struct control_labels_f *label_pvr_light
			= init_control_labels_f(num_label_pvr_light_f, 
									set_label_pvr_light_f);
	return label_pvr_light;
};
struct control_labels_f * init_label_pvr_cmap(){
	struct control_labels_f *label_pvr_cmap
			= init_control_labels_f(num_label_pvr_colormap_f, 
									set_label_pvr_colormap_f);
	return label_pvr_cmap;
};
struct control_labels_f * init_label_pvr_cbar(){
	struct control_labels_f *label_pvr_cbar
			= init_control_labels_f(num_label_pvr_colorbar_f, 
									set_label_pvr_colorbar_f);
	return label_pvr_cbar;
};
struct control_labels_f * init_label_pvr_cmap_bar(){
	struct control_labels_f *label_pvr_cmap_bar
			= init_control_labels_f(num_label_pvr_cmap_bar_f, 
									set_label_pvr_cmap_bar_f);
	return label_pvr_cmap_bar;
};

struct control_labels_f * init_label_pvr_section(){
	struct control_labels_f *label_pvr_section
			= init_control_labels_f(num_label_pvr_section_f, 
									set_label_pvr_section_f);
	return label_pvr_section;
};
struct control_labels_f * init_label_pvr_isosurf(){
	struct control_labels_f *label_pvr_isosurf
			= init_control_labels_f(num_label_pvr_isosurface_f, 
									set_label_pvr_isosurface_f);
	return label_pvr_isosurf;
};
struct control_labels_f * init_flag_pvr_isosurf_dir(){
	struct control_labels_f *flag_pvr_isosurf_dir
			= init_control_labels_f(num_flag_pvr_isosurf_dir_f, 
									set_flag_pvr_isosurf_dir_f);
	return flag_pvr_isosurf_dir;
};


struct control_labels_f * init_label_lic_movie(){
	struct control_labels_f *label_lic_movie
			= init_control_labels_f(num_label_LIC_movie_f, 
									set_label_LIC_movie_f);
	return label_lic_movie;
};

struct control_labels_f * init_flag_lic_movie_mode(){
	struct control_labels_f *flag_lic_movie_mode
			= init_control_labels_f(num_flag_LIC_movie_mode_f, 
									set_flag_LIC_movie_mode_f);
	return flag_lic_movie_mode;
};

struct pvr_control_labels * init_pvr_control_labels(){
		struct pvr_control_labels *pvr_ctl_labls;
	if((pvr_ctl_labls = (struct pvr_control_labels *) malloc(sizeof(struct pvr_control_labels))) == NULL){
		printf("malloc error for pvr_control_labels\n");
		exit(0);
	};
	
	pvr_ctl_labls->num_label_pvr_ctl_c = num_label_pvr_ctl_f();
	pvr_ctl_labls->label_pvr_ctl_w_dpl = init_label_pvr_ctl_w_dpl();
	pvr_ctl_labls->label_pvr_pixels =    init_label_pvr_pixels();
	pvr_ctl_labls->label_pvr_modelview = init_label_pvr_modelview();
	pvr_ctl_labls->label_pvr_project =   init_label_pvr_project();
	pvr_ctl_labls->label_pvr_streo =     init_label_pvr_streo();
	
	pvr_ctl_labls->label_pvr_area = init_label_pvr_area();
	
	pvr_ctl_labls->label_pvr_light =    init_label_pvr_light();
	pvr_ctl_labls->label_pvr_cmap =     init_label_pvr_cmap();
	pvr_ctl_labls->label_pvr_cbar =     init_label_pvr_cbar();
	pvr_ctl_labls->label_pvr_cmap_bar = init_label_pvr_cmap_bar();
	
	pvr_ctl_labls->label_pvr_section = init_label_pvr_section();
	pvr_ctl_labls->label_pvr_isosurf = init_label_pvr_isosurf();
	
	pvr_ctl_labls->num_label_pvr_movie_c = num_label_pvr_movie_f();
	pvr_ctl_labls->label_lic_movie = init_label_lic_movie();
	
	pvr_ctl_labls->label_pvr_dirs
			= init_control_labels_f(num_label_psf_dirs_f, 
									set_label_psf_dirs_f);
	
	pvr_ctl_labls->num_flag_pvr_movie_mode_c
			= num_flag_pvr_movie_mode_f();
	pvr_ctl_labls->flag_lic_movie_mode = init_flag_lic_movie_mode();
	
	pvr_ctl_labls->flag_pvr_isosurf_dir = init_flag_pvr_isosurf_dir();
	return pvr_ctl_labls;
};

void dealloc_pvr_control_labels(struct pvr_control_labels *pvr_ctl_labls){
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_ctl_w_dpl);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_modelview);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_pixels);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_project);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_streo);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_area);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_light);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_cmap);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_cbar);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_cmap_bar);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_section);
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_isosurf);
	dealloc_control_labels_f(pvr_ctl_labls->label_lic_movie);
	
	dealloc_control_labels_f(pvr_ctl_labls->label_pvr_dirs);
	dealloc_control_labels_f(pvr_ctl_labls->flag_pvr_isosurf_dir);
	dealloc_control_labels_f(pvr_ctl_labls->flag_lic_movie_mode);
	free(pvr_ctl_labls);
	return;
};

void check_pvr_control_labels(struct pvr_control_labels *pvr_ctl_labls){
	printf("Check pvr_ctl_labls->num_label_pvr_ctl_c %d\n", 
		   pvr_ctl_labls->num_label_pvr_ctl_c);
	printf("Check pvr_ctl_labls->label_pvr_ctl_w_dpl\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_ctl_w_dpl);
	printf("Check pvr_ctl_labls->label_pvr_modelview\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_modelview);
	printf("Check pvr_ctl_labls->label_pvr_pixels\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_pixels);
	printf("Check pvr_ctl_labls->label_pvr_project\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_project);
	printf("Check pvr_ctl_labls->label_pvr_streo\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_streo);
	
	printf("Check pvr_ctl_labls->label_pvr_area\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_area);
	printf("Check pvr_ctl_labls->label_pvr_light\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_light);
	printf("Check pvr_ctl_labls->label_pvr_cmap\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_cmap);
	printf("Check pvr_ctl_labls->label_pvr_cbar\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_cbar);
	printf("Check pvr_ctl_labls->label_pvr_cmap_bar\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_cmap_bar);
	printf("Check pvr_ctl_labls->label_pvr_section\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_section);
	printf("Check pvr_ctl_labls->label_pvr_isosurf\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_isosurf);
	
	printf("Check pvr_ctl_labls->num_label_pvr_movie_c %d\n", 
		   pvr_ctl_labls->num_label_pvr_movie_c);
	printf("Check pvr_ctl_labls->label_lic_movie\n");
	check_control_labels_f(pvr_ctl_labls->label_lic_movie);
	
	printf("Check pvr_ctl_labls->label_pvr_dirs\n");
	check_control_labels_f(pvr_ctl_labls->label_pvr_dirs);
	printf("Check pvr_ctl_labls->flag_pvr_isosurf_dir\n");
	check_control_labels_f(pvr_ctl_labls->flag_pvr_isosurf_dir);
	
	printf("Check pvr_ctl_labls->num_label_pvr_movie_c %d\n", 
		   pvr_ctl_labls->num_flag_pvr_movie_mode_c);
	printf("Check pvr_ctl_labls->flag_lic_movie_mode\n");
	check_control_labels_f(pvr_ctl_labls->flag_lic_movie_mode);
	return;
};

