/*
//  m_LIC_control_labels_from_f.c
//
//  Created by Hiroaki Matsui on 06/15/20.
//
*/

#include "m_LIC_control_labels_from_f.h"

int num_label_psf_dirs_f();
void set_label_psf_dirs_f(char *name1);

int num_ctl_label_LIC_pvr_f();
int num_ctl_label_LIC_f();
int num_ctl_label_LIC_noise_f();
int num_ctl_label_LIC_kernel_f();
int num_label_LIC_colormap_f();

void set_ctl_label_LIC_pvr_f(char *name1);
void set_ctl_label_LIC_f(char *name1);
void set_ctl_label_LIC_noise_f(char *name1);
void set_ctl_label_LIC_kernel_f(char *name1);
void set_label_LIC_colormap_f(char *name1);

struct control_labels_f * init_label_LIC_pvr_ctl_f(){
	struct control_labels_f *label_LIC_pvr_ctl_f
			= init_control_labels_f(num_ctl_label_LIC_pvr_f, 
									set_ctl_label_LIC_pvr_f);
	return label_LIC_pvr_ctl_f;
};

struct control_labels_f * init_label_LIC_ctl_f(){
	struct control_labels_f *label_LIC_ctl_f
			= init_control_labels_f(num_ctl_label_LIC_f, 
									set_ctl_label_LIC_f);
	return label_LIC_ctl_f;
};

struct control_labels_f * init_label_LIC_noise_f(){
	struct control_labels_f *label_LIC_noise_f
			= init_control_labels_f(num_ctl_label_LIC_noise_f, 
									set_ctl_label_LIC_noise_f);
	return label_LIC_noise_f;
};

struct control_labels_f * init_label_LIC_kernel_f(){
	struct control_labels_f *label_LIC_kernel_f
			= init_control_labels_f(num_ctl_label_LIC_kernel_f, 
									set_ctl_label_LIC_kernel_f);
	return label_LIC_kernel_f;
};

struct control_labels_f * init_label_LIC_cmap(){
	struct control_labels_f *label_lic_cmap
			= init_control_labels_f(num_label_LIC_colormap_f, 
									set_label_LIC_colormap_f);
	return label_lic_cmap;
};

struct lic_control_labels * init_lic_control_labels(){
		struct lic_control_labels *lic_ctl_labls;
	if((lic_ctl_labls = (struct lic_control_labels *) malloc(sizeof(struct lic_control_labels))) == NULL){
		printf("malloc error for lic_control_labels\n");
		exit(0);
	};
	
	lic_ctl_labls->label_LIC_pvr_ctl_f = init_label_LIC_pvr_ctl_f();
	lic_ctl_labls->label_LIC_ctl_f =     init_label_LIC_ctl_f();
	lic_ctl_labls->label_LIC_noise_f =   init_label_LIC_noise_f();
	lic_ctl_labls->label_LIC_kernel_f =  init_label_LIC_kernel_f();
	
	lic_ctl_labls->label_lic_pixels =    init_label_pvr_pixels();
	lic_ctl_labls->label_lic_modelview = init_label_pvr_modelview();
	lic_ctl_labls->label_lic_project =   init_label_pvr_project();
	lic_ctl_labls->label_lic_streo =     init_label_pvr_streo();
	
	lic_ctl_labls->label_lic_area = init_label_pvr_area();
	
	lic_ctl_labls->label_lic_light =    init_label_pvr_light();
	lic_ctl_labls->label_lic_cmap =     init_label_LIC_cmap();
	lic_ctl_labls->label_lic_cbar =     init_label_pvr_cbar();
	lic_ctl_labls->label_lic_cmap_bar = init_label_pvr_cmap_bar();
	
	lic_ctl_labls->label_lic_section = init_label_pvr_section();
	lic_ctl_labls->label_lic_isosurf = init_label_pvr_isosurf();
	lic_ctl_labls->label_lic_movie = init_label_lic_movie();
	
	lic_ctl_labls->label_lic_dirs
			= init_control_labels_f(num_label_psf_dirs_f, 
									set_label_psf_dirs_f);
	lic_ctl_labls->flag_lic_movie_mode = init_flag_lic_movie_mode();
	lic_ctl_labls->flag_lic_isosurf_dir = init_flag_pvr_isosurf_dir();
	return lic_ctl_labls;
};
void dealloc_lic_control_labels(struct lic_control_labels *lic_ctl_labls){
	dealloc_control_labels_f(lic_ctl_labls->label_LIC_pvr_ctl_f);
	dealloc_control_labels_f(lic_ctl_labls->label_LIC_ctl_f);
	dealloc_control_labels_f(lic_ctl_labls->label_LIC_noise_f);
	dealloc_control_labels_f(lic_ctl_labls->label_LIC_kernel_f);
	
	dealloc_control_labels_f(lic_ctl_labls->label_lic_modelview);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_pixels);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_project);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_streo);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_area);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_light);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_cmap);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_cbar);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_cmap_bar);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_section);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_isosurf);
	dealloc_control_labels_f(lic_ctl_labls->label_lic_movie);
	
	dealloc_control_labels_f(lic_ctl_labls->label_lic_dirs);
	dealloc_control_labels_f(lic_ctl_labls->flag_lic_movie_mode);
	dealloc_control_labels_f(lic_ctl_labls->flag_lic_isosurf_dir);
	free(lic_ctl_labls);
	return;
};
void check_lic_control_labels(struct lic_control_labels *lic_ctl_labls){
	printf("Check lic_ctl_labls->label_LIC_pvr_ctl_f\n");
	check_control_labels_f(lic_ctl_labls->label_LIC_pvr_ctl_f);
	printf("Check lic_ctl_labls->label_LIC_ctl_f\n");
	check_control_labels_f(lic_ctl_labls->label_LIC_ctl_f);
	printf("Check lic_ctl_labls->label_LIC_noise_f\n");
	check_control_labels_f(lic_ctl_labls->label_LIC_noise_f);
	printf("Check lic_ctl_labls->label_LIC_kernel_f\n");
	check_control_labels_f(lic_ctl_labls->label_LIC_kernel_f);
	
	printf("Check lic_ctl_labls->label_lic_modelview\n");
	check_control_labels_f(lic_ctl_labls->label_lic_modelview);
	printf("Check lic_ctl_labls->label_lic_pixels\n");
	check_control_labels_f(lic_ctl_labls->label_lic_pixels);
	printf("Check lic_ctl_labls->label_lic_project\n");
	check_control_labels_f(lic_ctl_labls->label_lic_project);
	printf("Check lic_ctl_labls->label_lic_streo\n");
	check_control_labels_f(lic_ctl_labls->label_lic_streo);
	printf("Check lic_ctl_labls->label_lic_area\n");
	check_control_labels_f(lic_ctl_labls->label_lic_area);
	printf("Check lic_ctl_labls->label_lic_light\n");
	check_control_labels_f(lic_ctl_labls->label_lic_light);
	printf("Check lic_ctl_labls->label_lic_cmap\n");
	check_control_labels_f(lic_ctl_labls->label_lic_cmap);
	printf("Check lic_ctl_labls->label_lic_cbar\n");
	check_control_labels_f(lic_ctl_labls->label_lic_cbar);
	printf("Check lic_ctl_labls->label_lic_cmap_bar\n");
	check_control_labels_f(lic_ctl_labls->label_lic_cmap_bar);
	printf("Check lic_ctl_labls->label_lic_section\n");
	check_control_labels_f(lic_ctl_labls->label_lic_section);
	printf("Check lic_ctl_labls->label_lic_isosurf\n");
	check_control_labels_f(lic_ctl_labls->label_lic_isosurf);
	printf("Check lic_ctl_labls->label_lic_movie\n");
	check_control_labels_f(lic_ctl_labls->label_lic_movie);
	
	printf("Check lic_ctl_labls->label_lic_dirs\n");
	check_control_labels_f(lic_ctl_labls->label_lic_dirs);
	printf("Check lic_ctl_labls->label_lic_dirs\n");
	check_control_labels_f(lic_ctl_labls->flag_lic_movie_mode);
	printf("Check lic_ctl_labls->flag_lic_isosurf_dir\n");
	check_control_labels_f(lic_ctl_labls->flag_lic_isosurf_dir);
	return;
};

