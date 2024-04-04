/*
//  m_FLINE_control_labels_from_f.c
//
//  Created by Hiroaki Matsui on 06/15/20.
//
*/

#include "m_FLINE_control_labels_from_f.h"

int num_label_psf_dirs_f();
void set_label_psf_dirs_f(char *name1);

int num_label_fline_ctl_f();
int num_fline_start_flags_f();
int num_fline_direction_flags_f();
int num_fline_seeds_flags_f();

void set_label_fline_ctl_f(char *name1);
void set_fline_start_flags_f(char *name1);
void set_fline_direction_flags_f(char *name1);
void set_fline_seeds_flags_f(char *name1);


struct fline_control_labels * init_fline_control_labels(){
		struct fline_control_labels *fline_ctl_lbls;
	if((fline_ctl_lbls = (struct fline_control_labels *) malloc(sizeof(struct fline_control_labels))) == NULL){
		printf("malloc error for fline_control_labels\n");
		exit(0);
	};
	
	fline_ctl_lbls->label_fline_ctl
			= init_control_labels_f(num_label_fline_ctl_f, 
									set_label_fline_ctl_f);
	
	fline_ctl_lbls->fline_start_flags
			= init_control_labels_f(num_fline_start_flags_f, 
									set_fline_start_flags_f);
	fline_ctl_lbls->fline_direction_flags
			= init_control_labels_f(num_fline_direction_flags_f, 
									set_fline_direction_flags_f);
	fline_ctl_lbls->fline_seeds_flags
			= init_control_labels_f(num_fline_seeds_flags_f, 
									set_fline_seeds_flags_f);
	return fline_ctl_lbls;
};

void dealloc_fline_control_labels(struct fline_control_labels *fline_ctl_lbls){
	dealloc_control_labels_f(fline_ctl_lbls->label_fline_ctl);
	
	dealloc_control_labels_f(fline_ctl_lbls->fline_start_flags);
	dealloc_control_labels_f(fline_ctl_lbls->fline_direction_flags);
	dealloc_control_labels_f(fline_ctl_lbls->fline_seeds_flags);
	
	free(fline_ctl_lbls);
	return;
};

void check_fline_control_labels(struct fline_control_labels *fline_ctl_lbls){
	printf("Check fline_ctl_lbls->label_fline_ctl\n");
	check_control_labels_f(fline_ctl_lbls->label_fline_ctl);
	
	printf("Check fline_ctl_lbls->fline_start_flags\n");
	check_control_labels_f(fline_ctl_lbls->fline_start_flags);
	printf("Check fline_ctl_lbls->fline_direction_flags\n");
	check_control_labels_f(fline_ctl_lbls->fline_direction_flags);
	printf("Check fline_ctl_lbls->fline_seeds_flags\n");
	check_control_labels_f(fline_ctl_lbls->fline_seeds_flags);
	return;
};

