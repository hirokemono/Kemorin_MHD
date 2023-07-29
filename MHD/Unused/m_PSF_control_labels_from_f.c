/*
//  m_PSF_control_labels_from_f.c
//
//  Created by Hiroaki Matsui on 06/15/20.
//
*/

#include "m_PSF_control_labels_from_f.h"

int num_label_psf_format_f();
int num_label_iso_format_f();

void set_ctl_label_psf_format_f(char *name1);
void set_ctl_label_iso_format_f(char *name1);

int num_label_iso_ctl_f();
int num_label_iso_ctl_w_dpl_f();
int num_label_iso_define_control_f();
int num_label_iso_type_f();

void set_label_iso_ctl_w_dpl_f(char *name1);
void set_label_iso_define_control_f(char *name1);
void set_label_iso_type_f(char *name1);


int num_label_fld_on_psf_control_f();
int num_label_psf_define_control_f();
int num_label_psf_ctl_w_dpl_f();
int num_label_psf_def_type_f();
int num_label_psf_def_type_grp_f();
int num_label_psf_dirs_f();
int num_label_psf_coefs_f();

void set_label_psf_define_control_f(char *name1);
void set_label_fld_on_psf_control_f(char *name1);
void set_label_psf_ctl_w_dpl_f(char *name1);
void set_label_psf_def_type_grp_f(char *name1);
void set_label_psf_dirs_f(char *name1);
void set_label_psf_coefs_f(char *name1);

void set_primary_psf_format_flag_f(char *name1);
void set_primary_iso_format_flag_f(char *name1);
void set_primary_section_coef_flag_f(char *name1);


struct control_labels_f * init_label_psf_ctl(){
	struct control_labels_f *label_psf_ctl
			= init_control_labels_f(num_label_psf_ctl_w_dpl_f, 
									set_label_psf_ctl_w_dpl_f);
	return label_psf_ctl;
};

struct control_labels_f * init_label_psf_def_ctl(){
	struct control_labels_f *label_psf_def_ctl
			= init_control_labels_f(num_label_psf_define_control_f, 
									set_label_psf_define_control_f);
	return label_psf_def_ctl;
};

struct control_labels_f * init_label_fld_on_psf_ctl(){
	struct control_labels_f *label_fld_on_psf_ctl
			= init_control_labels_f(num_label_fld_on_psf_control_f, 
									set_label_fld_on_psf_control_f);
	return label_fld_on_psf_ctl;
};


struct control_labels_f * init_label_iso_ctl_w_dpl(){
	struct control_labels_f *label_iso_ctl_w_dpl
			= init_control_labels_f(num_label_iso_ctl_w_dpl_f, 
									set_label_iso_ctl_w_dpl_f);
	return label_iso_ctl_w_dpl;
};

struct control_labels_f * init_label_iso_define_ctl(){
	struct control_labels_f *label_iso_define_ctl
			= init_control_labels_f(num_label_iso_define_control_f, 
									set_label_iso_define_control_f);
	return label_iso_define_ctl;
};

struct control_labels_f * init_flag_iso_format(){
	struct control_labels_f *flag_iso_format
			= init_control_labels_f(num_label_iso_format_f, 
									set_ctl_label_iso_format_f);
	return flag_iso_format;
};

struct control_labels_f * init_flag_iso_color(){
	struct control_labels_f *flag_iso_color
			= init_control_labels_f(num_label_iso_type_f, set_label_iso_type_f);
	return flag_iso_color;
};

void set_primary_psf_format_flag_c(char *name){
	set_primary_psf_format_flag_f(name);
};
void set_primary_iso_format_flag_c(char *name){
	set_primary_iso_format_flag_f(name);
};

struct psf_control_labels * init_psf_control_labels(){
		struct psf_control_labels *psf_ctl_lbls;
	if((psf_ctl_lbls = (struct psf_control_labels *) malloc(sizeof(struct psf_control_labels))) == NULL){
		printf("malloc error for psf_control_labels\n");
		exit(0);
	};
	
	psf_ctl_lbls->label_psf_ctl =        init_label_psf_ctl();
	psf_ctl_lbls->label_psf_def_ctl =    init_label_psf_def_ctl();
	psf_ctl_lbls->label_fld_on_psf_ctl = init_label_fld_on_psf_ctl();
	
	psf_ctl_lbls->num_label_psf_def_type_c = num_label_psf_def_type_f();
	psf_ctl_lbls->label_psf_define_ctl
			= init_control_labels_f(num_label_psf_def_type_grp_f, 
									set_label_psf_def_type_grp_f);
	psf_ctl_lbls->label_psf_format
			= init_control_labels_f(num_label_psf_format_f, 
									set_ctl_label_psf_format_f);
	psf_ctl_lbls->label_psf_dirs
			= init_control_labels_f(num_label_psf_dirs_f, 
									set_label_psf_dirs_f);
	psf_ctl_lbls->label_psf_coefs
			= init_control_labels_f(num_label_psf_coefs_f, 
									set_label_psf_coefs_f);
	return psf_ctl_lbls;
};

void dealloc_psf_control_labels(struct psf_control_labels *psf_ctl_lbls){
	dealloc_control_labels_f(psf_ctl_lbls->label_psf_def_ctl);
	dealloc_control_labels_f(psf_ctl_lbls->label_fld_on_psf_ctl);
	dealloc_control_labels_f(psf_ctl_lbls->label_psf_ctl);
	dealloc_control_labels_f(psf_ctl_lbls->label_psf_define_ctl);
	dealloc_control_labels_f(psf_ctl_lbls->label_psf_format);
	dealloc_control_labels_f(psf_ctl_lbls->label_psf_dirs);
	dealloc_control_labels_f(psf_ctl_lbls->label_psf_coefs);
	free(psf_ctl_lbls);
	return;
};

void check_psf_control_labels(struct psf_control_labels *psf_ctl_lbls){
	printf("Check psf_ctl_lbls->label_psf_def_ctl\n");
	check_control_labels_f(psf_ctl_lbls->label_psf_def_ctl);
	printf("Check psf_ctl_lbls->label_fld_on_psf_ctl\n");
	check_control_labels_f(psf_ctl_lbls->label_fld_on_psf_ctl);
	printf("Check psf_ctl_lbls->label_psf_ctl\n");
	check_control_labels_f(psf_ctl_lbls->label_psf_ctl);
	
	printf("Check psf_ctl_lbls->num_label_psf_def_type_c %d\n", 
		   psf_ctl_lbls->num_label_psf_def_type_c);
	printf("Check psf_ctl_lbls->label_psf_define_ctl\n");
	check_control_labels_f(psf_ctl_lbls->label_psf_define_ctl);
	printf("Check psf_ctl_lbls->label_psf_format\n");
	check_control_labels_f(psf_ctl_lbls->label_psf_format);
	printf("Check psf_ctl_lbls->label_psf_dirs\n");
	check_control_labels_f(psf_ctl_lbls->label_psf_dirs);
	printf("Check psf_ctl_lbls->label_psf_coefs\n");
	check_control_labels_f(psf_ctl_lbls->label_psf_coefs);
	return;
};


struct iso_control_labels * init_iso_control_labels(){
		struct iso_control_labels *iso_ctl_lbls;
	if((iso_ctl_lbls = (struct iso_control_labels *) malloc(sizeof(struct iso_control_labels))) == NULL){
		printf("malloc error for iso_control_labels\n");
		exit(0);
	};
	
	iso_ctl_lbls->num_label_iso_ctl_c = num_label_iso_ctl_f();
	iso_ctl_lbls->label_iso_ctl_w_dpl = init_label_iso_ctl_w_dpl();
	iso_ctl_lbls->label_iso_define_ctl = init_label_iso_define_ctl();
	iso_ctl_lbls->label_fld_on_iso_ctl = init_label_fld_on_psf_ctl();
	
	iso_ctl_lbls->flag_iso_color = init_flag_iso_color();
	iso_ctl_lbls->flag_iso_format = init_flag_iso_format();
	return iso_ctl_lbls;
};

void dealloc_iso_control_labels(struct iso_control_labels *iso_ctl_lbls){
	dealloc_control_labels_f(iso_ctl_lbls->label_iso_ctl_w_dpl);
	dealloc_control_labels_f(iso_ctl_lbls->label_iso_define_ctl);
	dealloc_control_labels_f(iso_ctl_lbls->label_fld_on_iso_ctl);
	dealloc_control_labels_f(iso_ctl_lbls->flag_iso_color);
	dealloc_control_labels_f(iso_ctl_lbls->flag_iso_format);
	free(iso_ctl_lbls);
	return;
};

void check_iso_control_labels(struct iso_control_labels *iso_ctl_lbls){
	printf("Check iso_ctl_lbls->num_label_iso_ctl_c %d\n", 
		   iso_ctl_lbls->num_label_iso_ctl_c);
	printf("Check iso_ctl_lbls->label_iso_ctl_w_dpl\n");
	check_control_labels_f(iso_ctl_lbls->label_iso_ctl_w_dpl);
	printf("Check iso_ctl_lbls->label_iso_define_ctl\n");
	check_control_labels_f(iso_ctl_lbls->label_iso_define_ctl);
	printf("Check iso_ctl_lbls->label_fld_on_iso_ctl\n");
	check_control_labels_f(iso_ctl_lbls->label_fld_on_iso_ctl);
	printf("Check iso_ctl_lbls->flag_iso_color\n");
	check_control_labels_f(iso_ctl_lbls->flag_iso_color);
	printf("Check iso_ctl_lbls->flag_iso_format\n");
	check_control_labels_f(iso_ctl_lbls->flag_iso_format);
	return;
};

