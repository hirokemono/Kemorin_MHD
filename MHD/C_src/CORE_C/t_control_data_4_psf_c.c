/*
//  t_control_data_4_psf_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_psf_c.h"

#define NLBL_PSF_DEFINE_CTL   10
#define NLBL_PSF_FIELD_CTL    1
#define NLBL_PSF_CTL          5

FILE *FP_PSF;

const char label_psf_define_ctl[NLBL_PSF_DEFINE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"section_method"},
	/*[ 1]*/	{"coefs_ctl"},
	/*[ 2]*/	{"normal_vector"},
	/*[ 3]*/	{"center_position"},
	/*[ 4]*/	{"axial_length"},
	/*[ 5]*/	{"radius"},
	/*[ 6]*/	{"group_name"},
	/*[ 7]*/	{"section_area_ctl"},
	
    /*[ 8]*/    {"plot_area_ctl"},
    /*[ 9]*/    {"chosen_ele_grp_ctl"}
};

const char label_psf_field_ctl[NLBL_PSF_FIELD_CTL][KCHARA_C] = {
	/*[ 0]*/	{"output_field"}
};

const char label_psf_ctl[NLBL_PSF_CTL][KCHARA_C] = {
	/*[ 0]*/	{"section_file_prefix"},
	/*[ 1]*/	{"psf_output_type"},
	
	/*[ 2]*/	{"surface_define"},
	/*[ 3]*/	{"output_field_define"},
	
    /*[ 4]*/    {"psf_file_head"}
};

const char label_psf_head[KCHARA_C] = "cross_section_ctl";
const char label_old_psf_head[KCHARA_C] = "surface_rendering";



void alloc_psf_define_ctl_c(struct psf_define_ctl_c *psf_def_c){
	int i;
	
	psf_def_c->maxlen = 0;
	for (i=0;i<NLBL_PSF_DEFINE_CTL;i++){
		if(strlen(label_psf_define_ctl[i]) > psf_def_c->maxlen){
			psf_def_c->maxlen = (int) strlen(label_psf_define_ctl[i]);
		};
	};
	
	psf_def_c->section_method_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(psf_def_c->section_method_ctl);
	
	psf_def_c->psf_coefs_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	psf_def_c->psf_normal_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	psf_def_c->psf_center_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	psf_def_c->psf_axis_ctl = (struct chara_real_ctl_array *) malloc(sizeof(struct chara_real_ctl_array));
	init_ctl_cr_array(psf_def_c->psf_coefs_ctl);
	init_ctl_cr_array(psf_def_c->psf_normal_ctl);
	init_ctl_cr_array(psf_def_c->psf_center_ctl);
	init_ctl_cr_array(psf_def_c->psf_axis_ctl);
	
	psf_def_c->radius_psf_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	psf_def_c->psf_group_name_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	init_ctl_real_item(psf_def_c->radius_psf_ctl);
	alloc_ctl_chara_item(psf_def_c->psf_group_name_ctl);
	
	psf_def_c->psf_area_ctl = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	init_ctl_chara_array(psf_def_c->psf_area_ctl);
	
	return;
};

void dealloc_psf_define_ctl_c(struct psf_define_ctl_c *psf_def_c){
	
	dealloc_ctl_chara_item(psf_def_c->section_method_ctl);
	free(psf_def_c->section_method_ctl);
	
	dealloc_ctl_cr_array(psf_def_c->psf_coefs_ctl);
	dealloc_ctl_cr_array(psf_def_c->psf_normal_ctl);
	dealloc_ctl_cr_array(psf_def_c->psf_center_ctl);
	dealloc_ctl_cr_array(psf_def_c->psf_axis_ctl);
	free(psf_def_c->psf_coefs_ctl);
	free(psf_def_c->psf_normal_ctl);
	free(psf_def_c->psf_center_ctl);
	free(psf_def_c->psf_axis_ctl);
	
	free(psf_def_c->radius_psf_ctl);
	dealloc_ctl_chara_item(psf_def_c->psf_group_name_ctl);
	free(psf_def_c->psf_group_name_ctl);
	
	dealloc_ctl_chara_array(psf_def_c->psf_area_ctl);
	free(psf_def_c->psf_area_ctl);
	
	return;
};

int read_psf_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_define_ctl_c *psf_def_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_array_c(fp, buf, label_psf_define_ctl[ 9], psf_def_c->psf_area_ctl);
	};
	return 1;
};

int read_psf_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_define_ctl_c *psf_def_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_psf_define_ctl[ 0], psf_def_c->section_method_ctl);
		
		read_cr_ctl_array_c(fp, buf, label_psf_define_ctl[ 1], psf_def_c->psf_coefs_ctl);
		read_cr_ctl_array_c(fp, buf, label_psf_define_ctl[ 2], psf_def_c->psf_normal_ctl);
		read_cr_ctl_array_c(fp, buf, label_psf_define_ctl[ 3], psf_def_c->psf_center_ctl);
		read_cr_ctl_array_c(fp, buf, label_psf_define_ctl[ 4], psf_def_c->psf_axis_ctl);
		
		read_real_ctl_item_c(buf, label_psf_define_ctl[ 5], psf_def_c->radius_psf_ctl);
		
		read_character_ctl_item_c(buf, label_psf_define_ctl[ 6], psf_def_c->psf_group_name_ctl);
		
		read_character_ctl_array_c(fp, buf, label_psf_define_ctl[ 7], psf_def_c->psf_area_ctl);
		
		if(right_begin_flag_c(buf, label_psf_define_ctl[ 8]) > 0){
			iflag = read_psf_area_ctl_c(fp, buf, 
						label_psf_define_ctl[ 8], psf_def_c);
		};
	};
	return 1;
};

int write_psf_define_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_define_ctl_c *psf_def_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 0], psf_def_c->section_method_ctl);
	
	if(psf_def_c->psf_coefs_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 1], psf_def_c->psf_coefs_ctl);
	
	if(psf_def_c->psf_normal_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 2], psf_def_c->psf_normal_ctl);
	
	if(psf_def_c->psf_center_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 3], psf_def_c->psf_center_ctl);
	
	if(psf_def_c->psf_axis_ctl->num > 0) fprintf(fp, "!\n");
	write_cr_ctl_array_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 4], psf_def_c->psf_axis_ctl);
	
	write_real_ctl_item_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 5], psf_def_c->radius_psf_ctl);
	
	write_character_ctl_item_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 6], psf_def_c->psf_group_name_ctl);
	
	if(psf_def_c->psf_area_ctl->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, psf_def_c->maxlen, label_psf_define_ctl[ 7], psf_def_c->psf_area_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_psf_field_ctl_c(struct psf_field_ctl_c *psf_fld_c){
	int i;
	
	psf_fld_c->maxlen = 0;
	for (i=0;i<NLBL_PSF_FIELD_CTL;i++){
		if(strlen(label_psf_field_ctl[i]) > psf_fld_c->maxlen){
			psf_fld_c->maxlen = (int) strlen(label_psf_field_ctl[i]);
		};
	};
	
	psf_fld_c->psf_out_field_ctl = (struct chara2_ctl_array *) malloc(sizeof(struct chara2_ctl_array));
	init_ctl_chara2_array(psf_fld_c->psf_out_field_ctl);
	
	return;
};

void dealloc_psf_field_ctl_c(struct psf_field_ctl_c *psf_fld_c){
	
	dealloc_ctl_chara2_array(psf_fld_c->psf_out_field_ctl);
	free(psf_fld_c->psf_out_field_ctl);
	
	return;
};

int read_psf_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_field_ctl_c *psf_fld_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_chara2_ctl_array_c(fp, buf, label_psf_field_ctl[ 0], psf_fld_c->psf_out_field_ctl);
	};
	return 1;
};

int write_psf_field_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_field_ctl_c *psf_fld_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara2_ctl_array_c(fp, level, psf_fld_c->maxlen, label_psf_field_ctl[ 0], psf_fld_c->psf_out_field_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_psf_ctl_c(struct psf_ctl_c *psf_c){
	int i;
	
	psf_c->maxlen = 0;
	for (i=0;i<NLBL_PSF_CTL;i++){
		if(strlen(label_psf_ctl[i]) > psf_c->maxlen){
			psf_c->maxlen = (int) strlen(label_psf_ctl[i]);
		};
	};
	
	psf_c->iflag_surface_define = 0;
	psf_c->iflag_output_field = 0;
	
	psf_c->psf_def_c = (struct psf_define_ctl_c *) malloc(sizeof(struct psf_define_ctl_c));
	alloc_psf_define_ctl_c(psf_c->psf_def_c);
    psf_c->psf_def_file_name = (char *)calloc(KCHARA_C, sizeof(char));
    
	psf_c->psf_fld_c = (struct psf_field_ctl_c *) malloc(sizeof(struct psf_field_ctl_c));
	alloc_psf_field_ctl_c(psf_c->psf_fld_c);
	
	psf_c->psf_file_head_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	psf_c->psf_output_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(psf_c->psf_file_head_ctl);
	alloc_ctl_chara_item(psf_c->psf_output_type_ctl);
	
	return;
};

void dealloc_psf_ctl_c(struct psf_ctl_c *psf_c){
	dealloc_psf_define_ctl_c(psf_c->psf_def_c);
	free(psf_c->psf_def_c);
    free(psf_c->psf_def_file_name);
	dealloc_psf_field_ctl_c(psf_c->psf_fld_c);
	free(psf_c->psf_fld_c);
	
	dealloc_ctl_chara_item(psf_c->psf_file_head_ctl);
	dealloc_ctl_chara_item(psf_c->psf_output_type_ctl);
	free(psf_c->psf_file_head_ctl);
	free(psf_c->psf_output_type_ctl);
	
	psf_c->iflag_surface_define = 0;
	psf_c->iflag_output_field = 0;
	
	return;
};

int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_ctl_c *psf_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_psf_ctl[ 0], psf_c->psf_file_head_ctl);
        read_character_ctl_item_c(buf, label_psf_ctl[ 4], psf_c->psf_file_head_ctl);
        
		read_character_ctl_item_c(buf, label_psf_ctl[ 1], psf_c->psf_output_type_ctl);
		
		if(right_begin_flag_c(buf, label_psf_ctl[ 2]) > 0){
			psf_c->iflag_surface_define = read_psf_define_ctl_c(fp, buf, 
						label_psf_ctl[ 2], psf_c->psf_def_c);
        } else if(right_file_flag_c(buf, label_psf_ctl[ 2], psf_c->psf_def_file_name)){
            psf_c->iflag_surface_define = -1;
        };
		if(right_begin_flag_c(buf, label_psf_ctl[ 3]) > 0){
			psf_c->iflag_output_field = read_psf_field_ctl_c(fp, buf, 
						label_psf_ctl[ 3], psf_c->psf_fld_c);
		};
	};
	return 1;
};

int write_psf_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_ctl_c *psf_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, psf_c->maxlen, label_psf_ctl[ 0], psf_c->psf_file_head_ctl);
	write_character_ctl_item_c(fp, level, psf_c->maxlen, label_psf_ctl[ 1], psf_c->psf_output_type_ctl);
	
    if(psf_c->iflag_surface_define == 1){
        fprintf(fp, "!\n");
        level = write_psf_define_ctl_c(fp, level, label_psf_ctl[ 2], psf_c->psf_def_c);
    } else if(psf_c->iflag_surface_define == -1){
        fprintf(fp, "!\n");
        write_file_flag_for_ctl_c(fp, level, label_psf_ctl[ 2],psf_c->psf_def_file_name);
    };
    if(psf_c->iflag_output_field > 0){
        fprintf(fp, "!\n");
        level = write_psf_field_ctl_c(fp, level, label_psf_ctl[ 3], psf_c->psf_fld_c);
    };
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


int read_psf_define_file_c(const char *file_name, char buf[LENGTHBUF],
			struct psf_define_ctl_c *psf_def_c){
	int iflag = 0;
	
	if ((FP_PSF = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
    skip_comment_c(FP_PSF);
	fgets(buf, LENGTHBUF, FP_PSF);
	if(right_begin_flag_c(buf, label_psf_ctl[2]) > 0){
		iflag = read_psf_define_ctl_c(FP_PSF, buf, label_psf_ctl[2], psf_def_c);
	};
	fclose(FP_PSF);
	
	return iflag;
};
int write_psf_define_file_c(const char *file_name, struct psf_define_ctl_c *psf_def_c){
	int level;
	
	if ((FP_PSF = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = write_psf_define_ctl_c(FP_PSF, 0, label_psf_ctl[2], psf_def_c);
	fclose(FP_PSF);
	
	return level;
};

int read_psf_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct psf_ctl_c *psf_c){
	int iflag = 0;
	
	if ((FP_PSF = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
    skip_comment_c(FP_PSF);
	fgets(buf, LENGTHBUF, FP_PSF);
	if(right_begin_flag_c(buf, label_psf_head) > 0){
		iflag = read_psf_ctl_c(FP_PSF, buf, label_psf_head, psf_c);
	} else if(right_begin_flag_c(buf, label_old_psf_head) > 0){
		iflag = read_psf_ctl_c(FP_PSF, buf, label_old_psf_head, psf_c);
	};
	fclose(FP_PSF);
	
    if(psf_c->iflag_surface_define ==-1){
        printf("Read PSF definition file name: %s\n", psf_c->psf_def_file_name);
        read_psf_define_file_c(psf_c->psf_def_file_name, buf,
                               psf_c->psf_def_c);
    };
    
	return iflag;
};
int write_psf_ctl_file_c(const char *file_name, struct psf_ctl_c *psf_c){
	int level;
    
    if(psf_c->iflag_surface_define ==-1){
        printf("write PSF definition file name: %s\n", psf_c->psf_def_file_name);
        write_psf_define_file_c(psf_c->psf_def_file_name, psf_c->psf_def_c);
    };
	
	if ((FP_PSF = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = write_psf_ctl_c(FP_PSF, 0, label_psf_head, psf_c);
	fclose(FP_PSF);
	
	return level;
};

