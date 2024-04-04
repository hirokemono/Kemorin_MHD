/*
//  t_control_data_4_psf_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_psf_c.h"

FILE *FP_PSF;

const char label_psf_head[KCHARA_C] = "cross_section_ctl";
const char label_old_psf_head[KCHARA_C] = "surface_rendering";

struct psf_define_ctl_c * init_psf_define_ctl_c(){
    struct psf_define_ctl_c *psf_def_c;
    if((psf_def_c = (struct psf_define_ctl_c *) malloc(sizeof(struct psf_define_ctl_c))) == NULL) {
        printf("malloc error for psf_define_ctl_c \n");
        exit(0);
    }
	
	psf_def_c->label_psf_def_ctl = init_label_psf_def_ctl();
	psf_def_c->section_method_ctl = init_chara_ctl_item_c();
	
    psf_def_c->psf_coefs_list =  init_chara_real_clist();
    psf_def_c->psf_normal_list = init_chara_real_clist();
    psf_def_c->psf_center_list = init_chara_real_clist();
    psf_def_c->psf_axis_list =   init_chara_real_clist();

    sprintf(psf_def_c->psf_coefs_list->c1_name, "Term");
    sprintf(psf_def_c->psf_normal_list->c1_name, "Direction");
    sprintf(psf_def_c->psf_center_list->c1_name, "Direction");
    sprintf(psf_def_c->psf_axis_list->c1_name, "Direction");
    sprintf(psf_def_c->psf_coefs_list->r1_name, "Value");
    sprintf(psf_def_c->psf_normal_list->r1_name, "Value");
    sprintf(psf_def_c->psf_center_list->r1_name, "Value");
    sprintf(psf_def_c->psf_axis_list->r1_name, "Value");
	
	psf_def_c->radius_psf_ctl = init_real_ctl_item_c();
	psf_def_c->psf_group_name_ctl = init_chara_ctl_item_c();
	
    psf_def_c->psf_area_list = init_chara_clist();
	
	return psf_def_c;
};

void dealloc_psf_define_ctl_c(struct psf_define_ctl_c *psf_def_c){
	
	dealloc_control_labels_f(psf_def_c->label_psf_def_ctl);
	dealloc_chara_ctl_item_c(psf_def_c->section_method_ctl);
	
	dealloc_chara_real_clist(psf_def_c->psf_coefs_list);
	dealloc_chara_real_clist(psf_def_c->psf_normal_list);
	dealloc_chara_real_clist(psf_def_c->psf_center_list);
	dealloc_chara_real_clist(psf_def_c->psf_axis_list);
	
	free(psf_def_c->radius_psf_ctl);
	dealloc_chara_ctl_item_c(psf_def_c->psf_group_name_ctl);
	
	dealloc_chara_clist(psf_def_c->psf_area_list);	
    free(psf_def_c);    
	return;
};

int read_psf_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_define_ctl_c *psf_def_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, psf_def_c->label_psf_def_ctl->label[ 0],
							  psf_def_c->section_method_ctl);
		
		read_chara_real_clist(fp, buf, psf_def_c->label_psf_def_ctl->label[ 1],
							  psf_def_c->psf_coefs_list);
		read_chara_real_clist(fp, buf, psf_def_c->label_psf_def_ctl->label[ 2],
							  psf_def_c->psf_normal_list);
		read_chara_real_clist(fp, buf, psf_def_c->label_psf_def_ctl->label[ 3],
							  psf_def_c->psf_axis_list);
		read_chara_real_clist(fp, buf, psf_def_c->label_psf_def_ctl->label[ 4],
							  psf_def_c->psf_center_list);
		
		read_real_ctl_item_c(buf, psf_def_c->label_psf_def_ctl->label[ 5],
							 psf_def_c->radius_psf_ctl);
		
		read_chara_ctl_item_c(buf, psf_def_c->label_psf_def_ctl->label[ 6],
							  psf_def_c->psf_group_name_ctl);
		
		read_chara_clist(fp, buf, psf_def_c->label_psf_def_ctl->label[ 7],
						 psf_def_c->psf_area_list);
	};
	return 1;
};

int write_psf_define_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_define_ctl_c *psf_def_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, psf_def_c->label_psf_def_ctl->maxlen,
						   psf_def_c->label_psf_def_ctl->label[ 0],
						   psf_def_c->section_method_ctl);
	
	write_chara_real_clist(fp, level, psf_def_c->label_psf_def_ctl->label[ 1],
						   psf_def_c->psf_coefs_list);
	write_chara_real_clist(fp, level, psf_def_c->label_psf_def_ctl->label[ 2],
						   psf_def_c->psf_normal_list);
	write_chara_real_clist(fp, level, psf_def_c->label_psf_def_ctl->label[ 3],
						   psf_def_c->psf_axis_list);
	write_chara_real_clist(fp, level, psf_def_c->label_psf_def_ctl->label[ 4],
						   psf_def_c->psf_center_list);
	
	write_real_ctl_item_c(fp, level,  psf_def_c->label_psf_def_ctl->maxlen,
						  psf_def_c->label_psf_def_ctl->label[ 5],
						  psf_def_c->radius_psf_ctl);
	
	write_chara_ctl_item_c(fp, level,  psf_def_c->label_psf_def_ctl->maxlen,
						   psf_def_c->label_psf_def_ctl->label[ 6],
						   psf_def_c->psf_group_name_ctl);
	
	write_chara_clist(fp, level, psf_def_c->label_psf_def_ctl->label[ 7],
					  psf_def_c->psf_area_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct psf_field_ctl_c * init_psf_field_ctl_c(){
    struct psf_field_ctl_c *psf_fld_c;
    if((psf_fld_c = (struct psf_field_ctl_c *) malloc(sizeof(struct psf_field_ctl_c))) == NULL){
        printf("malloc error for psf_field_ctl_c \n");
        exit(0);
    }
	
	psf_fld_c->label_fld_on_psf_ctl = init_label_fld_on_psf_ctl();
    psf_fld_c->psf_out_field_list = init_chara2_clist();
	return psf_fld_c;
};

void dealloc_psf_field_ctl_c(struct psf_field_ctl_c *psf_fld_c){
	dealloc_control_labels_f(psf_fld_c->label_fld_on_psf_ctl);
	dealloc_chara2_clist(psf_fld_c->psf_out_field_list);
    free(psf_fld_c);
	
	return;
};

int read_psf_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_field_ctl_c *psf_fld_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara2_clist(fp, buf, psf_fld_c->label_fld_on_psf_ctl->label[ 1],
						  psf_fld_c->psf_out_field_list);
	};
	return 1;
};

int write_psf_field_ctl_c(FILE *fp, int level, const char *label, 
						  struct psf_field_ctl_c *psf_fld_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara2_clist(fp, level, psf_fld_c->label_fld_on_psf_ctl->label[ 1],
					   psf_fld_c->psf_out_field_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct psf_ctl_c * init_psf_ctl_c(){
    struct psf_ctl_c *psf_c;
    if((psf_c = (struct psf_ctl_c *) malloc(sizeof(struct psf_ctl_c))) == NULL) {
        printf("malloc error for psf_ctl_c \n");
        exit(0);
    }
    
	psf_c->label_psf_ctl = init_label_psf_ctl();
	
	psf_c->iflag_surface_define = 0;
	psf_c->iflag_output_field = 0;
	
	psf_c->psf_def_c = init_psf_define_ctl_c();
    psf_c->psf_def_file_name = (char *)calloc(KCHARA_C, sizeof(char));
    
	psf_c->psf_fld_c = init_psf_field_ctl_c();
	
	psf_c->psf_file_head_ctl = init_chara_ctl_item_c();
	psf_c->psf_output_type_ctl = init_chara_ctl_item_c();
	return psf_c;
};

void dealloc_psf_ctl_c(struct psf_ctl_c *psf_c){
	dealloc_control_labels_f(psf_c->label_psf_ctl);
	dealloc_psf_define_ctl_c(psf_c->psf_def_c);
    free(psf_c->psf_def_file_name);
	dealloc_psf_field_ctl_c(psf_c->psf_fld_c);
	
	dealloc_chara_ctl_item_c(psf_c->psf_file_head_ctl);
	dealloc_chara_ctl_item_c(psf_c->psf_output_type_ctl);
	
	psf_c->iflag_surface_define = 0;
	psf_c->iflag_output_field = 0;
    free(psf_c);
	return;
};

int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_ctl_c *psf_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, psf_c->label_psf_ctl->label[ 0],
							  psf_c->psf_file_head_ctl);
		read_chara_ctl_item_c(buf, psf_c->label_psf_ctl->label[ 4],
							  psf_c->psf_file_head_ctl);
        
		read_chara_ctl_item_c(buf, psf_c->label_psf_ctl->label[ 1],
							  psf_c->psf_output_type_ctl);
		
		if(right_begin_flag_c(buf, psf_c->label_psf_ctl->label[ 2]) > 0){
			psf_c->iflag_surface_define
					= read_psf_define_ctl_c(fp, buf, 
											psf_c->label_psf_ctl->label[ 2],
											psf_c->psf_def_c);
        } else if(right_file_flag_c(buf, psf_c->label_psf_ctl->label[ 2])){
            psf_c->iflag_surface_define = read_file_flag_c(buf, psf_c->psf_def_file_name);
        };
		if(right_begin_flag_c(buf, psf_c->label_psf_ctl->label[ 3]) > 0){
			psf_c->iflag_output_field
					= read_psf_field_ctl_c(fp, buf, 
										   psf_c->label_psf_ctl->label[ 3],
										   psf_c->psf_fld_c);
		};
	};
	return 1;
};

int write_psf_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_ctl_c *psf_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, psf_c->label_psf_ctl->maxlen,
						   psf_c->label_psf_ctl->label[ 0],
						   psf_c->psf_file_head_ctl);
	write_chara_ctl_item_c(fp, level, psf_c->label_psf_ctl->maxlen,
						   psf_c->label_psf_ctl->label[ 1],
						   psf_c->psf_output_type_ctl);
	
    if(psf_c->iflag_surface_define == 1){
        fprintf(fp, "!\n");
		level = write_psf_define_ctl_c(fp, level, psf_c->label_psf_ctl->label[ 2],
									   psf_c->psf_def_c);
    } else if(psf_c->iflag_surface_define == -1){
        fprintf(fp, "!\n");
		write_file_flag_for_ctl_c(fp, level, psf_c->label_psf_ctl->label[ 2],
								  psf_c->psf_def_file_name);
    };
    if(psf_c->iflag_output_field > 0){
        fprintf(fp, "!\n");
		level = write_psf_field_ctl_c(fp, level, psf_c->label_psf_ctl->label[ 3],
									  psf_c->psf_fld_c);
    };
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

int read_psf_define_file_c(const char *file_name, char buf[LENGTHBUF],
						   struct control_labels_f *label_psf_ctl,
						   struct psf_define_ctl_c *psf_def_c){
	int iflag = 0;
	
    printf("read PVR sections file name: %s\n", file_name);
	if ((FP_PSF = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_PSF, buf);
	if(right_begin_flag_c(buf, label_psf_ctl->label[2]) > 0){
		iflag = read_psf_define_ctl_c(FP_PSF, buf, label_psf_ctl->label[2], psf_def_c);
	};
	fclose(FP_PSF);
	
	return iflag;
};
int write_psf_define_file_c(const char *file_name, 
							struct control_labels_f *label_psf_ctl,
							struct psf_define_ctl_c *psf_def_c){
	int level;
	
    printf("write PVR sections file name: %s\n", file_name);
	if ((FP_PSF = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	level = write_psf_define_ctl_c(FP_PSF, 0, label_psf_ctl->label[2],
								   psf_def_c);
	fclose(FP_PSF);
	
	return level;
};

void rename_psf_define_file_c(struct psf_ctl_c *psf_c){
    if(psf_c->iflag_surface_define ==-1){
        strcat(psf_c->psf_def_file_name, "_2");
    }
    return;
};

int read_psf_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct psf_ctl_c *psf_c){
	int iflag = 0;
	
    printf("read PVR control file name: %s\n", file_name);
	if ((FP_PSF = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_PSF, buf);
	if(right_begin_flag_c(buf, label_psf_head) > 0){
		iflag = read_psf_ctl_c(FP_PSF, buf, label_psf_head, psf_c);
	} else if(right_begin_flag_c(buf, label_old_psf_head) > 0){
		iflag = read_psf_ctl_c(FP_PSF, buf, label_old_psf_head, psf_c);
	};
	fclose(FP_PSF);
	
    if(psf_c->iflag_surface_define ==-1){
        read_psf_define_file_c(psf_c->psf_def_file_name, buf,
                               psf_c->label_psf_ctl, psf_c->psf_def_c);
    };
    
	return iflag;
};
int write_psf_ctl_file_c(const char *file_name, struct psf_ctl_c *psf_c){
	int level;
    
    if(psf_c->iflag_surface_define ==-1){
		write_psf_define_file_c(psf_c->psf_def_file_name, psf_c->label_psf_ctl, 
								psf_c->psf_def_c);
    };
	
    printf("write PVR control file name: %s\n", file_name);
	if ((FP_PSF = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	level = write_psf_ctl_c(FP_PSF, 0, label_psf_head, psf_c);
	fclose(FP_PSF);
	
	return level;
};

