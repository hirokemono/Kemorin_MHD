/*
//  t_control_data_4_iso_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_iso_c.h"

FILE *FP_ISO;

const char label_iso_head[KCHARA_C] = "isosurface_ctl";
const char label_old_iso_head[KCHARA_C] = "isosurf_rendering";

char * isosurface_control_head(){
    char * label = (char *)calloc(KCHARA_C, sizeof(char));
    strngcopy(label, label_iso_head);
    return label;
};

struct iso_define_ctl_c * init_iso_define_ctl_c(){
    struct iso_define_ctl_c *iso_def_c;
    if((iso_def_c = (struct iso_define_ctl_c *) malloc(sizeof(struct iso_define_ctl_c))) == NULL) {
        printf("malloc error for iso_define_ctl_c \n");
        exit(0);
    }
    iso_def_c->iflag_use = 0;
	iso_def_c->label_iso_define_ctl = init_label_iso_define_ctl();
	
	iso_def_c->isosurf_data_ctl = init_chara_ctl_item_c();
	iso_def_c->isosurf_comp_ctl = init_chara_ctl_item_c();
	
	iso_def_c->isosurf_value_ctl = init_real_ctl_item_c();
	
    iso_def_c->iso_area_list = init_chara_clist();
	
	return iso_def_c;
};

void dealloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c){
	dealloc_control_labels_f(iso_def_c->label_iso_define_ctl);
	dealloc_chara_ctl_item_c(iso_def_c->isosurf_data_ctl);
	dealloc_chara_ctl_item_c(iso_def_c->isosurf_comp_ctl);
	
	free(iso_def_c->isosurf_value_ctl);
	
	dealloc_chara_clist(iso_def_c->iso_area_list);
    free(iso_def_c);
	return;
};

void read_iso_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_define_ctl_c *iso_def_c){
    while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, iso_def_c->label_iso_define_ctl->label[ 0],
							  iso_def_c->isosurf_data_ctl);
		read_chara_ctl_item_c(buf, iso_def_c->label_iso_define_ctl->label[ 1],
							  iso_def_c->isosurf_comp_ctl);
		
		read_real_ctl_item_c(buf, iso_def_c->label_iso_define_ctl->label[ 2],
							 iso_def_c->isosurf_value_ctl);
		
		read_chara_clist(fp, buf, iso_def_c->label_iso_define_ctl->label[ 3],
						 iso_def_c->iso_area_list);
	};
    iso_def_c->iflag_use = 1;
	return;
};

int write_iso_define_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_define_ctl_c *iso_def_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, iso_def_c->label_iso_define_ctl->maxlen,
						   iso_def_c->label_iso_define_ctl->label[ 0],
						   iso_def_c->isosurf_data_ctl);
	write_chara_ctl_item_c(fp, level, iso_def_c->label_iso_define_ctl->maxlen,
						   iso_def_c->label_iso_define_ctl->label[ 1],
						   iso_def_c->isosurf_comp_ctl);
	
	write_real_ctl_item_c(fp, level, iso_def_c->label_iso_define_ctl->maxlen,
						  iso_def_c->label_iso_define_ctl->label[ 2],
						  iso_def_c->isosurf_value_ctl);
	
	write_chara_clist(fp, level, iso_def_c->label_iso_define_ctl->label[ 3],
					  iso_def_c->iso_area_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct iso_field_ctl_c * init_iso_field_ctl_c(){
    struct iso_field_ctl_c *iso_fld_c;
    if((iso_fld_c = (struct iso_field_ctl_c *) malloc(sizeof(struct iso_field_ctl_c))) == NULL) {
        printf("malloc error for iso_field_ctl_c \n");
        exit(0);
    }
    
    iso_fld_c->flag_iso_color = init_flag_iso_color();

    iso_fld_c->label_fld_on_iso_ctl = init_label_fld_on_psf_ctl();
	iso_fld_c->output_type_ctl = init_chara_ctl_item_c();
	
	iso_fld_c->output_value_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
    iso_fld_c->iso_out_field_list = init_chara2_clist();
	
	return iso_fld_c;
};

void dealloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c){
	
    dealloc_control_labels_f(iso_fld_c->flag_iso_color);

    dealloc_control_labels_f(iso_fld_c->label_fld_on_iso_ctl);
	dealloc_chara_ctl_item_c(iso_fld_c->output_type_ctl);
	free(iso_fld_c->output_value_ctl);
	
	dealloc_chara2_clist(iso_fld_c->iso_out_field_list);
    free(iso_fld_c);
	
	return;
};

void read_iso_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_field_ctl_c *iso_fld_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, iso_fld_c->label_fld_on_iso_ctl->label[ 0],
							  iso_fld_c->output_type_ctl);
		read_real_ctl_item_c(buf, iso_fld_c->label_fld_on_iso_ctl->label[ 2],
							 iso_fld_c->output_value_ctl);
		
		read_chara2_clist(fp, buf, iso_fld_c->label_fld_on_iso_ctl->label[ 1],
						  iso_fld_c->iso_out_field_list);
	};
    iso_fld_c->iflag_use = 1;
	return;
};

int write_iso_field_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_field_ctl_c *iso_fld_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, iso_fld_c->label_fld_on_iso_ctl->maxlen,
						   iso_fld_c->label_fld_on_iso_ctl->label[ 0],
						   iso_fld_c->output_type_ctl);
	write_real_ctl_item_c(fp, level, iso_fld_c->label_fld_on_iso_ctl->maxlen,
						  iso_fld_c->label_fld_on_iso_ctl->label[ 2],
						  iso_fld_c->output_value_ctl);
	
	write_chara2_clist(fp, level, iso_fld_c->label_fld_on_iso_ctl->label[ 1],
					   iso_fld_c->iso_out_field_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct iso_ctl_c * init_iso_ctl_c(){
    struct iso_ctl_c *iso_c;
    if((iso_c = (struct iso_ctl_c *) malloc(sizeof(struct iso_ctl_c))) == NULL) {
        printf("malloc error for iso_ctl_c \n");
        exit(0);
    }
	
	iso_c->label_iso_ctl_w_dpl = init_label_iso_ctl_w_dpl();
    iso_c->flag_iso_format = init_flag_iso_format();
	
	iso_c->iso_def_c = init_iso_define_ctl_c();
	iso_c->iso_fld_c = init_iso_field_ctl_c();
    iso_c->iso_fld_c->iflag_use = 0;
	
	iso_c->iso_file_head_ctl = init_chara_ctl_item_c();
	iso_c->iso_output_type_ctl = init_chara_ctl_item_c();
	return iso_c;
};

void dealloc_iso_ctl_c(struct iso_ctl_c *iso_c){
	dealloc_control_labels_f(iso_c->flag_iso_format);
    dealloc_control_labels_f(iso_c->label_iso_ctl_w_dpl);

    dealloc_iso_define_ctl_c(iso_c->iso_def_c);
	dealloc_iso_field_ctl_c(iso_c->iso_fld_c);
	
	dealloc_chara_ctl_item_c(iso_c->iso_file_head_ctl);
	dealloc_chara_ctl_item_c(iso_c->iso_output_type_ctl);
    free(iso_c);
	return;
};

int read_iso_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_ctl_c *iso_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, iso_c->label_iso_ctl_w_dpl->label[ 0],
							  iso_c->iso_file_head_ctl);
		read_chara_ctl_item_c(buf, iso_c->label_iso_ctl_w_dpl->label[ 4],
							  iso_c->iso_file_head_ctl);
		read_chara_ctl_item_c(buf, iso_c->label_iso_ctl_w_dpl->label[ 1],
							  iso_c->iso_output_type_ctl);
		
		if(right_begin_flag_c(buf, iso_c->label_iso_ctl_w_dpl->label[ 2]) > 0){
            read_iso_define_ctl_c(fp, buf, iso_c->label_iso_ctl_w_dpl->label[ 2],
                                  iso_c->iso_def_c);
		};
		if(right_begin_flag_c(buf, iso_c->label_iso_ctl_w_dpl->label[ 3]) > 0){
			read_iso_field_ctl_c(fp, buf, iso_c->label_iso_ctl_w_dpl->label[ 3],
                                 iso_c->iso_fld_c);
		};
        if(right_begin_flag_c(buf, iso_c->label_iso_ctl_w_dpl->label[ 5]) > 0){
            read_iso_field_ctl_c(fp, buf, iso_c->label_iso_ctl_w_dpl->label[ 5],
                                 iso_c->iso_fld_c);
        };
	};
	return 1;
};

int write_iso_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_ctl_c *iso_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, iso_c->label_iso_ctl_w_dpl->maxlen,
						   iso_c->label_iso_ctl_w_dpl->label[ 0],
						   iso_c->iso_file_head_ctl);
	write_chara_ctl_item_c(fp, level, iso_c->label_iso_ctl_w_dpl->maxlen,
						   iso_c->label_iso_ctl_w_dpl->label[ 1],
						   iso_c->iso_output_type_ctl);
	
    if(iso_c->iso_def_c->iflag_use > 0){
        fprintf(fp, "!\n");
		level = write_iso_define_ctl_c(fp, level, iso_c->label_iso_ctl_w_dpl->label[ 2],
									   iso_c->iso_def_c);
    };
    if(iso_c->iso_fld_c->iflag_use > 0){
        fprintf(fp, "!\n");
		level = write_iso_field_ctl_c(fp, level, iso_c->label_iso_ctl_w_dpl->label[ 3],
									  iso_c->iso_fld_c);
    };
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


int read_iso_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct iso_ctl_c *iso_c){
    int iflag = 0;
    
    if ((FP_ISO = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_read_line(FP_ISO, buf);
    if(right_begin_flag_c(buf, label_iso_head) > 0){
        iflag = read_iso_ctl_c(FP_ISO, buf, label_iso_head, iso_c);
    } else if(right_begin_flag_c(buf, label_old_iso_head) > 0){
        iflag = read_iso_ctl_c(FP_ISO, buf, label_old_iso_head, iso_c);
    };
    fclose(FP_ISO);
    
    return iflag;
};

int write_iso_ctl_file_c(const char *file_name, struct iso_ctl_c *iso_c){
    int level;

    if ((FP_ISO = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    level = write_iso_ctl_c(FP_ISO, 0, label_iso_head, iso_c);
    fclose(FP_ISO);
    
    return level;
};


