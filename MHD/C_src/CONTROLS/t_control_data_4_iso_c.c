/*
//  t_control_data_4_iso_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_iso_c.h"

#define NLBL_ISO_DEFINE_CTL   6
#define NLBL_ISO_FIELD_CTL    3
#define NLBL_ISO_CTL          6

FILE *FP_ISO;

const char label_iso_define_ctl[NLBL_ISO_DEFINE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"isosurf_field"},
	/*[ 1]*/	{"isosurf_component"},
	/*[ 2]*/	{"isosurf_value"},
	/*[ 3]*/	{"isosurf_area_ctl"},
    
    /*[ 4]*/    {"plot_area_ctl"},
    /*[ 5]*/    {"chosen_ele_grp_ctl"},
};

const char label_iso_field_ctl[NLBL_ISO_FIELD_CTL][KCHARA_C] = {
	/*[ 0]*/	{"result_type"},
	/*[ 1]*/	{"result_value"},
	/*[ 2]*/	{"output_field"}
};

const char label_iso_ctl[NLBL_ISO_CTL][KCHARA_C] = {
	/*[ 0]*/	{"isosurface_file_prefix"},
	/*[ 1]*/	{"iso_output_type"},
	
	/*[ 2]*/	{"isosurf_define"},
	/*[ 3]*/	{"field_on_isosurf"},
    
    /*[ 4]*/    {"iso_file_head"},
    /*[ 5]*/    {"isosurf_result_define"}
};
const char label_iso_head[KCHARA_C] = "isosurface_ctl";
const char label_old_iso_head[KCHARA_C] = "isosurf_rendering";


void get_label_iso_define_ctl(int index, char *label){
    if(index < NLBL_ISO_DEFINE_CTL) strngcopy(label, label_iso_define_ctl[index]);
    return;
};
void get_label_iso_field_ctl(int index, char *label){
    if(index < NLBL_ISO_FIELD_CTL) strngcopy(label, label_iso_field_ctl[index]);
    return;
};
void get_label_iso_ctl(int index, char *label){
    if(index < NLBL_ISO_CTL) strngcopy(label, label_iso_ctl[index]);
    return;
};

struct iso_define_ctl_c * init_iso_define_ctl_c(){
	int i;
    struct iso_define_ctl_c *iso_def_c;
    if((iso_def_c = (struct iso_define_ctl_c *) malloc(sizeof(struct iso_define_ctl_c))) == NULL) {
        printf("malloc error for iso_define_ctl_c \n");
        exit(0);
    }
	
	iso_def_c->maxlen = 0;
	for (i=0;i<NLBL_ISO_DEFINE_CTL;i++){
		if(strlen(label_iso_define_ctl[i]) > iso_def_c->maxlen){
			iso_def_c->maxlen = (int) strlen(label_iso_define_ctl[i]);
		};
	};
	
	iso_def_c->isosurf_data_ctl = init_chara_ctl_item_c();
	iso_def_c->isosurf_comp_ctl = init_chara_ctl_item_c();
	
	iso_def_c->isosurf_value_ctl = init_real_ctl_item_c();
	
    iso_def_c->iso_area_list = (struct chara_clist *) malloc(sizeof(struct chara_clist));
	init_chara_clist(iso_def_c->iso_area_list);
	
	return iso_def_c;
};

void dealloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c){
	
	dealloc_chara_ctl_item_c(iso_def_c->isosurf_data_ctl);
	dealloc_chara_ctl_item_c(iso_def_c->isosurf_comp_ctl);
	
	free(iso_def_c->isosurf_value_ctl);
	
	clear_chara_clist(iso_def_c->iso_area_list);
    free(iso_def_c->iso_area_list);
    free(iso_def_c);
	return;
};


int read_iso_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
                        struct iso_define_ctl_c *iso_def_c){
    while(find_control_end_flag_c(buf, label) == 0){
        skip_comment_read_line(fp, buf);
        
        read_chara_clist(fp, buf, label_iso_define_ctl[ 5], iso_def_c->iso_area_list);
    };
    return 1;
};

int read_iso_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_define_ctl_c *iso_def_c){
    int iflag;

    while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_iso_define_ctl[ 0], iso_def_c->isosurf_data_ctl);
		read_chara_ctl_item_c(buf, label_iso_define_ctl[ 1], iso_def_c->isosurf_comp_ctl);
		
		read_real_ctl_item_c(buf, label_iso_define_ctl[ 2], iso_def_c->isosurf_value_ctl);
		
		read_chara_clist(fp, buf, label_iso_define_ctl[ 3], iso_def_c->iso_area_list);
        
        if(right_begin_flag_c(buf, label_iso_define_ctl[ 4]) > 0){
            iflag = read_iso_area_ctl_c(fp, buf, 
                                        label_iso_define_ctl[ 4], iso_def_c);
        };
	};
	return 1;
};

int write_iso_define_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_define_ctl_c *iso_def_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 0], iso_def_c->isosurf_data_ctl);
	write_chara_ctl_item_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 1], iso_def_c->isosurf_comp_ctl);
	
	write_real_ctl_item_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 2], iso_def_c->isosurf_value_ctl);
	
	write_chara_clist(fp, level, label_iso_define_ctl[ 3], iso_def_c->iso_area_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct iso_field_ctl_c * init_iso_field_ctl_c(){
	int i;
    struct iso_field_ctl_c *iso_fld_c;
    if((iso_fld_c = (struct iso_field_ctl_c *) malloc(sizeof(struct iso_field_ctl_c))) == NULL) {
        printf("malloc error for iso_field_ctl_c \n");
        exit(0);
    }
    
	
	iso_fld_c->maxlen = 0;
	for (i=0;i<NLBL_ISO_FIELD_CTL;i++){
		if(strlen(label_iso_field_ctl[i]) > iso_fld_c->maxlen){
			iso_fld_c->maxlen = (int) strlen(label_iso_field_ctl[i]);
		};
	};
	
	iso_fld_c->iso_result_type_ctl = init_chara_ctl_item_c();
	
	iso_fld_c->result_value_iso_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
    iso_fld_c->iso_out_field_list = (struct chara2_clist *) malloc(sizeof(struct chara2_clist));
	init_chara2_clist(iso_fld_c->iso_out_field_list);
	
	return iso_fld_c;
};

void dealloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c){
	
	dealloc_chara_ctl_item_c(iso_fld_c->iso_result_type_ctl);
	free(iso_fld_c->result_value_iso_ctl);
	
	clear_chara2_clist(iso_fld_c->iso_out_field_list);
    free(iso_fld_c->iso_out_field_list);
    free(iso_fld_c);
	
	return;
};

int read_iso_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_field_ctl_c *iso_fld_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_iso_field_ctl[ 0], iso_fld_c->iso_result_type_ctl);
		read_real_ctl_item_c(buf, label_iso_field_ctl[ 1], iso_fld_c->result_value_iso_ctl);
		
		read_chara2_clist(fp, buf, label_iso_field_ctl[ 2], iso_fld_c->iso_out_field_list);
	};
	return 1;
};

int write_iso_field_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_field_ctl_c *iso_fld_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, iso_fld_c->maxlen, label_iso_field_ctl[ 0], iso_fld_c->iso_result_type_ctl);
	write_real_ctl_item_c(fp, level, iso_fld_c->maxlen, label_iso_field_ctl[ 1], iso_fld_c->result_value_iso_ctl);
	
	write_chara2_clist(fp, level, label_iso_field_ctl[ 2], iso_fld_c->iso_out_field_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct iso_ctl_c * init_iso_ctl_c(){
	int i;
    struct iso_ctl_c *iso_c;
    if((iso_c = (struct iso_ctl_c *) malloc(sizeof(struct iso_ctl_c))) == NULL) {
        printf("malloc error for iso_ctl_c \n");
        exit(0);
    }
	
	iso_c->maxlen = 0;
	for (i=0;i<NLBL_ISO_CTL;i++){
		if(strlen(label_iso_ctl[i]) > iso_c->maxlen){
			iso_c->maxlen = (int) strlen(label_iso_ctl[i]);
		};
	};
	
	iso_c->iflag_isosurf_define = 0;
	iso_c->iflag_iso_output_field = 0;
	
	iso_c->iso_def_c = init_iso_define_ctl_c();
	iso_c->iso_fld_c = init_iso_field_ctl_c();
	
	iso_c->iso_file_head_ctl = init_chara_ctl_item_c();
	iso_c->iso_output_type_ctl = init_chara_ctl_item_c();
	return iso_c;
};

void dealloc_iso_ctl_c(struct iso_ctl_c *iso_c){
	dealloc_iso_define_ctl_c(iso_c->iso_def_c);
	dealloc_iso_field_ctl_c(iso_c->iso_fld_c);
	
	dealloc_chara_ctl_item_c(iso_c->iso_file_head_ctl);
	dealloc_chara_ctl_item_c(iso_c->iso_output_type_ctl);
	
	iso_c->iflag_isosurf_define = 0;
	iso_c->iflag_iso_output_field = 0;
	
    free(iso_c);
	return;
};

int read_iso_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_ctl_c *iso_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_iso_ctl[ 0], iso_c->iso_file_head_ctl);
        read_chara_ctl_item_c(buf, label_iso_ctl[ 4], iso_c->iso_file_head_ctl);
		read_chara_ctl_item_c(buf, label_iso_ctl[ 1], iso_c->iso_output_type_ctl);
		
		if(right_begin_flag_c(buf, label_iso_ctl[ 2]) > 0){
			iso_c->iflag_isosurf_define = read_iso_define_ctl_c(fp, buf, 
						label_iso_ctl[ 2], iso_c->iso_def_c);
		};
		if(right_begin_flag_c(buf, label_iso_ctl[ 3]) > 0){
			iso_c->iflag_iso_output_field = read_iso_field_ctl_c(fp, buf, 
						label_iso_ctl[ 3], iso_c->iso_fld_c);
		};
        if(right_begin_flag_c(buf, label_iso_ctl[ 5]) > 0){
            iso_c->iflag_iso_output_field = read_iso_field_ctl_c(fp, buf, 
                        label_iso_ctl[ 5], iso_c->iso_fld_c);
        };
	};
	return 1;
};

int write_iso_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_ctl_c *iso_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, iso_c->maxlen, label_iso_ctl[ 0], iso_c->iso_file_head_ctl);
	write_chara_ctl_item_c(fp, level, iso_c->maxlen, label_iso_ctl[ 1], iso_c->iso_output_type_ctl);
	
    if(iso_c->iflag_isosurf_define > 0){
        fprintf(fp, "!\n");
        level = write_iso_define_ctl_c(fp, level, label_iso_ctl[ 2], iso_c->iso_def_c);
    };
    if(iso_c->iflag_iso_output_field > 0){
        fprintf(fp, "!\n");
        level = write_iso_field_ctl_c(fp, level, label_iso_ctl[ 3], iso_c->iso_fld_c);
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


