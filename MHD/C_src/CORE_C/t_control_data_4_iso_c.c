/*
//  t_control_data_4_iso_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_iso_c.h"

#define NLBL_ISO_DEFINE_CTL   4
#define NLBL_ISO_FIELD_CTL    3
#define NLBL_ISO_CTL          4

const char label_iso_define_ctl[NLBL_ISO_DEFINE_CTL][KCHARA_C] = {
	/*[ 0]*/	{"isosurf_field"},
	/*[ 1]*/	{"isosurf_component"},
	/*[ 2]*/	{"isosurf_value"},
	/*[ 3]*/	{"isosurf_area_ctl"}
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
	/*[ 3]*/	{"field_on_isosurf"}
};

void alloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c){
	int i;
	
	iso_def_c->maxlen = 0;
	for (i=0;i<NLBL_ISO_DEFINE_CTL;i++){
		if(strlen(label_iso_define_ctl[i]) > iso_def_c->maxlen){
			iso_def_c->maxlen = strlen(label_iso_define_ctl[i]);
		};
	};
	
	iso_def_c->isosurf_data_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	iso_def_c->isosurf_comp_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(iso_def_c->isosurf_data_ctl);
	alloc_ctl_chara_item(iso_def_c->isosurf_comp_ctl);
	
	iso_def_c->isosurf_value_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_ctl_real_item(iso_def_c->isosurf_value_ctl);
	
	iso_def_c->iso_area_ctl = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	
	return;
};

void dealloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c){
	
	dealloc_ctl_chara_item(iso_def_c->isosurf_data_ctl);
	dealloc_ctl_chara_item(iso_def_c->isosurf_comp_ctl);
	free(iso_def_c->isosurf_data_ctl);
	free(iso_def_c->isosurf_comp_ctl);
	
	free(iso_def_c->isosurf_value_ctl);
	
	dealloc_ctl_chara_array(iso_def_c->iso_area_ctl);
	free(iso_def_c->iso_area_ctl);
	
	return;
};

int read_iso_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_define_ctl_c *iso_def_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_iso_define_ctl[ 0], iso_def_c->isosurf_data_ctl);
		read_character_ctl_item_c(buf, label_iso_define_ctl[ 1], iso_def_c->isosurf_comp_ctl);
		
		read_real_ctl_item_c(buf, label_iso_define_ctl[ 2], iso_def_c->isosurf_value_ctl);
		
		read_character_ctl_array_c(fp, buf, label_iso_define_ctl[ 3], iso_def_c->iso_area_ctl);
	};
	return 1;
};

int write_iso_define_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_define_ctl_c *iso_def_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 0], iso_def_c->isosurf_data_ctl);
	write_character_ctl_item_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 1], iso_def_c->isosurf_comp_ctl);
	
	write_real_ctl_item_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 2], iso_def_c->isosurf_value_ctl);
	
	if(iso_def_c->iso_area_ctl->num > 0) fprintf(fp, "!\n");
	write_character_ctl_array_c(fp, level, iso_def_c->maxlen, label_iso_define_ctl[ 3], iso_def_c->iso_area_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c){
	int i;
	
	iso_fld_c->maxlen = 0;
	for (i=0;i<NLBL_ISO_FIELD_CTL;i++){
		if(strlen(label_iso_field_ctl[i]) > iso_fld_c->maxlen){
			iso_fld_c->maxlen = strlen(label_iso_field_ctl[i]);
		};
	};
	
	iso_fld_c->iso_result_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(iso_fld_c->iso_result_type_ctl);
	
	iso_fld_c->result_value_iso_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	iso_fld_c->iso_out_field_ctl = (struct chara2_ctl_array *) malloc(sizeof(struct chara2_ctl_array));
	
	return;
};

void dealloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c){
	
	dealloc_ctl_chara_item(iso_fld_c->iso_result_type_ctl);
	free(iso_fld_c->iso_result_type_ctl);
	free(iso_fld_c->result_value_iso_ctl);
	
	dealloc_ctl_chara2_array(iso_fld_c->iso_out_field_ctl);
	free(iso_fld_c->iso_out_field_ctl);
	
	return;
};

int read_iso_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_field_ctl_c *iso_fld_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_iso_field_ctl[ 0], iso_fld_c->iso_result_type_ctl);
		read_real_ctl_item_c(buf, label_iso_field_ctl[ 1], iso_fld_c->result_value_iso_ctl);
		
		read_chara2_ctl_array_c(fp, buf, label_iso_field_ctl[ 2], iso_fld_c->iso_out_field_ctl);
	};
	return 1;
};

int write_iso_field_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_field_ctl_c *iso_fld_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, iso_fld_c->maxlen, label_iso_field_ctl[ 0], iso_fld_c->iso_result_type_ctl);
	write_real_ctl_item_c(fp, level, iso_fld_c->maxlen, label_iso_field_ctl[ 1], iso_fld_c->result_value_iso_ctl);
	
	if(iso_fld_c->iso_out_field_ctl->num > 0) fprintf(fp, "!\n");
	write_chara2_ctl_array_c(fp, level, iso_fld_c->maxlen, label_iso_field_ctl[ 2], iso_fld_c->iso_out_field_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_iso_ctl_c(struct iso_ctl_c *iso_c){
	int i;
	
	iso_c->maxlen = 0;
	for (i=0;i<NLBL_ISO_CTL;i++){
		if(strlen(label_iso_ctl[i]) > iso_c->maxlen){
			iso_c->maxlen = strlen(label_iso_ctl[i]);
		};
	};
	
	iso_c->iflag_isosurf_define = 0;
	iso_c->iflag_iso_output_field = 0;
	
	iso_c->iso_def_c = (struct iso_define_ctl_c *) malloc(sizeof(struct iso_define_ctl_c));
	alloc_iso_define_ctl_c(iso_c->iso_def_c);
	iso_c->iso_fld_c = (struct iso_field_ctl_c *) malloc(sizeof(struct iso_field_ctl_c));
	alloc_iso_field_ctl_c(iso_c->iso_fld_c);
	
	iso_c->iso_file_head_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	iso_c->iso_output_type_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(iso_c->iso_file_head_ctl);
	alloc_ctl_chara_item(iso_c->iso_output_type_ctl);
	
	return;
};

void dealloc_iso_ctl_c(struct iso_ctl_c *iso_c){
	dealloc_iso_define_ctl_c(iso_c->iso_def_c);
	free(iso_c->iso_def_c);
	dealloc_iso_field_ctl_c(iso_c->iso_fld_c);
	free(iso_c->iso_fld_c);
	
	dealloc_ctl_chara_item(iso_c->iso_file_head_ctl);
	dealloc_ctl_chara_item(iso_c->iso_output_type_ctl);
	free(iso_c->iso_file_head_ctl);
	free(iso_c->iso_output_type_ctl);
	
	iso_c->iflag_isosurf_define = 0;
	iso_c->iflag_iso_output_field = 0;
	
	return;
};

int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_ctl_c *iso_c){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_iso_ctl[ 0], iso_c->iso_file_head_ctl);
		read_character_ctl_item_c(buf, label_iso_ctl[ 1], iso_c->iso_output_type_ctl);
		
		if(right_begin_flag_c(buf, label_iso_ctl[ 2]) > 0){
			iso_c->iflag_isosurf_define = read_iso_define_ctl_c(fp, buf, 
						label_iso_ctl[ 2], iso_c->iso_def_c);
		};
		if(right_begin_flag_c(buf, label_iso_ctl[ 3]) > 0){
			iso_c->iflag_iso_output_field = read_iso_field_ctl_c(fp, buf, 
						label_iso_ctl[ 3], iso_c->iso_fld_c);
		};
	};
	return 1;
};

int write_iso_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_ctl_c *iso_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, iso_c->maxlen, label_iso_ctl[ 0], iso_c->iso_file_head_ctl);
	write_character_ctl_item_c(fp, level, iso_c->maxlen, label_iso_ctl[ 1], iso_c->iso_output_type_ctl);
	
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

