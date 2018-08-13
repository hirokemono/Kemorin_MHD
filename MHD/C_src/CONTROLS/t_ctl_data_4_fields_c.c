/*
//  t_ctl_data_4_fields_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_fields_c.h"

#define NLBL_FIELD_CTL 3

const char label_field_ctl[NLBL_FIELD_CTL][KCHARA_C] = {
	/*[ 0]*/	{"nod_value_ctl"},

	/*[ 1]*/	{"quad_field_name_ctl"},
	/*[ 2]*/	{"linear_field_name_ctl"}
};

void get_label_field_ctl(int index, char *label){
    if(index < NLBL_FIELD_CTL) strngcopy(label, label_field_ctl[index]);
    return;
};


void alloc_field_ctl_c(struct field_ctl_c *fld_ctl){
	int i;
	
	fld_ctl->maxlen = 0;
	for (i=0;i<NLBL_FIELD_CTL;i++){
		if(strlen(label_field_ctl[i]) > fld_ctl->maxlen){
			fld_ctl->maxlen = (int) strlen(label_field_ctl[i]);
		};
	};
	
	init_chara3_ctl_list(&fld_ctl->field_list);
	
	init_chara_ctl_list(&fld_ctl->quad_phys_list);
	init_chara_ctl_list(&fld_ctl->linear_phys_list);
	
	return;
};

void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl){
	
	clear_chara3_ctl_list(&fld_ctl->field_list);
	
	clear_chara_ctl_list(&fld_ctl->quad_phys_list);
	clear_chara_ctl_list(&fld_ctl->linear_phys_list);
	
	return;
};

int read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara3_ctl_list(fp, buf, label_field_ctl[ 0], &fld_ctl->field_list);
		
		read_chara_ctl_list(fp, buf, label_field_ctl[ 1], &fld_ctl->quad_phys_list);
		read_chara_ctl_list(fp, buf, label_field_ctl[ 2], &fld_ctl->linear_phys_list);
	};
	return 1;
};

int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara3_ctl_list(fp, level, label_field_ctl[0], &fld_ctl->field_list);
	
	write_chara_ctl_list(fp, level, label_field_ctl[1], &fld_ctl->quad_phys_list);
	write_chara_ctl_list(fp, level, label_field_ctl[2], &fld_ctl->linear_phys_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};



void alloc_all_field_ctl_c(struct all_field_ctl_c **all_fld_tbl){
	int i;
	
	for (i=0;i<NUM_FIELD;i++){
		all_fld_tbl[i] = (struct all_field_ctl_c *) malloc(sizeof(struct all_field_ctl_c));
		
		all_fld_tbl[i]->num_comp = get_field_properties(i, all_fld_tbl[i]->field_name, all_fld_tbl[i]->field_math);
		all_fld_tbl[i]->iflag_use = 0;
		all_fld_tbl[i]->iflag_viz = 0;
		all_fld_tbl[i]->iflag_monitor = 0;
	}
	
	return;
}
void dealloc_all_field_ctl_c(struct all_field_ctl_c **all_fld_tbl){
	int i;
	for (i=0;i<NUM_FIELD;i++){
		free(all_fld_tbl[i]);
	};
	free(all_fld_tbl);
	return;
}


void set_viz_flag_to_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct chara3_ctl_list *field_list){
	if(all_fld_tbl->iflag_viz == 0){
		sprintf(field_list->c3_item->c2_tbl, "%s", "Viz_Off");
	} else {
		sprintf(field_list->c3_item->c2_tbl, "%s", "Viz_On");
	};
	return;
};
void set_monitor_flag_to_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct chara3_ctl_list *field_list){
	if(all_fld_tbl->iflag_monitor == 0){
		sprintf(field_list->c3_item->c3_tbl, "%s", "Monitor_Off");
	} else {
		sprintf(field_list->c3_item->c3_tbl, "%s", "Monitor_On");
	};
	return;
};

void set_viz_flag_from_ctl(struct chara3_ctl_list *field_list, 
			struct all_field_ctl_c *all_fld_tbl){
	if(cmp_no_case_c(field_list->c3_item->c2_tbl, "Viz_On")){
		all_fld_tbl->iflag_viz = 1;
	} else {
		all_fld_tbl->iflag_viz = 0;
	};
	return;
};

void set_monitor_flag_from_ctl(struct chara3_ctl_list *field_list, 
			struct all_field_ctl_c *all_fld_tbl){
	if(cmp_no_case_c(field_list->c3_item->c3_tbl, "Monitor_On")){
		all_fld_tbl->iflag_monitor = 1;
	} else {
		sprintf(field_list->c3_item->c3_tbl, "%s", "Monitor_On");
		all_fld_tbl->iflag_monitor = 0;
	};
	return;
};


void add_field_to_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct chara3_ctl_list *field_list_head){
	int i;
	for (i=0;i<count_chara3_ctl_list(field_list_head);i++){
		field_list_head = field_list_head->_next;
	};
	field_list_head = add_chara3_ctl_list(field_list_head);
	sprintf(field_list_head->c3_item->c1_tbl, "%s", all_fld_tbl->field_name);
	sprintf(field_list_head->c3_item->c2_tbl, "%s", "Viz_Off");
	sprintf(field_list_head->c3_item->c3_tbl, "%s", "Monitor_Off");
	
	return;
}

void delete_field_in_ctl(struct all_field_ctl_c *all_fld_tbl,
			struct chara3_ctl_list *field_list_head){
	
	field_list_head = field_list_head->_next;
	while (field_list_head != NULL){
		if(cmp_no_case_c(field_list_head->c3_item->c1_tbl, all_fld_tbl->field_name)){
			delete_chara3_ctl_list(field_list_head);
			break;
		};
		field_list_head = field_list_head->_next;
	};
	return;
}

void update_field_flag_in_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct chara3_ctl_list *field_list_head){
	
	field_list_head = field_list_head->_next;
	while (field_list_head != NULL){
		if(cmp_no_case_c(field_list_head->c3_item->c1_tbl, all_fld_tbl->field_name)){
			set_viz_flag_to_ctl(all_fld_tbl, field_list_head);
			set_monitor_flag_to_ctl(all_fld_tbl, field_list_head);
			break;
		};
		field_list_head = field_list_head->_next;
	};
	return;
}

void load_field_from_ctl(struct chara3_ctl_list *field_list_head, 
			struct all_field_ctl_c **all_fld_tbl){
	int i, j;
	int jst = 0;
	
	field_list_head = field_list_head->_next;
	while (field_list_head != NULL){
		for (j=0;j<NUM_FIELD;j++){
			i = (j+jst) % NUM_FIELD;
			if(cmp_no_case_c(field_list_head->c3_item->c1_tbl, all_fld_tbl[i]->field_name)){
				all_fld_tbl[i]->iflag_use = 1;
				set_viz_flag_from_ctl(field_list_head, all_fld_tbl[i]);
				set_monitor_flag_from_ctl(field_list_head, all_fld_tbl[i]);
				jst = i+1;
				break;
			};
		};
		field_list_head = field_list_head->_next;
    };
	
	return;
};

void load_field_to_ctl(struct all_field_ctl_c **all_fld_tbl, 
			struct chara3_ctl_list *field_list_head){
	int i;
	for (i=0;i<NUM_FIELD;i++){
		if(all_fld_tbl[i]->iflag_use > 0){
			field_list_head = add_chara3_ctl_list(field_list_head);
			
			sprintf(field_list_head->c3_item->c1_tbl, "%s", all_fld_tbl[i]->field_name);
			set_viz_flag_to_ctl(all_fld_tbl[i], field_list_head);
			set_monitor_flag_to_ctl(all_fld_tbl[i], field_list_head);
		};
	};
	return;
};

void reflesh_field_ctl_list(struct all_field_ctl_c **all_fld_tbl, 
			struct chara3_ctl_list *field_list_head){
	clear_chara3_ctl_list(field_list_head);
	load_field_to_ctl(all_fld_tbl, field_list_head);
	return;
};

