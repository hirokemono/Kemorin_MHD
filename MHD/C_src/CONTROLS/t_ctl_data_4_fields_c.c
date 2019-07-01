/*
//  t_ctl_data_4_fields_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_fields_c.h"

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
	
    fld_ctl->iflag_use = 0;
	fld_ctl->maxlen = 0;
	for (i=0;i<NLBL_FIELD_CTL;i++){
		if(strlen(label_field_ctl[i]) > fld_ctl->maxlen){
			fld_ctl->maxlen = (int) strlen(label_field_ctl[i]);
		};
	};
	
	if ((fld_ctl->tmp_fld_item
					= (struct chara3_ctl_item *) malloc(sizeof(struct chara3_ctl_item))) == NULL) {
        printf("malloc error for fld_ctl->tmp_fld_item\n");
        exit(0);
    }
	alloc_chara3_ctl_item_c(fld_ctl->tmp_fld_item);
	
	init_chara_int2_ctl_list(&fld_ctl->field_list);
	init_chara_ctl_list(&fld_ctl->quad_phys_list);
	
	return;
};

void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl){
	
	dealloc_chara3_ctl_item_c(fld_ctl->tmp_fld_item);
	
	clear_chara_int2_ctl_list(&fld_ctl->field_list);
	clear_chara_ctl_list(&fld_ctl->quad_phys_list);
	
    fld_ctl->iflag_use = 0;
	return;
};

static void set_viz_flags_from_text(struct chara3_ctl_item *tmp_fld_item, 
			struct chara_int2_ctl_item *field_item){
	
	field_item->iflag = 1;
	sprintf(field_item->c_tbl, "%s", tmp_fld_item->c1_tbl);
	
	if(cmp_no_case_c(tmp_fld_item->c2_tbl, "Viz_On")){
		field_item->i_data[0] = 1;
	} else {
		field_item->i_data[0] = 0;
	};
	if(cmp_no_case_c(tmp_fld_item->c3_tbl, "Monitor_On")){
		field_item->i_data[1] = 1;
	} else {
		field_item->i_data[1] = 0;
	};
	return;
};

static void set_viz_flags_to_text(struct chara_int2_ctl_item *field_item, 
			struct chara3_ctl_item *tmp_fld_item){
	
	tmp_fld_item->iflag = 1;
	sprintf(tmp_fld_item->c1_tbl, "%s", field_item->c_tbl);
	
	if(field_item->i_data[0] == 0){
		sprintf(tmp_fld_item->c2_tbl, "%s", "Viz_Off");
	} else {
		sprintf(tmp_fld_item->c2_tbl, "%s", "Viz_On");
	};
	
	if(field_item->i_data[1] == 0){
		sprintf(tmp_fld_item->c3_tbl, "%s", "Monitor_Off");
	} else {
		sprintf(tmp_fld_item->c3_tbl, "%s", "Monitor_On");
	};
	return;
};

static int read_field_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara3_ctl_item *tmp_fld_item, struct chara_int2_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->ci2_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_chara_int2_ctl_list_after(head);
        iflag = read_chara3_ctl_item_c(buf, label, tmp_fld_item);
		set_viz_flags_from_text(tmp_fld_item, head->ci2_item);
		tmp_fld_item->iflag = 0;
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_field_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara3_ctl_item *tmp_fld_item, struct chara_int2_ctl_list *head){
	int mlen2[2];
	int maxlen[3];
	
    if(count_maxlen_chara_int2_ctl_list(label, head, mlen2) == 0) return level;
	maxlen[0] = mlen2[0];
	maxlen[1] = mlen2[1];
	maxlen[2] = (int) strlen("Viz_Off");
    
	
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
	while (head != NULL) {    /* Go through null pointer*/
		set_viz_flags_to_text(head->ci2_item, tmp_fld_item);
        level = write_chara3_ctl_item_c(fp, level, maxlen,
                                     label, tmp_fld_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

void read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_field_ctl_list(fp, buf, label_field_ctl[ 0],
					fld_ctl->tmp_fld_item, &fld_ctl->field_list);
		
		read_chara_ctl_list(fp, buf, label_field_ctl[ 1], &fld_ctl->quad_phys_list);
	};
    fld_ctl->iflag_use = 1;
	return;
};

int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl){
    if(fld_ctl->iflag_use == 0) return level;
    
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_field_ctl_list(fp, level, label_field_ctl[0], 
				fld_ctl->tmp_fld_item, &fld_ctl->field_list);
	
	write_chara_ctl_list(fp, level, label_field_ctl[1], &fld_ctl->quad_phys_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


static void set_no_use_all_field_ctl_c(struct all_field_ctl_c *all_fld_tbl){
	all_fld_tbl->iflag_use = 0;
	all_fld_tbl->iflag_viz = 0;
	all_fld_tbl->iflag_monitor = 0;
    all_fld_tbl->iflag_quad = 0;
	return;
};


void alloc_all_field_ctl_c(struct all_field_ctl_c **all_fld_tbl){
	int i;
	
	for (i=0;i<NUM_FIELD;i++){
		all_fld_tbl[i] = (struct all_field_ctl_c *) malloc(sizeof(struct all_field_ctl_c));
		
		all_fld_tbl[i]->num_comp = get_field_properties(i, all_fld_tbl[i]->field_name, all_fld_tbl[i]->field_math);
		set_no_use_all_field_ctl_c(all_fld_tbl[i]);
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

static void add_field_to_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct chara_int2_ctl_list *field_list_head){
	int i;
	set_no_use_all_field_ctl_c(all_fld_tbl);
	all_fld_tbl->iflag_use = 1;
	
	for (i=0;i<count_chara_int2_ctl_list(field_list_head);i++){
		field_list_head = field_list_head->_next;
	};
	field_list_head = add_chara_int2_ctl_list_after(field_list_head);
	update_chara_int2_ctl_item_c(all_fld_tbl->field_name, 0, 0,  
				field_list_head->ci2_item);
	return;
}

static void delete_field_in_ctl(struct all_field_ctl_c *all_fld_tbl,
			struct chara_int2_ctl_list *field_list_head){
	set_no_use_all_field_ctl_c(all_fld_tbl);
	del_chara_int2_ctl_list_by_c_tbl(all_fld_tbl->field_name, field_list_head);
	return;
}

static void update_field_flag_in_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct chara_int2_ctl_list *field_list_head){
	field_list_head = find_ci2_ctl_list_item_by_c_tbl(all_fld_tbl->field_name, field_list_head);
	
	field_list_head->ci2_item->i_data[0] = all_fld_tbl->iflag_viz;
	field_list_head->ci2_item->i_data[1] = all_fld_tbl->iflag_monitor;
	return;
}


static void load_field_from_ctl(struct chara_int2_ctl_list *field_list_head, 
			struct all_field_ctl_c **all_fld_tbl){
	int i, j;
	int jst = 0;
	
	field_list_head = field_list_head->_next;
	while (field_list_head != NULL){
		for (j=0;j<NUM_FIELD;j++){
			i = (j+jst) % NUM_FIELD;
			if(cmp_no_case_c(field_list_head->ci2_item->c_tbl, all_fld_tbl[i]->field_name)){
				all_fld_tbl[i]->iflag_use = 1;
				all_fld_tbl[i]->iflag_viz = field_list_head->ci2_item->i_data[0];
				all_fld_tbl[i]->iflag_monitor = field_list_head->ci2_item->i_data[1];
				jst = i+1;
				break;
			};
		};
		field_list_head = field_list_head->_next;
    };
	
	return;
};

static void load_field_to_ctl(struct all_field_ctl_c **all_fld_tbl, 
			struct chara_int2_ctl_list *field_list_head){
	int i;
	for (i=0;i<NUM_FIELD;i++){
		if(all_fld_tbl[i]->iflag_use > 0){
			field_list_head = add_chara_int2_ctl_list_after(field_list_head);
			update_chara_int2_ctl_item_c(all_fld_tbl[i]->field_name, 
						all_fld_tbl[i]->iflag_viz, all_fld_tbl[i]->iflag_monitor, 
						field_list_head->ci2_item);
		};
	};
	return;
};

static void check_field_in_list(struct chara_int2_ctl_list *field_list_head){
    field_list_head = field_list_head->_next;
    while (field_list_head != NULL){
        printf("Field in the list: %s\n", field_list_head->ci2_item->c_tbl); 
        field_list_head = field_list_head->_next;
   };
	return;	
};


static void set_quadrature_flag_from_ctl(struct chara_ctl_list *quad_phys_head, 
			struct all_field_ctl_c **all_fld_tbl){
	int i, j;
	int jst = 0;
	
	for (j=0;j<NUM_FIELD;j++){
		all_fld_tbl[j]->iflag_quad = 0;
	}
	
	quad_phys_head = quad_phys_head->_next;
	while (quad_phys_head != NULL){
		for (j=0;j<NUM_FIELD;j++){
			i = (j+jst) % NUM_FIELD;
			if(cmp_no_case_c(quad_phys_head->c_item->c_tbl, all_fld_tbl[i]->field_name)){
				all_fld_tbl[i]->iflag_quad = 1;
				jst = i+1;
				break;
			};
		};
		quad_phys_head = quad_phys_head->_next;
    };
	return;
};

static void load_quadrature_field_to_ctl(struct all_field_ctl_c **all_fld_tbl, 
			struct chara_ctl_list *quad_phys_list){
	int i;
	for (i=0;i<NUM_FIELD;i++){
		if(all_fld_tbl[i]->iflag_quad > 0){
			quad_phys_list = add_chara_ctl_list_after(quad_phys_list);
			
			sprintf(quad_phys_list->c_item->c_tbl, "%s", all_fld_tbl[i]->field_name);
		};
	};
	return;
};

static void check_field_in_quad_list(struct chara_ctl_list *quad_phys_list){
    quad_phys_list = quad_phys_list->_next;
    while (quad_phys_list != NULL){
        printf("Field in the list: %s\n", quad_phys_list->c_item->c_tbl); 
        quad_phys_list = quad_phys_list->_next;
   };
	return;	
};


void add_field_wqflag_to_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct field_ctl_c *fld_ctl){
	
	add_field_to_ctl(all_fld_tbl, &fld_ctl->field_list);
	return;
}

void delete_field_wqflag_in_ctl(struct all_field_ctl_c *all_fld_tbl,
			struct field_ctl_c *fld_ctl){
	set_no_use_all_field_ctl_c(all_fld_tbl);
	del_chara_ctl_list_by_c_tbl(all_fld_tbl->field_name, &fld_ctl->quad_phys_list);
	delete_field_in_ctl(all_fld_tbl, &fld_ctl->field_list);
	return;
}

void update_field_flag_wqflag_in_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct field_ctl_c *fld_ctl){
	
    update_field_flag_in_ctl(all_fld_tbl, &fld_ctl->field_list);
	
	if(all_fld_tbl->iflag_quad == 0){
		del_chara_ctl_list_by_c_tbl(all_fld_tbl->field_name, &fld_ctl->quad_phys_list);
	} else {
		append_chara_ctl_list(all_fld_tbl->field_name, &fld_ctl->quad_phys_list);
	} 
	
	return;
};


void load_field_w_qflag_from_ctl(struct field_ctl_c *fld_ctl, 
			struct all_field_ctl_c **all_fld_tbl){
	load_field_from_ctl(&fld_ctl->field_list, all_fld_tbl);
	set_quadrature_flag_from_ctl(&fld_ctl->quad_phys_list, all_fld_tbl);
	return;
};

void load_field_w_qflag_to_ctl(struct all_field_ctl_c **all_fld_tbl, 
			struct field_ctl_c *fld_ctl){
	load_field_to_ctl(all_fld_tbl, &fld_ctl->field_list);
	load_quadrature_field_to_ctl(all_fld_tbl, &fld_ctl->quad_phys_list);
	return;
};

void reflesh_field_ctl_list(struct all_field_ctl_c **all_fld_tbl, 
			struct field_ctl_c *fld_ctl){
	clear_chara_int2_ctl_list(&fld_ctl->field_list);
	clear_chara_ctl_list(&fld_ctl->quad_phys_list);
	load_field_w_qflag_to_ctl(all_fld_tbl, fld_ctl);
	return;
};

void check_field_ctl_list(struct field_ctl_c *fld_ctl){
	check_field_in_list(&fld_ctl->field_list);
	check_field_in_quad_list(&fld_ctl->quad_phys_list);
	return;	
};
