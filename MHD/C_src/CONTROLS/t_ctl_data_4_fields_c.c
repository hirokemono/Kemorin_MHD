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


struct field_ctl_c * init_field_ctl_c(){
	int i;
    struct field_ctl_c *fld_ctl;
    if((fld_ctl = (struct field_ctl_c *) malloc(sizeof(struct field_ctl_c))) == NULL) {
        printf("malloc error for field_ctl_c \n");
        exit(0);
    }
    
	
    fld_ctl->iflag_use = 0;
	fld_ctl->maxlen = 0;
	for (i=0;i<NLBL_FIELD_CTL;i++){
		if(strlen(label_field_ctl[i]) > fld_ctl->maxlen){
			fld_ctl->maxlen = (int) strlen(label_field_ctl[i]);
		};
	};
	
	fld_ctl->tmp_fld_item = init_chara3_ctl_item_c();
	
	init_chara_int2_ctl_list(&fld_ctl->field_list);
	init_chara_ctl_list(&fld_ctl->quad_phys_list);
    init_chara_ctl_list(&fld_ctl->viz_comp_list);
	
	return fld_ctl;
};

void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl){
	
	dealloc_chara3_ctl_item_c(fld_ctl->tmp_fld_item);
	
	clear_chara_int2_ctl_list(&fld_ctl->field_list);
	clear_chara_ctl_list(&fld_ctl->quad_phys_list);
    clear_chara_ctl_list(&fld_ctl->viz_comp_list);
	
    free(fld_ctl);
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
    if(fld_ctl->iflag_use > 0) return;
    
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


struct all_field_ctl_c * init_all_field_ctl_c(){
	struct all_field_ctl_c *all_fld_list;
	if((all_fld_list = (struct all_field_ctl_c *) malloc(sizeof(struct all_field_ctl_c)))
	   == NULL){
		printf("malloc error for all_field_ctl_c\n");
		exit(0);
	};
	
	all_fld_list->fld_list = init_field_name_f();
	
	int num = all_fld_list->fld_list->ntot_fields;
	if((all_fld_list->iflag_use = (int *)calloc(num, sizeof(int))) == NULL){
			printf("malloc error for iflag_use\n");
			exit(0);
	};
	if((all_fld_list->iflag_viz = (int *)calloc(num, sizeof(int))) == NULL){
			printf("malloc error for iflag_viz\n");
			exit(0);
	};
	if((all_fld_list->iflag_monitor = (int *)calloc(num, sizeof(int))) == NULL){
			printf("malloc error for iflag_monitor\n");
			exit(0);
	};
	if((all_fld_list->iflag_quad = (int *)calloc(num, sizeof(int))) == NULL){
			printf("malloc error for iflag_quad\n");
			exit(0);
	};
	
	return all_fld_list;
};

void dealloc_all_field_ctl_c(struct all_field_ctl_c *all_fld_list){
	free(all_fld_list->iflag_quad);
	free(all_fld_list->iflag_monitor);
	free(all_fld_list->iflag_viz);
	free(all_fld_list->iflag_use);
	dealloc_field_name_f(all_fld_list->fld_list);
	
	free(all_fld_list);
	return;
};

static void set_no_use_all_field_ctl_c(int i_fld, struct all_field_ctl_c *all_fld_list){
	all_fld_list->iflag_use[i_fld] = 0;
	all_fld_list->iflag_viz[i_fld] = 0;
	all_fld_list->iflag_monitor[i_fld] = 0;
    all_fld_list->iflag_quad[i_fld] = 0;
	return;
};

static void add_field_to_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct chara_int2_ctl_list *field_list_head){
	int i;
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
	all_fld_list->iflag_use[i_fld] = 1;
	
	for (i=0;i<count_chara_int2_ctl_list(field_list_head);i++){
		field_list_head = field_list_head->_next;
	};
	field_list_head = add_chara_int2_ctl_list_after(field_list_head);
	update_chara_int2_ctl_item_c(all_fld_list->fld_list->field_name[i_fld], 0, 0,  
				field_list_head->ci2_item);
	return;
}

static void delete_field_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
			struct chara_int2_ctl_list *field_list_head){
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
	del_chara_int2_ctl_list_by_c_tbl(all_fld_list->fld_list->field_name[i_fld], field_list_head);
	return;
}

static void update_field_flag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct chara_int2_ctl_list *field_list_head){
	field_list_head = find_ci2_ctl_list_item_by_c_tbl(all_fld_list->fld_list->field_name[i_fld], field_list_head);
	
	field_list_head->ci2_item->i_data[0] = all_fld_list->iflag_viz[i_fld];
	field_list_head->ci2_item->i_data[1] = all_fld_list->iflag_monitor[i_fld];
	return;
}


static void load_field_from_ctl(struct chara_int2_ctl_list *field_list_head, 
			struct all_field_ctl_c *all_fld_list){
	int i, j;
	int jst = 0;
	
	field_list_head = field_list_head->_next;
	while (field_list_head != NULL){
		for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){
			i = (j+jst) % all_fld_list->fld_list->ntot_fields;
			if(cmp_no_case_c(field_list_head->ci2_item->c_tbl, 
							 all_fld_list->fld_list->field_name[i])){
				all_fld_list->iflag_use[i] = 1;
				all_fld_list->iflag_viz[i] = field_list_head->ci2_item->i_data[0];
				all_fld_list->iflag_monitor[i] = field_list_head->ci2_item->i_data[1];
				jst = i+1;
				break;
			};
		};
		field_list_head = field_list_head->_next;
    };
	
	return;
};

static void load_field_to_ctl(struct all_field_ctl_c *all_fld_list, 
			struct chara_int2_ctl_list *field_list_head){
	int i;
	for (i=0;i<all_fld_list->fld_list->ntot_fields;i++){
		if(all_fld_list->iflag_use[i] > 0){
			field_list_head = add_chara_int2_ctl_list_after(field_list_head);
			update_chara_int2_ctl_item_c(all_fld_list->fld_list->field_name[i], 
						all_fld_list->iflag_viz[i], all_fld_list->iflag_monitor[i], 
						field_list_head->ci2_item);
		};
	};
	return;
};

static void set_quadrature_flag_from_ctl(struct chara_ctl_list *quad_phys_head, 
			struct all_field_ctl_c *all_fld_list){
	int i, j;
	int jst = 0;
	
	for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){all_fld_list->iflag_quad[j] = 0;}
	
	quad_phys_head = quad_phys_head->_next;
	while (quad_phys_head != NULL){
		for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){
			i = (j+jst) % all_fld_list->fld_list->ntot_fields;
			if(cmp_no_case_c(quad_phys_head->c_item->c_tbl,
							 all_fld_list->fld_list->field_name[i])){
				all_fld_list->iflag_quad[i] = 1;
				jst = i+1;
				break;
			};
		};
		quad_phys_head = quad_phys_head->_next;
    };
	return;
};

static void load_quadrature_field_to_ctl(struct all_field_ctl_c *all_fld_list, 
			struct chara_ctl_list *quad_phys_list){
	int i;
	for (i=0;i<all_fld_list->fld_list->ntot_fields;i++){
		if(all_fld_list->iflag_quad[i] > 0){
			quad_phys_list = add_chara_ctl_list_after(quad_phys_list);
			
			sprintf(quad_phys_list->c_item->c_tbl, "%s",
					all_fld_list->fld_list->field_name[i]);
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

static void check_field_in_quad_list(struct chara_ctl_list *quad_phys_list){
    quad_phys_list = quad_phys_list->_next;
    while (quad_phys_list != NULL){
        printf("Field in the list: %s\n", quad_phys_list->c_item->c_tbl); 
        quad_phys_list = quad_phys_list->_next;
   };
	return;	
};

static void check_field_in_component_list(struct chara_ctl_list *viz_comp_list){
    viz_comp_list = viz_comp_list->_next;
    while (viz_comp_list != NULL){
        printf("Component in the field list: %s\n", viz_comp_list->c_item->c_tbl); 
        viz_comp_list = viz_comp_list->_next;
    };
    return;    
};


void add_field_wqflag_to_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl){
	
	add_field_to_ctl(i_fld, all_fld_list, &fld_ctl->field_list);
	return;
}

void delete_field_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
			struct field_ctl_c *fld_ctl){
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
	del_chara_ctl_list_by_c_tbl(all_fld_list->fld_list->field_name[i_fld],
								&fld_ctl->quad_phys_list);
	delete_field_in_ctl(i_fld, all_fld_list, &fld_ctl->field_list);
	return;
}

void update_field_flag_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl){
	
    update_field_flag_in_ctl(i_fld, all_fld_list, &fld_ctl->field_list);
	
	if(all_fld_list->iflag_quad[i_fld] == 0){
		del_chara_ctl_list_by_c_tbl(all_fld_list->fld_list->field_name[i_fld],
									&fld_ctl->quad_phys_list);
	} else {
		append_chara_ctl_list(all_fld_list->fld_list->field_name[i_fld],
							  &fld_ctl->quad_phys_list);
	} 
	
	return;
};

void load_field_w_qflag_from_ctl(struct field_ctl_c *fld_ctl, 
			struct all_field_ctl_c *all_fld_list){
	load_field_from_ctl(&fld_ctl->field_list, all_fld_list);
	set_quadrature_flag_from_ctl(&fld_ctl->quad_phys_list, all_fld_list);
	return;
};

void load_field_w_qflag_to_ctl(struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl){
	load_field_to_ctl(all_fld_list, &fld_ctl->field_list);
	load_quadrature_field_to_ctl(all_fld_list, &fld_ctl->quad_phys_list);
	return;
};

void reflesh_field_ctl_list(struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl){
	clear_chara_int2_ctl_list(&fld_ctl->field_list);
	clear_chara_ctl_list(&fld_ctl->quad_phys_list);
	load_field_w_qflag_to_ctl(all_fld_list, fld_ctl);
	return;
};


void check_field_ctl_list(struct field_ctl_c *fld_ctl){
	check_field_in_list(&fld_ctl->field_list);
	check_field_in_quad_list(&fld_ctl->quad_phys_list);
	return;	
};

void check_field_and_comp_ctl_list(struct field_ctl_c *fld_ctl){
    check_field_in_list(&fld_ctl->field_list);
    check_field_in_component_list(&fld_ctl->viz_comp_list);
    return;    
};
