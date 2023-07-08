/*
//  t_ctl_data_4_fields_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_fields_c.h"

const char label_field_ctl[NLBL_FIELD_CTL][KCHARA_C] = {
	/*[ 0]*/	{"nod_value_ctl"},
	/*[ 1]*/	{"quad_field_name_ctl"}
};

void get_label_field_ctl(int index, char *label){
    if(index < NLBL_FIELD_CTL) strngcopy(label, label_field_ctl[index]);
    return;
};


struct f_MHD_fields_control * init_field_ctl_c(){
	int i;
    struct f_MHD_fields_control *fld_ctl;
    if((fld_ctl = (struct f_MHD_fields_control *) malloc(sizeof(struct f_MHD_fields_control))) == NULL) {
        printf("malloc error for f_MHD_fields_control \n");
        exit(0);
    }
    
	
    fld_ctl->iflag_read = 0;
	fld_ctl->maxlen = 0;
	for (i=0;i<NLBL_FIELD_CTL;i++){
		if(strlen(label_field_ctl[i]) > fld_ctl->maxlen){
			fld_ctl->maxlen = (int) strlen(label_field_ctl[i]);
		};
	};
    fld_ctl->f_field_ctl = init_chara_int2_clist();
	
	return fld_ctl;
};

void dealloc_field_ctl_c(struct f_MHD_fields_control *fld_ctl){
    dealloc_chara_int2_clist(fld_ctl->f_field_ctl);
    dealloc_chara_clist(fld_ctl->f_quad_phys);
	
    free(fld_ctl);
	return;
};

void set_viz_flags_from_text(struct chara3_ctl_item *tmp_fld_item,
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

void set_viz_flags_to_text(struct chara_int2_ctl_item *field_item,
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
                               struct chara_int2_clist *f_field_ctl){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(count_chara_int2_clist(f_field_ctl) > 0) return 0;
    
    struct chara3_ctl_item *tmp_fld_ctl = init_chara3_ctl_item_c();
    struct chara_int2_ctl_item *fld_item;

    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        iflag = read_chara3_ctl_item_c(buf, label, tmp_fld_ctl);
        fld_item = init_chara_int2_ctl_item_c();
		set_viz_flags_from_text(tmp_fld_ctl, fld_item);
        append_chara_int2_clist(fld_item, f_field_ctl);
        tmp_fld_ctl->iflag = 0;
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    dealloc_chara3_ctl_item_c(tmp_fld_ctl);
    return icou;
};

static int write_field_ctl_list(FILE *fp, int level, const char *label, 
                                struct chara_int2_clist *f_field_ctl){
	int mlen2[2];
	int maxlen[3];
	
    if(count_maxlen_chara_int2_clist(label, f_field_ctl, mlen2) == 0) return level;
	maxlen[0] = mlen2[0];
	maxlen[1] = mlen2[1];
	maxlen[2] = (int) strlen("Viz_Off");
    
    int idx;
    struct chara3_ctl_item *tmp_fld_ctl = init_chara3_ctl_item_c();
    struct chara_int2_ctl_item *fld_item;
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    for(idx=0;idx<count_chara_int2_clist(f_field_ctl);idx++){
        fld_item = find_chara_int2_ctl_item_by_index(idx, f_field_ctl);
        set_viz_flags_to_text(fld_item, tmp_fld_ctl);
        level = write_chara3_ctl_item_c(fp, level, maxlen,
                                        label, tmp_fld_ctl);
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    dealloc_chara3_ctl_item_c(tmp_fld_ctl);
    return level;
};

void read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
                      struct f_MHD_fields_control *fld_ctl){
    if(fld_ctl->iflag_read > 0) return;
    
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
        read_field_ctl_list(fp, buf, label_field_ctl[ 0],
                            fld_ctl->f_field_ctl);
        read_chara_clist(fp, buf, label_field_ctl[ 1], fld_ctl->f_quad_phys);
	};
    fld_ctl->iflag_read = 1;
	return;
};

int write_field_ctl_c(FILE *fp, int level, const char *label, struct f_MHD_fields_control *fld_ctl){
    if(fld_ctl->iflag_read == 0) return level;
    
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_field_ctl_list(fp, level, label_field_ctl[0], fld_ctl->f_field_ctl);
    write_chara_clist(fp, level, label_field_ctl[1], fld_ctl->f_quad_phys);
	
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
                             struct chara_int2_clist *f_field_ctl){
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
	all_fld_list->iflag_use[i_fld] = 1;
    struct chara_int2_ctl_item *ci2_input = init_chara_int2_ctl_item_c();
    update_chara_int2_ctl_item_c(all_fld_list->fld_list->field_name[i_fld], 0, 0,
                                 ci2_input);
    append_chara_int2_clist(ci2_input, f_field_ctl);
	return;
}

void delete_field_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
                         struct chara_int2_clist *f_field_ctl){
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
    del_chara_int2_clist_by_c_tbl(all_fld_list->fld_list->field_name[i_fld], f_field_ctl);
	return;
}

static void update_field_flag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct chara_int2_clist *f_field_ctl){
    struct chara_int2_ctl_item *ci2_item = find_chara_int2_ctl_item_by_cref(all_fld_list->fld_list->field_name[i_fld], f_field_ctl);
	
	ci2_item->i_data[0] = all_fld_list->iflag_viz[i_fld];
	ci2_item->i_data[1] = all_fld_list->iflag_monitor[i_fld];
	return;
}


void load_field_from_ctl(struct chara_int2_clist *f_field_ctl,
                         struct all_field_ctl_c *all_fld_list){
    int i, j, idx;
    int jst = 0;
    struct chara_int2_ctl_item *ci2_item;
    for(idx=0;idx<count_chara_int2_clist(f_field_ctl);idx++){
        ci2_item = find_chara_int2_ctl_item_by_index(idx, f_field_ctl);
        for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){
            i = (j+jst) % all_fld_list->fld_list->ntot_fields;
            if(cmp_no_case_c(ci2_item->c_tbl, all_fld_list->fld_list->field_name[i]) > 0){
                all_fld_list->iflag_use[i] = 1;
                all_fld_list->iflag_viz[i] = ci2_item->i_data[0];
                all_fld_list->iflag_monitor[i] = ci2_item->i_data[1];
                jst = i+1;
                break;
            };
        };
    };
    return;
};

static void load_field_to_ctl(struct all_field_ctl_c *all_fld_list, 
                              struct chara_int2_clist *f_field_ctl){
	int i;
    struct chara_int2_ctl_item *ci2_input;
	for (i=0;i<all_fld_list->fld_list->ntot_fields;i++){
        if(all_fld_list->iflag_use[i] > 0){
        ci2_input = init_chara_int2_ctl_item_c();
        update_chara_int2_ctl_item_c(all_fld_list->fld_list->field_name[i],
                                     all_fld_list->iflag_viz[i], all_fld_list->iflag_monitor[i],
                                     ci2_input);
            append_chara_int2_clist(ci2_input, f_field_ctl);
		};
	};
	return;
};

static void set_quadrature_flag_from_ctl(struct chara_clist *f_quad_phys,
                                         struct all_field_ctl_c *all_fld_list){
	int i, j, idx;
	int jst = 0;
	
	for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){all_fld_list->iflag_quad[j] = 0;}
    if(f_quad_phys == NULL) return;
    if(f_quad_phys->f_self == NULL) return;
    for(idx=0;idx<count_chara_clist(f_quad_phys);idx++){
		for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){
			i = (j+jst) % all_fld_list->fld_list->ntot_fields;
			if(cmp_no_case_c(chara_clist_at_index(idx, f_quad_phys)->c_tbl,
							 all_fld_list->fld_list->field_name[i])){
				all_fld_list->iflag_quad[i] = 1;
				jst = i+1;
				break;
			};
		};
    };
	return;
};

static void load_quadrature_field_to_ctl(struct all_field_ctl_c *all_fld_list, 
                                         struct chara_clist *f_quad_phys){
	int i;
	for (i=0;i<all_fld_list->fld_list->ntot_fields;i++){
		if(all_fld_list->iflag_quad[i] > 0){
            append_chara_clist(all_fld_list->fld_list->field_name[i], f_quad_phys);
        };
	};
	return;
};


static void check_field_in_list(struct chara_int2_clist *f_field_ctl){
    int idx;
    for(idx=0;idx<count_chara_int2_clist(f_field_ctl);idx++){
        printf("Field in the list: %s\n",
               find_chara_int2_ctl_item_by_index(idx,f_field_ctl)->c_tbl);
   };
	return;	
};

static void check_field_in_quad_list(struct chara_clist *f_quad_phys){
    int i;
    for(i=0;i<count_chara_clist(f_quad_phys);i++){
        printf("Field in the list: %s\n", chara_clist_at_index(i,f_quad_phys)->c_tbl);
   };
	return;	
};


void add_field_wqflag_to_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
                             struct f_MHD_fields_control *fld_ctl){
	add_field_to_ctl(i_fld, all_fld_list, fld_ctl->f_field_ctl);
	return;
}

void delete_field_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
                                struct f_MHD_fields_control *fld_ctl){
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
    del_chara_clist_by_c_tbl(all_fld_list->fld_list->field_name[i_fld],
                             fld_ctl->f_quad_phys);
	delete_field_in_ctl(i_fld, all_fld_list, fld_ctl->f_field_ctl);
	return;
}

void update_field_flag_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
                                     struct f_MHD_fields_control *fld_ctl){
	
    update_field_flag_in_ctl(i_fld, all_fld_list, fld_ctl->f_field_ctl);
	
	if(all_fld_list->iflag_quad[i_fld] == 0){
        del_chara_clist_by_c_tbl(all_fld_list->fld_list->field_name[i_fld],
                                 fld_ctl->f_quad_phys);
	} else {
        append_chara_clist(all_fld_list->fld_list->field_name[i_fld],
                           fld_ctl->f_quad_phys);
	} 
	
	return;
};

void load_field_w_qflag_from_ctl(struct f_MHD_fields_control *fld_ctl,
                                 struct all_field_ctl_c *all_fld_list){
	load_field_from_ctl(fld_ctl->f_field_ctl, all_fld_list);
	set_quadrature_flag_from_ctl(fld_ctl->f_quad_phys, all_fld_list);
	return;
};

void load_field_w_qflag_to_ctl(struct all_field_ctl_c *all_fld_list, 
                               struct f_MHD_fields_control *fld_ctl){
	load_field_to_ctl(all_fld_list, fld_ctl->f_field_ctl);
	load_quadrature_field_to_ctl(all_fld_list, fld_ctl->f_quad_phys);
	return;
};

void reflesh_field_ctl_list(struct all_field_ctl_c *all_fld_list, 
                            struct f_MHD_fields_control *fld_ctl){
    dealloc_chara_int2_clist(fld_ctl->f_field_ctl);
    dealloc_chara_clist(fld_ctl->f_quad_phys);
	load_field_w_qflag_to_ctl(all_fld_list, fld_ctl);
	return;
};


void check_field_ctl_list(struct f_MHD_fields_control *fld_ctl){
	check_field_in_list(fld_ctl->f_field_ctl);
	check_field_in_quad_list(fld_ctl->f_quad_phys);
	return;	
};

void check_field_and_comp_ctl_list(struct f_MHD_fields_control *fld_ctl){
    check_field_in_list(fld_ctl->f_field_ctl);
    return;
};
