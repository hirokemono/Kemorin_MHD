/*
//  t_ctl_data_4_fields_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_fields_c.h"


extern void * c_MHD_field_ctl_block_name(void *f_fld_ctl);
extern void * c_MHD_field_ctl_iflag(void *f_fld_ctl);
extern void * c_MHD_field_ctl_field_ctl(void *f_fld_ctl);
extern void * c_MHD_field_quad_phys_ctl(void *f_fld_ctl);
extern void * c_MHD_scalar_phys_ctl(void *f_fld_ctl);
extern void * c_MHD_vector_phys_ctl(void *f_fld_ctl);


const char label_field_ctl[NLBL_FIELD_CTL][KCHARA_C] = {
	/*[ 0]*/	{"nod_value_ctl"},
	/*[ 1]*/	{"quad_field_name_ctl"}
};

void get_label_field_ctl(int index, char *label){
    if(index < NLBL_FIELD_CTL) strngcopy(label, label_field_ctl[index]);
    return;
};



static struct chara_int2_clist * init_f_ctl_field_array(void *(*c_load_self)(void *f_parent),
                                                 void *f_parent)
{
    struct chara_int2_clist * f_field_ctl = init_chara_int2_clist();
    f_field_ctl->f_self =  c_load_self(f_parent);
    char * ctmp1 = (char *) c_chara3_array_block_name(f_field_ctl->f_self);
    f_field_ctl->clist_name = strngcopy_from_f(ctmp1);
    
    int i, num;
    num = c_chara_int_array_num(f_field_ctl->f_self);
    if(num == 0) {c_alloc_chara_int3_array(num, f_field_ctl->f_self);};
    
    struct chara3_ctl_item     *tmp_fld_ctl =  init_chara3_ctl_item_c();
    for(i=0;i<c_chara3_array_num(f_field_ctl->f_self);i++){
        struct chara_int2_ctl_item *tmp_fld_item = init_chara_int2_ctl_item_c();
        ctmp1 = (char *) c_chara3_array_c1_tbl(i, f_field_ctl->f_self);
        tmp_fld_ctl->c1_tbl = strngcopy_from_f(ctmp1);
        ctmp1 = (char *) c_chara3_array_c2_tbl(i, f_field_ctl->f_self);
        tmp_fld_ctl->c2_tbl = strngcopy_from_f(ctmp1);
        ctmp1 = (char *) c_chara3_array_c3_tbl(i, f_field_ctl->f_self);
        tmp_fld_ctl->c3_tbl = strngcopy_from_f(ctmp1);
        set_viz_flags_from_text(tmp_fld_ctl, tmp_fld_item);
        append_chara_int2_clist(tmp_fld_item, f_field_ctl);
    };
    dealloc_chara3_ctl_item_c(tmp_fld_ctl);
    return f_field_ctl;
}

struct f_MHD_fields_control * init_f_MHD_fields_control(void *(*c_load_self)(void *f_parent),
                                                              void *f_parent)
{
    struct f_MHD_fields_control *f_fld_ctl
            = (struct f_MHD_fields_control *) malloc(sizeof(struct f_MHD_fields_control));
    if(f_fld_ctl == NULL){
        printf("malloc error for f_fld_ctl\n");
        exit(0);
    };
    
    f_fld_ctl->f_self =  c_load_self(f_parent);
    
    f_fld_ctl->f_iflag =   (int *) c_MHD_field_ctl_iflag(f_fld_ctl->f_self);
    char *f_block_name =   (char *) c_MHD_field_ctl_block_name(f_fld_ctl->f_self);
    f_fld_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_fld_ctl->f_field_ctl =   init_f_ctl_field_array(c_MHD_field_ctl_field_ctl, f_fld_ctl->f_self);
    f_fld_ctl->f_quad_phys =   init_f_ctl_chara_array(c_MHD_field_quad_phys_ctl, f_fld_ctl->f_self);
    f_fld_ctl->f_scalar_phys = init_f_ctl_ci_array(c_MHD_scalar_phys_ctl, f_fld_ctl->f_self);
    f_fld_ctl->f_vector_phys = init_f_ctl_ci3_array(c_MHD_vector_phys_ctl, f_fld_ctl->f_self);
    return f_fld_ctl;
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
	
	tmp_fld_item->f_iflag[0] = 1;
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
        tmp_fld_ctl->f_iflag[0] = 0;
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
    struct chara2_int_ctl_item *tmp_item
        = chara2_int_clist_at_index(i_fld, all_fld_list->fld_list->field_label);
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
	all_fld_list->iflag_use[i_fld] = 1;
    struct chara_int2_ctl_item *ci2_input = init_chara_int2_ctl_item_c();
    update_chara_int2_ctl_item_c(tmp_item->c1_tbl, 0, 0, ci2_input);
    append_chara_int2_clist(ci2_input, f_field_ctl);
	return;
}

void delete_field_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
                         struct chara_int2_clist *f_field_ctl){
    struct chara2_int_ctl_item *tmp_item
        = chara2_int_clist_at_index(i_fld, all_fld_list->fld_list->field_label);
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
    del_chara_int2_clist_by_c_tbl(tmp_item->c1_tbl, f_field_ctl);
	return;
}

static void update_field_flag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct chara_int2_clist *f_field_ctl){
    struct chara2_int_ctl_item *tmp_item
        = chara2_int_clist_at_index(i_fld, all_fld_list->fld_list->field_label);
    struct chara_int2_ctl_item *ci2_item = find_chara_int2_ctl_item_by_cref(tmp_item->c1_tbl,
                                                                            f_field_ctl);
	
	ci2_item->i_data[0] = all_fld_list->iflag_viz[i_fld];
	ci2_item->i_data[1] = all_fld_list->iflag_monitor[i_fld];
	return;
}


void load_field_from_ctl(struct chara_int2_clist *f_field_ctl,
                         struct all_field_ctl_c *all_fld_list){
    struct chara2_int_ctl_item *tmp_item;
    int i, j, idx;
    int jst = 0;
    struct chara_int2_ctl_item *ci2_item;
    for(idx=0;idx<count_chara_int2_clist(f_field_ctl);idx++){
        ci2_item = find_chara_int2_ctl_item_by_index(idx, f_field_ctl);
        for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){
            i = (j+jst) % all_fld_list->fld_list->ntot_fields;
            tmp_item = chara2_int_clist_at_index(i, all_fld_list->fld_list->field_label);
            if(cmp_no_case_c(ci2_item->c_tbl, tmp_item->c1_tbl) > 0){
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
    struct chara2_int_ctl_item *tmp_item;
	int i;
    struct chara_int2_ctl_item *ci2_input;
	for (i=0;i<all_fld_list->fld_list->ntot_fields;i++){
        if(all_fld_list->iflag_use[i] > 0){
            tmp_item = chara2_int_clist_at_index(i, all_fld_list->fld_list->field_label);

            ci2_input = init_chara_int2_ctl_item_c();
            update_chara_int2_ctl_item_c(tmp_item->c1_tbl,
                                         all_fld_list->iflag_viz[i],
                                         all_fld_list->iflag_monitor[i],
                                         ci2_input);
            append_chara_int2_clist(ci2_input, f_field_ctl);
		};
	};
	return;
};

static void set_quadrature_flag_from_ctl(struct chara_clist *f_quad_phys,
                                         struct all_field_ctl_c *all_fld_list){
    struct chara2_int_ctl_item *tmp_item;
	int i, j, idx;
	int jst = 0;
	
	for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){all_fld_list->iflag_quad[j] = 0;}
    if(f_quad_phys == NULL) return;
    if(f_quad_phys->f_self == NULL) return;
    for(idx=0;idx<count_chara_clist(f_quad_phys);idx++){
		for (j=0;j<all_fld_list->fld_list->ntot_fields;j++){
			i = (j+jst) % all_fld_list->fld_list->ntot_fields;
            tmp_item = chara2_int_clist_at_index(i, all_fld_list->fld_list->field_label);

			if(cmp_no_case_c(chara_clist_at_index(idx, f_quad_phys)->c_tbl,
                             tmp_item->c1_tbl)){
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
    struct chara2_int_ctl_item *tmp_item;
	int i;
	for (i=0;i<all_fld_list->fld_list->ntot_fields;i++){
		if(all_fld_list->iflag_quad[i] > 0){
            tmp_item = chara2_int_clist_at_index(i, all_fld_list->fld_list->field_label);
            append_chara_clist(tmp_item->c1_tbl, f_quad_phys);
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
    struct chara2_int_ctl_item *tmp_item = chara2_int_clist_at_index(i_fld,
                                                                     all_fld_list->fld_list->field_label);
	set_no_use_all_field_ctl_c(i_fld, all_fld_list);
    del_chara_clist_by_c_tbl(tmp_item->c1_tbl, fld_ctl->f_quad_phys);
	delete_field_in_ctl(i_fld, all_fld_list, fld_ctl->f_field_ctl);
	return;
}

void update_field_flag_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
                                     struct f_MHD_fields_control *fld_ctl){
    struct chara2_int_ctl_item *tmp_item = chara2_int_clist_at_index(i_fld,
                                                                     all_fld_list->fld_list->field_label);
    update_field_flag_in_ctl(i_fld, all_fld_list, fld_ctl->f_field_ctl);
	
	if(all_fld_list->iflag_quad[i_fld] == 0){
        del_chara_clist_by_c_tbl(tmp_item->c1_tbl, fld_ctl->f_quad_phys);
	} else {
        append_chara_clist(tmp_item->c1_tbl, fld_ctl->f_quad_phys);
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
