/*
//  t_control_int_real_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_int_real_IO.h"


struct int_real_ctl_item * init_int_real_ctl_item_c(void){
    struct int_real_ctl_item *ir_item;
    if((ir_item = (struct int_real_ctl_item *) malloc(sizeof(struct int_real_ctl_item))) == NULL) {
        printf("malloc error for int_real_ctl_item \n");
        exit(0);
    }
	if((ir_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for ir_item->f_iflag\n");
		exit(0);
	}
    
	ir_item->i_data = 0;
	ir_item->r_data = 0.0;
    return ir_item;
};

void dealloc_f_ctl_ir_item(struct int_real_ctl_item *ir_item)
{
	if(ir_item->c_block_name !=NULL) free(ir_item->c_block_name);
	
	ir_item->f_iflag = NULL;
	ir_item->f_self = NULL;
	return;
}

int read_int_real_ctl_item_c(char buf[LENGTHBUF], const char *label,
                          struct int_real_ctl_item *ir_item){
	char header_chara[KCHARA_C];
	
	if(ir_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %d %lf", header_chara, 
					&ir_item->i_data, &ir_item->r_data);
		ir_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_int_real_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct int_real_ctl_item *ir_item){
    
	if(ir_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp, "%d  %.12e\n", ir_item->i_data, ir_item->r_data);
    return level;
};


void update_int_real_ctl_item_c(int i1_in, double r2_in,  
			struct int_real_ctl_item *ir_item){
	ir_item->f_iflag[0] = 1;
	ir_item->i_data = i1_in;
	ir_item->r_data = r2_in;
    return;
};
void set_from_int_real_ctl_item_c(struct int_real_ctl_item *ir_item,
                              int *i1_out, double *r2_out){
	if(ir_item->f_iflag[0] == 0) return;
	*i1_out = ir_item->r_data;
	*r2_out = ir_item->r_data;
    return;
};



static void init_int_real_ctl_list(struct int_real_ctl_list *head){
    head->ir_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct int_real_ctl_list *add_int_real_ctl_list_before(struct int_real_ctl_list *current){
    struct int_real_ctl_list *added;
    struct int_real_ctl_list *old_prev;
    
    if ((added = (struct int_real_ctl_list *) malloc(sizeof(struct int_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    added->ir_item = init_int_real_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct int_real_ctl_list *add_int_real_ctl_list_after(struct int_real_ctl_list *current){
    struct int_real_ctl_list *added;
    struct int_real_ctl_list *old_next;
    
    if ((added = (struct int_real_ctl_list *) malloc(sizeof(struct int_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ir_item = init_int_real_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_int_real_ctl_list(struct int_real_ctl_list *current){
    struct int_real_ctl_list *old_prev = current->_prev;
    struct int_real_ctl_list *old_next = current->_next;
    
    free(current->ir_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_int_real_ctl_list(struct int_real_ctl_list *head){
    while (head->_next != NULL) {
        delete_int_real_ctl_list(head->_next);
	}
    return;
};


static int count_int_real_ctl_list(struct int_real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct int_real_ctl_list *find_ir_ctl_list_item_by_index(int index, struct int_real_ctl_list *head){
    int i;
    if(index < 0 || index > count_int_real_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct int_real_ctl_list *find_ir_ctl_list_item_by_value(int iref_1, double ref_2,
			struct int_real_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->ir_item->i_data == iref_1 && head->ir_item->r_data == ref_2) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %d, %lf does not exist.\n", iref_1, ref_2);
    return head;
};

static int read_int_real_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->ir_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_int_real_ctl_list_after(head);
        iflag = read_int_real_ctl_item_c(buf, label, head->ir_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_int_real_ctl_list(FILE *fp, int level, const char *label, 
                       struct int_real_ctl_list *head){
    if(count_int_real_ctl_list(head) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_int_real_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->ir_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_int_real_ctl_list(int i1_in, double r2_in, struct int_real_ctl_list *head){
	int num = count_int_real_ctl_list(head);
    if(num > 0) head = find_ir_ctl_list_item_by_index(num-1, head);
	head = add_int_real_ctl_list_after(head);
	update_int_real_ctl_item_c(i1_in, r2_in, head->ir_item);
    return;
};

static void del_int_real_ctl_list_by_index(int index, struct int_real_ctl_list *head){
	head = find_ir_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_int_real_ctl_list(head);
	return;
};

static void update_int_real_ctl_list_by_index(int index, int i1_in, double r2_in,
			struct int_real_ctl_list *head){
	head = find_ir_ctl_list_item_by_index(index, head);
	if(head != NULL) update_int_real_ctl_item_c(i1_in, r2_in, head->ir_item);
	return;
};

static void set_from_int_real_ctl_list_at_index(int index, struct int_real_ctl_list *head,
			int *i1_out, double *r2_out){
	head = find_ir_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_int_real_ctl_item_c(head->ir_item, i1_out, r2_out);
	return;
};


static void set_int_real_ctl_list_by_midvalue(struct int_real_ctl_list *current, int *iave, double *rave){
	if(current->_prev->ir_item == NULL){
        *iave = current->_next->ir_item->i_data;
        *rave = current->_next->ir_item->r_data;
	}else if(current->_next->ir_item == NULL){
        *iave = current->_next->ir_item->i_data;
        *rave = current->_next->ir_item->r_data;
	}else{
        *iave = 0.5 * (current->_prev->ir_item->i_data + current->_next->ir_item->i_data);
        *rave = 0.5 * (current->_prev->ir_item->r_data + current->_next->ir_item->r_data);
	};
	return;
};

static void add_int_real_ctl_list_before_c_tbl(int iref_1, double ref_2,
                                               struct int_real_ctl_list *head){
    int iave;
    double rave;
	head = find_ir_ctl_list_item_by_value(iref_1, ref_2, head);
	if(head == NULL) return;
	head = add_int_real_ctl_list_before(head);
	set_int_real_ctl_list_by_midvalue(head, &iave, &rave);
	update_int_real_ctl_item_c(iave, rave, head->ir_item);
	return;
};
static void add_int_real_ctl_list_after_c_tbl(int iref_1, double ref_2,
                                              struct int_real_ctl_list *head){
    int iave;
    double rave;
	head = find_ir_ctl_list_item_by_value(iref_1, ref_2, head);
	if(head == NULL) return;
	head = add_int_real_ctl_list_after(head);
	set_int_real_ctl_list_by_midvalue(head, &iave, &rave);
	update_int_real_ctl_item_c(iave, rave, head->ir_item);
	return;
};
static void del_int_real_ctl_list_by_c_tbl(int iref_1, double ref_2, struct int_real_ctl_list *head){
	head = find_ir_ctl_list_item_by_value(iref_1, ref_2, head);
	if(head != NULL) delete_int_real_ctl_list(head);
	return;
};

static void update_int_real_ctl_list_by_c_tbl(int iref_1, double ref_2,
			int i1_in, double r2_in, struct int_real_ctl_list *head){
	head = find_ir_ctl_list_item_by_value(iref_1, ref_2, head);
	if(head != NULL) update_int_real_ctl_item_c(i1_in, r2_in, head->ir_item);
	return;
};

static void set_from_int_real_ctl_list_at_c_tbl(int iref_1, double ref_2, struct int_real_ctl_list *head,
			int *i1_out, double *r2_out){
	head = find_ir_ctl_list_item_by_value(iref_1, ref_2, head);
	if(head != NULL) set_from_int_real_ctl_item_c(head->ir_item, i1_out, r2_out);
	return;
};

static void copy_from_int_real_ctl_list(struct int_real_ctl_list *head, int num,
			int *iv1, double *v2){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        set_from_int_real_ctl_item_c(head->ir_item, &iv1[i], &v2[i]);
	};
	return;
};
static void copy_to_int_real_ctl_list(int num, int *iv1, double *v2,
			struct int_real_ctl_list *head) {
	int i;
	
	for(i=0;i<num;i++){
		head = add_int_real_ctl_list_after(head);
		update_int_real_ctl_item_c(iv1[i], v2[i], head->ir_item);
	};
	return;
};




struct int_real_clist * init_int_real_clist(void){
    struct int_real_clist *ir_clst;
    if((ir_clst = (struct int_real_clist *) malloc(sizeof(struct int_real_clist))) == NULL) {
        printf("malloc error for int_real_clist \n");
        exit(0);
    }
    init_int_real_ctl_list(&ir_clst->ir_item_head);
    
    ir_clst->clist_name = (char *)calloc(32,sizeof(char));
    ir_clst->i1_name = (char *)calloc(32,sizeof(char));
    ir_clst->r1_name = (char *)calloc(32,sizeof(char));
    return ir_clst;
};

void dealloc_int_real_clist(struct int_real_clist *ir_clst){
    clear_int_real_ctl_list(&ir_clst->ir_item_head);
    
    free(ir_clst->clist_name);
    free(ir_clst->i1_name);
    free(ir_clst->r1_name);

    free(ir_clst);
    return;
};
int count_int_real_clist(struct int_real_clist *ir_clst){
    return count_int_real_ctl_list(&ir_clst->ir_item_head);
};

int read_int_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_real_clist *ir_clst){
    sprintf(ir_clst->clist_name,"%s", label);
    return read_int_real_ctl_list(fp, buf, label, &ir_clst->ir_item_head);
};
int write_int_real_clist(FILE *fp, int level, const char *label, 
                       struct int_real_clist *ir_clst){
    return write_int_real_ctl_list(fp, level, label, &ir_clst->ir_item_head);
};

void append_int_real_clist(int i1_in, double r2_in, struct int_real_clist *ir_clst){
    append_int_real_ctl_list(i1_in, r2_in, &ir_clst->ir_item_head);
    return;
};
void del_int_real_clist_by_index(int index, struct int_real_clist *ir_clst){
    del_int_real_ctl_list_by_index(index, &ir_clst->ir_item_head);
    return;
};
void update_int_real_clist_by_index(int index, int i1_in, double r2_in,
            struct int_real_clist *ir_clst){
    update_int_real_ctl_list_by_index(index, i1_in, r2_in, &ir_clst->ir_item_head);
    return;
};
void set_from_int_real_clist_at_index(int index, struct int_real_clist *ir_clst,
            int *i1_out, double *r2_out){
    set_from_int_real_ctl_list_at_index(index, &ir_clst->ir_item_head,
            i1_out, r2_out);
    return;
};

struct int_real_ctl_item *int_real_clist_at_index(int index, struct int_real_clist *ir_clst){
    struct int_real_ctl_list *ct_tmp = find_ir_ctl_list_item_by_index(index, &ir_clst->ir_item_head);
    return ct_tmp->ir_item;
}


void add_int_real_clist_before_c_tbl(int iref_1, double ref_2, struct int_real_clist *ir_clst){
    add_int_real_ctl_list_before_c_tbl(iref_1, ref_2, &ir_clst->ir_item_head);
    return;
};
void add_int_real_clist_after_c_tbl(int iref_1, double ref_2, struct int_real_clist *ir_clst){
    add_int_real_ctl_list_after_c_tbl(iref_1, ref_2, &ir_clst->ir_item_head);
    return;
};
void del_int_real_clist_by_c_tbl(int iref_1, double ref_2,
            struct int_real_clist *ir_clst){
    del_int_real_ctl_list_by_c_tbl(iref_1, ref_2, &ir_clst->ir_item_head);
    return;
};
void update_int_real_clist_by_c_tbl(int iref_1, double ref_2, 
            int i1_in, double r2_in, struct int_real_clist *ir_clst){
    update_int_real_ctl_list_by_c_tbl(iref_1, ref_2,
            i1_in, r2_in, &ir_clst->ir_item_head);
    return;
};
void set_from_int_real_clist_at_c_tbl(int iref_1, double ref_2,
            struct int_real_clist *ir_clst, int *i1_out, double *r2_out){
    set_from_int_real_ctl_list_at_c_tbl(iref_1, ref_2, &ir_clst->ir_item_head,
            i1_out, r2_out);
    return;
};

void copy_from_int_real_clist(struct int_real_clist *ir_clst, int num,
            int *iv1, double *v2){
    copy_from_int_real_ctl_list(&ir_clst->ir_item_head, num, iv1, v2);
    return;
};
void copy_to_int_real_clist(int num, int *iv1, double *v2,
            struct int_real_clist *ir_clst){
    copy_to_int_real_ctl_list(num, iv1, v2, &ir_clst->ir_item_head);
    return;
};

