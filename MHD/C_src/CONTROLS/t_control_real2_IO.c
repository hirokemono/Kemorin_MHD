/*
//  t_control_real2_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_real2_IO.h"


void init_real2_ctl_item_c(struct real2_ctl_item *r2_item){
	r2_item->r_data[0] = 0.0;
	r2_item->r_data[1] = 0.0;
	r2_item->iflag = 0;
    return;
};

int read_real2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real2_ctl_item *r2_item){
	char header_chara[KCHARA_C];
	
	if(r2_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %lf %lf", header_chara, 
					&r2_item->r_data[0], &r2_item->r_data[1]);
		r2_item->iflag = 1;
	};
	return 1;
};

int write_real2_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct real2_ctl_item *r2_item){
    
	if(r2_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp, "%.12e  %.12e\n", r2_item->r_data[0], r2_item->r_data[1]);
    return level;
};


void update_real2_ctl_item_c(double r1_in, double r2_in,  
                              struct real2_ctl_item *r2_item){
	r2_item->iflag = 1;
	r2_item->r_data[0] = r1_in;
	r2_item->r_data[1] = r2_in;
    return;
};
void set_from_real2_ctl_item_c(struct real2_ctl_item *r2_item,
                              double *r1_out, double *r2_out){
	if(r2_item->iflag == 0) return;
	*r1_out = r2_item->r_data[0];
	*r2_out = r2_item->r_data[1];
    return;
};



static void init_real2_ctl_list(struct real2_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static void clear_real2_ctl_list(struct real2_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        init_real2_ctl_item_c(head->r2_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

static struct real2_ctl_list *add_real2_ctl_list_before(struct real2_ctl_list *current){
    struct real2_ctl_list *added;
    struct real2_ctl_list *old_prev;
    
    if ((added = (struct real2_ctl_list *) malloc(sizeof(struct real2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->r2_item = (struct real2_ctl_item *) malloc(sizeof(struct real2_ctl_item))) == NULL) {
        printf("malloc error for r2_item\n");
        exit(0);
    }
	init_real2_ctl_item_c(added->r2_item);
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct real2_ctl_list *add_real2_ctl_list_after(struct real2_ctl_list *current){
    struct real2_ctl_list *added;
    struct real2_ctl_list *old_next;
    
    if ((added = (struct real2_ctl_list *) malloc(sizeof(struct real2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->r2_item = (struct real2_ctl_item *) malloc(sizeof(struct real2_ctl_item))) == NULL) {
        printf("malloc error for r2_item\n");
        exit(0);
    }
	init_real2_ctl_item_c(added->r2_item);
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_real2_ctl_list(struct real2_ctl_list *current){
    struct real2_ctl_list *old_prev = current->_prev;
    struct real2_ctl_list *old_next = current->_next;
    
    init_real2_ctl_item_c(current->r2_item);
    free(current->r2_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static int count_real2_ctl_list(struct real2_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct real2_ctl_list *find_r2_ctl_list_item_by_index(int index, struct real2_ctl_list *head){
    int i;
    if(index < 0 || index > count_real2_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct real2_ctl_list *find_r2_ctl_list_item_by_value(double ref_1, double ref_2,
			struct real2_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->r2_item->r_data[0] == ref_1 && head->r2_item->r_data[1] == ref_2) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %lf, %lf does not exist.\n", ref_1, ref_2);
    return head;
};
struct real2_ctl_list *find_r2_between_item_by_value1(double ref_1,
			struct real2_ctl_list *head){
    head = head->_next;
    if(ref_1 < head->r2_item->r_data[0]) return head;
    while (head->_next != NULL){
		if(head->r2_item->r_data[0] <= ref_1 
					&& ref_1 < head->_next->r2_item->r_data[0]) return head;
        head = head->_next;
    };
    return head;
};


static int read_real2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real2_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag*num_array == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_real2_ctl_list_after(head);
        iflag = read_real2_ctl_item_c(buf, label, head->r2_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

static int write_real2_ctl_list(FILE *fp, int level, const char *label, 
                       struct real2_ctl_list *head){
    
    int num = count_real2_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_real2_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->r2_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

static void append_real2_ctl_list(double r1_in, double r2_in, struct real2_ctl_list *head){
	int num = count_real2_ctl_list(head);
	if(num > 0) head = find_r2_ctl_list_item_by_index(num-1, head);
	head = add_real2_ctl_list_after(head);
	update_real2_ctl_item_c(r1_in, r2_in, head->r2_item);
    return;
};

static void del_real2_ctl_list_by_index(int index, struct real2_ctl_list *head){
	head = find_r2_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_real2_ctl_list(head);
	return;
};

static void update_real2_ctl_list_by_index(int index, double r1_in, double r2_in,
			struct real2_ctl_list *head){
	head = find_r2_ctl_list_item_by_index(index, head);
	if(head != NULL) update_real2_ctl_item_c(r1_in, r2_in, head->r2_item);
	return;
};

static void set_from_real2_ctl_list_at_index(int index, struct real2_ctl_list *head,
			double *r1_out, double *r2_out){
	head = find_r2_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_real2_ctl_item_c(head->r2_item, r1_out, r2_out);
	return;
};


static void set_real2_ctl_list_by_midvalue(struct real2_ctl_list *current, double ave[2]){
	int i;
	if(current->_prev->r2_item == NULL){
        for(i=0;i<2;i++) {ave[i] = current->_next->r2_item->r_data[i];};
	}else if(current->_next->r2_item == NULL){
        for(i=0;i<2;i++) {ave[i] = current->_prev->r2_item->r_data[i];};
	}else{
		for(i=0;i<2;i++){
			ave[i] = 0.5 * (current->_prev->r2_item->r_data[i] + current->_next->r2_item->r_data[i]);
		};
	};
	return;
};
static void add_real2_ctl_list_before_c_tbl(double ref_1, double ref_2, 
			struct real2_ctl_list *head){
	double ave[2];
	head = find_r2_ctl_list_item_by_value(ref_1, ref_2, head);
	if(head == NULL) return;
	head = add_real2_ctl_list_before(head);
	set_real2_ctl_list_by_midvalue(head, ave);
	update_real2_ctl_item_c(ave[0], ave[1], head->r2_item);
	return;
};
static void add_real2_ctl_list_after_c_tbl(double ref_1, double ref_2, 
			struct real2_ctl_list *head){
	double ave[2];
	head = find_r2_ctl_list_item_by_value(ref_1, ref_2, head);
	if(head == NULL) return;
	head = add_real2_ctl_list_after(head);
	set_real2_ctl_list_by_midvalue(head, ave);
	update_real2_ctl_item_c(ave[0], ave[1], head->r2_item);
	return;
};
static void add_real2_ctl_list_between_value1(double ref_1, double ref_2, 
			struct real2_ctl_list *head){
	head = find_r2_between_item_by_value1(ref_1, head);
	head = add_real2_ctl_list_after(head);
	update_real2_ctl_item_c(ref_1, ref_2, head->r2_item);
	return;
};
static void del_real2_ctl_list_by_c_tbl(double ref_1, double ref_2, struct real2_ctl_list *head){
	head = find_r2_ctl_list_item_by_value(ref_1, ref_2, head);
	if(head != NULL) delete_real2_ctl_list(head);
	return;
};

static void update_real2_ctl_list_by_c_tbl(double ref_1, double ref_2,
			double r1_in, double r2_in, struct real2_ctl_list *head){
	head = find_r2_ctl_list_item_by_value(ref_1, ref_2, head);
	if(head != NULL) update_real2_ctl_item_c(r1_in, r2_in, head->r2_item);
	return;
};

static void set_from_real2_ctl_list_at_c_tbl(double ref_1, double ref_2, struct real2_ctl_list *head,
			double *r1_out, double *r2_out){
	head = find_r2_ctl_list_item_by_value(ref_1, ref_2, head);
	if(head != NULL) set_from_real2_ctl_item_c(head->r2_item, r1_out, r2_out);
	return;
};

static void copy_from_real2_ctl_list(struct real2_ctl_list *head, int num,
			double *v1, double *v2){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        set_from_real2_ctl_item_c(head->r2_item, &v1[i], &v2[i]);
	};
	return;
};
static void copy_to_real2_ctl_list(int num, double *v1, double *v2,
			struct real2_ctl_list *head) {
	int i;
	
	for(i=0;i<num;i++){
		head = add_real2_ctl_list_after(head);
		update_real2_ctl_item_c(v1[i], v2[i], head->r2_item);
	};
	return;
};

static void dup_real2_ctl_list(struct real2_ctl_list *head_src,
			struct real2_ctl_list *head_tgt){
	clear_real2_ctl_list(head_tgt);
	init_real2_ctl_list(head_tgt);
	
	head_src = head_src->_next;
    while (head_src != NULL){
        head_tgt = add_real2_ctl_list_after(head_tgt);
        head_tgt->r2_item->iflag = 1;;
        head_tgt->r2_item->r_data[0] = head_src->r2_item->r_data[0];
        head_tgt->r2_item->r_data[1] = head_src->r2_item->r_data[1];
        head_src = head_src->_next;
    }
	return;
};



void init_real2_clist(struct real2_clist *r2_clst){
    init_real2_ctl_list(&r2_clst->r2_item_head);
    
    r2_clst->clist_name = (char *)calloc(32,sizeof(char));
    r2_clst->r1_name = (char *)calloc(32,sizeof(char));
    r2_clst->r2_name = (char *)calloc(32,sizeof(char));
    return;
};

void clear_real2_clist(struct real2_clist *r2_clst){
    clear_real2_ctl_list(&r2_clst->r2_item_head);
    free(r2_clst->clist_name);
    free(r2_clst->r1_name);
    free(r2_clst->r2_name);
    return;
};
int count_real2_clist(struct real2_clist *r2_clst){
    return count_real2_ctl_list(&r2_clst->r2_item_head);
};

int read_real2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real2_clist *r2_clst){
    sprintf(r2_clst->clist_name,"%s", label);
    return read_real2_ctl_list(fp, buf, label, &r2_clst->r2_item_head);
};
int write_real2_clist(FILE *fp, int level, const char *label, 
                       struct real2_clist *r2_clst){
    return write_real2_ctl_list(fp, level, label, &r2_clst->r2_item_head);
};

void append_real2_clist(double r1_in, double r2_in, struct real2_clist *r2_clst){
    append_real2_ctl_list(r1_in, r2_in, &r2_clst->r2_item_head);
    return;
};
void del_real2_clist_by_index(int index, struct real2_clist *r2_clst){
    del_real2_ctl_list_by_index(index, &r2_clst->r2_item_head);
    return;
};
void update_real2_clist_by_index(int index, double r1_in, double r2_in,
            struct real2_clist *r2_clst){
    update_real2_ctl_list_by_index(index, r1_in, r2_in, &r2_clst->r2_item_head);
    return;
};
void set_from_real2_clist_at_index(int index, struct real2_clist *r2_clst,
            double *r1_out, double *r2_out){
    set_from_real2_ctl_list_at_index(index, &r2_clst->r2_item_head,
            r1_out, r2_out);
    return;
};

void add_real2_clist_before_c_tbl(double ref_1, double ref_2, struct real2_clist *r2_clst){
    add_real2_ctl_list_before_c_tbl(ref_1, ref_2, &r2_clst->r2_item_head);
    return;
};
void add_real2_clist_after_c_tbl(double ref_1, double ref_2, struct real2_clist *r2_clst){
    add_real2_ctl_list_after_c_tbl(ref_1, ref_2, &r2_clst->r2_item_head);
    return;
};
void add_real2_clist_between_value1(double ref_1, double ref_2, struct real2_clist *r2_clst){
    add_real2_ctl_list_between_value1(ref_1, ref_2, &r2_clst->r2_item_head);
    return;
};
void del_real2_clist_by_c_tbl(double ref_1, double ref_2,
            struct real2_clist *r2_clst){
    del_real2_ctl_list_by_c_tbl(ref_1, ref_2, &r2_clst->r2_item_head);
    return;
};
void update_real2_clist_by_c_tbl(double ref_1, double ref_2, 
            double r1_in, double r2_in, struct real2_clist *r2_clst){
    update_real2_ctl_list_by_c_tbl(ref_1, ref_2,
            r1_in, r2_in, &r2_clst->r2_item_head);
    return;
};
void set_from_real2_clist_at_c_tbl(double ref_1, double ref_2,
            struct real2_clist *r2_clst, double *r1_out, double *r2_out){
    set_from_real2_ctl_list_at_c_tbl(ref_1, ref_2, &r2_clst->r2_item_head,
            r1_out, r2_out);
    return;
};

void copy_from_real2_clist(struct real2_clist *r2_clst, int num,
            double *v1, double *v2){
    copy_from_real2_ctl_list(&r2_clst->r2_item_head, num, v1, v2);
    return;
};
void copy_to_real2_clist(int num, double *v1, double *v2,
            struct real2_clist *r2_clst){
    copy_to_real2_ctl_list(num, v1, v2, &r2_clst->r2_item_head);
    return;
};
void dup_real2_clist(struct real2_clist *r2_src_clst,
            struct real2_clist *r2_tgt_clst){
    dup_real2_ctl_list(&r2_src_clst->r2_item_head, &r2_tgt_clst->r2_item_head);
    return;
};










