/*
//  t_control_real3_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_real3_IO.h"


struct real3_ctl_item * init_real3_ctl_item_c(void){
	int i;
    struct real3_ctl_item *r3_item;
    if((r3_item = (struct real3_ctl_item *) malloc(sizeof(struct real3_ctl_item))) == NULL) {
        printf("malloc error for real3_ctl_item \n");
        exit(0);
    }

    for (i=0; i<3; i++) {r3_item->r_data[i] = 0.0;};
	r3_item->iflag = 0;
    return r3_item;
};

int read_real3_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real3_ctl_item *r3_item){
	char header_chara[KCHARA_C];
	
	if(r3_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %lf %lf %lf", header_chara, 
					&r3_item->r_data[0], &r3_item->r_data[1], &r3_item->r_data[2]);
		r3_item->iflag = 1;
	};
	return 1;
};

int write_real3_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct real3_ctl_item *r3_item){
    
	if(r3_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp, "%.12e  %.12e  %.12e\n", r3_item->r_data[0], 
				r3_item->r_data[1], r3_item->r_data[2]);
    return level;
};


void update_real3_ctl_item_c(double r1_in, double r2_in, double r3_in,
			struct real3_ctl_item *r3_item){
	r3_item->iflag = 1;
	r3_item->r_data[0] = r1_in;
	r3_item->r_data[1] = r2_in;
	r3_item->r_data[2] = r3_in;
	return;
};
void set_from_real3_ctl_item_c(struct real3_ctl_item *r3_item, 
			double *r1_out, double *r2_out, double *r3_out){
	if(r3_item->iflag == 0) return;
	*r1_out = r3_item->r_data[0];
	*r2_out = r3_item->r_data[1];
	*r3_out = r3_item->r_data[2];
	return;
};


static void init_real3_ctl_list(struct real3_ctl_list *head){
    head->r3_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct real3_ctl_list *add_real3_ctl_list_before(struct real3_ctl_list *current){
    struct real3_ctl_list *added;
    struct real3_ctl_list *old_prev;
    
    if ((added = (struct real3_ctl_list *) malloc(sizeof(struct real3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->r3_item = init_real3_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct real3_ctl_list *add_real3_ctl_list_after(struct real3_ctl_list *current){
    struct real3_ctl_list *added;
    struct real3_ctl_list *old_next;
    
    if ((added = (struct real3_ctl_list *) malloc(sizeof(struct real3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->r3_item = init_real3_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_real3_ctl_list(struct real3_ctl_list *current){
    struct real3_ctl_list *old_prev = current->_prev;
    struct real3_ctl_list *old_next = current->_next;
    
    free(current->r3_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_real3_ctl_list(struct real3_ctl_list *head){
	while (head->_next != NULL) {
		delete_real3_ctl_list(head->_next);
	}
    return;
};


static int count_real3_ctl_list(struct real3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct real3_ctl_list *find_r3_ctl_list_item_by_index(int index, struct real3_ctl_list *head){
    int i;
    if(index < 0 || index > count_real3_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct real3_ctl_list *find_r3_ctl_list_item_by_value(double ref_1, double ref_2,
			double ref_3, struct real3_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->r3_item->r_data[0] == ref_1 
					&& head->r3_item->r_data[1] == ref_2
					&& head->r3_item->r_data[2] == ref_3) break;
        head = head->_next;
    };
	if(head == NULL) printf("array item %lf, %lf, %lf does not exist.\n", 
				ref_1, ref_2, ref_3);
    return head;
};

static int read_real3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real3_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->r3_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_real3_ctl_list_after(head);
        iflag = read_real3_ctl_item_c(buf, label, head->r3_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_real3_ctl_list(FILE *fp, int level, const char *label, 
                       struct real3_ctl_list *head){
    if(count_real3_ctl_list(head) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_real3_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->r3_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

void append_real3_ctl_list(double r1_in, double r2_in, double r3_in, 
			struct real3_ctl_list *head){
	int num = count_real3_ctl_list(head);
    if(num > 0) head = find_r3_ctl_list_item_by_index(num-1, head);
	head = add_real3_ctl_list_after(head);
	update_real3_ctl_item_c(r1_in, r2_in, r3_in, head->r3_item);
    return;
};

void del_real3_ctl_list_by_index(int index, struct real3_ctl_list *head){
	head = find_r3_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_real3_ctl_list(head);
	return;
};

void update_real3_ctl_list_by_index(int index, double r1_in, double r2_in, double r3_in, 
			struct real3_ctl_list *head){
	head = find_r3_ctl_list_item_by_index(index, head);
	if(head != NULL) update_real3_ctl_item_c(r1_in, r2_in, r3_in, head->r3_item);
	return;
};

void set_from_real3_ctl_list_at_index(int index, struct real3_ctl_list *head,
			double *r1_out, double *r2_out, double *r3_out){
	head = find_r3_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_real3_ctl_item_c(head->r3_item, r1_out, r2_out, r3_out);
	return;
};


static void set_real3_ctl_list_by_midvalue(struct real3_ctl_list *current, double ave[3]){
	int i;
	if(current->_prev->r3_item == NULL){
        for(i=0;i<3;i++) {ave[i] = current->_next->r3_item->r_data[i];};
	}else if(current->_next->r3_item == NULL){
        for(i=0;i<3;i++) {ave[i] = current->_prev->r3_item->r_data[i];};
	}else{
		for(i=0;i<3;i++){
			ave[i] = 0.5 * (current->_prev->r3_item->r_data[i] + current->_next->r3_item->r_data[i]);
		};
	};
	return;
};
static void add_real3_ctl_list_before_c_tbl(double ref_1, double ref_2, double ref_3, 
			struct real3_ctl_list *head){
	double ave[3];
	head = find_r3_ctl_list_item_by_value(ref_1, ref_2, ref_3, head);
	if(head == NULL) return;
	head = add_real3_ctl_list_before(head);
	set_real3_ctl_list_by_midvalue(head, ave);
	update_real3_ctl_item_c(ave[0], ave[1], ave[2], head->r3_item);
	return;
};
static void add_real3_ctl_list_after_c_tbl(double ref_1, double ref_2, double ref_3, 
			struct real3_ctl_list *head){
	double ave[3];
	head = find_r3_ctl_list_item_by_value(ref_1, ref_2, ref_3, head);
	if(head == NULL) return;
	head = add_real3_ctl_list_after(head);
	set_real3_ctl_list_by_midvalue(head, ave);
	update_real3_ctl_item_c(ave[0], ave[1], ave[2], head->r3_item);
	return;
};
void del_real3_ctl_list_by_c_tbl(double ref_1, double ref_2, double ref_3, 
			struct real3_ctl_list *head){
	head = find_r3_ctl_list_item_by_value(ref_1, ref_2, ref_3, head);
	if(head != NULL) delete_real3_ctl_list(head);
	return;
};

static void update_real3_ctl_list_by_c_tbl(double ref_1, double ref_2, double ref_3, 
			double r1_in, double r2_in, double r3_in, struct real3_ctl_list *head){
	head = find_r3_ctl_list_item_by_value(ref_1, ref_2, ref_3, head);
	if(head != NULL) update_real3_ctl_item_c(r1_in, r2_in, r3_in, head->r3_item);
	return;
};

static void set_from_real3_ctl_list_at_c_tbl(double ref_1, double ref_2, double ref_3, 
			struct real3_ctl_list *head,
			double *r1_out, double *r2_out, double *r3_out){
	head = find_r3_ctl_list_item_by_value(ref_1, ref_2, ref_3, head);
	if(head != NULL) set_from_real3_ctl_item_c(head->r3_item, r1_out, r2_out, r3_out);
	return;
};


struct real3_clist * init_real3_clist(void){
    struct real3_clist *r3_clst;
    if((r3_clst = (struct real3_clist *) malloc(sizeof(struct real3_clist))) == NULL) {
        printf("malloc error for real3_clist \n");
        exit(0);
    }
    init_real3_ctl_list(&r3_clst->r3_item_head);

    r3_clst->clist_name = (char *)calloc(32,sizeof(char));
    r3_clst->r1_name = (char *)calloc(32,sizeof(char));
    r3_clst->r2_name = (char *)calloc(32,sizeof(char));
    r3_clst->r3_name = (char *)calloc(32,sizeof(char));
    return r3_clst;
};

void dealloc_real3_clist(struct real3_clist *r3_clst){
    clear_real3_ctl_list(&r3_clst->r3_item_head);
    free(r3_clst);
    return;
};
int count_real3_clist(struct real3_clist *r3_clst){
    return count_real3_ctl_list(&r3_clst->r3_item_head);
};

int read_real3_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real3_clist *r3_clst){
    return read_real3_ctl_list(fp, buf, label, &r3_clst->r3_item_head);
};
int write_real3_clist(FILE *fp, int level, const char *label, 
                       struct real3_clist *r3_clst){
    return write_real3_ctl_list(fp, level, label, &r3_clst->r3_item_head);
};

void append_real3_clist(double r1_in, double r2_in, double r3_in, 
            struct real3_clist *r3_clst){
    append_real3_ctl_list(r1_in, r2_in, r3_in, &r3_clst->r3_item_head);
    return;
};
void del_real3_clist_by_index(int index, struct real3_clist *r3_clst){
    del_real3_ctl_list_by_index(index, &r3_clst->r3_item_head);
    return;
};
void update_real3_clist_by_index(int index, double r1_in, double r2_in, double r3_in, 
            struct real3_clist *r3_clst){
    update_real3_ctl_list_by_index(index, r1_in, r2_in, r3_in, &r3_clst->r3_item_head);
    return;
};
void set_from_real3_clist_at_index(int index, struct real3_clist *r3_clst,
            double *r1_out, double *r2_out, double *r3_out){
    set_from_real3_ctl_list_at_index(index, &r3_clst->r3_item_head,
            r1_out, r2_out, r3_out);
    return;
};

void add_real3_clist_before_c_tbl(double ref_1, double ref_2, double ref_3, 
            struct real3_clist *r3_clst){
    add_real3_ctl_list_before_c_tbl(ref_1, ref_2, ref_3, &r3_clst->r3_item_head);
    return;
};
void add_real3_clist_after_c_tbl(double ref_1, double ref_2, double ref_3, 
            struct real3_clist *r3_clst){
    add_real3_ctl_list_after_c_tbl(ref_1, ref_2, ref_3, &r3_clst->r3_item_head);
    return;
};
void del_real3_clist_by_c_tbl(double ref_1, double ref_2, double ref_3, 
            struct real3_clist *r3_clst){
    del_real3_ctl_list_by_c_tbl(ref_1, ref_2, ref_3, &r3_clst->r3_item_head);
    return;
};
void update_real3_clist_by_c_tbl(double ref_1, double ref_2, double ref_3, 
            double r1_in, double r2_in, double r3_in, struct real3_clist *r3_clst){
    update_real3_ctl_list_by_c_tbl(ref_1, ref_2, ref_3, 
            r1_in, r2_in, r3_in, &r3_clst->r3_item_head);
    return;
};
void set_from_real3_clist_at_c_tbl(double ref_1, double ref_2, double ref_3, 
            struct real3_clist *r3_clst, double *r1_out, double *r2_out, double *r3_out){
    set_from_real3_ctl_list_at_c_tbl(ref_1, ref_2, ref_3, &r3_clst->r3_item_head,
            r1_out, r2_out, r3_out);
    return;
};

void copy_from_real3_ctl_list(struct real3_ctl_list *head, int num,
			double *v1, double *v2, double *v3){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        set_from_real3_ctl_item_c(head->r3_item, &v1[i], &v2[i], &v3[i]);
	};
	return;
};
void copy_to_real3_ctl_list(int num, double *v1, double *v2, double *v3,
			struct real3_ctl_list *head) {
	int i;
	
	for(i=0;i<num;i++){
		head = add_real3_ctl_list_after(head);
		update_real3_ctl_item_c(v1[i], v2[i], v3[i], head->r3_item);
	};
	return;
};

void copy_from_real3_clist(struct real3_clist *r3_clst, int num,
			double *v1, double *v2, double *v3){
	copy_from_real3_ctl_list(&r3_clst->r3_item_head, num, v1, v2, v3);
	return;
}
void copy_to_real3_clist(int num, double *v1, double *v2, double *v3,
			struct real3_clist *r3_clst){
	copy_to_real3_ctl_list(num, v1, v2, v3, &r3_clst->r3_item_head);
	return;
};

