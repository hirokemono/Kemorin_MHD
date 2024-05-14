/*
//  t_control_real_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_real_IO.h"


struct real_ctl_item * init_real_ctl_item_c(void){
    struct real_ctl_item *r_item;
    if((r_item = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item))) == NULL) {
        printf("malloc error for real_ctl_item \n");
        exit(0);
    }
	if((r_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for r_item->f_iflag\n");
		exit(0);
	}
    
    r_item->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	r_item->r_data = 0.0;
    return r_item;
};

void dealloc_real_ctl_item_c(struct real_ctl_item *r_item)
{
	if(r_item->c_block_name !=NULL) free(r_item->c_block_name);
	
	r_item->f_iflag = NULL;
	r_item->f_self = NULL;
	
	free(r_item);
	return;
}



int read_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real_ctl_item *r_item){
	char header_chara[KCHARA_C];
	
	if(r_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %lf", header_chara, &r_item->r_data);
		r_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_real_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct real_ctl_item *r_item){
    
	if(r_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp,  "%.12e\n", r_item->r_data);
    return level;
};


void update_real_ctl_item_c(double r1_in, struct real_ctl_item *r_item){
	r_item->f_iflag[0] = 1;
	r_item->r_data = r1_in;
    return;
};
void set_from_real_ctl_item_c(struct real_ctl_item *r_item, double *r1_out){
	if(r_item->f_iflag[0] == 0) return;
	*r1_out = r_item->r_data;
    return;
};


static void init_real_ctl_list(struct real_ctl_list *head){
    head->r_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};


static struct real_ctl_list *add_real_ctl_list_before(struct real_ctl_list *current){
    struct real_ctl_list *added;
    struct real_ctl_list *old_prev;
    
    if ((added = (struct real_ctl_list *) malloc(sizeof(struct real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->r_item = init_real_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct real_ctl_list *add_real_ctl_list_after(struct real_ctl_list *current){
    struct real_ctl_list *added;
    struct real_ctl_list *old_next;
    
    if ((added = (struct real_ctl_list *) malloc(sizeof(struct real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->r_item = init_real_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_real_ctl_list(struct real_ctl_list *current){
    struct real_ctl_list *old_prev = current->_prev;
    struct real_ctl_list *old_next = current->_next;
    
    free(current->r_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_real_ctl_list(struct real_ctl_list *head){
    while (head->_next != NULL) {
        delete_real_ctl_list(head->_next);
	}
    return;
};


static int count_real_ctl_list(struct real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct real_ctl_list *find_r_ctl_list_item_by_index(int index, struct real_ctl_list *head){
    int i;
    if(index < 0 || index > count_real_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct real_ctl_list *find_r_ctl_list_item_by_value(double ref,
			struct real_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->r_item->r_data == ref) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %lf does not exist.\n", ref);
    return head;
};

static int read_real_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->r_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_real_ctl_list_after(head);
        iflag = read_real_ctl_item_c(buf, label, head->r_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_real_ctl_list(FILE *fp, int level, const char *label, 
                       struct real_ctl_list *head){
    if(count_real_ctl_list(head) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_real_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->r_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_real_ctl_list(double r1_in, struct real_ctl_list *head){
	int num = count_real_ctl_list(head);
    if(num > 0) head = find_r_ctl_list_item_by_index(num-1, head);
	head = add_real_ctl_list_after(head);
	update_real_ctl_item_c(r1_in, head->r_item);
    return;
};

static void del_real_ctl_list_by_index(int index, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_real_ctl_list(head);
	return;
};

static void update_real_ctl_list_by_index(int index, double r1_in, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_index(index, head);
	if(head != NULL) update_real_ctl_item_c(r1_in, head->r_item);
	return;
};

static void set_from_real_ctl_list_at_index(int index, struct real_ctl_list *head, double *r1_out){
	head = find_r_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_real_ctl_item_c(head->r_item, r1_out);
	return;
};


static double set_real_ctl_list_by_midvalue(struct real_ctl_list *current){
	double ave;
	if(current->_prev->r_item == NULL){
        ave = current->_next->r_item->r_data;
	}else if(current->_next->r_item == NULL){
        ave = current->_prev->r_item->r_data;
	}else{
        ave = 0.5 * (current->_prev->r_item->r_data + current->_next->r_item->r_data);
	};
	return ave;
};

static void add_real_ctl_list_before_c_tbl(double ref, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head == NULL) return;
	head = add_real_ctl_list_before(head);
	double ave = set_real_ctl_list_by_midvalue(head);
	update_real_ctl_item_c(ave, head->r_item);
	return;
};
static void add_real_ctl_list_after_c_tbl(double ref, double r1_in,
			struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head == NULL) return;
	head = add_real_ctl_list_after(head);
	update_real_ctl_item_c(r1_in, head->r_item);
	return;
};
static void del_real_ctl_list_by_c_tbl(double ref, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head != NULL) delete_real_ctl_list(head);
	return;
};

static void update_real_ctl_list_by_c_tbl(double ref,
			double r1_in, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head != NULL) update_real_ctl_item_c(r1_in, head->r_item);
	return;
};

static void set_from_real_ctl_list_at_c_tbl(double ref, struct real_ctl_list *head,
			double *r1_out){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head != NULL) set_from_real_ctl_item_c(head->r_item, r1_out);
	return;
};

static void copy_from_real_ctl_list(struct real_ctl_list *head, int num, double *v1){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        set_from_real_ctl_item_c(head->r_item, &v1[i]);
	};
	return;
};
static void copy_to_real_ctl_list(int num, double *v1, struct real_ctl_list *head){
	int i;
	
	for(i=0;i<num;i++){
		head = add_real_ctl_list_after(head);
		update_real_ctl_item_c(v1[i], head->r_item);
	};
	return;
};


struct real_clist * init_real_clist(void){
    struct real_clist *r_clst;
    if((r_clst = (struct real_clist *) malloc(sizeof(struct real_clist))) == NULL) {
        printf("malloc error for real_clist \n");
        exit(0);
    }
    
	init_real_ctl_list(&r_clst->r_item_head);
    
    r_clst->clist_name = (char *)calloc(32,sizeof(char));
    r_clst->r1_name = (char *)calloc(32,sizeof(char));
	return r_clst;
};

void dealloc_real_clist(struct real_clist *r_clst){
    clear_real_ctl_list(&r_clst->r_item_head);

    free(r_clst->clist_name);
    free(r_clst->r1_name);

    free(r_clst);
    return;
};
int count_real_clist(struct real_clist *r_clst){
    return count_real_ctl_list(&r_clst->r_item_head);
};

int read_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real_clist *r_clst){
    sprintf(r_clst->clist_name,"%s", label);
    return read_real_ctl_list(fp, buf, label, &r_clst->r_item_head);
};
int write_real_clist(FILE *fp, int level, const char *label, 
                       struct real_clist *r_clst){
    return write_real_ctl_list(fp, level, label, &r_clst->r_item_head);
};

void append_real_clist(double r1_in, struct real_clist *r_clst){
    append_real_ctl_list(r1_in, &r_clst->r_item_head);
    return;
};
void del_real_clist_by_index(int index, struct real_clist *r_clst){
    del_real_ctl_list_by_index(index, &r_clst->r_item_head);
    return;
};
void update_real_clist_by_index(int index, double r1_in, struct real_clist *r_clst){
    update_real_ctl_list_by_index(index, r1_in, &r_clst->r_item_head);
    return;
};
void set_from_real_clist_at_index(int index, struct real_clist *r_clst, double *r1_out){
    set_from_real_ctl_list_at_index(index, &r_clst->r_item_head, r1_out);
    return;
};

struct real_ctl_item *real_clist_at_index(int index, struct real_clist *r_clst){
    struct real_ctl_list *ct_tmp = find_r_ctl_list_item_by_index(index, &r_clst->r_item_head);
    return ct_tmp->r_item;
}



void add_real_clist_before_c_tbl(double ref, struct real_clist *r_clst){
    add_real_ctl_list_before_c_tbl(ref, &r_clst->r_item_head);
    return;
};
void add_real_clist_after_c_tbl(double ref, double r1_in, struct real_clist *r_clst){
    add_real_ctl_list_after_c_tbl(ref, r1_in, &r_clst->r_item_head);
    return;
};
void del_real_clist_by_c_tbl(double ref, struct real_clist *r_clst){
    del_real_ctl_list_by_c_tbl(ref, &r_clst->r_item_head);
    return;
};
void update_real_clist_by_c_tbl(double ref, double r1_in, struct real_clist *r_clst){
    update_real_ctl_list_by_c_tbl(ref, r1_in, &r_clst->r_item_head);
    return;
};
void set_from_real_clist_at_c_tbl(double ref, struct real_clist *r_clst, double *r1_out){
    set_from_real_ctl_list_at_c_tbl(ref, &r_clst->r_item_head, r1_out);
    return;
};

void copy_from_real_clist(struct real_clist *r_clst, int num, double *v1){
    copy_from_real_ctl_list(&r_clst->r_item_head, num, v1);
    return;
};
void copy_to_real_clist(int num, double *v1, struct real_clist *r_clst){
    copy_to_real_ctl_list(num, v1, &r_clst->r_item_head);
    return;
};
 
