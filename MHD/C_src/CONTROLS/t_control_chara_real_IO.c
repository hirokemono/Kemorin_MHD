/*
//  t_control_chara_real_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_real_IO.h"


struct chara_real_ctl_item * init_chara_real_ctl_item_c(void){
    struct chara_real_ctl_item *cr_item;
    if((cr_item = (struct chara_real_ctl_item *) malloc(sizeof(struct chara_real_ctl_item))) == NULL) {
        printf("malloc error for chara_real_ctl_item\n");
        exit(0);
    }
	if((cr_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for cr_item->f_iflag\n");
		exit(0);
	}
	cr_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	cr_item->r_data = 0.0;
    return cr_item;
};

void dealloc_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item){
	cr_item->c_block_name = NULL;
	cr_item->f_iflag = NULL;
    
	cr_item->f_self = NULL;
    free(cr_item->c_tbl);
    free(cr_item);
    return;
};

int read_chara_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                               struct chara_real_ctl_item *cr_item){
	char header_chara[KCHARA_C];
	
	if(cr_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %lf", header_chara, 
					cr_item->c_tbl, &cr_item->r_data);
		strip_cautation_marks(cr_item->c_tbl);
		cr_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_chara_real_ctl_item_c(FILE *fp, int level, int maxlen[2], 
                           const char *label, struct chara_real_ctl_item *cr_item){
    
	if(cr_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], cr_item->c_tbl);
	fprintf(fp, "%.12e\n", cr_item->r_data);
    return level;
};


void update_chara_real_ctl_item_c(char *c_in, double r_in,  
                              struct chara_real_ctl_item *cr_item){
	cr_item->f_iflag[0] = 1;
	sprintf(cr_item->c_tbl,"%s", c_in);
	cr_item->r_data = r_in;
    return;
};
void set_from_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item,
                              char *c_out, double *r_out){
	if(cr_item->f_iflag[0] == 0) return;
	sprintf(c_out,"%s", cr_item->c_tbl);
	*r_out = cr_item->r_data;
    return;
};


static void init_chara_real_ctl_list(struct chara_real_ctl_list *head){
    head->cr_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};


static struct chara_real_ctl_list *add_chara_real_ctl_list_before(struct chara_real_ctl_list *current){
    struct chara_real_ctl_list *added;
    struct chara_real_ctl_list *old_prev;
    
    if ((added = (struct chara_real_ctl_list *) malloc(sizeof(struct chara_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->cr_item = init_chara_real_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct chara_real_ctl_list *add_chara_real_ctl_list_after(struct chara_real_ctl_list *current){
    struct chara_real_ctl_list *added;
    struct chara_real_ctl_list *old_next;
    
    if ((added = (struct chara_real_ctl_list *) malloc(sizeof(struct chara_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->cr_item = init_chara_real_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_chara_real_ctl_list(struct chara_real_ctl_list *current){
    struct chara_real_ctl_list *old_prev = current->_prev;
    struct chara_real_ctl_list *old_next = current->_next;
    
    dealloc_chara_real_ctl_item_c(current->cr_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_chara_real_ctl_list(struct chara_real_ctl_list *head){
    while (head->_next != NULL) {
        delete_chara_real_ctl_list(head->_next);
	}
    return;
};

static int count_maxlen_chara_real_ctl_list(const char *label, 
			struct chara_real_ctl_list *head, int mlen2[2]){
    int num = 0;
    mlen2[0] = (int) strlen(label);
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->cr_item->c_tbl) > mlen2[1]){
            mlen2[1] = (int) strlen(head->cr_item->c_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static int count_chara_real_ctl_list(struct chara_real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct chara_real_ctl_list *find_cr_ctl_list_item_by_index(int index, struct chara_real_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara_real_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct chara_real_ctl_list *find_cr_ctl_list_item_by_c_tbl(char *ref, struct chara_real_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->cr_item->c_tbl, ref)) return head;
        head = head->_next;
    };
    return head;
};

static int read_chara_real_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->cr_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_chara_real_ctl_list_after(head);
        iflag = read_chara_real_ctl_item_c(buf, label, head->cr_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_chara_real_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_real_ctl_list *head){
	int mlen2[2];
	mlen2[0] = 0;
	mlen2[1] = 0;

    if(count_maxlen_chara_real_ctl_list(label, head, mlen2) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_real_ctl_item_c(fp, level, mlen2,
                                     label, head->cr_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

void append_chara_real_ctl_list(char *c_in, double r_in,
			struct chara_real_ctl_list *head){
	int num = count_chara_real_ctl_list(head);
	if(num > 0) head = find_cr_ctl_list_item_by_index(num-1, head);
	head = add_chara_real_ctl_list_after(head);
	if(head !=NULL) update_chara_real_ctl_item_c(c_in, r_in, head->cr_item);
    return;
};
static void del_chara_real_ctl_list_by_index(int index, struct chara_real_ctl_list *head){
	head = find_cr_ctl_list_item_by_index(index, head);
	if(head !=NULL) delete_chara_real_ctl_list(head);
	return;
};

static void update_chara_real_ctl_list_by_index(int index, char *c_in, double r_in,
			struct chara_real_ctl_list *head){
	head = find_cr_ctl_list_item_by_index(index, head);
	if(head !=NULL) update_chara_real_ctl_item_c(c_in, r_in, head->cr_item);
	return;
};

static void set_from_chara_real_ctl_list_at_index(int index, struct chara_real_ctl_list *head,
			char *c_out, double *r_out){
	head = find_cr_ctl_list_item_by_index(index, head);
	if(head !=NULL) set_from_chara_real_ctl_item_c(head->cr_item, c_out, r_out);
	return;
};



static void add_chara_real_ctl_list_before_c_tbl(char *ref, char *c_in, double r_in,
			struct chara_real_ctl_list *head){
	head = find_cr_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_real_ctl_list_before(head);
	update_chara_real_ctl_item_c(c_in, r_in, head->cr_item);
	return;
};
static void add_chara_real_ctl_list_after_c_tbl(char *ref, char *c_in, double r_in,
			struct chara_real_ctl_list *head){
	head = find_cr_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_real_ctl_list_after(head);
	update_chara_real_ctl_item_c(c_in, r_in, head->cr_item);
	return;
};
void del_chara_real_ctl_list_by_c_tbl(char *ref, struct chara_real_ctl_list *head){
	head = find_cr_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_chara_real_ctl_list(head);
	return;
};

static void update_chara_real_ctl_list_by_c_tbl(char *ref, char *c_in, double r_in,
			struct chara_real_ctl_list *head){
	head = find_cr_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) update_chara_real_ctl_item_c(c_in, r_in, head->cr_item);
	return;
};

static void set_from_chara_real_ctl_list_at_c_tbl(char *ref, struct chara_real_ctl_list *head,
			char *c_out, double *r_out){
	head = find_cr_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) set_from_chara_real_ctl_item_c(head->cr_item, c_out, r_out);
	return;
};


struct chara_real_clist * init_chara_real_clist(void){
    struct chara_real_clist *cr_clst;
    if((cr_clst = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist))) == NULL) {
        printf("malloc error for chara_real_clist\n");
        exit(0);
    }
    init_chara_real_ctl_list(&cr_clst->cr_item_head);
    
    cr_clst->clist_name = (char *)calloc(32,sizeof(char));
    cr_clst->c1_name = (char *)calloc(32,sizeof(char));
    cr_clst->r1_name = (char *)calloc(32,sizeof(char));
    return cr_clst;
};
void dealloc_chara_real_clist(struct chara_real_clist *cr_clst){
    clear_chara_real_ctl_list(&cr_clst->cr_item_head);
	
	free(cr_clst->clist_name);
    free(cr_clst->c1_name);
    free(cr_clst->r1_name);
    
    free(cr_clst);
    return;
};
int count_chara_real_clist(struct chara_real_clist *cr_clst){
    return count_chara_real_ctl_list(&cr_clst->cr_item_head);
};

int read_chara_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_real_clist *cr_clst){
    sprintf(cr_clst->clist_name,"%s", label);
    return read_chara_real_ctl_list(fp, buf, label, &cr_clst->cr_item_head);
};
int write_chara_real_clist(FILE *fp, int level, const char *label, 
                       struct chara_real_clist *cr_clst){
    return write_chara_real_ctl_list(fp, level, label, &cr_clst->cr_item_head);
};

void append_chara_real_clist(char *c_in, double r_in,
                      struct chara_real_clist *cr_clst){
    append_chara_real_ctl_list(c_in, r_in, &cr_clst->cr_item_head);
     return;
};
void del_chara_real_clist_by_index(int index, struct chara_real_clist *cr_clst){
    del_chara_real_ctl_list_by_index(index, &cr_clst->cr_item_head);
     return;
};
void update_chara_real_clist_by_index(int index, char *c_in, double r_in,
            struct chara_real_clist *cr_clst){
    update_chara_real_ctl_list_by_index(index, c_in, r_in, &cr_clst->cr_item_head);
     return;
};
void set_from_chara_real_clist_at_index(int index, struct chara_real_clist *cr_clst,
            char *c_out, double *r_out){
    set_from_chara_real_ctl_list_at_index(index, &cr_clst->cr_item_head,
            c_out, r_out);
     return;
};

struct chara_real_ctl_item *chara_real_clist_at_index(int index, struct chara_real_clist *cr_clst){
    struct chara_real_ctl_list *ct_tmp = find_cr_ctl_list_item_by_index(index, &cr_clst->cr_item_head);
    return ct_tmp->cr_item;
}


void add_chara_real_clist_before_c_tbl(char *ref, char *c_in, double r_in, struct chara_real_clist *cr_clst){
    add_chara_real_ctl_list_before_c_tbl(ref, c_in, r_in, &cr_clst->cr_item_head);
    return;
};
void add_chara_real_clist_after_c_tbl(char *ref, char *c_in, double r_in, struct chara_real_clist *cr_clst){
    add_chara_real_ctl_list_after_c_tbl(ref, c_in, r_in, &cr_clst->cr_item_head);
    return;
};
void del_chara_real_clist_by_c_tbl(char *ref, struct chara_real_clist *cr_clst){
    del_chara_real_ctl_list_by_c_tbl(ref, &cr_clst->cr_item_head);
    return;
};
void update_chara_real_clist_by_c_tbl(char *ref, char *c_in, double r_in,
            struct chara_real_clist *cr_clst){
    update_chara_real_ctl_list_by_c_tbl(ref, c_in, r_in,
            &cr_clst->cr_item_head);
    return;
};
void set_from_chara_real_clist_at_c_tbl(char *ref, struct chara_real_clist *cr_clst,
            char *c_out, double *r_out){
    set_from_chara_real_ctl_list_at_c_tbl(ref, &cr_clst->cr_item_head,
            c_out, r_out);
    return;
};





