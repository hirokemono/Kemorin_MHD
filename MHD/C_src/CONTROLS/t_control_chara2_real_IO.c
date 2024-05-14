/*
//  t_control_chara2_real_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara2_real_IO.h"


struct chara2_real_ctl_item * init_c2r_ctl_item_c(void){
    struct chara2_real_ctl_item *c2r_item;
    if((c2r_item = (struct chara2_real_ctl_item *) malloc(sizeof(struct chara2_real_ctl_item))) == NULL) {
        printf("malloc error for c2r_item\n");
        exit(0);
    }
	if((c2r_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for c2r_item->f_iflag\n");
		exit(0);
	}

    c2r_item->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->c1_tbl =       (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->c2_tbl =       (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->r_data = 0.0;
    return c2r_item;
};

void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
    free(c2r_item->c1_tbl);
    free(c2r_item->c2_tbl);
	if(c2r_item->c_block_name !=NULL) free(c2r_item->c_block_name);
	c2r_item->f_iflag = NULL;
    
	c2r_item->f_self = NULL;
    free(c2r_item);
    return;
};

int read_c2r_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                        struct chara2_real_ctl_item *c2r_item){
	char header_chara[KCHARA_C];
	
	if(c2r_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s %lf", header_chara, 
					c2r_item->c1_tbl, c2r_item->c2_tbl, &c2r_item->r_data);
		strip_cautation_marks(c2r_item->c1_tbl);
		strip_cautation_marks(c2r_item->c2_tbl);
		c2r_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
                           const char *label, struct chara2_real_ctl_item *c2r_item){
    
	if(c2r_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c2r_item->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c2r_item->c2_tbl);
	fprintf(fp, "%.12e\n", c2r_item->r_data);
    return level;
};


void update_chara2_real_ctl_item_c(char *c1_in, char *c2_in, double r_in,  
                              struct chara2_real_ctl_item *c2r_item){
	c2r_item->f_iflag[0] = 1;
	sprintf(c2r_item->c1_tbl,"%s", c1_in);
	sprintf(c2r_item->c2_tbl,"%s", c2_in);
	c2r_item->r_data = r_in;
    return;
};
void set_from_chara2_real_ctl_item_c(struct chara2_real_ctl_item *c2r_item,
                              char *c1_out, char *c2_out, double *r_out){
	if(c2r_item->f_iflag[0] == 0) return;
	sprintf(c1_out,"%s", c2r_item->c1_tbl);
	sprintf(c2_out,"%s", c2r_item->c2_tbl);
	*r_out = c2r_item->r_data;
    return;
};



static void init_c2r_ctl_list(struct chara2_real_ctl_list *head){
    head->c2r_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct chara2_real_ctl_list *add_c2r_ctl_list_before(struct chara2_real_ctl_list *current){
    struct chara2_real_ctl_list *added;
    struct chara2_real_ctl_list *old_prev;
    
    if ((added = (struct chara2_real_ctl_list *) malloc(sizeof(struct chara2_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->c2r_item = init_c2r_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct chara2_real_ctl_list *add_c2r_ctl_list_after(struct chara2_real_ctl_list *current){
    struct chara2_real_ctl_list *added;
    struct chara2_real_ctl_list *old_next;
    
    if ((added = (struct chara2_real_ctl_list *) malloc(sizeof(struct chara2_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->c2r_item = init_c2r_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_c2r_ctl_list(struct chara2_real_ctl_list *current){
    struct chara2_real_ctl_list *old_prev = current->_prev;
    struct chara2_real_ctl_list *old_next = current->_next;
    
    dealloc_c2r_ctl_item_c(current->c2r_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_c2r_ctl_list(struct chara2_real_ctl_list *head){
    while (head->_next != NULL) {
        delete_c2r_ctl_list(head->_next);
	}
    return;
};

static int count_maxlen_c2r_ctl_list(const char *label, 
			struct chara2_real_ctl_list *head, int mlen3[3]){
    int num = 0;
    mlen3[0] = (int) strlen(label);
    mlen3[1] = 0;
    mlen3[2] = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->c2r_item->c1_tbl) > mlen3[1]){
            mlen3[1] = (int) strlen(head->c2r_item->c1_tbl);
        };
        if((int) strlen(head->c2r_item->c2_tbl) > mlen3[2]){ 
            mlen3[2] = (int) strlen(head->c2r_item->c2_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static int count_c2r_ctl_list(struct chara2_real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct chara2_real_ctl_list *find_c2r_ctl_list_item_by_index(int index,
			struct chara2_real_ctl_list *head){
    int i;
    if(index < 0 || index > count_c2r_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};

static struct chara2_real_ctl_list *find_c2r_ctl_list_item_by_c_tbl(char *ref_1, char *ref_2,
			struct chara2_real_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->c2r_item->c1_tbl, ref_1)
					&& cmp_no_case_c(head->c2r_item->c2_tbl, ref_2)) return head;
        head = head->_next;
    };
    return head;
};


static int read_c2r_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->c2r_item != NULL) return 0;
   
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_c2r_ctl_list_after(head);
        iflag = read_c2r_ctl_item_c(buf, label, head->c2r_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_c2r_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara2_real_ctl_list *head){
    int mlen3[3];
    
    if(count_maxlen_c2r_ctl_list(label, head, mlen3) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_c2r_ctl_item_c(fp, level, mlen3, label, head->c2r_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};



static void append_chara2_real_ctl_list(char *c1_in, char *c2_in, double r_in,
                      struct chara2_real_ctl_list *head){
	int num = count_c2r_ctl_list(head);
    if(num > 0) head = find_c2r_ctl_list_item_by_index(num-1, head);
	head = add_c2r_ctl_list_after(head);
	if(head !=NULL) update_chara2_real_ctl_item_c(c1_in, c2_in, r_in, head->c2r_item);
    return;
};
static void del_chara2_real_ctl_list_by_index(int index, struct chara2_real_ctl_list *head){
	head = find_c2r_ctl_list_item_by_index(index, head);
	if(head !=NULL) delete_c2r_ctl_list(head);
	return;
};

static void update_chara2_real_ctl_list_by_index(int index, char *c1_in, char *c2_in, double r_in,
			struct chara2_real_ctl_list *head){
	head = find_c2r_ctl_list_item_by_index(index, head);
	if(head !=NULL) update_chara2_real_ctl_item_c(c1_in, c2_in, r_in, head->c2r_item);
	return;
};

static void set_from_chara2_real_ctl_list_at_index(int index, struct chara2_real_ctl_list *head,
			char *c1_out, char *c2_out, double *r_out){
	head = find_c2r_ctl_list_item_by_index(index, head);
	if(head !=NULL) set_from_chara2_real_ctl_item_c(head->c2r_item, c1_out, c2_out, r_out);
	return;
};


static void add_chara2_real_ctl_list_before_c_tbl(char *ref_1, char *ref_2, 
			char *c1_in, char *c2_in, double r_in, struct chara2_real_ctl_list *head){
	head = find_c2r_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head == NULL) return;
	head = add_c2r_ctl_list_before(head);
	update_chara2_real_ctl_item_c(c1_in, c2_in, r_in, head->c2r_item);
	return;
};
static void add_chara2_real_ctl_list_after_c_tbl(char *ref_1, char *ref_2, 
			char *c1_in, char *c2_in, double r_in, struct chara2_real_ctl_list *head){
	head = find_c2r_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head == NULL) return;
	head = add_c2r_ctl_list_after(head);
	update_chara2_real_ctl_item_c(c1_in, c2_in, r_in, head->c2r_item);
	return;
};
static void del_chara2_real_ctl_list_by_c_tbl(char *ref_1, char *ref_2,
			struct chara2_real_ctl_list *head){
	head = find_c2r_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head != NULL) delete_c2r_ctl_list(head);
	return;
};

static void update_chara2_real_ctl_list_by_c_tbl(char *ref_1, char *ref_2,
			char *c1_in, char *c2_in, double r_in,
			struct chara2_real_ctl_list *head){
	head = find_c2r_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head != NULL) update_chara2_real_ctl_item_c(c1_in, c2_in, r_in, head->c2r_item);
	return;
};

static void set_from_chara2_real_ctl_list_at_c_tbl(char *ref_1, char *ref_2,
			struct chara2_real_ctl_list *head,
			char *c1_out, char *c2_out, double *r_out){
	head = find_c2r_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head != NULL) set_from_chara2_real_ctl_item_c(head->c2r_item, c1_out, c2_out, r_out);
	return;
};



struct chara2_real_clist * init_chara2_real_clist(void){
    struct chara2_real_clist *c2r_clst;
    if((c2r_clst = (struct chara2_real_clist *) malloc(sizeof(struct chara2_real_clist))) == NULL) {
        printf("malloc error for chara2_real_clist\n");
        exit(0);
    }
	init_c2r_ctl_list(&c2r_clst->c2r_item_head);

    c2r_clst->clist_name = (char *)calloc(32,sizeof(char));
    c2r_clst->c1_name = (char *)calloc(32,sizeof(char));
    c2r_clst->c2_name = (char *)calloc(32,sizeof(char));
    c2r_clst->r1_name = (char *)calloc(32,sizeof(char));
    return c2r_clst;
};
void dealloc_chara2_real_clist(struct chara2_real_clist *c2r_clst){
	clear_c2r_ctl_list(&c2r_clst->c2r_item_head);

    free(c2r_clst->clist_name);
    free(c2r_clst->c1_name);
    free(c2r_clst->c2_name);
    free(c2r_clst->r1_name);
    free(c2r_clst);
    return;
};

int count_chara2_real_clist(struct chara2_real_clist *c2r_clst){
	 return count_c2r_ctl_list(&c2r_clst->c2r_item_head);
};

int read_chara2_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                   struct chara2_real_clist *c2r_clst){
    sprintf(c2r_clst->clist_name,"%s", label);
	 return read_c2r_ctl_list(fp, buf, label, &c2r_clst->c2r_item_head);
};
int write_chara2_real_clist(FILE *fp, int level, const char *label, 
                    struct chara2_real_clist *c2r_clst){
	 return write_c2r_ctl_list(fp, level, label, &c2r_clst->c2r_item_head);
};

void append_chara2_real_clist(char *c1_in, char *c2_in, double r_in,
                      struct chara2_real_clist *c2r_clst){
    append_chara2_real_ctl_list(c1_in, c2_in, r_in, &c2r_clst->c2r_item_head);
};
void del_chara2_real_clist_by_index(int index, struct chara2_real_clist *c2r_clst){
    del_chara2_real_ctl_list_by_index(index, &c2r_clst->c2r_item_head);
};
void update_chara2_real_clist_by_index(int index, char *c1_in, char *c2_in, double r_in,
            struct chara2_real_clist *c2r_clst){
    update_chara2_real_ctl_list_by_index(index, c1_in, c2_in, r_in,
            &c2r_clst->c2r_item_head);
};
void set_from_chara2_real_clist_at_index(int index, struct chara2_real_clist *c2r_clst,
            char *c1_out, char *c2_out, double *r_out){
    set_from_chara2_real_ctl_list_at_index(index, &c2r_clst->c2r_item_head,
            c1_out, c2_out, r_out);
};

struct chara2_real_ctl_item *chara2_real_clist_at_index(int index, struct chara2_real_clist *c2r_clst){
    struct chara2_real_ctl_list *ct_tmp = find_c2r_ctl_list_item_by_index(index, &c2r_clst->c2r_item_head);
    return ct_tmp->c2r_item;
}


void add_chara2_real_clist_before_c_tbl(char *ref_1, char *ref_2,
            char *c1_in, char *c2_in, double r_in,
            struct chara2_real_clist *c2r_clst){
    add_chara2_real_ctl_list_before_c_tbl(ref_1, ref_2, c1_in, c2_in, r_in,
            &c2r_clst->c2r_item_head);
};
void add_chara2_real_clist_after_c_tbl(char *ref_1, char *ref_2,
            char *c1_in, char *c2_in, double r_in,
            struct chara2_real_clist *c2r_clst){
    add_chara2_real_ctl_list_after_c_tbl(ref_1, ref_2, c1_in, c2_in, r_in,
            &c2r_clst->c2r_item_head);
};
void del_chara2_real_clist_by_c_tbl(char *ref_1, char *ref_2,
            struct chara2_real_clist *c2r_clst){
    del_chara2_real_ctl_list_by_c_tbl(ref_1, ref_2, &c2r_clst->c2r_item_head);
};
void update_chara2_real_clist_by_c_tbl(char *ref_1, char *ref_2,
            char *c1_in, char *c2_in, double r_in,
            struct chara2_real_clist *c2r_clst){
    update_chara2_real_ctl_list_by_c_tbl(ref_1, ref_2, c1_in, c2_in, r_in,
            &c2r_clst->c2r_item_head);
};
void set_from_chara2_real_clist_at_c_tbl(char *ref_1, char *ref_2,
            struct chara2_real_clist *c2r_clst,
            char *c1_out, char *c2_out, double *r_out){
    set_from_chara2_real_ctl_list_at_c_tbl(ref_1, ref_2, &c2r_clst->c2r_item_head,
            c1_out, c2_out, r_out);
};

