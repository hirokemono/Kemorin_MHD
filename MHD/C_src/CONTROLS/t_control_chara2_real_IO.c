/*
//  t_control_chara2_real_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara2_real_IO.h"


void alloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
	c2r_item->c1_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->c2_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->r_data = 0.0;
	c2r_item->iflag = 0;
    return;
};

void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
    free(c2r_item->c1_tbl);
    free(c2r_item->c2_tbl);
	c2r_item->iflag = 0;
    return;
};

int read_c2r_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                        struct chara2_real_ctl_item *c2r_item){
	char header_chara[KCHARA_C];
	
	if(c2r_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s %lf", header_chara, 
					c2r_item->c1_tbl, c2r_item->c2_tbl, &c2r_item->r_data);
		strip_cautation_marks(c2r_item->c1_tbl);
		strip_cautation_marks(c2r_item->c2_tbl);
		c2r_item->iflag = 1;
	};
	return 1;
};

int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
                           const char *label, struct chara2_real_ctl_item *c2r_item){
    
	if(c2r_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c2r_item->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c2r_item->c2_tbl);
	fprintf(fp, "%.12e\n", c2r_item->r_data);
    return level;
};


void update_chara2_real_ctl_item_c(char *c1_in, char *c2_in, double r_in,  
                              struct chara2_real_ctl_item *c2r_item){
	c2r_item->iflag = 1;
	sprintf(c2r_item->c1_tbl,"%s", c1_in);
	sprintf(c2r_item->c2_tbl,"%s", c2_in);
	c2r_item->r_data = r_in;
    return;
};
void set_from_chara2_real_ctl_item_c(struct chara2_real_ctl_item *c2r_item,
                              char *c1_out, char *c2_out, double *r_out){
	if(c2r_item->iflag == 0) return;
	sprintf(c1_out,"%s", c2r_item->c1_tbl);
	sprintf(c2_out,"%s", c2r_item->c2_tbl);
	*r_out = c2r_item->r_data;
    return;
};



static void init_c2r_ctl_list(struct chara2_real_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static void clear_c2r_ctl_list(struct chara2_real_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        dealloc_c2r_ctl_item_c(head->c2r_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

static struct chara2_real_ctl_list *add_c2r_ctl_list_after(struct chara2_real_ctl_list *current){
    struct chara2_real_ctl_list *added;
    struct chara2_real_ctl_list *old_next;
    
    if ((added = (struct chara2_real_ctl_list *) malloc(sizeof(struct chara2_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c2r_item = (struct chara2_real_ctl_item *) malloc(sizeof(struct chara2_real_ctl_item))) == NULL) {
        printf("malloc error for c2r_item\n");
        exit(0);
    }
	alloc_c2r_ctl_item_c(added->c2r_item);
    
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
    free(current->c2r_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
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
    for(i=0;i<index;i++){head = head->_next;};
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
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_c2r_ctl_list_after(head);
        iflag = read_c2r_ctl_item_c(buf, label, head->c2r_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

static int write_c2r_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara2_real_ctl_list *head){
    int mlen3[3];
    int num = count_maxlen_c2r_ctl_list(label, head, mlen3);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
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
	head = find_c2r_ctl_list_item_by_index(num, head);
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



void init_c2r_clist(struct chara2_real_clist *c2r_clst){
	init_c2r_ctl_list(&c2r_clst->c2r_item_head);
};
void clear_c2r_clist(struct chara2_real_clist *c2r_clst){
	clear_c2r_ctl_list(&c2r_clst->c2r_item_head);
};
int count_c2r_clist(struct chara2_real_clist *c2r_clst){
	 return count_c2r_ctl_list(&c2r_clst->c2r_item_head);
};

int read_c2r_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                   struct chara2_real_clist *c2r_clst){
	 return read_c2r_ctl_list(fp, buf, label, &c2r_clst->c2r_item_head);
};
int write_c2r_clist(FILE *fp, int level, const char *label, 
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

