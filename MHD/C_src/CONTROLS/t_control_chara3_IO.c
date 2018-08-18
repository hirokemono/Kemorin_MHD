/*
//  t_control_chara3_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara3_IO.h"


void alloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item){
	c3_item->c1_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c3_item->c2_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c3_item->c3_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c3_item->iflag = 0;
    return;
};

void dealloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item){
    free(c3_item->c1_tbl);
    free(c3_item->c2_tbl);
    free(c3_item->c3_tbl);
	c3_item->iflag = 0;
    return;
};

int read_chara3_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct chara3_ctl_item *c3_item){
	char header_chara[KCHARA_C];
	
	if(c3_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s %s", header_chara, 
					c3_item->c1_tbl, c3_item->c2_tbl, c3_item->c3_tbl);
		strip_cautation_marks(c3_item->c1_tbl);
		strip_cautation_marks(c3_item->c2_tbl);
		strip_cautation_marks(c3_item->c3_tbl);
		c3_item->iflag = 1;
	};
	return 1;
};

int write_chara3_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara3_ctl_item *c3_item){
	if(c3_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c3_item->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c3_item->c2_tbl);
	write_one_label_w_lf_c(fp, c3_item->c3_tbl);
    return level;
};


void update_chara3_ctl_item_c(char *c1_in, char *c2_in, char *c3_in,  
			struct chara3_ctl_item *c3_item){
	c3_item->iflag = 1;
	sprintf(c3_item->c1_tbl,"%s", c1_in);
	sprintf(c3_item->c2_tbl,"%s", c2_in);
	sprintf(c3_item->c3_tbl,"%s", c3_in);
    return;
};
void set_from_chara3_ctl_item_c(struct chara3_ctl_item *c3_item,
                              char *c1_out, char *c2_out, char *c3_out){
	if(c3_item->iflag == 0) return;
	sprintf(c1_out,"%s", c3_item->c1_tbl);
	sprintf(c2_out,"%s", c3_item->c2_tbl);
	sprintf(c3_out,"%s", c3_item->c3_tbl);
    return;
};


static void init_chara3_ctl_list(struct chara3_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static void clear_chara3_ctl_list(struct chara3_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        dealloc_chara3_ctl_item_c(head->c3_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

static struct chara3_ctl_list *add_chara3_ctl_list_after(struct chara3_ctl_list *current){
    struct chara3_ctl_list *added;
    struct chara3_ctl_list *old_next;
    
    if ((added = (struct chara3_ctl_list *) malloc(sizeof(struct chara3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c3_item = (struct chara3_ctl_item *) malloc(sizeof(struct chara3_ctl_item))) == NULL) {
        printf("malloc error for c3_item\n");
        exit(0);
    }
	alloc_chara3_ctl_item_c(added->c3_item);
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_chara3_ctl_list(struct chara3_ctl_list *current){
    struct chara3_ctl_list *old_prev = current->_prev;
    struct chara3_ctl_list *old_next = current->_next;
    
    dealloc_chara3_ctl_item_c(current->c3_item);
    free(current->c3_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static int count_maxlen_chara3_ctl_list(const char *label, 
			struct chara3_ctl_list *head, int mlen3[3]){
    int num = 0;
    mlen3[0] = (int) strlen(label);
    mlen3[1] = 0;
    mlen3[2] = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->c3_item->c1_tbl) > mlen3[1]){
            mlen3[1] = (int) strlen(head->c3_item->c1_tbl);
        };
        if((int) strlen(head->c3_item->c2_tbl) > mlen3[2]){ 
            mlen3[2] = (int) strlen(head->c3_item->c2_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static int count_chara3_ctl_list(struct chara3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara3_ctl_list *find_c3_ctl_list_item_by_index(int index, struct chara3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if(num == index) break;
        head = head->_next;
        num = num + 1;
    };
    if(head == NULL) printf("array does not exist at index %d of %d.\n", index, num);
    return head;
};

static struct chara3_ctl_list *find_c3_ctl_list_item_by_c_tbl(char *ref_1,
			char *ref_2, char *ref_3, struct chara3_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->c3_item->c1_tbl, ref_1)
					&& cmp_no_case_c(head->c3_item->c2_tbl, ref_2)
					&& cmp_no_case_c(head->c3_item->c3_tbl, ref_3)) return head;
        head = head->_next;
    };
    return head;
};

static int read_chara3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara3_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara3_ctl_list_after(head);
        iflag = read_chara3_ctl_item_c(buf, label, head->c3_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

static int write_chara3_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara3_ctl_list *head){
    int mlen3[3];
    int num = count_maxlen_chara3_ctl_list(label, head, mlen3);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara3_ctl_item_c(fp, level, mlen3,
                                     label, head->c3_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_chara3_ctl_list(char *c1_in, char *c2_in, char *c3_in,
                      struct chara3_ctl_list *head){
	int num = count_chara3_ctl_list(head);
	head = find_c3_ctl_list_item_by_index(num, head);
	head = add_chara3_ctl_list_after(head);
	if(head !=NULL) update_chara3_ctl_item_c(c1_in, c2_in, c3_in, head->c3_item);
    return;
};
static void del_chara3_ctl_list_by_index(int index, struct chara3_ctl_list *head){
	head = find_c3_ctl_list_item_by_index(index, head);
	if(head !=NULL) delete_chara3_ctl_list(head);
	return;
};

static void update_chara3_ctl_list_by_index(int index, char *c1_in, char *c2_in, char *c3_in,
			struct chara3_ctl_list *head){
	head = find_c3_ctl_list_item_by_index(index, head);
	if(head !=NULL) update_chara3_ctl_item_c(c1_in, c2_in, c3_in, head->c3_item);
	return;
};

static void set_from_chara3_ctl_list_at_index(int index, struct chara3_ctl_list *head,
			char *c1_out, char *c2_out,  char *c3_out){
	head = find_c3_ctl_list_item_by_index(index, head);
	if(head !=NULL) set_from_chara3_ctl_item_c(head->c3_item, c1_out, c2_out, c3_out);
	return;
};



static void del_chara3_ctl_list_by_c_tbl(char *ref_1, char *ref_2, char *ref_3,
			struct chara3_ctl_list *head){
	head = find_c3_ctl_list_item_by_c_tbl(ref_1, ref_2, ref_3, head);
	if(head != NULL) delete_chara3_ctl_list(head);
	return;
};

static void update_chara3_ctl_list_by_c_tbl(char *ref_1, char *ref_2, char *ref_3,
			char *c1_in, char *c2_in, char *c3_in,
			struct chara3_ctl_list *head){
	head = find_c3_ctl_list_item_by_c_tbl(ref_1, ref_2, ref_3, head);
	if(head != NULL) update_chara3_ctl_item_c(c1_in, c2_in, c3_in, head->c3_item);
	return;
};

static void set_from_chara3_ctl_list_at_c_tbl(char *ref_1, char *ref_2, char *ref_3,
			struct chara3_ctl_list *head,
			char *c1_out, char *c2_out, char *c3_out){
	head = find_c3_ctl_list_item_by_c_tbl(ref_1, ref_2, ref_3, head);
	if(head != NULL) set_from_chara3_ctl_item_c(head->c3_item, c1_out, c2_out, c3_out);
	return;
};



void init_chara3_clist(struct chara3_clist *c3_clst){
	init_chara3_ctl_list(&c3_clst->c3_item_head);
};
void clear_chara3_clist(struct chara3_clist *c3_clst){
	clear_chara3_ctl_list(&c3_clst->c3_item_head);
};
int count_chara3_clist(struct chara3_clist *c3_clst){
	 return count_chara3_ctl_list(&c3_clst->c3_item_head);
};

int read_chara3_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                   struct chara3_clist *c3_clst){
	 return read_chara3_ctl_list(fp, buf, label, &c3_clst->c3_item_head);
};
int write_chara3_clist(FILE *fp, int level, const char *label, 
                    struct chara3_clist *c3_clst){
	 return write_chara3_ctl_list(fp, level, label, &c3_clst->c3_item_head);
};

void append_chara3_clist(char *c1_in, char *c2_in, char *c3_in,
                      struct chara3_clist *c3_clst){
    append_chara3_ctl_list(c1_in, c2_in, c3_in, &c3_clst->c3_item_head);
};
void del_chara3_clist_by_index(int index, struct chara3_clist *c3_clst){
    del_chara3_ctl_list_by_index(index, &c3_clst->c3_item_head);
};
void update_chara3_clist_by_index(int index, char *c1_in, char *c2_in, char *c3_in,
            struct chara3_clist *c3_clst){
    update_chara3_ctl_list_by_index(index, c1_in, c2_in, c3_in,
            &c3_clst->c3_item_head);
};
void set_from_chara3_clist_at_index(int index, struct chara3_clist *c3_clst,
            char *c1_out, char *c2_out, char *c3_out){
    set_from_chara3_ctl_list_at_index(index, &c3_clst->c3_item_head,
            c1_out, c2_out, c3_out);
};

void del_chara3_clist_by_c_tbl(char *ref_1, char *ref_2, char *ref_3,
            struct chara3_clist *c3_clst){
    del_chara3_ctl_list_by_c_tbl(ref_1, ref_2, ref_3, &c3_clst->c3_item_head);
};
void update_chara3_clist_by_c_tbl(char *ref_1, char *ref_2, char *ref_3,
            char *c1_in, char *c2_in, char *c3_in, struct chara3_clist *c3_clst){
    update_chara3_ctl_list_by_c_tbl(ref_1, ref_2, ref_3, c1_in, c2_in, c3_in,
            &c3_clst->c3_item_head);
};
void set_from_chara3_clist_at_c_tbl(char *ref_1, char *ref_2, char *ref_3,
            struct chara3_clist *c3_clst, char *c1_out, char *c2_out, char *c3_out){
    set_from_chara3_ctl_list_at_c_tbl(ref_1, ref_2, ref_3, &c3_clst->c3_item_head,
            c1_out, c2_out, c3_out);
};

