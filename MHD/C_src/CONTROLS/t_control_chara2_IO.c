/*
//  t_control_chara2_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara2_IO.h"


void alloc_chara2_ctl_item_c(struct chara2_ctl_item *c2_item){
	c2_item->c1_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2_item->c2_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2_item->iflag = 0;
    return;
};

void dealloc_chara2_ctl_item_c(struct chara2_ctl_item *c2_item){
    free(c2_item->c1_tbl);
    free(c2_item->c2_tbl);
	c2_item->iflag = 0;
    return;
};

int read_chara2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                           struct chara2_ctl_item *c2_item){
	char header_chara[KCHARA_C];
	
	if(c2_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s", header_chara, 
					c2_item->c1_tbl, c2_item->c2_tbl);
		strip_cautation_marks(c2_item->c1_tbl);
		strip_cautation_marks(c2_item->c2_tbl);
		c2_item->iflag = 1;
	};
	return 1;
};

int write_chara2_ctl_item_c(FILE *fp, int level, int maxlen[2], 
                           const char *label, struct chara2_ctl_item *c2_item){
    
	if(c2_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c2_item->c1_tbl);
	write_one_label_w_lf_c(fp, c2_item->c2_tbl);
    return level;
};


void update_chara2_ctl_item_c(char *c1_in, char *c2_in, 
			struct chara2_ctl_item *c2_item){
	c2_item->iflag = 1;
	sprintf(c2_item->c1_tbl,"%s", c1_in);
	sprintf(c2_item->c2_tbl,"%s", c2_in);
    return;
};
void set_from_chara2_ctl_item_c(struct chara2_ctl_item *c2_item,
			char *c1_out, char *c2_out){
	if(c2_item->iflag == 0) return;
	sprintf(c1_out,"%s", c2_item->c1_tbl);
	sprintf(c2_out,"%s", c2_item->c2_tbl);
    return;
};



static void init_chara2_ctl_list(struct chara2_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static void clear_chara2_ctl_list(struct chara2_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        dealloc_chara2_ctl_item_c(head->c2_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

static struct chara2_ctl_list *add_chara2_ctl_list_before(struct chara2_ctl_list *current){
    struct chara2_ctl_list *added;
    struct chara2_ctl_list *old_prev;
    
    if ((added = (struct chara2_ctl_list *) malloc(sizeof(struct chara2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c2_item = (struct chara2_ctl_item *) malloc(sizeof(struct chara2_ctl_item))) == NULL) {
        printf("malloc error for c2_item\n");
        exit(0);
    }
	alloc_chara2_ctl_item_c(added->c2_item);
	
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
	
    return added;
};

static struct chara2_ctl_list *add_chara2_ctl_list_after(struct chara2_ctl_list *current){
    struct chara2_ctl_list *added;
    struct chara2_ctl_list *old_next;
    
    if ((added = (struct chara2_ctl_list *) malloc(sizeof(struct chara2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c2_item = (struct chara2_ctl_item *) malloc(sizeof(struct chara2_ctl_item))) == NULL) {
        printf("malloc error for c2_item\n");
        exit(0);
    }
	alloc_chara2_ctl_item_c(added->c2_item);
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_chara2_ctl_list(struct chara2_ctl_list *current){
    struct chara2_ctl_list *old_prev = current->_prev;
    struct chara2_ctl_list *old_next = current->_next;
    
    dealloc_chara2_ctl_item_c(current->c2_item);
    free(current->c2_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static int count_maxlen_ctl_list(const char *label, 
			struct chara2_ctl_list *head, int mlen2[2]){
    int num = 0;
    mlen2[0] = (int) strlen(label);
    mlen2[1] = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->c2_item->c1_tbl) > mlen2[1]){
            mlen2[1] = (int) strlen(head->c2_item->c1_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static int count_chara2_ctl_list(struct chara2_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara2_ctl_list *find_c2_ctl_list_item_by_index(int index, struct chara2_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara2_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct chara2_ctl_list *find_c2_ctl_list_item_by_c_tbl(char *ref_1, char *ref_2,
			struct chara2_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->c2_item->c1_tbl, ref_1) 
					&& cmp_no_case_c(head->c2_item->c2_tbl, ref_2)) return head;
        head = head->_next;
    };
    return head;
};

static int read_chara2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag*num_array == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara2_ctl_list_after(head);
        iflag = read_chara2_ctl_item_c(buf, label, head->c2_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

static int write_chara2_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara2_ctl_list *head){
    int mlen2[2];
    int num = count_maxlen_ctl_list(label, head, mlen2);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara2_ctl_item_c(fp, level, mlen2, label, head->c2_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_chara2_ctl_list(char *c1_in, char *c2_in,
                      struct chara2_ctl_list *head){
	int num = count_chara2_ctl_list(head);
    if(num > 0) head = find_c2_ctl_list_item_by_index(num-1, head);
	head = add_chara2_ctl_list_after(head);
	if(head !=NULL) update_chara2_ctl_item_c(c1_in, c2_in, head->c2_item);
    return;
};
static void del_chara2_ctl_list_by_index(int index, struct chara2_ctl_list *head){
	head = find_c2_ctl_list_item_by_index(index, head);
	if(head !=NULL) delete_chara2_ctl_list(head);
	return;
};

static void update_chara2_ctl_list_by_index(int index, char *c1_in, char *c2_in,
			struct chara2_ctl_list *head){
	head = find_c2_ctl_list_item_by_index(index, head);
	if(head !=NULL) update_chara2_ctl_item_c(c1_in, c2_in, head->c2_item);
	return;
};

static void set_from_chara2_ctl_list_at_index(int index, struct chara2_ctl_list *head,
			char *c1_out, char *c2_out){
	head = find_c2_ctl_list_item_by_index(index, head);
	if(head !=NULL) set_from_chara2_ctl_item_c(head->c2_item, c1_out, c2_out);
	return;
};


static void add_chara2_ctl_list_before_c_tbl(char *ref_1, char *ref_2, 
			char *c1_in, char *c2_in, struct chara2_ctl_list *head){
	head = find_c2_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head == NULL) return;
	head = add_chara2_ctl_list_before(head);
	update_chara2_ctl_item_c(c1_in, c2_in, head->c2_item);
	return;
};
static void add_chara2_ctl_list_after_c_tbl(char *ref_1, char *ref_2,
			char *c1_in, char *c2_in, struct chara2_ctl_list *head){
	head = find_c2_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head == NULL) return;
	head = add_chara2_ctl_list_after(head);
	update_chara2_ctl_item_c(c1_in, c2_in, head->c2_item);
	return;
};
static void del_chara2_ctl_list_by_c_tbl(char *ref_1, char *ref_2,
			struct chara2_ctl_list *head){
	head = find_c2_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head != NULL) delete_chara2_ctl_list(head);
	return;
};

static void update_chara2_ctl_list_by_c_tbl(char *ref_1, char *ref_2,
			char *c1_in, char *c2_in, struct chara2_ctl_list *head){
	head = find_c2_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head != NULL) update_chara2_ctl_item_c(c1_in, c2_in, head->c2_item);
	return;
};

static void set_from_chara2_ctl_list_at_c_tbl(char *ref_1, char *ref_2, 
			struct chara2_ctl_list *head, char *c1_out, char *c2_out){
	head = find_c2_ctl_list_item_by_c_tbl(ref_1, ref_2, head);
	if(head != NULL) set_from_chara2_ctl_item_c(head->c2_item, c1_out, c2_out);
	return;
};



void init_chara2_clist(struct chara2_clist *c2_clst){
	init_chara2_ctl_list(&c2_clst->c2_item_head);
};
void clear_chara2_clist(struct chara2_clist *c2_clst){
	clear_chara2_ctl_list(&c2_clst->c2_item_head);
};
int count_chara2_clist(struct chara2_clist *c2_clst){
	 return count_chara2_ctl_list(&c2_clst->c2_item_head);
};

int read_chara2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                   struct chara2_clist *c2_clst){
	 return read_chara2_ctl_list(fp, buf, label, &c2_clst->c2_item_head);
};
int write_chara2_clist(FILE *fp, int level, const char *label, 
                    struct chara2_clist *c2_clst){
	 return write_chara2_ctl_list(fp, level, label, &c2_clst->c2_item_head);
};

void append_chara2_clist(char *c1_in, char *c2_in,
                      struct chara2_clist *c2_clst){
    append_chara2_ctl_list(c1_in, c2_in, &c2_clst->c2_item_head);
};
void del_chara2_clist_by_index(int index, struct chara2_clist *c2_clst){
    del_chara2_ctl_list_by_index(index, &c2_clst->c2_item_head);
};
void update_chara2_clist_by_index(int index, char *c1_in, char *c2_in,
            struct chara2_clist *c2_clst){
    update_chara2_ctl_list_by_index(index, c1_in, c2_in, &c2_clst->c2_item_head);
};
void set_from_chara2_clist_at_index(int index, struct chara2_clist *c2_clst,
            char *c1_out, char *c2_out){
    set_from_chara2_ctl_list_at_index(index, &c2_clst->c2_item_head,
            c1_out, c2_out);
};

void add_chara2_clist_before_c_tbl(char *ref_1, char *ref_2, char *c1_in, char *c2_in,
            struct chara2_clist *c2_clst){
    add_chara2_ctl_list_before_c_tbl(ref_1, ref_2, c1_in, c2_in, &c2_clst->c2_item_head);
};
void add_chara2_clist_after_c_tbl(char *ref_1, char *ref_2, char *c1_in, char *c2_in,
            struct chara2_clist *c2_clst){
    add_chara2_ctl_list_after_c_tbl(ref_1, ref_2, c1_in, c2_in, &c2_clst->c2_item_head);
};
void del_chara2_clist_by_c_tbl(char *ref_1, char *ref_2, struct chara2_clist *c2_clst){
    del_chara2_ctl_list_by_c_tbl(ref_1, ref_2, &c2_clst->c2_item_head);
};
void update_chara2_clist_by_c_tbl(char *ref_1, char *ref_2, char *c1_in, char *c2_in,
            struct chara2_clist *c2_clst){
    update_chara2_ctl_list_by_c_tbl(ref_1, ref_2, c1_in, c2_in, &c2_clst->c2_item_head);
};
void set_from_chara2_clist_at_c_tbl(char *ref_1, char *ref_2,
            struct chara2_clist *c2_clst, char *c1_out, char *c2_out){
    set_from_chara2_ctl_list_at_c_tbl(ref_1, ref_2, &c2_clst->c2_item_head, c1_out, c2_out);
};
