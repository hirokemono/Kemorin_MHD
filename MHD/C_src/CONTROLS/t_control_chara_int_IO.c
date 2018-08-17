/*
//  t_control_chara_int_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_int_IO.h"


void alloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item){
	ci_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	ci_item->i_data = 0;
	ci_item->iflag = 0;
    return;
};

void dealloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item){
    free(ci_item->c_tbl);
	ci_item->i_data = 0;
	ci_item->iflag = 0;
    return;
};

int read_chara_int_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                              struct chara_int_ctl_item *ci_item){
	char header_chara[KCHARA_C];
	
	if(ci_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %d", header_chara, 
					ci_item->c_tbl, &ci_item->i_data);
		strip_cautation_marks(ci_item->c_tbl);
		ci_item->iflag = 1;
	};
	return 1;
};

int write_chara_int_ctl_item_c(FILE *fp, int level, int maxlen[2], 
                           const char *label, struct chara_int_ctl_item *ci_item){
    
	if(ci_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], ci_item->c_tbl);
	fprintf(fp, "%d\n", ci_item->i_data);
    return level;
};

void update_chara_int_ctl_item_c(char *c_in, int i1_in,
                              struct chara_int_ctl_item *ci_item){
	ci_item->iflag = 1;
	sprintf(ci_item->c_tbl,"%s", c_in);
	ci_item->i_data = i1_in;
    return;
};
void set_from_chara_int_ctl_item_c( struct chara_int_ctl_item *ci_item,
                              char *c_out, int *i1_out){
	if(ci_item->iflag == 0) return;
	sprintf(c_out,"%s", ci_item->c_tbl);
	*i1_out = ci_item->i_data;
    return;
};


static void init_chara_int_ctl_list(struct chara_int_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static void clear_chara_int_ctl_list(struct chara_int_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        dealloc_chara_int_ctl_item_c(head->ci_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

static struct chara_int_ctl_list *add_chara_int_ctl_list(struct chara_int_ctl_list *current){
    struct chara_int_ctl_list *added;
    struct chara_int_ctl_list *old_next;
    
    if ((added = (struct chara_int_ctl_list *) malloc(sizeof(struct chara_int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->ci_item = (struct chara_int_ctl_item *) malloc(sizeof(struct chara_int_ctl_item))) == NULL) {
        printf("malloc error for ci_item\n");
        exit(0);
    }
	alloc_chara_int_ctl_item_c(added->ci_item);
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_chara_int_ctl_list(struct chara_int_ctl_list *current){
    struct chara_int_ctl_list *old_prev = current->_prev;
    struct chara_int_ctl_list *old_next = current->_next;
    
    dealloc_chara_int_ctl_item_c(current->ci_item);
    free(current->ci_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static int count_maxlen_chara_int_ctl_list(const char *label, 
			struct chara_int_ctl_list *head, int mlen2[2]){
    int num = 0;
	mlen2[0] = (int) strlen(label);
	mlen2[1] = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->ci_item->c_tbl) > mlen2[1]){
            mlen2[1] = (int) strlen(head->ci_item->c_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static int count_chara_int_ctl_list(struct chara_int_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct chara_int_ctl_list *find_ci_ctl_list_item_by_index(int index, struct chara_int_ctl_list *head){
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
static struct chara_int_ctl_list *find_ci_ctl_list_item_by_c_tbl(char *ref, struct chara_int_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->ci_item->c_tbl, ref)) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %s does not exist.\n", ref);
    return head;
};

static int read_chara_int_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara_int_ctl_list(head);
        iflag = read_chara_int_ctl_item_c(buf, label, head->ci_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

static int write_chara_int_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_int_ctl_list *head){
    int mlen2[2];
    int num = count_maxlen_chara_int_ctl_list(label, head, mlen2);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_int_ctl_item_c(fp, level, mlen2, label, head->ci_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_chara_int_ctl_list(char *c_in, int i1_in,
                      struct chara_int_ctl_list *head){
	int num = count_chara_int_ctl_list(head);
	head = find_ci_ctl_list_item_by_index(num, head);
	head = add_chara_int_ctl_list(head);
	update_chara_int_ctl_item_c(c_in, i1_in, head->ci_item);
    return;
};

static void del_chara_int_ctl_list_by_index(int index, struct chara_int_ctl_list *head){
	head = find_ci_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_chara_int_ctl_list(head);
	return;
};

static void update_chara_int_ctl_list_by_index(int index, char *c_in, int i1_in,
			struct chara_int_ctl_list *head){
	head = find_ci_ctl_list_item_by_index(index, head);
	if(head != NULL) update_chara_int_ctl_item_c(c_in, i1_in, head->ci_item);
	return;
};

static void set_from_chara_int_ctl_list_at_index(int index, struct chara_int_ctl_list *head,
			char *c_out, int *i1_out){
	head = find_ci_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_chara_int_ctl_item_c(head->ci_item, c_out, i1_out);
	return;
};


static void del_chara_int_ctl_list_by_c_tbl(char *ref, struct chara_int_ctl_list *head){
	head = find_ci_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_chara_int_ctl_list(head);
	return;
};

static void update_chara_int_ctl_list_by_c_tbl(char *ref, char *c_in, int i1_in,
			struct chara_int_ctl_list *head){
	head = find_ci_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) update_chara_int_ctl_item_c(c_in, i1_in, head->ci_item);
	return;
};

static void set_from_chara_int_ctl_list_at_c_tbl(char *ref, struct chara_int_ctl_list *head,
			char *c_out, int *i1_out){
	head = find_ci_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) set_from_chara_int_ctl_item_c(head->ci_item, c_out, i1_out);
	return;
};


void init_chara_int_clist(struct chara_int_clist *ci_clst){
	init_chara_int_ctl_list(&ci_clst->ci_item_head);
	return;
};
void clear_chara_int_clist(struct chara_int_clist *ci_clst){
	clear_chara_int_ctl_list(&ci_clst->ci_item_head);
	return;
};
int count_chara_int_clist(struct chara_int_clist *ci_clst){
	return count_chara_int_ctl_list(&ci_clst->ci_item_head);
};

int read_chara_int_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int_clist *ci_clst){
	return read_chara_int_ctl_list(fp, buf, label, &ci_clst->ci_item_head);
};
int write_chara_int_clist(FILE *fp, int level, const char *label, 
                       struct chara_int_clist *ci_clst){
	return write_chara_int_ctl_list(fp, level, label, &ci_clst->ci_item_head);
};

void append_chara_int_clist(char *c_in, int i1_in,
                      struct chara_int_clist *ci_clst){
	append_chara_int_ctl_list(c_in, i1_in, &ci_clst->ci_item_head);
	return;
};
void del_chara_int_clist_by_index(int index, struct chara_int_clist *ci_clst){
	del_chara_int_ctl_list_by_index(index, &ci_clst->ci_item_head);
	return;
};
void update_chara_int_clist_by_index(int index, char *c_in, int i1_in,
			struct chara_int_clist *ci_clst){
	update_chara_int_ctl_list_by_index(index, c_in, i1_in,
			&ci_clst->ci_item_head);
	return;
};
void set_from_chara_int_clist_at_index(int index, struct chara_int_clist *ci_clst,
			char *c_out, int *i1_out){
	set_from_chara_int_ctl_list_at_index(index, &ci_clst->ci_item_head,
			c_out, i1_out);
	return;
};

void del_chara_int_clist_by_c_tbl(char *ref, struct chara_int_clist *ci_clst){
	del_chara_int_ctl_list_by_c_tbl(ref, &ci_clst->ci_item_head);
	return;
};
void update_chara_int_clist_by_c_tbl(char *ref, char *c_in, int i1_in,
			struct chara_int_clist *ci_clst){
	update_chara_int_ctl_list_by_c_tbl(ref, c_in, i1_in,
			&ci_clst->ci_item_head);
	return;
};
void set_from_chara_int_clist_at_c_tbl(char *ref, struct chara_int_clist *ci_clst,
			char *c_out, int *i1_out){
	set_from_chara_int_ctl_list_at_c_tbl(ref, &ci_clst->ci_item_head,
			c_out, i1_out);
	return;
};

