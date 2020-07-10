/*
//  t_control_chara_int_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_int_IO.h"


struct chara_int_ctl_item * init_chara_int_ctl_item_c(void){
    struct chara_int_ctl_item *ci_item;
    if((ci_item = (struct chara_int_ctl_item *) malloc(sizeof(struct chara_int_ctl_item))) == NULL) {
        printf("malloc error for ci_item\n");
        exit(0);
    }

    ci_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	ci_item->i_data = 0;
	ci_item->iflag = 0;
    return ci_item;
};

void dealloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item){
    free(ci_item->c_tbl);
    free(ci_item);
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

void update_chara_int_ctl_item_c(const char *c_in, const int i1_in,
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
    head->ci_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};


static struct chara_int_ctl_list *add_chara_int_ctl_list_before(struct chara_int_ctl_list *current){
    struct chara_int_ctl_list *added;
    struct chara_int_ctl_list *old_prev;
    
    if ((added = (struct chara_int_ctl_list *) malloc(sizeof(struct chara_int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ci_item = init_chara_int_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct chara_int_ctl_list *add_chara_int_ctl_list_after(struct chara_int_ctl_list *current){
    struct chara_int_ctl_list *added;
    struct chara_int_ctl_list *old_next;
    
    if ((added = (struct chara_int_ctl_list *) malloc(sizeof(struct chara_int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ci_item = init_chara_int_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
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
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_chara_int_ctl_list(struct chara_int_ctl_list *head){
    while (head->_next != NULL) {
        delete_chara_int_ctl_list(head->_next);
	}
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

static struct chara_int_ctl_list *find_ci_ctl_list_item_by_index(const int index, struct chara_int_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara_int_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
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
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->ci_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_chara_int_ctl_list_after(head);
        iflag = read_chara_int_ctl_item_c(buf, label, head->ci_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_chara_int_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_int_ctl_list *head){
    int mlen2[2];
    
    if(count_maxlen_chara_int_ctl_list(label, head, mlen2) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_int_ctl_item_c(fp, level, mlen2, label, head->ci_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_chara_int_ctl_list(const char *c_in, const int i1_in,
                      struct chara_int_ctl_list *head){
	int num = count_chara_int_ctl_list(head);
    if(num > 0) head = find_ci_ctl_list_item_by_index(num-1, head);
	head = add_chara_int_ctl_list_after(head);
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

static struct chara_int_ctl_item *chara_int_ctl_list_at_index(int index, struct chara_int_ctl_list *head){
    struct chara_int_ctl_list *tmp_list = find_ci_ctl_list_item_by_index(index, head);
    return tmp_list->ci_item;
};

static void add_chara_int_ctl_list_before_c_tbl(char *ref, char *c_in, int i1_in,
			struct chara_int_ctl_list *head){
	head = find_ci_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_int_ctl_list_before(head);
	update_chara_int_ctl_item_c(c_in, i1_in, head->ci_item);
	return;
};
static void add_chara_int_ctl_list_after_c_tbl(char *ref, char *c_in, int i1_in,
			struct chara_int_ctl_list *head){
	head = find_ci_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_int_ctl_list_after(head);
	update_chara_int_ctl_item_c(c_in, i1_in, head->ci_item);
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


struct chara_int_clist * init_chara_int_clist(void){
    struct chara_int_clist *ci_clst;
    if((ci_clst = (struct chara_int_clist *) malloc(sizeof(struct chara_int_clist))) == NULL){
        printf("malloc error for chara_int_clist\n");
        exit(0);
    }
    init_chara_int_ctl_list(&ci_clst->ci_item_head);
	
	ci_clst->iflag_use = 0;
    ci_clst->clist_name = (char *)calloc(32,sizeof(char));
    ci_clst->c1_name = (char *)calloc(32,sizeof(char));
    ci_clst->i1_name = (char *)calloc(32,sizeof(char));
    return ci_clst;
};

void dealloc_chara_int_clist(struct chara_int_clist *ci_clst){
    clear_chara_int_ctl_list(&ci_clst->ci_item_head);
	
	free(ci_clst->clist_name);
    free(ci_clst->c1_name);
    free(ci_clst->i1_name);

    free(ci_clst);
    return;
};

int count_chara_int_clist(struct chara_int_clist *ci_clst){
    return count_chara_int_ctl_list(&ci_clst->ci_item_head);
};

void read_chara_int_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int_clist *ci_clst){
    if(ci_clst->iflag_use > 0) return;
    
	sprintf(ci_clst->clist_name,"%s", label);
	ci_clst->iflag_use = read_chara_int_ctl_list(fp, buf, label, &ci_clst->ci_item_head);
    return;
};

int write_chara_int_clist(FILE *fp, int level, const char *label, 
                       struct chara_int_clist *ci_clst){
    if(ci_clst->iflag_use == 0) return level;
    
    return write_chara_int_ctl_list(fp, level, label, &ci_clst->ci_item_head);
};

void append_chara_int_clist(const char *c_in, const int i1_in,
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
struct chara_int_ctl_item *chara_int_clist_at_index(int index, struct chara_int_clist *ci_clst){
    return chara_int_ctl_list_at_index(index, &ci_clst->ci_item_head);
};

void add_chara_int_clist_before_c_tbl(char *ref, char *c_in, int i1_in,
            struct chara_int_clist *ci_clst){
    add_chara_int_ctl_list_before_c_tbl(ref, c_in, i1_in,
            &ci_clst->ci_item_head);
    return;
};
void add_chara_int_clist_after_c_tbl(char *ref, char *c_in, int i1_in,
            struct chara_int_clist *ci_clst){
    add_chara_int_ctl_list_after_c_tbl(ref, c_in, i1_in,
            &ci_clst->ci_item_head);
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
