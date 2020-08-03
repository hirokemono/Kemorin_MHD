/*
//  t_control_chara_int3_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_int3_IO.h"


struct chara_int3_ctl_item * init_chara_int3_ctl_item_c(void){
    struct chara_int3_ctl_item *ci3_item;
    if ((ci3_item = (struct chara_int3_ctl_item *) malloc(sizeof(struct chara_int3_ctl_item))) == NULL) {
        printf("malloc error for chara_int3_ctl_item\n");
        exit(0);
    }
	ci3_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	ci3_item->i_data[0] = 0;
	ci3_item->i_data[1] = 0;
	ci3_item->i_data[2] = 0;
	ci3_item->iflag = 0;
    return ci3_item;
};

void dealloc_chara_int3_ctl_item_c(struct chara_int3_ctl_item *ci3_item){
    free(ci3_item->c_tbl);
    free(ci3_item);
    return;
};

int read_chara_int3_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                              struct chara_int3_ctl_item *ci3_item){
	char header_chara[KCHARA_C];
	
	if(ci3_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %d %d %d", header_chara, ci3_item->c_tbl, 
					&ci3_item->i_data[0], &ci3_item->i_data[1], &ci3_item->i_data[2]);
		strip_cautation_marks(ci3_item->c_tbl);
		ci3_item->iflag = 1;
	};
	return 1;
};

int write_chara_int3_ctl_item_c(FILE *fp, int level, int maxlen[2], 
                           const char *label, struct chara_int3_ctl_item *ci3_item){
    
	if(ci3_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], ci3_item->c_tbl);
	fprintf(fp, "%d %d %d\n", ci3_item->i_data[0], ci3_item->i_data[1], ci3_item->i_data[2]);
    return level;
};

void update_chara_int3_ctl_item_c(char *c_in, int i1_in, int i2_in, int i3_in,
                              struct chara_int3_ctl_item *ci3_item){
	ci3_item->iflag = 1;
	sprintf(ci3_item->c_tbl,"%s", c_in);
	ci3_item->i_data[0] = i1_in;
	ci3_item->i_data[1] = i2_in;
	ci3_item->i_data[2] = i3_in;
    return;
};
void set_from_chara_int3_ctl_item_c( struct chara_int3_ctl_item *ci3_item,
                              char *c_out, int *i1_out, int *i2_out, int *i3_out){
	if(ci3_item->iflag == 0) return;
	sprintf(c_out,"%s", ci3_item->c_tbl);
	*i1_out = ci3_item->i_data[0];
	*i2_out = ci3_item->i_data[1];
	*i3_out = ci3_item->i_data[2];
    return;
};


void init_chara_int3_ctl_list(struct chara_int3_ctl_list *head){
    head->ci3_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

struct chara_int3_ctl_list *add_chara_int3_ctl_list_before(struct chara_int3_ctl_list *current){
    struct chara_int3_ctl_list *added;
    struct chara_int3_ctl_list *old_prev;
    
    if ((added = (struct chara_int3_ctl_list *) malloc(sizeof(struct chara_int3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ci3_item = init_chara_int3_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

struct chara_int3_ctl_list *add_chara_int3_ctl_list_after(struct chara_int3_ctl_list *current){
    struct chara_int3_ctl_list *added;
    struct chara_int3_ctl_list *old_next;
    
    if ((added = (struct chara_int3_ctl_list *) malloc(sizeof(struct chara_int3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ci3_item = init_chara_int3_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_chara_int3_ctl_list(struct chara_int3_ctl_list *current){
    struct chara_int3_ctl_list *old_prev = current->_prev;
    struct chara_int3_ctl_list *old_next = current->_next;
    
    dealloc_chara_int3_ctl_item_c(current->ci3_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
void clear_chara_int3_ctl_list(struct chara_int3_ctl_list *head){
    while (head->_next != NULL) {
        delete_chara_int3_ctl_list(head->_next);
	}
    return;
};


int count_maxlen_chara_int3_ctl_list(const char *label, 
			struct chara_int3_ctl_list *head, int mlen2[2]){
	int num = 0;
	mlen2[0] = (int) strlen(label);
	mlen2[1] = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->ci3_item->c_tbl) > mlen2[1]){
            mlen2[1] = (int) strlen(head->ci3_item->c_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

int count_chara_int3_ctl_list(struct chara_int3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara_int3_ctl_list *find_ci3_ctl_list_item_by_index(int index, struct chara_int3_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara_int3_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
struct chara_int3_ctl_list *find_ci3_ctl_list_item_by_c_tbl(char *ref, struct chara_int3_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->ci3_item->c_tbl, ref)) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %s does not exist.\n", ref);
    return head;
};

int read_chara_int3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int3_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->ci3_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_chara_int3_ctl_list_after(head);
        iflag = read_chara_int3_ctl_item_c(buf, label, head->ci3_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    return icou;
};

int write_chara_int3_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_int3_ctl_list *head){
    int mlen2[2];
    
    if(count_maxlen_chara_int3_ctl_list(label, head, mlen2) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_int3_ctl_item_c(fp, level, mlen2,
                                     label, head->ci3_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_chara_int3_ctl_list(char *c_in, int i1_in, int i2_in, int i3_in, 
                      struct chara_int3_ctl_list *head){
	int num = count_chara_int3_ctl_list(head);
    if(num > 0) head = find_ci3_ctl_list_item_by_index(num-1, head);
	head = add_chara_int3_ctl_list_after(head);
	update_chara_int3_ctl_item_c(c_in, i1_in, i2_in, i3_in, head->ci3_item);
    return;
};

static void del_chara_int3_ctl_list_by_index(int index, struct chara_int3_ctl_list *head){
	head = find_ci3_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_chara_int3_ctl_list(head);
	return;
};

static void update_chara_int3_ctl_list_by_index(int index, char *c_in, int i1_in, int i2_in, int i3_in, 
			struct chara_int3_ctl_list *head){
	head = find_ci3_ctl_list_item_by_index(index, head);
	if(head != NULL) update_chara_int3_ctl_item_c(c_in, i1_in, i2_in, i3_in, head->ci3_item);
	return;
};

static void set_from_chara_int3_ctl_list_at_index(int index, struct chara_int3_ctl_list *head,
			char *c_out, int *i1_out, int *i2_out, int *i3_out){
	head = find_ci3_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_chara_int3_ctl_item_c(head->ci3_item, c_out, i1_out, i2_out, i3_out);
	return;
};


static void add_chara_int3_ctl_list_before_c_tbl(char *ref, char *c_in, int i1_in, int i2_in, int i3_in,
			struct chara_int3_ctl_list *head){
	head = find_ci3_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_int3_ctl_list_before(head);
	update_chara_int3_ctl_item_c(c_in, i1_in, i2_in, i3_in, head->ci3_item);
	return;
};
static void add_chara_int3_ctl_list_after_c_tbl(char *ref, char *c_in, int i1_in, int i2_in, int i3_in,
			struct chara_int3_ctl_list *head){
	head = find_ci3_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_int3_ctl_list_after(head);
	update_chara_int3_ctl_item_c(c_in, i1_in, i2_in, i3_in, head->ci3_item);
	return;
};
void del_chara_int3_ctl_list_by_c_tbl(char *ref, struct chara_int3_ctl_list *head){
	head = find_ci3_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_chara_int3_ctl_list(head);
	return;
};

static void update_chara_int3_ctl_list_by_c_tbl(char *ref, char *c_in, int i1_in, int i2_in, int i3_in, 
			struct chara_int3_ctl_list *head){
	head = find_ci3_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) update_chara_int3_ctl_item_c(c_in, i1_in, i2_in, i3_in, head->ci3_item);
	return;
};

static void set_from_chara_int3_ctl_list_at_c_tbl(char *ref, struct chara_int3_ctl_list *head,
			char *c_out, int *i1_out, int *i2_out, int *i3_out){
	head = find_ci3_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) set_from_chara_int3_ctl_item_c(head->ci3_item, c_out, i1_out, i2_out, i3_out);
	return;
};


struct chara_int3_clist * init_chara_int3_clist(void){
    struct chara_int3_clist *ci3_clst;
    if ((ci3_clst = (struct chara_int3_clist *) malloc(sizeof(struct chara_int3_clist))) == NULL) {
        printf("malloc error for chara_int3_clist\n");
        exit(0);
    }

    init_chara_int3_ctl_list(&ci3_clst->ci3_item_head);

    ci3_clst->clist_name = (char *)calloc(32,sizeof(char));
    ci3_clst->c1_name = (char *)calloc(32,sizeof(char));
    ci3_clst->i1_name = (char *)calloc(32,sizeof(char));
    ci3_clst->i2_name = (char *)calloc(32,sizeof(char));
    ci3_clst->i3_name = (char *)calloc(32,sizeof(char));
	return ci3_clst;
};

void dealloc_chara_int3_clist(struct chara_int3_clist *ci3_clst){
    clear_chara_int3_ctl_list(&ci3_clst->ci3_item_head);
    
    free(ci3_clst->clist_name);
    free(ci3_clst->c1_name);
    free(ci3_clst->i1_name);
    free(ci3_clst->i2_name);
    free(ci3_clst->i3_name);

    free(ci3_clst);
	return;
};
int count_chara_int3_clist(struct chara_int3_clist *ci3_clst){
    return count_chara_int3_ctl_list(&ci3_clst->ci3_item_head);
};

int read_chara_int3_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int3_clist *ci3_clst){
    sprintf(ci3_clst->clist_name,"%s", label);
    return read_chara_int3_ctl_list(fp, buf, label, &ci3_clst->ci3_item_head);
};
int write_chara_int3_clist(FILE *fp, int level, const char *label, 
                       struct chara_int3_clist *ci3_clst){
    return write_chara_int3_ctl_list(fp, level, label, &ci3_clst->ci3_item_head);
};

void append_chara_int3_clist(char *c_in, int i1_in, int i2_in, int i3_in,
                      struct chara_int3_clist *ci3_clst){
    append_chara_int3_ctl_list(c_in, i1_in, i2_in, i3_in, &ci3_clst->ci3_item_head);
    return;
};
void del_chara_int3_clist_by_index(int index, struct chara_int3_clist *ci3_clst){
    del_chara_int3_ctl_list_by_index(index, &ci3_clst->ci3_item_head);
    return;
};
void update_chara_int3_clist_by_index(int index, char *c_in, int i1_in, int i2_in, int i3_in, 
            struct chara_int3_clist *ci3_clst){
    update_chara_int3_ctl_list_by_index(index, c_in, i1_in, i2_in, i3_in, 
            &ci3_clst->ci3_item_head);
    return;
};
void set_from_chara_int3_clist_at_index(int index, struct chara_int3_clist *ci3_clst,
            char *c_out, int *i1_out, int *i2_out, int *i3_out){
    set_from_chara_int3_ctl_list_at_index(index, &ci3_clst->ci3_item_head,
            c_out, i1_out, i2_out, i3_out);
    return;
};

void add_chara_int3_clist_before_c_tbl(char *ref, char *c_in, int i1_in, int i2_in, int i3_in,
            struct chara_int3_clist *ci3_clst){
    add_chara_int3_ctl_list_before_c_tbl(ref, c_in, i1_in, i2_in, i3_in,
            &ci3_clst->ci3_item_head);
    return;
};
void add_chara_int3_clist_after_c_tbl(char *ref, char *c_in, int i1_in, int i2_in, int i3_in, 
            struct chara_int3_clist *ci3_clst){
    add_chara_int3_ctl_list_after_c_tbl(ref, c_in, i1_in, i2_in, i3_in, 
            &ci3_clst->ci3_item_head);
    return;
};
void del_chara_int3_clist_by_c_tbl(char *ref, struct chara_int3_clist *ci3_clst){
    del_chara_int3_ctl_list_by_c_tbl(ref, &ci3_clst->ci3_item_head);
    return;
};
void update_chara_int3_clist_by_c_tbl(char *ref, char *c_in, int i1_in, int i2_in, int i3_in, 
            struct chara_int3_clist *ci3_clst){
    update_chara_int3_ctl_list_by_c_tbl(ref, c_in, i1_in, i2_in, i3_in,
            &ci3_clst->ci3_item_head);
    return;
};
void set_from_chara_int3_clist_at_c_tbl(char *ref, struct chara_int3_clist *ci3_clst,
            char *c_out, int *i1_out, int *i2_out, int *i3_out){
    set_from_chara_int3_ctl_list_at_c_tbl(ref, &ci3_clst->ci3_item_head,
            c_out, i1_out, i2_out, i3_out);
    return;
};
