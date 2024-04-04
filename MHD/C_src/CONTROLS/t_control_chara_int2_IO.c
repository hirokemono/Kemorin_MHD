/*
//  t_control_chara_int2_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_int2_IO.h"


struct chara_int2_ctl_item * init_chara_int2_ctl_item_c(void){
    struct chara_int2_ctl_item *ci2_item;
    if((ci2_item = (struct chara_int2_ctl_item *) malloc(sizeof(struct chara_int2_ctl_item))) == NULL){
        printf("malloc error for chara_int2_ctl_item\n");
        exit(0);
    }
	ci2_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	ci2_item->i_data[0] = 0;
	ci2_item->i_data[1] = 0;
	ci2_item->iflag = 0;
    return ci2_item;
};

void dealloc_chara_int2_ctl_item_c(struct chara_int2_ctl_item *ci2_item){
    free(ci2_item->c_tbl);
    free(ci2_item);
    return;
};


void update_chara_int2_ctl_item_c(char *c_in, int i1_in, int i2_in,
                                  struct chara_int2_ctl_item *ci2_item){
	ci2_item->iflag = 1;
	sprintf(ci2_item->c_tbl,"%s", c_in);
	ci2_item->i_data[0] = i1_in;
	ci2_item->i_data[1] = i2_in;
    return;
};


static void init_chara_int2_ctl_list(struct chara_int2_ctl_list *head){
    head->ci2_item = NULL;
    head->_next = NULL;
    return;
};

static struct chara_int2_ctl_list *add_chara_int2_ctl_list_before(struct chara_int2_ctl_item *ci2_input,
                                                                  struct chara_int2_ctl_list *current){
    struct chara_int2_ctl_list *added;
    struct chara_int2_ctl_list *old_prev;
    
    if ((added = (struct chara_int2_ctl_list *) malloc(sizeof(struct chara_int2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ci2_item = ci2_input;
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct chara_int2_ctl_list *add_chara_int2_ctl_list_after(struct chara_int2_ctl_item *ci2_input,
                                                          struct chara_int2_ctl_list *current){
    struct chara_int2_ctl_list *added;
    struct chara_int2_ctl_list *old_next;
    
    if ((added = (struct chara_int2_ctl_list *) malloc(sizeof(struct chara_int2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->ci2_item = ci2_input;
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_chara_int2_ctl_list(struct chara_int2_ctl_list *current){
    struct chara_int2_ctl_list *old_prev = current->_prev;
    struct chara_int2_ctl_list *old_next = current->_next;
    
    dealloc_chara_int2_ctl_item_c(current->ci2_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static void clear_chara_int2_ctl_list(struct chara_int2_ctl_list *head){
    while (head->_next != NULL) {
        delete_chara_int2_ctl_list(head->_next);
	}
    return;
};


static int count_maxlen_chara_int2_ctl_list(const char *label,
			struct chara_int2_ctl_list *head, int mlen2[2]){
	int num = 0;
	mlen2[0] = (int) strlen(label);
	mlen2[1] = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->ci2_item->c_tbl) > mlen2[1]){
            mlen2[1] = (int) strlen(head->ci2_item->c_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

int count_maxlen_chara_int2_clist(const char *label, struct chara_int2_clist *ci2_clst,
                                  int mlen2[2]){
    return count_maxlen_chara_int2_ctl_list(label, &ci2_clst->ci2_item_head, mlen2);
}

static int count_chara_int2_ctl_list(struct chara_int2_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara_int2_ctl_list *find_ci2_ctl_list_item_by_index(int index, struct chara_int2_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara_int2_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct chara_int2_ctl_list *find_ci2_ctl_list_item_by_c_tbl(char *ref, struct chara_int2_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->ci2_item->c_tbl, ref)) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %s does not exist.\n", ref);
    return head;
};
int find_ci2_ctl_list_index_by_c_tbl(char *ref, struct chara_int2_ctl_list *head){
    int idx = 0;
    head = head->_next;
    while (head != NULL){
        if(cmp_no_case_c(head->ci2_item->c_tbl, ref)) break;
        head = head->_next;
        idx = idx + 1;
    };
    if(head == NULL){
        printf("array item %s does not exist.\n", ref);
        idx = -1;
    };
    return idx;
};


static void append_chara_int2_ctl_list(struct chara_int2_ctl_item *ci2_input,
                                       struct chara_int2_ctl_list *head){
	int num = count_chara_int2_ctl_list(head);
    if(num > 0) head = find_ci2_ctl_list_item_by_index(num-1, head);
	head = add_chara_int2_ctl_list_after(ci2_input, head);
    return;
};

static void del_chara_int2_ctl_list_by_index(int index, struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_chara_int2_ctl_list(head);
	return;
};

static void update_chara_int2_ctl_list_by_index(int index, struct chara_int2_ctl_item *ci2_in,
			struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_index(index, head);
	if(head != NULL) update_chara_int2_ctl_item_c(ci2_in->c_tbl, ci2_in->i_data[0], ci2_in->i_data[1],
                                                  head->ci2_item);
	return;
};

static void add_chara_int2_ctl_list_before_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_input,
                                                 struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_int2_ctl_list_before(ci2_input, head);
	return;
};
static void add_chara_int2_ctl_list_after_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_input,
                                                struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_int2_ctl_list_after(ci2_input, head);
	return;
};
static void del_chara_int2_ctl_list_by_c_tbl(char *ref, struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_chara_int2_ctl_list(head);
	return;
};

static void update_chara_int2_ctl_list_by_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_in,
			struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) update_chara_int2_ctl_item_c(ci2_in->c_tbl, ci2_in->i_data[0], ci2_in->i_data[1],
                                                  head->ci2_item);
	return;
};

struct chara_int2_clist * init_chara_int2_clist(void){
    struct chara_int2_clist *ci2_clst;
    if((ci2_clst = (struct chara_int2_clist *) malloc(sizeof(struct chara_int2_clist))) == NULL){
        printf("malloc error for chara_int2_clist\n");
        exit(0);
    }
    init_chara_int2_ctl_list(&ci2_clst->ci2_item_head);

    ci2_clst->clist_name = (char *)calloc(32,sizeof(char));
    ci2_clst->c1_name = (char *)calloc(32,sizeof(char));
    ci2_clst->i1_name = (char *)calloc(32,sizeof(char));
    ci2_clst->i2_name = (char *)calloc(32,sizeof(char));
    return ci2_clst;
};
void dealloc_chara_int2_clist(struct chara_int2_clist *ci2_clst){
    clear_chara_int2_ctl_list(&ci2_clst->ci2_item_head);
    
    free(ci2_clst->clist_name);
    free(ci2_clst->c1_name);
    free(ci2_clst->i1_name);
    free(ci2_clst->i2_name);

    free(ci2_clst);
	return;
};


int count_chara_int2_clist(struct chara_int2_clist *ci2_clst){
    return count_chara_int2_ctl_list(&ci2_clst->ci2_item_head);
};

void append_chara_int2_clist(struct chara_int2_ctl_item *ci2_input,
                             struct chara_int2_clist *ci2_clst){
    append_chara_int2_ctl_list(ci2_input, &ci2_clst->ci2_item_head);
    return;
};
void del_chara_int2_clist_by_index(int index, struct chara_int2_clist *ci2_clst){
    del_chara_int2_ctl_list_by_index(index, &ci2_clst->ci2_item_head);
    return;
};
void update_chara_int2_clist_by_index(int index, struct chara_int2_ctl_item *ci2_in,
                                      struct chara_int2_clist *ci2_clst){
    update_chara_int2_ctl_list_by_index(index, ci2_in, &ci2_clst->ci2_item_head);
    return;
};

struct chara_int2_ctl_item *find_chara_int2_ctl_item_by_index(int index,
            struct chara_int2_clist *ci2_clst){
	struct chara_int2_ctl_list *head
            = find_ci2_ctl_list_item_by_index(index, &ci2_clst->ci2_item_head);
    return head->ci2_item;
};
struct chara_int2_ctl_item *find_chara_int2_ctl_item_by_cref(char *ref,
            struct chara_int2_clist *ci2_clst){
	struct chara_int2_ctl_list *head
            = find_ci2_ctl_list_item_by_c_tbl(ref, &ci2_clst->ci2_item_head);
    return head->ci2_item;
};
int find_chara_int2_ctl_index_by_cref(char *ref, struct chara_int2_clist *ci2_clst){
    return find_ci2_ctl_list_index_by_c_tbl(ref, &ci2_clst->ci2_item_head);
};


void add_chara_int2_clist_before_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_input,
                                       struct chara_int2_clist *ci2_clst){
    add_chara_int2_ctl_list_before_c_tbl(ref, ci2_input, &ci2_clst->ci2_item_head);
    return;
};
void add_chara_int2_clist_after_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_input,
                                      struct chara_int2_clist *ci2_clst){
    add_chara_int2_ctl_list_after_c_tbl(ref, ci2_input, &ci2_clst->ci2_item_head);
    return;
};
void del_chara_int2_clist_by_c_tbl(char *ref, struct chara_int2_clist *ci2_clst){
    del_chara_int2_ctl_list_by_c_tbl(ref, &ci2_clst->ci2_item_head);
    return;
};
void update_chara_int2_clist_by_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_in,
                                      struct chara_int2_clist *ci2_clst){
    update_chara_int2_ctl_list_by_c_tbl(ref, ci2_in, &ci2_clst->ci2_item_head);
    return;
};
