/*
//  t_control_c_lists.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_c_lists.h"


static void init_void_ctl_list(struct void_ctl_list *head){
    head->void_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct void_ctl_list *add_void_ctl_list_before(void *void_in, struct void_ctl_list *current){
    struct void_ctl_list *added;
    struct void_ctl_list *old_prev;
    
    if ((added = (struct void_ctl_list *) malloc(sizeof(struct void_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->void_item = void_in;
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct void_ctl_list *add_void_ctl_list_after(void *void_in, struct void_ctl_list *current){
    struct void_ctl_list *added;
    struct void_ctl_list *old_next;
    
    if ((added = (struct void_ctl_list *) malloc(sizeof(struct void_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->void_item = void_in;
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_void_ctl_list(struct void_ctl_list *current){
    struct void_ctl_list *old_prev = current->_prev;
    struct void_ctl_list *old_next = current->_next;
    
    current->void_item = NULL;
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static void clear_void_ctl_list(struct void_ctl_list *head){
    while (head->_next != NULL) {
        delete_void_ctl_list(head->_next);
	}
    return;
};


static int count_void_ctl_list(struct void_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct void_ctl_list *find_v_ctl_list_item_by_index(int index, struct void_ctl_list *head){
    int i;
    if(index < 0 || index > count_void_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};

static void append_void_ctl_list(void *void_in, struct void_ctl_list *head){
    int num = count_void_ctl_list(head);
    if(num > 0) head = find_v_ctl_list_item_by_index(num-1, head);
	head = add_void_ctl_list_after(void_in, head);
    return;
};

static void add_void_ctl_list_before_index(int index, void *void_in, struct void_ctl_list *head){
	head = find_v_ctl_list_item_by_index(index, head);
	head = add_void_ctl_list_before(void_in, head);
	return;
};

static void add_void_ctl_list_after_index(int index, void *void_in, struct void_ctl_list *head){
	head = find_v_ctl_list_item_by_index(index, head);
	head = add_void_ctl_list_after(void_in, head);
	return;
};

static void del_void_ctl_list_by_index(int index, struct void_ctl_list *head){
	head = find_v_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_void_ctl_list(head);
	return;
};




struct void_clist * init_void_clist(void){
    struct void_clist *v_clist;
    if((v_clist = (struct void_clist *) malloc(sizeof(struct void_clist))) == NULL) {
        printf("malloc error for void_clist\n");
        exit(0);
    }
    init_void_ctl_list(&v_clist->c_item_head);

    v_clist->iflag_use = 0;
    v_clist->clist_name = (char *)calloc(32,sizeof(char));
    return v_clist;
};
void dealloc_void_clist(struct void_clist *v_clist){
    clear_void_ctl_list(&v_clist->c_item_head);
    
    free(v_clist->clist_name);

    free(v_clist);
    return;
};

int count_void_clist(struct void_clist *v_clist){
    return count_void_ctl_list(&v_clist->c_item_head);
};

void append_void_clist(void *void_in, struct void_clist *v_clist){
    append_void_ctl_list(void_in, &v_clist->c_item_head);
    return;
};
void add_void_clist_before_index(int index, void *void_in, struct void_clist *v_clist){
    add_void_ctl_list_before_index(index, void_in, &v_clist->c_item_head);
    return;
};
void add_void_clist_after_index(int index, void *void_in, struct void_clist *v_clist){
    add_void_ctl_list_after_index(index, void_in, &v_clist->c_item_head);
    return;
};
void del_void_clist_by_index(int index, struct void_clist *v_clist){
    del_void_ctl_list_by_index(index, &v_clist->c_item_head);
    return;
};

void * void_clist_at_index(int index, struct void_clist *v_clist){
    struct void_ctl_list *tmp_list = find_v_ctl_list_item_by_index(index, &v_clist->c_item_head);
    return tmp_list->void_item;
}
