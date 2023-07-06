/*
//  t_control_c_lists_w_file.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_c_lists_w_file.h"


static void init_void_file_ctl_list(struct void_file_ctl_list *head){
    head->void_item =  NULL;
    head->file_name =  alloc_string(KCHARA_C);
    head->list_label = alloc_string(KCHARA_C);
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct void_file_ctl_list *add_void_file_ctl_list_before(char *clist_name, int *icount, char *fname_in, 
														   void *void_in, struct void_file_ctl_list *current){
    struct void_file_ctl_list *added;
    struct void_file_ctl_list *old_prev;
    
    if ((added = (struct void_file_ctl_list *) malloc(sizeof(struct void_file_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
	}
	init_void_file_ctl_list(added);
	added->void_item = void_in;
	sprintf(added->file_name,"%s", fname_in);
	icount[0] = icount[0] +1;
	sprintf(added->list_label,"%s_%d", clist_name, icount[0]);
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct void_file_ctl_list *add_void_file_ctl_list_after(char *clist_name, int *icount, char *fname_in, 
														  void *void_in, struct void_file_ctl_list *current){
    struct void_file_ctl_list *added;
    struct void_file_ctl_list *old_next;
    
    if ((added = (struct void_file_ctl_list *) malloc(sizeof(struct void_file_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	init_void_file_ctl_list(added);
	added->void_item = void_in;
	sprintf(added->file_name,"%s", fname_in);
	icount[0] = icount[0] + 1;
	sprintf(added->list_label,"%s_%d", clist_name, icount[0]);
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_void_file_ctl_list(struct void_file_ctl_list *current){
    struct void_file_ctl_list *old_prev = current->_prev;
    struct void_file_ctl_list *old_next = current->_next;
    
    current->void_item = NULL;
    free(current->file_name);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static void clear_void_file_ctl_list(struct void_file_ctl_list *head){
    while (head->_next != NULL) {
        delete_void_file_ctl_list(head->_next);
	}
    return;
};


static int count_void_file_ctl_list(struct void_file_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct void_file_ctl_list *find_v_file_ctl_list_item_by_index(int index, struct void_file_ctl_list *head){
    int i;
    if(index < 0 || index > count_void_file_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct void_file_ctl_list *find_v_file_ctl_list_item_by_c_tbl(char *ref, struct void_file_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->list_label, ref)) return head;
        head = head->_next;
    };
    return head;
};

static int find_v_file_ctl_index_by_c_tbl(char *ref, struct void_file_ctl_list *head){
    int icou = 0;
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->list_label, ref)) return icou;
        head = head->_next;
		icou = icou + 1;
    };
    return icou;
};

static void append_void_file_ctl_list(char *clist_name, int *icount, char *fname_in, 
									  void *void_in, struct void_file_ctl_list *head){
    int num = count_void_file_ctl_list(head);
    if(num > 0) head = find_v_file_ctl_list_item_by_index(num-1, head);
	head = add_void_file_ctl_list_after(clist_name, icount, fname_in, void_in, head);
    return;
};
static void add_void_file_ctl_list_before_index(int index, char *clist_name, int *icount, 
										   char *fname_in, void *void_in, 
										   struct void_file_ctl_list *head){
	head = find_v_file_ctl_list_item_by_index(index, head);
	head = add_void_file_ctl_list_before(clist_name, icount, fname_in, void_in, head);
	return;
};
static void add_void_file_ctl_list_after_index(int index, char *clist_name, int *icount, 
											   char *fname_in, void *void_in, 
											   struct void_file_ctl_list *head){
	head = find_v_file_ctl_list_item_by_index(index, head);
	head = add_void_file_ctl_list_after(clist_name, icount, fname_in, void_in, head);
	return;
};
static void del_void_file_ctl_list_by_index(int index, struct void_file_ctl_list *head){
	head = find_v_file_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_void_file_ctl_list(head);
	return;
};

static void add_void_file_ctl_list_before_c_tbl(char *ref, char *clist_name, int *icount, 
												char *fname_in, void *void_in, 
												struct void_file_ctl_list *head){
	head = find_v_file_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_void_file_ctl_list_before(clist_name, icount, fname_in, void_in, head);
	return;
};
static void add_void_file_ctl_list_after_c_tbl(char *ref, char *clist_name, int *icount, 
											   char *fname_in, void *void_in, 
											   struct void_file_ctl_list *head){
	head = find_v_file_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_void_file_ctl_list_after(clist_name, icount, fname_in, void_in, head);
	return;
};

static void del_void_file_ctl_list_by_c_tbl(char *ref, struct void_file_ctl_list *head){
	head = find_v_file_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_void_file_ctl_list(head);
	return;
};


struct void_file_clist * init_void_file_clist(char *label){
    struct void_file_clist *vf_clist;
    if((vf_clist = (struct void_file_clist *) malloc(sizeof(struct void_file_clist))) == NULL) {
        printf("malloc error for void_file_clist\n");
        exit(0);
    }
    init_void_file_ctl_list(&vf_clist->c_item_head);
	
    vf_clist->iflag_use = 0;
	vf_clist->icount = 0;
	
	vf_clist->clist_name = alloc_string(KCHARA_C);
	sprintf(vf_clist->clist_name, "%s", label);
    return vf_clist;
};
void dealloc_void_file_clist(struct void_file_clist *vf_clist){
    clear_void_file_ctl_list(&vf_clist->c_item_head);
    
    free(vf_clist->clist_name);

    free(vf_clist);
    return;
};

int count_void_file_clist(struct void_file_clist *vf_clist){
    return count_void_file_ctl_list(&vf_clist->c_item_head);
};

void append_void_file_clist(char *fname_in, void *void_in,
							struct void_file_clist *vf_clist){
	append_void_file_ctl_list(vf_clist->clist_name, &vf_clist->icount, 
							  fname_in, void_in, &vf_clist->c_item_head);
    return;
};

void add_void_file_clist_before_index(int index, char *fname_in, void *void_in,
									  struct void_file_clist *vf_clist){
	add_void_file_ctl_list_before_index(index, vf_clist->clist_name, &vf_clist->icount, 
										fname_in, void_in, &vf_clist->c_item_head);
    return;
};
void add_void_file_clist_after_index(int index, char *fname_in, void *void_in, 
									 struct void_file_clist *vf_clist){
	add_void_file_ctl_list_after_index(index, vf_clist->clist_name, &vf_clist->icount, 
									   fname_in, void_in, &vf_clist->c_item_head);
    return;
};
void del_void_file_clist_by_index(int index, struct void_file_clist *vf_clist){
    del_void_file_ctl_list_by_index(index, &vf_clist->c_item_head);
    return;
};


void add_void_file_clist_before_c_tbl(char *ref, char *fname_in, void *void_in, 
									  struct void_file_clist *vf_clist){
    add_void_file_ctl_list_before_c_tbl(ref, vf_clist->clist_name, &vf_clist->icount, 
										fname_in, void_in, &vf_clist->c_item_head);
    return;
};
void add_void_file_clist_after_c_tbl(char *ref, char *fname_in, void *void_in,
									 struct void_file_clist *vf_clist){
    add_void_file_ctl_list_after_c_tbl(ref, vf_clist->clist_name, &vf_clist->icount, 
									   fname_in, void_in, &vf_clist->c_item_head);
    return;
};
void del_void_file_clist_by_c_tbl(char *ref, struct void_file_clist *vf_clist){
    del_void_file_ctl_list_by_c_tbl(ref, &vf_clist->c_item_head);
    return;
};


int find_void_file_clist_index_by_c_tbl(char *ref, struct void_file_clist *vf_clist){
    return find_v_file_ctl_index_by_c_tbl(ref, &vf_clist->c_item_head);
};


void replace_void_file_clist_at_index(int index, char *fname_in, void *void_in, 
									  struct void_file_clist *vf_clist){
	struct void_file_ctl_list *tmp_list 
			= find_v_file_ctl_list_item_by_index(index, &vf_clist->c_item_head);
	tmp_list->void_item = void_in;
	sprintf(tmp_list->file_name,"%s", fname_in);
    return;
}

void * void_file_clist_at_index(int index, struct void_file_clist *vf_clist){
	struct void_file_ctl_list *tmp_list 
			= find_v_file_ctl_list_item_by_index(index, &vf_clist->c_item_head);
    return tmp_list->void_item;
}

char * void_file_name_clist_at_index(int index, struct void_file_clist *vf_clist){
	struct void_file_ctl_list *tmp_list 
			= find_v_file_ctl_list_item_by_index(index, &vf_clist->c_item_head);
    return tmp_list->file_name;
}

char * void_file_clist_label_at_index(int index, struct void_file_clist *vf_clist){
    struct void_file_ctl_list *tmp_list = find_v_file_ctl_list_item_by_index(index, &vf_clist->c_item_head);
    return tmp_list->list_label;
}
