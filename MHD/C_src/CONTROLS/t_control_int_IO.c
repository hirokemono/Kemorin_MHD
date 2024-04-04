/*
//  t_control_int_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_int_IO.h"


struct int_ctl_item * init_int_ctl_item_c(void){
    struct int_ctl_item *i_item;
    if((i_item = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item))) == NULL) {
        printf("malloc error for int_ctl_item \n");
        exit(0);
    }
	if((i_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for i_item->f_iflag\n");
		exit(0);
	}
	
    i_item->i_data = 0;
    return i_item;
};

void dealloc_int_ctl_item_c(struct int_ctl_item *i_item)
{
	if(i_item->c_block_name !=NULL) free(i_item->c_block_name);
    i_item->f_iflag = NULL;
	i_item->f_self = NULL;
	free(i_item);
	return;
}

int read_integer_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct int_ctl_item *i_item){
	char header_chara[KCHARA_C];
	
	if(i_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %d", header_chara, &i_item->i_data);
		i_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_integer_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct int_ctl_item *i_item){
    
	if(i_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp,  "%d\n", i_item->i_data);
    return level;
};

void update_int_ctl_item_c(int index, struct int_ctl_item *i_item){
	i_item->f_iflag[0] = 1;
	i_item->i_data = index;
	return;
};
int set_from_int_ctl_item_c(struct int_ctl_item *i_item){
	if(i_item->f_iflag[0] == 0) return 0.0;
	return i_item->i_data;
};


static void init_int_ctl_list(struct int_ctl_list *head){
    head->i_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct int_ctl_list *add_int_ctl_list_before(struct int_ctl_list *current){
    struct int_ctl_list *added;
    struct int_ctl_list *old_prev;
    
    if ((added = (struct int_ctl_list *) malloc(sizeof(struct int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->i_item = init_int_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct int_ctl_list *add_int_ctl_list_after(struct int_ctl_list *current){
    struct int_ctl_list *added;
    struct int_ctl_list *old_next;
    
    if ((added = (struct int_ctl_list *) malloc(sizeof(struct int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->i_item = init_int_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_int_ctl_list(struct int_ctl_list *current){
    struct int_ctl_list *old_prev = current->_prev;
    struct int_ctl_list *old_next = current->_next;
    
    free(current->i_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_int_ctl_list(struct int_ctl_list *head){
    while (head->_next != NULL) {
        delete_int_ctl_list(head->_next);
	}
    return;
};


static int count_int_ctl_list(struct int_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct int_ctl_list *find_i_ctl_list_item_by_index(int index, struct int_ctl_list *head){
    int i;
    if(index < 0 || index > count_int_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct int_ctl_list *find_i_ctl_list_item_by_value(int i_ref,
			struct int_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->i_item->i_data == i_ref) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %d does not exist.\n", i_ref);
    return head;
};

static int read_int_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->i_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_int_ctl_list_after(head);
        iflag = read_integer_ctl_item_c(buf, label, head->i_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_int_ctl_list(FILE *fp, int level, const char *label, 
                       struct int_ctl_list *head){
    if(count_int_ctl_list(head) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_integer_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->i_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


void append_int_ctl_list(int i1_in, struct int_ctl_list *head){
	int num = count_int_ctl_list(head);
    if(num > 0) head = find_i_ctl_list_item_by_index(num-1, head);
    head = add_int_ctl_list_after(head);
	update_int_ctl_item_c(i1_in, head->i_item);
    return;
};

void del_int_ctl_list_by_index(int index, struct int_ctl_list *head){
	head = find_i_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_int_ctl_list(head);
	return;
};

void update_int_ctl_list_by_index(int index, int i1_in, struct int_ctl_list *head){
	head = find_i_ctl_list_item_by_index(index, head);
	if(head != NULL) update_int_ctl_item_c(i1_in, head->i_item);
	return;
};

void set_from_int_ctl_list_at_index(int index, struct int_ctl_list *head, int *i1_out){
	head = find_i_ctl_list_item_by_index(index, head);
	if(head != NULL) *i1_out = set_from_int_ctl_item_c(head->i_item);
	return;
};


static void add_int_ctl_list_before_c_tbl(int i_ref,
			int i1_in, struct int_ctl_list *head){
	head = find_i_ctl_list_item_by_value(i_ref, head);
	if(head == NULL) return;
	head = add_int_ctl_list_before(head);
	update_int_ctl_item_c(i1_in, head->i_item);
	return;
};
static void add_int_ctl_list_after_c_tbl(int i_ref,
			int i1_in, struct int_ctl_list *head){
	head = find_i_ctl_list_item_by_value(i_ref, head);
	if(head == NULL) return;
	head = add_int_ctl_list_after(head);
	update_int_ctl_item_c(i1_in, head->i_item);
	return;
};
void del_int_ctl_list_by_c_tbl(int i_ref, struct int_ctl_list *head){
	head = find_i_ctl_list_item_by_value(i_ref, head);
	if(head != NULL) delete_int_ctl_list(head);
	return;
};

static void update_int_ctl_list_by_c_tbl(int i_ref,
			int i1_in, struct int_ctl_list *head){
	head = find_i_ctl_list_item_by_value(i_ref, head);
	if(head != NULL) update_int_ctl_item_c(i1_in, head->i_item);
	return;
};

static void set_from_int_ctl_list_at_c_tbl(int i_ref, struct int_ctl_list *head,
			int *i1_out){
	head = find_i_ctl_list_item_by_value(i_ref, head);
	if(head != NULL) *i1_out = set_from_int_ctl_item_c(head->i_item);
	return;
};

static void copy_from_int_ctl_list(struct int_ctl_list *head, int num, int *iv1){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        iv1[i] = set_from_int_ctl_item_c(head->i_item);
	};
	return;
};
static void copy_to_int_ctl_list(int num, int *iv1, struct int_ctl_list *head){
	int i;
	
	for(i=0;i<num;i++){
		head = add_int_ctl_list_after(head);
		update_int_ctl_item_c(iv1[i], head->i_item);
	};
	return;
};



struct int_clist * init_int_clist(void){
    struct int_clist *i_clst;
    if((i_clst = (struct int_clist *) malloc(sizeof(struct int_clist))) == NULL) {
        printf("malloc error for int_clist \n");
        exit(0);
    }
    
    init_int_ctl_list(&i_clst->i_item_head);
    
    i_clst->clist_name = (char *)calloc(32,sizeof(char));
    i_clst->i1_name = (char *)calloc(32,sizeof(char));
    return i_clst;
};

void dealloc_int_clist(struct int_clist *i_clst){
    clear_int_ctl_list(&i_clst->i_item_head);

    free(i_clst->clist_name);
    free(i_clst->i1_name);

    free(i_clst);
	return;
};
int count_int_clist(struct int_clist *i_clst){
    return count_int_ctl_list(&i_clst->i_item_head);
};

int read_int_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_clist *i_clst){
    sprintf(i_clst->clist_name,"%s", label);
    return read_int_ctl_list(fp, buf, label, &i_clst->i_item_head);
};
int write_int_clist(FILE *fp, int level, const char *label, 
                       struct int_clist *i_clst){
    return write_int_ctl_list(fp, level, label, &i_clst->i_item_head);
};

void append_int_clist(int i1_in, struct int_clist *i_clst){
    append_int_ctl_list(i1_in, &i_clst->i_item_head);
    return;
};
void del_int_clist_by_index(int index, struct int_clist *i_clst){
    del_int_ctl_list_by_index(index, &i_clst->i_item_head);
    return;
};
void update_int_clist_by_index(int index, int i1_in, struct int_clist *i_clst){
    update_int_ctl_list_by_index(index, i1_in, &i_clst->i_item_head);
    return;
};
void set_from_int_clist_at_index(int index, struct int_clist *i_clst, int *i1_out){
    set_from_int_ctl_list_at_index(index, &i_clst->i_item_head, i1_out);
    return;
};

struct int_ctl_item *int_clist_at_index(int index, struct int_clist *i_clst){
    struct int_ctl_list *ct_tmp = find_i_ctl_list_item_by_index(index, &i_clst->i_item_head);
    return ct_tmp->i_item;
}

void add_int_clist_before_c_tbl(int i_ref, int i1_in, struct int_clist *i_clst){
    add_int_ctl_list_before_c_tbl(i_ref, i1_in, &i_clst->i_item_head);
    return;
};
void add_int_clist_after_c_tbl(int i_ref, int i1_in, struct int_clist *i_clst){
    add_int_ctl_list_after_c_tbl(i_ref, i1_in, &i_clst->i_item_head);
    return;
};
void del_int_clist_by_c_tbl(int i_ref, struct int_clist *i_clst){
    del_int_ctl_list_by_c_tbl(i_ref, &i_clst->i_item_head);
    return;
};
void update_int_clist_by_c_tbl(int i_ref, int i1_in, struct int_clist *i_clst){
    update_int_ctl_list_by_c_tbl(i_ref, i1_in, &i_clst->i_item_head);
    return;
};
void set_from_int_clist_at_c_tbl(int i_ref, struct int_clist *i_clst, int *i1_out){
    set_from_int_ctl_list_at_c_tbl(i_ref, &i_clst->i_item_head, i1_out);
    return;
};

void copy_from_int_clist(struct int_clist *i_clst, int num, int *iv1){
    copy_from_int_ctl_list(&i_clst->i_item_head, num, iv1);
    return;
};
void copy_to_int_clist(int num, int *iv1, struct int_clist *i_clst){
    copy_to_int_ctl_list(num, iv1, &i_clst->i_item_head);
    return;
};

