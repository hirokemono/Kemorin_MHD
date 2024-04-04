/*
//  t_control_int2_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_int2_IO.h"


struct int2_ctl_item * init_int2_ctl_item_c(void){
    struct int2_ctl_item *i2_item;
    if((i2_item = (struct int2_ctl_item *) malloc(sizeof(struct int2_ctl_item))) == NULL) {
        printf("malloc error for int2_ctl_item \n");
        exit(0);
    }
	if((i2_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for r_item->f_iflag\n");
		exit(0);
	}
        
	i2_item->i_data[0] = 0;
	i2_item->i_data[1] = 0;
    return i2_item;
};

void dealloc_f_ctl_i2_item(struct int2_ctl_item *i2_item)
{
	if(i2_item->c_block_name !=NULL) free(i2_item->c_block_name);
    i2_item->f_iflag = NULL;
	i2_item->f_self = NULL;
	return;
}


int read_int2_ctl_item_c(char buf[LENGTHBUF], const char *label,
                          struct int2_ctl_item *i2_item){
	char header_chara[KCHARA_C];
	
	if(i2_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %d %d", header_chara, 
					&i2_item->i_data[0], &i2_item->i_data[1]);
		i2_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_int2_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct int2_ctl_item *i2_item){
    
	if(i2_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp, "%d  %d\n", i2_item->i_data[0], i2_item->i_data[1]);
    return level;
};


void update_int2_ctl_item_c(int i1_in, int i2_in,  
			struct int2_ctl_item *i2_item){
	i2_item->f_iflag[0] = 1;
	i2_item->i_data[0] = i1_in;
	i2_item->i_data[1] = i2_in;
    return;
};

void set_from_int2_ctl_item_c(struct int2_ctl_item *i2_item,
			int *i1_out, int *i2_out){
	if(i2_item->f_iflag[0] == 0) return;
	*i1_out = i2_item->i_data[0];
	*i2_out = i2_item->i_data[1];
    return;
};


static void init_int2_ctl_list(struct int2_ctl_list *head){
    head->i2_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

static struct int2_ctl_list *add_int2_ctl_list_before(struct int2_ctl_list *current){
    struct int2_ctl_list *added;
    struct int2_ctl_list *old_prev;
    
    if ((added = (struct int2_ctl_list *) malloc(sizeof(struct int2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->i2_item = init_int2_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct int2_ctl_list *add_int2_ctl_list_after(struct int2_ctl_list *current){
    struct int2_ctl_list *added;
    struct int2_ctl_list *old_next;
    
    if ((added = (struct int2_ctl_list *) malloc(sizeof(struct int2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->i2_item =init_int2_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_int2_ctl_list(struct int2_ctl_list *current){
    struct int2_ctl_list *old_prev = current->_prev;
    struct int2_ctl_list *old_next = current->_next;
    
    free(current->i2_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_int2_ctl_list(struct int2_ctl_list *head){
    while (head->_next != NULL) {
        delete_int2_ctl_list(head->_next);
	}
    return;
};


int count_int2_ctl_list(struct int2_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct int2_ctl_list *find_i2_ctl_list_item_by_index(int index, struct int2_ctl_list *head){
    int i;
    if(index < 0 || index > count_int2_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct int2_ctl_list *find_i2_ctl_list_item_by_value(int iref_1, int iref_2,
			struct int2_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->i2_item->i_data[0] == iref_1 && head->i2_item->i_data[1] == iref_2) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %d, %d does not exist.\n", iref_1, iref_2);
    return head;
};

static int read_int2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int2_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->i2_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_int2_ctl_list_after(head);
        iflag = read_int2_ctl_item_c(buf, label, head->i2_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_int2_ctl_list(FILE *fp, int level, const char *label, 
                       struct int2_ctl_list *head){
    if(count_int2_ctl_list(head) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_int2_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->i2_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

void append_int2_ctl_list(int i1_in, int i2_in, struct int2_ctl_list *head){
	int num = count_int2_ctl_list(head);
    if(num > 0) head = find_i2_ctl_list_item_by_index(num-1, head);
	head = add_int2_ctl_list_after(head);
	update_int2_ctl_item_c(i1_in, i2_in, head->i2_item);
    return;
};

void del_int2_ctl_list_by_index(int index, struct int2_ctl_list *head){
	head = find_i2_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_int2_ctl_list(head);
	return;
};

void update_int2_ctl_list_by_index(int index, int i1_in, int i2_in,
			struct int2_ctl_list *head){
	head = find_i2_ctl_list_item_by_index(index, head);
	if(head != NULL) update_int2_ctl_item_c(i1_in, i2_in, head->i2_item);
	return;
};

void set_from_int2_ctl_list_at_index(int index, struct int2_ctl_list *head,
			int *i1_out, int *i2_out){
	head = find_i2_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_int2_ctl_item_c(head->i2_item, i1_out, i2_out);
	return;
};


static void add_int2_ctl_list_before_c_tbl(int iref_1, int iref_2,
			int i1_in, int i2_in, struct int2_ctl_list *head){
	head = find_i2_ctl_list_item_by_value(iref_1, iref_2, head);
	if(head == NULL) return;
	head = add_int2_ctl_list_before(head);
	update_int2_ctl_item_c(i1_in, i2_in, head->i2_item);
	return;
};
static void add_int2_ctl_list_after_c_tbl(int iref_1, int iref_2,
			int i1_in, int i2_in, struct int2_ctl_list *head){
	head = find_i2_ctl_list_item_by_value(iref_1, iref_2, head);
	if(head == NULL) return;
	head = add_int2_ctl_list_after(head);
	update_int2_ctl_item_c(i1_in, i2_in, head->i2_item);
	return;
};
void del_int2_ctl_list_by_c_tbl(int iref_1, int iref_2, struct int2_ctl_list *head){
	head = find_i2_ctl_list_item_by_value(iref_1, iref_2, head);
	if(head != NULL) delete_int2_ctl_list(head);
	return;
};

static void update_int2_ctl_list_by_c_tbl(int iref_1, int iref_2,
			int i1_in, int i2_in, struct int2_ctl_list *head){
	head = find_i2_ctl_list_item_by_value(iref_1, iref_2, head);
	if(head != NULL) update_int2_ctl_item_c(i1_in, i2_in, head->i2_item);
	return;
};

static void set_from_int2_ctl_list_at_c_tbl(int iref_1, int iref_2, struct int2_ctl_list *head,
			int *i1_out, int *i2_out){
	head = find_i2_ctl_list_item_by_value(iref_1, iref_2, head);
	if(head != NULL) set_from_int2_ctl_item_c(head->i2_item, i1_out, i2_out);
	return;
};

static void copy_from_int2_ctl_list(struct int2_ctl_list *head, int num,
			int *iv1, int *iv2){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        set_from_int2_ctl_item_c(head->i2_item, &iv1[i], &iv2[i]);
	};
	return;
};
static void copy_to_int2_ctl_list(int num, int *iv1, int *iv2,
			struct int2_ctl_list *head) {
	int i;
	
	for(i=0;i<num;i++){
		head = add_int2_ctl_list_after(head);
		update_int2_ctl_item_c(iv1[i], iv2[i], head->i2_item);
	};
	return;
};



struct int2_clist * init_int2_clist(void){
    struct int2_clist *i2_clst;
    if((i2_clst = (struct int2_clist *) malloc(sizeof(struct int2_clist))) == NULL) {
        printf("malloc error for int2_clist \n");
        exit(0);
    }
    
    init_int2_ctl_list(&i2_clst->i2_item_head);
    
    i2_clst->clist_name = (char *)calloc(32,sizeof(char));
    i2_clst->i1_name = (char *)calloc(32,sizeof(char));
    i2_clst->i2_name = (char *)calloc(32,sizeof(char));
    return i2_clst;
};

void dealloc_int2_clist(struct int2_clist *i2_clst){
    clear_int2_ctl_list(&i2_clst->i2_item_head);

    free(i2_clst->clist_name);
    free(i2_clst->i1_name);
    free(i2_clst->i2_name);
    
    i2_clst->f_self = NULL;
    
    free(i2_clst);
    return;
};
int count_int2_clist(struct int2_clist *i2_clst){
    return count_int2_ctl_list(&i2_clst->i2_item_head);
};

int read_int2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int2_clist *i2_clst){
    sprintf(i2_clst->clist_name,"%s", label);
    return read_int2_ctl_list(fp, buf, label, &i2_clst->i2_item_head);
};
int write_int2_clist(FILE *fp, int level, const char *label, 
                       struct int2_clist *i2_clst){
    return write_int2_ctl_list(fp, level, label, &i2_clst->i2_item_head);
};

void append_int2_clist(int i1_in, int i2_in, struct int2_clist *i2_clst){
    append_int2_ctl_list(i1_in, i2_in, &i2_clst->i2_item_head);
    return;
};
void del_int2_clist_by_index(int index, struct int2_clist *i2_clst){
    del_int2_ctl_list_by_index(index, &i2_clst->i2_item_head);
    return;
};
void update_int2_clist_by_index(int index, int i1_in, int i2_in,
            struct int2_clist *i2_clst){
    update_int2_ctl_list_by_index(index, i1_in, i2_in, &i2_clst->i2_item_head);
    return;
};
void set_from_int2_clist_at_index(int index, struct int2_clist *i2_clst,
            int *i1_out, int *i2_out){
    set_from_int2_ctl_list_at_index(index, &i2_clst->i2_item_head,
            i1_out, i2_out);
    return;
};

struct int2_ctl_item *int2_clist_at_index(int index, struct int2_clist *i2_clst){
    struct int2_ctl_list *ct_tmp = find_i2_ctl_list_item_by_index(index, &i2_clst->i2_item_head);
    return ct_tmp->i2_item;
}


void add_int2_clist_before_c_tbl(int iref_1, int iref_2, 
            int i1_in, int i2_in, struct int2_clist *i2_clst){
    add_int2_ctl_list_before_c_tbl(iref_1, iref_2,
            i1_in, i2_in, &i2_clst->i2_item_head);
    return;
};
void add_int2_clist_after_c_tbl(int iref_1, int iref_2, 
            int i1_in, int i2_in, struct int2_clist *i2_clst){
    add_int2_ctl_list_after_c_tbl(iref_1, iref_2,
            i1_in, i2_in, &i2_clst->i2_item_head);
    return;
};
void del_int2_clist_by_c_tbl(int iref_1, int iref_2,
            struct int2_clist *i2_clst){
    del_int2_ctl_list_by_c_tbl(iref_1, iref_2, &i2_clst->i2_item_head);
    return;
};
void update_int2_clist_by_c_tbl(int iref_1, int iref_2, 
            int i1_in, int i2_in, struct int2_clist *i2_clst){
    update_int2_ctl_list_by_c_tbl(iref_1, iref_2,
            i1_in, i2_in, &i2_clst->i2_item_head);
    return;
};
void set_from_int2_clist_at_c_tbl(int iref_1, int iref_2,
            struct int2_clist *i2_clst, int *i1_out, int *i2_out){
    set_from_int2_ctl_list_at_c_tbl(iref_1, iref_2, &i2_clst->i2_item_head,
            i1_out, i2_out);
    return;
};

void copy_from_int2_clist(struct int2_clist *i2_clst, int num,
            int *iv1, int *iv2){
    copy_from_int2_ctl_list(&i2_clst->i2_item_head, num, iv1, iv2);
    return;
};
void copy_to_int2_clist(int num, int *iv1, int *iv2,
            struct int2_clist *i2_clst){
    copy_to_int2_ctl_list(num, iv1, iv2, &i2_clst->i2_item_head);
    return;
};

