/*
//  t_control_chara_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_IO.h"


struct chara_ctl_item * init_chara_ctl_item_c(void){
    struct chara_ctl_item *c_item;
    if((c_item = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item))) == NULL) {
        printf("malloc error for chara_ctl_item\n");
        exit(0);
    }
	if((c_item->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for c_item->f_iflag\n");
		exit(0);
	}
    c_item->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	c_item->c_tbl =        (char *)calloc(KCHARA_C, sizeof(char));
    return c_item;
};

void dealloc_chara_ctl_item_c(struct chara_ctl_item *c_item){
    free(c_item->c_tbl);
	c_item->f_self = NULL;
	c_item->f_iflag = NULL;
    free(c_item->c_block_name);
	
	free(c_item);
    return;
};

int read_chara_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                               struct chara_ctl_item *c_item){
	char header_chara[KCHARA_C];
	
	if(c_item->f_iflag[0] > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s", header_chara, c_item->c_tbl);
		strip_cautation_marks(c_item->c_tbl);
		c_item->f_iflag[0] = 1;
	};
	return 1;
};

int write_chara_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct chara_ctl_item *c_item){
    
	if(c_item->f_iflag[0] == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	write_one_label_w_lf_c(fp, c_item->c_tbl);
    return level;
};

void update_chara_ctl_item_c(char *c_in, struct chara_ctl_item *c_item){
	c_item->f_iflag[0] = 1;
	sprintf(c_item->c_tbl,"%s", c_in);
    return;
};
void set_from_chara_ctl_item_c(struct chara_ctl_item *c_item, char *c_out){
	if(c_item->f_iflag[0] == 0) return;
	sprintf(c_out,"%s", c_item->c_tbl);
    return;
};


int find_boolean_from_chara_ctl_item(struct chara_ctl_item *c_item){
    int iflag = 0;
	if(c_item->f_iflag[0] == 0) return iflag;
	
	iflag = cmp_no_case_c(c_item->c_tbl, "ON");
    if(iflag == 0) iflag = cmp_no_case_c(c_item->c_tbl, "YES");
    return iflag;
};

void set_boolean_by_chara_ctl_item(int iflag, struct chara_ctl_item *c_item){
	c_item->f_iflag[0] = 1;
    if(iflag > 0){
        sprintf(c_item->c_tbl, "%s", "On");
    } else {
        sprintf(c_item->c_tbl, "%s", "Off");
    };
    return;
};

void copy_from_chara_ctl_item(struct chara_ctl_item *c_item, char *c_data){
	if(c_item->f_iflag[0] == 0) return;
	sprintf(c_data, "%s", c_item->c_tbl);
	return;
};
void copy_to_chara_ctl_item(const char *c_data, struct chara_ctl_item *c_item){
	c_item->f_iflag[0] = 1;
	sprintf(c_item->c_tbl, "%s", c_data);
	return;
};



static void init_chara_ctl_list(struct chara_ctl_list *head){
    head->c_item = NULL;
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

struct chara_ctl_list *add_chara_ctl_list_before(struct chara_ctl_list *current){
    struct chara_ctl_list *added;
    struct chara_ctl_list *old_prev;
    
    if ((added = (struct chara_ctl_list *) malloc(sizeof(struct chara_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->c_item = init_chara_ctl_item_c();
    
	/* replace from  prev -> current to prev -> new -> current */
	old_prev = current->_prev;
	current->_prev = added;
	added->_prev = old_prev;
	old_prev->_next = added;
	added->_next = current;
    
    return added;
};

static struct chara_ctl_list *add_chara_ctl_list_after(struct chara_ctl_list *current){
    struct chara_ctl_list *added;
    struct chara_ctl_list *old_next;
    
    if ((added = (struct chara_ctl_list *) malloc(sizeof(struct chara_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	added->c_item = init_chara_ctl_item_c();
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_chara_ctl_list(struct chara_ctl_list *current){
    struct chara_ctl_list *old_prev = current->_prev;
    struct chara_ctl_list *old_next = current->_next;
    
    dealloc_chara_ctl_item_c(current->c_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};
static void clear_chara_ctl_list(struct chara_ctl_list *head){
    while (head->_next != NULL) {delete_chara_ctl_list(head->_next);}
    return;
};


static int count_chara_ctl_list(struct chara_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct chara_ctl_list *find_c_ctl_list_item_by_index(int index, struct chara_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara_ctl_list(head)) return NULL;
    for(i=0;i<index+1;i++){head = head->_next;};
    return head;
};
static struct chara_ctl_list *find_c_ctl_list_item_by_c_tbl(char *ref, struct chara_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->c_item->c_tbl, ref)) return head;
        head = head->_next;
    };
    return head;
};

static int read_chara_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->c_item != NULL) return 0;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label) == 0){
        head = add_chara_ctl_list_after(head);
        iflag = read_chara_ctl_item_c(buf, label, head->c_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    return icou;
};

static int write_chara_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_ctl_list *head){
    if(count_chara_ctl_list(head) == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->c_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


static void append_chara_ctl_list(char *c_in, struct chara_ctl_list *head){
    int num = count_chara_ctl_list(head);
    if(num > 0) head = find_c_ctl_list_item_by_index(num-1, head);
	head = add_chara_ctl_list_after(head);
	update_chara_ctl_item_c(c_in, head->c_item);
    return;
};

static void del_chara_ctl_list_by_index(int index, struct chara_ctl_list *head){
	head = find_c_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_chara_ctl_list(head);
	return;
};

static void update_chara_ctl_list_by_index(int index, char *c_in,
			struct chara_ctl_list *head){
	head = find_c_ctl_list_item_by_index(index, head);
	if(head != NULL) update_chara_ctl_item_c(c_in, head->c_item);
	return;
};

static void set_from_chara_ctl_list_at_index(int index, struct chara_ctl_list *head, char *c_out){
	head = find_c_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_chara_ctl_item_c(head->c_item, c_out);
	return;
};


static void add_chara_ctl_list_before_c_tbl(char *ref, char *c_in,
			struct chara_ctl_list *head){
	head = find_c_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_ctl_list_before(head);
	update_chara_ctl_item_c(c_in, head->c_item);
	return;
};
static void add_chara_ctl_list_after_c_tbl(char *ref, char *c_in,
			struct chara_ctl_list *head){
	head = find_c_ctl_list_item_by_c_tbl(ref, head);
	if(head == NULL) return;
	head = add_chara_ctl_list_after(head);
	update_chara_ctl_item_c(c_in, head->c_item);
	return;
};
static void del_chara_ctl_list_by_c_tbl(char *ref, struct chara_ctl_list *head){
	head = find_c_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_chara_ctl_list(head);
	return;
};
static void update_chara_ctl_list_by_c_tbl(char *ref, char *c_in,
			struct chara_ctl_list *head){
	head = find_c_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) update_chara_ctl_item_c(c_in, head->c_item);
	return;
};
static void set_from_chara_ctl_list_at_c_tbl(char *ref, struct chara_ctl_list *head,
			char *c_out){
	head = find_c_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) set_from_chara_ctl_item_c(head->c_item, c_out);
	return;
};


struct chara_clist * init_chara_clist(void){
    struct chara_clist *c_clst;
    if((c_clst = (struct chara_clist *) malloc(sizeof(struct chara_clist))) == NULL) {
        printf("malloc error for chara_clist\n");
        exit(0);
    }
    init_chara_ctl_list(&c_clst->c_item_head);

    c_clst->iflag_use = 0;
    c_clst->clist_name = (char *)calloc(KCHARA_C,sizeof(char));
    c_clst->c1_name = (char *)calloc(KCHARA_C,sizeof(char));
    return c_clst;
};
void dealloc_chara_clist(struct chara_clist *c_clst){
    clear_chara_ctl_list(&c_clst->c_item_head);
    
    free(c_clst->clist_name);
    free(c_clst->c1_name);

    free(c_clst);
    return;
};

int count_chara_clist(struct chara_clist *c_clst){
    return count_chara_ctl_list(&c_clst->c_item_head);
};

void read_chara_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_clist *c_clst){
    if(c_clst->iflag_use > 0) return;
    
    sprintf(c_clst->clist_name,"%s", label);
    c_clst->iflag_use = read_chara_ctl_list(fp, buf, label, &c_clst->c_item_head);
    return;
};
int write_chara_clist(FILE *fp, int level, const char *label, 
                       struct chara_clist *c_clst){
    if(c_clst->iflag_use == 0) return level;

    return write_chara_ctl_list(fp, level, label, &c_clst->c_item_head);
};

void append_chara_clist(char *c_in, struct chara_clist *c_clst){
    append_chara_ctl_list(c_in, &c_clst->c_item_head);
    return;
};
void del_chara_clist_by_index(int index, struct chara_clist *c_clst){
    del_chara_ctl_list_by_index(index, &c_clst->c_item_head);
    return;
};
void update_chara_clist_by_index(int index, char *c_in, struct chara_clist *c_clst){
    update_chara_ctl_list_by_index(index, c_in, &c_clst->c_item_head);
    return;
};
void set_from_chara_clist_at_index(int index, struct chara_clist *c_clst, char *c_out){
    set_from_chara_ctl_list_at_index(index, &c_clst->c_item_head, c_out);
    return;
};

struct chara_ctl_item *chara_clist_at_index(int index, struct chara_clist *c_clst){
    struct chara_ctl_list *tmp_list = find_c_ctl_list_item_by_index(index, &c_clst->c_item_head);
    return tmp_list->c_item;
}

void add_chara_clist_before_c_tbl(char *ref, char *c_in, struct chara_clist *c_clst){
    add_chara_ctl_list_before_c_tbl(ref, c_in, &c_clst->c_item_head);
    return;
};
void add_chara_clist_after_c_tbl(char *ref, char *c_in, struct chara_clist *c_clst){
    add_chara_ctl_list_after_c_tbl(ref, c_in, &c_clst->c_item_head);
    return;
};
void del_chara_clist_by_c_tbl(char *ref, struct chara_clist *c_clst){
    del_chara_ctl_list_by_c_tbl(ref, &c_clst->c_item_head);
    return;
};
void update_chara_clist_by_c_tbl(char *ref, char *c_in, struct chara_clist *c_clst){
    update_chara_ctl_list_by_c_tbl(ref, c_in, &c_clst->c_item_head);
    return;
};
void set_from_chara_clist_at_c_tbl(char *ref, struct chara_clist *c_clst, char *c_out){
    set_from_chara_ctl_list_at_c_tbl(ref, &c_clst->c_item_head, c_out);
    return;
};
