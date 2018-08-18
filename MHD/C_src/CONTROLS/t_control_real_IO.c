/*
//  t_control_real_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_real_IO.h"


void init_real_ctl_item_c(struct real_ctl_item *r_item){
	r_item->r_data = 0.0;
	r_item->iflag = 0;
    return;
};

int read_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real_ctl_item *r_item){
	char header_chara[KCHARA_C];
	
	if(r_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %lf", header_chara, &r_item->r_data);
		r_item->iflag = 1;
	};
	return 1;
};

int write_real_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct real_ctl_item *r_item){
    
	if(r_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp,  "%.12e\n", r_item->r_data);
    return level;
};


void update_real_ctl_item_c(double r1_in, struct real_ctl_item *r_item){
	r_item->iflag = 1;
	r_item->r_data = r1_in;
    return;
};
void set_from_real_ctl_item_c(struct real_ctl_item *r_item, double *r1_out){
	if(r_item->iflag == 0) return;
	*r1_out = r_item->r_data;
    return;
};


void init_real_ctl_list(struct real_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_real_ctl_list(struct real_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        init_real_ctl_item_c(head->r_item);
        free(head);
        head = head->_next;
	}
	
    return;
};


static struct real_ctl_list *add_real_ctl_list_after(struct real_ctl_list *current){
    struct real_ctl_list *added;
    struct real_ctl_list *old_next;
    
    if ((added = (struct real_ctl_list *) malloc(sizeof(struct real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->r_item = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item))) == NULL) {
        printf("malloc error for r_item\n");
        exit(0);
    }
	init_real_ctl_item_c(added->r_item);
    
    /* replace from  current -> next to current -> new -> next */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

static void delete_real_ctl_list(struct real_ctl_list *current){
    struct real_ctl_list *old_prev = current->_prev;
    struct real_ctl_list *old_next = current->_next;
    
    init_real_ctl_item_c(current->r_item);
    free(current->r_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

static int count_real_ctl_list(struct real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

static struct real_ctl_list *find_r_ctl_list_item_by_index(int index, struct real_ctl_list *head){
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
static struct real_ctl_list *find_r_ctl_list_item_by_value(double ref,
			struct real_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(head->r_item->r_data == ref) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %lf does not exist.\n", ref);
    return head;
};

int read_real_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_real_ctl_list_after(head);
        iflag = read_real_ctl_item_c(buf, label, head->r_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_real_ctl_list(FILE *fp, int level, const char *label, 
                       struct real_ctl_list *head){
    
    int num = count_real_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_real_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->r_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


void append_real_ctl_list(double r1_in, struct real_ctl_list *head){
	int num = count_real_ctl_list(head);
	head = find_r_ctl_list_item_by_index(num, head);
	head = add_real_ctl_list_after(head);
	update_real_ctl_item_c(r1_in, head->r_item);
    return;
};

void del_real_ctl_list_by_index(int index, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_real_ctl_list(head);
	return;
};

void update_real_ctl_list_by_index(int index, double r1_in, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_index(index, head);
	if(head != NULL) update_real_ctl_item_c(r1_in, head->r_item);
	return;
};

void set_from_real_ctl_list_at_index(int index, struct real_ctl_list *head, double *r1_out){
	head = find_r_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_real_ctl_item_c(head->r_item, r1_out);
	return;
};


void del_real_ctl_list_by_c_tbl(double ref, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head != NULL) delete_real_ctl_list(head);
	return;
};

static void update_real_ctl_list_by_c_tbl(double ref,
			double r1_in, struct real_ctl_list *head){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head != NULL) update_real_ctl_item_c(r1_in, head->r_item);
	return;
};

static void set_from_real_ctl_list_at_c_tbl(double ref, struct real_ctl_list *head,
			double *r1_out){
	head = find_r_ctl_list_item_by_value(ref, head);
	if(head != NULL) set_from_real_ctl_item_c(head->r_item, r1_out);
	return;
};

static void copy_from_real_ctl_list(struct real_ctl_list *head, int num, double *v1){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        set_from_real_ctl_item_c(head->r_item, &v1[i]);
	};
	return;
};
static void copy_to_real_ctl_list(int num, double *v1, struct real_ctl_list *head){
	int i;
	
	for(i=0;i<num;i++){
		head = add_real_ctl_list_after(head);
		update_real_ctl_item_c(v1[i], head->r_item);
	};
	return;
};





