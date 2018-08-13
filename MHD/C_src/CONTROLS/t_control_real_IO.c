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


struct real_ctl_list *add_real_ctl_list(struct real_ctl_list *current){
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
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_real_ctl_list(struct real_ctl_list *current){
    struct real_ctl_list *old_prev = current->_prev;
    struct real_ctl_list *old_next = current->_next;
    
    init_real_ctl_item_c(current->r_item);
    free(current->r_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_real_ctl_list(struct real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct real_ctl_list *set_real_ctl_list_pointer(int index, struct real_ctl_list *head){
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

int read_real_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_real_ctl_list(head);
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


double copy_from_real_ctl_item(struct real_ctl_item *r_item){
	if(r_item->iflag == 0) return 0.0;
	return r_item->r_data;
};
void copy_to_real_ctl_item(double value, struct real_ctl_item *r_item){
	r_item->iflag = 1;
	r_item->r_data = value;
	return;
};

void copy_from_real_ctl_list(struct real_ctl_list *head, int num, double *vector) {
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        vector[i] = copy_from_real_ctl_item(head->r_item);
	};
	return;
};
void copy_to_real_ctl_list(int num, double *vector, struct real_ctl_list *head) {
	int i;
	
	for(i=0;i<num;i++){
		head = add_real_ctl_list(head);
		copy_to_real_ctl_item(vector[i], head->r_item);
	};
	return;
};

