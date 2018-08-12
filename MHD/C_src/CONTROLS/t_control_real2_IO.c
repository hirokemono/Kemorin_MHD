/*
//  t_control_real2_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_real2_IO.h"


void init_real2_ctl_item_c(struct real2_ctl_item *r2_item){
	r2_item->r_data[0] = 0.0;
	r2_item->r_data[1] = 0.0;
	r2_item->iflag = 0;
    return;
};

int read_real2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real2_ctl_item *r2_item){
	char header_chara[KCHARA_C];
	
	if(r2_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %lf %lf", header_chara, 
					&r2_item->r_data[0], &r2_item->r_data[1]);
		r2_item->iflag = 1;
	};
	return 1;
};

int write_real2_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct real2_ctl_item *r2_item){
    
	if(r2_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp, "%.12e  %.12e\n", r2_item->r_data[0], r2_item->r_data[1]);
    return level;
};



void init_real2_ctl_list(struct real2_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_real2_ctl_list(struct real2_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        init_real2_ctl_item_c(head->r2_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct real2_ctl_list *add_real2_ctl_list(struct real2_ctl_list *current){
    struct real2_ctl_list *added;
    struct real2_ctl_list *old_next;
    
    if ((added = (struct real2_ctl_list *) malloc(sizeof(struct real2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->r2_item = (struct real2_ctl_item *) malloc(sizeof(struct real2_ctl_item))) == NULL) {
        printf("malloc error for r2_item\n");
        exit(0);
    }
	init_real2_ctl_item_c(added->r2_item);
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_real2_ctl_list(struct real2_ctl_list *current){
    struct real2_ctl_list *old_prev = current->_prev;
    struct real2_ctl_list *old_next = current->_next;
    
    init_real2_ctl_item_c(current->r2_item);
    free(current->r2_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_real2_ctl_list(struct real2_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct real2_ctl_list *set_real2_ctl_list_pointer(int index, struct real2_ctl_list *head){
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

int read_real2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real2_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_real2_ctl_list(head);
        iflag = read_real2_ctl_item_c(buf, label, head->r2_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_real2_ctl_list(FILE *fp, int level, const char *label, 
                       struct real2_ctl_list *head){
    
    int num = count_real2_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_real2_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->r2_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


void copy_from_real2_ctl_item(struct real2_ctl_item *r2_item, 
			double *r1, double *r2){
	if(r2_item->iflag == 0) return;
	*r1 = r2_item->r_data[0];
	*r2 = r2_item->r_data[1];
	return;
};
void copy_to_real2_ctl_item(double r1, double r2,
			struct real2_ctl_item *r2_item){
	r2_item->iflag = 1;
	r2_item->r_data[0] = r1;
	r2_item->r_data[1] = r2;
	return;
};

void copy_from_real2_ctl_list(struct real2_ctl_list *head, int num,
			double *v1, double *v2){
	int i;
    head = head->_next;
	for(i=0; i<num; i++){
		if(head != NULL) break;
        copy_from_real2_ctl_item(head->r2_item, &v1[i], &v2[i]);
	};
	return;
};
void copy_to_real2_ctl_list(int num, double *v1, double *v2,
			struct real2_ctl_list *head) {
	int i;
	
	for(i=0;i<num;i++){
		head = add_real2_ctl_list(head);
		copy_to_real2_ctl_item(v1[i], v2[i], head->r2_item);
	};
	return;
};

