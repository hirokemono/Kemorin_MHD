/*
//  t_control_data_lic_masking_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_control_data_lic_masking_list.h"

int num_ctl_label_LIC_masking_f();
void set_ctl_label_LIC_masking_f(char *label_1d);

struct label_list_f * init_ctl_label_LIC_masking_f(){
	int len_fix = lengthchara_f();
	struct label_list_f *label_list = alloc_ctl_label();
	label_list->num_labels = num_ctl_label_LIC_masking_f();
	
    char *packed_name = alloc_string((long) (len_fix*label_list->num_labels));	
	set_ctl_label_LIC_masking_f(packed_name);
	set_labels_from_packed(len_fix, packed_name, label_list);
	free(packed_name);
	return label_list;
};


struct lic_masking_ctl_c * init_lic_masking_ctl_c(){
    struct lic_masking_ctl_c *mask_ctl;
    if((mask_ctl = (struct lic_masking_ctl_c *) malloc(sizeof(struct lic_masking_ctl_c))) == NULL) {
        printf("malloc error for lic_masking_ctl_c \n");
        exit(0);
    }
    
	mask_ctl->label_lic_masking = init_ctl_label_LIC_masking_f();
	mask_ctl->masking_type_ctl = init_chara_ctl_item_c();
	mask_ctl->field_name_ctl = init_chara_ctl_item_c();
	mask_ctl->component_ctl = init_chara_ctl_item_c();
	
	mask_ctl->mask_range_list = init_real2_clist();
    sprintf(mask_ctl->mask_range_list->r1_name, "data");
    sprintf(mask_ctl->mask_range_list->r2_name, "mask_value");
	
	return mask_ctl;
};

void dealloc_lic_masking_ctl_c(struct lic_masking_ctl_c *mask_ctl){
	dealloc_ctl_label(mask_ctl->label_lic_masking);
	dealloc_chara_ctl_item_c(mask_ctl->masking_type_ctl);
	dealloc_chara_ctl_item_c(mask_ctl->field_name_ctl);
	dealloc_chara_ctl_item_c(mask_ctl->component_ctl);
	
	dealloc_real2_clist(mask_ctl->mask_range_list);
    free(mask_ctl);
	return;
};


int read_lic_masking_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_masking_ctl_c *mask_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, mask_ctl->label_lic_masking->label[ 0], 
							  mask_ctl->masking_type_ctl);
		read_chara_ctl_item_c(buf, mask_ctl->label_lic_masking->label[ 1], 
							  mask_ctl->field_name_ctl);
		read_chara_ctl_item_c(buf, mask_ctl->label_lic_masking->label[ 2], 
							  mask_ctl->component_ctl);
		
		read_real2_clist(fp, buf, mask_ctl->label_lic_masking->label[ 3], 
						 mask_ctl->mask_range_list);
	};
	return 1;
};

int write_lic_masking_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_masking_ctl_c *mask_ctl){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, mask_ctl->label_lic_masking->maxlen, 
						   mask_ctl->label_lic_masking->label[ 0],
						   mask_ctl->masking_type_ctl);
	write_chara_ctl_item_c(fp, level, mask_ctl->label_lic_masking->maxlen, 
						   mask_ctl->label_lic_masking->label[ 1],
						   mask_ctl->field_name_ctl);
	write_chara_ctl_item_c(fp, level, mask_ctl->label_lic_masking->maxlen,
						   mask_ctl->label_lic_masking->label[ 2],
						   mask_ctl->component_ctl);
	
	write_real2_clist(fp, level, mask_ctl->label_lic_masking->label[ 3],
					  mask_ctl->mask_range_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void init_lic_masking_ctl_list(struct lic_masking_ctl_list *head){
    head->lic_mask_c = NULL;
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

struct lic_masking_ctl_list *add_lic_masking_ctl_list_after(struct lic_masking_ctl_list *current){
	struct lic_masking_ctl_list *added;
	struct lic_masking_ctl_list *old_next;
	
	if ((added = (struct lic_masking_ctl_list *) malloc(sizeof(struct lic_masking_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
	added->lic_mask_c = init_lic_masking_ctl_c();
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_lic_masking_ctl_list(struct lic_masking_ctl_list *current){
    struct lic_masking_ctl_list *old_prev = current->_prev;
	struct lic_masking_ctl_list *old_next = current->_next;
	
	dealloc_lic_masking_ctl_c(current->lic_mask_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};
void clear_lic_masking_ctl_list(struct lic_masking_ctl_list *head){
	while (head->_next != NULL) {
		delete_lic_masking_ctl_list(head->_next);
	}
	return;
};


int count_lic_masking_ctl_list(struct lic_masking_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct lic_masking_ctl_list *set_lic_masking_ctl_list_pointer(int index, struct lic_masking_ctl_list *head){
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


int read_lic_masking_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct lic_masking_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->lic_mask_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		if(right_begin_flag_c(buf, label) > 0){
			head = add_lic_masking_ctl_list_after(head);
			iflag = read_lic_masking_ctl_c(fp, buf, label, head->lic_mask_c);
			icou = icou + iflag;
		}
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_lic_masking_ctl_list(FILE *fp, int level, const char *label, 
			struct lic_masking_ctl_list *head){
	if(count_lic_masking_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_lic_masking_ctl_c(fp, level, label, head->lic_mask_c);
		head = head->_next;
		if(head != NULL) fprintf(fp, "!\n");
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
    fprintf(fp, "!\n");

    return level;
};
