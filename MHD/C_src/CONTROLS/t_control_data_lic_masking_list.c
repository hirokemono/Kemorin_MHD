/*
//  t_control_data_lic_masking_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_control_data_lic_masking_list.h"

#define NLBL_LIC_MASKING_CTL  3

const char label_lic_masking_ctl[NLBL_LIC_MASKING_CTL][KCHARA_C] = {
	/*[ 0]*/	{"masking_field"},
	/*[ 1]*/	{"masking_component"},
	/*[ 2]*/	{"masking_range"}
};


void get_label_lic_masking_ctl(int index, char *label){
    if(index < NLBL_LIC_MASKING_CTL) strngcopy(label, label_lic_masking_ctl[index]);
    return;
};

void alloc_lic_masking_ctl_c(struct lic_masking_ctl_c *mask_ctl){
	int i;
	
	mask_ctl->maxlen = 0;
	for (i=0;i<NLBL_LIC_MASKING_CTL;i++){
		if(strlen(label_lic_masking_ctl[i]) > mask_ctl->maxlen){
			mask_ctl->maxlen = (int) strlen(label_lic_masking_ctl[i]);
		};
	};
	
	mask_ctl->field_name_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	mask_ctl->component_ctl = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(mask_ctl->field_name_ctl);
	alloc_chara_ctl_item_c(mask_ctl->component_ctl);
	
    mask_ctl->mask_range_list = (struct real2_clist *) malloc(sizeof(struct real2_clist));
	init_real2_clist(mask_ctl->mask_range_list);
    sprintf(mask_ctl->mask_range_list->r1_name, "data");
    sprintf(mask_ctl->mask_range_list->r2_name, "mask_value");
	
	return;
};

void dealloc_lic_masking_ctl_c(struct lic_masking_ctl_c *mask_ctl){
	
	dealloc_chara_ctl_item_c(mask_ctl->field_name_ctl);
	dealloc_chara_ctl_item_c(mask_ctl->component_ctl);
	free(mask_ctl->field_name_ctl);
	free(mask_ctl->component_ctl);
	
	clear_real2_clist(mask_ctl->mask_range_list);
    free(mask_ctl->mask_range_list);
	
	return;
};


int read_lic_masking_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_masking_ctl_c *mask_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_lic_masking_ctl[ 0], mask_ctl->field_name_ctl);
		read_chara_ctl_item_c(buf, label_lic_masking_ctl[ 1], mask_ctl->component_ctl);
		
		read_real2_clist(fp, buf, label_lic_masking_ctl[ 2], mask_ctl->mask_range_list);
	};
	return 1;
};

int write_lic_masking_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_masking_ctl_c *mask_ctl){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, mask_ctl->maxlen, label_lic_masking_ctl[ 0], mask_ctl->field_name_ctl);
	write_chara_ctl_item_c(fp, level, mask_ctl->maxlen, label_lic_masking_ctl[ 1], mask_ctl->component_ctl);
	
	write_real2_clist(fp, level, label_lic_masking_ctl[ 2], mask_ctl->mask_range_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void init_lic_masking_ctl_list(struct lic_masking_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

void clear_lic_masking_ctl_list(struct lic_masking_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
		dealloc_lic_masking_ctl_c(head->lic_mask_c);
		free(head);
        head = head->_next;
	}
	return;
};

struct lic_masking_ctl_list *add_lic_masking_ctl_list_after(struct lic_masking_ctl_list *current){
	struct lic_masking_ctl_list *added;
	struct lic_masking_ctl_list *old_next;
	
	if ((added = (struct lic_masking_ctl_list *) malloc(sizeof(struct lic_masking_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->lic_mask_c = (struct lic_masking_ctl_c *) malloc(sizeof(struct lic_masking_ctl_c))) == NULL) {
        printf("malloc error for lic_mask_c\n");
        exit(0);
    }
	alloc_lic_masking_ctl_c(added->lic_mask_c);
	
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
    free(current->lic_mask_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
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
	int num_array = 0;
	
	iflag = find_control_array_flag_c(buf, label, &num_array);
	if(iflag*num_array == 0) return iflag;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
		if(right_begin_flag_c(buf, label) > 0){
			head = add_lic_masking_ctl_list_after(head);
			iflag = read_lic_masking_ctl_c(fp, buf, label, head->lic_mask_c);
			icou = icou + iflag;
		}
		skip_comment_read_line(fp, buf);
	};
	
	if(num_array /= icou+1){
		printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
	};
	return icou;
};

int write_lic_masking_ctl_list(FILE *fp, int level, const char *label, 
			struct lic_masking_ctl_list *head){
	
	int num = count_lic_masking_ctl_list(head);
	
	if(num == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, num);
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
