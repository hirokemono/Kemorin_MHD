/*
//  t_control_data_pvr_isosurf_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_control_data_pvr_isosurf_list.h"

struct pvr_isosurf_ctl_c * init_pvr_isosurf_ctl_c(){
	int i;
    struct pvr_isosurf_ctl_c *pvr_iso_c;
    if((pvr_iso_c = (struct pvr_isosurf_ctl_c *) malloc(sizeof(struct pvr_isosurf_ctl_c))) == NULL) {
        printf("malloc error for pvr_isosurf_ctl_c \n");
        exit(0);
    }
	
    pvr_iso_c->label_pvr_isosurf = init_label_pvr_isosurf();
    pvr_iso_c->iso_value_ctl = init_real_ctl_item_c();
    pvr_iso_c->opacity_ctl =   init_real_ctl_item_c();
	
	pvr_iso_c->isosurf_type_ctl = init_chara_ctl_item_c();
	
	return pvr_iso_c;
};

void dealloc_pvr_isosurf_ctl_c(struct pvr_isosurf_ctl_c *pvr_iso_c){
	
	free(pvr_iso_c->iso_value_ctl);
	free(pvr_iso_c->opacity_ctl);
	
	dealloc_control_labels_f(pvr_iso_c->label_pvr_isosurf);
	dealloc_chara_ctl_item_c(pvr_iso_c->isosurf_type_ctl);
    free(pvr_iso_c);
	return;
};

int read_pvr_isosurf_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_isosurf_ctl_c *pvr_iso_c){
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_flag_c(buf, label) == 0){
		read_real_ctl_item_c(buf, pvr_iso_c->label_pvr_isosurf->label[ 0],
							 pvr_iso_c->iso_value_ctl);
		read_real_ctl_item_c(buf, pvr_iso_c->label_pvr_isosurf->label[ 1],
							 pvr_iso_c->opacity_ctl);
		read_chara_ctl_item_c(buf, pvr_iso_c->label_pvr_isosurf->label[ 2],
							  pvr_iso_c->isosurf_type_ctl);

        skip_comment_read_line(fp, buf);
	};
	
	return 1;
};

int write_pvr_isosurf_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_isosurf_ctl_c *pvr_iso_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, pvr_iso_c->label_pvr_isosurf->maxlen,
						  pvr_iso_c->label_pvr_isosurf->label[ 0],
						  pvr_iso_c->iso_value_ctl);
	write_real_ctl_item_c(fp, level, pvr_iso_c->label_pvr_isosurf->maxlen,
						  pvr_iso_c->label_pvr_isosurf->label[ 1],
						  pvr_iso_c->opacity_ctl);
	write_chara_ctl_item_c(fp, level, pvr_iso_c->label_pvr_isosurf->maxlen,
						   pvr_iso_c->label_pvr_isosurf->label[ 2],
						   pvr_iso_c->isosurf_type_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void init_pvr_iso_ctl_list(struct pvr_iso_ctl_list *head){
    head->pvr_iso_c = NULL;
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

struct pvr_iso_ctl_list *add_pvr_iso_ctl_list_after(struct pvr_iso_ctl_list *current){
	struct pvr_iso_ctl_list *added;
	struct pvr_iso_ctl_list *old_next;
	
	if ((added = (struct pvr_iso_ctl_list *) malloc(sizeof(struct pvr_iso_ctl_list))) == NULL) {
		printf("malloc error\n");
		exit(0);
	}
	added->pvr_iso_c = init_pvr_isosurf_ctl_c();
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_pvr_iso_ctl_list(struct pvr_iso_ctl_list *current){
    struct pvr_iso_ctl_list *old_prev = current->_prev;
	struct pvr_iso_ctl_list *old_next = current->_next;
	
	dealloc_pvr_isosurf_ctl_c(current->pvr_iso_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};
void clear_pvr_iso_ctl_list(struct pvr_iso_ctl_list *head){
	while (head->_next != NULL) {
		delete_pvr_iso_ctl_list(head->_next);
	}
	return;
};


int count_pvr_iso_ctl_list(struct pvr_iso_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct pvr_iso_ctl_list *set_pvr_iso_ctl_list_pointer(int index, struct pvr_iso_ctl_list *head){
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


int read_pvr_iso_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct pvr_iso_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->pvr_iso_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		if(right_begin_flag_c(buf, label) > 0){
			head = add_pvr_iso_ctl_list_after(head);
			iflag = read_pvr_isosurf_ctl_c(fp, buf, label, head->pvr_iso_c);
			icou = icou + iflag;
		}
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_pvr_iso_ctl_list(FILE *fp, int level, const char *label, 
			struct pvr_iso_ctl_list *head){
	if(count_pvr_iso_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_pvr_isosurf_ctl_c(fp, level, label, head->pvr_iso_c);
		head = head->_next;
		fprintf(fp, "!\n");
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
