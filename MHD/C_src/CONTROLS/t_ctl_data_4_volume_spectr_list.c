/*
//  t_ctl_data_4_volume_spectr_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_ctl_data_4_volume_spectr_list.h"

#define NLBL_VOLUME_SPECTR 4

const char label_volume_spectr_ctl[NLBL_VOLUME_SPECTR][KCHARA_C] = {
    /*[ 0]*/    {"volume_pwr_spectr_prefix"},
    /*[ 1]*/    {"volume_average_prefix"},
    
    /*[ 2]*/    {"inner_radius_ctl"},
    /*[ 3]*/    {"outer_radius_ctl"}
};


void get_label_volume_spectr_ctl(int index, char *label){
    if(index < NLBL_VOLUME_SPECTR) strngcopy(label, label_volume_spectr_ctl[index]);
    return;
};


void alloc_volume_spectr_control_c(struct volume_spectr_control_c *v_pwr_c){
    int i;
    
    v_pwr_c->maxlen = 0;
    for (i=0;i<NLBL_VOLUME_SPECTR;i++){
        if(strlen(label_volume_spectr_ctl[i]) > v_pwr_c->maxlen){
            v_pwr_c->maxlen = (int) strlen(label_volume_spectr_ctl[i]);
        };
	};
	
	v_pwr_c->volume_spec_file_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_chara_ctl_item_c(v_pwr_c->volume_spec_file_c);
	v_pwr_c->volume_ave_file_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_chara_ctl_item_c(v_pwr_c->volume_ave_file_c);
	
	v_pwr_c->inner_radius_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(v_pwr_c->inner_radius_c);
	v_pwr_c->outer_radius_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(v_pwr_c->outer_radius_c);
	
	return;
};

void dealloc_volume_spectr_control_c(struct volume_spectr_control_c *v_pwr_c){
	
    dealloc_chara_ctl_item_c(v_pwr_c->volume_spec_file_c);
	free(v_pwr_c->volume_spec_file_c);
	dealloc_chara_ctl_item_c(v_pwr_c->volume_ave_file_c);
	free(v_pwr_c->volume_ave_file_c);
	
	free(v_pwr_c->inner_radius_c);
	free(v_pwr_c->outer_radius_c);
	return;
};


int read_volume_spectr_control_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct volume_spectr_control_c *v_pwr_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_volume_spectr_ctl[0], v_pwr_c->volume_spec_file_c);
		read_chara_ctl_item_c(buf, label_volume_spectr_ctl[1], v_pwr_c->volume_ave_file_c);
		
		read_real_ctl_item_c(buf, label_volume_spectr_ctl[2], v_pwr_c->inner_radius_c);
		read_real_ctl_item_c(buf, label_volume_spectr_ctl[3], v_pwr_c->outer_radius_c);
	};
	return 1;
};

int write_volume_spectr_control_c(FILE *fp, int level, const char *label, 
			struct volume_spectr_control_c *v_pwr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, v_pwr_c->maxlen, 
				label_volume_spectr_ctl[0], v_pwr_c->volume_spec_file_c);
	write_chara_ctl_item_c(fp, level, v_pwr_c->maxlen, 
				label_volume_spectr_ctl[1], v_pwr_c->volume_ave_file_c);
	
	write_real_ctl_item_c(fp, level, v_pwr_c->maxlen, 
				label_volume_spectr_ctl[2], v_pwr_c->inner_radius_c);
	write_real_ctl_item_c(fp, level, v_pwr_c->maxlen, 
				label_volume_spectr_ctl[3], v_pwr_c->outer_radius_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void init_sph_vol_spectr_list(struct volume_spectr_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

void clear_sph_vol_spectr_ctl_list(struct volume_spectr_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
		dealloc_volume_spectr_control_c(head->v_pwr_c);
		free(head);
        head = head->_next;
	}
	return;
};

struct volume_spectr_ctl_list *add_sph_vol_spectr_ctl_list_after(struct volume_spectr_ctl_list *current){
	struct volume_spectr_ctl_list *added;
	struct volume_spectr_ctl_list *old_next;
	
	if ((added = (struct volume_spectr_ctl_list *) malloc(sizeof(struct volume_spectr_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->v_pwr_c = (struct volume_spectr_control_c *) malloc(sizeof(struct volume_spectr_control_c))) == NULL) {
        printf("malloc error for v_pwr_c\n");
        exit(0);
    }
	alloc_volume_spectr_control_c(added->v_pwr_c);
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_vol_spectr_ctl_list(struct volume_spectr_ctl_list *current){
    struct volume_spectr_ctl_list *old_prev = current->_prev;
	struct volume_spectr_ctl_list *old_next = current->_next;
	
	dealloc_volume_spectr_control_c(current->v_pwr_c);
    free(current->v_pwr_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};

int count_vol_spectr_ctl_list(struct volume_spectr_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct volume_spectr_ctl_list *set_sph_vol_spec_ctl_list_pointer(int index, struct volume_spectr_ctl_list *head){
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


int read_sph_vol_spectr_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct volume_spectr_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->v_pwr_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		if(right_begin_flag_c(buf, label) > 0){
			head = add_sph_vol_spectr_ctl_list_after(head);
			iflag = read_volume_spectr_control_c(fp, buf, label, head->v_pwr_c);
			icou = icou + iflag;
		}
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_sph_vol_spectr_ctl_list(FILE *fp, int level, const char *label, 
			struct volume_spectr_ctl_list *head){
	if(count_vol_spectr_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_volume_spectr_control_c(fp, level, label, head->v_pwr_c);
		head = head->_next;
		if(head != NULL) fprintf(fp, "!\n");
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
    fprintf(fp, "!\n");

    return level;
};
