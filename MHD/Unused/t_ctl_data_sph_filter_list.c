/*
//  t_ctl_data_sph_filter_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_ctl_data_sph_filter_list.h"

#define NLBL_SPH_FILTER_CTL     7

const char label_sph_filter_ctl[NLBL_SPH_FILTER_CTL][KCHARA_C] = {
	/*[ 0]*/	{"sph_filter_type"},
	/*[ 1]*/	{"radial_filter_type"},
	/*[ 2]*/	{"number_of_moments"},
	/*[ 3]*/	{"radial_filter_width"},
	/*[ 4]*/	{"sphere_filter_width"},
	/*[ 5]*/	{"first_reference_filter_ID"},
	/*[ 6]*/	{"second_reference_filter_ID"}
};

void get_label_sph_filter_ctl(int index, char *label){
    if(index < NLBL_SPH_FILTER_CTL) strngcopy(label, label_sph_filter_ctl[index]);
    return;
};


struct sph_filter_ctl_c * init_sph_filter_ctl_c(){
	int i;
    struct sph_filter_ctl_c *sph_filter_c;
    if((sph_filter_c = (struct sph_filter_ctl_c *) malloc(sizeof(struct sph_filter_ctl_c))) == NULL) {
        printf("malloc error for sph_filter_ctl_c \n");
        exit(0);
    }
	
	sph_filter_c->maxlen = 0;
	for (i=0;i<NLBL_SPH_FILTER_CTL;i++){
		if(strlen(label_sph_filter_ctl[i]) > sph_filter_c->maxlen){
			sph_filter_c->maxlen = (int) strlen(label_sph_filter_ctl[i]);
		};
	};
	
	sph_filter_c->sph_filter_type_c =    init_chara_ctl_item_c();
	sph_filter_c->radial_filter_type_c = init_chara_ctl_item_c();
	
    sph_filter_c->maximum_moments_c =  init_int_ctl_item_c();
    sph_filter_c->first_reference_c =  init_int_ctl_item_c();
    sph_filter_c->second_reference_c = init_int_ctl_item_c();
	
    sph_filter_c->sphere_filter_width_c = init_real_ctl_item_c();
    sph_filter_c->radial_filter_width_c = init_real_ctl_item_c();
	
	return sph_filter_c;
};

void dealloc_sph_filter_ctl_c(struct sph_filter_ctl_c *sph_filter_c){
	
	dealloc_chara_ctl_item_c(sph_filter_c->sph_filter_type_c);
	dealloc_chara_ctl_item_c(sph_filter_c->radial_filter_type_c);
	
	free(sph_filter_c->maximum_moments_c);
	free(sph_filter_c->first_reference_c);
	free(sph_filter_c->second_reference_c);
	
	free(sph_filter_c->sphere_filter_width_c);
	free(sph_filter_c->radial_filter_width_c);
	
    free(sph_filter_c);
	return;
};

int read_sph_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_filter_ctl_c *sph_filter_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_sph_filter_ctl[ 0], sph_filter_c->sph_filter_type_c);
		read_chara_ctl_item_c(buf, label_sph_filter_ctl[ 1], sph_filter_c->radial_filter_type_c);
		
		read_integer_ctl_item_c(buf, label_sph_filter_ctl[ 2], sph_filter_c->maximum_moments_c);
		read_integer_ctl_item_c(buf, label_sph_filter_ctl[ 5], sph_filter_c->first_reference_c);
		read_integer_ctl_item_c(buf, label_sph_filter_ctl[ 6], sph_filter_c->second_reference_c);
		
		read_real_ctl_item_c(buf, label_sph_filter_ctl[ 3], sph_filter_c->sphere_filter_width_c);
		read_real_ctl_item_c(buf, label_sph_filter_ctl[ 4], sph_filter_c->radial_filter_width_c);
	};
	return 1;
};

int write_sph_filter_ctl_c(FILE *fp, int level, const char *label, 
                                struct sph_filter_ctl_c *sph_filter_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 0], sph_filter_c->sph_filter_type_c);
	write_chara_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 1], sph_filter_c->radial_filter_type_c);
	
	write_integer_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 2], sph_filter_c->maximum_moments_c);
	write_real_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 3], sph_filter_c->sphere_filter_width_c);
	write_real_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 4], sph_filter_c->radial_filter_width_c);
	
	write_integer_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 5], sph_filter_c->first_reference_c);
	write_integer_ctl_item_c(fp, level, sph_filter_c->maxlen, label_sph_filter_ctl[ 6], sph_filter_c->second_reference_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void init_sph_filter_ctl_list(struct sph_filter_ctl_list *head){
    head->sph_filter_c = NULL;
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

struct sph_filter_ctl_list *add_sph_filter_ctl_list_after(struct sph_filter_ctl_list *current){
	struct sph_filter_ctl_list *added;
	struct sph_filter_ctl_list *old_next;
	
	if ((added = (struct sph_filter_ctl_list *) malloc(sizeof(struct sph_filter_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
	added->sph_filter_c = init_sph_filter_ctl_c();
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_sph_filter_ctl_list(struct sph_filter_ctl_list *current){
    struct sph_filter_ctl_list *old_prev = current->_prev;
	struct sph_filter_ctl_list *old_next = current->_next;
	
	dealloc_sph_filter_ctl_c(current->sph_filter_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};
void clear_sph_filter_ctl_list(struct sph_filter_ctl_list *head){
	while (head->_next != NULL) {
		delete_sph_filter_ctl_list(head->_next);
	}
	return;
};


int count_sph_filter_ctl_list(struct sph_filter_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct sph_filter_ctl_list *set_sph_filter_ctl_list_pointer(int index, struct sph_filter_ctl_list *head){
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


int read_sph_filter_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct sph_filter_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->sph_filter_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		if(right_begin_flag_c(buf, label) > 0){
			head = add_sph_filter_ctl_list_after(head);
			iflag = read_sph_filter_ctl_c(fp, buf, label, head->sph_filter_c);
			icou = icou + iflag;
		}
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_sph_filter_ctl_list(FILE *fp, int level, const char *label, 
			struct sph_filter_ctl_list *head){
	if(count_sph_filter_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_sph_filter_ctl_c(fp, level, label, head->sph_filter_c);
		head = head->_next;
		if(head != NULL) fprintf(fp, "!\n");
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
    fprintf(fp, "!\n");

    return level;
};
