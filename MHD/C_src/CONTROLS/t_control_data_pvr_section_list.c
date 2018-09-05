/*
//  t_control_data_pvr_section_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_control_data_pvr_section_list.h"

#define NLBL_PVR_SECTIONS_CTL   2

const char label_pvr_sections_ctl[NLBL_PVR_SECTIONS_CTL][KCHARA_C] = {
	/*[ 0]*/	{"surface_define"},
	/*[ 1]*/	{"opacity_ctl"},
};


void get_label_pvr_sections_ctl(int index, char *label){
    if(index < NLBL_PVR_SECTIONS_CTL) strngcopy(label, label_pvr_sections_ctl[index]);
    return;
};

void alloc_pvr_section_ctl_c(struct pvr_section_ctl_c *pvr_sect_c){
	int i;
	
	pvr_sect_c->maxlen = 0;
	for (i=0;i<NLBL_PVR_SECTIONS_CTL;i++){
		if(strlen(label_pvr_sections_ctl[i]) > pvr_sect_c->maxlen){
			pvr_sect_c->maxlen = (int) strlen(label_pvr_sections_ctl[i]);
		};
	};
	
	pvr_sect_c->iflag_psf_define_ctl = 0;
	pvr_sect_c->fname_sect_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	
	pvr_sect_c->psf_def_c = (struct psf_define_ctl_c *) malloc(sizeof(struct psf_define_ctl_c));
	alloc_psf_define_ctl_c(pvr_sect_c->psf_def_c);
	
	pvr_sect_c->opacity_ctl = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_real_ctl_item_c(pvr_sect_c->opacity_ctl);
	
	return;
};

void dealloc_pvr_section_ctl_c(struct pvr_section_ctl_c *pvr_sect_c){
	dealloc_psf_define_ctl_c(pvr_sect_c->psf_def_c);
	free(pvr_sect_c->psf_def_c);
	free(pvr_sect_c->opacity_ctl);
	free(pvr_sect_c->fname_sect_ctl);
	pvr_sect_c->iflag_psf_define_ctl = 0;
	
	return;
};

int read_pvr_section_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_section_ctl_c *pvr_sect_c){
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_flag_c(buf, label) == 0){
		read_real_ctl_item_c(buf, label_pvr_sections_ctl[ 1], pvr_sect_c->opacity_ctl);
		
		if(right_begin_flag_c(buf, label_pvr_sections_ctl[ 0]) > 0){
			pvr_sect_c->iflag_psf_define_ctl = read_psf_define_ctl_c(fp, buf, 
						label_pvr_sections_ctl[0], pvr_sect_c->psf_def_c);
		} else if(right_file_flag_c(buf, label_pvr_sections_ctl[ 0])){
			pvr_sect_c->iflag_psf_define_ctl = read_file_flag_c(buf, pvr_sect_c->fname_sect_ctl);
		};
		
		skip_comment_read_line(fp, buf);
	};
	return 1;
};

int write_pvr_section_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_section_ctl_c *pvr_sect_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, pvr_sect_c->maxlen, label_pvr_sections_ctl[ 1], pvr_sect_c->opacity_ctl);
	
	if(pvr_sect_c->iflag_psf_define_ctl == 1){
		fprintf(fp, "!\n");
		level = write_psf_define_ctl_c(fp, level, label_pvr_sections_ctl[ 0], pvr_sect_c->psf_def_c);
	} else if(pvr_sect_c->iflag_psf_define_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, label_pvr_sections_ctl[ 0], pvr_sect_c->fname_sect_ctl);
	};
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void read_pvr_section_subfile_c(char buf[LENGTHBUF], struct pvr_section_ctl_c *pvr_sect_c){
	if(pvr_sect_c->iflag_psf_define_ctl == -1){
		read_psf_define_file_c(pvr_sect_c->fname_sect_ctl, buf, pvr_sect_c->psf_def_c);
	};
 	return;
};

void write_pvr_section_subfile_c(struct pvr_section_ctl_c *pvr_sect_c){
	if(pvr_sect_c->iflag_psf_define_ctl == -1){
		write_psf_define_file_c(pvr_sect_c->fname_sect_ctl, pvr_sect_c->psf_def_c);
	};
 	return;
};



void init_pvr_section_ctl_list(struct pvr_sect_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

void clear_pvr_section_ctl_list(struct pvr_sect_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
		dealloc_pvr_section_ctl_c(head->pvr_sect_c);
		free(head);
        head = head->_next;
	}
	return;
};

struct pvr_sect_ctl_list *add_pvr_section_ctl_list_after(struct pvr_sect_ctl_list *current){
	struct pvr_sect_ctl_list *added;
	struct pvr_sect_ctl_list *old_next;
	
	if ((added = (struct pvr_sect_ctl_list *) malloc(sizeof(struct pvr_sect_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->pvr_sect_c = (struct pvr_section_ctl_c *) malloc(sizeof(struct pvr_section_ctl_c))) == NULL) {
        printf("malloc error for pvr_sect_c\n");
        exit(0);
    }
	alloc_pvr_section_ctl_c(added->pvr_sect_c);
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_pvr_section_ctl_list(struct pvr_sect_ctl_list *current){
    struct pvr_sect_ctl_list *old_prev = current->_prev;
	struct pvr_sect_ctl_list *old_next = current->_next;
	
	dealloc_pvr_section_ctl_c(current->pvr_sect_c);
    free(current->pvr_sect_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};

int count_pvr_section_ctl_list(struct pvr_sect_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct pvr_sect_ctl_list *set_pvr_section_ctl_list_pointer(int index, struct pvr_sect_ctl_list *head){
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

void rename_pvr_section_subfile_list(struct pvr_sect_ctl_list *head){
    head = head->_next;
	while (head != NULL){
        if(head->pvr_sect_c->iflag_psf_define_ctl == -1){
            strcat(head->pvr_sect_c->fname_sect_ctl, "_2");
        };
		head = head->_next;
	};
	return;
};

void read_pvr_section_subfile_list(char buf[LENGTHBUF], struct pvr_sect_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		read_pvr_section_subfile_c(buf, head->pvr_sect_c);
		head = head->_next;
	};
	return;
};

void write_pvr_section_subfile_list(struct pvr_sect_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		write_pvr_section_subfile_c(head->pvr_sect_c);
		head = head->_next;
	};
	return;
};

int read_pvr_section_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct pvr_sect_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	int num_array = 0;
	
	iflag = find_control_array_flag_c(buf, label, &num_array);
	if(iflag*num_array == 0) return iflag;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
		head = add_pvr_section_ctl_list_after(head);
		iflag = read_pvr_section_ctl_c(fp, buf, label, head->pvr_sect_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	
	if(num_array /= icou+1){
		printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
	};
	return icou;
};

int write_pvr_section_ctl_list(FILE *fp, int level, const char *label, 
			struct pvr_sect_ctl_list *head){
	
	int num = count_pvr_section_ctl_list(head);
	
	if(num == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_pvr_section_ctl_c(fp, level, label, head->pvr_sect_c);
		head = head->_next;
		fprintf(fp, "!\n");
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
