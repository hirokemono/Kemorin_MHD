/*
//  t_control_data_pvr_section_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/09.
*/

#include "t_control_data_pvr_section_list.h"

struct pvr_section_ctl_c * init_pvr_section_ctl_c(){
	int i;
    struct pvr_section_ctl_c *pvr_sect_c;
    if((pvr_sect_c = (struct pvr_section_ctl_c *) malloc(sizeof(struct pvr_section_ctl_c))) == NULL) {
        printf("malloc error for pvr_section_ctl_c \n");
        exit(0);
    }
	
	pvr_sect_c->iflag_psf_define_ctl = 0;
	pvr_sect_c->fname_sect_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	
    pvr_sect_c->label_psf_ctl = init_label_psf_ctl();
	pvr_sect_c->label_pvr_section = init_label_pvr_section();
	
	pvr_sect_c->psf_def_c = init_psf_define_ctl_c();
	pvr_sect_c->opacity_ctl = init_real_ctl_item_c();
	
	return pvr_sect_c;
};

void dealloc_pvr_section_ctl_c(struct pvr_section_ctl_c *pvr_sect_c){
    dealloc_control_labels_f(pvr_sect_c->label_psf_ctl);
	dealloc_control_labels_f(pvr_sect_c->label_pvr_section);
	dealloc_psf_define_ctl_c(pvr_sect_c->psf_def_c);
	free(pvr_sect_c->opacity_ctl);
	free(pvr_sect_c->fname_sect_ctl);
    free(pvr_sect_c);
	return;
};

int read_pvr_section_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct pvr_section_ctl_c *pvr_sect_c){
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_flag_c(buf, label) == 0){
		read_real_ctl_item_c(buf, pvr_sect_c->label_pvr_section->label[ 1],
							 pvr_sect_c->opacity_ctl);
		
		if(right_begin_flag_c(buf, pvr_sect_c->label_pvr_section->label[ 0]) > 0){
			pvr_sect_c->iflag_psf_define_ctl
					= read_psf_define_ctl_c(fp, buf, pvr_sect_c->label_pvr_section->label[0],
											pvr_sect_c->psf_def_c);
		} else if(right_file_flag_c(buf, pvr_sect_c->label_pvr_section->label[ 0])){
			pvr_sect_c->iflag_psf_define_ctl = read_file_flag_c(buf, pvr_sect_c->fname_sect_ctl);
		};
		
		skip_comment_read_line(fp, buf);
	};
	return 1;
};

int write_pvr_section_ctl_c(FILE *fp, int level, const char *label, 
			struct pvr_section_ctl_c *pvr_sect_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_real_ctl_item_c(fp, level, pvr_sect_c->label_pvr_section->maxlen,
						  pvr_sect_c->label_pvr_section->label[ 1],
						  pvr_sect_c->opacity_ctl);
	
	if(pvr_sect_c->iflag_psf_define_ctl == 1){
		fprintf(fp, "!\n");
		level = write_psf_define_ctl_c(fp, level, pvr_sect_c->label_pvr_section->label[ 0], 
									   pvr_sect_c->psf_def_c);
	} else if(pvr_sect_c->iflag_psf_define_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, pvr_sect_c->label_pvr_section->label[ 0], 
								  pvr_sect_c->fname_sect_ctl);
	};
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void read_pvr_section_subfile_c(char buf[LENGTHBUF], struct pvr_section_ctl_c *pvr_sect_c){
	if(pvr_sect_c->iflag_psf_define_ctl == -1){
		read_psf_define_file_c(pvr_sect_c->fname_sect_ctl, buf, pvr_sect_c->label_psf_ctl,
                               pvr_sect_c->psf_def_c);
	};
 	return;
};

void write_pvr_section_subfile_c(struct pvr_section_ctl_c *pvr_sect_c){
	if(pvr_sect_c->iflag_psf_define_ctl == -1){
		write_psf_define_file_c(pvr_sect_c->fname_sect_ctl, pvr_sect_c->label_psf_ctl,
                                pvr_sect_c->psf_def_c);
	};
 	return;
};



void init_pvr_section_ctl_list(struct pvr_sect_ctl_list *head){
    head->pvr_sect_c = NULL;
	head->_prev = NULL;
	head->_next = NULL;
	return;
};


struct pvr_sect_ctl_list *add_pvr_section_ctl_list_after(struct pvr_sect_ctl_list *current){
	struct pvr_sect_ctl_list *added;
	struct pvr_sect_ctl_list *old_next;
	
	if ((added = (struct pvr_sect_ctl_list *) malloc(sizeof(struct pvr_sect_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
	added->pvr_sect_c = init_pvr_section_ctl_c();
	
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
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};
void clear_pvr_section_ctl_list(struct pvr_sect_ctl_list *head){
	while (head->_next != NULL) {
		delete_pvr_section_ctl_list(head->_next);
	}
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
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->pvr_sect_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		head = add_pvr_section_ctl_list_after(head);
		iflag = read_pvr_section_ctl_c(fp, buf, label, head->pvr_sect_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_pvr_section_ctl_list(FILE *fp, int level, const char *label, 
			struct pvr_sect_ctl_list *head){
	if(count_pvr_section_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_pvr_section_ctl_c(fp, level, label, head->pvr_sect_c);
		head = head->_next;
		fprintf(fp, "!\n");
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
