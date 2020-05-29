/*
//  t_control_data_PSF_ctl_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_PSF_ctl_list.h"


void alloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c){
	sections_c->iflag_psf_ctl = 0;
	sections_c->fname_psf_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	sections_c->psf_c = init_psf_ctl_c();
	return;

};

void dealloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c){
	dealloc_psf_ctl_c(sections_c->psf_c);
	free(sections_c->fname_psf_ctl);
	sections_c->iflag_psf_ctl = 0;
	return;
};

int read_sectioning_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct sectioning_ctl_c *sections_c){
	
	if(right_begin_flag_c(buf, label) > 0){
		sections_c->iflag_psf_ctl = read_psf_ctl_c(fp, buf, label, sections_c->psf_c);
	} else if(right_file_flag_c(buf, label)){
		sections_c->iflag_psf_ctl = read_file_flag_c(buf, sections_c->fname_psf_ctl);
	} else {
		sections_c->iflag_psf_ctl = 0;
	};
	return abs(sections_c->iflag_psf_ctl);
};

int write_sectioning_ctl_c(FILE *fp, int level, const char *label, 
			struct sectioning_ctl_c *sections_c){
	
	if(sections_c->iflag_psf_ctl == 1){
		level = write_psf_ctl_c(fp, level, label, sections_c->psf_c);
		fprintf(fp, "!\n");
	} else if(sections_c->iflag_psf_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, label, sections_c->fname_psf_ctl);
	};
	return level;
};

void read_sectioning_ctl_file_c(char buf[LENGTHBUF], struct sectioning_ctl_c *sections_c){
	if(sections_c->iflag_psf_ctl == -1){
		read_psf_ctl_file_c(sections_c->fname_psf_ctl, buf, sections_c->psf_c);
	};
 	return;
};

void write_sectioning_ctl_file_c(struct sectioning_ctl_c *sections_c){
	if(sections_c->iflag_psf_ctl == -1){
		write_psf_ctl_file_c(sections_c->fname_psf_ctl, sections_c->psf_c);
	};
 	return;
};



void init_PSF_ctl_list(struct PSF_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

struct PSF_ctl_list *add_PSF_ctl_list_after(struct PSF_ctl_list *current){
	struct PSF_ctl_list *added;
	struct PSF_ctl_list *old_next;
	
	if ((added = (struct PSF_ctl_list *) malloc(sizeof(struct PSF_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->sections_c = (struct sectioning_ctl_c *) malloc(sizeof(struct sectioning_ctl_c))) == NULL) {
        printf("malloc error for sections_c\n");
        exit(0);
    }
	alloc_sectioning_ctl_c(added->sections_c);
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_PSF_ctl_list(struct PSF_ctl_list *current){
    struct PSF_ctl_list *old_prev = current->_prev;
	struct PSF_ctl_list *old_next = current->_next;
	
	dealloc_sectioning_ctl_c(current->sections_c);
    free(current->sections_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};
void clear_PSF_ctl_list(struct PSF_ctl_list *head){
	while (head->_next != NULL) {
		delete_PSF_ctl_list(head->_next);
	}
	return;
};


int count_PSF_ctl_list(struct PSF_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct PSF_ctl_list *set_PSF_ctl_list_pointer(int index, struct PSF_ctl_list *head){
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

void rename_PSF_subfile_list(struct PSF_ctl_list *head){
    head = head->_next;
	while (head != NULL){
        if(head->sections_c->iflag_psf_ctl == -1){
            strcat(head->sections_c->fname_psf_ctl, "_2");
        };
		rename_psf_define_file_c(head->sections_c->psf_c);
		head = head->_next;
	};
	return;
};

void read_PSF_subfile_list(char buf[LENGTHBUF], struct PSF_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		read_sectioning_ctl_file_c(buf, head->sections_c);
		head = head->_next;
	};
	return;
};

void write_PSF_subfile_list(struct PSF_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		write_sectioning_ctl_file_c(head->sections_c);
		head = head->_next;
	};
	return;
};

int read_PSF_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct PSF_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->sections_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		head = add_PSF_ctl_list_after(head);
		iflag = read_sectioning_ctl_c(fp, buf, label, head->sections_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_PSF_ctl_list(FILE *fp, int level, const char *label, 
			struct PSF_ctl_list *head){
	if(count_PSF_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_sectioning_ctl_c(fp, level, label, head->sections_c);
		head = head->_next;
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
