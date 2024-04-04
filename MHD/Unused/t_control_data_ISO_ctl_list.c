/*
//  t_control_data_ISO_ctl_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_ISO_ctl_list.h"


struct isosurface_ctl_c * init_isosurface_ctl_c(){
    struct isosurface_ctl_c *isosurfs_c;
    if ((isosurfs_c = (struct isosurface_ctl_c *) malloc(sizeof(struct isosurface_ctl_c))) == NULL) {
        printf("malloc error for isosurface_ctl_c\n");
        exit(0);
    }
	isosurfs_c->iflag_iso_ctl = 0;
	isosurfs_c->fname_iso_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	isosurfs_c->iso_c = init_iso_ctl_c();
	return isosurfs_c;
};

void dealloc_isosurface_ctl_c(struct isosurface_ctl_c *isosurfs_c){
	dealloc_iso_ctl_c(isosurfs_c->iso_c);
	free(isosurfs_c->fname_iso_ctl);
    free(isosurfs_c);
	return;
};

int read_isosurface_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct isosurface_ctl_c *isosurfs_c){
	
	if(right_begin_flag_c(buf, label) > 0){
		isosurfs_c->iflag_iso_ctl = read_iso_ctl_c(fp, buf, label, isosurfs_c->iso_c);
	} else if(right_file_flag_c(buf, label)){
		isosurfs_c->iflag_iso_ctl = read_file_flag_c(buf, isosurfs_c->fname_iso_ctl);
	} else {
		isosurfs_c->iflag_iso_ctl = 0;
	};
	return abs(isosurfs_c->iflag_iso_ctl);
};

int write_isosurface_ctl_c(FILE *fp, int level, const char *label, 
			struct isosurface_ctl_c *isosurfs_c){
	
	if(isosurfs_c->iflag_iso_ctl == 1){
		level = write_iso_ctl_c(fp, level, label, isosurfs_c->iso_c);
		fprintf(fp, "!\n");
	} else if(isosurfs_c->iflag_iso_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, label, isosurfs_c->fname_iso_ctl);
	};
	return level;
};

void read_isosurface_ctl_file_c(char buf[LENGTHBUF], struct isosurface_ctl_c *isosurfs_c){
	if(isosurfs_c->iflag_iso_ctl == -1){
		read_iso_ctl_file_c(isosurfs_c->fname_iso_ctl, buf, isosurfs_c->iso_c);
	};
 	return;
};

void write_isosurface_ctl_file_c(struct isosurface_ctl_c *isosurfs_c){
	if(isosurfs_c->iflag_iso_ctl == -1){
		write_iso_ctl_file_c(isosurfs_c->fname_iso_ctl, isosurfs_c->iso_c);
	};
 	return;
};



void init_ISO_ctl_list(struct ISO_ctl_list *head){
    head->isosurfs_c = NULL;
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

struct ISO_ctl_list *add_ISO_ctl_list_after(struct ISO_ctl_list *current){
	struct ISO_ctl_list *added;
	struct ISO_ctl_list *old_next;
	
	if ((added = (struct ISO_ctl_list *) malloc(sizeof(struct ISO_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
	added->isosurfs_c = init_isosurface_ctl_c();
	
	/* replace from  current -> next to current -> new -> next */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_ISO_ctl_list(struct ISO_ctl_list *current){
    struct ISO_ctl_list *old_prev = current->_prev;
	struct ISO_ctl_list *old_next = current->_next;
	
	dealloc_isosurface_ctl_c(current->isosurfs_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};
void clear_ISO_ctl_list(struct ISO_ctl_list *head){
	while (head->_next != NULL) {
		delete_ISO_ctl_list(head->_next);
	}
	return;
};


int count_ISO_ctl_list(struct ISO_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct ISO_ctl_list *set_ISO_ctl_list_pointer(int index, struct ISO_ctl_list *head){
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

void rename_ISO_subfile_list(struct ISO_ctl_list *head){
    head = head->_next;
	while (head != NULL){
        if(head->isosurfs_c->iflag_iso_ctl == -1){
            strcat(head->isosurfs_c->fname_iso_ctl, "_2");
        };
		head = head->_next;
	};
	return;
};

void read_ISO_subfile_list(char buf[LENGTHBUF], struct ISO_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		read_isosurface_ctl_file_c(buf, head->isosurfs_c);
		head = head->_next;
	};
	return;
};

void write_ISO_subfile_list(struct ISO_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		write_isosurface_ctl_file_c(head->isosurfs_c);
		head = head->_next;
	};
	return;
};

int read_ISO_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct ISO_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	
    if(find_control_array_flag_c(buf, label) == 0) return 0;
    if(head->isosurfs_c != NULL) return 0;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label) == 0){
		head = add_ISO_ctl_list_after(head);
		iflag = read_isosurface_ctl_c(fp, buf, label, head->isosurfs_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_ISO_ctl_list(FILE *fp, int level, const char *label, 
			struct ISO_ctl_list *head){
	if(count_ISO_ctl_list(head) == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_isosurface_ctl_c(fp, level, label, head->isosurfs_c);
		head = head->_next;
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
