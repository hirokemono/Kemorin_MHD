/*
//  t_control_data_FLINE_ctl_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#include "t_control_data_FLINE_ctl_list.h"


void alloc_fieldline_ctl_c(struct fieldline_ctl_c *fldlines_c){
	fldlines_c->iflag_fline_ctl = 0;
	fldlines_c->fname_fline_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	fldlines_c->fline_c = (struct fline_ctl_c *) malloc(sizeof(struct fline_ctl_c));
	alloc_fline_ctl_c(fldlines_c->fline_c);
	return;

};

void dealloc_fieldline_ctl_c(struct fieldline_ctl_c *fldlines_c){
	dealloc_fline_ctl_c(fldlines_c->fline_c);
	free(fldlines_c->fline_c);
	free(fldlines_c->fname_fline_ctl);
	fldlines_c->iflag_fline_ctl = 0;
	return;
};

int read_fieldline_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct fieldline_ctl_c *fldlines_c){
	
	if(right_begin_flag_c(buf, label) > 0){
		fldlines_c->iflag_fline_ctl = read_fline_ctl_c(fp, buf, label, fldlines_c->fline_c);
	} else if(right_file_flag_c(buf, label)){
		fldlines_c->iflag_fline_ctl = read_file_flag_c(buf, fldlines_c->fname_fline_ctl);
	} else {
		fldlines_c->iflag_fline_ctl = 0;
	};
	return abs(fldlines_c->iflag_fline_ctl);
};

int write_fieldline_ctl_c(FILE *fp, int level, const char *label, 
			struct fieldline_ctl_c *fldlines_c){
	
	if(fldlines_c->iflag_fline_ctl == 1){
		level = write_fline_ctl_c(fp, level, label, fldlines_c->fline_c);
		fprintf(fp, "!\n");
	} else if(fldlines_c->iflag_fline_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, label, fldlines_c->fname_fline_ctl);
	};
	return level;
};

void read_fieldline_ctl_file_c(char buf[LENGTHBUF], struct fieldline_ctl_c *fldlines_c){
	if(fldlines_c->iflag_fline_ctl == -1){
		read_fline_ctl_file_c(fldlines_c->fname_fline_ctl, buf, fldlines_c->fline_c);
	};
 	return;
};

void write_fieldline_ctl_file_c(struct fieldline_ctl_c *fldlines_c){
	if(fldlines_c->iflag_fline_ctl == -1){
		write_fline_ctl_file_c(fldlines_c->fname_fline_ctl, fldlines_c->fline_c);
	};
 	return;
};



void init_FLINE_ctl_list(struct FLINE_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

void clear_FLINE_ctl_list(struct FLINE_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
		dealloc_fieldline_ctl_c(head->fldlines_c);
		free(head);
        head = head->_next;
	}
	return;
};

struct FLINE_ctl_list *add_FLINE_ctl_list(struct FLINE_ctl_list *current){
	struct FLINE_ctl_list *added;
	struct FLINE_ctl_list *old_next;
	
	if ((added = (struct FLINE_ctl_list *) malloc(sizeof(struct FLINE_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->fldlines_c = (struct fieldline_ctl_c *) malloc(sizeof(struct fieldline_ctl_c))) == NULL) {
        printf("malloc error for fldlines_c\n");
        exit(0);
    }
	alloc_fieldline_ctl_c(added->fldlines_c);
	
	/* replace from  current -> p2　to current -> p1 -> p2 */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_FLINE_ctl_list(struct FLINE_ctl_list *current){
    struct FLINE_ctl_list *old_prev = current->_prev;
	struct FLINE_ctl_list *old_next = current->_next;
	
	dealloc_fieldline_ctl_c(current->fldlines_c);
    free(current->fldlines_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};

int count_FLINE_ctl_list(struct FLINE_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct FLINE_ctl_list *set_FLINE_ctl_list_pointer(int index, struct FLINE_ctl_list *head){
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

void rename_FLINE_subfile_list(struct FLINE_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		if(head->fldlines_c->iflag_fline_ctl == -1){
			strcat(head->fldlines_c->fname_fline_ctl, "_2");
		};
		head = head->_next;
	};
	return;
};

void read_FLINE_subfile_list(char buf[LENGTHBUF], struct FLINE_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		read_fieldline_ctl_file_c(buf, head->fldlines_c);
		head = head->_next;
	};
	return;
};

void write_FLINE_subfile_list(struct FLINE_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		write_fieldline_ctl_file_c(head->fldlines_c);
		head = head->_next;
	};
	return;
};

int read_FLINE_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct FLINE_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	int num_array = 0;
	
	iflag = find_control_array_flag_c(buf, label, &num_array);
	if(iflag == 0) return iflag;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
		head = add_FLINE_ctl_list(head);
		iflag = read_fieldline_ctl_c(fp, buf, label, head->fldlines_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	
	if(num_array /= icou+1){
		printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
	};
	return icou;
};

int write_FLINE_ctl_list(FILE *fp, int level, const char *label, 
			struct FLINE_ctl_list *head){
	
	int num = count_FLINE_ctl_list(head);
	
	if(num == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_fieldline_ctl_c(fp, level, label, head->fldlines_c);
		head = head->_next;
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
