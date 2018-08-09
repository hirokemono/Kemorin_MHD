/*
//  t_control_data_LIC_ctl_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_LIC_ctl_list.h"


void alloc_LIC_rendering_ctl_c(struct LIC_rendering_ctl_c *lic_render_c){
	lic_render_c->iflag_lic_pvr_ctl = 0;
	lic_render_c->fname_lic_pvr_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	lic_render_c->lic_pvr_c = (struct LIC_pvr_ctl_c *) malloc(sizeof(struct LIC_pvr_ctl_c));
	alloc_LIC_pvr_ctl_c(lic_render_c->lic_pvr_c);
	return;

};

void dealloc_LIC_rendering_ctl_c(struct LIC_rendering_ctl_c *lic_render_c){
	dealloc_LIC_pvr_ctl_c(lic_render_c->lic_pvr_c);
	free(lic_render_c->lic_pvr_c);
	free(lic_render_c->fname_lic_pvr_ctl);
	lic_render_c->iflag_lic_pvr_ctl = 0;
	return;
};

int read_LIC_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_rendering_ctl_c *lic_render_c){
	
	if(right_begin_flag_c(buf, label) > 0){
		lic_render_c->iflag_lic_pvr_ctl = read_LIC_pvr_ctl_c(fp, buf, label, lic_render_c->lic_pvr_c);
	} else if(right_file_flag_c(buf, label)){
		lic_render_c->iflag_lic_pvr_ctl = read_file_flag_c(buf, lic_render_c->fname_lic_pvr_ctl);
	} else {
		lic_render_c->iflag_lic_pvr_ctl = 0;
	};
	return abs(lic_render_c->iflag_lic_pvr_ctl);
};

int write_LIC_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_rendering_ctl_c *lic_render_c){
	
	if(lic_render_c->iflag_lic_pvr_ctl == 1){
		level = write_LIC_pvr_ctl_c(fp, level, label, lic_render_c->lic_pvr_c);
		fprintf(fp, "!\n");
	} else if(lic_render_c->iflag_lic_pvr_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, label, lic_render_c->fname_lic_pvr_ctl);
	};
	return level;
};

void read_LIC_rendering_ctl_file_c(char buf[LENGTHBUF], struct LIC_rendering_ctl_c *lic_render_c){
	if(lic_render_c->iflag_lic_pvr_ctl == -1){
		read_LIC_pvr_ctl_file_c(lic_render_c->fname_lic_pvr_ctl, buf, lic_render_c->lic_pvr_c);
	};
 	return;
};

void write_LIC_rendering_ctl_file_c(struct LIC_rendering_ctl_c *lic_render_c){
	if(lic_render_c->iflag_lic_pvr_ctl == -1){
		write_LIC_pvr_ctl_file_c(lic_render_c->fname_lic_pvr_ctl, lic_render_c->lic_pvr_c);
	};
 	return;
};



void init_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

void clear_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
		dealloc_LIC_rendering_ctl_c(head->lic_render_c);
		free(head);
		head = head->_next;
	}
	return;
};

struct LIC_PVR_ctl_list *add_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current){
	struct LIC_PVR_ctl_list *added;
	struct LIC_PVR_ctl_list *old_next;
	
	if ((added = (struct LIC_PVR_ctl_list *) malloc(sizeof(struct LIC_PVR_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->lic_render_c = (struct LIC_rendering_ctl_c *) malloc(sizeof(struct LIC_rendering_ctl_c))) == NULL) {
        printf("malloc error for lic_render_c\n");
        exit(0);
    }
	alloc_LIC_rendering_ctl_c(added->lic_render_c);
	
	/* replace from  current -> p2　to current -> p1 -> p2 */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current){
    struct LIC_PVR_ctl_list *old_prev = current->_prev;
	struct LIC_PVR_ctl_list *old_next = current->_next;
	
	dealloc_LIC_rendering_ctl_c(current->lic_render_c);
    free(current->lic_render_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};

int count_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct LIC_PVR_ctl_list *set_LIC_PVR_ctl_list_pointer(int index, struct LIC_PVR_ctl_list *head){
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


void rename_LIC_PVR_subfile_list(struct LIC_PVR_ctl_list *head){
    head = head->_next;
	while (head != NULL){
        if(head->lic_render_c->iflag_lic_pvr_ctl == -1){
            strcat(head->lic_render_c->fname_lic_pvr_ctl, "_2");
        };
		rename_LIC_pvr_ctl_subfiles(head->lic_render_c->lic_pvr_c);
		head = head->_next;
	};
	return;
};

void read_LIC_PVR_subfile_list(char buf[LENGTHBUF], struct LIC_PVR_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		read_LIC_rendering_ctl_file_c(buf, head->lic_render_c);
		head = head->_next;
	};
	return;
};

void write_LIC_PVR_subfile_list(struct LIC_PVR_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		write_LIC_rendering_ctl_file_c(head->lic_render_c);
		head = head->_next;
	};
	return;
};

int read_LIC_PVR_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct LIC_PVR_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	int num_array = 0;
	
	iflag = find_control_array_flag_c(buf, label, &num_array);
	if(iflag == 0) return iflag;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
		head = add_LIC_PVR_ctl_list(head);
		iflag = read_LIC_rendering_ctl_c(fp, buf, label, head->lic_render_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	
	if(num_array /= icou+1){
		printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
	};
	return icou;
};

int write_LIC_PVR_ctl_list(FILE *fp, int level, const char *label, 
			struct LIC_PVR_ctl_list *head){
	
	int num = count_LIC_PVR_ctl_list(head);
	
	if(num == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_LIC_rendering_ctl_c(fp, level, label, head->lic_render_c);
		head = head->_next;
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
