/*
//  t_control_data_PVR_ctl_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_PVR_ctl_list.h"


void alloc_volume_rendering_ctl_c(struct volume_rendering_ctl_c *v_render_c){
	v_render_c->iflag_pvr_ctl = 0;
	v_render_c->fname_pvr_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	v_render_c->pvr_c = (struct pvr_ctl_c *) malloc(sizeof(struct pvr_ctl_c));
	alloc_pvr_ctl_c(v_render_c->pvr_c);
	return;

};

void dealloc_volume_rendering_ctl_c(struct volume_rendering_ctl_c *v_render_c){
	dealloc_pvr_ctl_c(v_render_c->pvr_c);
	free(v_render_c->pvr_c);
	free(v_render_c->fname_pvr_ctl);
	v_render_c->iflag_pvr_ctl = 0;
	return;
};

int read_volume_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct volume_rendering_ctl_c *v_render_c){
	
	if(right_begin_flag_c(buf, label) > 0){
		v_render_c->iflag_pvr_ctl = read_pvr_ctl_c(fp, buf, label, v_render_c->pvr_c);
	} else if(right_file_flag_c(buf, label)){
		v_render_c->iflag_pvr_ctl = read_file_flag_c(buf, v_render_c->fname_pvr_ctl);
	} else {
		v_render_c->iflag_pvr_ctl = 0;
	};
	return abs(v_render_c->iflag_pvr_ctl);
};

int write_volume_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct volume_rendering_ctl_c *v_render_c){
	
	if(v_render_c->iflag_pvr_ctl == 1){
		level = write_pvr_ctl_c(fp, level, label, v_render_c->pvr_c);
		fprintf(fp, "!\n");
	} else if(v_render_c->iflag_pvr_ctl == -1){
		write_file_flag_for_ctl_c(fp, level, label, v_render_c->fname_pvr_ctl);
	};
	return level;
};

void read_volume_rendering_ctl_file_c(char buf[LENGTHBUF], struct volume_rendering_ctl_c *v_render_c){
	if(v_render_c->iflag_pvr_ctl == -1){
		read_pvr_ctl_file_c(v_render_c->fname_pvr_ctl, buf, v_render_c->pvr_c);
	};
 	return;
};

void write_volume_rendering_ctl_file_c(struct volume_rendering_ctl_c *v_render_c){
	if(v_render_c->iflag_pvr_ctl == -1){
		write_pvr_ctl_file_c(v_render_c->fname_pvr_ctl, v_render_c->pvr_c);
	};
 	return;
};



void init_PVR_ctl_list(struct PVR_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

void clear_PVR_ctl_list(struct PVR_ctl_list *head){
    head =  head->_next;
    while (head != NULL) {
		dealloc_volume_rendering_ctl_c(head->v_render_c);
		free(head);
		head =  head->_next;
	}
	return;
};

struct PVR_ctl_list *add_PVR_ctl_list(struct PVR_ctl_list *current){
	struct PVR_ctl_list *added;
	struct PVR_ctl_list *old_next;
	
	if ((added = (struct PVR_ctl_list *) malloc(sizeof(struct PVR_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
    if ((added->v_render_c = (struct volume_rendering_ctl_c *) malloc(sizeof(struct volume_rendering_ctl_c))) == NULL) {
        printf("malloc error for v_render_c\n");
        exit(0);
    }
	alloc_volume_rendering_ctl_c(added->v_render_c);
	
	/* replace from  current -> p2　to current -> p1 -> p2 */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_PVR_ctl_list(struct PVR_ctl_list *current){
    struct PVR_ctl_list *old_prev = current->_prev;
	struct PVR_ctl_list *old_next = current->_next;
	
	dealloc_volume_rendering_ctl_c(current->v_render_c);
    free(current->v_render_c);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};

int count_PVR_ctl_list(struct PVR_ctl_list *head){
	int num = 0;
	head = head->_next;
	while (head != NULL){
		head = head->_next;
		num = num + 1;
	};
	return num;
};

struct PVR_ctl_list *set_PVR_ctl_list_pointer(int index, struct PVR_ctl_list *head){
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


void rename_PVR_subfile_list(struct PVR_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		strcat(head->v_render_c->fname_pvr_ctl, "_2");
		rename_pvr_ctl_subfiles(head->v_render_c->pvr_c);
		head = head->_next;
	};
	return;
};

void read_PVR_subfile_list(char buf[LENGTHBUF], struct PVR_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		read_volume_rendering_ctl_file_c(buf, head->v_render_c);
		head = head->_next;
	};
	return;
};

void write_PVR_subfile_list(struct PVR_ctl_list *head){
    head = head->_next;
	while (head != NULL){
		write_volume_rendering_ctl_file_c(head->v_render_c);
		head = head->_next;
	};
	return;
};

int read_PVR_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct PVR_ctl_list *head){
	int iflag = 0;
	int icou = 0;
	int num_array;
	
	iflag = find_control_array_flag_c(buf, label, &num_array);
	if(iflag == 0) return iflag;
	
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
		head = add_PVR_ctl_list(head);
		iflag = read_volume_rendering_ctl_c(fp, buf, label, head->v_render_c);
		icou = icou + iflag;
		skip_comment_read_line(fp, buf);
	};
	
	if(num_array /= icou+1){
		printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
	};
	return icou;
};

int write_PVR_ctl_list(FILE *fp, int level, const char *label, 
			struct PVR_ctl_list *head){
	
	int num = count_PVR_ctl_list(head);
	
	if(num == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
	
	while (head != NULL) {	/* Go through null pointer*/
		level = write_volume_rendering_ctl_c(fp, level, label, head->v_render_c);
		head = head->_next;
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
