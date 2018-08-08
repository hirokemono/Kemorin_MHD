/*
//  t_control_data_lic_ctl_list.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_lic_ctl_list.h"


void alloc_LIC_rendering_ctl_c(struct LIC_rendering_ctl_c *lic_render_c){
	lic_render_c->iflag_lic_pvr_ctl = 0;
	lic_render_c->fname_lic_pvr_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	lic_render_c->lic_pvr_c = (struct LIC_pvr_ctl_c *) malloc(sizeof(struct LIC_pvr_ctl_c));
	alloc_LIC_pvr_ctl_c(lic_render_c->lic_pvr_c);
	return;

};

void dealloc_LIC_renderingctl_c(struct LIC_rendering_ctl_c *lic_render_c){
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



int init_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head){
	
	head->_prev = NULL;
	head->_next = NULL;
	return;
};

int clear_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *head){
	LIC_PVR_ctl_list *p2;
	
    while (head != NULL) {     /* 次ポインタがNULLまで処理 */
		
		p2 = head->_next;
		
		dealloc_LIC_rendering_ctl_c(head);
		free(head);
		head = p2;
    }
	return;
};

LIC_PVR_ctl_list *add_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current){
	LIC_PVR_ctl_list *added;
	LIC_PVR_ctl_list *old_next;
	
	if ((added = (LIC_PVR_ctl_list *) malloc(sizeof(LIC_PVR_ctl_list))) == NULL) {
	printf("malloc error\n");
	exit(0);
	}
	alloc_LIC_rendering_ctl_c(added);
	
	/* replace from  current -> p2　to current -> p1 -> p2 */
	old_next= current->_next;
	current->_next = added;
	added->_next = old_next;		
	if (old_next != NULL) old_next->_prev = added;
	added->_prev = current;
	
	return added;
};

void delete_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current){
    cell_t *old_prev = current->_prev;
	cell_t *old_next = current->_next;
	
	dealloc_LIC_renderingctl_c(current);
	free(current);
	
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
	return;
};

int count_LIC_PVR_ctl_list(struct LIC_PVR_ctl_list *current){
	int num = 0;
    while (current != NULL) num = num + 1;
	return num;
};

int read_LIC_PVR_ctl_list(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_PVR_ctl_list *current){
	
	
    if(viz_c->num_LIC_renderings_ctl == 0) return 0;
	skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, viz_c->num_LIC_renderings_ctl, icou) == 0){
		current = add_LIC_PVR_ctl_list(current);
		iflag = read_LIC_rendering_ctl_c(fp, buf, label, current->lic_render_c);
		icou = icou + iflag;
	};
	fprintf(fp, "!\n");
	return icou;
};

int write_LIC_PVR_ctl_list(FILE *fp, int level, const char *label, 
			struct LIC_PVR_ctl_list *current){
	
	int num = count_LIC_PVR_ctl_list(current);
	
	if(num == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, num);
	
	while (current != NULL) {	/* Go through null pointer*/
		level = write_LIC_rendering_ctl_c(fp, level, label, current->lic_render_c);
		fprintf(fp, "!\n");
		current = current->next;
	}
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};
