/*
//  t_control_data_4_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_pvr_c.h"

FILE *FP_PVR;

const char label_pvr_head[KCHARA_C] = "volume_rendering";


struct pvr_plot_area_ctl_c * init_pvr_plot_area_ctl_c(){
	int i;
    struct pvr_plot_area_ctl_c *area_c;
    if((area_c = (struct pvr_plot_area_ctl_c *) malloc(sizeof(struct pvr_plot_area_ctl_c))) == NULL) {
        printf("malloc error for pvr_plot_area_ctl_c \n");
        exit(0);
    }
	
    area_c->iflag_use = 0;
	area_c->label_pvr_area =   init_label_pvr_area();
    area_c->pvr_area_list =    init_chara_clist();
    area_c->surf_enhanse_ctl = init_chara2_real_clist();
    
    sprintf(area_c->surf_enhanse_ctl->c1_name, "Group_name");
    sprintf(area_c->surf_enhanse_ctl->c2_name, "Direction");
    sprintf(area_c->surf_enhanse_ctl->r1_name, "Opacity");
	
	return area_c;
};

void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c){
	
	dealloc_control_labels_f(area_c->label_pvr_area);
	dealloc_chara_clist(area_c->pvr_area_list);
	dealloc_chara2_real_clist(area_c->surf_enhanse_ctl);
	free(area_c);
	return;
};

void read_pvr_plot_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_plot_area_ctl_c *area_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_clist(fp, buf, area_c->label_pvr_area->label[0], 
						 area_c->pvr_area_list);
		read_chara2_real_clist(fp, buf, area_c->label_pvr_area->label[1], 
							   area_c->surf_enhanse_ctl);
	};
	area_c->iflag_use = 1;
    return;
};

int write_pvr_plot_area_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_plot_area_ctl_c *area_c){
    if(area_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_chara_clist(fp, level, area_c->label_pvr_area->label[ 0], 
							  area_c->pvr_area_list);
	level = write_chara2_real_clist(fp, level, area_c->label_pvr_area->label[ 1], 
									area_c->surf_enhanse_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct pvr_ctl_c * init_pvr_ctl_c(){
	int i;
    struct pvr_ctl_c *pvr_c;
    if((pvr_c = (struct pvr_ctl_c *) malloc(sizeof(struct pvr_ctl_c))) == NULL) {
        printf("malloc error for pvr_ctl_c \n");
        exit(0);
    }
	
    pvr_c->iflag_use = 0;
	pvr_c->label_pvr_ctl_w_dpl = init_label_pvr_ctl_w_dpl();
	pvr_c->updated_ctl = init_chara_ctl_item_c();
	
	pvr_c->file_head_ctl = init_chara_ctl_item_c();
	pvr_c->file_fmt_ctl = init_chara_ctl_item_c();
	pvr_c->monitoring_ctl = init_chara_ctl_item_c();
	pvr_c->transparent_ctl = init_chara_ctl_item_c();
	
	pvr_c->streo_ctl = init_chara_ctl_item_c();
	pvr_c->anaglyph_ctl = init_chara_ctl_item_c();
	
	pvr_c->pvr_field_ctl = init_chara_ctl_item_c();
	pvr_c->pvr_comp_ctl = init_chara_ctl_item_c();
	
    pvr_c->maxpe_composit_ctl = init_int_ctl_item_c();

	pvr_c->area_c = init_pvr_plot_area_ctl_c();
	
    pvr_c->pvr_modelview_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	pvr_c->mat_c = init_modelview_ctl_c();
	
	pvr_c->light_c = init_lighting_ctl_c();
	
    pvr_c->pvr_colormap_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	pvr_c->cmap_cbar_c = init_colormap_colorbar_ctl_c();
	
	pvr_c->movie_c = init_pvr_movie_ctl_c();
	
	init_pvr_section_ctl_list(&pvr_c->pvr_sect_c_list);
	init_pvr_iso_ctl_list(&pvr_c->pvr_iso_c_list);
	
	return pvr_c;
};

void dealloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c){
    dealloc_control_labels_f(pvr_c->label_pvr_ctl_w_dpl);
	
	dealloc_chara_ctl_item_c(pvr_c->updated_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->file_head_ctl);
	dealloc_chara_ctl_item_c(pvr_c->file_fmt_ctl);
	dealloc_chara_ctl_item_c(pvr_c->monitoring_ctl);
	dealloc_chara_ctl_item_c(pvr_c->transparent_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->streo_ctl);
	dealloc_chara_ctl_item_c(pvr_c->anaglyph_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->pvr_field_ctl);
	dealloc_chara_ctl_item_c(pvr_c->pvr_comp_ctl);
    free(pvr_c->maxpe_composit_ctl);
	
    
	dealloc_pvr_plot_area_ctl_c(pvr_c->area_c);
	
	dealloc_modelview_ctl_c(pvr_c->mat_c);
	free(pvr_c->mat_c);
    free(pvr_c->pvr_modelview_file_name);
	
	dealloc_lighting_ctl_c(pvr_c->light_c);
	
	dealloc_colormap_colorbar_ctl_c(pvr_c->cmap_cbar_c);
    free(pvr_c->pvr_colormap_file_name);
	
	dealloc_pvr_movie_ctl_c(pvr_c->movie_c);
	
	clear_pvr_section_ctl_list(&pvr_c->pvr_sect_c_list);
	clear_pvr_iso_ctl_list(&pvr_c->pvr_iso_c_list);
		
    free(pvr_c);
	return;
};


void read_pvr_ctl_items(FILE *fp, char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c){
	int iflag;
	
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 0],
						  pvr_c->updated_ctl);
	
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 3],
						  pvr_c->monitoring_ctl);
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 4],
						  pvr_c->transparent_ctl);
	
	read_integer_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 5],
							pvr_c->maxpe_composit_ctl);
	
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 6],
						  pvr_c->streo_ctl);
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 7],
						  pvr_c->anaglyph_ctl);
	
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 8],
						  pvr_c->pvr_field_ctl);
	read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 9],
						  pvr_c->pvr_comp_ctl);
	if(right_begin_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[10]) > 0){
		read_pvr_plot_area_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[10],
								 pvr_c->area_c);
	};
	if(right_begin_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[11]) > 0){
		read_modelview_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[11],
							 pvr_c->mat_c);
	} else if(right_file_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[11])){
		pvr_c->mat_c->iflag_use 
				= read_file_flag_c(buf, pvr_c->pvr_modelview_file_name);
	};
	
	if(right_begin_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[14]) > 0){
		read_lighting_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[14],
							pvr_c->light_c);
	};
	
	if(right_file_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[12])){
		pvr_c->iflag_cmap_cbar_ctl = read_file_flag_c(buf, pvr_c->pvr_colormap_file_name);
	}else{
		read_colormap_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[12],
							pvr_c->cmap_cbar_c->cmap_c);
		read_colormap_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[13],
							pvr_c->cmap_cbar_c->cmap_c);
		
		read_colorbar_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[15],
							pvr_c->cmap_cbar_c->cbar_c);
	};
	
	
	iflag = read_pvr_section_ctl_list(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[16],
									  &pvr_c->pvr_sect_c_list);
	iflag = read_pvr_iso_ctl_list(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[17],
								  &pvr_c->pvr_iso_c_list);
	
	
	if(right_begin_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[18]) > 0){
		read_pvr_movie_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[18],
							 pvr_c->movie_c);
	};
	
	if(right_begin_flag_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[19]) > 0){
		read_pvr_movie_ctl_c(fp, buf, pvr_c->label_pvr_ctl_w_dpl->label[19],
							 pvr_c->movie_c);
	};
    pvr_c->iflag_use = 1;
	return;
};

int write_pvr_ctl_items(FILE *fp, int level, struct pvr_ctl_c *pvr_c){
    if(pvr_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen, 
								   pvr_c->label_pvr_ctl_w_dpl->label[ 0],
								   pvr_c->updated_ctl);
	
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen, 
								   pvr_c->label_pvr_ctl_w_dpl->label[ 3],
								   pvr_c->monitoring_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
								   pvr_c->label_pvr_ctl_w_dpl->label[ 4],
								   pvr_c->transparent_ctl);
	
	level = write_integer_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
									 pvr_c->label_pvr_ctl_w_dpl->label[ 5],
									 pvr_c->maxpe_composit_ctl);
	
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
								   pvr_c->label_pvr_ctl_w_dpl->label[ 6],
								   pvr_c->streo_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
								   pvr_c->label_pvr_ctl_w_dpl->label[ 7],
								   pvr_c->anaglyph_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
								   pvr_c->label_pvr_ctl_w_dpl->label[ 8],
								   pvr_c->pvr_field_ctl);
	level = write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
								   pvr_c->label_pvr_ctl_w_dpl->label[ 9],
								   pvr_c->pvr_comp_ctl);
    
	level = write_pvr_plot_area_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[10],
									  pvr_c->area_c);
	
	if(pvr_c->mat_c->iflag_use > 0){
		level = write_modelview_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[11],
									  pvr_c->mat_c);
	} else if(pvr_c->mat_c->iflag_use == -1){
		write_file_flag_for_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[11],
								  pvr_c->pvr_modelview_file_name);
	};
	
	level = write_lighting_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[14],
								 pvr_c->light_c);
	
	if(pvr_c->iflag_cmap_cbar_ctl == -1){
		fprintf(fp, "!\n");
		write_file_flag_for_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[12], 
								  pvr_c->pvr_colormap_file_name);
	}else{
		level = write_colormap_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[13],
									 pvr_c->cmap_cbar_c->cmap_c);
		level = write_colorbar_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[15],
									 pvr_c->cmap_cbar_c->cbar_c);
	};
	
	level = write_pvr_section_ctl_list(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[16],
									   &pvr_c->pvr_sect_c_list);
	level = write_pvr_iso_ctl_list(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[17],
								   &pvr_c->pvr_iso_c_list);
	
	level = write_pvr_movie_ctl_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->label[18],
								  pvr_c->movie_c);	
	return level;
};

void read_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_ctl_c *pvr_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 1],
							  pvr_c->file_head_ctl);
		read_chara_ctl_item_c(buf, pvr_c->label_pvr_ctl_w_dpl->label[ 2],
							  pvr_c->file_fmt_ctl);
        
		read_pvr_ctl_items(fp, buf, pvr_c);
	};
	return;
};

int write_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_ctl_c *pvr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen,
						   pvr_c->label_pvr_ctl_w_dpl->label[ 1], 
						   pvr_c->file_head_ctl);
	write_chara_ctl_item_c(fp, level, pvr_c->label_pvr_ctl_w_dpl->maxlen, 
						   pvr_c->label_pvr_ctl_w_dpl->label[ 2],
						   pvr_c->file_fmt_ctl);
    
	write_pvr_ctl_items(fp, level, pvr_c);
    
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void rename_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c){
    if(pvr_c->mat_c->iflag_use  ==-1){
        strcat(pvr_c->pvr_modelview_file_name, "_2");
    }
    if(pvr_c->iflag_cmap_cbar_ctl ==-1){
        strcat(pvr_c->pvr_colormap_file_name, "_2");
    }
	
	rename_pvr_section_subfile_list(&pvr_c->pvr_sect_c_list);
    return;
}

void read_pvr_ctl_subfiles(char buf[LENGTHBUF], struct pvr_ctl_c *pvr_c){
	if(pvr_c->mat_c->iflag_use ==-1){
		read_modelview_file_c(pvr_c->pvr_modelview_file_name, buf, pvr_c->mat_c);
	};
	if(pvr_c->cmap_cbar_c->iflag_use ==-1){
		read_colormap_file_c(pvr_c->pvr_colormap_file_name, buf, pvr_c->cmap_cbar_c);
	};
	
	read_pvr_section_subfile_list(buf, &pvr_c->pvr_sect_c_list);
	return;
};

void write_pvr_ctl_subfiles(struct pvr_ctl_c *pvr_c){
	if(pvr_c->mat_c->iflag_use ==-1){
		write_modelview_file_c(pvr_c->pvr_modelview_file_name, pvr_c->mat_c);
	};
	if(pvr_c->iflag_cmap_cbar_ctl ==-1){
		write_colormap_file_c(pvr_c->pvr_colormap_file_name, pvr_c->cmap_cbar_c);
	};
	
	write_pvr_section_subfile_list(&pvr_c->pvr_sect_c_list);
	return;
};

void read_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
						struct pvr_ctl_c *pvr_c){
	
	if ((FP_PVR = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);					/* terminate with error message */
	};
	
	skip_comment_read_line(FP_PVR, buf);
	if(right_begin_flag_c(buf, label_pvr_head) > 0){
		read_pvr_ctl_c(FP_PVR, buf, label_pvr_head, pvr_c);
	};
	fclose(FP_PVR);
	
	read_pvr_ctl_subfiles(buf, pvr_c);
	return;
};

int write_pvr_ctl_file_c(const char *file_name, struct pvr_ctl_c *pvr_c){
	int level;
	
	write_pvr_ctl_subfiles(pvr_c);
	
	if ((FP_PVR = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);					/* terminate with error message */
	};
	
	level = write_pvr_ctl_c(FP_PVR, 0, label_pvr_head, pvr_c);
	fclose(FP_PVR);
	
	return level;
};

