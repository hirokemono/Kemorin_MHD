/*
//  t_control_data_LIC_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/06.
*/

#include "t_control_data_LIC_pvr_c.h"

FILE *FP_LIC;


const char label_LIC_pvr_head[KCHARA_C] = "LIC_rendering";

struct LIC_pvr_ctl_c * init_LIC_pvr_ctl_c(){
    struct LIC_pvr_ctl_c *lic_pvr_c;
    if((lic_pvr_c = (struct LIC_pvr_ctl_c *) malloc(sizeof(struct LIC_pvr_ctl_c))) == NULL) {
        printf("malloc error for LIC_pvr_ctl_c \n");
        exit(0);
    }
    lic_pvr_c->label_lic_pvr = init_label_LIC_pvr_ctl_f();
	lic_pvr_c->pvr_c = init_pvr_ctl_c();

	lic_pvr_c->iflag_lic_ctl = 0;
	lic_pvr_c->lic_c = init_lic_ctl_c();
	return lic_pvr_c;
};

void dealloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	
    dealloc_control_labels_f(lic_pvr_c->label_lic_pvr);
	dealloc_pvr_ctl_c(lic_pvr_c->pvr_c);
	dealloc_lic_ctl_c(lic_pvr_c->lic_c);
	
    free(lic_pvr_c);
	return;
};

int read_LIC_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
					   struct LIC_pvr_ctl_c *lic_pvr_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 1],
							  lic_pvr_c->pvr_c->file_head_ctl);
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 2],
							  lic_pvr_c->pvr_c->file_fmt_ctl);
	
		if(right_begin_flag_c(buf, lic_pvr_c->label_lic_pvr->label[ 8]) > 0){
			lic_pvr_c->iflag_lic_ctl
					= read_lic_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[ 8], 
									 lic_pvr_c->lic_c);
		};
		
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 0],
							  lic_pvr_c->pvr_c->updated_ctl);
	
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 3],
							  lic_pvr_c->pvr_c->monitoring_ctl);
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 4],
							  lic_pvr_c->pvr_c->transparent_ctl);
	
		read_integer_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 5],
								lic_pvr_c->pvr_c->maxpe_composit_ctl);
	
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 6],
							  lic_pvr_c->pvr_c->streo_ctl);
		read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 7],
							  lic_pvr_c->pvr_c->anaglyph_ctl);
	
		if(right_begin_flag_c(buf, lic_pvr_c->label_lic_pvr->label[ 9]) > 0){
			read_pvr_plot_area_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[ 9],
									 lic_pvr_c->pvr_c->area_c);
		};
		if(right_begin_flag_c(buf, lic_pvr_c->label_lic_pvr->label[10]) > 0){
			read_modelview_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[10],
								 lic_pvr_c->pvr_c->mat_c);
		} else if(right_file_flag_c(buf, lic_pvr_c->label_lic_pvr->label[10])){
			lic_pvr_c->pvr_c->mat_c->iflag_use
					= read_file_flag_c(buf, lic_pvr_c->pvr_c->pvr_modelview_file_name);
		};
		
		if(right_begin_flag_c(buf, lic_pvr_c->label_lic_pvr->label[13]) > 0){
			read_lighting_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[13],
								lic_pvr_c->pvr_c->light_c);
		};
		
		if(right_file_flag_c(buf, lic_pvr_c->label_lic_pvr->label[11])){
			lic_pvr_c->pvr_c->iflag_cmap_cbar_ctl
					= read_file_flag_c(buf, lic_pvr_c->pvr_c->pvr_colormap_file_name);
		}else{
			read_colormap_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[11],
								lic_pvr_c->pvr_c->cmap_cbar_c->cmap_c);
			read_colormap_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[12],
							lic_pvr_c->pvr_c->cmap_cbar_c->cmap_c);
			
			read_colorbar_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[14],
								lic_pvr_c->pvr_c->cmap_cbar_c->cbar_c);
		};
		
		if(right_begin_flag_c(buf, lic_pvr_c->label_lic_pvr->label[17]) > 0){
			read_pvr_movie_ctl_c(fp, buf, lic_pvr_c->label_lic_pvr->label[17],
								 lic_pvr_c->pvr_c->movie_c);
		};
		
		iflag = read_pvr_section_ctl_list(fp, buf, lic_pvr_c->label_lic_pvr->label[15],
										  &lic_pvr_c->pvr_c->pvr_sect_c_list);
		iflag = read_pvr_iso_ctl_list(fp, buf, lic_pvr_c->label_lic_pvr->label[16],
									  &lic_pvr_c->pvr_c->pvr_iso_c_list);
		
		lic_pvr_c->pvr_c->iflag_use = 1;
	};
	return 1;
};

int write_LIC_pvr_ctl_c(FILE *fp, int level, const char *label, 
						struct LIC_pvr_ctl_c *lic_pvr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);

    write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, 
                           lic_pvr_c->label_lic_pvr->label[ 1], 
                           lic_pvr_c->pvr_c->file_head_ctl);
    write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, 
                           lic_pvr_c->label_lic_pvr->label[ 2], 
                           lic_pvr_c->pvr_c->file_fmt_ctl);
    
    if(lic_pvr_c->iflag_lic_ctl > 0){
        fprintf(fp, "!\n");
        write_lic_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[ 8], lic_pvr_c->lic_c);
    };
    
    fprintf(fp, "!\n");
	level = write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen,
								   lic_pvr_c->label_lic_pvr->label[ 0],
								   lic_pvr_c->pvr_c->updated_ctl);
	
	level = write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen,
								   lic_pvr_c->label_lic_pvr->label[ 3],
								   lic_pvr_c->pvr_c->monitoring_ctl);
	level = write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen,
								   lic_pvr_c->label_lic_pvr->label[ 4],
								   lic_pvr_c->pvr_c->transparent_ctl);
	
	level = write_integer_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen,
									 lic_pvr_c->label_lic_pvr->label[ 5],
									 lic_pvr_c->pvr_c->maxpe_composit_ctl);
	
	level = write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen,
								   lic_pvr_c->label_lic_pvr->label[ 6],
								   lic_pvr_c->pvr_c->streo_ctl);
	level = write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, 
								   lic_pvr_c->label_lic_pvr->label[ 7],
								   lic_pvr_c->pvr_c->anaglyph_ctl);
	
	level = write_pvr_plot_area_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[ 9],
									  lic_pvr_c->pvr_c->area_c);
	
	if(lic_pvr_c->pvr_c->mat_c->iflag_use > 0){
		level = write_modelview_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[10],
									  lic_pvr_c->pvr_c->mat_c);
	} else if(lic_pvr_c->pvr_c->mat_c->iflag_use == -1){
		write_file_flag_for_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[10], 
								  lic_pvr_c->pvr_c->pvr_modelview_file_name);
	};
	
	level = write_lighting_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[13], 
								 lic_pvr_c->pvr_c->light_c);
	
	if(lic_pvr_c->pvr_c->iflag_cmap_cbar_ctl == -1){
		fprintf(fp, "!\n");
		write_file_flag_for_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[11],
								  lic_pvr_c->pvr_c->pvr_colormap_file_name);
	}else{
		level = write_colormap_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[12],
									 lic_pvr_c->pvr_c->cmap_cbar_c->cmap_c);
		level = write_colorbar_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[14],
									 lic_pvr_c->pvr_c->cmap_cbar_c->cbar_c);
	};
	
	level = write_pvr_section_ctl_list(fp, level, lic_pvr_c->label_lic_pvr->label[15],
									   &lic_pvr_c->pvr_c->pvr_sect_c_list);
	level = write_pvr_iso_ctl_list(fp, level, lic_pvr_c->label_lic_pvr->label[16],
								   &lic_pvr_c->pvr_c->pvr_iso_c_list);
	level = write_lic_movie_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[17],
								  lic_pvr_c->pvr_c->movie_c);	
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_LIC_pvr_ctl_subfiles(struct LIC_pvr_ctl_c *lic_pvr_c){
    rename_pvr_ctl_subfiles(lic_pvr_c->pvr_c);
    return;
};

int read_LIC_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct LIC_pvr_ctl_c *lic_pvr_c){
    int iflag = 0;
    
    fprintf(stderr, "Read LIC_PVR control file: %s\n", file_name);
    if ((FP_LIC = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_read_line(FP_LIC, buf);
    if(right_begin_flag_c(buf, label_LIC_pvr_head) > 0){
        iflag = read_LIC_pvr_ctl_c(FP_LIC, buf, label_LIC_pvr_head, lic_pvr_c);
    };
    fclose(FP_LIC);
    
	read_pvr_ctl_subfiles(buf, lic_pvr_c->pvr_c);
    
    return iflag;
    
};
int write_LIC_pvr_ctl_file_c(const char *file_name, struct LIC_pvr_ctl_c *lic_pvr_c){
    int level;
	
    fprintf(stderr, "Write LIC_PVR control file: %s\n", file_name);
	write_pvr_ctl_subfiles(lic_pvr_c->pvr_c);
    
    if ((FP_LIC = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    level = write_LIC_pvr_ctl_c(FP_LIC, 0, label_LIC_pvr_head, lic_pvr_c);
    fclose(FP_LIC);
    
    return level;
    
};

