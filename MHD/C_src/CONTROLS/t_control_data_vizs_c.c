/*
//  t_control_data_vizs_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_vizs_c.h"

#define NLBL_VIZ_CTL  7

const char label_viz_ctl[NLBL_VIZ_CTL][KCHARA_C] = {
	/*[ 0]*/	{"cross_section_ctl"},
	/*[ 1]*/	{"isosurface_ctl"},
	/*[ 2]*/	{"fieldline"},
	/*[ 3]*/	{"volume_rendering"},
	/*[ 4]*/	{"LIC_rendering"},
	
	/*[ 5]*/	{"surface_rendering"},
	/*[ 6]*/	{"isosurf_rendering"}
};


void get_label_viz_ctl(int index, char *label){
    if(index < NLBL_VIZ_CTL) strngcopy(label, label_viz_ctl[index]);
    return;
};

struct visualizers_ctl_c * init_vizs_ctl_c(){
	int i;
    struct visualizers_ctl_c *viz_c;
    if ((viz_c = (struct visualizers_ctl_c *) malloc(sizeof(struct visualizers_ctl_c))) == NULL) {
        printf("malloc error for visualizers_ctl_c\n");
        exit(0);
    }
	
    viz_c->iflag_use = 0;
	viz_c->maxlen = 0;
	for (i=0;i<NLBL_VIZ_CTL;i++){
		if(strlen(label_viz_ctl[i]) > viz_c->maxlen){
			viz_c->maxlen = (int) strlen(label_viz_ctl[i]);
		};
	};
	
	init_PSF_ctl_list(&viz_c->psf_ctl_list);
	init_ISO_ctl_list(&viz_c->iso_ctl_list);
	init_PVR_ctl_list(&viz_c->pvr_ctl_list);
	init_LIC_PVR_ctl_list(&viz_c->lic_ctl_list);
	init_FLINE_ctl_list(&viz_c->fline_ctl_list);
	
	return viz_c;
};

void dealloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c){
	
	clear_PSF_ctl_list(&viz_c->psf_ctl_list);
	clear_ISO_ctl_list(&viz_c->iso_ctl_list);
	clear_PVR_ctl_list(&viz_c->pvr_ctl_list);
	clear_LIC_PVR_ctl_list(&viz_c->lic_ctl_list);
	clear_FLINE_ctl_list(&viz_c->fline_ctl_list);
    free(viz_c);
	return;
};

void read_vizs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		iflag = read_PSF_ctl_list(fp, buf, label_viz_ctl[ 0], &viz_c->psf_ctl_list);
		iflag = read_ISO_ctl_list(fp, buf, label_viz_ctl[ 1], &viz_c->iso_ctl_list);
		iflag = read_PVR_ctl_list(fp, buf, label_viz_ctl[ 3], &viz_c->pvr_ctl_list);
		iflag = read_LIC_PVR_ctl_list(fp, buf, label_viz_ctl[ 4], &viz_c->lic_ctl_list);
		iflag = read_FLINE_ctl_list(fp, buf, label_viz_ctl[ 2], &viz_c->fline_ctl_list);
		
		iflag = read_PSF_ctl_list(fp, buf, label_viz_ctl[ 5], &viz_c->psf_ctl_list);
		iflag = read_ISO_ctl_list(fp, buf, label_viz_ctl[ 6], &viz_c->iso_ctl_list);
	};
	viz_c->iflag_use = 1;
    return;
};

int write_vizs_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
    if(viz_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_PSF_ctl_list(fp, level, label_viz_ctl[ 0], &viz_c->psf_ctl_list);
	level = write_ISO_ctl_list(fp, level, label_viz_ctl[ 1], &viz_c->iso_ctl_list);
	level = write_PVR_ctl_list(fp, level, label_viz_ctl[ 3], &viz_c->pvr_ctl_list);
	level = write_LIC_PVR_ctl_list(fp, level, label_viz_ctl[ 4], &viz_c->lic_ctl_list);
	level = write_FLINE_ctl_list(fp, level, label_viz_ctl[ 2], &viz_c->fline_ctl_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_vizs_ctl_subfiles(struct visualizers_ctl_c *viz_c){
	
	rename_PSF_subfile_list(&viz_c->psf_ctl_list);
	rename_ISO_subfile_list(&viz_c->iso_ctl_list);
	rename_PVR_subfile_list(&viz_c->pvr_ctl_list);
	rename_LIC_PVR_subfile_list(&viz_c->lic_ctl_list);
	rename_FLINE_subfile_list(&viz_c->fline_ctl_list);
    return;
};

void read_vizs_ctl_files_c(char buf[LENGTHBUF], struct visualizers_ctl_c *viz_c){
	
	read_PSF_subfile_list(buf, &viz_c->psf_ctl_list);
	read_ISO_subfile_list(buf, &viz_c->iso_ctl_list);
	read_PVR_subfile_list(buf, &viz_c->pvr_ctl_list);
	read_LIC_PVR_subfile_list(buf, &viz_c->lic_ctl_list);
	read_FLINE_subfile_list(buf, &viz_c->fline_ctl_list);
	return;
};

void write_vizs_ctl_files_c(struct visualizers_ctl_c *viz_c){
	
	write_PSF_subfile_list(&viz_c->psf_ctl_list);
	write_ISO_subfile_list(&viz_c->iso_ctl_list);
	write_PVR_subfile_list(&viz_c->pvr_ctl_list);
	write_LIC_PVR_subfile_list(&viz_c->lic_ctl_list);
	write_FLINE_subfile_list(&viz_c->fline_ctl_list);
    
    return;
};
