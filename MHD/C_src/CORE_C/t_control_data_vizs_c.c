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



void alloc_fieldlines_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	
	for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		viz_c->fldlines_c[i] = (struct fieldline_ctl_c *) malloc(sizeof(struct fieldline_ctl_c));
		alloc_fieldline_ctl_c(viz_c->fldlines_c[i]);
	};
	return;
};

int read_fieldlines_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag = 0;
	int icou = 0;
	
    if(viz_c->num_fieldlines_ctl == 0) return icou;
	alloc_fieldlines_ctl_c(viz_c);
    skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, viz_c->num_fieldlines_ctl, icou) == 0){
		iflag = read_fieldline_ctl_c(fp, buf, label, viz_c->fldlines_c[icou]);
		icou = icou + iflag;
        skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_fieldlines_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
	int i = 0;
	
	if(viz_c->num_fieldlines_ctl == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, viz_c->num_fieldlines_ctl);
	for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		level = write_fieldline_ctl_c(fp, level, label, viz_c->fldlines_c[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	
	viz_c->maxlen = 0;
	for (i=0;i<NLBL_VIZ_CTL;i++){
		if(strlen(label_viz_ctl[i]) > viz_c->maxlen){
			viz_c->maxlen = (int) strlen(label_viz_ctl[i]);
		};
	};
	
	viz_c->fldlines_c = (struct fieldline_ctl_c **) malloc(sizeof(struct fieldline_ctl_c *));
	
	init_PSF_ctl_list(&viz_c->psf_ctl_list);
	init_ISO_ctl_list(&viz_c->iso_ctl_list);
	init_PVR_ctl_list(&viz_c->pvr_ctl_list);
	init_LIC_PVR_ctl_list(&viz_c->lic_ctl_list);
	
	viz_c->num_fieldlines_ctl =  0;
	return;
};

void dealloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	/*
	for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		dealloc_fieldline_ctl_c(viz_c->fldlines_c[i]);
		free(viz_c->fldlines_c[i]);
	};
	free(viz_c->fldlines_c);
	*/
	clear_PSF_ctl_list(&viz_c->psf_ctl_list);
	clear_ISO_ctl_list(&viz_c->iso_ctl_list);
	clear_PVR_ctl_list(&viz_c->pvr_ctl_list);
	clear_LIC_PVR_ctl_list(&viz_c->lic_ctl_list);
	
	viz_c->num_fieldlines_ctl =  0;
	return;
};

int read_vizs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 2], &viz_c->num_fieldlines_ctl);
		if(iflag > 0) iflag = read_fieldlines_ctl_c(fp, buf, label_viz_ctl[ 2], viz_c);
		
		iflag = read_PSF_ctl_list(fp, buf, label_viz_ctl[ 0], &viz_c->psf_ctl_list);
		iflag = read_ISO_ctl_list(fp, buf, label_viz_ctl[ 1], &viz_c->iso_ctl_list);
		iflag = read_PVR_ctl_list(fp, buf, label_viz_ctl[ 3], &viz_c->pvr_ctl_list);
		iflag = read_LIC_PVR_ctl_list(fp, buf, label_viz_ctl[ 4], &viz_c->lic_ctl_list);
		
		iflag = read_PSF_ctl_list(fp, buf, label_viz_ctl[ 5], &viz_c->psf_ctl_list);
		iflag = read_ISO_ctl_list(fp, buf, label_viz_ctl[ 6], &viz_c->iso_ctl_list);
	};
	return 1;
};

int write_vizs_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(viz_c->num_fieldlines_ctl > 0){
		fprintf(fp, "!\n");
		level = write_fieldlines_ctl_c(fp, level, label_viz_ctl[ 2], viz_c);
	};
	
	level = write_PSF_ctl_list(fp, level, label_viz_ctl[ 0], &viz_c->psf_ctl_list);
	level = write_ISO_ctl_list(fp, level, label_viz_ctl[ 1], &viz_c->iso_ctl_list);
	level = write_PVR_ctl_list(fp, level, label_viz_ctl[ 3], &viz_c->pvr_ctl_list);
	level = write_LIC_PVR_ctl_list(fp, level, label_viz_ctl[ 4], &viz_c->lic_ctl_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_vizs_ctl_subfiles(struct visualizers_ctl_c *viz_c){
    int i;
    
    for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		if(viz_c->fldlines_c[i]->iflag_fline_ctl == -1){
			strcat(viz_c->fldlines_c[i]->fname_fline_ctl, "_2");
		};
    };
	
	rename_PSF_subfile_list(&viz_c->psf_ctl_list);
	rename_ISO_subfile_list(&viz_c->iso_ctl_list);
	rename_PVR_subfile_list(&viz_c->pvr_ctl_list);
	rename_LIC_PVR_subfile_list(&viz_c->lic_ctl_list);
    return;
};

void read_vizs_ctl_files_c(char buf[LENGTHBUF], struct visualizers_ctl_c *viz_c){
    int i;
	
    for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		read_fieldline_ctl_file_c(buf, viz_c->fldlines_c[i]);
    };
	
	read_PSF_subfile_list(buf, &viz_c->psf_ctl_list);
	read_ISO_subfile_list(buf, &viz_c->iso_ctl_list);
	read_PVR_subfile_list(buf, &viz_c->pvr_ctl_list);
	read_LIC_PVR_subfile_list(buf, &viz_c->lic_ctl_list);
	return;
};

void write_vizs_ctl_files_c(struct visualizers_ctl_c *viz_c){
    int i;
    
    for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		write_fieldline_ctl_file_c(viz_c->fldlines_c[i]);
    };
	
	write_PSF_subfile_list(&viz_c->psf_ctl_list);
	write_ISO_subfile_list(&viz_c->iso_ctl_list);
	write_PVR_subfile_list(&viz_c->pvr_ctl_list);
	write_LIC_PVR_subfile_list(&viz_c->lic_ctl_list);
    
    return;
};
