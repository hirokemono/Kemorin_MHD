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



void alloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c){
	sections_c->iflag_psf_ctl = 0;
	sections_c->fname_psf_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	sections_c->psf_c = (struct psf_ctl_c *) malloc(sizeof(struct psf_ctl_c));
	alloc_psf_ctl_c(sections_c->psf_c);
	return;
};

void dealloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c){
	dealloc_psf_ctl_c(sections_c->psf_c);
	free(sections_c->psf_c);
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


void alloc_isosurface_ctl_c(struct isosurface_ctl_c *isosurfs_c){
	isosurfs_c->iflag_iso_ctl = 0;
	isosurfs_c->fname_iso_ctl = (char *)calloc(KCHARA_C, sizeof(char));
	isosurfs_c->iso_c = (struct iso_ctl_c *) malloc(sizeof(struct iso_ctl_c));
	alloc_iso_ctl_c(isosurfs_c->iso_c);
	return;
};

void dealloc_isosurface_ctl_c(struct isosurface_ctl_c *isosurfs_c){
	dealloc_iso_ctl_c(isosurfs_c->iso_c);
	free(isosurfs_c->iso_c);
	free(isosurfs_c->fname_iso_ctl);
	isosurfs_c->iflag_iso_ctl = 0;
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


void alloc_sectionings_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	
	for(i=0;i<viz_c->num_sectionings_ctl;i++){
		viz_c->sections_c[i] = (struct sectioning_ctl_c *) malloc(sizeof(struct sectioning_ctl_c));
		alloc_sectioning_ctl_c(viz_c->sections_c[i]);
	};
	return;
};

int read_sectionings_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag = 0;
	int icou = 0;
	
    if(viz_c->num_sectionings_ctl == 0) return icou;
	alloc_sectionings_ctl_c(viz_c);
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, viz_c->num_sectionings_ctl, icou) == 0
          || icou < viz_c->num_sectionings_ctl){
		iflag = read_sectioning_ctl_c(fp, buf, label, viz_c->sections_c[icou]);
		icou = icou + iflag;
        skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_sectionings_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
	int i = 0;
	
	if(viz_c->num_sectionings_ctl == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, viz_c->num_sectionings_ctl);
	for(i=0;i<viz_c->num_sectionings_ctl;i++){
		level = write_sectioning_ctl_c(fp, level, label, viz_c->sections_c[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_isosurfaces_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	
	for(i=0;i<viz_c->num_isosurfaces_ctl;i++){
		viz_c->isosurfs_c[i] = (struct isosurface_ctl_c *) malloc(sizeof(struct isosurface_ctl_c));
		alloc_isosurface_ctl_c(viz_c->isosurfs_c[i]);
	};
	return;
};

int read_isosurfaces_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag = 0;
	int icou = 0;
	
    if(viz_c->num_isosurfaces_ctl == 0) return icou;
	alloc_isosurfaces_ctl_c(viz_c);
    skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, viz_c->num_isosurfaces_ctl, icou) == 0){
		iflag = read_isosurface_ctl_c(fp, buf, label, viz_c->isosurfs_c[icou]);
		icou = icou + iflag;
        skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_isosurfaces_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
	int i = 0;
	
	if(viz_c->num_isosurfaces_ctl == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, viz_c->num_isosurfaces_ctl);
	for(i=0;i<viz_c->num_isosurfaces_ctl;i++){
		level = write_isosurface_ctl_c(fp, level, label, viz_c->isosurfs_c[i]);
	};
	level = write_end_array_flag_for_ctl_c(fp, level, label);
	return level;
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


void alloc_volume_renderings_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	
	for(i=0;i<viz_c->num_volume_renderings_ctl;i++){
		viz_c->v_render_c[i] = (struct volume_rendering_ctl_c *) malloc(sizeof(struct volume_rendering_ctl_c));
		alloc_volume_rendering_ctl_c(viz_c->v_render_c[i]);
	};
	return;
};

int read_volume_renderings_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag = 0;
	int icou = 0;
	
    if(viz_c->num_volume_renderings_ctl == 0) return icou;
	alloc_volume_renderings_ctl_c(viz_c);
    skip_comment_read_line(fp, buf);
	while(find_control_end_array_flag_c(buf, label, viz_c->num_volume_renderings_ctl, icou) == 0){
		iflag = read_volume_rendering_ctl_c(fp, buf, label, viz_c->v_render_c[icou]);
		icou = icou + iflag;
        skip_comment_read_line(fp, buf);
	};
	return icou;
};

int write_volume_renderings_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
	int i = 0;
	
	if(viz_c->num_volume_renderings_ctl == 0) return level;
	fprintf(fp, "!\n");
	level = write_array_flag_for_ctl_c(fp, level, label, viz_c->num_volume_renderings_ctl);
	for(i=0;i<viz_c->num_volume_renderings_ctl;i++){
		level = write_volume_rendering_ctl_c(fp, level, label, viz_c->v_render_c[i]);
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
	
	viz_c->sections_c = (struct sectioning_ctl_c **) malloc(sizeof(struct sectioning_ctl_c *));
	viz_c->isosurfs_c = (struct isosurface_ctl_c **) malloc(sizeof(struct isosurface_ctl_c *));
	viz_c->fldlines_c = (struct fieldline_ctl_c **) malloc(sizeof(struct fieldline_ctl_c *));
	viz_c->v_render_c = (struct volume_rendering_ctl_c **) malloc(sizeof(struct volume_rendering_ctl_c *));

    init_LIC_PVR_ctl_list(&viz_c->lic_ctl_list);
	
	viz_c->num_sectionings_ctl = 0;
	viz_c->num_isosurfaces_ctl = 0;
	viz_c->num_fieldlines_ctl =  0;
	viz_c->num_volume_renderings_ctl = 0;
	return;
};

void dealloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c){
	int i;
	/*
	for(i=0;i<viz_c->num_sectionings_ctl;i++){
		dealloc_sectioning_ctl_c(viz_c->sections_c[i]);
		free(viz_c->sections_c[i]);
	};
	free(viz_c->sections_c);
	
	for(i=0;i<viz_c->num_isosurfaces_ctl;i++){
		dealloc_isosurface_ctl_c(viz_c->isosurfs_c[i]);
		free(viz_c->isosurfs_c[i]);
	};
	free(viz_c->isosurfs_c);
	
	for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		dealloc_fieldline_ctl_c(viz_c->fldlines_c[i]);
		free(viz_c->fldlines_c[i]);
	};
	free(viz_c->fldlines_c);
	
	for(i=0;i<viz_c->num_volume_renderings_ctl;i++){
		dealloc_volume_rendering_ctl_c(viz_c->v_render_c[i]);
		free(viz_c->v_render_c[i]);
	};
	free(viz_c->v_render_c);
	*/
	clear_LIC_PVR_ctl_list(&viz_c->lic_ctl_list);
	
	viz_c->num_sectionings_ctl = 0;
	viz_c->num_isosurfaces_ctl = 0;
	viz_c->num_fieldlines_ctl =  0;
	viz_c->num_volume_renderings_ctl = 0;
	return;
};

int read_vizs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c){
	int iflag;
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 0], &viz_c->num_sectionings_ctl);
		if(iflag > 0) iflag = read_sectionings_ctl_c(fp, buf, label_viz_ctl[ 0], viz_c);
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 1], &viz_c->num_isosurfaces_ctl);
		if(iflag > 0) iflag = read_isosurfaces_ctl_c(fp, buf, label_viz_ctl[ 1], viz_c);
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 2], &viz_c->num_fieldlines_ctl);
		if(iflag > 0) iflag = read_fieldlines_ctl_c(fp, buf, label_viz_ctl[ 2], viz_c);
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 3], &viz_c->num_volume_renderings_ctl);
		if(iflag > 0) iflag = read_volume_renderings_ctl_c(fp, buf, label_viz_ctl[ 3], viz_c);
		
		iflag = read_LIC_PVR_ctl_list(fp, buf, label_viz_ctl[ 4], &viz_c->lic_ctl_list);
		
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 5], &viz_c->num_sectionings_ctl);
		if(iflag > 0) iflag = read_sectionings_ctl_c(fp, buf, label_viz_ctl[ 5], viz_c);
		
		iflag = find_control_array_flag_c(buf, label_viz_ctl[ 6], &viz_c->num_isosurfaces_ctl);
		if(iflag > 0) iflag = read_isosurfaces_ctl_c(fp, buf, label_viz_ctl[ 6], viz_c);
	};
	return 1;
};

int write_vizs_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(viz_c->num_sectionings_ctl > 0){
		level = write_sectionings_ctl_c(fp, level, label_viz_ctl[ 0], viz_c);
	};
	
	if(viz_c->num_isosurfaces_ctl > 0){
		fprintf(fp, "!\n");
		level = write_isosurfaces_ctl_c(fp, level, label_viz_ctl[ 1], viz_c);
	};
	
	if(viz_c->num_fieldlines_ctl > 0){
		fprintf(fp, "!\n");
		level = write_fieldlines_ctl_c(fp, level, label_viz_ctl[ 2], viz_c);
	};
	
	if(viz_c->num_volume_renderings_ctl > 0){
		fprintf(fp, "!\n");
		level = write_volume_renderings_ctl_c(fp, level, label_viz_ctl[ 3], viz_c);
	};
	
	level = write_LIC_PVR_ctl_list(fp, level, label_viz_ctl[ 4], &viz_c->lic_ctl_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_vizs_ctl_subfiles(struct visualizers_ctl_c *viz_c){
    int i;
    
    for(i=0;i<viz_c->num_sectionings_ctl;i++){
		if(viz_c->sections_c[i]->iflag_psf_ctl == -1){
			strcat(viz_c->sections_c[i]->fname_psf_ctl, "_2");
            rename_psf_define_file_c(viz_c->sections_c[i]->psf_c);
		};
    };
    for(i=0;i<viz_c->num_isosurfaces_ctl;i++){
		if(viz_c->isosurfs_c[i]->iflag_iso_ctl == -1){
			strcat(viz_c->isosurfs_c[i]->fname_iso_ctl, "_2");
		};
    };
    for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		if(viz_c->fldlines_c[i]->iflag_fline_ctl == -1){
			strcat(viz_c->fldlines_c[i]->fname_fline_ctl, "_2");
		};
    };
	for(i=0;i<viz_c->num_volume_renderings_ctl;i++){
		if(viz_c->v_render_c[i]->iflag_pvr_ctl == -1){
			strcat(viz_c->v_render_c[i]->fname_pvr_ctl, "_2");
			rename_pvr_ctl_subfiles(viz_c->v_render_c[i]->pvr_c);
		};
	};
	
	rename_LIC_PVR_subfile_list(&viz_c->lic_ctl_list);
    return;
};

void read_vizs_ctl_files_c(char buf[LENGTHBUF], struct visualizers_ctl_c *viz_c){
    int i;
	
    for(i=0;i<viz_c->num_sectionings_ctl;i++){
		read_sectioning_ctl_file_c(buf, viz_c->sections_c[i]);
    };
	for(i=0;i<viz_c->num_isosurfaces_ctl;i++){
		read_isosurface_ctl_file_c(buf, viz_c->isosurfs_c[i]);
    };
    for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		read_fieldline_ctl_file_c(buf, viz_c->fldlines_c[i]);
    };
    for(i=0;i<viz_c->num_volume_renderings_ctl;i++){
		read_volume_rendering_ctl_file_c(buf, viz_c->v_render_c[i]);
	};
	
	read_LIC_PVR_subfile_list(buf, &viz_c->lic_ctl_list);
	return;
};

void write_vizs_ctl_files_c(struct visualizers_ctl_c *viz_c){
    int i;
    
    for(i=0;i<viz_c->num_sectionings_ctl;i++){
		write_sectioning_ctl_file_c(viz_c->sections_c[i]);
    };
	for(i=0;i<viz_c->num_isosurfaces_ctl;i++){
		write_isosurface_ctl_file_c(viz_c->isosurfs_c[i]);
    };
    for(i=0;i<viz_c->num_fieldlines_ctl;i++){
		write_fieldline_ctl_file_c(viz_c->fldlines_c[i]);
    };
    for(i=0;i<viz_c->num_volume_renderings_ctl;i++){
		write_volume_rendering_ctl_file_c(viz_c->v_render_c[i]);
    };
	write_LIC_PVR_subfile_list(&viz_c->lic_ctl_list);
    
    return;
};
