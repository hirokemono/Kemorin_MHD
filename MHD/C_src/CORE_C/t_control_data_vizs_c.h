/*
//  t_control_data_vizs_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_vizs_c_h_
#define t_control_data_vizs_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_data_4_iso_c.h"
#include "t_control_data_4_psf_c.h"
#include "t_control_data_4_fline_c.h"
#include "t_control_data_4_pvr_c.h"
#include "t_control_data_LIC_pvr_c.h"

struct sectioning_ctl_c{
    int maxlen;
    
    char *fname_psf_ctl;
    int iflag_psf_ctl;
    struct psf_ctl_c *psf_c;
};

struct isosurface_ctl_c{
    int maxlen;
    
    char *fname_iso_ctl;
    int iflag_iso_ctl;
    struct iso_ctl_c *iso_c;
};

struct fieldline_ctl_c{
    int maxlen;
    
    char *fname_fline_ctl;
    int iflag_fline_ctl;
    struct fline_ctl_c *fline_c;
};

struct volume_rendering_ctl_c{
    int maxlen;
    
    char *fname_pvr_ctl;
    int iflag_pvr_ctl;
    struct pvr_ctl_c *pvr_c;
};

struct LIC_rendering_ctl_c{
    int maxlen;
    
    char *fname_lic_pvr_ctl;
    int iflag_lic_pvr_ctl;
    struct LIC_pvr_ctl_c *lic_pvr_c;
};

struct visualizers_ctl_c{
    int maxlen;
    
    int num_sectionings_ctl;
    struct sectioning_ctl_c       **sections_c;
    
    int num_isosurfaces_ctl;
    struct isosurface_ctl_c       **isosurfs_c;
    
    int num_fieldlines_ctl;
    struct fieldline_ctl_c        **fldlines_c;
    
    int num_volume_renderings_ctl;
    struct volume_rendering_ctl_c **v_render_c;
    
    int num_LIC_renderings_ctl;
    struct LIC_rendering_ctl_c    **lic_render_c;
};

/* prototypes */

void alloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c);
void dealloc_sectioning_ctl_c(struct sectioning_ctl_c *sections_c);
int read_sectioning_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct sectioning_ctl_c *sections_c);
int write_sectioning_ctl_c(FILE *fp, int level, const char *label, 
			struct sectioning_ctl_c *sections_c);
void read_sectioning_ctl_file_c(char buf[LENGTHBUF], struct sectioning_ctl_c *sections_c);
void write_sectioning_ctl_file_c(struct sectioning_ctl_c *sections_c);

void alloc_isosurface_ctl_c(struct isosurface_ctl_c *isosurfs_c);
void dealloc_isosurface_ctl_c(struct isosurface_ctl_c *isosurfs_c);
int read_isosurface_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct isosurface_ctl_c *isosurfs_c);
int write_isosurface_ctl_c(FILE *fp, int level, const char *label, 
			struct isosurface_ctl_c *isosurfs_c);
void read_isosurface_ctl_file_c(char buf[LENGTHBUF], struct isosurface_ctl_c *isosurfs_c);
void write_isosurface_ctl_file_c(struct isosurface_ctl_c *isosurfs_c);

void alloc_fieldline_ctl_c(struct fieldline_ctl_c *fldlines_c);
void dealloc_fieldline_ctl_c(struct fieldline_ctl_c *fldlines_c);
int read_fieldline_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct fieldline_ctl_c *fldlines_c);
int write_fieldline_ctl_c(FILE *fp, int level, const char *label, 
			struct fieldline_ctl_c *fldlines_c);
void read_fieldline_ctl_file_c(char buf[LENGTHBUF], struct fieldline_ctl_c *fldlines_c);
void write_fieldline_ctl_file_c(struct fieldline_ctl_c *fldlines_c);

void alloc_volume_rendering_ctl_c(struct volume_rendering_ctl_c *v_render_c);
void dealloc_volume_rendering_ctl_c(struct volume_rendering_ctl_c *v_render_c);
int read_volume_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct volume_rendering_ctl_c *v_render_c);
int write_volume_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct volume_rendering_ctl_c *v_render_c);
void read_volume_rendering_ctl_file_c(char buf[LENGTHBUF], struct volume_rendering_ctl_c *v_render_c);
void write_volume_rendering_ctl_file_c(struct volume_rendering_ctl_c *v_render_c);

void alloc_LIC_rendering_ctl_c(struct LIC_rendering_ctl_c *lic_render_c);
void dealloc_LIC_renderingctl_c(struct LIC_rendering_ctl_c *lic_render_c);
int read_LIC_rendering_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct LIC_rendering_ctl_c *lic_render_c);
int write_LIC_rendering_ctl_c(FILE *fp, int level, const char *label, 
			struct LIC_rendering_ctl_c *lic_render_c);
void read_LIC_rendering_ctl_file_c(char buf[LENGTHBUF], struct LIC_rendering_ctl_c *lic_render_c);
void write_LIC_rendering_ctl_file_c(struct LIC_rendering_ctl_c *lic_render_c);


void alloc_sectionings_ctl_c(struct visualizers_ctl_c *viz_c);
int read_sectionings_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_sectionings_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void alloc_isosurfaces_ctl_c(struct visualizers_ctl_c *viz_c);
int read_isosurfaces_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_isosurfaces_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void alloc_fieldlines_ctl_c(struct visualizers_ctl_c *viz_c);
int read_fieldlines_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_fieldlines_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void alloc_volume_renderings_ctl_c(struct visualizers_ctl_c *viz_c);
int read_volume_renderings_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_volume_renderings_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void alloc_LIC_renderings_ctl_c(struct visualizers_ctl_c *viz_c);
int read_LIC_renderings_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_LIC_renderings_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void alloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c);
void dealloc_vizs_ctl_c(struct visualizers_ctl_c *viz_c);
int read_vizs_ctl_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct visualizers_ctl_c *viz_c);
int write_vizs_ctl_c(FILE *fp, int level, const char *label, 
			struct visualizers_ctl_c *viz_c);

void rename_vizs_ctl_subfiles(struct visualizers_ctl_c *viz_c);
void read_vizs_ctl_files_c(char buf[LENGTHBUF], struct visualizers_ctl_c *viz_c);
void write_vizs_ctl_files_c(struct visualizers_ctl_c *viz_c);

#endif /* t_control_data_vizs_c_h_ */
