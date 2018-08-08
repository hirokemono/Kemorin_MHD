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
#include "t_control_data_PSF_ctl_list.h"
#include "t_control_data_4_iso_c.h"
#include "t_control_data_4_fline_c.h"
#include "t_control_data_PVR_ctl_list.h"
#include "t_control_data_LIC_ctl_list.h"

struct isosurface_ctl_c{
    char *fname_iso_ctl;
    int iflag_iso_ctl;
    struct iso_ctl_c *iso_c;
	
	struct isosurface_ctl_c *_prev;
	struct isosurface_ctl_c *_next;
};

struct fieldline_ctl_c{
    char *fname_fline_ctl;
    int iflag_fline_ctl;
    struct fline_ctl_c *fline_c;
	
	struct fieldline_ctl_c *_prev;
	struct fieldline_ctl_c *_next;
};


struct visualizers_ctl_c{
    int maxlen;
    
    int num_isosurfaces_ctl;
    struct isosurface_ctl_c       **isosurfs_c;
    
    int num_fieldlines_ctl;
    struct fieldline_ctl_c        **fldlines_c;
    
    struct PSF_ctl_list psf_ctl_list;
    struct PVR_ctl_list pvr_ctl_list;
    struct LIC_PVR_ctl_list lic_ctl_list;
};

/* prototypes */

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
