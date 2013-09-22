
/*  m_psf_data_4_viewer_c.h */
#ifndef M_PSF_DATA_4_VIEWER_C_
#define M_PSF_DATA_4_VIEWER_C_

#define UCD_LABEL_LEN		1024

#define IFLAG_MESH       99
#define IFLAG_SURFACES   2
#define IFLAG_LINES      1

#define IFLAG_SURF_UDT   10
#define IFLAG_SURF_UCD   11
#define IFLAG_SURF_UDT_GZ   110
#define IFLAG_SURF_UCD_GZ   111

#include "kemosrc_param_c.h"
#include "skip_comment_c.h"


struct psf_data{
	int nnod_viz;
	int nele_viz;
	int nnod_4_ele_viz;
	
	int nfield;
	int ncomptot;
	
	int *inod_viz;
	int **ie_viz;
	
	int *id_coord;
	int *ncomp;
	int *istack_comp;
	
	double **xx_viz;
	double **d_nod;
	double **d_amp;
	
	double *d_ave;
	double *d_rms;
	double *d_min;
	double *d_max;
	double *amp_min;
	double *amp_max;
	
	char **data_name;
	
	double **x_ele_viz;
	
	double **norm_ele;
	double **norm_nod;
	double *area_viz;
	double area_total;
	
	double **dir_ele;
	double **dir_nod;
	double *length_ele;
	double length_total;
	
	int *iele_viz_far;
	double *z_ele_viz;
	double **color_nod;
	
	int nnod_added_4_map;
	int **inod_org_4_map_itp;
	double **coef_4_map_itp;
};

/* prototypes */

void alloc_viz_node_s(struct psf_data *viz_s);
void alloc_viz_ele_s(struct psf_data *viz_s);
void alloc_psf_field_name_c(struct psf_data *viz_s);
void alloc_psf_data_s(struct psf_data *viz_s);
void alloc_psf_field_data_c(struct psf_data *viz_s);
void alloc_psf_cutting_4_map(struct psf_data *viz_s);

void alloc_psf_norm_s(struct psf_data *viz_s);
void alloc_psf_length_s(struct psf_data *viz_s);
void alloc_psf_cutting_4_map(struct psf_data *viz_s);

void dealloc_psf_mesh_c(struct psf_data *viz_s);
void dealloc_psf_field_data_c(struct psf_data *viz_s);
void dealloc_psf_data_s(struct psf_data *viz_s);
void dealloc_psf_cutting_4_map(struct psf_data *viz_s);
void deallc_all_psf_data(struct psf_data *viz_s);
void deallc_all_fline_data(struct psf_data *viz_s);

void copy_viewer_udt_node(struct psf_data *viz_copied, struct psf_data *viz_org);
void copy_viewer_udt_connect(struct psf_data *viz_copied, struct psf_data *viz_org);
void copy_viewer_udt_field_name(struct psf_data *viz_copied, struct psf_data *viz_org);
void copy_viewer_udt_data(struct psf_data *viz_copied, struct psf_data *viz_org);

#endif
