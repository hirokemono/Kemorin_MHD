
/*  m_psf_data_4_viewer_c.h */
#ifndef M_PSF_DATA_4_VIEWER_C_
#define M_PSF_DATA_4_VIEWER_C_

#define IFLAG_MESH       99
#define IFLAG_SURFACES   2
#define IFLAG_LINES      1

#define IFLAG_SURF_UDT   10
#define IFLAG_SURF_UCD   11
#define IFLAG_SURF_UDT_GZ   110
#define IFLAG_SURF_UCD_GZ   111

#include "calypso_param_c.h"
#include "skip_comment_c.h"


struct psf_data{
	long nnod_viz;
	long nele_viz;
	long nnod_4_ele_viz;
	
	long nfield;
	long ncomptot;
	
	long *inod_viz;
	long **ie_viz;
	
	int *id_coord;
	long *ncomp;
	long *istack_comp;
	
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
    
    double center_psf[3];
    double xmin_psf[3];
    double xmax_psf[3];
    double rmax_psf;
	
	double **color_nod;
	
	int nnod_added_4_map;
	int **inod_org_4_map_itp;
	double **coef_4_map_itp;
};

typedef struct vtk_fields_list vtk_fields_t;

struct vtk_fields_list{
	char field_name[255];
    int  nnod_fld;
    int  ncomp_vtk;
    double **d_vtk;
    
    vtk_fields_t *next_fld;
};

struct vtk_field{
    int nnod_vtk;
    int nfld_vtk;
    struct vtk_fields_list *vtk_fields;
};

/* prototypes */

void alloc_viz_node_s(struct psf_data *psf_s);
void alloc_viz_ele_s(struct psf_data *psf_s);
void alloc_psf_field_name_c(struct psf_data *psf_s);
void alloc_psf_data_s(struct psf_data *psf_s);
void alloc_psf_field_data_c(struct psf_data *psf_s);
void alloc_psf_cutting_4_map(struct psf_data *psf_s);

void alloc_psf_norm_s(struct psf_data *psf_s);
void alloc_psf_length_s(struct psf_data *psf_s);
void alloc_psf_cutting_4_map(struct psf_data *psf_s);

void dealloc_psf_mesh_c(struct psf_data *psf_s);
void dealloc_psf_field_data_c(struct psf_data *psf_s);
void dealloc_psf_data_s(struct psf_data *psf_s);
void dealloc_psf_cutting_4_map(struct psf_data *psf_s);
void deallc_all_psf_data(struct psf_data *psf_s);
void deallc_all_fline_data(struct psf_data *psf_s);

struct vtk_field *alloc_vtk_fields_list_c(void);
void alloc_vtk_field_data_c(vtk_fields_t *vtk_s);
void dealloc_vtk_field_data_c(vtk_fields_t *vtk_s);
void dealloc_vtk_fields_list_c(struct vtk_field *vtk_list);


void copy_viewer_udt_node(struct psf_data *viz_copied, struct psf_data *viz_org);
void copy_viewer_udt_connect(struct psf_data *viz_copied, struct psf_data *viz_org);
void copy_viewer_udt_field_name(struct psf_data *viz_copied, struct psf_data *viz_org);
void copy_viewer_udt_data(struct psf_data *viz_copied, struct psf_data *viz_org);

void copy_vtk_list_2_udt_name(struct psf_data *viz_copied, struct vtk_field *vtk_list);
void copy_vtk_list_2_udt_data(struct psf_data *viz_copied, struct vtk_field *vtk_list);

void check_psf_read(struct psf_data *psf_s);
void compare_psf_data(struct psf_data *psf_s, struct psf_data *psf_z);

#endif
