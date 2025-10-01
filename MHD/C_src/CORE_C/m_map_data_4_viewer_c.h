
/*  m_map_data_4_viewer_c.h */
#ifndef M_MAP_DATA_4_VIEWER_C_
#define M_MAP_DATA_4_VIEWER_C_

struct psf_map_data{
	long nnod_map;
    long nele_map;
	int nnod_add_map;
	int nele_add_map;
	
	long ncomptot_map;
	
	long *inod_map;
	long **ie_map;
	
	double **xx_map;
	double **d_nod_map;
	
    long **inod_org_map;
	double **coef_itp_map;
};

/* prototypes */

void alloc_map_data_s(struct psf_map_data *psf_map_s);
void dealloc_map_data_s(struct psf_map_data *psf_map_s);

#endif
