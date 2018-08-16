/*
//  t_ctl_data_SGS_MHD_model_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#ifndef t_ctl_data_SGS_MHD_model_c_h_
#define t_ctl_data_SGS_MHD_model_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"

#include "t_ctl_data_SGS_model_c.h"
#include "t_ctl_data_temp_model_c.h"
#include "t_ctl_data_mhd_forces_c.h"
#include "t_ctl_data_mhd_normalize_c.h"
#include "t_ctl_data_MHD_boundary_c.h"
#include "t_ctl_data_mhd_evolution_c.h"
#include "t_ctl_data_4_fields_c.h"
#include "t_ctl_data_4_time_steps_c.h"
#include "t_ctl_data_mhd_evo_scheme_c.h"

#define NLBL_MHD_MODEL_CTL  15
#define NLBL_MHD_CONTROL_CTL  15

struct mhd_model_control_c{
	int maxlen;
	
	int iflag_field_control;
	struct field_ctl_c *fld_ctl;
	int iflag_mhd_evolution_control;
	struct mhd_evolution_ctl_c *evo_ctl;
	int iflag_mhd_evo_area_control;
	struct mhd_evo_area_ctl_c *earea_ctl;
	int iflag_node_bc_control;
	struct MHD_boundary_ctl_c *nbc_ctl;
	int iflag_surf_bc_control;
	struct MHD_boundary_ctl_c *sbc_ctl;
	int iflag_forces_control;
	struct forces_ctl_c *frc_ctl;
	int iflag_dimless_control;
	struct dimless_ctl_c *dless_ctl;
	int iflag_equations_control;
	struct equations_ctl_c *eqs_ctl;
	int iflag_gravity_control;
	struct gravity_ctl_c *g_ctl;
	int iflag_coriolis_control;
	struct coriolis_ctl_c *cor_ctl;
	int iflag_magneto_convection_control;
	struct magneto_cv_ctl_c *mcv_ctl;
	int iflag_reference_temp_control;
	struct reference_temperature_c *reft_ctl;
	int iflag_reference_comp_control;
	struct reference_temperature_c *refc_ctl;
	int iflag_sgs_model_control;
	struct SGS_model_control_c *sgs_ctl;
};

struct sph_mhd_control_control_c{
	int maxlen;
	
	int iflag_time_data_control;
	struct time_data_control_c *tctl;
	int iflag_mhd_restart_control;
	struct mhd_restart_control_c *mrst_ctl;
	int iflag_mhd_evo_scheme_control;
	struct mhd_evo_scheme_control_c *mevo_ctl;
};

/* Prototypes */
void get_label_mhd_model_ctl(int index, char *label);
void get_label_mhd_control_ctl(int index, char *label);

void alloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl);
void dealloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl);

int read_mhd_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct mhd_model_control_c *model_ctl);
int write_mhd_model_ctl_c(FILE *fp, int level, const char *label,
			struct mhd_model_control_c *model_ctl);

void alloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl);
void dealloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl);
int read_mhd_control_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_mhd_control_control_c *control_ctl);
int write_mhd_control_ctl_c(FILE *fp, int level, const char *label,
			struct sph_mhd_control_control_c *control_ctl);


#endif /* t_ctl_data_SGS_MHD_model_c_ */
