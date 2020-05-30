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
#include "calypso_param_c.h"
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

#define NLBL_MHD_MODEL_CTL  14
#define NLBL_MHD_CONTROL_CTL  3

struct mhd_model_control_c{
    int iflag_use;
	int maxlen;
	
	struct field_ctl_c *fld_ctl;
	struct mhd_evolution_ctl_c *evo_ctl;
	struct mhd_evo_area_ctl_c *earea_ctl;
	struct MHD_boundary_ctl_c *nbc_ctl;
	struct MHD_boundary_ctl_c *sbc_ctl;
	struct forces_ctl_c *frc_ctl;
	struct dimless_ctl_c *dless_ctl;
	struct equations_ctl_c *eqs_ctl;
	struct gravity_ctl_c *g_ctl;
	struct coriolis_ctl_c *cor_ctl;
	struct magneto_cv_ctl_c *mcv_ctl;
	struct reference_temperature_c *reft_ctl;
	struct reference_temperature_c *refc_ctl;
	struct SGS_model_control_c *sgs_ctl;
};

struct sph_mhd_control_control_c{
    int iflag_use;
	int maxlen;
	
	struct time_data_control_c *tctl;
	struct mhd_restart_control_c *mrst_ctl;
	struct mhd_evo_scheme_control_c *mevo_ctl;
};

/* Prototypes */
void get_label_mhd_model_ctl(int index, char *label);
void get_label_mhd_control_ctl(int index, char *label);

struct mhd_model_control_c * init_mhd_model_control_c();
void dealloc_mhd_model_control_c(struct mhd_model_control_c *model_ctl);

void read_mhd_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct mhd_model_control_c *model_ctl);
int write_mhd_model_ctl_c(FILE *fp, int level, const char *label,
			struct mhd_model_control_c *model_ctl);

struct sph_mhd_control_control_c * init_sph_mhd_control_control_c();
void dealloc_sph_mhd_control_control_c(struct sph_mhd_control_control_c *control_ctl);
void read_mhd_control_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sph_mhd_control_control_c *control_ctl);
int write_mhd_control_ctl_c(FILE *fp, int level, const char *label,
			struct sph_mhd_control_control_c *control_ctl);


#endif /* t_ctl_data_SGS_MHD_model_c_ */
