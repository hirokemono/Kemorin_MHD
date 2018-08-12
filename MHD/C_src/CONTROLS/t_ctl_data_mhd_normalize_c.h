/*
//  t_ctl_data_mhd_normalize_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_mhd_normalize_c_h_
#define t_ctl_data_mhd_normalize_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_real_IO.h"


struct momentum_equation_ctl_c{
	int maxlen;
	
	struct chara_real_ctl_list coef_4_viscous_list;
	struct chara_real_ctl_list coef_4_intertia_list;
	struct chara_real_ctl_list coef_4_grad_p_list;
	
	struct chara_real_ctl_list coef_4_termal_buo_list;
	struct chara_real_ctl_list coef_4_comp_buo_list;
	struct chara_real_ctl_list coef_4_Coriolis_list;
	struct chara_real_ctl_list coef_4_Lorentz_list;
};

struct induction_equation_ctl_c{
	int maxlen;
	
	struct chara_real_ctl_list coef_4_magne_evo_list;
	struct chara_real_ctl_list coef_4_mag_diffuse_list;
	struct chara_real_ctl_list coef_4_mag_potential_list;
	struct chara_real_ctl_list coef_4_induction_list;
};

struct heat_equation_ctl_c{
	int maxlen;
	
	struct chara_real_ctl_list coef_4_adv_flux_list;
	struct chara_real_ctl_list coef_4_diffuse_list;
	struct chara_real_ctl_list coef_4_source_list;
};


struct dimless_ctl_c{
	int maxlen;
	
	struct chara_real_ctl_list dimless_list;
};

struct equations_ctl_c{
	int maxlen;
	
	int iflag_mom_ctl;
	struct momentum_equation_ctl_c *mom_ctl_c;
	int iflag_heat_ctl;
	struct heat_equation_ctl_c *heat_ctl_c;
	int iflag_comp_ctl;
	struct heat_equation_ctl_c *comp_ctl_c;
	int iflag_induct_ctl;
	struct induction_equation_ctl_c *induct_ctl_c;
};


/* prototype */
void get_label_momentum_equation_ctl(int index, char *label);
void get_label_induction_equation_ctl(int index, char *label);
void get_label_heat_equation_ctl(int index, char *label);
void get_label_comp_equation_ctl(int index, char *label);
void get_label_dimless_ctl(int index, char *label);
void get_label_equations_ctl(int index, char *label);

void alloc_momentum_equation_ctl_c(struct momentum_equation_ctl_c *mom_ctl_c);
void dealloc_momentum_equation_ctl_c(struct momentum_equation_ctl_c *mom_ctl_c);
int read_momentum_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct momentum_equation_ctl_c *mom_ctl_c);
int write_momentum_equation_ctl_c(FILE *fp, int level, const char *label, 
                                  struct momentum_equation_ctl_c *mom_ctl_c);

void alloc_induction_equation_ctl_c(struct induction_equation_ctl_c *induct_ctl_c);
void dealloc_induction_equation_ctl_c(struct induction_equation_ctl_c *induct_ctl_c);
int read_induction_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct induction_equation_ctl_c *induct_ctl_c);
int write_induction_equation_ctl_c(FILE *fp, int level, const char *label, 
                                   struct induction_equation_ctl_c *induct_ctl_c);

void alloc_scalar_advection_eq_ctl_c(struct heat_equation_ctl_c *scalar_ctl_c);
void alloc_heat_equation_ctl_c(struct heat_equation_ctl_c *heat_ctl_c);
void dealloc_heat_equation_ctl_c(struct heat_equation_ctl_c *heat_ctl_c);
int read_heat_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct heat_equation_ctl_c *heat_ctl_c);
int write_heat_equation_ctl_c(FILE *fp, int level, const char *label, 
                              struct heat_equation_ctl_c *heat_ctl_c);

void alloc_comp_equation_ctl_c(struct heat_equation_ctl_c *comp_ctl_c);
int read_comp_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct heat_equation_ctl_c *comp_ctl_c);
int write_comp_equation_ctl_c(FILE *fp, int level, const char *label, 
                              struct heat_equation_ctl_c *comp_ctl_c);

void alloc_dimless_ctl_c(struct dimless_ctl_c *dless_ctl_c);
void dealloc_dimless_ctl_c(struct dimless_ctl_c *dless_ctl_c);
int read_dimless_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct dimless_ctl_c *dless_ctl_c);
int write_dimless_ctl_c(FILE *fp, int level, const char *label,
                        struct dimless_ctl_c *dless_ctl_c);

void alloc_equations_ctl_c(struct equations_ctl_c *eqs_ctl_c);
void dealloc_equations_ctl_c(struct equations_ctl_c *eqs_ctl_c);
int read_equations_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct equations_ctl_c *eqs_ctl_c);
int write_equations_ctl_c(FILE *fp, int level,
			const char *label, struct equations_ctl_c *eqs_ctl_c);


#endif /* t_ctl_data_mhd_normalize_c_h_ */
