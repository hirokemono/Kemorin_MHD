/*
//  t_ctl_data_mhd_normalize_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_mhd_normalize_c.h"

#define NLBL_MOMENTUM_EQ_CTL   7
#define NLBL_INDUCTION_EQ_CTL  4
#define NLBL_HEAT_EQ_CTL       3

#define NLBL_DIMLESS_CTL       1
#define NLBL_EQUATIONS_CTL     4


const char label_momentum_equation_ctl[NLBL_MOMENTUM_EQ_CTL][KCHARA_C] = {
	/*[ 0]*/	{"coef_4_velocity_ctl"},
	/*[ 1]*/	{"coef_4_press_ctl"},
	/*[ 2]*/	{"coef_4_v_diffuse_ctl"},
	
	/*[ 3]*/	{"coef_4_buoyancy_ctl"},
	/*[ 4]*/	{"coef_4_composit_buoyancy_ctl"},
	/*[ 5]*/	{"coef_4_Coriolis_ctl"},
	/*[ 6]*/	{"coef_4_Lorentz_ctl"},
};

const char label_induction_equation_ctl[NLBL_INDUCTION_EQ_CTL][KCHARA_C] = {
	/*[ 0]*/	{"coef_4_magnetic_ctl"},
	/*[ 1]*/	{"coef_4_mag_p_ctl"},
	/*[ 2]*/	{"coef_4_m_diffuse_ctl"},
	/*[ 3]*/	{"coef_4_induction_ctl"},
};

const char label_heat_equation_ctl[NLBL_HEAT_EQ_CTL][KCHARA_C] = {
	/*[ 0]*/	{"coef_4_termal_ctl"},
	/*[ 1]*/	{"coef_4_t_diffuse_ctl"},
	/*[ 2]*/	{"coef_4_heat_source_ctl"},
};

const char label_comp_equation_ctl[NLBL_HEAT_EQ_CTL][KCHARA_C] = {
	/*[ 0]*/	{"coef_4_composition_ctl"},
	/*[ 1]*/	{"coef_4_c_diffuse_ctl"},
	/*[ 2]*/	{"coef_4_light_source_ctl"},
};


const char label_dimless_ctl[NLBL_DIMLESS_CTL][KCHARA_C] = {
	/*[ 0]*/	{"dimless_ctl"},
};

const char label_equations_ctl[NLBL_EQUATIONS_CTL][KCHARA_C] = {
	/*[ 0]*/	{"momentum"},
	/*[ 1]*/	{"thermal"},
	/*[ 2]*/	{"composition"},
	/*[ 3]*/	{"induction"},
};


void get_label_momentum_equation_ctl(int index, char *label){
    if(index < NLBL_MOMENTUM_EQ_CTL) strngcopy(label, label_momentum_equation_ctl[index]);
    return;
};
void get_label_induction_equation_ctl(int index, char *label){
    if(index < NLBL_INDUCTION_EQ_CTL) strngcopy(label, label_induction_equation_ctl[index]);
    return;
};
void get_label_heat_equation_ctl(int index, char *label){
    if(index < NLBL_HEAT_EQ_CTL) strngcopy(label, label_heat_equation_ctl[index]);
    return;
};
void get_label_comp_equation_ctl(int index, char *label){
    if(index < NLBL_HEAT_EQ_CTL) strngcopy(label, label_comp_equation_ctl[index]);
    return;
};
void get_label_dimless_ctl(int index, char *label){
    if(index < NLBL_DIMLESS_CTL) strngcopy(label, label_dimless_ctl[index]);
    return;
};
void get_label_equations_ctl(int index, char *label){
    if(index < NLBL_EQUATIONS_CTL) strngcopy(label, label_equations_ctl[index]);
    return;
};


void alloc_momentum_equation_ctl_c(struct momentum_equation_ctl_c *mom_ctl_c){
	int i;
	
	mom_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_MOMENTUM_EQ_CTL;i++){
		if(strlen(label_momentum_equation_ctl[i]) > mom_ctl_c->maxlen){
			mom_ctl_c->maxlen = (int) strlen(label_momentum_equation_ctl[i]);
		};
	};
	
    mom_ctl_c->coef_4_viscous_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    mom_ctl_c->coef_4_intertia_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    mom_ctl_c->coef_4_grad_p_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
	init_chara_real_clist(mom_ctl_c->coef_4_viscous_list);
	init_chara_real_clist(mom_ctl_c->coef_4_intertia_list);
	init_chara_real_clist(mom_ctl_c->coef_4_grad_p_list);
	
    mom_ctl_c->coef_4_termal_buo_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    mom_ctl_c->coef_4_comp_buo_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    mom_ctl_c->coef_4_Coriolis_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    mom_ctl_c->coef_4_Lorentz_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
	init_chara_real_clist(mom_ctl_c->coef_4_termal_buo_list);
	init_chara_real_clist(mom_ctl_c->coef_4_comp_buo_list);
	init_chara_real_clist(mom_ctl_c->coef_4_Coriolis_list);
	init_chara_real_clist(mom_ctl_c->coef_4_Lorentz_list);
	
	return;
};

void dealloc_momentum_equation_ctl_c(struct momentum_equation_ctl_c *mom_ctl_c){
	
	clear_chara_real_clist(mom_ctl_c->coef_4_viscous_list);
	clear_chara_real_clist(mom_ctl_c->coef_4_intertia_list);
	clear_chara_real_clist(mom_ctl_c->coef_4_grad_p_list);
    free(mom_ctl_c->coef_4_viscous_list);
    free(mom_ctl_c->coef_4_intertia_list);
    free(mom_ctl_c->coef_4_grad_p_list);
	
	clear_chara_real_clist(mom_ctl_c->coef_4_termal_buo_list);
	clear_chara_real_clist(mom_ctl_c->coef_4_comp_buo_list);
	clear_chara_real_clist(mom_ctl_c->coef_4_Coriolis_list);
	clear_chara_real_clist(mom_ctl_c->coef_4_Lorentz_list);
    free(mom_ctl_c->coef_4_termal_buo_list);
    free(mom_ctl_c->coef_4_comp_buo_list);
    free(mom_ctl_c->coef_4_Coriolis_list);
    free(mom_ctl_c->coef_4_Lorentz_list);
	return;
};

int read_momentum_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct momentum_equation_ctl_c *mom_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 0], mom_ctl_c->coef_4_intertia_list);
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 1], mom_ctl_c->coef_4_grad_p_list);
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 2], mom_ctl_c->coef_4_viscous_list);
		
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 3], mom_ctl_c->coef_4_termal_buo_list);
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 4], mom_ctl_c->coef_4_comp_buo_list);
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 5], mom_ctl_c->coef_4_Coriolis_list);
		read_chara_real_clist(fp, buf, label_momentum_equation_ctl[ 6], mom_ctl_c->coef_4_Lorentz_list);
	};
	return 1;
};

int write_momentum_equation_ctl_c(FILE *fp, int level,
			const char *label, struct momentum_equation_ctl_c *mom_ctl_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[0], mom_ctl_c->coef_4_intertia_list);
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[1], mom_ctl_c->coef_4_grad_p_list);
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[2], mom_ctl_c->coef_4_viscous_list);
	
	
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[3], mom_ctl_c->coef_4_termal_buo_list);
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[4], mom_ctl_c->coef_4_comp_buo_list);
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[5], mom_ctl_c->coef_4_Coriolis_list);
	write_chara_real_clist(fp, level, label_momentum_equation_ctl[6], mom_ctl_c->coef_4_Lorentz_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_induction_equation_ctl_c(struct induction_equation_ctl_c *induct_ctl_c){
	int i;
	
	induct_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_MOMENTUM_EQ_CTL;i++){
		if(strlen(label_induction_equation_ctl[i]) > induct_ctl_c->maxlen){
			induct_ctl_c->maxlen = (int) strlen(label_induction_equation_ctl[i]);
		};
	};
	
    induct_ctl_c->coef_4_magne_evo_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    induct_ctl_c->coef_4_mag_diffuse_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    induct_ctl_c->coef_4_mag_potential_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    induct_ctl_c->coef_4_induction_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
	init_chara_real_clist(induct_ctl_c->coef_4_magne_evo_list);
	init_chara_real_clist(induct_ctl_c->coef_4_mag_diffuse_list);
	init_chara_real_clist(induct_ctl_c->coef_4_mag_potential_list);
	init_chara_real_clist(induct_ctl_c->coef_4_induction_list);
	
	return;
};

void dealloc_induction_equation_ctl_c(struct induction_equation_ctl_c *induct_ctl_c){
	clear_chara_real_clist(induct_ctl_c->coef_4_magne_evo_list);
	clear_chara_real_clist(induct_ctl_c->coef_4_mag_diffuse_list);
	clear_chara_real_clist(induct_ctl_c->coef_4_mag_potential_list);
	clear_chara_real_clist(induct_ctl_c->coef_4_induction_list);
    free(induct_ctl_c->coef_4_magne_evo_list);
    free(induct_ctl_c->coef_4_mag_diffuse_list);
    free(induct_ctl_c->coef_4_mag_potential_list);
    free(induct_ctl_c->coef_4_induction_list);
	return;
};

int read_induction_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct induction_equation_ctl_c *induct_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_clist(fp, buf, label_induction_equation_ctl[ 0], induct_ctl_c->coef_4_magne_evo_list);
		read_chara_real_clist(fp, buf, label_induction_equation_ctl[ 1], induct_ctl_c->coef_4_mag_potential_list);
		read_chara_real_clist(fp, buf, label_induction_equation_ctl[ 2], induct_ctl_c->coef_4_mag_diffuse_list);
		read_chara_real_clist(fp, buf, label_induction_equation_ctl[ 3], induct_ctl_c->coef_4_induction_list);
	};
	return 1;
};

int write_induction_equation_ctl_c(FILE *fp, int level,
			const char *label, struct induction_equation_ctl_c *induct_ctl_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_real_clist(fp, level, label_induction_equation_ctl[0], induct_ctl_c->coef_4_magne_evo_list);
	write_chara_real_clist(fp, level, label_induction_equation_ctl[1], induct_ctl_c->coef_4_mag_potential_list);
	write_chara_real_clist(fp, level, label_induction_equation_ctl[2], induct_ctl_c->coef_4_mag_diffuse_list);
	write_chara_real_clist(fp, level, label_induction_equation_ctl[3], induct_ctl_c->coef_4_induction_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_scalar_advection_eq_ctl_c(struct heat_equation_ctl_c *scalar_ctl_c){
    
    scalar_ctl_c->coef_4_adv_flux_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    scalar_ctl_c->coef_4_diffuse_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    scalar_ctl_c->coef_4_source_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
    init_chara_real_clist(scalar_ctl_c->coef_4_adv_flux_list);
    init_chara_real_clist(scalar_ctl_c->coef_4_diffuse_list);
    init_chara_real_clist(scalar_ctl_c->coef_4_source_list);
    
    return;
};

void alloc_heat_equation_ctl_c(struct heat_equation_ctl_c *heat_ctl_c){
	int i;
	
	heat_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_MOMENTUM_EQ_CTL;i++){
		if(strlen(label_heat_equation_ctl[i]) > heat_ctl_c->maxlen){
			heat_ctl_c->maxlen = (int) strlen(label_heat_equation_ctl[i]);
		};
	};
	
    alloc_scalar_advection_eq_ctl_c(heat_ctl_c);
	return;
};

void dealloc_heat_equation_ctl_c(struct heat_equation_ctl_c *heat_ctl_c){
	
	clear_chara_real_clist(heat_ctl_c->coef_4_adv_flux_list);
	clear_chara_real_clist(heat_ctl_c->coef_4_diffuse_list);
	clear_chara_real_clist(heat_ctl_c->coef_4_source_list);
    free(heat_ctl_c->coef_4_adv_flux_list);
    free(heat_ctl_c->coef_4_diffuse_list);
    free(heat_ctl_c->coef_4_source_list);
	
	return;
};

int read_heat_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct heat_equation_ctl_c *heat_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_clist(fp, buf, label_heat_equation_ctl[ 0], heat_ctl_c->coef_4_adv_flux_list);
		read_chara_real_clist(fp, buf, label_heat_equation_ctl[ 1], heat_ctl_c->coef_4_diffuse_list);
		read_chara_real_clist(fp, buf, label_heat_equation_ctl[ 2], heat_ctl_c->coef_4_source_list);
	};
	return 1;
};

int write_heat_equation_ctl_c(FILE *fp, int level, const char *label,
                              struct heat_equation_ctl_c *heat_ctl_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_real_clist(fp, level, label_heat_equation_ctl[0], heat_ctl_c->coef_4_adv_flux_list);
	write_chara_real_clist(fp, level, label_heat_equation_ctl[1], heat_ctl_c->coef_4_diffuse_list);
	write_chara_real_clist(fp, level, label_heat_equation_ctl[2], heat_ctl_c->coef_4_source_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_comp_equation_ctl_c(struct heat_equation_ctl_c *comp_ctl_c){
	int i;
	
	comp_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_MOMENTUM_EQ_CTL;i++){
		if(strlen(label_comp_equation_ctl[i]) > comp_ctl_c->maxlen){
			comp_ctl_c->maxlen = (int) strlen(label_comp_equation_ctl[i]);
		};
	};
	
    alloc_scalar_advection_eq_ctl_c(comp_ctl_c);
	return;
};

int read_comp_equation_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct heat_equation_ctl_c *comp_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_clist(fp, buf, label_comp_equation_ctl[ 0], comp_ctl_c->coef_4_adv_flux_list);
		read_chara_real_clist(fp, buf, label_comp_equation_ctl[ 1], comp_ctl_c->coef_4_diffuse_list);
		read_chara_real_clist(fp, buf, label_comp_equation_ctl[ 2], comp_ctl_c->coef_4_source_list);
	};
	return 1;
};

int write_comp_equation_ctl_c(FILE *fp, int level,
			const char *label, struct heat_equation_ctl_c *comp_ctl_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_real_clist(fp, level, label_comp_equation_ctl[0], comp_ctl_c->coef_4_adv_flux_list);
	write_chara_real_clist(fp, level, label_comp_equation_ctl[1], comp_ctl_c->coef_4_diffuse_list);
	write_chara_real_clist(fp, level, label_comp_equation_ctl[2], comp_ctl_c->coef_4_source_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_dimless_ctl_c(struct dimless_ctl_c *dless_ctl_c){
	int i;
	
	dless_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_MOMENTUM_EQ_CTL;i++){
		if(strlen(label_dimless_ctl[i]) > dless_ctl_c->maxlen){
			dless_ctl_c->maxlen = (int) strlen(label_dimless_ctl[i]);
		};
	};
	
    dless_ctl_c->dimless_list = (struct chara_real_clist *) malloc(sizeof(struct chara_real_clist));
	init_chara_real_clist(dless_ctl_c->dimless_list);
	return;
};

void dealloc_dimless_ctl_c(struct dimless_ctl_c *dless_ctl_c){
	
	clear_chara_real_clist(dless_ctl_c->dimless_list);
    free(dless_ctl_c->dimless_list);
	
	return;
};

int read_dimless_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct dimless_ctl_c *dless_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_real_clist(fp, buf, label_dimless_ctl[ 0], dless_ctl_c->dimless_list);
	};
	return 1;
};

int write_dimless_ctl_c(FILE *fp, int level, const char *label,
                        struct dimless_ctl_c *dless_ctl_c){
	
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_real_clist(fp, level, label_dimless_ctl[0], dless_ctl_c->dimless_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_equations_ctl_c(struct equations_ctl_c *eqs_ctl_c){
	int i;
	
	eqs_ctl_c->maxlen = 0;
	for (i=0;i<NLBL_MOMENTUM_EQ_CTL;i++){
		if(strlen(label_equations_ctl[i]) > eqs_ctl_c->maxlen){
			eqs_ctl_c->maxlen = (int) strlen(label_equations_ctl[i]);
		};
	};
	
	eqs_ctl_c->mom_ctl_c = (struct momentum_equation_ctl_c *) malloc(sizeof(struct momentum_equation_ctl_c));
	alloc_momentum_equation_ctl_c(eqs_ctl_c->mom_ctl_c);
	eqs_ctl_c->induct_ctl_c = (struct induction_equation_ctl_c *) malloc(sizeof(struct induction_equation_ctl_c));
	alloc_induction_equation_ctl_c(eqs_ctl_c->induct_ctl_c);
	eqs_ctl_c->heat_ctl_c = (struct heat_equation_ctl_c *) malloc(sizeof(struct heat_equation_ctl_c));
	alloc_heat_equation_ctl_c(eqs_ctl_c->heat_ctl_c);
	eqs_ctl_c->comp_ctl_c = (struct heat_equation_ctl_c *) malloc(sizeof(struct heat_equation_ctl_c));
	alloc_comp_equation_ctl_c(eqs_ctl_c->comp_ctl_c);
	
	return;
};

void dealloc_equations_ctl_c(struct equations_ctl_c *eqs_ctl_c){
	
	dealloc_momentum_equation_ctl_c(eqs_ctl_c->mom_ctl_c);
	dealloc_induction_equation_ctl_c(eqs_ctl_c->induct_ctl_c);
	dealloc_heat_equation_ctl_c(eqs_ctl_c->heat_ctl_c);
	dealloc_heat_equation_ctl_c(eqs_ctl_c->comp_ctl_c);
	
	free(eqs_ctl_c->mom_ctl_c);
	free(eqs_ctl_c->induct_ctl_c);
	free(eqs_ctl_c->heat_ctl_c);
	free(eqs_ctl_c->comp_ctl_c);
	
	return;
};

int read_equations_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct equations_ctl_c *eqs_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_equations_ctl[ 1]) > 0) 
		eqs_ctl_c->iflag_heat_ctl = read_heat_equation_ctl_c(fp, buf, 
					label_equations_ctl[ 1], eqs_ctl_c->heat_ctl_c);
		
		if(right_begin_flag_c(buf, label_equations_ctl[ 2]) > 0) 
		eqs_ctl_c->iflag_comp_ctl = read_comp_equation_ctl_c(fp, buf, 
					label_equations_ctl[ 2], eqs_ctl_c->comp_ctl_c);
		
		if(right_begin_flag_c(buf, label_equations_ctl[ 0]) > 0) 
		eqs_ctl_c->iflag_mom_ctl = read_momentum_equation_ctl_c(fp, buf, 
					label_equations_ctl[ 0], eqs_ctl_c->mom_ctl_c);
		
		if(right_begin_flag_c(buf, label_equations_ctl[ 3]) > 0) 
		eqs_ctl_c->iflag_induct_ctl = read_induction_equation_ctl_c(fp, buf, 
					label_equations_ctl[ 3], eqs_ctl_c->induct_ctl_c);
	};
	return 1;
};

int write_equations_ctl_c(FILE *fp, int level, const char *label, struct equations_ctl_c *eqs_ctl_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    if(eqs_ctl_c->iflag_heat_ctl > 0){
/*        fprintf(fp, "!\n");*/
        level = write_heat_equation_ctl_c(fp, level, label_equations_ctl[ 1], eqs_ctl_c->heat_ctl_c);
    };
    if(eqs_ctl_c->iflag_comp_ctl > 0){
        fprintf(fp, "!\n");
        level = write_comp_equation_ctl_c(fp, level, label_equations_ctl[ 2], eqs_ctl_c->comp_ctl_c);
    };
    if(eqs_ctl_c->iflag_mom_ctl > 0){
        fprintf(fp, "!\n");
        level = write_momentum_equation_ctl_c(fp, level, label_equations_ctl[ 0], eqs_ctl_c->mom_ctl_c);
    };
    if(eqs_ctl_c->iflag_induct_ctl > 0){
        fprintf(fp, "!\n");
        level = write_induction_equation_ctl_c(fp, level, label_equations_ctl[ 3], eqs_ctl_c->induct_ctl_c);
    };
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
