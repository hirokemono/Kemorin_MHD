/*
//  t_ctl_data_4_sph_monitor_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/19.
*/

#include "t_ctl_data_4_sph_monitor_c.h"

#define NLBL_SPH_MONITOR 8

#define NLBL_PICK_SPECTR   5
#define NLBL_GAUSS_SPECTR  5
#define NLBL_LAYERD_SPECTR 6
#define NLBL_MID_EQUATOR   4

const char label_sph_monitor_ctl[NLBL_SPH_MONITOR][KCHARA_C] = {
    /*[ 0]*/    {"volume_spectrum_ctl"},
    /*[ 1]*/    {"layered_spectrum_ctl"},
    
    /*[ 2]*/    {"gauss_coefficient_ctl"},
    /*[ 3]*/    {"pickup_spectr_ctl"},
    /*[ 4]*/    {"mid_equator_monitor_ctl"},
	
    
	/*[ 5]*/    {"volume_average_prefix"},
    /*[ 6]*/    {"volume_pwr_spectr_prefix"},
    /*[ 7]*/    {"nusselt_number_prefix"}
};


const char label_pick_spectr_ctl[NLBL_PICK_SPECTR][KCHARA_C] = {
    /*[ 0]*/    {"picked_sph_prefix"},
    /*[ 1]*/    {"pick_layer_ctl"},
    
    /*[ 2]*/    {"pick_sph_spectr_ctl"},
    /*[ 3]*/    {"pick_sph_degree_ctl"},
    /*[ 4]*/    {"pick_sph_order_ctl"}
};

const char label_gauss_spectr_ctl[NLBL_GAUSS_SPECTR][KCHARA_C] = {
    /*[ 0]*/    {"gauss_coefs_prefix"},
    /*[ 1]*/    {"gauss_coefs_radius_ctl"},
    
    /*[ 2]*/    {"pick_gauss_coefs_ctl"},
    /*[ 3]*/    {"pick_gauss_coef_degree_ctl"},
    /*[ 4]*/    {"pick_gauss_coef_order_ctl"}
};

const char label_layerd_spectr_ctl[NLBL_LAYERD_SPECTR][KCHARA_C] = {
    /*[ 0]*/    {"layered_pwr_spectr_prefix"},
    /*[ 1]*/    {"spectr_layer_ctl"},
    
    /*[ 2]*/    {"degree_spectr_switch"},
    /*[ 3]*/    {"order_spectr_switch"},
    /*[ 4]*/    {"diff_lm_spectr_switch"},
    /*[ 5]*/    {"axisymmetric_spectr_switch"}
};

const char label_mid_equator_ctl[NLBL_MID_EQUATOR][KCHARA_C] = {
    /*[ 0]*/    {"pick_circle_coord_ctl"},
    /*[ 1]*/    {"nphi_mid_eq_ctl"},
    /*[ 2]*/    {"pick_cylindrical_radius_ctl"},
    /*[ 3]*/    {"pick_vertical_position_ctl"}
};


void get_label_sph_monitor_ctl(int index, char *label){
    if(index < NLBL_SPH_MONITOR) strngcopy(label, label_sph_monitor_ctl[index]);
    return;
};
void get_label_pick_spectr_ctl(int index, char *label){
    if(index < NLBL_PICK_SPECTR) strngcopy(label, label_pick_spectr_ctl[index]);
    return;
};
void get_label_gauss_spectr_ctl(int index, char *label){
    if(index < NLBL_GAUSS_SPECTR) strngcopy(label, label_gauss_spectr_ctl[index]);
    return;
};
void get_label_layerd_spectr_ctl(int index, char *label){
    if(index < NLBL_LAYERD_SPECTR) strngcopy(label, label_layerd_spectr_ctl[index]);
    return;
};
void get_label_mid_equator_ctl(int index, char *label){
    if(index < NLBL_MID_EQUATOR) strngcopy(label, label_mid_equator_ctl[index]);
    return;
};

struct pick_spectr_control_c * init_pick_spectr_control_c(){
    int i;
    struct pick_spectr_control_c *pspec_ctl_c;
    if((pspec_ctl_c = (struct pick_spectr_control_c *) malloc(sizeof(struct pick_spectr_control_c))) == NULL) {
        printf("malloc error for pick_spectr_control_c \n");
        exit(0);
    }
    
    
    pspec_ctl_c->iflag_use = 0;
    pspec_ctl_c->maxlen = 0;
    for (i=0;i<NLBL_PICK_SPECTR;i++){
        if(strlen(label_pick_spectr_ctl[i]) > pspec_ctl_c->maxlen){
            pspec_ctl_c->maxlen = (int) strlen(label_pick_spectr_ctl[i]);
        };
	};
	
	pspec_ctl_c->picked_mode_head_c = init_chara_ctl_item_c();
		
    pspec_ctl_c->idx_pick_layer_list = init_int_clist();
    sprintf(pspec_ctl_c->idx_pick_layer_list->i1_name, "Radial_ID");
    
    pspec_ctl_c->idx_pick_sph_list = init_int2_clist();
    sprintf(pspec_ctl_c->idx_pick_sph_list->i1_name, "Degree");
    sprintf(pspec_ctl_c->idx_pick_sph_list->i2_name, "Order");

    pspec_ctl_c->idx_pick_sph_l_list = init_int_clist();
    sprintf(pspec_ctl_c->idx_pick_sph_l_list->i1_name, "Degree");

    pspec_ctl_c->idx_pick_sph_m_list = init_int_clist();
    sprintf(pspec_ctl_c->idx_pick_sph_m_list->i1_name, "Order");
	
	return pspec_ctl_c;
};

void dealloc_pick_spectr_control_c(struct pick_spectr_control_c *pspec_ctl_c){
    dealloc_int2_clist(pspec_ctl_c->idx_pick_sph_list);

    dealloc_int_clist(pspec_ctl_c->idx_pick_sph_l_list);
    dealloc_int_clist(pspec_ctl_c->idx_pick_sph_m_list);
    dealloc_int_clist(pspec_ctl_c->idx_pick_layer_list);
	
    dealloc_chara_ctl_item_c(pspec_ctl_c->picked_mode_head_c);
    free(pspec_ctl_c);
	return;
};

void read_pick_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pick_spectr_control_c *pspec_ctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_pick_spectr_ctl[0], pspec_ctl_c->picked_mode_head_c);
		
		read_int_clist(fp, buf, label_pick_spectr_ctl[1], pspec_ctl_c->idx_pick_layer_list);
		
		read_int2_clist(fp, buf, label_pick_spectr_ctl[2], pspec_ctl_c->idx_pick_sph_list);
		read_int_clist(fp, buf, label_pick_spectr_ctl[3], pspec_ctl_c->idx_pick_sph_l_list);
		read_int_clist(fp, buf, label_pick_spectr_ctl[4], pspec_ctl_c->idx_pick_sph_m_list);
	};
    pspec_ctl_c->iflag_use = 1;
    return;
};

int write_pick_spectr_control_c(FILE *fp, int level, const char *label, 
                                struct pick_spectr_control_c *pspec_ctl_c){
    if(pspec_ctl_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_chara_ctl_item_c(fp, level, pspec_ctl_c->maxlen, 
				label_pick_spectr_ctl[0], pspec_ctl_c->picked_mode_head_c);
	
	write_int_clist(fp, level, label_pick_spectr_ctl[1], pspec_ctl_c->idx_pick_layer_list);
	
	write_int2_clist(fp, level, label_pick_spectr_ctl[2], pspec_ctl_c->idx_pick_sph_list);
	
	write_int_clist(fp, level, label_pick_spectr_ctl[3], pspec_ctl_c->idx_pick_sph_l_list);
	write_int_clist(fp, level, label_pick_spectr_ctl[4], pspec_ctl_c->idx_pick_sph_m_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

struct gauss_spectr_control_c * init_gauss_spectr_control_c(){
	int i;
    struct gauss_spectr_control_c *g_pwr;
    if((g_pwr = (struct gauss_spectr_control_c *) malloc(sizeof(struct gauss_spectr_control_c))) == NULL) {
        printf("malloc error for gauss_spectr_control_c \n");
        exit(0);
    }
    
    g_pwr->iflag_use = 0;
	g_pwr->maxlen = 0;
	for (i=0;i<NLBL_GAUSS_SPECTR;i++){
		if(strlen(label_gauss_spectr_ctl[i]) > g_pwr->maxlen){
			g_pwr->maxlen = (int) strlen(label_gauss_spectr_ctl[i]);
		};
	};
	
	g_pwr->gauss_coefs_prefix_c = init_chara_ctl_item_c();
    g_pwr->gauss_coefs_radius_c = init_real_ctl_item_c();
	
    g_pwr->idx_gauss_list = init_int2_clist();
    sprintf(g_pwr->idx_gauss_list->i1_name, "Degree");
    sprintf(g_pwr->idx_gauss_list->i2_name, "Order");

    g_pwr->idx_gauss_l_list = init_int_clist();
    sprintf(g_pwr->idx_gauss_l_list->i1_name, "Degree");
    
    g_pwr->idx_gauss_m_list = init_int_clist();
    sprintf(g_pwr->idx_gauss_m_list->i1_name, "Order");
	
	return g_pwr;
};

void dealloc_gauss_spectr_control_c(struct gauss_spectr_control_c *g_pwr){
	dealloc_int2_clist(g_pwr->idx_gauss_list);
    dealloc_int_clist(g_pwr->idx_gauss_l_list);
	dealloc_int_clist(g_pwr->idx_gauss_m_list);
	
	dealloc_chara_ctl_item_c(g_pwr->gauss_coefs_prefix_c);
    free(g_pwr);
	return;
};

void read_gauss_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct gauss_spectr_control_c *g_pwr){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		read_chara_ctl_item_c(buf, label_gauss_spectr_ctl[0], g_pwr->gauss_coefs_prefix_c);
		
		read_real_ctl_item_c(buf, label_gauss_spectr_ctl[1], g_pwr->gauss_coefs_radius_c);
		
		read_int2_clist(fp, buf, label_gauss_spectr_ctl[2], g_pwr->idx_gauss_list);
		read_int_clist(fp, buf, label_gauss_spectr_ctl[3], g_pwr->idx_gauss_l_list);
		read_int_clist(fp, buf, label_gauss_spectr_ctl[4], g_pwr->idx_gauss_m_list);
	};
    g_pwr->iflag_use = 1;
    return;
};

int write_gauss_spectr_control_c(FILE *fp, int level, const char *label, 
                                 struct gauss_spectr_control_c *g_pwr){
    if(g_pwr->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_chara_ctl_item_c(fp, level, g_pwr->maxlen, 
				label_gauss_spectr_ctl[0], g_pwr->gauss_coefs_prefix_c);
	level = write_real_ctl_item_c(fp, level, g_pwr->maxlen, 
				label_gauss_spectr_ctl[1], g_pwr->gauss_coefs_radius_c);
	
	write_int2_clist(fp, level, label_gauss_spectr_ctl[2], g_pwr->idx_gauss_list);
	
	write_int_clist(fp, level, label_gauss_spectr_ctl[3], g_pwr->idx_gauss_l_list);
	write_int_clist(fp, level, label_gauss_spectr_ctl[4], g_pwr->idx_gauss_m_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}

struct layerd_spectr_control_c * init_layerd_spectr_control_c(){
    int i;
    struct layerd_spectr_control_c *lp_ctl;
    if((lp_ctl = (struct layerd_spectr_control_c *) malloc(sizeof(struct layerd_spectr_control_c))) == NULL) {
        printf("malloc error for layerd_spectr_control_c \n");
        exit(0);
    }
    
    lp_ctl->iflag_use = 0;
    lp_ctl->maxlen = 0;
    for (i=0;i<NLBL_LAYERD_SPECTR;i++){
        if(strlen(label_layerd_spectr_ctl[i]) > lp_ctl->maxlen){
            lp_ctl->maxlen = (int) strlen(label_layerd_spectr_ctl[i]);
        };
	};
	
	lp_ctl->layered_pwr_spectr_prefix_c = init_chara_ctl_item_c();
	
	lp_ctl->degree_spectr_switch_c = init_chara_ctl_item_c();
	lp_ctl->order_spectr_switch_c = init_chara_ctl_item_c();
	lp_ctl->diff_lm_spectr_switch_c = init_chara_ctl_item_c();
	lp_ctl->axis_spectr_switch_c = init_chara_ctl_item_c();
	
    lp_ctl->idx_spec_layer_list = init_int_clist();
    sprintf(lp_ctl->idx_spec_layer_list->i1_name, "Radial_ID");
	return lp_ctl;
};

void dealloc_layerd_spectr_control_c(struct layerd_spectr_control_c *lp_ctl){
    dealloc_int_clist(lp_ctl->idx_spec_layer_list);
	
    dealloc_chara_ctl_item_c(lp_ctl->degree_spectr_switch_c);
    dealloc_chara_ctl_item_c(lp_ctl->order_spectr_switch_c);
    dealloc_chara_ctl_item_c(lp_ctl->diff_lm_spectr_switch_c);
    dealloc_chara_ctl_item_c(lp_ctl->axis_spectr_switch_c);
	
	dealloc_chara_ctl_item_c(lp_ctl->layered_pwr_spectr_prefix_c);
    free(lp_ctl);
	return;
};

void read_layerd_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct layerd_spectr_control_c *lp_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_layerd_spectr_ctl[0], lp_ctl->layered_pwr_spectr_prefix_c);
		
		read_chara_ctl_item_c(buf, label_layerd_spectr_ctl[2], lp_ctl->degree_spectr_switch_c);
		read_chara_ctl_item_c(buf, label_layerd_spectr_ctl[3], lp_ctl->order_spectr_switch_c);
		read_chara_ctl_item_c(buf, label_layerd_spectr_ctl[4], lp_ctl->diff_lm_spectr_switch_c);
		read_chara_ctl_item_c(buf, label_layerd_spectr_ctl[5], lp_ctl->axis_spectr_switch_c);
		
		read_int_clist(fp, buf, label_layerd_spectr_ctl[1], lp_ctl->idx_spec_layer_list);
	};
    lp_ctl->iflag_use = 1;
    return;
};

int write_layerd_spectr_control_c(FILE *fp, int level, const char *label, 
                                  struct layerd_spectr_control_c *lp_ctl){
    if(lp_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_chara_ctl_item_c(fp, level, lp_ctl->maxlen, 
				label_layerd_spectr_ctl[0], lp_ctl->layered_pwr_spectr_prefix_c);
	
	write_int_clist(fp, level, label_layerd_spectr_ctl[1], lp_ctl->idx_spec_layer_list);
	
	level = write_chara_ctl_item_c(fp, level, lp_ctl->maxlen, 
				label_layerd_spectr_ctl[2], lp_ctl->degree_spectr_switch_c);
	level = write_chara_ctl_item_c(fp, level, lp_ctl->maxlen, 
				label_layerd_spectr_ctl[3], lp_ctl->order_spectr_switch_c);
	level = write_chara_ctl_item_c(fp, level, lp_ctl->maxlen, 
				label_layerd_spectr_ctl[4], lp_ctl->diff_lm_spectr_switch_c);
	level = write_chara_ctl_item_c(fp, level, lp_ctl->maxlen, 
				label_layerd_spectr_ctl[5], lp_ctl->axis_spectr_switch_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}


struct mid_equator_control_c * init_mid_equator_control_c(){
    int i;
    struct mid_equator_control_c *meq_ctl;
    if((meq_ctl = (struct mid_equator_control_c *) malloc(sizeof(struct mid_equator_control_c))) == NULL) {
        printf("malloc error for mid_equator_control_c \n");
        exit(0);
    }
    
    meq_ctl->iflag_use = 0;
    meq_ctl->maxlen = 0;
    for (i=0;i<NLBL_MID_EQUATOR;i++){
        if(strlen(label_mid_equator_ctl[i]) > meq_ctl->maxlen){
            meq_ctl->maxlen = (int) strlen(label_mid_equator_ctl[i]);
        };
	};
	
	meq_ctl->pick_circle_coord_c = init_chara_ctl_item_c();
	
	meq_ctl->nphi_mid_eq_c = init_int_ctl_item_c();
    meq_ctl->pick_s_c = init_real_ctl_item_c();
    meq_ctl->pick_z_c = init_real_ctl_item_c();
	
	return meq_ctl;
};

void dealloc_mid_equator_control_c(struct mid_equator_control_c *meq_ctl){
	
    dealloc_chara_ctl_item_c(meq_ctl->pick_circle_coord_c);
	
	free(meq_ctl->nphi_mid_eq_c);
	free(meq_ctl->pick_s_c);
	free(meq_ctl->pick_z_c);
	
    free(meq_ctl);
	return;
};
void read_mid_equator_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mid_equator_control_c *meq_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_mid_equator_ctl[0], meq_ctl->pick_circle_coord_c);
		
		read_integer_ctl_item_c(buf, label_mid_equator_ctl[1], meq_ctl->nphi_mid_eq_c);
		read_real_ctl_item_c(buf, label_mid_equator_ctl[2], meq_ctl->pick_s_c);
		read_real_ctl_item_c(buf, label_mid_equator_ctl[3], meq_ctl->pick_z_c);
	};
    meq_ctl->iflag_use = 1;
    return;
}
int write_mid_equator_control_c(FILE *fp, int level, const char *label, 
                                struct mid_equator_control_c *meq_ctl){
    if(meq_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	level = write_chara_ctl_item_c(fp, level, meq_ctl->maxlen, 
				label_mid_equator_ctl[0], meq_ctl->pick_circle_coord_c);
	
	level = write_integer_ctl_item_c(fp, level, meq_ctl->maxlen, 
				label_mid_equator_ctl[1], meq_ctl->nphi_mid_eq_c);
	level = write_real_ctl_item_c(fp, level, meq_ctl->maxlen, 
				label_mid_equator_ctl[2], meq_ctl->pick_s_c);
	level = write_real_ctl_item_c(fp, level, meq_ctl->maxlen, 
				label_mid_equator_ctl[3], meq_ctl->pick_z_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct sph_monitor_control_c * init_sph_monitor_ctl_c(){
    int i;
    struct sph_monitor_control_c *monitor_ctl;
    if((monitor_ctl = (struct sph_monitor_control_c *) malloc(sizeof(struct sph_monitor_control_c))) == NULL) {
        printf("malloc error for sph_monitor_control_c \n");
        exit(0);
    }
    
    
    monitor_ctl->iflag_use = 0;
    monitor_ctl->maxlen = 0;
    for (i=0;i<NLBL_SPH_MONITOR;i++){
        if(strlen(label_sph_monitor_ctl[i]) > monitor_ctl->maxlen){
            monitor_ctl->maxlen = (int) strlen(label_sph_monitor_ctl[i]);
        };
	};
    monitor_ctl->pspec_ctl_c = init_pick_spectr_control_c();
    monitor_ctl->g_pwr = init_gauss_spectr_control_c();
    monitor_ctl->lp_ctl = init_layerd_spectr_control_c();
    monitor_ctl->meq_ctl = init_mid_equator_control_c();
    
    monitor_ctl->volume_average_prefix_c = init_chara_ctl_item_c();
    monitor_ctl->volume_pwr_spectr_prefix_c = init_chara_ctl_item_c();
    monitor_ctl->Nusselt_file_prefix_c = init_chara_ctl_item_c();
	
	init_sph_vol_spectr_list(&monitor_ctl->v_pwr_list);
	return monitor_ctl;
};

void dealloc_sph_monitor_ctl_c(struct sph_monitor_control_c *monitor_ctl){
    dealloc_chara_ctl_item_c(monitor_ctl->volume_average_prefix_c);
    dealloc_chara_ctl_item_c(monitor_ctl->volume_pwr_spectr_prefix_c);
    dealloc_chara_ctl_item_c(monitor_ctl->Nusselt_file_prefix_c);
	
	dealloc_mid_equator_control_c(monitor_ctl->meq_ctl);
	dealloc_pick_spectr_control_c(monitor_ctl->pspec_ctl_c);
	dealloc_gauss_spectr_control_c(monitor_ctl->g_pwr);
	dealloc_layerd_spectr_control_c(monitor_ctl->lp_ctl);
	
	clear_sph_vol_spectr_ctl_list(&monitor_ctl->v_pwr_list);
    free(monitor_ctl);

	return;
};

void read_sph_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct sph_monitor_control_c *monitor_ctl){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_sph_vol_spectr_ctl_list(fp, buf, label_sph_monitor_ctl[0], &monitor_ctl->v_pwr_list);
		
		if(right_begin_flag_c(buf, label_sph_monitor_ctl[1]) > 0) 
		read_layerd_spectr_control_c(fp, buf, label_sph_monitor_ctl[1], monitor_ctl->lp_ctl);
		if(right_begin_flag_c(buf, label_sph_monitor_ctl[2]) > 0) 
		read_gauss_spectr_control_c(fp, buf, label_sph_monitor_ctl[2], monitor_ctl->g_pwr);
		if(right_begin_flag_c(buf, label_sph_monitor_ctl[3]) > 0) 
		read_pick_spectr_control_c(fp, buf, label_sph_monitor_ctl[3], monitor_ctl->pspec_ctl_c);
		if(right_begin_flag_c(buf, label_sph_monitor_ctl[4]) > 0) 
		read_mid_equator_control_c(fp, buf, label_sph_monitor_ctl[4], monitor_ctl->meq_ctl);
		
		read_chara_ctl_item_c(buf, label_sph_monitor_ctl[5], monitor_ctl->volume_average_prefix_c);
        read_chara_ctl_item_c(buf, label_sph_monitor_ctl[6], monitor_ctl->volume_pwr_spectr_prefix_c);
        read_chara_ctl_item_c(buf, label_sph_monitor_ctl[7], monitor_ctl->Nusselt_file_prefix_c);
	};
    monitor_ctl->iflag_use = 1;
    return;
}

int write_sph_monitor_ctl_c(FILE *fp, int level, const char *label,
			struct sph_monitor_control_c *monitor_ctl){
    if(monitor_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    level = write_chara_ctl_item_c(fp, level, monitor_ctl->maxlen, label_sph_monitor_ctl[5],
                                   monitor_ctl->volume_average_prefix_c);
    level = write_chara_ctl_item_c(fp, level, monitor_ctl->maxlen, label_sph_monitor_ctl[6],
                                   monitor_ctl->volume_pwr_spectr_prefix_c);
	level = write_chara_ctl_item_c(fp, level, monitor_ctl->maxlen, label_sph_monitor_ctl[7],
                                   monitor_ctl->Nusselt_file_prefix_c);
	
    level = write_pick_spectr_control_c(fp, level, label_sph_monitor_ctl[3], monitor_ctl->pspec_ctl_c);
    level = write_gauss_spectr_control_c(fp, level, label_sph_monitor_ctl[2], monitor_ctl->g_pwr);
    level = write_layerd_spectr_control_c(fp, level, label_sph_monitor_ctl[1], monitor_ctl->lp_ctl);
    
	level = write_sph_vol_spectr_ctl_list(fp, level, label_sph_monitor_ctl[0], &monitor_ctl->v_pwr_list);
    level = write_mid_equator_control_c(fp, level, label_sph_monitor_ctl[4], monitor_ctl->meq_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
