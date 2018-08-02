/*
//  t_ctl_data_MHD_boundary_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_MHD_boundary_c.h"

#define NLBL_NODE_BC_CTL      8
#define NLBL_SURF_BC_CTL      9

const char label_MHD_node_bc_ctl[NLBL_NODE_BC_CTL][KCHARA_C] = {
	/*[ 0]*/	{"bc_temperature"},
	/*[ 1]*/	{"bc_velocity"},
	/*[ 2]*/	{"bc_pressure"},
	/*[ 3]*/	{"bc_composition"},
	/*[ 4]*/	{"bc_magnetic_field"},
	/*[ 5]*/	{"bc_electric_potential"},
	/*[ 6]*/	{"bc_vector_potential"},
	/*[ 7]*/	{"bc_current"},
};

const char label_MHD_surf_bc_ctl[NLBL_SURF_BC_CTL][KCHARA_C] = {
	/*[ 0]*/	{"heat_flux_surf"},
	/*[ 1]*/	{"velocity_surf"},
	/*[ 2]*/	{"pressure_surf"},
	/*[ 3]*/	{"composition_flux_surf"},
	/*[ 4]*/	{"magnetic_field_surf"},
	/*[ 5]*/	{"electric_potential_surf"},
	/*[ 6]*/	{"vector_potential_surf"},
	/*[ 7]*/	{"current_surf"},
	/*[ 8]*/	{"infinity_surf"}
};

void alloc_MHD_node_bc_ctl_c(struct MHD_boundary_ctl_c *nod_bc_ctl){
	int i;
	
	nod_bc_ctl->maxlen = 0;
	for (i=0;i<NLBL_NODE_BC_CTL;i++){
		if(strlen(label_MHD_node_bc_ctl[i]) > nod_bc_ctl->maxlen){
			nod_bc_ctl->maxlen = strlen(label_MHD_node_bc_ctl[i]);
		};
	};
	
	nod_bc_ctl->bc_T_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_U_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_P_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_C_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_B_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_MP_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_A_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_J_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	nod_bc_ctl->bc_infty_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	
	return;
};

void alloc_MHD_surf_bc_ctl_c(struct MHD_boundary_ctl_c *surf_bc_ctl){
	int i;
	
	surf_bc_ctl->maxlen = 0;
	for (i=0;i<NLBL_SURF_BC_CTL;i++){
		if(strlen(label_MHD_surf_bc_ctl[i]) > surf_bc_ctl->maxlen){
			surf_bc_ctl->maxlen = strlen(label_MHD_surf_bc_ctl[i]);
		};
	};
	
	surf_bc_ctl->bc_T_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_U_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_P_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_C_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_B_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_MP_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_A_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_J_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	surf_bc_ctl->bc_infty_ctl = (struct chara2_real_ctl_array *) malloc(sizeof(struct chara2_real_ctl_array));
	
	return;
};

void dealloc_MHD_boundary_ctl_c(struct MHD_boundary_ctl_c *bc_ctl){
	
	dealloc_ctl_c2r_array(bc_ctl->bc_T_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_U_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_P_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_C_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_B_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_MP_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_A_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_J_ctl);
	dealloc_ctl_c2r_array(bc_ctl->bc_infty_ctl);
	
	free(bc_ctl->bc_T_ctl);
	free(bc_ctl->bc_U_ctl);
	free(bc_ctl->bc_P_ctl);
	free(bc_ctl->bc_C_ctl);
	free(bc_ctl->bc_B_ctl);
	free(bc_ctl->bc_MP_ctl);
	free(bc_ctl->bc_A_ctl);
	free(bc_ctl->bc_J_ctl);
	free(bc_ctl->bc_infty_ctl);
	
	return;
};

int read_MHD_node_bc_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct MHD_boundary_ctl_c *nod_bc_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 0], nod_bc_ctl->bc_T_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 1], nod_bc_ctl->bc_U_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 2], nod_bc_ctl->bc_P_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 3], nod_bc_ctl->bc_C_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 4], nod_bc_ctl->bc_B_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 5], nod_bc_ctl->bc_MP_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 6], nod_bc_ctl->bc_A_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_node_bc_ctl[ 7], nod_bc_ctl->bc_J_ctl);
	};
	return 1;
};

int read_MHD_surf_bc_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct MHD_boundary_ctl_c *surf_bc_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);
		
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 0], surf_bc_ctl->bc_T_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 1], surf_bc_ctl->bc_U_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 2], surf_bc_ctl->bc_P_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 3], surf_bc_ctl->bc_C_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 4], surf_bc_ctl->bc_B_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 5], surf_bc_ctl->bc_MP_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 6], surf_bc_ctl->bc_A_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 7], surf_bc_ctl->bc_J_ctl);
		read_c2r_ctl_array_c(fp, buf, label_MHD_surf_bc_ctl[ 8], surf_bc_ctl->bc_infty_ctl);
	};
	return 1;
};

int write_MHD_node_bc_ctl_c(FILE *fp, int level, int *iflag,
			const char *label, struct MHD_boundary_ctl_c *nod_bc_ctl){
	
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[0]),
				label_MHD_node_bc_ctl[0], nod_bc_ctl->bc_T_ctl);
	
	if(nod_bc_ctl->bc_U_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[1]),
				label_MHD_node_bc_ctl[1], nod_bc_ctl->bc_U_ctl);
	
	if(nod_bc_ctl->bc_P_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[2]),
				label_MHD_node_bc_ctl[2], nod_bc_ctl->bc_P_ctl);
	
	if(nod_bc_ctl->bc_C_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[3]),
				label_MHD_node_bc_ctl[3], nod_bc_ctl->bc_C_ctl);
	
	if(nod_bc_ctl->bc_B_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[4]),
				label_MHD_node_bc_ctl[4], nod_bc_ctl->bc_B_ctl);
	
	if(nod_bc_ctl->bc_MP_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[5]),
				label_MHD_node_bc_ctl[5], nod_bc_ctl->bc_MP_ctl);
	
	if(nod_bc_ctl->bc_A_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[6]),
				label_MHD_node_bc_ctl[6], nod_bc_ctl->bc_A_ctl);
	
	if(nod_bc_ctl->bc_J_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_node_bc_ctl[7]),
				label_MHD_node_bc_ctl[7], nod_bc_ctl->bc_J_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

int write_MHD_surf_bc_ctl_c(FILE *fp, int level, int *iflag,
			const char *label, struct MHD_boundary_ctl_c *surf_bc_ctl){
	
	if(*iflag == 0) return level;
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[0]),
				label_MHD_surf_bc_ctl[0], surf_bc_ctl->bc_T_ctl);
	
	if(surf_bc_ctl->bc_U_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[1]),
				label_MHD_surf_bc_ctl[1], surf_bc_ctl->bc_U_ctl);
	
	if(surf_bc_ctl->bc_P_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[2]),
				label_MHD_surf_bc_ctl[2], surf_bc_ctl->bc_P_ctl);
	
	if(surf_bc_ctl->bc_C_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[3]),
				label_MHD_surf_bc_ctl[3], surf_bc_ctl->bc_C_ctl);
	
	if(surf_bc_ctl->bc_B_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[4]),
				label_MHD_surf_bc_ctl[4], surf_bc_ctl->bc_B_ctl);
	
	if(surf_bc_ctl->bc_MP_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[5]),
				label_MHD_surf_bc_ctl[5], surf_bc_ctl->bc_MP_ctl);
	
	if(surf_bc_ctl->bc_A_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[6]),
				label_MHD_surf_bc_ctl[6], surf_bc_ctl->bc_A_ctl);
	
	if(surf_bc_ctl->bc_J_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[7]),
				label_MHD_surf_bc_ctl[7], surf_bc_ctl->bc_J_ctl);
	
	if(surf_bc_ctl->bc_infty_ctl->num > 0) fprintf(fp, "!\n");
	write_c2r_ctl_array_c(fp, level, strlen(label_MHD_surf_bc_ctl[8]),
				label_MHD_surf_bc_ctl[8], surf_bc_ctl->bc_infty_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


