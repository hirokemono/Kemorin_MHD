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


void get_label_MHD_node_bc_ctl(int index, char *label){
    if(index < NLBL_NODE_BC_CTL) strngcopy(label, label_MHD_node_bc_ctl[index]);
    return;
};
void get_label_MHD_surf_bc_ctl(int index, char *label){
    if(index < NLBL_SURF_BC_CTL) strngcopy(label, label_MHD_surf_bc_ctl[index]);
    return;
};


static void alloc_MHD_bc_ctl_c(struct MHD_boundary_ctl_c *nod_bc_ctl){
    nod_bc_ctl->bc_T_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_U_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_P_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_C_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_B_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_MP_ctl =    init_chara2_real_clist();
    nod_bc_ctl->bc_A_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_J_ctl =     init_chara2_real_clist();
    nod_bc_ctl->bc_infty_ctl = init_chara2_real_clist();
    return;
};

struct MHD_boundary_ctl_c *init_MHD_node_bc_ctl_c(){
	int i;
    struct MHD_boundary_ctl_c *nod_bc_ctl;
    if ((nod_bc_ctl = (struct MHD_boundary_ctl_c *) malloc(sizeof(struct MHD_boundary_ctl_c))) == NULL) {
        printf("malloc error for MHD_boundary_ctl_c\n");
        exit(0);
    }
    nod_bc_ctl->iflag_use = 0;
	nod_bc_ctl->maxlen = 0;
	for (i=0;i<NLBL_NODE_BC_CTL;i++){
		if(strlen(label_MHD_node_bc_ctl[i]) > nod_bc_ctl->maxlen){
			nod_bc_ctl->maxlen = (int) strlen(label_MHD_node_bc_ctl[i]);
		};
	};
    alloc_MHD_bc_ctl_c(nod_bc_ctl);
	return nod_bc_ctl;
};

struct MHD_boundary_ctl_c * init_MHD_surf_bc_ctl_c(){
	int i;
    struct MHD_boundary_ctl_c *surf_bc_ctl;
    if ((surf_bc_ctl = (struct MHD_boundary_ctl_c *) malloc(sizeof(struct MHD_boundary_ctl_c))) == NULL) {
        printf("malloc error for MHD_boundary_ctl_c\n");
        exit(0);
    }
    surf_bc_ctl->iflag_use = 0;
	surf_bc_ctl->maxlen = 0;
	for (i=0;i<NLBL_SURF_BC_CTL;i++){
		if(strlen(label_MHD_surf_bc_ctl[i]) > surf_bc_ctl->maxlen){
			surf_bc_ctl->maxlen = (int) strlen(label_MHD_surf_bc_ctl[i]);
		};
	};
    alloc_MHD_bc_ctl_c(surf_bc_ctl);
	return surf_bc_ctl;
};

void dealloc_MHD_boundary_ctl_c(struct MHD_boundary_ctl_c *bc_ctl){
	
	dealloc_chara2_real_clist(bc_ctl->bc_T_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_U_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_P_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_C_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_B_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_MP_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_A_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_J_ctl);
	dealloc_chara2_real_clist(bc_ctl->bc_infty_ctl);

    free(bc_ctl);
	return;
};

void read_MHD_node_bc_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct MHD_boundary_ctl_c *nod_bc_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 0], nod_bc_ctl->bc_T_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 1], nod_bc_ctl->bc_U_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 2], nod_bc_ctl->bc_P_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 3], nod_bc_ctl->bc_C_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 4], nod_bc_ctl->bc_B_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 5], nod_bc_ctl->bc_MP_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 6], nod_bc_ctl->bc_A_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_node_bc_ctl[ 7], nod_bc_ctl->bc_J_ctl);
	};
    nod_bc_ctl->iflag_use = 1;
	return;
};

void read_MHD_surf_bc_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct MHD_boundary_ctl_c *surf_bc_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 0], surf_bc_ctl->bc_T_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 1], surf_bc_ctl->bc_U_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 2], surf_bc_ctl->bc_P_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 3], surf_bc_ctl->bc_C_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 4], surf_bc_ctl->bc_B_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 5], surf_bc_ctl->bc_MP_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 6], surf_bc_ctl->bc_A_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 7], surf_bc_ctl->bc_J_ctl);
		read_chara2_real_clist(fp, buf, label_MHD_surf_bc_ctl[ 8], surf_bc_ctl->bc_infty_ctl);
	};
    surf_bc_ctl->iflag_use = 1;
	return;
};

int write_MHD_node_bc_ctl_c(FILE *fp, int level, const char *label,
                            struct MHD_boundary_ctl_c *nod_bc_ctl){
    if(nod_bc_ctl->iflag_use == 0) return level;
	
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[0], nod_bc_ctl->bc_T_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[1], nod_bc_ctl->bc_U_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[2], nod_bc_ctl->bc_P_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[3], nod_bc_ctl->bc_C_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[4], nod_bc_ctl->bc_B_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[5], nod_bc_ctl->bc_MP_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[6], nod_bc_ctl->bc_A_ctl);
	write_chara2_real_clist(fp, level, label_MHD_node_bc_ctl[7], nod_bc_ctl->bc_J_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

int write_MHD_surf_bc_ctl_c(FILE *fp, int level, const char *label,
                            struct MHD_boundary_ctl_c *surf_bc_ctl){
    if(surf_bc_ctl->iflag_use == 0) return level;
	
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[0], surf_bc_ctl->bc_T_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[1], surf_bc_ctl->bc_U_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[2], surf_bc_ctl->bc_P_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[3], surf_bc_ctl->bc_C_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[4], surf_bc_ctl->bc_B_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[5], surf_bc_ctl->bc_MP_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[6], surf_bc_ctl->bc_A_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[7], surf_bc_ctl->bc_J_ctl);
	write_chara2_real_clist(fp, level, label_MHD_surf_bc_ctl[8], surf_bc_ctl->bc_infty_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


