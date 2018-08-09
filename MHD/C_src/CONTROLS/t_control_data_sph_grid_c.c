/*
//  t_control_data_sph_grid_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#include "t_control_data_sph_grid_c.h"

#define NLBL_SPH_SHELL_CTL      3

FILE *FP_Shell;

const char label_sph_shell_ctl[NLBL_SPH_SHELL_CTL][KCHARA_C] = {
	/*[ 0]*/	{"FEM_mesh_ctl"},
	/*[ 1]*/	{"num_domain_ctl"},
	/*[ 2]*/	{"num_grid_sph"}
};

const char label_sph_shell_head[KCHARA_C] = "spherical_shell_ctl";

void alloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	int i;
	
	shell_ctl->maxlen = 0;
	for (i=0;i<NLBL_SPH_SHELL_CTL;i++){
		if(strlen(label_sph_shell_ctl[i]) > shell_ctl->maxlen){
			shell_ctl->maxlen = (int) strlen(label_sph_shell_ctl[i]);
		};
	};
	
	
	shell_ctl->Fmesh_ctl = (struct FEM_mesh_control_c *) malloc(sizeof(struct FEM_mesh_control_c));
	alloc_FEM_mesh_control_c(shell_ctl->Fmesh_ctl);
	
	shell_ctl->sdctl_c = (struct sphere_domain_ctl_c *) malloc(sizeof(struct sphere_domain_ctl_c));
	alloc_sphere_domain_ctl_c(shell_ctl->sdctl_c);
	shell_ctl->spctl_c = (struct sphere_data_ctl_c *) malloc(sizeof(struct sphere_data_ctl_c));
	alloc_sphere_data_ctl_c(shell_ctl->spctl_c);
	
	return;
}

void dealloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	shell_ctl->iflag_FEM_mesh_ctl = 0;
	shell_ctl->iflag_sph_domain =   0;
	shell_ctl->iflag_sph_shell =    0;
	
	dealloc_FEM_mesh_control_c(shell_ctl->Fmesh_ctl);
	free(shell_ctl->Fmesh_ctl);
	dealloc_sphere_domain_ctl_c(shell_ctl->sdctl_c);
	free(shell_ctl->sdctl_c);
	dealloc_sphere_data_ctl_c(shell_ctl->spctl_c);
	free(shell_ctl->spctl_c);
	return;
}

int read_spherical_shell_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct parallel_sph_shell_control_c *shell_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_sph_shell_ctl[0]) > 0){
			shell_ctl->iflag_FEM_mesh_ctl = read_FEM_mesh_control_c(fp, buf,
						label_sph_shell_ctl[0], shell_ctl->Fmesh_ctl);
		};
		if(right_begin_flag_c(buf, label_sph_shell_ctl[1]) > 0){
			shell_ctl->iflag_sph_domain = read_sphere_domain_ctl_c(fp, buf,
						label_sph_shell_ctl[1], shell_ctl->sdctl_c);
		};
		if(right_begin_flag_c(buf, label_sph_shell_ctl[2]) > 0){
			shell_ctl->iflag_sph_shell = read_sphere_data_ctl_c(fp, buf,
						label_sph_shell_ctl[2], shell_ctl->spctl_c);
		};
	};
	return 1;
}

int write_spherical_shell_ctl_c(FILE *fp, int level, const char *label, 
                                struct parallel_sph_shell_control_c *shell_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    if(shell_ctl->iflag_FEM_mesh_ctl > 0){
        level = write_FEM_mesh_control_c(fp, level, label_sph_shell_ctl[0], shell_ctl->Fmesh_ctl);
    };
	if(shell_ctl->iflag_sph_domain > 0){
		fprintf(fp, "!\n");
		level = write_sphere_domain_ctl_c(fp, level, label_sph_shell_ctl[1], shell_ctl->sdctl_c);
	};
    if(shell_ctl->iflag_sph_shell > 0){
        fprintf(fp, "!\n");
        level = write_sphere_data_ctl_c(fp, level, label_sph_shell_ctl[2], shell_ctl->spctl_c);
    };
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}

int read_spherical_shell_file_c(const char *file_name, char buf[LENGTHBUF],
			struct parallel_sph_shell_control_c *shell_ctl){
	int iflag = 0;
	
    printf("Read spherical shell definition file: %s\n", file_name);
	if ((FP_Shell = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_Shell, buf);
	if(right_begin_flag_c(buf, label_sph_shell_head) > 0){
		iflag = read_spherical_shell_ctl_c(FP_Shell, buf, label_sph_shell_head, shell_ctl);
	};
	fclose(FP_Shell);
	
	return iflag;
};

int write_spherical_shell_file_c(const char *file_name, 
			struct parallel_sph_shell_control_c *shell_ctl){
	int level;
	
    printf("Write spherical shell definition file: %s\n", file_name);
	if ((FP_Shell = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	};
	
	level = write_spherical_shell_ctl_c(FP_Shell, 0, label_sph_shell_head, shell_ctl);
	fclose(FP_Shell);
	
	return level;
};
