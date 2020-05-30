/*
//  t_control_data_sph_grid_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#include "t_control_data_sph_grid_c.h"


FILE *FP_Shell;

const char label_sph_shell_ctl[NLBL_SPH_SHELL_CTL][KCHARA_C] = {
	/*[ 0]*/	{"FEM_mesh_ctl"},
	/*[ 1]*/	{"num_domain_ctl"},
	/*[ 2]*/	{"num_grid_sph"}
};

const char label_sph_shell_head[KCHARA_C] = "spherical_shell_ctl";


void get_label_sph_shell_ctl(int index, char *label){
    if(index < NLBL_SPH_SHELL_CTL) strngcopy(label, label_sph_shell_ctl[index]);
    return;
};

struct parallel_sph_shell_control_c * init_parallel_sph_shell_control_c(){
	int i;
    struct parallel_sph_shell_control_c *shell_ctl;
    if((shell_ctl = (struct parallel_sph_shell_control_c *) malloc(sizeof(struct parallel_sph_shell_control_c))) == NULL) {
        printf("malloc error for parallel_sph_shell_control_c \n");
        exit(0);
    }
	
    shell_ctl->iflag_use_file = 0;
	shell_ctl->maxlen = 0;
	for (i=0;i<NLBL_SPH_SHELL_CTL;i++){
		if(strlen(label_sph_shell_ctl[i]) > shell_ctl->maxlen){
			shell_ctl->maxlen = (int) strlen(label_sph_shell_ctl[i]);
		};
	};
	
	
	shell_ctl->Fmesh_ctl = init_FEM_mesh_control_c();
	
	shell_ctl->sdctl_c = init_sphere_domain_ctl_c();
	shell_ctl->spctl_c = init_sphere_data_ctl_c();
	
	return shell_ctl;
}

void dealloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl){
	dealloc_FEM_mesh_control_c(shell_ctl->Fmesh_ctl);
	dealloc_sphere_domain_ctl_c(shell_ctl->sdctl_c);
	dealloc_sphere_data_ctl_c(shell_ctl->spctl_c);

    free(shell_ctl);
	return;
}

void read_spherical_shell_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct parallel_sph_shell_control_c *shell_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_sph_shell_ctl[0]) > 0){
			read_FEM_mesh_control_c(fp, buf, label_sph_shell_ctl[0], shell_ctl->Fmesh_ctl);
		};
		if(right_begin_flag_c(buf, label_sph_shell_ctl[1]) > 0){
			read_sphere_domain_ctl_c(fp, buf, label_sph_shell_ctl[1], shell_ctl->sdctl_c);
		};
		if(right_begin_flag_c(buf, label_sph_shell_ctl[2]) > 0){
			read_sphere_data_ctl_c(fp, buf, label_sph_shell_ctl[2], shell_ctl->spctl_c);
		};
	};
    shell_ctl->iflag_use_file = 1;
	return;
}

int write_spherical_shell_ctl_c(FILE *fp, int level, const char *label, 
                                struct parallel_sph_shell_control_c *shell_ctl){
    if(shell_ctl->iflag_use_file == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
    
    level = write_FEM_mesh_control_c(fp, level, label_sph_shell_ctl[0], shell_ctl->Fmesh_ctl);
    level = write_sphere_domain_ctl_c(fp, level, label_sph_shell_ctl[1], shell_ctl->sdctl_c);
    level = write_sphere_data_ctl_c(fp, level, label_sph_shell_ctl[2], shell_ctl->spctl_c);
    
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}

void read_spherical_shell_file_c(const char *file_name, char buf[LENGTHBUF],
			struct parallel_sph_shell_control_c *shell_ctl){
    if(shell_ctl->iflag_use_file != -1) return;
    
    printf("Read spherical shell definition file: %s\n", file_name);
	if ((FP_Shell = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	skip_comment_read_line(FP_Shell, buf);
	if(right_begin_flag_c(buf, label_sph_shell_head) > 0){
		read_spherical_shell_ctl_c(FP_Shell, buf, label_sph_shell_head, shell_ctl);
	};
	fclose(FP_Shell);
    shell_ctl->iflag_use_file = -1;
	return;
};

int write_spherical_shell_file_c(const char *file_name, 
			struct parallel_sph_shell_control_c *shell_ctl){
	int level;
	
    if(shell_ctl->iflag_use_file != -1) return 0;
    
    printf("Write spherical shell definition file: %s\n", file_name);
	if ((FP_Shell = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		exit (2);                    /* terminate with error message */
	};
	
	level = write_spherical_shell_ctl_c(FP_Shell, 0, label_sph_shell_head, shell_ctl);
	fclose(FP_Shell);
	
	return level;
};
