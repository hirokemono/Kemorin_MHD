/*
//  t_control_data_gen_sph_grid_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#include "t_control_data_gen_sph_grid_c.h"
#define NLBL_GEN_SPH_GRID_CTL  2

FILE *FP_GRID;

const char label_gen_sph_grid_ctl[NLBL_GEN_SPH_GRID_CTL][KCHARA_C] = {
    /*[ 0]*/    {"data_files_def"},
    /*[ 1]*/    {"spherical_shell_ctl"}
};

const char label_gen_sph_grid_head[KCHARA_C] = "control_MHD";


void get_label_gen_sph_grid_ctl(int index, char *label){
    if(index < NLBL_GEN_SPH_GRID_CTL) strngcopy(label, label_gen_sph_grid_ctl[index]);
    return;
};

struct gen_sph_grid_ctl_c * init_gen_sph_shell_ctl_c(){
	int i;
    struct gen_sph_grid_ctl_c *gen_sph_c;
    if ((gen_sph_c = (struct gen_sph_grid_ctl_c *) malloc(sizeof(struct gen_sph_grid_ctl_c))) == NULL) {
        printf("malloc error for gen_sph_grid_ctl_c\n");
        exit(0);
    }
	gen_sph_c->maxlen = 0;
	for (i=0;i<NLBL_GEN_SPH_GRID_CTL;i++){
		if(strlen(label_gen_sph_grid_ctl[i]) > gen_sph_c->maxlen){
			gen_sph_c->maxlen = (int) strlen(label_gen_sph_grid_ctl[i]);
		};
	};

	gen_sph_c->files = init_platform_data_control_c();
	gen_sph_c->shell_ctl = init_parallel_sph_shell_control_c();
	return gen_sph_c;
};

void dealloc_gen_sph_shell_ctl_c(struct gen_sph_grid_ctl_c *gen_sph_c){
	dealloc_platform_data_control_c(gen_sph_c->files);
	dealloc_parallel_sph_shell_control_c(gen_sph_c->shell_ctl);

    free(gen_sph_c);
    return;
};

int read_gen_sph_shell_ctl_c(FILE *fp, char buf[LENGTHBUF], 
                           const char *label, struct gen_sph_grid_ctl_c *gen_sph_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_gen_sph_grid_ctl[ 0]) > 0){
			read_platform_data_control_c(fp, buf, label_gen_sph_grid_ctl[ 0], gen_sph_c->files);
		};
		if(right_begin_flag_c(buf, label_gen_sph_grid_ctl[ 1]) > 0){
			read_spherical_shell_ctl_c(fp, buf, label_gen_sph_grid_ctl[ 1], gen_sph_c->shell_ctl);
		} else if(right_file_flag_c(buf, label_gen_sph_grid_ctl[ 1])){
			gen_sph_c->shell_ctl->iflag_use_file = read_file_flag_c(buf, gen_sph_c->fname_shell_ctl);
		};
	};
	return 1;
};

int write_gen_sph_shell_ctl_c(FILE *fp, int level, const char *label, 
                            struct gen_sph_grid_ctl_c *gen_sph_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
    level = write_platform_data_control_c(fp, level, 
					label_gen_sph_grid_ctl[ 0], gen_sph_c->files);
	if(gen_sph_c->shell_ctl->iflag_use_file == -1){
        fprintf(fp, "!\n");
        write_file_flag_for_ctl_c(fp, level, 
                                  label_gen_sph_grid_ctl[ 1], gen_sph_c->fname_shell_ctl);
    } else {
		level = write_spherical_shell_ctl_c(fp, level, 
					label_gen_sph_grid_ctl[ 1], gen_sph_c->shell_ctl);
	};
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_gen_sph_subfiles_c(struct gen_sph_grid_ctl_c *gen_sph_c){
    if(gen_sph_c->shell_ctl->iflag_use_file ==-1){
        strcat(gen_sph_c->fname_shell_ctl, "_2");
    }
    return;
};

int read_gen_sph_shell_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct gen_sph_grid_ctl_c *gen_sph_c){
    int iflag = 0;
    
    printf("Read spherical shell generation control: %s\n", file_name);
    if ((FP_GRID = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_read_line(FP_GRID, buf);
    if(right_begin_flag_c(buf, label_gen_sph_grid_head) > 0){
        iflag = read_gen_sph_shell_ctl_c(FP_GRID, buf, label_gen_sph_grid_head, gen_sph_c);
    };
    fclose(FP_GRID);
	
    read_spherical_shell_file_c(gen_sph_c->fname_shell_ctl, buf, gen_sph_c->shell_ctl);
	
    return iflag;
};

int write_gen_sph_shell_file_c(const char *file_name, struct gen_sph_grid_ctl_c *gen_sph_c){
    int level = 0;
    
    write_spherical_shell_file_c(gen_sph_c->fname_shell_ctl, gen_sph_c->shell_ctl);
    
    printf("Write spherical shell generation control: %s\n", file_name);
    if ((FP_GRID = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    level = write_gen_sph_shell_ctl_c(FP_GRID, 0, label_gen_sph_grid_head, gen_sph_c);
    fclose(FP_GRID);
    
    return level;
};
