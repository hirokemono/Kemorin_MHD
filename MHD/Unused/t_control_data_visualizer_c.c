/*
//  t_control_data_visualizer_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#include "t_control_data_visualizer_c.h"

#define NLBL_VIZ_ONLY_CTL  3

FILE *FP_VIZ;

const char label_viz_only_ctl[NLBL_VIZ_ONLY_CTL][KCHARA_C] = {
    /*[ 0]*/    {"data_files_def"},
    /*[ 1]*/    {"time_step_ctl"},
    /*[ 2]*/    {"visual_control"}
};

const char label_viz_only_head[KCHARA_C] = "visualizer";


void get_label_viz_only_ctl(int index, char *label){
    if(index < NLBL_VIZ_ONLY_CTL) strngcopy(label, label_viz_only_ctl[index]);
    return;
};

struct viz_only_ctl_c * init_visualizers_ctl_c(){
    struct viz_only_ctl_c *viz_only;
    if ((viz_only = (struct viz_only_ctl_c *) malloc(sizeof(struct viz_only_ctl_c))) == NULL) {
        printf("malloc error for viz_only_ctl_c\n");
        exit(0);
    }
	viz_only->files = init_platform_data_control_c();
	viz_only->tctl = init_time_data_control_c();
	viz_only->viz_c = init_vizs_ctl_c();
	return viz_only;
};

void dealloc_visualizers_ctl_c(struct viz_only_ctl_c *viz_only){
	dealloc_platform_data_control_c(viz_only->files);
	dealloc_time_data_control_c(viz_only->tctl);
	dealloc_vizs_ctl_c(viz_only->viz_c);
    free(viz_only);
    return;
};

int read_visualizers_ctl_c(FILE *fp, char buf[LENGTHBUF], 
                           const char *label, struct viz_only_ctl_c *viz_only){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_viz_only_ctl[ 0]) > 0){
			read_platform_data_control_c(fp, buf, label_viz_only_ctl[ 0], viz_only->files);
            };
		if(right_begin_flag_c(buf, label_viz_only_ctl[ 1]) > 0){
			read_time_data_control_c(fp, buf, label_viz_only_ctl[ 1], viz_only->tctl);
            };
		if(right_begin_flag_c(buf, label_viz_only_ctl[ 2]) > 0){
			read_vizs_ctl_c(fp, buf, label_viz_only_ctl[ 2], viz_only->viz_c);
            };
	};
	return 1;
};

int write_visualizers_ctl_c(FILE *fp, int level, const char *label, 
                            struct viz_only_ctl_c *viz_only){
	level = write_begin_flag_for_ctl_c(fp, level, label);
    level = write_platform_data_control_c(fp, level, 
					label_viz_only_ctl[ 0], viz_only->files);
    level = write_time_data_control_c(fp, level, 
					label_viz_only_ctl[ 1], viz_only->tctl);
    level = write_vizs_ctl_c(fp, level, 
					label_viz_only_ctl[ 2], viz_only->viz_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_visualizer_subfiles_c(struct viz_only_ctl_c *viz_only){
    rename_vizs_ctl_subfiles(viz_only->viz_c);
    return;
};

int read_visualizers_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct viz_only_ctl_c *viz_only){
    int iflag = 0;
    
    printf("Read Visualizer control: %s\n", file_name);
    if ((FP_VIZ = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_read_line(FP_VIZ, buf);
    if(right_begin_flag_c(buf, label_viz_only_head) > 0){
        iflag = read_visualizers_ctl_c(FP_VIZ, buf, label_viz_only_head, viz_only);
    };
    fclose(FP_VIZ);
	
	read_vizs_ctl_files_c(buf, viz_only->viz_c);
	
    return iflag;
};

int write_visualizers_ctl_file_c(const char *file_name, struct viz_only_ctl_c *viz_only){
    int level = 0;
    
	write_vizs_ctl_files_c(viz_only->viz_c);
    
    printf("Write Visualizer control: %s\n", file_name);
    if ((FP_VIZ = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    level = write_visualizers_ctl_c(FP_VIZ, 0, label_viz_only_head, viz_only);
    fclose(FP_VIZ);
    
    return level;
};

