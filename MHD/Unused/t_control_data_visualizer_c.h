/*
//  t_control_data_visualizer_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/07.
*/

#ifndef t_control_data_visualizer_c_h_
#define t_control_data_visualizer_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_ctl_data_4_platforms_c.h"
#include "t_ctl_data_4_time_steps_c.h"
#include "t_control_data_vizs_c.h"

struct viz_only_ctl_c{
    int maxlen;
	
    struct platform_data_control_c *files;
    struct time_data_control_c *tctl;
    struct visualizers_ctl_c *viz_c;
};

/* prototypes */
void get_label_viz_only_ctl(int index, char *label);

struct viz_only_ctl_c * init_visualizers_ctl_c();
void dealloc_visualizers_ctl_c(struct viz_only_ctl_c *viz_only);
int read_visualizers_ctl_c(FILE *fp, char buf[LENGTHBUF], 
                           const char *label, struct viz_only_ctl_c *viz_only);
int write_visualizers_ctl_c(FILE *fp, int level, const char *label, 
                            struct viz_only_ctl_c *viz_only);

void rename_visualizer_subfiles_c(struct viz_only_ctl_c *viz_only);
int read_visualizers_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct viz_only_ctl_c *viz_only);
int write_visualizers_ctl_file_c(const char *file_name, struct viz_only_ctl_c *viz_only);

#endif /* t_control_data_visualizer_c_h_ */
