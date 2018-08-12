/*
//  t_ctl_data_node_monitor_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#include "t_ctl_data_node_monitor_c.h"

#define NLBL_NODE_MONITOR 8

const char label_node_monitor_ctl[NLBL_NODE_MONITOR][KCHARA_C] = {
    /*[ 0]*/    {"monitor_grp_ctl"},
    /*[ 1]*/    {"monitor_position_list"},
    /*[ 2]*/    {"monitor_node_list"}
};


void get_label_node_monitor_ctl(int index, char *label){
    if(index < NLBL_NODE_MONITOR) strngcopy(label, label_node_monitor_ctl[index]);
    return;
};

void alloc_node_monitor_ctl_c(struct node_monitor_ctl_c *nmtr_ctl){
    int i;
    
    nmtr_ctl->maxlen = 0;
    for (i=0;i<NLBL_NODE_MONITOR;i++){
        if(strlen(label_node_monitor_ctl[i]) > nmtr_ctl->maxlen){
            nmtr_ctl->maxlen = (int) strlen(label_node_monitor_ctl[i]);
        };
	};
	
	nmtr_ctl->group_4_monitor_ctl = (struct chara_ctl_array *) malloc(sizeof(struct chara_ctl_array));
	nmtr_ctl->node_4_monitor_ctl = (struct int2_ctl_array *) malloc(sizeof(struct int2_ctl_array));
    init_ctl_chara_array(nmtr_ctl->group_4_monitor_ctl);
    init_real3_ctl_list(&nmtr_ctl->xx_4_monitor_list);
    init_ctl_int2_array(nmtr_ctl->node_4_monitor_ctl);
};

void dealloc_node_monitor_ctl_c(struct node_monitor_ctl_c *nmtr_ctl){
    dealloc_ctl_chara_array(nmtr_ctl->group_4_monitor_ctl);
    clear_real3_ctl_list(&nmtr_ctl->xx_4_monitor_list);
    dealloc_ctl_int2_array(nmtr_ctl->node_4_monitor_ctl);
	
    free(nmtr_ctl->group_4_monitor_ctl);
    free(nmtr_ctl->node_4_monitor_ctl);
};

int read_node_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct node_monitor_ctl_c *nmtr_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_character_ctl_array_c(fp, buf, label_node_monitor_ctl[0], nmtr_ctl->group_4_monitor_ctl);
		read_real3_ctl_list(fp, buf, label_node_monitor_ctl[1], &nmtr_ctl->xx_4_monitor_list);
		read_int2_ctl_array_c(fp, buf, label_node_monitor_ctl[2], nmtr_ctl->node_4_monitor_ctl);
	};
	return 1;
};

int write_node_monitor_ctl_c(FILE *fp, int level, const char *label, 
			struct node_monitor_ctl_c *nmtr_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_array_c(fp, level, (int) strlen(label_node_monitor_ctl[0]),
			label_node_monitor_ctl[0], nmtr_ctl->group_4_monitor_ctl);
	
	write_real3_ctl_list(fp, level, label_node_monitor_ctl[1], &nmtr_ctl->xx_4_monitor_list);
	
	if(nmtr_ctl->node_4_monitor_ctl->num > 0) fprintf(fp, "!\n");
	write_int2_ctl_array_c(fp, level, (int) strlen(label_node_monitor_ctl[2]),
			label_node_monitor_ctl[2], nmtr_ctl->node_4_monitor_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
