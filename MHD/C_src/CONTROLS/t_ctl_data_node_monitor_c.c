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
	
    nmtr_ctl->group_4_monitor_list = (struct chara_clist *) malloc(sizeof(struct chara_clist));
    init_chara_clist(nmtr_ctl->group_4_monitor_list);

    nmtr_ctl->xx_4_monitor_list = (struct real3_clist *) malloc(sizeof(struct real3_clist));
    init_real3_clist(nmtr_ctl->xx_4_monitor_list);
    sprintf(nmtr_ctl->xx_4_monitor_list->r1_name, "x");
    sprintf(nmtr_ctl->xx_4_monitor_list->r2_name, "y");
    sprintf(nmtr_ctl->xx_4_monitor_list->r3_name, "z");
    
    nmtr_ctl->node_4_monitor_list = (struct int2_clist *) malloc(sizeof(struct int2_clist));
    init_int2_clist(nmtr_ctl->node_4_monitor_list);
    return;
};

void dealloc_node_monitor_ctl_c(struct node_monitor_ctl_c *nmtr_ctl){
    clear_chara_clist(nmtr_ctl->group_4_monitor_list);
    free(nmtr_ctl->group_4_monitor_list);
    clear_real3_clist(nmtr_ctl->xx_4_monitor_list);
    free(nmtr_ctl->xx_4_monitor_list);
    clear_int2_clist(nmtr_ctl->node_4_monitor_list);
    free(nmtr_ctl->node_4_monitor_list);
	
    return;
};

int read_node_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct node_monitor_ctl_c *nmtr_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_clist(fp, buf, label_node_monitor_ctl[0], nmtr_ctl->group_4_monitor_list);
		read_real3_clist(fp, buf, label_node_monitor_ctl[1], nmtr_ctl->xx_4_monitor_list);
		read_int2_clist(fp, buf, label_node_monitor_ctl[2], nmtr_ctl->node_4_monitor_list);
	};
	return 1;
};

int write_node_monitor_ctl_c(FILE *fp, int level, const char *label, 
			struct node_monitor_ctl_c *nmtr_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_clist(fp, level, label_node_monitor_ctl[0], nmtr_ctl->group_4_monitor_list);
	
	write_real3_clist(fp, level, label_node_monitor_ctl[1], nmtr_ctl->xx_4_monitor_list);
	write_int2_clist(fp, level, label_node_monitor_ctl[2], nmtr_ctl->node_4_monitor_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
