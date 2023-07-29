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

struct node_monitor_ctl_c * init_node_monitor_ctl_c(){
    int i;
    struct node_monitor_ctl_c *nmtr_ctl;
    if((nmtr_ctl = (struct node_monitor_ctl_c *) malloc(sizeof(struct node_monitor_ctl_c))) == NULL) {
        printf("malloc error for node_monitor_ctl_c \n");
        exit(0);
    }
    
    nmtr_ctl->iflag_use = 0;
    nmtr_ctl->maxlen = 0;
    for (i=0;i<NLBL_NODE_MONITOR;i++){
        if(strlen(label_node_monitor_ctl[i]) > nmtr_ctl->maxlen){
            nmtr_ctl->maxlen = (int) strlen(label_node_monitor_ctl[i]);
        };
	};
	
    nmtr_ctl->group_4_monitor_list = init_chara_clist();

    nmtr_ctl->xx_4_monitor_list = init_real3_clist();
    sprintf(nmtr_ctl->xx_4_monitor_list->r1_name, "x");
    sprintf(nmtr_ctl->xx_4_monitor_list->r2_name, "y");
    sprintf(nmtr_ctl->xx_4_monitor_list->r3_name, "z");
    
    nmtr_ctl->node_4_monitor_list = init_int2_clist();
    sprintf(nmtr_ctl->node_4_monitor_list->i1_name, "Index");
    sprintf(nmtr_ctl->node_4_monitor_list->i2_name, "Node_ID");
    return nmtr_ctl;
};

void dealloc_node_monitor_ctl_c(struct node_monitor_ctl_c *nmtr_ctl){
    dealloc_chara_clist(nmtr_ctl->group_4_monitor_list);
    dealloc_real3_clist(nmtr_ctl->xx_4_monitor_list);
    dealloc_int2_clist(nmtr_ctl->node_4_monitor_list);
	
    free(nmtr_ctl);
    return;
};

void read_node_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct node_monitor_ctl_c *nmtr_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_clist(fp, buf, label_node_monitor_ctl[0], nmtr_ctl->group_4_monitor_list);
		read_real3_clist(fp, buf, label_node_monitor_ctl[1], nmtr_ctl->xx_4_monitor_list);
		read_int2_clist(fp, buf, label_node_monitor_ctl[2], nmtr_ctl->node_4_monitor_list);
	};
    nmtr_ctl->iflag_use = 1;
};

int write_node_monitor_ctl_c(FILE *fp, int level, const char *label, 
			struct node_monitor_ctl_c *nmtr_ctl){
    if(nmtr_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_clist(fp, level, label_node_monitor_ctl[0], nmtr_ctl->group_4_monitor_list);
	
	write_real3_clist(fp, level, label_node_monitor_ctl[1], nmtr_ctl->xx_4_monitor_list);
	write_int2_clist(fp, level, label_node_monitor_ctl[2], nmtr_ctl->node_4_monitor_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
