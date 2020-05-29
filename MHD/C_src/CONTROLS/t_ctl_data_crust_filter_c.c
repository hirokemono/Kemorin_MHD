/*
//  t_ctl_data_crust_filter_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#include "t_ctl_data_crust_filter_c.h"

const char label_crustal_filter_ctl[NLBL_CRUSTAL_FILTER_CTL][KCHARA_C] = {
    /*[ 0]*/    {"truncation_degree_ctl"}
};


void get_label_crustal_filter_ctl(int index, char *label){
    if(index < NLBL_CRUSTAL_FILTER_CTL) strngcopy(label, label_crustal_filter_ctl[index]);
    return;
};

struct crustal_filter_ctl_c * init_crustal_filter_ctl_c(){
    int i;
    struct crustal_filter_ctl_c *crust_f_ctl;
    if((crust_f_ctl = (struct crustal_filter_ctl_c *) malloc(sizeof(struct crustal_filter_ctl_c))) == NULL) {
        printf("malloc error for crustal_filter_ctl_c \n");
        exit(0);
    }
    
    crust_f_ctl->iflag_use = 0;
    crust_f_ctl->maxlen = 0;
    for (i=0;i<NLBL_CRUSTAL_FILTER_CTL;i++){
        if(strlen(label_crustal_filter_ctl[i]) > crust_f_ctl->maxlen){
            crust_f_ctl->maxlen = (int) strlen(label_crustal_filter_ctl[i]);
        };
    };
    
    crust_f_ctl->crustal_truncation_c = init_int_ctl_item_c();
    return crust_f_ctl;
};

void dealloc_crustal_filter_ctl_c(struct crustal_filter_ctl_c *crust_f_ctl){
    free(crust_f_ctl->crustal_truncation_c);
    free(crust_f_ctl);
    return;
};

void read_crustal_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct crustal_filter_ctl_c *crust_f_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
        skip_comment_read_line(fp, buf);
		
        read_integer_ctl_item_c(buf, label_crustal_filter_ctl[0], crust_f_ctl->crustal_truncation_c);
	};
    crust_f_ctl->iflag_use = 1;
    return;
}

int write_crustal_filter_ctl_c(FILE *fp, int level, const char *label, 
                                  struct crustal_filter_ctl_c *crust_f_ctl){
    if(crust_f_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
    
	write_integer_ctl_item_c(fp, level, crust_f_ctl->maxlen, 
				label_crustal_filter_ctl[0], crust_f_ctl->crustal_truncation_c);
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
}



