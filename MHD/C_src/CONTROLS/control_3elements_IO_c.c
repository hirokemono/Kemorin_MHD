/*
//  control_3elements_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "control_3elements_IO_c.h"


void init_real3_ctl_item_c(struct real3_ctl_item *r3_item){
	int i;
    for (i=0; i<3; i++) {r3_item->r_data[i] = 0.0;};
	r3_item->iflag = 0;
    return;
};

int read_real3_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
                          const char *label, struct real3_ctl_item *r3_item){
	char header_chara[KCHARA_C];
	
	if(r3_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %lf %lf %lf", header_chara, 
					&r3_item->r_data[0], &r3_item->r_data[1], &r3_item->r_data[2]);
		r3_item->iflag = 1;
	};
	return 1;
};

int write_real3_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct real3_ctl_item *r3_item){
    
	if(r3_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp, "%.12e  %.12e  %.12e\n", r3_item->r_data[0], 
				r3_item->r_data[1], r3_item->r_data[2]);
    return level;
};

