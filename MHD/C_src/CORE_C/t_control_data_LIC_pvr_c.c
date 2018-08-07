/*
//  t_control_data_LIC_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/06.
*/

#include "t_control_data_LIC_pvr_c.h"

#define NLBL_LIC_PVR_CTL            3

FILE *FP_LIC;


const char label_LIC_pvr_ctl[NLBL_LIC_PVR_CTL][KCHARA_C] = {
	/*[ 0]*/	{"LIC_ctl"},
    
    /*[ 1]*/    {"lic_file_prefix"},
    /*[ 2]*/    {"lic_image_format"}
};
const char label_LIC_pvr_head[KCHARA_C] = "LIC_rendering";


void alloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	int i;
	
	lic_pvr_c->pvr_c = (struct pvr_ctl_c *) malloc(sizeof(struct pvr_ctl_c));
	alloc_pvr_ctl_c(lic_pvr_c->pvr_c);
	
	lic_pvr_c->iflag_lic_ctl = 0;
	lic_pvr_c->lic_c = (struct lic_ctl_c *) malloc(sizeof(struct lic_ctl_c));
	alloc_lic_ctl_c(lic_pvr_c->lic_c);
	
	lic_pvr_c->maxlen = lic_pvr_c->lic_c->maxlen;
	for (i=0;i<NLBL_LIC_PVR_CTL;i++){
		if(strlen(label_LIC_pvr_ctl[i]) > lic_pvr_c->maxlen){
			lic_pvr_c->maxlen = (int) strlen(label_LIC_pvr_ctl[i]);
		};
	};
	lic_pvr_c->lic_c->maxlen = lic_pvr_c->maxlen;
	
	return;
};

void dealloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	
	dealloc_pvr_ctl_c(lic_pvr_c->pvr_c);
	free(lic_pvr_c->pvr_c);
	
	dealloc_lic_ctl_c(lic_pvr_c->lic_c);
	free(lic_pvr_c->lic_c);
	lic_pvr_c->iflag_lic_ctl = 0;
	return;
};


int read_LIC_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		fgets(buf, LENGTHBUF, fp);

        read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 1],
                                  lic_pvr_c->pvr_c->file_head_ctl);
        read_character_ctl_item_c(buf, label_LIC_pvr_ctl[ 2],
                                  lic_pvr_c->pvr_c->file_fmt_ctl);
        
		read_pvr_ctl_items(fp, buf, lic_pvr_c->pvr_c);
		
		if(right_begin_flag_c(buf, label_LIC_pvr_ctl[ 0]) > 0){
			lic_pvr_c->iflag_lic_ctl = read_lic_ctl_c(fp, buf, 
						label_LIC_pvr_ctl[ 0], lic_pvr_c->lic_c);
		};
	};
	return 1;
};

int write_LIC_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);

    write_character_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, label_LIC_pvr_ctl[ 1], 
                               lic_pvr_c->pvr_c->file_head_ctl);
    write_character_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, label_LIC_pvr_ctl[ 2], 
                               lic_pvr_c->pvr_c->file_fmt_ctl);
    
    if(lic_pvr_c->iflag_lic_ctl > 0){
        fprintf(fp, "!\n");
        write_lic_ctl_c(fp, level, label_LIC_pvr_ctl[ 0], lic_pvr_c->lic_c);
    };
    
    fprintf(fp, "!\n");
	write_pvr_ctl_items(fp, level, lic_pvr_c->pvr_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


int read_LIC_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct LIC_pvr_ctl_c *lic_pvr_c){
    int iflag = 0;
    
    if ((FP_LIC = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!\n");
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_c(FP_LIC);
    fgets(buf, LENGTHBUF, FP_LIC);
    if(right_begin_flag_c(buf, label_LIC_pvr_head) > 0){
        iflag = read_LIC_pvr_ctl_c(FP_LIC, buf, label_LIC_pvr_head, lic_pvr_c);
    };
    fclose(FP_LIC);
    
	read_pvr_ctl_subfiles(buf, lic_pvr_c->pvr_c);
    
    return iflag;
    
};
int write_LIC_pvr_ctl_file_c(const char *file_name, struct LIC_pvr_ctl_c *lic_pvr_c){
    int level;
	
	write_pvr_ctl_subfiles(lic_pvr_c->pvr_c);
    
    if ((FP_LIC = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!\n");
        exit (2);                    /* terminate with error message */
    };
    
    level = write_LIC_pvr_ctl_c(FP_LIC, 0, label_LIC_pvr_head, lic_pvr_c);
    fclose(FP_LIC);
    
    return level;
    
};

