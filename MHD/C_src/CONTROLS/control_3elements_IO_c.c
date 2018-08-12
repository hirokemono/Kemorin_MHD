/*
//  control_3elements_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "control_3elements_IO_c.h"


void alloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item){
	c3_item->c1_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c3_item->c2_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c3_item->c3_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c3_item->iflag = 0;
    return;
};

void dealloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item){
    free(c3_item->c1_tbl);
    free(c3_item->c2_tbl);
    free(c3_item->c3_tbl);
	c3_item->iflag = 0;
    return;
};

int read_chara3_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
                          const char *label, struct chara3_ctl_item *c3_item){
	char header_chara[KCHARA_C];
	
	if(c3_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s %s", header_chara, 
					c3_item->c1_tbl, c3_item->c2_tbl, c3_item->c3_tbl);
		strip_cautation_marks(c3_item->c1_tbl);
		strip_cautation_marks(c3_item->c2_tbl);
		strip_cautation_marks(c3_item->c3_tbl);
		c3_item->iflag = 1;
	};
	return 1;
};

int write_chara3_ctl_item_c(FILE *fp, int level, int maxlen[3], 
                           const char *label, struct chara3_ctl_item *c3_item){
    
	if(c3_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c3_item->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c3_item->c2_tbl);
	write_one_label_w_lf_c(fp, c3_item->c3_tbl);
    return level;
};


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


void alloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
	c2r_item->c1_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->c2_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->r_data = 0.0;
	c2r_item->iflag = 0;
    return;
};

void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
    free(c2r_item->c1_tbl);
    free(c2r_item->c2_tbl);
	c2r_item->iflag = 0;
    return;
};

int read_c2r_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
                          const char *label, struct chara2_real_ctl_item *c2r_item){
	char header_chara[KCHARA_C];
	
	if(c2r_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s %lf", header_chara, 
					c2r_item->c1_tbl, c2r_item->c2_tbl, &c2r_item->r_data);
		strip_cautation_marks(c2r_item->c1_tbl);
		strip_cautation_marks(c2r_item->c2_tbl);
		c2r_item->iflag = 1;
	};
	return 1;
};

int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
                           const char *label, struct chara2_real_ctl_item *c2r_item){
    
	if(c2r_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c2r_item->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c2r_item->c2_tbl);
	fprintf(fp, "%.12e\n", c2r_item->r_data);
    return level;
};

