/*
//  t_control_data_LIC_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/06.
*/

#include "t_control_data_LIC_pvr_c.h"

FILE *FP_LIC;


const char label_LIC_pvr_head[KCHARA_C] = "LIC_rendering";

int num_ctl_label_LIC_pvr_f();
void set_ctl_label_LIC_pvr_f(char *label_1d);

struct label_list_f * init_ctl_label_LIC_pvr_f(){
	int len_fix = lengthchara_f();
	struct label_list_f *label_list = alloc_ctl_label();
	label_list->num_labels = num_ctl_label_LIC_pvr_f();
	
    char *packed_name = alloc_string((long) (len_fix*label_list->num_labels));
	set_ctl_label_LIC_pvr_f(packed_name);
	set_labels_from_packed(len_fix, packed_name, label_list);
	free(packed_name);
	return label_list;
};



struct LIC_pvr_ctl_c * init_LIC_pvr_ctl_c(){
    struct LIC_pvr_ctl_c *lic_pvr_c;
    if((lic_pvr_c = (struct LIC_pvr_ctl_c *) malloc(sizeof(struct LIC_pvr_ctl_c))) == NULL) {
        printf("malloc error for LIC_pvr_ctl_c \n");
        exit(0);
    }
	lic_pvr_c->pvr_c = init_pvr_ctl_c();
	
	lic_pvr_c->iflag_lic_ctl = 0;
	lic_pvr_c->lic_c = init_lic_ctl_c();
	
	lic_pvr_c->label_lic_pvr = init_ctl_label_LIC_pvr_f();
	if(lic_pvr_c->lic_c->label_lic_ctl->maxlen > lic_pvr_c->label_lic_pvr->maxlen){
		lic_pvr_c->label_lic_pvr->maxlen = lic_pvr_c->lic_c->label_lic_ctl->maxlen;
	};
	
	return lic_pvr_c;
};

void dealloc_LIC_pvr_ctl_c(struct LIC_pvr_ctl_c *lic_pvr_c){
	
	dealloc_pvr_ctl_c(lic_pvr_c->pvr_c);
	dealloc_lic_ctl_c(lic_pvr_c->lic_c);
	
	dealloc_ctl_label(lic_pvr_c->label_lic_pvr);
    dealloc_ctl_label(lic_pvr_c);
	return;
};


int read_LIC_pvr_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c){
	
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
        read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 1],
                                  lic_pvr_c->pvr_c->file_head_ctl);
        read_chara_ctl_item_c(buf, lic_pvr_c->label_lic_pvr->label[ 2],
                                  lic_pvr_c->pvr_c->file_fmt_ctl);
        
		read_pvr_ctl_items(fp, buf, lic_pvr_c->pvr_c);
		
		if(right_begin_flag_c(buf, lic_pvr_c->label_lic_pvr->label[ 7]) > 0){
			lic_pvr_c->iflag_lic_ctl = read_lic_ctl_c(fp, buf, 
						lic_pvr_c->label_lic_pvr->label[ 7], lic_pvr_c->lic_c);
		};
	};
	return 1;
};

int write_LIC_pvr_ctl_c(FILE *fp, int level, const char *label,
			struct LIC_pvr_ctl_c *lic_pvr_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);

    write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, 
                           lic_pvr_c->label_lic_pvr->label[ 1], 
                           lic_pvr_c->pvr_c->file_head_ctl);
    write_chara_ctl_item_c(fp, level, lic_pvr_c->pvr_c->maxlen, 
                           lic_pvr_c->label_lic_pvr->label[ 2], 
                           lic_pvr_c->pvr_c->file_fmt_ctl);
    
    if(lic_pvr_c->iflag_lic_ctl > 0){
        fprintf(fp, "!\n");
        write_lic_ctl_c(fp, level, lic_pvr_c->label_lic_pvr->label[ 7], lic_pvr_c->lic_c);
    };
    
	write_pvr_ctl_items(fp, level, lic_pvr_c->pvr_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

void rename_LIC_pvr_ctl_subfiles(struct LIC_pvr_ctl_c *lic_pvr_c){
    rename_pvr_ctl_subfiles(lic_pvr_c->pvr_c);
    return;
};

int read_LIC_pvr_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct LIC_pvr_ctl_c *lic_pvr_c){
    int iflag = 0;
    
    fprintf(stderr, "Read LIC_PVR control file: %s\n", file_name);
    if ((FP_LIC = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_read_line(FP_LIC, buf);
    if(right_begin_flag_c(buf, label_LIC_pvr_head) > 0){
        iflag = read_LIC_pvr_ctl_c(FP_LIC, buf, label_LIC_pvr_head, lic_pvr_c);
    };
    fclose(FP_LIC);
    
	read_pvr_ctl_subfiles(buf, lic_pvr_c->pvr_c);
    
    return iflag;
    
};
int write_LIC_pvr_ctl_file_c(const char *file_name, struct LIC_pvr_ctl_c *lic_pvr_c){
    int level;
	
    fprintf(stderr, "Write LIC_PVR control file: %s\n", file_name);
	write_pvr_ctl_subfiles(lic_pvr_c->pvr_c);
    
    if ((FP_LIC = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    level = write_LIC_pvr_ctl_c(FP_LIC, 0, label_LIC_pvr_head, lic_pvr_c);
    fclose(FP_LIC);
    
    return level;
    
};

