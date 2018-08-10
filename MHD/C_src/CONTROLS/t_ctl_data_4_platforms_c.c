/*
//  t_ctl_data_4_platforms_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#include "t_ctl_data_4_platforms_c.h"

#define NLBL_PLATFORM_CTL 20

const char label_platform_ctl[NLBL_PLATFORM_CTL][KCHARA_C] = {
    /*[ 0]*/    {"debug_flag_ctl"},
    
    /*[ 1]*/    {"num_subdomain_ctl"},
    /*[ 2]*/    {"num_smp_ctl"},
    
    /*[ 3]*/    {"mesh_file_prefix"},
    /*[ 4]*/    {"field_file_prefix"},
    /*[ 5]*/    {"restart_file_prefix"},
    /*[ 6]*/    {"spectr_field_file_prefix"},
    /*[ 7]*/    {"sph_file_prefix"},
    
    /*[ 8]*/    {"coriolis_int_file_name"},
    /*[ 9]*/    {"boundary_data_file_name"},
    /*[10]*/    {"interpolate_sph_to_fem_ctl"},
    /*[11]*/    {"interpolate_fem_to_sph_ctl"},
    
    /*[12]*/    {"mesh_file_fmt_ctl"},
    /*[13]*/    {"restart_file_fmt_ctl"},
    /*[14]*/    {"field_file_fmt_ctl"},
    /*[15]*/    {"sph_file_fmt_ctl"},
    /*[16]*/    {"itp_file_fmt_ctl"},
    /*[17]*/    {"spectr_field_fmt_ctl"},
    /*[18]*/    {"coriolis_file_fmt_ctl"},
    
    /*[19]*/    {"delete_original_data_flag"}
};


void get_label_platform_ctl(int index, char *label){
    if(index < NLBL_PLATFORM_CTL) strngcopy(label, label_platform_ctl[index]);
    return;
};

void alloc_platform_data_control_c(struct platform_data_control_c *files){
    int i;
    
    files->maxlen = 0;
    for (i=0;i<NLBL_PLATFORM_CTL;i++){
        if(strlen(label_platform_ctl[i]) > files->maxlen){
            files->maxlen = (int) strlen(label_platform_ctl[i]);
        };
    };
    
    files->debug_flag_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->debug_flag_c);
    
    files->ndomain_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
    init_ctl_int_item(files->ndomain_c);
    files->num_smp_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
    init_ctl_int_item(files->num_smp_c);

    files->mesh_file_prefix_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->mesh_file_prefix_c);
    files->field_file_prefix_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->field_file_prefix_c);
    files->restart_file_prefix_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->restart_file_prefix_c);
    files->spectr_field_file_prefix_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->spectr_field_file_prefix_c);
    files->sph_file_prefix_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->sph_file_prefix_c);

    files->coriolis_int_file_name_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->coriolis_int_file_name_c);
    files->bc_data_file_name_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->bc_data_file_name_c);
    files->interpolate_sph_to_fem_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->interpolate_sph_to_fem_c);
    files->interpolate_fem_to_sph_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->interpolate_fem_to_sph_c);

    files->mesh_file_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->mesh_file_fmt_c);
    files->sph_file_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->sph_file_fmt_c);
    files->restart_file_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->restart_file_fmt_c);
    files->field_file_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->field_file_fmt_c);
    files->itp_file_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->itp_file_fmt_c);
    files->spectr_field_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->spectr_field_fmt_c);
    files->coriolis_file_fmt_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->coriolis_file_fmt_c);

    files->del_org_data_ctl_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
    alloc_ctl_chara_item(files->del_org_data_ctl_c);
    
    return;
};

void dealloc_platform_data_control_c(struct platform_data_control_c *files){
    free(files->ndomain_c);
    free(files->num_smp_c);
    
    dealloc_ctl_chara_item(files->debug_flag_c);

    dealloc_ctl_chara_item(files->mesh_file_prefix_c);
    dealloc_ctl_chara_item(files->field_file_prefix_c);
    dealloc_ctl_chara_item(files->restart_file_prefix_c);
    dealloc_ctl_chara_item(files->spectr_field_file_prefix_c);
    dealloc_ctl_chara_item(files->sph_file_prefix_c);

    dealloc_ctl_chara_item(files->coriolis_int_file_name_c);
    dealloc_ctl_chara_item(files->bc_data_file_name_c);
    dealloc_ctl_chara_item(files->interpolate_sph_to_fem_c);
    dealloc_ctl_chara_item(files->interpolate_fem_to_sph_c);

    dealloc_ctl_chara_item(files->mesh_file_fmt_c);
    dealloc_ctl_chara_item(files->sph_file_fmt_c);
    dealloc_ctl_chara_item(files->restart_file_fmt_c);
    dealloc_ctl_chara_item(files->field_file_fmt_c);
    dealloc_ctl_chara_item(files->itp_file_fmt_c);
    dealloc_ctl_chara_item(files->spectr_field_fmt_c);
    dealloc_ctl_chara_item(files->coriolis_file_fmt_c);

    dealloc_ctl_chara_item(files->del_org_data_ctl_c);
    return;
};

int read_platform_data_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct platform_data_control_c *files){
	while(find_control_end_flag_c(buf, label) == 0){
		
        skip_comment_read_line(fp, buf);

        read_character_ctl_item_c(buf, label_platform_ctl[0], files->debug_flag_c);
        
        read_integer_ctl_item_c(buf, label_platform_ctl[1], files->ndomain_c);
		read_integer_ctl_item_c(buf, label_platform_ctl[2], files->num_smp_c);
		
        read_character_ctl_item_c(buf, label_platform_ctl[3], files->mesh_file_prefix_c);
        read_character_ctl_item_c(buf, label_platform_ctl[4], files->field_file_prefix_c);
        read_character_ctl_item_c(buf, label_platform_ctl[5], files->restart_file_prefix_c);
        read_character_ctl_item_c(buf, label_platform_ctl[6], files->spectr_field_file_prefix_c);
        read_character_ctl_item_c(buf, label_platform_ctl[7], files->sph_file_prefix_c);
		
        read_character_ctl_item_c(buf, label_platform_ctl[8], files->coriolis_int_file_name_c);
        read_character_ctl_item_c(buf, label_platform_ctl[9], files->bc_data_file_name_c);
        read_character_ctl_item_c(buf, label_platform_ctl[10], files->interpolate_sph_to_fem_c);
        read_character_ctl_item_c(buf, label_platform_ctl[11], files->interpolate_fem_to_sph_c);
		
        read_character_ctl_item_c(buf, label_platform_ctl[12], files->mesh_file_fmt_c);
        read_character_ctl_item_c(buf, label_platform_ctl[15], files->sph_file_fmt_c);
        read_character_ctl_item_c(buf, label_platform_ctl[13], files->restart_file_fmt_c);
        read_character_ctl_item_c(buf, label_platform_ctl[14], files->field_file_fmt_c);
        read_character_ctl_item_c(buf, label_platform_ctl[16], files->itp_file_fmt_c);
        read_character_ctl_item_c(buf, label_platform_ctl[17], files->spectr_field_fmt_c);
        read_character_ctl_item_c(buf, label_platform_ctl[18], files->coriolis_file_fmt_c);
		
        read_character_ctl_item_c(buf, label_platform_ctl[19], files->del_org_data_ctl_c);
	};
    return 1;
}

int write_platform_data_control_c(FILE *fp, int level, const char *label, 
                                  struct platform_data_control_c *files){
    level = write_begin_flag_for_ctl_c(fp, level, label);
    
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[0], files->debug_flag_c);
    
    write_integer_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[1], files->ndomain_c);
    write_integer_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[2], files->num_smp_c);
	
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[3], files->mesh_file_prefix_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[4], files->field_file_prefix_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[5], files->restart_file_prefix_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[6], files->spectr_field_file_prefix_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[7], files->sph_file_prefix_c);
	
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[8], files->coriolis_int_file_name_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[9], files->bc_data_file_name_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[10], files->interpolate_sph_to_fem_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[11], files->interpolate_fem_to_sph_c);
	
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[12], files->mesh_file_fmt_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[15], files->sph_file_fmt_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[13], files->restart_file_fmt_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[14], files->field_file_fmt_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[16], files->itp_file_fmt_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[17], files->spectr_field_fmt_c);
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[18], files->coriolis_file_fmt_c);
	
    write_character_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[19], files->del_org_data_ctl_c);
	
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
}



