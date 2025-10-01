/*
//  t_ctl_data_4_platforms_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#include "t_ctl_data_4_platforms_c.h"

const char label_platform_ctl[NLBL_PLATFORM_CTL][KCHARA_C] = {
    /*[ 0]*/    {"debug_flag_ctl"},
    
    /*[ 1]*/    {"num_subdomain_ctl"},
    /*[ 2]*/    {"num_smp_ctl"},
    
    /*[ 3]*/    {"sph_file_prefix"},
    /*[ 4]*/    {"mesh_file_prefix"},
    /*[ 5]*/    {"field_file_prefix"},
    /*[ 6]*/    {"restart_file_prefix"},
    /*[ 7]*/    {"spectr_field_file_prefix"},
    
    /*[ 8]*/    {"coriolis_int_file_name"},
    /*[ 9]*/    {"boundary_data_file_name"},
    /*[10]*/    {"interpolate_sph_to_fem_ctl"},
    /*[11]*/    {"interpolate_fem_to_sph_ctl"},
    
    /*[12]*/    {"sph_file_fmt_ctl"},
    /*[13]*/    {"mesh_file_fmt_ctl"},
    /*[14]*/    {"restart_file_fmt_ctl"},
    /*[15]*/    {"field_file_fmt_ctl"},
    /*[16]*/    {"itp_file_fmt_ctl"},
    /*[17]*/    {"spectr_field_fmt_ctl"},
    /*[18]*/    {"coriolis_file_fmt_ctl"},
    
    /*[19]*/    {"delete_original_data_flag"}
};


void get_label_platform_ctl(int index, char *label){
    if(index < NLBL_PLATFORM_CTL) strngcopy(label, label_platform_ctl[index]);
    return;
};

struct platform_data_control_c * init_platform_data_control_c(){
    int i;
    struct platform_data_control_c *files;
    if((files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c))) == NULL) {
        printf("malloc error for platform_data_control_c \n");
        exit(0);
    }

    files->iflag_use = 0;
    files->maxlen = 0;
    for (i=0;i<NLBL_PLATFORM_CTL;i++){
        if(strlen(label_platform_ctl[i]) > files->maxlen){
            files->maxlen = (int) strlen(label_platform_ctl[i]);
        };
    };
    
    files->debug_flag_c = init_chara_ctl_item_c();
    
    files->ndomain_c = init_int_ctl_item_c();
    files->num_smp_c = init_int_ctl_item_c();

    files->sph_file_prefix_c = init_chara_ctl_item_c();
    files->mesh_file_prefix_c = init_chara_ctl_item_c();
    files->field_file_prefix_c = init_chara_ctl_item_c();
    files->restart_file_prefix_c = init_chara_ctl_item_c();
    files->spectr_field_file_prefix_c = init_chara_ctl_item_c();

    files->coriolis_int_file_name_c = init_chara_ctl_item_c();
    files->bc_data_file_name_c = init_chara_ctl_item_c();
    files->interpolate_sph_to_fem_c = init_chara_ctl_item_c();
    files->interpolate_fem_to_sph_c = init_chara_ctl_item_c();

    files->sph_file_fmt_c = init_chara_ctl_item_c();
    files->mesh_file_fmt_c = init_chara_ctl_item_c();
    files->restart_file_fmt_c = init_chara_ctl_item_c();
    files->field_file_fmt_c = init_chara_ctl_item_c();
    files->itp_file_fmt_c = init_chara_ctl_item_c();
    files->spectr_field_fmt_c = init_chara_ctl_item_c();
    files->coriolis_file_fmt_c = init_chara_ctl_item_c();

    files->del_org_data_ctl_c = init_chara_ctl_item_c();
    
    return files;
};

void dealloc_platform_data_control_c(struct platform_data_control_c *files){
    free(files->ndomain_c);
    free(files->num_smp_c);
    
    dealloc_chara_ctl_item_c(files->debug_flag_c);

    dealloc_chara_ctl_item_c(files->sph_file_prefix_c);
    dealloc_chara_ctl_item_c(files->mesh_file_prefix_c);
    dealloc_chara_ctl_item_c(files->field_file_prefix_c);
    dealloc_chara_ctl_item_c(files->restart_file_prefix_c);
    dealloc_chara_ctl_item_c(files->spectr_field_file_prefix_c);

    dealloc_chara_ctl_item_c(files->coriolis_int_file_name_c);
    dealloc_chara_ctl_item_c(files->bc_data_file_name_c);
    dealloc_chara_ctl_item_c(files->interpolate_sph_to_fem_c);
    dealloc_chara_ctl_item_c(files->interpolate_fem_to_sph_c);

    dealloc_chara_ctl_item_c(files->sph_file_fmt_c);
    dealloc_chara_ctl_item_c(files->mesh_file_fmt_c);
    dealloc_chara_ctl_item_c(files->restart_file_fmt_c);
    dealloc_chara_ctl_item_c(files->field_file_fmt_c);
    dealloc_chara_ctl_item_c(files->itp_file_fmt_c);
    dealloc_chara_ctl_item_c(files->spectr_field_fmt_c);
    dealloc_chara_ctl_item_c(files->coriolis_file_fmt_c);

    dealloc_chara_ctl_item_c(files->del_org_data_ctl_c);
    
    free(files);
    return;
};

void read_platform_data_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct platform_data_control_c *files){
	while(find_control_end_flag_c(buf, label) == 0){
        skip_comment_read_line(fp, buf);

        read_chara_ctl_item_c(buf, label_platform_ctl[0], files->debug_flag_c);
        
        read_integer_ctl_item_c(buf, label_platform_ctl[1], files->ndomain_c);
		read_integer_ctl_item_c(buf, label_platform_ctl[2], files->num_smp_c);
		
        read_chara_ctl_item_c(buf, label_platform_ctl[3], files->sph_file_prefix_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[4], files->mesh_file_prefix_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[5], files->field_file_prefix_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[6], files->restart_file_prefix_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[7], files->spectr_field_file_prefix_c);
		
        read_chara_ctl_item_c(buf, label_platform_ctl[8], files->coriolis_int_file_name_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[9], files->bc_data_file_name_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[10], files->interpolate_sph_to_fem_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[11], files->interpolate_fem_to_sph_c);
		
        read_chara_ctl_item_c(buf, label_platform_ctl[12], files->sph_file_fmt_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[13], files->mesh_file_fmt_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[14], files->restart_file_fmt_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[15], files->field_file_fmt_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[16], files->itp_file_fmt_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[17], files->spectr_field_fmt_c);
        read_chara_ctl_item_c(buf, label_platform_ctl[18], files->coriolis_file_fmt_c);
		
        read_chara_ctl_item_c(buf, label_platform_ctl[19], files->del_org_data_ctl_c);
	};
    files->iflag_use = 1;
    return;
}

int write_platform_data_control_c(FILE *fp, int level, const char *label, 
                                  struct platform_data_control_c *files){
    if(files->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
    
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[0], files->debug_flag_c);
    
    write_integer_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[1], files->ndomain_c);
    write_integer_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[2], files->num_smp_c);
	
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[3], files->sph_file_prefix_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[4], files->mesh_file_prefix_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[5], files->field_file_prefix_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[6], files->restart_file_prefix_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[7], files->spectr_field_file_prefix_c);
	
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[8], files->coriolis_int_file_name_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[9], files->bc_data_file_name_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[10], files->interpolate_sph_to_fem_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[11], files->interpolate_fem_to_sph_c);
	
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[12], files->mesh_file_fmt_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[15], files->sph_file_fmt_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[13], files->restart_file_fmt_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[14], files->field_file_fmt_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[16], files->itp_file_fmt_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[17], files->spectr_field_fmt_c);
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[18], files->coriolis_file_fmt_c);
	
    write_chara_ctl_item_c(fp, level, files->maxlen, label_platform_ctl[19], files->del_org_data_ctl_c);
	
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
}



