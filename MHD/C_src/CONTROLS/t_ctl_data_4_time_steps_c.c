/*
//  t_ctl_data_4_time_steps_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#include "t_ctl_data_4_time_steps_c.h"

#define NLBL_TIME_DATA_CTL   39

const char label_time_data_ctl[NLBL_TIME_DATA_CTL][KCHARA_C] = {
    /*[ 0]*/    {"flexible_step_ctl"},
    /*[ 1]*/    {"elapsed_time_ctl"},
    
    /*[ 2]*/    {"i_step_init_ctl"},
    /*[ 3]*/    {"i_step_number_ctl"},
    /*[ 4]*/    {"i_step_finish_ctl"},
    /*[ 5]*/    {"i_step_check_ctl"},
    
    /*[ 6]*/    {"i_step_rst_ctl"},
    /*[ 7]*/    {"i_step_sectioning_ctl"},
    /*[ 8]*/    {"i_step_isosurface_ctl"},
    /*[ 9]*/    {"i_step_psf_ctl"},
    /*[10]*/    {"i_step_iso_ctl"},
    /*[11]*/    {"i_step_pvr_ctl"},
    /*[12]*/    {"i_step_fline_ctl"},
    /*[13]*/    {"i_step_LIC_ctl"},
    
    /*[14]*/    {"i_step_field_ctl"},
    /*[15]*/    {"i_step_snapshot_ctl"},
    /*[16]*/    {"i_step_monitor_ctl"},
    /*[17]*/    {"i_step_sgs_coefs_ctl"},
    /*[18]*/    {"i_step_boundary_ctl"},
    /*[19]*/    {"i_diff_steps_ctl"},
    
    /*[20]*/    {"dt_ctl"},
    /*[21]*/    {"time_init_ctl"},
    
    /*[22]*/    {"min_delta_t_ctl"},
    /*[23]*/    {"max_delta_t_ctl"},
    /*[24]*/    {"min_eps_to_expand_ctl"},
    /*[25]*/    {"max_eps_to_shrink_ctl"},
    
    /*[26]*/    {"start_rst_step_ctl"},
    /*[27]*/    {"end_rst_step_ctl"},
    
    /*[28]*/    {"delta_t_check_ctl"},
    /*[29]*/    {"delta_t_rst_ctl"},
    /*[30]*/    {"delta_t_psf_ctl"},
    /*[31]*/    {"delta_t_iso_ctl"},
    /*[32]*/    {"delta_t_pvr_ctl"},
    /*[33]*/    {"delta_t_fline_ctl"},
    /*[34]*/    {"delta_t_LIC_ctl"},
    /*[35]*/    {"delta_t_field_ctl"},
    /*[36]*/    {"delta_t_monitor_ctl"},
    /*[37]*/    {"delta_t_sgs_coefs_ctl"},
    /*[38]*/    {"delta_t_boundary_ctl"}
};

void get_label_time_data_ctl(int index, char *label){
    if(index < NLBL_TIME_DATA_CTL) strngcopy(label, label_time_data_ctl[index]);
    return;
};

struct time_data_control_c * init_time_data_control_c(){
    int i;
    struct time_data_control_c *tctl;
    if((tctl = (struct time_data_control_c *) malloc(sizeof(struct time_data_control_c))) == NULL) {
        printf("malloc error for time_data_control_c \n");
        exit(0);
    }
    
    tctl->iflag_use = 0;
    tctl->maxlen = 0;
    for (i=0;i<NLBL_TIME_DATA_CTL;i++){
        if(strlen(label_time_data_ctl[i]) > tctl->maxlen){
            tctl->maxlen = (int) strlen(label_time_data_ctl[i]);
        };
    };
    
    tctl->flexible_step_c = init_chara_ctl_item_c();
    
    tctl->i_step_init_c =   init_int_ctl_item_c();
    tctl->i_step_number_c = init_int_ctl_item_c();
    tctl->elapsed_time_c =  init_real_ctl_item_c();
    
    tctl->i_step_check_c = init_int_ctl_item_c();
    tctl->i_step_rst_c =   init_int_ctl_item_c();
    tctl->i_step_pvr_c =   init_int_ctl_item_c();
    tctl->i_step_psf_c =   init_int_ctl_item_c();
    tctl->i_step_iso_c =   init_int_ctl_item_c();
    tctl->i_step_ucd_c =   init_int_ctl_item_c();
    tctl->i_step_fline_c = init_int_ctl_item_c();
    tctl->i_step_lic_c =   init_int_ctl_item_c();
    
    tctl->i_step_monitor_c =   init_int_ctl_item_c();
    tctl->i_step_sgs_coefs_c = init_int_ctl_item_c();
    tctl->i_step_boundary_c =  init_int_ctl_item_c();
    
    tctl->dt_c =        init_real_ctl_item_c();
    tctl->time_init_c = init_real_ctl_item_c();
    tctl->i_diff_steps_c = init_int_ctl_item_c();
    
    tctl->start_rst_step_c = init_int_ctl_item_c();
    tctl->end_rst_step_c =   init_int_ctl_item_c();
    tctl->min_delta_t_c =       init_real_ctl_item_c();
    tctl->max_delta_t_c =       init_real_ctl_item_c();
    tctl->min_eps_to_expand_c = init_real_ctl_item_c();
    tctl->max_eps_to_shrink_c = init_real_ctl_item_c();
    
    tctl->delta_t_check_c =     init_real_ctl_item_c();
    tctl->delta_t_rst_c =       init_real_ctl_item_c();
    tctl->delta_t_field_c =     init_real_ctl_item_c();
    tctl->delta_t_psf_c =       init_real_ctl_item_c();
    tctl->delta_t_iso_c =       init_real_ctl_item_c();
    tctl->delta_t_pvr_c =       init_real_ctl_item_c();
    tctl->delta_t_fline_c =     init_real_ctl_item_c();
    tctl->delta_t_lic_c =       init_real_ctl_item_c();
    tctl->delta_t_monitor_c =   init_real_ctl_item_c();
    tctl->delta_t_sgs_coefs_c = init_real_ctl_item_c();
    tctl->delta_t_boundary_c =  init_real_ctl_item_c();
    
    return tctl;
};

void dealloc_time_data_control_c(struct time_data_control_c *tctl){
    dealloc_chara_ctl_item_c(tctl->flexible_step_c);
    
    free(tctl->i_step_init_c);
    free(tctl->i_step_number_c);
    free(tctl->elapsed_time_c);
    
    free(tctl->i_step_check_c);
    free(tctl->i_step_rst_c);
    free(tctl->i_step_pvr_c);
    free(tctl->i_step_psf_c);
    free(tctl->i_step_iso_c);
    free(tctl->i_step_ucd_c);
    free(tctl->i_step_fline_c);
    free(tctl->i_step_lic_c);
    
    free(tctl->i_step_monitor_c);
    free(tctl->i_step_sgs_coefs_c);
    free(tctl->i_step_boundary_c);
    
    free(tctl->dt_c);
    free(tctl->time_init_c);
    free(tctl->i_diff_steps_c);
    
    free(tctl->start_rst_step_c);
    free(tctl->end_rst_step_c);
    free(tctl->min_delta_t_c);
    free(tctl->max_delta_t_c);
    free(tctl->min_eps_to_expand_c);
    free(tctl->max_eps_to_shrink_c);
    
    free(tctl->delta_t_check_c);
    free(tctl->delta_t_rst_c);
    free(tctl->delta_t_field_c);
    free(tctl->delta_t_psf_c);
    free(tctl->delta_t_iso_c);
    free(tctl->delta_t_pvr_c);
    free(tctl->delta_t_fline_c);
    free(tctl->delta_t_lic_c);
    free(tctl->delta_t_monitor_c);
    free(tctl->delta_t_sgs_coefs_c);
    free(tctl->delta_t_boundary_c);
    
    free(tctl);
    return;
};

void read_time_data_control_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct time_data_control_c *tctl){
    while(find_control_end_flag_c(buf, label) == 0){
        skip_comment_read_line(fp, buf);

        read_chara_ctl_item_c(buf, label_time_data_ctl[0], tctl->flexible_step_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[1], tctl->elapsed_time_c);
        
        read_integer_ctl_item_c(buf, label_time_data_ctl[2], tctl->i_step_init_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[4], tctl->i_step_number_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[3], tctl->i_step_number_c);
        
        read_integer_ctl_item_c(buf, label_time_data_ctl[5], tctl->i_step_check_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[6], tctl->i_step_rst_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[14], tctl->i_step_ucd_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[15], tctl->i_step_ucd_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[7], tctl->i_step_psf_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[8], tctl->i_step_iso_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[11], tctl->i_step_pvr_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[12], tctl->i_step_fline_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[13], tctl->i_step_lic_c);
        
        read_integer_ctl_item_c(buf, label_time_data_ctl[9], tctl->i_step_psf_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[10], tctl->i_step_iso_c);
        
        read_integer_ctl_item_c(buf, label_time_data_ctl[16], tctl->i_step_monitor_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[17], tctl->i_step_sgs_coefs_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[18], tctl->i_step_boundary_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[19], tctl->i_diff_steps_c);
        
        read_real_ctl_item_c(buf, label_time_data_ctl[20], tctl->dt_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[21], tctl->time_init_c);
        
        read_integer_ctl_item_c(buf, label_time_data_ctl[26], tctl->start_rst_step_c);
        read_integer_ctl_item_c(buf, label_time_data_ctl[27], tctl->end_rst_step_c);
        
        read_real_ctl_item_c(buf, label_time_data_ctl[22], tctl->min_delta_t_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[23], tctl->max_delta_t_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[24], tctl->min_eps_to_expand_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[25], tctl->max_eps_to_shrink_c);
        
        read_real_ctl_item_c(buf, label_time_data_ctl[28], tctl->delta_t_check_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[29], tctl->delta_t_rst_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[35], tctl->delta_t_field_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[30], tctl->delta_t_psf_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[31], tctl->delta_t_iso_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[32], tctl->delta_t_pvr_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[33], tctl->delta_t_fline_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[34], tctl->delta_t_lic_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[36], tctl->delta_t_monitor_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[37], tctl->delta_t_sgs_coefs_c);
        read_real_ctl_item_c(buf, label_time_data_ctl[38], tctl->delta_t_boundary_c);
    };
    tctl->iflag_use = 1;
    return;
}

int write_time_data_control_c(FILE *fp, int level, const char *label, 
			struct time_data_control_c *tctl){
    if(tctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
    
    write_chara_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[0], tctl->flexible_step_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[1], tctl->elapsed_time_c);
    
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[2], tctl->i_step_init_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[4], tctl->i_step_number_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[3], tctl->i_step_number_c);
    
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[5], tctl->i_step_check_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[6], tctl->i_step_rst_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[14], tctl->i_step_ucd_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[15], tctl->i_step_ucd_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[7], tctl->i_step_psf_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[8], tctl->i_step_iso_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[11], tctl->i_step_pvr_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[12], tctl->i_step_fline_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[13], tctl->i_step_lic_c);
    
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[9], tctl->i_step_psf_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[10], tctl->i_step_iso_c);
    
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[16], tctl->i_step_monitor_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[17], tctl->i_step_sgs_coefs_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[18], tctl->i_step_boundary_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[19], tctl->i_diff_steps_c);
    
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[20], tctl->dt_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[21], tctl->time_init_c);
    
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[26], tctl->start_rst_step_c);
    write_integer_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[27], tctl->end_rst_step_c);
    
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[22], tctl->min_delta_t_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[23], tctl->max_delta_t_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[24], tctl->min_eps_to_expand_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[25], tctl->max_eps_to_shrink_c);
    
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[28], tctl->delta_t_check_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[29], tctl->delta_t_rst_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[35], tctl->delta_t_field_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[30], tctl->delta_t_psf_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[31], tctl->delta_t_iso_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[32], tctl->delta_t_pvr_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[33], tctl->delta_t_fline_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[34], tctl->delta_t_lic_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[36], tctl->delta_t_monitor_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[37], tctl->delta_t_sgs_coefs_c);
    write_real_ctl_item_c(fp, level, tctl->maxlen, label_time_data_ctl[38], tctl->delta_t_boundary_c);
    
    level = write_end_flag_for_ctl_c(fp, level, label);
    return level;
}


