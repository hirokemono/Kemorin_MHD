/*
//  t_SGS_MHD_control_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#include "t_SGS_MHD_control_c.h"

FILE *FP_MHD;

const char label_SGS_MHD_ctl[NLBL_SGS_MHD_CTL][KCHARA_C] = {
    /*[ 0]*/    {"data_files_def"},
    /*[ 1]*/    {"org_data_files_def"},
    /*[ 2]*/    {"new_data_files_def"},
    /*[ 3]*/    {"spherical_shell_ctl"},
	
	/*[ 4]*/    {"model"},
    /*[ 5]*/    {"control"},
	
    /*[ 6]*/    {"sph_monitor_ctl"},
    /*[ 7]*/    {"zonal_mean_control"},
    /*[ 8]*/    {"visual_control"},
    /*[ 9]*/    {"monitor_data_ctl"}
};

const char label_MHD_control_head[KCHARA_C] = "MHD_control";


void get_label_SGS_MHD_ctl(int index, char *label){
    if(index < NLBL_SGS_MHD_CTL) strngcopy(label, label_SGS_MHD_ctl[index]);
    return;
};
void get_label_MHD_control_head(char *label){
    strngcopy(label, label_MHD_control_head);
    return;
};

struct SGS_MHD_control_c *alloc_SGS_MHD_control_c(void){
	int i;
    struct SGS_MHD_control_c *mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	
	mhd_ctl->maxlen = 0;
	for (i=0;i<NLBL_SGS_MHD_CTL;i++){
		if(strlen(label_SGS_MHD_ctl[i]) > mhd_ctl->maxlen){
			mhd_ctl->maxlen = (int) strlen(label_SGS_MHD_ctl[i]);
		};
	};
	
	mhd_ctl->files =     init_platform_data_control_c();
	mhd_ctl->org_files = init_platform_data_control_c();
	mhd_ctl->new_files = init_platform_data_control_c();
	
	mhd_ctl->shell_ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	mhd_ctl->shell_ctl = init_parallel_sph_shell_control_c();
	
	mhd_ctl->model_ctl = init_mhd_model_control_c();
	mhd_ctl->control_ctl = init_sph_mhd_control_control_c();
	
	mhd_ctl->smtr_ctl = init_sph_monitor_ctl_c();
	mhd_ctl->nmtr_ctl = init_node_monitor_ctl_c();
	
	mhd_ctl->viz_c =   init_vizs_ctl_c();
	mhd_ctl->zm_ctls = init_sph_zonal_means_controls_c();
	return mhd_ctl;
}

void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
	dealloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	dealloc_vizs_ctl_c(mhd_ctl->viz_c);
	dealloc_sph_monitor_ctl_c(mhd_ctl->smtr_ctl);
	dealloc_node_monitor_ctl_c(mhd_ctl->nmtr_ctl);
	dealloc_sph_mhd_control_control_c(mhd_ctl->control_ctl);
	dealloc_mhd_model_control_c(mhd_ctl->model_ctl);
	dealloc_parallel_sph_shell_control_c(mhd_ctl->shell_ctl);
	free(mhd_ctl->shell_ctl_file_name);
	dealloc_platform_data_control_c(mhd_ctl->new_files);
	dealloc_platform_data_control_c(mhd_ctl->org_files);
	dealloc_platform_data_control_c(mhd_ctl->files);
	return;
}


int read_SGS_MHD_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct SGS_MHD_control_c *mhd_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[0]) > 0){
			read_platform_data_control_c(fp, buf,label_SGS_MHD_ctl[0], mhd_ctl->files);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[1]) > 0){ 
			read_platform_data_control_c(fp, buf, label_SGS_MHD_ctl[1], mhd_ctl->org_files);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[2]) > 0){
			read_platform_data_control_c(fp, buf, label_SGS_MHD_ctl[2], mhd_ctl->new_files);
		};
		
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[3]) > 0){
			read_spherical_shell_ctl_c(fp, buf, label_SGS_MHD_ctl[3], mhd_ctl->shell_ctl);
		} else if(right_file_flag_c(buf, label_SGS_MHD_ctl[3]) > 0){
			mhd_ctl->shell_ctl->iflag_use_file = read_file_flag_c(buf, mhd_ctl->shell_ctl_file_name);
		};
		
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[4]) > 0){
			read_mhd_model_ctl_c(fp, buf, label_SGS_MHD_ctl[4], mhd_ctl->model_ctl);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[5]) > 0){
			read_mhd_control_ctl_c(fp, buf, label_SGS_MHD_ctl[5], mhd_ctl->control_ctl);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[6]) > 0){
			read_sph_monitor_ctl_c(fp, buf, label_SGS_MHD_ctl[6], mhd_ctl->smtr_ctl);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[7]) > 0){
			read_zonal_mean_control_c(fp, buf, label_SGS_MHD_ctl[7], mhd_ctl->zm_ctls);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[8]) > 0){
			read_vizs_ctl_c(fp, buf, label_SGS_MHD_ctl[8], mhd_ctl->viz_c);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[9]) > 0){
			read_node_monitor_ctl_c(fp, buf, label_SGS_MHD_ctl[9], mhd_ctl->nmtr_ctl);
		};
	};
	return 1;
}

int write_SGS_MHD_control_c(FILE *fp, int level, const char *label, 
			struct SGS_MHD_control_c *mhd_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
    level = write_platform_data_control_c(fp, level, label_SGS_MHD_ctl[0], mhd_ctl->files);
    level = write_platform_data_control_c(fp, level, label_SGS_MHD_ctl[1], mhd_ctl->org_files);
    level = write_platform_data_control_c(fp, level, label_SGS_MHD_ctl[2], mhd_ctl->new_files);
	
	if(mhd_ctl->shell_ctl->iflag_use_file == -1){
        fprintf(fp, "!\n");
        write_file_flag_for_ctl_c(fp, level, label_SGS_MHD_ctl[3], mhd_ctl->shell_ctl_file_name);
    } else {
		level = write_spherical_shell_ctl_c(fp, level, label_SGS_MHD_ctl[3], mhd_ctl->shell_ctl);
	};
	
    level = write_mhd_model_ctl_c(fp, level, label_SGS_MHD_ctl[4], mhd_ctl->model_ctl);
    level = write_mhd_control_ctl_c(fp, level, label_SGS_MHD_ctl[5], mhd_ctl->control_ctl);
    level = write_sph_monitor_ctl_c(fp, level, label_SGS_MHD_ctl[6], mhd_ctl->smtr_ctl);
    level = write_zonal_mean_control_c(fp, level, label_SGS_MHD_ctl[7], mhd_ctl->zm_ctls);
    level = write_vizs_ctl_c(fp, level, label_SGS_MHD_ctl[8], mhd_ctl->viz_c);
    level = write_node_monitor_ctl_c(fp, level, label_SGS_MHD_ctl[9], mhd_ctl->nmtr_ctl);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}

void rename_SGS_MHD_ctl_subfile_c(struct SGS_MHD_control_c *mhd_ctl){
	
    if(mhd_ctl->shell_ctl->iflag_use_file ==-1){
        strcat(mhd_ctl->shell_ctl_file_name, "_2");
    }
	
	rename_vizs_ctl_subfiles(mhd_ctl->viz_c);
	
	if(mhd_ctl->zm_ctls->iflag_zrms_section_controls == -1){
		strcat(mhd_ctl->zm_ctls->zmean_psf_file_name, "_2");
	};
	rename_psf_define_file_c(mhd_ctl->zm_ctls->zmean_psf_c);
	
	if(mhd_ctl->zm_ctls->iflag_zmean_section_controls == -1){
		strcat(mhd_ctl->zm_ctls->zrms_psf_file_name, "_2");
	};
	rename_psf_define_file_c(mhd_ctl->zm_ctls->zrms_psf_c);
	
	
	return;
};

void read_SGS_MHD_ctl_subfile_c(char buf[LENGTHBUF], struct SGS_MHD_control_c *mhd_ctl){
	
    read_spherical_shell_file_c(mhd_ctl->shell_ctl_file_name, buf, mhd_ctl->shell_ctl);
	read_vizs_ctl_files_c(buf, mhd_ctl->viz_c);
    read_zonal_mean_psf_ctl_file_c(buf, mhd_ctl->zm_ctls);
 	return;
};

void write_SGS_MHD_ctl_subfile_c(struct SGS_MHD_control_c *mhd_ctl){
	
    write_spherical_shell_file_c(mhd_ctl->shell_ctl_file_name, mhd_ctl->shell_ctl);
	write_vizs_ctl_files_c(mhd_ctl->viz_c);
    write_zonal_mean_psf_ctl_file_c(mhd_ctl->zm_ctls);
 	return;
};

void read_SGS_MHD_control_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct SGS_MHD_control_c *mhd_ctl){
    int iflag = 0;
    
    printf("Read spherical shell dynamo control: %s\n", file_name);
    if ((FP_MHD = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    skip_comment_read_line(FP_MHD, buf);
    if(right_begin_flag_c(buf, label_MHD_control_head) > 0){
        iflag = read_SGS_MHD_control_c(FP_MHD, buf, label_MHD_control_head, mhd_ctl);
    };
    fclose(FP_MHD);
	
	read_SGS_MHD_ctl_subfile_c(buf, mhd_ctl);
	
    return;
};

void write_SGS_MHD_control_file_c(const char *file_name, struct SGS_MHD_control_c *mhd_ctl){
    int level = 0;
        
    printf("Write spherical shell dynamo control: %s\n", file_name);
    if ((FP_MHD = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        exit (2);                    /* terminate with error message */
    };
    
    level = write_SGS_MHD_control_c(FP_MHD, 0, label_MHD_control_head, mhd_ctl);
    fclose(FP_MHD);
    
    return;
};
