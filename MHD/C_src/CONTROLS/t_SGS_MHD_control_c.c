/*
//  t_SGS_MHD_control_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/15.
*/

#include "t_SGS_MHD_control_c.h"

#define NLBL_SGS_MHD_CTL        10

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

void alloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
	int i;
	
	mhd_ctl->maxlen = 0;
	for (i=0;i<NLBL_SGS_MHD_CTL;i++){
		if(strlen(label_SGS_MHD_ctl[i]) > mhd_ctl->maxlen){
			mhd_ctl->maxlen = (int) strlen(label_SGS_MHD_ctl[i]);
		};
	};
	
	mhd_ctl->iflag_data_files_def =      0;
	mhd_ctl->iflag_org_files_def =       0;
	mhd_ctl->iflag_new_files_def =       0;
	mhd_ctl->iflag_spherical_shell_ctl = 0;
	mhd_ctl->iflag_model =               0;
	mhd_ctl->iflag_control =             0;
    mhd_ctl->iflag_sph_monitor_ctl =     0;
	mhd_ctl->iflag_node_monitor_ctl =    0;
	mhd_ctl->iflag_visual_control =      0;
	mhd_ctl->iflag_zonal_mean_control =  0;
	
	mhd_ctl->files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c));
	alloc_platform_data_control_c(mhd_ctl->files);
	
	mhd_ctl->org_files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c));
	alloc_platform_data_control_c(mhd_ctl->org_files);
	
	mhd_ctl->new_files = (struct platform_data_control_c *) malloc(sizeof(struct platform_data_control_c));
	alloc_platform_data_control_c(mhd_ctl->new_files);
	
	mhd_ctl->shell_ctl_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	mhd_ctl->shell_ctl = (struct parallel_sph_shell_control_c *) malloc(sizeof(struct parallel_sph_shell_control_c));
	alloc_parallel_sph_shell_control_c(mhd_ctl->shell_ctl);
	
	mhd_ctl->model_ctl = (struct mhd_model_control_c *) malloc(sizeof(struct mhd_model_control_c));
	alloc_mhd_model_control_c(mhd_ctl->model_ctl);
	mhd_ctl->control_ctl = (struct sph_mhd_control_control_c *) malloc(sizeof(struct sph_mhd_control_control_c));
	alloc_sph_mhd_control_control_c(mhd_ctl->control_ctl);
	
	mhd_ctl->smtr_ctl = (struct sph_monitor_control_c *) malloc(sizeof(struct sph_monitor_control_c));
	alloc_sph_monitor_ctl_c(mhd_ctl->smtr_ctl);
	mhd_ctl->nmtr_ctl = (struct node_monitor_ctl_c *) malloc(sizeof(struct node_monitor_ctl_c));
	alloc_node_monitor_ctl_c(mhd_ctl->nmtr_ctl);
	
	mhd_ctl->viz_c = (struct visualizers_ctl_c *) malloc(sizeof(struct visualizers_ctl_c));
	alloc_vizs_ctl_c(mhd_ctl->viz_c);
	mhd_ctl->zm_ctls = (struct sph_zonal_means_ctl_c *) malloc(sizeof(struct sph_zonal_means_ctl_c));
	alloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	return;
}

void dealloc_SGS_MHD_control_c(struct SGS_MHD_control_c *mhd_ctl){
    mhd_ctl->iflag_zonal_mean_control =  0;
    mhd_ctl->iflag_visual_control =      0;
    mhd_ctl->iflag_node_monitor_ctl =    0;
    mhd_ctl->iflag_sph_monitor_ctl =     0;
    mhd_ctl->iflag_control =             0;
    mhd_ctl->iflag_model =               0;
    mhd_ctl->iflag_spherical_shell_ctl = 0;
    mhd_ctl->iflag_new_files_def =       0;
    mhd_ctl->iflag_org_files_def =       0;
    mhd_ctl->iflag_data_files_def =      0;
	
	dealloc_sph_zonal_means_controls_c(mhd_ctl->zm_ctls);
	free(mhd_ctl->zm_ctls);
	dealloc_vizs_ctl_c(mhd_ctl->viz_c);
	free(mhd_ctl->viz_c);
	dealloc_sph_monitor_ctl_c(mhd_ctl->smtr_ctl);
	free(mhd_ctl->smtr_ctl);
	dealloc_node_monitor_ctl_c(mhd_ctl->nmtr_ctl);
	free(mhd_ctl->nmtr_ctl);
	dealloc_sph_mhd_control_control_c(mhd_ctl->control_ctl);
	free(mhd_ctl->control_ctl);
	dealloc_mhd_model_control_c(mhd_ctl->model_ctl);
	free(mhd_ctl->model_ctl);
	dealloc_parallel_sph_shell_control_c(mhd_ctl->shell_ctl);
	free(mhd_ctl->shell_ctl);
	free(mhd_ctl->shell_ctl_file_name);
	alloc_platform_data_control_c(mhd_ctl->new_files);
	free(mhd_ctl->new_files);
	alloc_platform_data_control_c(mhd_ctl->org_files);
	free(mhd_ctl->org_files);
	alloc_platform_data_control_c(mhd_ctl->files);
	free(mhd_ctl->files);
	return;
}


int read_SGS_MHD_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct SGS_MHD_control_c *mhd_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[0]) > 0){
			mhd_ctl->iflag_data_files_def = read_platform_data_control_c(fp, buf,label_SGS_MHD_ctl[0], mhd_ctl->files);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[1]) > 0){ 
			mhd_ctl->iflag_org_files_def = read_platform_data_control_c(fp, buf, label_SGS_MHD_ctl[1], mhd_ctl->org_files);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[2]) > 0){
			mhd_ctl->iflag_new_files_def = read_platform_data_control_c(fp, buf, label_SGS_MHD_ctl[2], mhd_ctl->new_files);
		};
		
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[3]) > 0){
			mhd_ctl->iflag_spherical_shell_ctl = read_spherical_shell_ctl_c(fp, buf, label_SGS_MHD_ctl[3], mhd_ctl->shell_ctl);
		} else if(right_file_flag_c(buf, label_SGS_MHD_ctl[3]) > 0){
			mhd_ctl->iflag_spherical_shell_ctl = read_file_flag_c(buf, mhd_ctl->shell_ctl_file_name);
		};
		
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[4]) > 0){
			mhd_ctl->iflag_model = read_mhd_model_ctl_c(fp, buf, label_SGS_MHD_ctl[4], mhd_ctl->model_ctl);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[5]) > 0){
			mhd_ctl->iflag_control = read_mhd_control_ctl_c(fp, buf, label_SGS_MHD_ctl[5], mhd_ctl->control_ctl);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[6]) > 0){
			mhd_ctl->iflag_sph_monitor_ctl = read_sph_monitor_ctl_c(fp, buf, label_SGS_MHD_ctl[6], mhd_ctl->smtr_ctl);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[7]) > 0){
			mhd_ctl->iflag_zonal_mean_control = read_zonal_mean_control_c(fp, buf, label_SGS_MHD_ctl[7], mhd_ctl->zm_ctls);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[8]) > 0){
			mhd_ctl->iflag_visual_control = read_vizs_ctl_c(fp, buf, label_SGS_MHD_ctl[8], mhd_ctl->viz_c);
		};
		if(right_begin_flag_c(buf, label_SGS_MHD_ctl[9]) > 0){
			mhd_ctl->iflag_node_monitor_ctl = read_node_monitor_ctl_c(fp, buf, label_SGS_MHD_ctl[9], mhd_ctl->nmtr_ctl);
		};
	};
	return 1;
}

int write_SGS_MHD_control_c(FILE *fp, int level, const char *label, 
			struct SGS_MHD_control_c *mhd_ctl){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    if(mhd_ctl->iflag_data_files_def > 0){
        level = write_platform_data_control_c(fp, level, label_SGS_MHD_ctl[0], mhd_ctl->files);
    };
    if(mhd_ctl->iflag_org_files_def > 0){
        fprintf(fp, "!\n");
        level = write_platform_data_control_c(fp, level, label_SGS_MHD_ctl[1], mhd_ctl->org_files);
    };
    if(mhd_ctl->iflag_new_files_def > 0){
        fprintf(fp, "!\n");
        level = write_platform_data_control_c(fp, level, label_SGS_MHD_ctl[2], mhd_ctl->new_files);
	};
	
	if(mhd_ctl->iflag_spherical_shell_ctl == 1){
		fprintf(fp, "!\n");
		level = write_spherical_shell_ctl_c(fp, level, label_SGS_MHD_ctl[3], mhd_ctl->shell_ctl);
	}else if(mhd_ctl->iflag_spherical_shell_ctl == -1){
		fprintf(fp, "!\n");
		write_file_flag_for_ctl_c(fp, level, label_SGS_MHD_ctl[3], mhd_ctl->shell_ctl_file_name);
	};
	
	if(mhd_ctl->iflag_model > 0){
		fprintf(fp, "!\n");
		level = write_mhd_model_ctl_c(fp, level, label_SGS_MHD_ctl[4], mhd_ctl->model_ctl);
	};
	if(mhd_ctl->iflag_control > 0){
		fprintf(fp, "!\n");
		level = write_mhd_control_ctl_c(fp, level, label_SGS_MHD_ctl[5], mhd_ctl->control_ctl);
	};
	if(mhd_ctl->iflag_sph_monitor_ctl > 0){
		fprintf(fp, "!\n");
		level = write_sph_monitor_ctl_c(fp, level, label_SGS_MHD_ctl[6], mhd_ctl->smtr_ctl);
	};
	if(mhd_ctl->iflag_zonal_mean_control > 0){
		fprintf(fp, "!\n");
		level = write_zonal_mean_control_c(fp, level, label_SGS_MHD_ctl[7], mhd_ctl->zm_ctls);
	};
	if(mhd_ctl->iflag_visual_control > 0){
		fprintf(fp, "!\n");
		level = write_vizs_ctl_c(fp, level, label_SGS_MHD_ctl[8], mhd_ctl->viz_c);
	};
	if(mhd_ctl->iflag_node_monitor_ctl > 0){
		fprintf(fp, "!\n");
		level = write_node_monitor_ctl_c(fp, level, label_SGS_MHD_ctl[9], mhd_ctl->nmtr_ctl);
	};
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}

void rename_SGS_MHD_ctl_subfile_c(struct SGS_MHD_control_c *mhd_ctl){
	
    if(mhd_ctl->iflag_spherical_shell_ctl ==-1){
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
	
	if(mhd_ctl->iflag_spherical_shell_ctl ==-1){
		read_spherical_shell_file_c(mhd_ctl->shell_ctl_file_name, buf, mhd_ctl->shell_ctl);
	};
	
	read_vizs_ctl_files_c(buf, mhd_ctl->viz_c);
	
	if(mhd_ctl->iflag_zonal_mean_control > 0){
		read_zonal_mean_psf_ctl_file_c(buf, mhd_ctl->zm_ctls);
	};
 	return;
};

void write_SGS_MHD_ctl_subfile_c(struct SGS_MHD_control_c *mhd_ctl){
	
	if(mhd_ctl->iflag_spherical_shell_ctl ==-1){
		write_spherical_shell_file_c(mhd_ctl->shell_ctl_file_name, mhd_ctl->shell_ctl);
	};
	
	write_vizs_ctl_files_c(mhd_ctl->viz_c);
	
	if(mhd_ctl->iflag_zonal_mean_control > 0){
		write_zonal_mean_psf_ctl_file_c(mhd_ctl->zm_ctls);
	};
 	return;
};

void read_SGS_MHD_control_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct SGS_MHD_control_c *mhd_ctl){
    int iflag = 0;
    
    printf("Read spherical shell dynamo control: %s\n", file_name);
    if ((FP_MHD = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!\n");
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
    
	write_SGS_MHD_ctl_subfile_c(mhd_ctl);
    
    printf("Write spherical shell dynamo control: %s\n", file_name);
    if ((FP_MHD = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!\n");
        exit (2);                    /* terminate with error message */
    };
    
    level = write_SGS_MHD_control_c(FP_MHD, 0, label_MHD_control_head, mhd_ctl);
    fclose(FP_MHD);
    
    return;
};
