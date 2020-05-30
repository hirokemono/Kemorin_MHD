/*
//  t_ctl_data_zonal_means_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#include "t_ctl_data_zonal_means_c.h"

#define NLBL_SPH_ZONAL_MEAN_CTL  3

const char label_sph_zonal_means_ctl[NLBL_SPH_ZONAL_MEAN_CTL][KCHARA_C] = {
    /*[ 0]*/    {"zonal_mean_section_ctl"},
    /*[ 1]*/    {"zonal_RMS_section_ctl"},
	/*[ 2]*/    {"crustal_filtering_ctl"}
};

void get_label_sph_zonal_means_ctl(int index, char *label){
    if(index < NLBL_SPH_ZONAL_MEAN_CTL) strngcopy(label, label_sph_zonal_means_ctl[index]);
    return;
};


struct sph_zonal_means_ctl_c * init_sph_zonal_means_controls_c(){
	int i;
    struct sph_zonal_means_ctl_c *zm_ctls;
    if((zm_ctls = (struct sph_zonal_means_ctl_c *) malloc(sizeof(struct sph_zonal_means_ctl_c))) == NULL) {
        printf("malloc error for sph_zonal_means_ctl_c \n");
        exit(0);
    }
	
    zm_ctls->iflag_use = 0;
	zm_ctls->maxlen = 0;
	for (i=0;i<NLBL_SPH_ZONAL_MEAN_CTL;i++){
		if(strlen(label_sph_zonal_means_ctl[i]) > zm_ctls->maxlen){
			zm_ctls->maxlen = (int) strlen(label_sph_zonal_means_ctl[i]);
		};
	};
	
	zm_ctls->iflag_zmean_section_controls = 0;
	zm_ctls->iflag_zrms_section_controls = 0;
	
	zm_ctls->zmean_psf_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	zm_ctls->zmean_psf_c = init_psf_ctl_c();
	
	zm_ctls->zrms_psf_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	zm_ctls->zrms_psf_c = init_psf_ctl_c();
	
	zm_ctls->crust_filter_c = init_crustal_filter_ctl_c();
	return zm_ctls;
}

void dealloc_sph_zonal_means_controls_c(struct sph_zonal_means_ctl_c *zm_ctls){
    zm_ctls->iflag_zmean_section_controls = 0;
    zm_ctls->iflag_zrms_section_controls =  0;
	
	dealloc_psf_ctl_c(zm_ctls->zmean_psf_c);
	dealloc_psf_ctl_c(zm_ctls->zrms_psf_c);
	dealloc_crustal_filter_ctl_c(zm_ctls->crust_filter_c);
	
	free(zm_ctls->zmean_psf_file_name);
	free(zm_ctls->zrms_psf_file_name);
    
    free(zm_ctls);
	return;
}

void read_zonal_mean_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct sph_zonal_means_ctl_c *zm_ctls){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		if(right_begin_flag_c(buf, label_sph_zonal_means_ctl[0]) > 0){
			zm_ctls->iflag_zmean_section_controls = read_psf_ctl_c(fp, buf, label_sph_zonal_means_ctl[0], zm_ctls->zmean_psf_c);
		}else if(right_file_flag_c(buf, label_sph_zonal_means_ctl[0]) > 0){
			zm_ctls->iflag_zmean_section_controls = read_file_flag_c(buf, zm_ctls->zmean_psf_file_name);
		};
		
		if(right_begin_flag_c(buf, label_sph_zonal_means_ctl[1]) > 0){
			zm_ctls->iflag_zmean_section_controls = read_psf_ctl_c(fp, buf, label_sph_zonal_means_ctl[1],
						zm_ctls->zrms_psf_c);
		}else if(right_file_flag_c(buf, label_sph_zonal_means_ctl[1]) > 0){
			zm_ctls->iflag_zrms_section_controls = read_file_flag_c(buf, zm_ctls->zrms_psf_file_name);
		};
		
		if(right_begin_flag_c(buf, label_sph_zonal_means_ctl[1]) > 0){
			read_crustal_filter_ctl_c(fp, buf, label_sph_zonal_means_ctl[2], zm_ctls->crust_filter_c);
		};
	};
	zm_ctls->iflag_use = 1;
    return;
}

int write_zonal_mean_control_c(FILE *fp, int level, const char *label, 
			struct sph_zonal_means_ctl_c *zm_ctls){
    if(zm_ctls->iflag_use == 0) return level;

    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	if(zm_ctls->iflag_zmean_section_controls == 1){
		level = write_psf_ctl_c(fp, level, label_sph_zonal_means_ctl[0], zm_ctls->zmean_psf_c);
		fprintf(fp, "!\n");
	}else if(zm_ctls->iflag_zmean_section_controls == -1){
		write_file_flag_for_ctl_c(fp, level, label_sph_zonal_means_ctl[0], zm_ctls->zmean_psf_file_name);
	};
	if(zm_ctls->iflag_zrms_section_controls == 1){
		level = write_psf_ctl_c(fp, level, label_sph_zonal_means_ctl[1], zm_ctls->zrms_psf_c);
	}else if(zm_ctls->iflag_zrms_section_controls == -1){
		write_file_flag_for_ctl_c(fp, level, label_sph_zonal_means_ctl[1], zm_ctls->zrms_psf_file_name);
	};
	if(zm_ctls->crust_filter_c->iflag_use == 1){
		write_crustal_filter_ctl_c(fp, level, label_sph_zonal_means_ctl[2], zm_ctls->crust_filter_c);
	};
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
}

void read_zonal_mean_psf_ctl_file_c(char buf[LENGTHBUF], struct sph_zonal_means_ctl_c *zm_ctls){
    if(zm_ctls->iflag_use == 0) return;
        
	if(zm_ctls->iflag_zmean_section_controls == -1){
		read_psf_ctl_file_c(zm_ctls->zmean_psf_file_name, buf, zm_ctls->zmean_psf_c);
	};
	if(zm_ctls->iflag_zrms_section_controls == -1){
		read_psf_ctl_file_c(zm_ctls->zrms_psf_file_name, buf, zm_ctls->zrms_psf_c);
	};
 	return;
};

void write_zonal_mean_psf_ctl_file_c(struct sph_zonal_means_ctl_c *zm_ctls){
    if(zm_ctls->iflag_use == 0) return;
    
	if(zm_ctls->iflag_zmean_section_controls == -1){
		write_psf_ctl_file_c(zm_ctls->zmean_psf_file_name, zm_ctls->zmean_psf_c);
	};
	if(zm_ctls->iflag_zrms_section_controls == -1){
		write_psf_ctl_file_c(zm_ctls->zrms_psf_file_name, zm_ctls->zrms_psf_c);
	};
 	return;
};

