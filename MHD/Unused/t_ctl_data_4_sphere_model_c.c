/*
//  t_ctl_data_4_sphere_model_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_sphere_model_c.h"

const char label_sphere_domain_ctl[NLBL_SPHERE_DOMAIN_CTL][KCHARA_C] = {
	/*[ 0]*/	{"inner_decomp_direction"},
	
	/*[ 1]*/	{"num_radial_domain_ctl"},
	/*[ 2]*/	{"num_horizontal_domain_ctl"},
	
	/*[ 3]*/	{"num_domain_sph_grid"},
	/*[ 4]*/	{"num_domain_legendre"},
	/*[ 5]*/	{"num_domain_spectr"}
};

const char label_sphere_data_ctl[NLBL_SPHERE_DATA_CTL][KCHARA_C] = {
    /*[ 0]*/    {"sph_coef_type_ctl"},
    /*[ 1]*/    {"sph_grid_type_ctl"},
    
    /*[ 2]*/    {"truncation_level_ctl"},
    /*[ 3]*/    {"longitude_symmetry_ctl"},
    /*[ 4]*/    {"ngrid_meridonal_ctl"},
    /*[ 5]*/    {"ngrid_zonal_ctl"},

    /*[ 6]*/    {"radial_grid_type_ctl"},
    /*[ 7]*/	{"r_layer"},
	
	/*[ 8]*/	{"num_fluid_grid_ctl"},
    /*[ 9]*/    {"fluid_core_size_ctl"},
    /*[10]*/    {"ICB_to_CMB_ratio_ctl"},
	/*[11]*/	{"Min_radius_ctl"},
	/*[12]*/	{"ICB_radius_ctl"},
	/*[13]*/	{"CMB_radius_ctl"},
	/*[14]*/	{"Max_radius_ctl"},
	
	/*[15]*/	{"boundaries_ctl"},
	
	/*[16]*/	{"num_radial_layering_ctl"},
	/*[17]*/	{"num_meridional_layering_ctl"},
	/*[18]*/	{"radial_layering_ctl"},
	/*[19]*/	{"meridional_layering_ctl"}
};


void get_label_sphere_domain_ctl(int index, char *label){
    if(index < NLBL_SPHERE_DOMAIN_CTL) strngcopy(label, label_sphere_domain_ctl[index]);
    return;
};
void get_label_sphere_data_ctl(int index, char *label){
    if(index < NLBL_SPHERE_DATA_CTL) strngcopy(label, label_sphere_data_ctl[index]);
    return;
};

struct chara_int_clist * init_ndomain_list_c(){
    struct chara_int_clist *ndomain_list = init_chara_int_clist();
    sprintf(ndomain_list->c1_name, "Direction");
    sprintf(ndomain_list->i1_name, "Value");
	
	return ndomain_list;
};

struct sphere_domain_ctl_c * init_sphere_domain_ctl_c(){
	int i;
    struct sphere_domain_ctl_c *sdctl_c;
    if((sdctl_c = (struct sphere_domain_ctl_c *) malloc(sizeof(struct sphere_domain_ctl_c))) == NULL) {
        printf("malloc error for sphere_domain_ctl_c \n");
        exit(0);
    }

    sdctl_c->iflag_use = 0;
	sdctl_c->maxlen = 0;
	for (i=0;i<NLBL_SPHERE_DOMAIN_CTL;i++){
		if(strlen(label_sphere_domain_ctl[i]) > sdctl_c->maxlen){
			sdctl_c->maxlen = (int) strlen(label_sphere_domain_ctl[i]);
		};
	};
	
	sdctl_c->inner_decomp_c = init_chara_ctl_item_c();
	
    sdctl_c->num_radial_domain_c = init_int_ctl_item_c();
    sdctl_c->num_horiz_domain_c =  init_int_ctl_item_c();
	
    sdctl_c->ndomain_sph_grid_list = init_ndomain_list_c();
    sdctl_c->ndomain_legendre_list = init_ndomain_list_c();
    sdctl_c->ndomain_spectr_list = init_ndomain_list_c();
	
	return sdctl_c;
};

void dealloc_sphere_domain_ctl_c(struct sphere_domain_ctl_c *sdctl_c){
	
	dealloc_chara_ctl_item_c(sdctl_c->inner_decomp_c);
	
	free(sdctl_c->num_radial_domain_c);
	free(sdctl_c->num_horiz_domain_c);
	
	dealloc_chara_int_clist(sdctl_c->ndomain_sph_grid_list);
	dealloc_chara_int_clist(sdctl_c->ndomain_legendre_list);
	dealloc_chara_int_clist(sdctl_c->ndomain_spectr_list);
	
    free(sdctl_c);
	return;
};

void read_sphere_domain_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sphere_domain_ctl_c *sdctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_sphere_domain_ctl[ 0], sdctl_c->inner_decomp_c);
		
		read_integer_ctl_item_c(buf, label_sphere_domain_ctl[ 1], sdctl_c->num_radial_domain_c);
		read_integer_ctl_item_c(buf, label_sphere_domain_ctl[ 2], sdctl_c->num_horiz_domain_c);
		
		read_chara_int_clist(fp, buf, label_sphere_domain_ctl[ 3], sdctl_c->ndomain_sph_grid_list);
		read_chara_int_clist(fp, buf, label_sphere_domain_ctl[ 4], sdctl_c->ndomain_legendre_list);
		read_chara_int_clist(fp, buf, label_sphere_domain_ctl[ 5], sdctl_c->ndomain_spectr_list);
	};
    sdctl_c->iflag_use = 1;
	return;
};

int write_sphere_domain_ctl_c(FILE *fp, int level,
			const char *label, struct sphere_domain_ctl_c *sdctl_c){
    if(sdctl_c->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_chara_ctl_item_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 0], sdctl_c->inner_decomp_c);
	
	write_integer_ctl_item_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 1], sdctl_c->num_radial_domain_c);
	write_integer_ctl_item_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 2], sdctl_c->num_horiz_domain_c);
	
	level = write_chara_int_clist(fp, level, label_sphere_domain_ctl[ 3], sdctl_c->ndomain_sph_grid_list);
	level = write_chara_int_clist(fp, level, label_sphere_domain_ctl[ 4], sdctl_c->ndomain_legendre_list);
	level = write_chara_int_clist(fp, level, label_sphere_domain_ctl[ 5], sdctl_c->ndomain_spectr_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


struct sphere_data_ctl_c * init_sphere_data_ctl_c(){
	int i;
    struct sphere_data_ctl_c *spctl_c;
    if((spctl_c = (struct sphere_data_ctl_c *) malloc(sizeof(struct sphere_data_ctl_c))) == NULL) {
        printf("malloc error for sphere_data_ctl_c \n");
        exit(0);
    }
    	
    spctl_c->iflag_use = 0;
	spctl_c->maxlen = 0;
	for (i=0;i<NLBL_SPHERE_DATA_CTL;i++){
		if(strlen(label_sphere_data_ctl[i]) > spctl_c->maxlen){
			spctl_c->maxlen = (int) strlen(label_sphere_data_ctl[i]);
		};
	};
	
    spctl_c->ltr_c =          init_int_ctl_item_c();
    spctl_c->phi_symmetry_c = init_int_ctl_item_c();
	
	spctl_c->sph_grid_type_c = init_chara_ctl_item_c();
	spctl_c->sph_coef_type_c = init_chara_ctl_item_c();
	
    spctl_c->ngrid_elevation_c = init_int_ctl_item_c();
    spctl_c->ngrid_azimuth_c =   init_int_ctl_item_c();
	
    spctl_c->radius_list = init_int_real_clist();
    sprintf(spctl_c->radius_list->i1_name, "Radial_ID");
    sprintf(spctl_c->radius_list->r1_name, "Radious");
    
    spctl_c->radial_grp_list = init_chara_int_clist();
    sprintf(spctl_c->radial_grp_list->c1_name, "Name");
    sprintf(spctl_c->radial_grp_list->i1_name, "Index");
	
	spctl_c->radial_grid_type_c = init_chara_ctl_item_c();
	spctl_c->num_fluid_grid_c = init_int_ctl_item_c();
	
    spctl_c->Min_radius_c = init_real_ctl_item_c();
    spctl_c->ICB_radius_c = init_real_ctl_item_c();
    spctl_c->CMB_radius_c = init_real_ctl_item_c();
    spctl_c->Max_radius_c = init_real_ctl_item_c();
    spctl_c->fluid_core_size_c =  init_real_ctl_item_c();
    spctl_c->ICB_to_CMB_ratio_c = init_real_ctl_item_c();
	
    spctl_c->num_radial_layer_c = init_int_ctl_item_c();
    spctl_c->num_med_layer_c =    init_int_ctl_item_c();
	
    spctl_c->radial_layer_list = init_int2_clist();
    sprintf(spctl_c->radial_layer_list->i1_name, "Start_ID");
    sprintf(spctl_c->radial_layer_list->i2_name, "End_ID");

    spctl_c->med_layer_list =    init_int2_clist();
    sprintf(spctl_c->med_layer_list->i1_name, "Start_ID");
    sprintf(spctl_c->med_layer_list->i2_name, "End_ID");
	
	return spctl_c;
};

void dealloc_sphere_data_ctl_c(struct sphere_data_ctl_c *spctl_c){
	
	free(spctl_c->ltr_c);
	free(spctl_c->phi_symmetry_c);
	
	dealloc_chara_ctl_item_c(spctl_c->sph_grid_type_c);
	dealloc_chara_ctl_item_c(spctl_c->sph_coef_type_c);
	
	free(spctl_c->ngrid_elevation_c);
	free(spctl_c->ngrid_azimuth_c);
	
	dealloc_int_real_clist(spctl_c->radius_list);
	dealloc_chara_int_clist(spctl_c->radial_grp_list);
	
	dealloc_chara_ctl_item_c(spctl_c->radial_grid_type_c);
	free(spctl_c->num_fluid_grid_c);
	
	free(spctl_c->Min_radius_c);
	free(spctl_c->ICB_radius_c);
	free(spctl_c->CMB_radius_c);
	free(spctl_c->Max_radius_c);
	free(spctl_c->fluid_core_size_c);
	free(spctl_c->ICB_to_CMB_ratio_c);
	
	free(spctl_c->num_radial_layer_c);
	free(spctl_c->num_med_layer_c);
	
	dealloc_int2_clist(spctl_c->radial_layer_list);
	dealloc_int2_clist(spctl_c->med_layer_list);
    
    free(spctl_c);
	
	return;
};

void read_sphere_data_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sphere_data_ctl_c *spctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		skip_comment_read_line(fp, buf);
		
        read_chara_ctl_item_c(buf, label_sphere_data_ctl[ 0], spctl_c->sph_coef_type_c);
        read_chara_ctl_item_c(buf, label_sphere_data_ctl[ 1], spctl_c->sph_grid_type_c);
        
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 2], spctl_c->ltr_c);
        read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 3], spctl_c->phi_symmetry_c);
        read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 4], spctl_c->ngrid_elevation_c);
        read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 5], spctl_c->ngrid_azimuth_c);
        
		
        read_chara_ctl_item_c(buf, label_sphere_data_ctl[ 6], spctl_c->radial_grid_type_c);
		read_int_real_clist(fp, buf, label_sphere_data_ctl[ 7], spctl_c->radius_list);
		
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 8], spctl_c->num_fluid_grid_c);
        read_real_ctl_item_c(buf, label_sphere_data_ctl[ 9], spctl_c->fluid_core_size_c);
        read_real_ctl_item_c(buf, label_sphere_data_ctl[10], spctl_c->ICB_to_CMB_ratio_c);
		
		read_real_ctl_item_c(buf, label_sphere_data_ctl[11], spctl_c->Min_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[12], spctl_c->ICB_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[13], spctl_c->CMB_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[14], spctl_c->Max_radius_c);
		
		read_chara_int_clist(fp, buf, label_sphere_data_ctl[15], spctl_c->radial_grp_list);
		
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[16], spctl_c->num_radial_layer_c);
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[17], spctl_c->num_med_layer_c);
		
		read_int2_clist(fp, buf, label_sphere_data_ctl[18], spctl_c->radial_layer_list);
		read_int2_clist(fp, buf, label_sphere_data_ctl[19], spctl_c->med_layer_list);
	};
    spctl_c->iflag_use = 1;
	return;
};

int write_sphere_data_ctl_c(FILE *fp, int level,
			const char *label, struct sphere_data_ctl_c *spctl_c){
    if(spctl_c->iflag_use == 0) return level;

    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
    write_chara_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 0], spctl_c->sph_coef_type_c);
    write_chara_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 1], spctl_c->sph_grid_type_c);
    
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 2], spctl_c->ltr_c);
    write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 3], spctl_c->phi_symmetry_c);
    write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 4], spctl_c->ngrid_elevation_c);
    write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 5], spctl_c->ngrid_azimuth_c);
    
    write_chara_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 6], spctl_c->radial_grid_type_c);
	write_int_real_clist(fp, level, label_sphere_data_ctl[ 7], spctl_c->radius_list);
	
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 8], spctl_c->num_fluid_grid_c);
	
    write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 9], spctl_c->fluid_core_size_c);
    write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[10], spctl_c->ICB_to_CMB_ratio_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[11], spctl_c->Min_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[12], spctl_c->ICB_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[13], spctl_c->CMB_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[14], spctl_c->Max_radius_c);
	
	level = write_chara_int_clist(fp, level, label_sphere_data_ctl[15], spctl_c->radial_grp_list);
	
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[16], spctl_c->num_radial_layer_c);
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[17], spctl_c->num_med_layer_c);
	
	write_int2_clist(fp, level, label_sphere_data_ctl[18], spctl_c->radial_layer_list);
	write_int2_clist(fp, level, label_sphere_data_ctl[19], spctl_c->med_layer_list);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
