/*
//  t_ctl_data_4_sphere_model_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#include "t_ctl_data_4_sphere_model_c.h"

#define NLBL_SPHERE_DOMAIN_CTL  6
#define NLBL_SPHERE_DATA_CTL   20

const char label_sphere_domain_ctl[NLBL_SPHERE_DOMAIN_CTL][KCHARA_C] = {
	/*[ 0]*/	{"inner_decomp_direction"},
	
	/*[ 1]*/	{"num_radial_domain_ctl"},
	/*[ 2]*/	{"num_horizontal_domain_ctl"},
	
	/*[ 3]*/	{"num_domain_sph_grid"},
	/*[ 4]*/	{"num_domain_legendre"},
	/*[ 5]*/	{"num_domain_spectr"}
};

const char label_sphere_data_ctl[NLBL_SPHERE_DATA_CTL][KCHARA_C] = {
	/*[ 0]*/	{"r_layer"},
	
	/*[ 1]*/	{"ngrid_meridonal_ctl"},
	/*[ 2]*/	{"ngrid_zonal_ctl"},
	/*[ 3]*/	{"truncation_level_ctl"},
	/*[ 4]*/	{"longitude_symmetry_ctl"},
	/*[ 5]*/	{"sph_coef_type_ctl"},
	/*[ 6]*/	{"sph_grid_type_ctl"},
	
	/*[ 7]*/	{"radial_grid_type_ctl"},
	/*[ 8]*/	{"num_fluid_grid_ctl"},
	/*[ 9]*/	{"Min_radius_ctl"},
	/*[10]*/	{"ICB_radius_ctl"},
	/*[11]*/	{"CMB_radius_ctl"},
	/*[12]*/	{"Max_radius_ctl"},
	/*[13]*/	{"fluid_core_size_ctl"},
	/*[14]*/	{"ICB_to_CMB_ratio_ctl"},
	
	/*[15]*/	{"boundaries_ctl"},
	
	/*[16]*/	{"num_radial_layering_ctl"},
	/*[17]*/	{"num_meridional_layering_ctl"},
	/*[18]*/	{"radial_layering_ctl"},
	/*[19]*/	{"meridional_layering_ctl"}
};


void alloc_sphere_domain_ctl_c(struct sphere_domain_ctl_c *sdctl_c){
	int i;
	
	sdctl_c->maxlen = 0;
	for (i=0;i<NLBL_SPHERE_DOMAIN_CTL;i++){
		if(strlen(label_sphere_domain_ctl[i]) > sdctl_c->maxlen){
			sdctl_c->maxlen = strlen(label_sphere_domain_ctl[i]);
		};
	};
	
	sdctl_c->inner_decomp_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(sdctl_c->inner_decomp_c);
	
	sdctl_c->num_radial_domain_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	sdctl_c->num_horiz_domain_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_ctl_int_item(sdctl_c->num_radial_domain_c);
	init_ctl_int_item(sdctl_c->num_horiz_domain_c);
	
	sdctl_c->ndomain_sph_grid_c = (struct chara_int_ctl_array *) malloc(sizeof(struct chara_int_ctl_array));
	sdctl_c->ndomain_legendre_c = (struct chara_int_ctl_array *) malloc(sizeof(struct chara_int_ctl_array));
	sdctl_c->ndomain_spectr_c = (struct chara_int_ctl_array *) malloc(sizeof(struct chara_int_ctl_array));
	init_ctl_ci_array(sdctl_c->ndomain_sph_grid_c);
	init_ctl_ci_array(sdctl_c->ndomain_legendre_c);
	init_ctl_ci_array(sdctl_c->ndomain_spectr_c);
	
	return;
};

void dealloc_sphere_domain_ctl_c(struct sphere_domain_ctl_c *sdctl_c){
	
	dealloc_ctl_chara_item(sdctl_c->inner_decomp_c);
	free(sdctl_c->inner_decomp_c);
	
	free(sdctl_c->num_radial_domain_c);
	free(sdctl_c->num_horiz_domain_c);
	
	dealloc_ctl_ci_array(sdctl_c->ndomain_sph_grid_c);
	dealloc_ctl_ci_array(sdctl_c->ndomain_legendre_c);
	dealloc_ctl_ci_array(sdctl_c->ndomain_spectr_c);
	free(sdctl_c->ndomain_sph_grid_c);
	free(sdctl_c->ndomain_legendre_c);
	free(sdctl_c->ndomain_spectr_c);
	
	return;
};

int read_sphere_domain_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sphere_domain_ctl_c *sdctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		fgets(buf, LENGTHBUF, fp);
		
		read_character_ctl_item_c(buf, label_sphere_domain_ctl[ 0], sdctl_c->inner_decomp_c);
		
		read_integer_ctl_item_c(buf, label_sphere_domain_ctl[ 1], sdctl_c->num_radial_domain_c);
		read_integer_ctl_item_c(buf, label_sphere_domain_ctl[ 2], sdctl_c->num_horiz_domain_c);
		
		read_ci_ctl_array_c(fp, buf, label_sphere_domain_ctl[ 3],sdctl_c->ndomain_sph_grid_c);
		read_ci_ctl_array_c(fp, buf, label_sphere_domain_ctl[ 4],sdctl_c->ndomain_legendre_c);
		read_ci_ctl_array_c(fp, buf, label_sphere_domain_ctl[ 5],sdctl_c->ndomain_spectr_c);
	};
	return 1;
};

int write_sphere_domain_ctl_c(FILE *fp, int level,
			const char *label, struct sphere_domain_ctl_c *sdctl_c){
    level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_character_ctl_item_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 0], sdctl_c->inner_decomp_c);
	
	write_integer_ctl_item_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 1], sdctl_c->num_radial_domain_c);
	write_integer_ctl_item_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 2], sdctl_c->num_horiz_domain_c);
	
	if(sdctl_c->ndomain_sph_grid_c->num > 0) fprintf(fp, "!\n");
	write_ci_ctl_array_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 3], sdctl_c->ndomain_sph_grid_c);
	if(sdctl_c->ndomain_legendre_c->num > 0) fprintf(fp, "!\n");
	write_ci_ctl_array_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 4], sdctl_c->ndomain_legendre_c);
	if(sdctl_c->ndomain_spectr_c->num > 0) fprintf(fp, "!\n");
	write_ci_ctl_array_c(fp, level, sdctl_c->maxlen, label_sphere_domain_ctl[ 5], sdctl_c->ndomain_spectr_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};


void alloc_sphere_data_ctl_c(struct sphere_data_ctl_c *spctl_c){
	int i;
	
	spctl_c->maxlen = 0;
	for (i=0;i<NLBL_SPHERE_DATA_CTL;i++){
		if(strlen(label_sphere_data_ctl[i]) > spctl_c->maxlen){
			spctl_c->maxlen = strlen(label_sphere_data_ctl[i]);
		};
	};
	
	spctl_c->ltr_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	spctl_c->phi_symmetry_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_ctl_int_item(spctl_c->ltr_c);
	init_ctl_int_item(spctl_c->phi_symmetry_c);
	
	spctl_c->sph_grid_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	spctl_c->sph_coef_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_ctl_chara_item(spctl_c->sph_grid_type_c);
	alloc_ctl_chara_item(spctl_c->sph_coef_type_c);
	
	spctl_c->ngrid_elevation_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	spctl_c->ngrid_azimuth_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_ctl_int_item(spctl_c->ngrid_elevation_c);
	init_ctl_int_item(spctl_c->ngrid_azimuth_c);
	
	spctl_c->radius_c = (struct int_real_ctl_array *) malloc(sizeof(struct int_real_ctl_array));
	init_ctl_ir_array(spctl_c->radius_c);
	spctl_c->radial_grp_c = (struct chara_int_ctl_array *) malloc(sizeof(struct chara_int_ctl_array));
	init_ctl_ci_array(spctl_c->radial_grp_c);
	
	spctl_c->radial_grid_type_c = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	spctl_c->num_fluid_grid_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	alloc_ctl_chara_item(spctl_c->radial_grid_type_c);
	init_ctl_int_item(spctl_c->num_fluid_grid_c);
	
	spctl_c->Min_radius_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	spctl_c->ICB_radius_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	spctl_c->CMB_radius_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	spctl_c->Max_radius_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	spctl_c->fluid_core_size_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	spctl_c->ICB_to_CMB_ratio_c = (struct real_ctl_item *) malloc(sizeof(struct real_ctl_item));
	init_ctl_real_item(spctl_c->Min_radius_c);
	init_ctl_real_item(spctl_c->ICB_radius_c);
	init_ctl_real_item(spctl_c->CMB_radius_c);
	init_ctl_real_item(spctl_c->Max_radius_c);
	init_ctl_real_item(spctl_c->fluid_core_size_c);
	init_ctl_real_item(spctl_c->ICB_to_CMB_ratio_c);
	
	spctl_c->num_radial_layer_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	spctl_c->num_med_layer_c = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item));
	init_ctl_int_item(spctl_c->num_radial_layer_c);
	init_ctl_int_item(spctl_c->num_med_layer_c);
	
	spctl_c->radial_layer_list_c = (struct int2_ctl_array *) malloc(sizeof(struct int2_ctl_array));
	spctl_c->med_layer_list_c = (struct int2_ctl_array *) malloc(sizeof(struct int2_ctl_array));
	init_ctl_int2_array(spctl_c->radial_layer_list_c);
	init_ctl_int2_array(spctl_c->med_layer_list_c);
	
	return;
};

void dealloc_sphere_data_ctl_c(struct sphere_data_ctl_c *spctl_c){
	
	free(spctl_c->ltr_c);
	free(spctl_c->phi_symmetry_c);
	
	dealloc_ctl_chara_item(spctl_c->sph_grid_type_c);
	dealloc_ctl_chara_item(spctl_c->sph_coef_type_c);
	free(spctl_c->sph_grid_type_c);
	free(spctl_c->sph_coef_type_c);
	
	free(spctl_c->ngrid_elevation_c);
	free(spctl_c->ngrid_azimuth_c);
	
	dealloc_ctl_ir_array(spctl_c->radius_c);
	dealloc_ctl_ci_array(spctl_c->radial_grp_c);
	free(spctl_c->radius_c);
	free(spctl_c->radial_grp_c);
	
	dealloc_ctl_chara_item(spctl_c->radial_grid_type_c);
	free(spctl_c->radial_grid_type_c);
	free(spctl_c->num_fluid_grid_c);
	
	free(spctl_c->Min_radius_c);
	free(spctl_c->ICB_radius_c);
	free(spctl_c->CMB_radius_c);
	free(spctl_c->Max_radius_c);
	free(spctl_c->fluid_core_size_c);
	free(spctl_c->ICB_to_CMB_ratio_c);
	
	free(spctl_c->num_radial_layer_c);
	free(spctl_c->num_med_layer_c);
	
	dealloc_ctl_int2_array(spctl_c->radial_layer_list_c);
	dealloc_ctl_int2_array(spctl_c->med_layer_list_c);
	free(spctl_c->radial_layer_list_c);
	free(spctl_c->med_layer_list_c);
	
	return;
};

int read_sphere_data_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sphere_data_ctl_c *spctl_c){
	while(find_control_end_flag_c(buf, label) == 0){
		
		fgets(buf, LENGTHBUF, fp);
		
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 3], spctl_c->ltr_c);
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 4], spctl_c->phi_symmetry_c);
		
		read_character_ctl_item_c(buf, label_sphere_data_ctl[ 6], spctl_c->sph_grid_type_c);
		read_character_ctl_item_c(buf, label_sphere_data_ctl[ 5], spctl_c->sph_coef_type_c);
		
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 1], spctl_c->ngrid_elevation_c);
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 2], spctl_c->ngrid_azimuth_c);
		
		read_ir_ctl_array_c(fp, buf, label_sphere_data_ctl[ 0], spctl_c->radius_c);
		
		read_character_ctl_item_c(buf, label_sphere_data_ctl[ 7], spctl_c->radial_grid_type_c);
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[ 8], spctl_c->num_fluid_grid_c);
		
		read_real_ctl_item_c(buf, label_sphere_data_ctl[ 9], spctl_c->Min_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[10], spctl_c->ICB_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[11], spctl_c->CMB_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[12], spctl_c->Max_radius_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[13], spctl_c->fluid_core_size_c);
		read_real_ctl_item_c(buf, label_sphere_data_ctl[14], spctl_c->ICB_to_CMB_ratio_c);
		
		read_ci_ctl_array_c(fp, buf, label_sphere_data_ctl[15], spctl_c->radial_grp_c);
		
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[16], spctl_c->num_radial_layer_c);
		read_integer_ctl_item_c(buf, label_sphere_data_ctl[17], spctl_c->num_med_layer_c);
		
		read_int2_ctl_array_c(fp, buf, label_sphere_data_ctl[18], spctl_c->radial_layer_list_c);
		read_int2_ctl_array_c(fp, buf, label_sphere_data_ctl[19], spctl_c->med_layer_list_c);
	};
	return 1;
};

int write_sphere_data_ctl_c(FILE *fp, int level,
			const char *label, struct sphere_data_ctl_c *spctl_c){
	level = write_begin_flag_for_ctl_c(fp, level, label);
	
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 3], spctl_c->ltr_c);
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 4], spctl_c->phi_symmetry_c);
	
	write_character_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 6], spctl_c->sph_grid_type_c);
	write_character_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 5], spctl_c->sph_coef_type_c);
	
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 1], spctl_c->ngrid_elevation_c);
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 2], spctl_c->ngrid_azimuth_c);
	
	if(spctl_c->radius_c->num > 0) fprintf(fp, "!\n");
	write_ir_ctl_array_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 0], spctl_c->radius_c);
	
	write_character_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 7], spctl_c->radial_grid_type_c);
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 8], spctl_c->num_fluid_grid_c);
	
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[ 9], spctl_c->Min_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[10], spctl_c->ICB_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[11], spctl_c->CMB_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[12], spctl_c->Max_radius_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[13], spctl_c->fluid_core_size_c);
	write_real_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[14], spctl_c->ICB_to_CMB_ratio_c);
	
	if(spctl_c->radial_grp_c->num > 0) fprintf(fp, "!\n");
	write_ci_ctl_array_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[15], spctl_c->radial_grp_c);
	
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[16], spctl_c->num_radial_layer_c);
	write_integer_ctl_item_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[17], spctl_c->num_med_layer_c);
	
	if(spctl_c->radial_layer_list_c->num > 0) fprintf(fp, "!\n");
	write_int2_ctl_array_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[18], spctl_c->radial_layer_list_c);
	if(spctl_c->med_layer_list_c->num > 0) fprintf(fp, "!\n");
	write_int2_ctl_array_c(fp, level, spctl_c->maxlen, label_sphere_data_ctl[19], spctl_c->med_layer_list_c);
	
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};
