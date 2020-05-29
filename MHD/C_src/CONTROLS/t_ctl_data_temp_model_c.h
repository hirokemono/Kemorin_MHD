/*
//  t_ctl_data_temp_model_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/01.
*/

#ifndef t_ctl_data_temp_model_c_h_
#define t_ctl_data_temp_model_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

struct reference_point_ctl_c{
    int iflag_use;
    int maxlen;
	
	struct real_ctl_item *value_c;
	struct real_ctl_item *depth_c;
};

struct takepiro_model_ctl_c{
    int iflag_use;
    int maxlen;
	
	struct real_ctl_item *stratified_sigma_c;
	struct real_ctl_item *stratified_width_c;
	struct real_ctl_item *stratified_outer_r_c;
};

struct reference_temperature_c{
    int iflag_use;
    int maxlen;
	
    struct chara_ctl_item *reference_c;
	struct chara_ctl_item *stratified_c;
	
	struct reference_point_ctl_c *low_c;
	struct reference_point_ctl_c *high_c;
	struct takepiro_model_ctl_c *takepiro_c;
};

/* prototypes */
void get_label_reference_point_ctl(int index, char *label);
void get_label_takepiro_model_ctl(int index, char *label);
void get_label_ref_temperature_ctl(int index, char *label);

struct reference_point_ctl_c * init_reference_point_ctl_c();
void dealloc_reference_point_ctl_c(struct reference_point_ctl_c *ref_c);
void read_reftemp_point_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_point_ctl_c *ref_c);
void read_refcomp_point_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_point_ctl_c *ref_c);
int write_reftemp_point_ctl_c(FILE *fp, int level, const char *label, 
                              struct reference_point_ctl_c *ref_c);
int write_refcomp_point_ctl_c(FILE *fp, int level, const char *label, 
                              struct reference_point_ctl_c *ref_c);

struct takepiro_model_ctl_c * init_takepiro_model_ctl_c();
void dealloc_takepiro_model_ctl_c(struct takepiro_model_ctl_c *takepiro_c);
void read_rtakepiro_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct takepiro_model_ctl_c *takepiro_c);
int write_takepiro_model_ctl_c(FILE *fp, int level, const char *label, 
                               struct takepiro_model_ctl_c *takepiro_c);

struct reference_temperature_c * init_ref_temperature_ctl_c();
void dealloc_ref_temperature_ctl_c(struct reference_temperature_c *reft_ctl);
void read_ref_temperature_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_temperature_c *reft_ctl);
void read_ref_composition_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_temperature_c *refc_ctl);
int write_ref_temperature_ctl_c(FILE *fp, int level, const char *label, 
                                struct reference_temperature_c *reft_ctl);
int write_ref_composition_ctl_c(FILE *fp, int level, const char *label,
                                struct reference_temperature_c *refc_ctl);



#endif /* t_ctl_data_temp_model_c_h_ */
