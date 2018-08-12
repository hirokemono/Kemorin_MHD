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
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"

struct reference_point_ctl_c{
    int maxlen;
	
	struct real_ctl_item *value_c;
	struct real_ctl_item *depth_c;
};

struct takepiro_model_ctl_c{
    int maxlen;
	
	struct real_ctl_item *stratified_sigma_c;
	struct real_ctl_item *stratified_width_c;
	struct real_ctl_item *stratified_outer_r_c;
};

struct reference_temperature_c{
    int maxlen;
	
    struct chara_ctl_item *reference_c;
	struct chara_ctl_item *stratified_c;
	
	int iflag_low_temp_ctl;
	struct reference_point_ctl_c *low_c;
	int iflag_high_temp_ctl;
	struct reference_point_ctl_c *high_c;
	int iflag_takepiro_c;
	struct takepiro_model_ctl_c *takepiro_c;
};

/* prototypes */
void get_label_reference_point_ctl(int index, char *label);
void get_label_takepiro_model_ctl(int index, char *label);
void get_label_ref_temperature_ctl(int index, char *label);

void alloc_reference_point_ctl_c(struct reference_point_ctl_c *ref_c);
void dealloc_reference_point_ctl_c(struct reference_point_ctl_c *ref_c);
int read_reftemp_point_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_point_ctl_c *ref_c);
int read_refcomp_point_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_point_ctl_c *ref_c);
int write_reftemp_point_ctl_c(FILE *fp, int level, const char *label, 
                              struct reference_point_ctl_c *ref_c);
int write_refcomp_point_ctl_c(FILE *fp, int level, const char *label, 
                              struct reference_point_ctl_c *ref_c);

void alloc_takepiro_model_ctl_c(struct takepiro_model_ctl_c *takepiro_c);
void dealloc_takepiro_model_ctl_c(struct takepiro_model_ctl_c *takepiro_c);
int read_rtakepiro_model_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct takepiro_model_ctl_c *takepiro_c);
int write_takepiro_model_ctl_c(FILE *fp, int level, const char *label, 
                               struct takepiro_model_ctl_c *takepiro_c);

void alloc_ref_temperature_ctl_c(struct reference_temperature_c *reft_ctl);
void dealloc_ref_temperature_ctl_c(struct reference_temperature_c *reft_ctl);
int read_ref_temperature_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_temperature_c *reft_ctl);
int read_ref_composition_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct reference_temperature_c *refc_ctl);
int write_ref_temperature_ctl_c(FILE *fp, int level, const char *label, 
                                struct reference_temperature_c *reft_ctl);
int write_ref_composition_ctl_c(FILE *fp, int level, const char *label,
                                struct reference_temperature_c *refc_ctl);



#endif /* t_ctl_data_temp_model_c_h_ */
