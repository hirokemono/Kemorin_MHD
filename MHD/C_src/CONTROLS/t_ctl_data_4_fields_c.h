/*
//  t_ctl_data_4_fields_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_4_fields_c_h_
#define t_ctl_data_4_fields_c_h_

#define NLBL_FIELD_CTL 2

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"
#include "t_control_chara3_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara3_items_c.h"
#include "t_ctl_array_chara_int_items_c.h"
#include "t_ctl_array_chara_int3_items_c.h"

#include "m_field_name_from_f.h"

struct f_MHD_fields_control{
    void * f_self;
    int * f_iflag;
    
    char * c_block_name;

	struct chara_int2_clist *f_field_ctl;
	struct chara_clist      *f_quad_phys;
    struct chara_int_clist  *f_scalar_phys;
    struct chara_int3_clist *f_vector_phys;
    
    int iflag_read;
    int maxlen;
};

struct all_field_ctl_c{
	struct field_names_f *fld_list;
	
	int *iflag_use;
	int *iflag_viz;
	int *iflag_monitor;
    int *iflag_quad;
};

/* prototype */
void get_label_field_ctl(int index, char *label);

struct f_MHD_fields_control * init_f_MHD_fields_control(void *(*c_load_self)(void *f_parent),
                                                        void *f_parent);


struct f_MHD_fields_control * init_field_ctl_c();
void dealloc_field_ctl_c(struct f_MHD_fields_control *fld_ctl);


void set_viz_flags_to_text(struct chara_int2_ctl_item *field_item,
                           struct chara3_ctl_item *tmp_fld_item);
void set_viz_flags_from_text(struct chara3_ctl_item *tmp_fld_item,
                             struct chara_int2_ctl_item *field_item);


void read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct f_MHD_fields_control *fld_ctl);
int write_field_ctl_c(FILE *fp, int level, const char *label,
                      struct f_MHD_fields_control *fld_ctl);


struct all_field_ctl_c * init_all_field_ctl_c();
void dealloc_all_field_ctl_c(struct all_field_ctl_c *all_fld_list);

void delete_field_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
                         struct chara_int2_clist *f_field_ctl);
void load_field_from_ctl(struct chara_int2_clist *f_field_ctl,
                         struct all_field_ctl_c *all_fld_list);


void add_field_wqflag_to_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
                             struct f_MHD_fields_control *fld_ctl);
void delete_field_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
                                struct f_MHD_fields_control *fld_ctl);
void update_field_flag_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
                                     struct f_MHD_fields_control *fld_ctl);

void load_field_w_qflag_from_ctl(struct f_MHD_fields_control *fld_ctl,
                                 struct all_field_ctl_c *all_fld_list);
void load_field_w_qflag_to_ctl(struct all_field_ctl_c *all_fld_list, 
                               struct f_MHD_fields_control *fld_ctl);
void reflesh_field_ctl_list(struct all_field_ctl_c *all_fld_list, 
                            struct f_MHD_fields_control *fld_ctl);

void check_field_ctl_list(struct f_MHD_fields_control *fld_ctl);
void check_field_and_comp_ctl_list(struct f_MHD_fields_control *fld_ctl);

#endif /* t_ctl_data_4_fields_c_h_ */
