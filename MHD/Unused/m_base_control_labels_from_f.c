/*
//  m_base_control_labels_from_f.c
//
//  Created by Hiroaki Matsui on 07/03/20.
//
*/

#include "m_base_control_labels_from_f.h"

int num_label_time_step_ctl_f();
int num_label_time_step_ctl_w_dep_f();
void set_label_time_step_ctl_f(char *name1);

struct control_labels_f * init_label_time_step_ctl_w_dpl(){
	struct control_labels_f *label_time_step_ctl_w_dpl
			= init_control_labels_f(num_label_time_step_ctl_w_dep_f, 
									set_label_time_step_ctl_f);
	return label_time_step_ctl_w_dpl;
};
