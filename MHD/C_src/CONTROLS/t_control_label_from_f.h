/*
//  t_control_label_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"

#ifndef T_CONTROL_LABEL_FROM_F_
#define T_CONTROL_LABEL_FROM_F_

struct control_labels_f{
	int len_f;
	int maxlen;
	
	int num_labels;
	char **label;
};

/*  prototype */

struct control_labels_f * init_control_labels_f(int (*num_list_func)(void),
                                                void (*name_list_func)(char *));
void dealloc_control_labels_f(struct control_labels_f *ctl_list);
void check_control_labels_f(struct control_labels_f *ctl_list);

#endif    /* T_CONTROL_LABEL_FROM_F_ */
