/*
//  m_force_name_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifndef M_FORCE_NAME_FROM_F_
#define M_FORCE_NAME_FROM_F_

struct advection_names_f{
	int len_f;
	
	int istack_advection_controls[2];
	
	int ntot_advection;
	
	int *num_comp;
	char **term_name;
	char **term_math;
};

struct force_names_f{
	int len_f;
	
	int istack_force_controls[2];
	
	int ntot_forcrs;
	
	int *num_comp;
	char **force_name;
	char **forth_math;
};

/*  prototype */

struct advection_names_f * init_advection_name_f();
void dealloc_advection_name_f(struct advection_names_f *fnames);

struct force_names_f * init_force_name_f();
void dealloc_force_name_f(struct force_names_f *fnames);

#endif

