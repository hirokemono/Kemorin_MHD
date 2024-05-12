/*
 *  m_fline_data_4_viewer_c.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef M_FLINE_DATA_4_VIEWER_C_
#define M_FLINE_DATA_4_VIEWER_C_

#include <stdlib.h>
#include <stdio.h>
#include "calypso_param_c.h"

struct fline_data{
    long nnod_fline;
	double *dir_nod;
};

/* prototypes */

struct fline_data * init_fline_data(void);
void alloc_fline_data(long nnod_fline, struct fline_data *fline_d);

void dealloc_fline_data(struct fline_data *fline_d);

#endif  /* M_FLINE_DATA_4_VIEWER_C_ */
