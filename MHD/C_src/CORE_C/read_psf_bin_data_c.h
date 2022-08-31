/*
//  read_psf_bin_data_c.h
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2020/06/12.
*/

#ifndef read_psf_bin_data_c_h_
#define read_psf_bin_data_c_h_

#include <stdio.h>
#include <stdlib.h>
#include "skip_comment_c.h"
#include "calypso_rawfile_io_c.h"
#include "m_psf_data_4_viewer_c.h"

struct psf_bin_work{
    int ierr;
    int i_UNIX;
    int i_XINU;
    
    int iflag_keep;
    int iflag_swap;
    int ilength;
    int lchar_out;
    long nprocs;
    
    long *itmp_mp;
};

/*  prototype  */

struct psf_bin_work * init_psf_bin_work(void);
void dealloc_psf_bin_work(struct psf_bin_work *psf_b_WK);

int read_alloc_psf_mesh_bin(const char *bin_name, struct psf_data *psf_b);
int read_alloc_psf_bin(const char *bin_name, double *time, struct psf_data *psf_b);
int read_alloc_iso_bin(const char *bin_name, double *time, struct psf_data *psf_b);

#endif /* read_psf_bin_data_c_h_ */
