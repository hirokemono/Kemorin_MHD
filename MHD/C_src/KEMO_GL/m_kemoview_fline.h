/*
//  m_kemoview_fline.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/


#ifndef M_KEMOVIEWER_FLINE_
#define M_KEMOVIEWER_FLINE_


#include "kemoviewer.h"
#include "m_kemoviewer_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "read_data_4_kemoviewer.h"

struct kemoview_fline{
    struct psf_data           *fline_d;
    struct fline_menu_val     *fline_m;
};

/* prototypes */ 
struct kemoview_fline * init_kemoview_fline();
void dealloc_kemoview_fline(struct kemoview_fline *kemo_fline);

void close_fieldline_view(struct kemoview_fline *kemo_fline);
int evolution_fline_viewer(struct kemoview_fline *kemo_fline,
			struct psf_data *psf_ucd_tmp, int istep_sync);
#endif
