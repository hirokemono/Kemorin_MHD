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
#include "kemoviewer_base.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_kemoview_fline_menu.h"
#include "set_each_fline_parameters.h"
#include "read_data_4_kemoviewer.h"
#include "set_each_psf_parameters.h"

struct kemoview_fline{
    struct psf_data           *fline_d;
    struct fline_directions   *fline_dir;
    struct psf_menu_val       *fline_m;
};

/* prototypes */ 
struct kemoview_fline * init_kemoview_fline(void);
void dealloc_kemoview_fline(struct kemoview_fline *kemo_fline);

void close_fieldline_view(struct kemoview_fline *kemo_fline);
void init_draw_fline(struct kemoview_fline *kemo_fline, struct psf_data *ucd_tmp,
			int iformat_ucd_file, int istep, const char *ucd_header);

int evolution_fline_viewer(struct kemoview_fline *kemo_fline,
			struct psf_data *psf_ucd_tmp, int istep_sync);

#endif
