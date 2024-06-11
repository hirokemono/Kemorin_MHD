/*
//  m_kemoview_tracer.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/


#ifndef M_KEMOVIEW_TRACER_
#define M_KEMOVIEW_TRACER_


#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "read_data_4_kemoviewer.h"

struct kemoview_tracer{
    struct psf_data           *tracer_d;
    struct psf_menu_val       *tracer_m;
};

/* prototypes */ 
struct kemoview_tracer * init_kemoview_tracer(void);
void dealloc_kemoview_tracer(struct kemoview_tracer *kemo_tracer);

void close_tracer_view(struct kemoview_tracer *kemo_tracer);
void init_draw_tracer(struct kemoview_tracer *kemo_tracer, struct psf_data *ucd_tmp,
                      int iformat_ucd_file, int istep, const char *ucd_header);

#endif   /*  M_KEMOVIEW_TRACER_  */
