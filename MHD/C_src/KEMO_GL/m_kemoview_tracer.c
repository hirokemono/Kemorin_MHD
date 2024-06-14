/*
//  m_kemoview_tracer.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoview_tracer.h"

struct kemoview_tracer * init_kemoview_tracer(void){
	struct kemoview_tracer *kemo_tracer
			= (struct kemoview_tracer *) malloc(sizeof(struct kemoview_tracer));
	if(kemo_tracer == NULL){
		printf("malloc error for kemoview_tracer\n");
		exit(0);
	}
	
    kemo_tracer->tracer_d =   init_psf_data();
    kemo_tracer->tracer_m =   init_psf_menu_val();
	return kemo_tracer;
};

void dealloc_kemoview_tracer(struct kemoview_tracer *kemo_tracer){
    free(kemo_tracer->tracer_d);
	free(kemo_tracer->tracer_m);
	free(kemo_tracer);
	return;
};

void close_tracer_view(struct kemoview_tracer *kemo_tracer){
	kemo_tracer->tracer_m->iflag_draw_viz = IZERO;
    dealloc_draw_psf_flags(kemo_tracer->tracer_d->nfield,
                           kemo_tracer->tracer_d->ncomptot,
                           kemo_tracer->tracer_m);
    
	dealloc_psf_data_s(kemo_tracer->tracer_d);
    dealloc_psf_color_data_c(kemo_tracer->tracer_d);
    dealloc_psf_field_data_c(kemo_tracer->tracer_d);
	dealloc_viz_node_s(kemo_tracer->tracer_d);
    return;
}

void init_draw_tracer(struct kemoview_tracer *kemo_tracer, struct psf_data *ucd_tmp,
			int iformat_ucd_file, int istep, const char *ucd_header){
    kemo_tracer->tracer_m->viz_prefix_c = alloc_kvstring();
    alloc_copy_string(ucd_header, kemo_tracer->tracer_m->viz_prefix_c);
	kemo_tracer->tracer_m->viz_step_c = istep;
	kemo_tracer->tracer_m->iformat_viz_file = iformat_ucd_file;
    
	if(kemo_tracer->tracer_m->iflag_draw_viz > 0) close_tracer_view(kemo_tracer);

    set_points_data_by_UCD(kemo_tracer->tracer_d, ucd_tmp);
	set_kemoview_viz_color_data(ORANGE_CYAN_MODE,
                                kemo_tracer->tracer_d,
                                kemo_tracer->tracer_m);
    return;
};

