/*
//  m_kemoview_fline.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoview_fline.h"

struct kemoview_fline * init_kemoview_fline(void){
	struct kemoview_fline *kemo_fline
			= (struct kemoview_fline *) malloc(sizeof(struct kemoview_fline));
	if(kemo_fline == NULL){
		printf("malloc error for kemoview_fline\n");
		exit(0);
	}
	
    kemo_fline->fline_d =   init_psf_data();
    kemo_fline->fline_dir = init_fline_directions();
    kemo_fline->fline_m =   init_psf_menu_val();
	return kemo_fline;
};

void dealloc_kemoview_fline(struct kemoview_fline *kemo_fline){
    free(kemo_fline->fline_d);
    free(kemo_fline->fline_dir);
	free(kemo_fline->fline_m);
	free(kemo_fline);
	return;
};

void close_fieldline_view(struct kemoview_fline *kemo_fline){
	kemo_fline->fline_m->iflag_draw_viz = IZERO;
    dealloc_draw_psf_flags(kemo_fline->fline_d->nfield,
                           kemo_fline->fline_d->ncomptot,
                           kemo_fline->fline_m);

    dealloc_fline_direction_data(kemo_fline->fline_dir);
    deallc_all_psf_data(kemo_fline->fline_d);
    return;
}

void init_draw_fline(struct kemoview_fline *kemo_fline, struct psf_data *ucd_tmp,
			int iformat_ucd_file, int istep, const char *ucd_header){
    kemo_fline->fline_m->viz_prefix_c = alloc_kvstring();
    alloc_copy_string(ucd_header, kemo_fline->fline_m->viz_prefix_c);
	kemo_fline->fline_m->viz_step_c = istep;
	kemo_fline->fline_m->iformat_viz_file = iformat_ucd_file;
    
	if(kemo_fline->fline_m->iflag_draw_viz > 0) close_fieldline_view(kemo_fline);

	set_fline_data_by_UCD(kemo_fline->fline_d, kemo_fline->fline_dir, ucd_tmp);
	set_kemoview_viz_color_data(ORANGE_CYAN_MODE,
                                kemo_fline->fline_d,
                                kemo_fline->fline_m);
    return;
};


int evolution_fline_viewer(struct kemoview_fline *kemo_fline,
			struct psf_data *psf_ucd_tmp, int istep_sync){
	int ierr = 0;
	if (kemo_fline->fline_m->iflag_draw_viz > 0) {
		kemo_fline->fline_m->viz_step_c = istep_sync;
		ierr = refresh_FLINE_data(psf_ucd_tmp,
                                  kemo_fline->fline_d,
                                  kemo_fline->fline_dir,
                                  kemo_fline->fline_m);
	}
	return ierr;
}
