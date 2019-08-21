/*
//  m_kemoview_fline.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoview_fline.h"

struct kemoview_fline * init_kemoview_fline(){
	struct kemoview_fline *kemo_fline
			= (struct kemoview_fline *) malloc(sizeof(struct kemoview_fline));
	if(kemo_fline == NULL){
		printf("malloc error for kemoview_fline\n");
		exit(0);
	}
	
	kemo_fline->fline_d =  (struct psf_data *)     malloc(NMAX_PSF*sizeof(struct psf_data));
	kemo_fline->fline_m =  (struct fline_menu_val *) malloc(NMAX_PSF*sizeof(struct fline_menu_val));
	return kemo_fline;
};

void dealloc_kemoview_fline(struct kemoview_fline *kemo_fline){
	free(kemo_fline->fline_d);
	free(kemo_fline->fline_m);
	free(kemo_fline);
	return;
};

void close_fieldline_view(struct kemoview_fline *kemo_fline){
	kemo_fline->fline_m->iflag_draw_fline = IZERO;
	dealloc_draw_fline_flags(kemo_fline->fline_d, kemo_fline->fline_m);
	deallc_all_fline_data(kemo_fline->fline_d);
	return;
}


int evolution_fline_viewer(struct kemoview_fline *kemo_fline,
			struct psf_data *psf_ucd_tmp, int istep_sync){
	int ierr = 0;
	if (kemo_fline->fline_m->iflag_draw_fline > 0) {
		kemo_fline->fline_m->fline_step = istep_sync;
		ierr = refresh_FLINE_data(kemo_fline->fline_d, 
					psf_ucd_tmp, kemo_fline->fline_m);
	}
	return ierr;
}
