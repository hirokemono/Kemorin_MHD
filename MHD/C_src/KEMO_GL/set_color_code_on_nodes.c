/*
 *  set_color_code_on_nodes.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/15.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_color_code_on_nodes.h"

static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};
static double gray[4] =   {0.2,0.2,0.2,0.5};
static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};

static void set_color_code_for_psf(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int inod, nd;
	double d_patch = 0.0;
	
	if (   psf_m->psf_patch_color == WHITE_SURFACE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			for(nd=0;nd<3;nd++){psf_s->color_nod[inod][nd] = white[nd];};
            psf_s->color_nod[inod][3] 
				= set_opacity_from_value_s(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], d_patch);
		};
	}

    if (psf_m->psf_patch_color == TEXTURED_SURFACE) {
        for (inod=0; inod< psf_s->nnod_viz; inod++){
            for(nd=0;nd<3;nd++){psf_s->color_nod[inod][nd] = gray[nd];};
            psf_s->color_nod[inod][3]
				= set_opacity_from_value_s(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], d_patch);
        };
    }

    if (   psf_m->psf_patch_color == SINGLE_COLOR) {
        for (inod=0; inod< psf_s->nnod_viz; inod++){
            for(nd=0;nd<3;nd++){
				psf_s->color_nod[inod][nd] = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]->single_color[nd];};
            psf_s->color_nod[inod][3]
				= set_opacity_from_value_s(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], d_patch);
        };
    }

/*
	else if (psf_m->psf_patch_color == BLACK_LINE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			for(nd=0;nd<4;nd++){psf_s->color_nod[inod][nd] = black[nd];};
		};
	}
*/
	else if (psf_m->psf_patch_color == RAINBOW_SURFACE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			d_patch =  psf_s->d_nod[inod][psf_m->icomp_draw_psf];
			set_rainbow_color_code(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], 
								   d_patch, &psf_s->color_nod[inod][0]);
		};
	};
/*
	else if (psf_m->psf_patch_color == TWO_COLOR_LINE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			d_patch =  psf_s->d_nod[inod][psf_m->icomp_draw_fline];
			set_two_color_scale_c(d_patch, psf_s->color_nod[inod]);
			psf_s->color_nod[inod][3]
 				= set_opacity_from_value_s(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], d_patch);
		};
	}
	else if (psf_m->psf_patch_color == TWO_GRAY_LINE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			d_patch =  psf_s->d_nod[inod][psf_m->icomp_draw_fline];
			set_two_color_scale_g(d_patch, psf_s->color_nod[inod]);
			psf_s->color_nod[inod][3] 
 				= set_opacity_from_value_s(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], d_patch);
		};
	};
*/	
	return;
}

void set_color_code_for_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m, 
                             struct kemo_array_control *psf_a){
    int i;
    
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_solid){
            set_color_code_for_psf(psf_s[i], psf_m[i]);
        };
    };
    return;
}


void set_color_code_for_fieldlines(struct psf_data *fline_s,
								   struct fline_menu_val *fline_m){
	int inod, nd;
	double d_edge;
	
	if (fline_m->fieldline_color == WHITE_LINE) {
		for (inod=0; inod< fline_s->nnod_viz; inod++){
			for(nd=0;nd<4;nd++){fline_s->color_nod[inod][nd] = white[nd];};
		};
	}
	else if (fline_m->fieldline_color == BLACK_LINE) {
		for (inod=0; inod< fline_s->nnod_viz; inod++){
			for(nd=0;nd<4;nd++){fline_s->color_nod[inod][nd] = black[nd];};
		};
	}
	else if (fline_m->fieldline_color == RAINBOW_LINE) {
		for (inod=0; inod< fline_s->nnod_viz; inod++){
			d_edge =  fline_s->d_nod[inod][fline_m->icomp_draw_fline];
			set_rainbow_color_code(fline_m->cmap_fline, d_edge, &fline_s->color_nod[inod][0]);
		};
	}
	else if (fline_m->fieldline_color == TWO_COLOR_LINE) {
		for (inod=0; inod< fline_s->nnod_viz; inod++){
			d_edge =  fline_s->d_nod[inod][fline_m->icomp_draw_fline];
			set_two_color_scale_c(d_edge, fline_s->color_nod[inod]);
			fline_s->color_nod[inod][3] = set_opacity_from_value_s(fline_m->cmap_fline, d_edge);
		};
	}
	else if (fline_m->fieldline_color == TWO_GRAY_LINE) {
		for (inod=0; inod< fline_s->nnod_viz; inod++){
			d_edge =  fline_s->d_nod[inod][fline_m->icomp_draw_fline];
			set_two_color_scale_g(d_edge, fline_s->color_nod[inod]);
			fline_s->color_nod[inod][3] = set_opacity_from_value_s(fline_m->cmap_fline, d_edge);
		};
	};

	return;
}
