/*
 *  set_color_code_on_nodes.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/15.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_color_code_on_nodes.h"

static const double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static const double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};
static const double gray[4] =    {0.2,0.2,0.2,0.5};

static void set_color_code_for_psf(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int inod, nd;
	double d_patch = 0.0;
	
	struct colormap_params *cmap_s = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	if(psf_m->psf_patch_color == WHITE_SURFACE) {
		for(inod=0; inod< psf_s->nnod_viz; inod++){
			for(nd=0;nd<3;nd++){psf_s->color_nod[4*inod+nd] = white[nd];};
            psf_s->color_nod[4*inod+3] = set_opacity_from_value_s(omap_array, d_patch);
		};
	}else if(psf_m->psf_patch_color == TEXTURED_SURFACE) {
        for(inod=0; inod< psf_s->nnod_viz; inod++){
            d_patch =  psf_s->d_nod[inod*psf_s->ncomptot + psf_m->icomp_draw_psf];
            set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                   d_patch, &psf_s->color_nod[4*inod]);

            for(nd=0;nd<2;nd++){
                psf_s->color_nod[4*inod+nd] = gray[nd] * psf_s->color_nod[4*inod+nd];
                
            };
/*
            for(nd=0;nd<3;nd++){psf_s->color_nod[4*inod+nd] = gray[nd];};
 */
            psf_s->color_nod[4*inod+3] = set_opacity_from_value_s(omap_array, d_patch);
        };
    }else if(psf_m->psf_patch_color == SINGLE_COLOR) {
        for (inod=0; inod< psf_s->nnod_viz; inod++){
            for(nd=0;nd<3;nd++){
				psf_s->color_nod[4*inod+nd] = cmap_s->single_color[nd];
			};
            psf_s->color_nod[4*inod+3] = set_opacity_from_value_s(omap_array, d_patch);
        };
/*
	}else if(psf_m->psf_patch_color == BLACK_LINE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			for(nd=0;nd<4;nd++){psf_s->color_nod[4*inod+nd] = black[nd];};
		};
*/
	}else if(psf_m->psf_patch_color == RAINBOW_SURFACE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			d_patch =  psf_s->d_nod[inod*psf_s->ncomptot + psf_m->icomp_draw_psf];
			set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                   d_patch, &psf_s->color_nod[4*inod]);
		};
/*
    }else if(psf_m->psf_patch_color == TWO_COLOR_LINE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			d_patch =  psf_s->d_nod[inod*psf_s->ncomptot + psf_m->icomp_draw_fline];
			set_two_color_scale_c(d_patch, &psf_s->color_nod[4*inod]);

			psf_s->color_nod[4*inod+3] = set_opacity_from_value_s(omap_array, d_patch);
		};
	}else if(psf_m->psf_patch_color == TWO_GRAY_LINE) {
		for (inod=0; inod< psf_s->nnod_viz; inod++){
			d_patch =  psf_s->d_nod[inod*psf_s->ncomptot + psf_m->icomp_draw_fline];
			set_two_color_scale_g(d_patch, &psf_s->color_nod[4*inod]);

			psf_s->color_nod[4*inod+3]  = set_opacity_from_value_s(omap_array, d_patch);
		};
*/	
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	return;
}

void set_color_code_for_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m, 
                             struct kemo_array_control *psf_a){
    int i;
    
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0){
            set_color_code_for_psf(psf_s[i], psf_m[i]);
        };
    };
    return;
}


void set_color_code_for_fieldlines(struct fline_data *fline_d,
								   struct fline_menu_val *fline_m){
	int inod, nd;
	double d_edge;
	
	struct colormap_params *cmap_s = fline_m->cmap_fline;
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
	struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	if (fline_m->fieldline_color == WHITE_LINE) {
		for (inod=0; inod< fline_d->nnod_fline; inod++){
			for(nd=0;nd<4;nd++){fline_d->color_nod[4*inod+nd] = white[nd];};
		};
	}
	else if (fline_m->fieldline_color == BLACK_LINE) {
		for (inod=0; inod< fline_d->nnod_fline; inod++){
			for(nd=0;nd<4;nd++){fline_d->color_nod[4*inod+nd] = black[nd];};
		};
	}
	else if (fline_m->fieldline_color == RAINBOW_LINE) {
		for (inod=0; inod< fline_d->nnod_fline; inod++){
			d_edge =  fline_d->d_nod[inod*fline_d->ncomptot + fline_m->icomp_draw_fline];
			set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                   d_edge, &fline_d->color_nod[4*inod]);
		};
	}
	else if (fline_m->fieldline_color == TWO_COLOR_LINE) {
		for (inod=0; inod< fline_d->nnod_fline; inod++){
			d_edge =  fline_d->d_nod[inod*fline_d->ncomptot + fline_m->icomp_draw_fline];
			set_two_color_scale_c(d_edge, &fline_d->color_nod[4*inod]);
            
            fline_d->color_nod[4*inod+3] = set_opacity_from_value_s(omap_array, d_edge);
		};
	}
	else if (fline_m->fieldline_color == TWO_GRAY_LINE) {
		for (inod=0; inod< fline_d->nnod_fline; inod++){
			d_edge =  fline_d->d_nod[inod*fline_d->ncomptot + fline_m->icomp_draw_fline];
			set_two_color_scale_g(d_edge, &fline_d->color_nod[4*inod]);
            
            fline_d->color_nod[4*inod+3] = set_opacity_from_value_s(omap_array, d_edge);
		};
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	return;
}
