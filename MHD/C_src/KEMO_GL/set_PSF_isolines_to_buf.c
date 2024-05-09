/*
// set_PSF_isolines_to_buf.c
*/

#include "set_PSF_isolines_to_buf.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

double cal_isoline_value(int j, int n_isoline, struct colormap_params *cmap_s){
	double v_line, range_min, range_max;
	double v1, v2;
	int num = count_real2_clist(cmap_s->colormap);
	set_from_real2_clist_at_index(0,     cmap_s->colormap, &range_min, &v1);
	set_from_real2_clist_at_index(num-1, cmap_s->colormap, &range_max, &v2);
	
	v_line = range_min + (range_max - range_min)
			* ((double) j) / ((double) (n_isoline-1));
	
	return v_line;
}

int find_start_positive_lines(int n_isoline, struct colormap_params *cmap_s){
	int ist_positive_line = 0;
    int j;
    double pre_value, current_value, range_min, range_max;
	double v1, v2;
	int num = count_real2_clist(cmap_s->colormap);
	set_from_real2_clist_at_index(0,     cmap_s->colormap, &range_min, &v1);
	set_from_real2_clist_at_index(num-1, cmap_s->colormap, &range_max, &v2);
    
    if(range_min >= ZERO) ist_positive_line = 0;
    else if(range_max <= ZERO){
        ist_positive_line = n_isoline;
    } else {
        ist_positive_line = 0;
        current_value = range_min;
        for (j = 1; j < n_isoline; j++){
            pre_value = current_value;
            current_value = cal_isoline_value(j, n_isoline, cmap_s);
            if((current_value*pre_value) <= ZERO){return j;};
        };
    };
    
    return ist_positive_line;
}

static long add_num_PSF_isolines(long ist_patch, const int nthreads, int ist, int ied,
                                 struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                 long *istack_smp_psf_iso){
	int j;
	double v_line;
	
	long num_patch = ist_patch;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline,
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		num_patch = add_each_isoline_npatch_pthread(num_patch, nthreads, v_line,
                                                    psf_m->icomp_draw_psf, psf_s,
                                                    &istack_smp_psf_iso[j*nthreads]);
	};
	return num_patch;
}

static long set_PSF_isolines_to_buf(const long ist_patch, const int nthreads,
                                    long *istack_smp_psf_iso, int ist, int ied,
                                    struct psf_data *psf_s, struct psf_menu_val *psf_m, 
                                    struct gl_strided_buffer *psf_buf,
                                    struct gl_local_buffer_address **para_point_buf){
	int j, nd;
	double v_line;
	double f_color[4];
	
    struct colormap_params *cmap_s = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	long inum_patch = ist_patch;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline, 
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		
		if (psf_m->isoline_color == RAINBOW_LINE){
			set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                   v_line, f_color);
		};
        inum_patch = set_each_isoline_to_buf_pthread(inum_patch, nthreads,
                                                     &istack_smp_psf_iso[j*nthreads],
                                                     psf_m->isoline_width, v_line,
                                                     psf_m->icomp_draw_psf, f_color,
                                                     psf_s, psf_buf, para_point_buf);
/*
		inum_patch = set_each_isoline_to_buf(inum_patch, IZERO, psf_s->nele_viz,
                                             psf_m->isoline_width,
											 v_line, psf_m->icomp_draw_psf,
                                             f_color, psf_s, psf_buf, point_buf);
*/
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	return inum_patch;
}


long add_PSF_all_isolines_num(const long ist_patch, const int nthreads,
                              struct psf_data *psf_s, struct psf_menu_val *psf_m,
                              long *istack_smp_psf_iso){
	long num_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			num_patch = add_num_PSF_isolines(num_patch, nthreads,
                                             IZERO, psf_m->ist_positive_line,
											 psf_s, psf_m, istack_smp_psf_iso);
		};
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            num_patch = add_num_PSF_isolines(num_patch, nthreads,
                                             psf_m->ist_positive_line, psf_m->n_isoline,
											 psf_s, psf_m, istack_smp_psf_iso);
        };
    };
	if(psf_m->draw_psf_zero != 0){
		num_patch = add_each_isoline_npatch_pthread(num_patch, nthreads, ZERO,
                                                    psf_m->icomp_draw_psf, psf_s,
                                                    &istack_smp_psf_iso[psf_m->n_isoline*nthreads]);
    };
	return num_patch;
}

long set_PSF_all_isolines_to_buf(const long ist_patch,
                                 const int nthreads, long *istack_smp_psf_iso,
                                 struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                 struct gl_strided_buffer *psf_buf,
                                 struct gl_local_buffer_address **para_point_buf){
	double dub_r;
	long inum_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			inum_patch = set_PSF_isolines_to_buf(inum_patch, nthreads, istack_smp_psf_iso,
                                                 IZERO, psf_m->ist_positive_line,
                                                 psf_s, psf_m, psf_buf, para_point_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			inum_patch = set_PSF_isolines_to_buf(inum_patch, nthreads, istack_smp_psf_iso,
                                                 psf_m->ist_positive_line, psf_m->n_isoline,
                                                 psf_s, psf_m, psf_buf, para_point_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
		dub_r = 2.0 * psf_m->isoline_width;
        inum_patch = set_each_isoline_to_buf_pthread(inum_patch, nthreads,
                                                     istack_smp_psf_iso[psf_m->n_isoline*nthreads],
                                                     dub_r, ZERO, psf_m->icomp_draw_psf, black,
                                                     psf_s, psf_buf, para_point_buf);
	};
	
	return inum_patch;
}
