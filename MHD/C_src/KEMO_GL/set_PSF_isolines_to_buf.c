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


static int count_isoline_to_buf(int ncorner, double v_line, int icomp, 
			struct psf_data *psf_s){
	double d_tri[3];
	int inod, iele, k;
	int num_patch = 0;
	
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] = psf_s->d_nod[inod][icomp];
		};
		
		/*  find isoline */
		if(mark_isoline_on_patch_c(d_tri, v_line) == 1){num_patch = num_patch + 2*ncorner;};
	};
	
	return num_patch;
};

static int set_isoline_to_buf(int ist_patch, int ncorner, double width, 
			double v_line, int icomp, double *f_color,
			struct psf_data *psf_s, struct gl_strided_buffer *strided_buf){
	
	double d_tri[3], xx_tri[9];
	double x_line[6], dir_line[6], norm_line[6], color_line[8];
	
	int idraw;
	int inod, iele, k, nd;
	
	int inum_patch = ist_patch;
	
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		/*  find isoline */
		idraw = find_isoline_on_patch_c(x_line, dir_line, norm_line, 
										xx_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
			for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
			inum_patch = set_tube_strided_buffer(inum_patch, ncorner, width, 
						x_line, dir_line, norm_line, color_line, strided_buf);
		};
	};

	return inum_patch;
};


static int count_PSF_zeroline(int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	/*
	int num_patch = count_isoline_to_buf(ncorner, ZERO, psf_m->icomp_draw_psf, psf_s);
	*/
	return add_each_isoline_npatch(0, ZERO, psf_m->icomp_draw_psf, psf_s);
}

static int count_PSF_isolines(int ist, int ied, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int j;
	double v_line;
	
	int num_patch = 0;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline,
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		num_patch = num_patch 
				+ count_isoline_to_buf(ncorner, v_line, psf_m->icomp_draw_psf, psf_s);
	};
	return num_patch;
}


static int set_PSF_zeroline_to_buf(int ist_patch, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
	double dub_r = 2.0 * psf_m->isoline_width;
	int inum_patch = ist_patch;
	/*
	inum_patch = set_isoline_to_buf(inum_patch, ncorner, dub_r, ZERO, 
									psf_m->icomp_draw_psf, black, psf_s, psf_buf);
	*/
	inum_patch = set_each_isoline_to_buf(inum_patch, dub_r, ZERO,
                                        psf_m->icomp_draw_psf, black,
                                        psf_s, psf_buf);
	return inum_patch;
}

static int set_PSF_isolines_to_buf(int ist_patch, int ist, int ied, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *psf_buf){
	int inum_patch;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	inum_patch = ist_patch;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline, 
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		
		if (psf_m->isoline_color == RAINBOW_LINE){
			set_rainbow_color_code(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], v_line, f_color);
		};
		inum_patch = set_isoline_to_buf(inum_patch, ncorner, psf_m->isoline_width, 
					v_line, psf_m->icomp_draw_psf, f_color, psf_s, psf_buf);
	};
	
	return inum_patch;
}


int count_PSF_all_isolines_to_buf(int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_patch = 0;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			num_patch = num_patch + count_PSF_isolines(IZERO, psf_m->ist_positive_line,
						ncorner, psf_s, psf_m);
		};
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            num_patch = num_patch + count_PSF_isolines(psf_m->ist_positive_line, psf_m->n_isoline,
						ncorner, psf_s, psf_m);
        };
    };
	if(psf_m->draw_psf_zero != 0){
        num_patch = num_patch + count_PSF_zeroline(ncorner, psf_s, psf_m);
    };
	return num_patch;
}

int set_PSF_all_isolines_to_buf(int ist_patch, int ncorner, 
								struct psf_data *psf_s, struct psf_menu_val *psf_m,
								struct gl_strided_buffer *psf_buf){
	int inum_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			inum_patch = set_PSF_isolines_to_buf(inum_patch, 
							IZERO, psf_m->ist_positive_line,
							ncorner, psf_s, psf_m, psf_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			inum_patch = set_PSF_isolines_to_buf(inum_patch, 
							psf_m->ist_positive_line, psf_m->n_isoline,
							ncorner, psf_s, psf_m, psf_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
		inum_patch = set_PSF_zeroline_to_buf(inum_patch, ncorner, psf_s, psf_m, psf_buf);
	};
	
	return inum_patch;
}
