/*
// set_PSF_isolines_to_buf.c
*/

#include "set_PSF_isolines_to_buf.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

double cal_isoline_value(int j, struct psf_menu_val *psf_m){
	double v_line, range_min, range_max;
	range_min = psf_m->cmap_psf->color_data[0];
	range_max = psf_m->cmap_psf->color_data[psf_m->cmap_psf->n_color_point-1];
	
	v_line = range_min + (range_max - range_min)
			* ((double) j) / ((double) psf_m->n_isoline-1);
	
	return v_line;
}

void find_start_positive_lines(struct psf_menu_val *psf_m){
    int j;
    double pre_value, current_value, range_min, range_max;
    range_min = psf_m->cmap_psf->color_data[0];
    range_max = psf_m->cmap_psf->color_data[psf_m->cmap_psf->n_color_point-1];
    
    if(range_min >= ZERO) psf_m->ist_positive_line = 1;
    else if(range_max <= ZERO){
        psf_m->ist_positive_line = psf_m->n_isoline + 1;
    } else {
        psf_m->ist_positive_line = 1;
        current_value = range_min;
        for (j = 1; j < psf_m->n_isoline; j++){
            pre_value = current_value;
            current_value = cal_isoline_value(j, psf_m);
            if( (current_value*pre_value) <= ZERO){
                psf_m->ist_positive_line = j + 1;
                return;
            };
        };
    };
    
    return;
}


static int count_isoline_to_buf(double width, double v_line, int icomp, struct psf_data *psf_s){
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	
	int idraw;
	int inod, iele, k;
	int num_line = 0;
	
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		/*  find isoline */
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xx_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){num_line = num_line + 1;};
	};
	
	return num_line;
};

static int set_isoline_to_buf(int ist_line, double width, double v_line, int icomp, double *f_color,
			struct psf_data *psf_s, struct gl_strided_buffer *strided_buf){
	
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	
	int icou_line;
	int idraw;
	int inod, iele, k, nd, k1;
	
	icou_line = ist_line;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		/*  find isoline */
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xx_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(k1=0;k1<6;k1++){
				set_node_stride_VBO((6*icou_line+k1), strided_buf);
				for(nd=0;nd<3;nd++) strided_buf->x_draw[nd] = x_ribbon[3*k1+nd];
				for(nd=0;nd<4;nd++) strided_buf->c_draw[nd] = f_color[nd];
			};
			icou_line = icou_line + 1;
		};
	};

	return icou_line;
};


static int count_PSF_zeroline(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_edge = count_isoline_to_buf(0.005, ZERO, psf_m->icomp_draw_psf, psf_s);
	return num_edge;
}

static int count_PSF_isolines(int ist, int ied, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int j;
	double v_line;
	
	int num_edge = 0;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		num_edge = num_edge 
				+ count_isoline_to_buf(0.002, v_line, psf_m->icomp_draw_psf, psf_s);
	};
	return num_edge;
}


static int set_PSF_zeroline_to_buf(int ist_edge, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
	int iedge_buf = ist_edge;
	iedge_buf = set_isoline_to_buf(iedge_buf, 0.005, ZERO, psf_m->icomp_draw_psf, black,
							 psf_s, psf_buf);
	
	return iedge_buf;
}

static int set_PSF_isolines_to_buf(int ist_edge, int ist, int ied, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *psf_buf){
	int iedge_buf;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	iedge_buf = ist_edge;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		
		if (psf_m->isoline_color == RAINBOW_LINE){
			set_rainbow_color_code(psf_m->cmap_psf, v_line, f_color);
		};
		iedge_buf = set_isoline_to_buf(iedge_buf, 0.002, v_line, psf_m->icomp_draw_psf, f_color,
								 psf_s, psf_buf);
	};
	
	return iedge_buf;
}


int count_PSF_all_isolines_to_buf(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int ierr;
	int num_patch = 0;
	int num_edge = 0;
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			num_edge = num_edge + count_PSF_isolines(IZERO, psf_m->ist_positive_line,
						psf_s, psf_m);
		};
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            num_edge = num_edge + count_PSF_isolines(psf_m->ist_positive_line, psf_m->n_isoline,
						psf_s, psf_m);
        };
    };
	if(psf_m->draw_psf_zero != 0){
        num_edge = num_edge + count_PSF_zeroline(psf_s, psf_m);
    };
	num_patch = 2*num_edge;
	return num_patch;
}

int set_PSF_all_isolines_to_buf(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
    int ierr;
	
	int iedge_buf = 0;
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			iedge_buf = set_PSF_isolines_to_buf(iedge_buf, 
							IZERO, psf_m->ist_positive_line,
							psf_s, psf_m, psf_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			iedge_buf = set_PSF_isolines_to_buf(iedge_buf, 
							psf_m->ist_positive_line, psf_m->n_isoline,
							psf_s, psf_m, psf_buf);
        };
    };
	if(psf_m->draw_psf_zero  != 0){
		iedge_buf = set_PSF_zeroline_to_buf(iedge_buf, psf_s, psf_m, psf_buf);
	};
	
	return iedge_buf;
}
