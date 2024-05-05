
/* set_fieldline_to_buf.c */

#include "set_fieldline_to_buf.h"

long count_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s){
    long num_patch = 2 * fline_s->nele_viz * ncorner; 
	return num_patch;
};
long count_fieldlines_to_buf(struct psf_data *fline_s){
	return fline_s->nele_viz;
}

long set_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s, 
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf) {
    long inum_patch, k;
	int num_wall;
	int iele, nd;
    long inod;
	double xyz[9*2*ncorner], nor[9*2*ncorner], col[12*2*ncorner];
	double x_line[6], dir_line[6], color_line[8];
	double norm_line[6];
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	inum_patch = 0;
	for (iele = 0; iele < fline_s->nele_viz; iele++) {
		for (k = 0; k < 2; k++) {
			inod = fline_s->ie_viz[iele][k] - 1;
			for (nd=0; nd<3; nd++) {
				x_line[3*k+nd] =   (float) fline_s->xyzw_viz[inod*IFOUR + nd];
				dir_line[3*k+nd] = (float) fline_s->dir_nod[inod*IFOUR + nd];
			};
			for (nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_s->color_nod[4*inod+nd];};
		};
		find_normal_of_line(norm_line, x_line, dir_line);
		num_wall = set_tube_vertex(ncorner, fline_m->fieldline_thick, 
								   x_line, dir_line, norm_line, color_line,
								   xyz, nor, col);
		
		for (k=0; k<3*num_wall; k++) {
            set_node_stride_buffer((ITHREE*inum_patch+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xyz[3*k+nd];};
			for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = nor[3*k+nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = col[4*k+nd];};
		};
		inum_patch = inum_patch + num_wall; 
	};
	return inum_patch;
};


long set_fieldlines_to_buf(struct psf_data *fline_s, struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf) {
	long iele, k, nd, inod;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	for(iele=0; iele<fline_s->nele_viz; iele++){
		for(k=0;k<ITWO;k++){
			inod =fline_s->ie_viz[iele][k] - 1;
            set_node_stride_buffer((ITWO*iele+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = fline_s->xyzw_viz[inod*IFOUR + nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = fline_s->color_nod[inod*IFOUR + nd];};
		};
	};
	
	return fline_s->nele_viz;
}
