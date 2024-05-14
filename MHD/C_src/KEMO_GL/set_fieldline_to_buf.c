
/* set_fieldline_to_buf.c */

#include "set_fieldline_to_buf.h"


long count_fieldtubes_to_buf(int ncorner, struct fline_data *fline_d){
    long num_patch = 2 * fline_d->nedge_fline * ncorner;
	return num_patch;
};
long count_fieldlines_to_buf(struct fline_data *fline_d){
	return fline_d->nedge_fline;
}

long set_fieldtubes_to_buf(long ist_patch, long ist_line, long ied_line,
                           struct fline_data *fline_d,
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long k;
	int num_wall;
	int nd;
    long iele, inod;
    double xyzw[4*6 * fline_m->ncorner];
    double norm[4*6 * fline_m->ncorner];
    double col[ 4*6 * fline_m->ncorner];
	double x_line[8], dir_line[8], color_line[8];
	
	set_color_code_for_fieldlines(fline_d, fline_m);
	
	long inum_tube = ist_patch;
	for (iele=ist_line; iele<ied_line; iele++) {
		for (k = 0; k < 2; k++) {
			inod = fline_d->iedge_fline[iele][k] - 1;
			for (nd=0; nd<3; nd++) {
				x_line[4*k+nd] =   (float) fline_d->xyzw_fline[4*inod + nd];
				dir_line[4*k+nd] = (float) fline_d->dir_nod[4*inod + nd];
			};
			for (nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_d->color_nod[4*inod+nd];};
		};
        inum_tube = set_tube_strided_buffer(inum_tube, 
                                            fline_m->ncorner, fline_m->fieldline_thick,
                                            x_line, dir_line, color_line, strided_buf);
	};
	return inum_tube;
};

long set_fieldlines_to_buf(long ist_patch, long ist_line, long ied_line,
                           struct fline_data *fline_d,
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	long iele, k, nd, inod;
	
	set_color_code_for_fieldlines(fline_d, fline_m);
	
	for(iele=ist_line; iele<ied_line; iele++){
		for(k=0;k<ITWO;k++){
			inod = fline_d->iedge_fline[iele][k] - 1;
            set_node_stride_buffer((ITWO*iele+k), strided_buf, &point_buf);
			for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf.igl_xyzw]
                    = fline_d->xyzw_fline[4*inod + nd];
            };
			for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf.igl_color]
                    = fline_d->color_nod[4*inod + nd];
            };
		};
	};
	
	return fline_d->nedge_fline;
}
