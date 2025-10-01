
/* set_fieldline_to_buf.c */

#include "set_fieldline_to_buf.h"


long count_fieldlines_to_buf(struct psf_data *fline_d){
	return fline_d->nele_viz;
}

long set_fieldtubes_to_buf(long ist_patch, long ist_line, long ied_line,
                           double tube_width,
                           struct psf_data *fline_d,
                           struct fline_directions *fline_dir,
                           struct psf_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_index_buffer *index_buf){
    long inod, nd;
	double x_line[8], dir_line[8], color_line[8];
    
    long iele, k;
	long inum_tube = ist_patch;
	for(iele=ist_line; iele<ied_line; iele++) {
		for(k = 0; k < 2; k++) {
			inod = fline_d->ie_viz[iele][k] - 1;
			for(nd=0; nd<3; nd++) {
				x_line[4*k+nd] =   (float) fline_d->xyzw_viz[4*inod + nd];
				dir_line[4*k+nd] = (float) fline_dir->dir_nod[4*inod + nd];
			};
			for(nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_d->color_nod[4*inod+nd];};
		};
        inum_tube = set_tube_node_index_buffer(inum_tube,
                                               fline_m->ncorner_viz_line, tube_width,
                                               x_line, dir_line, color_line,
                                               strided_buf, index_buf);
	};
	return inum_tube;
};

long set_fieldlines_to_buf(long ist_patch, long ist_line, long ied_line,
                           struct psf_data *fline_d,
                           struct psf_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf){
    double xyzw_line[8], color_line[8];
	long iele, k, nd, inod;

    long inum_line = ist_patch;
	for(iele=ist_line; iele<ied_line; iele++){
		for(k=0;k<ITWO;k++){
			inod = fline_d->ie_viz[iele][k] - 1;
			for(nd=0;nd<4;nd++){
                xyzw_line[4*k+nd] =  fline_d->xyzw_viz[4*inod + nd];
                color_line[4*k+nd] = fline_d->color_nod[4*inod + nd];
            };
		};
        inum_line = set_line_strided_buffer(inum_line, xyzw_line, color_line,
                                            strided_buf);
	};
	
	return fline_d->nele_viz;
}
