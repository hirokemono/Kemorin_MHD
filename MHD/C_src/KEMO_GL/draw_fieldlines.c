
/* draw_fieldlines.c */

#include "draw_fieldlines.h"

void set_fieldtubes_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct VAO_ids *fline_VAO, struct gl_strided_buffer *fline_buf){
	int ncorner = ISIX;
	int icou;
	
	int num_patch = count_fieldtubes_to_buf(ncorner, fline_s);
	fline_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	set_buffer_address_4_patch(ITHREE*num_patch, fline_buf);
	resize_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	icou = set_fieldtubes_to_buf(ncorner, fline_s, fline_m, fline_buf);
	
	Const_VAO_4_Phong(fline_VAO, fline_buf);	
	return;
}

void set_fieldlines_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct VAO_ids *fline_VAO, struct gl_strided_buffer *fline_buf){
	int icou;
	
	int num_edge = count_fieldlines_to_buf(fline_s);
	fline_VAO->npoint_draw = ITWO * num_edge;
	if(num_edge <= 0) return;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	set_buffer_address_4_patch(ITWO*num_edge, fline_buf);
	resize_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	icou = set_fieldlines_to_buf(fline_s, fline_m, fline_buf);
	
	Const_VAO_4_Simple(fline_VAO, fline_buf);
	return;
}


void sel_fieldlines_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct VAO_ids **fline_VAO){
	fline_VAO[0]->npoint_draw = 0;
	fline_VAO[1]->npoint_draw = 0;
	if(fline_m->iflag_draw_fline <= 0) return;
	
	struct gl_strided_buffer *fline_buf 
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, fline_buf);
	alloc_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	if(fline_m->fieldline_type == IFLAG_PIPE){
		set_fieldtubes_VAO(fline_s, fline_m, fline_VAO[0], fline_buf);
	};
	set_fieldlines_VAO(fline_s, fline_m, fline_VAO[1], fline_buf);
	
	free(fline_buf->v_buf);
	free(fline_buf);
	return;
};

