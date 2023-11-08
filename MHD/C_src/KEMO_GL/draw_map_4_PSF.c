
/* draw_map_4_PSF.c */

#include "draw_map_4_PSF.h"

void set_map_patch_buffer(int ist_psf, int ied_psf, struct psf_data **psf_s,
                          struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *map_buf){
    set_color_code_for_psfs(psf_s, psf_m, psf_a);

    int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
    if(num_patch > 0){
        set_buffer_address_4_patch((ITHREE * num_patch), map_buf);
        alloc_strided_buffer(map_buf);
        set_psf_map_to_buf(ist_psf, ied_psf, psf_s, psf_a, map_buf);
    } else {
        map_buf->num_nod_buf = 0;
    }
	return;
}

void set_map_PSF_isolines_buffer(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a, struct view_element *view_s,
                                 struct gl_strided_buffer *mline_buf){
	double ref_width = 1.5;
	int i, iflag;
	int inum_patch;
	int num_patch = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
		if(iflag > 0){
			num_patch = count_map_PSF_isoline(num_patch, psf_s[i], psf_m[i]);
		};
	};
    
    if(num_patch > 0){
        set_buffer_address_4_patch((ITHREE * num_patch), mline_buf);
        alloc_strided_buffer(mline_buf);
        
        inum_patch = 0;
        for(i=0; i<psf_a->nmax_loaded; i++){
            iflag = psf_a->iflag_loaded[i]
            * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
            if(iflag > 0){
                if(psf_m[i]->isoline_width <= 0.0){
                    psf_m[i]->isoline_width = set_tube_radius_by_view(view_s, ref_width);
                };
                inum_patch = set_map_PSF_isoline_to_buf(inum_patch, psf_s[i],
                                                        psf_m[i], mline_buf);
            };
        };
    } else {
        mline_buf->num_nod_buf = 0;
    }
    return;
}

int check_draw_map(struct kemo_array_control *psf_a){
	int i;
    int iflag_map = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_map = iflag_map + psf_a->iflag_loaded[i];
	};
	return iflag_map;
};

void set_map_objects_VAO(struct view_element *view_s, 
						 struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
						 struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
						 struct VAO_ids **map_VAO){
	struct gl_strided_buffer *map_buf
				= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_map_patch_buffer(IZERO, psf_a->istack_solid_psf_patch,
                         psf_s, psf_m, psf_a, map_buf);
    map_VAO[0]->npoint_draw = map_buf->num_nod_buf;
    if(map_VAO[0]->npoint_draw > 0){
        Const_VAO_4_Simple(map_VAO[0], map_buf);
        free(map_buf->v_buf);
    };
    free(map_buf);

    struct gl_strided_buffer *mline_buf
                = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_map_PSF_isolines_buffer(psf_s, psf_m, psf_a, view_s, mline_buf);
    map_VAO[1]->npoint_draw = mline_buf->num_nod_buf;
    if(map_VAO[1]->npoint_draw > 0){
        Const_VAO_4_Simple(map_VAO[1], mline_buf);
        free(mline_buf->v_buf);
    };
    free(mline_buf);
	return;
};
