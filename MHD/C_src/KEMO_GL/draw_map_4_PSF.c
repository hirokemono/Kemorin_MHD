
/* draw_map_4_PSF.c */

#include "draw_map_4_PSF.h"

int check_draw_map(struct kemo_array_control *psf_a){
    int i;
    int iflag_map = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag_map = iflag_map + psf_a->iflag_loaded[i];
    };
    return iflag_map;
};

void set_map_patch_buffer(long ist_psf, long ied_psf, struct psf_data **psf_s,
                          struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *map_buf,
                          struct gl_local_buffer_address *point_buf){
    set_color_code_for_psfs(psf_s, psf_m, psf_a);

    long num_patch =  count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch((ITHREE * num_patch), map_buf);
    if(map_buf->num_nod_buf > 0){
        resize_strided_buffer(map_buf);
        set_psf_map_to_buf(ist_psf, ied_psf, psf_s, psf_a,
                           map_buf, point_buf);
    }
	return;
}

void set_map_PSF_isolines_buffer(const int nthreads,
                                 struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a, struct view_element *view_s,
                                 struct gl_strided_buffer *mline_buf,
                                 struct gl_local_buffer_address *point_buf){
    long *istack_smp_map_iso_n = (long *) calloc(nthreads+1, sizeof(long));
    if(istack_smp_map_iso_n == NULL) {
        printf("malloc error for istack_smp_map_iso_n\n");
        exit(0);
    }
    long *istack_smp_map_iso_p = (long *) calloc(nthreads+1, sizeof(long));
    if(istack_smp_map_iso_p == NULL) {
        printf("malloc error for istack_smp_map_iso_p\n");
        exit(0);
    }
    long *istack_smp_map_iso_0 = (long *) calloc(nthreads+1, sizeof(long));
    if(istack_smp_map_iso_0 == NULL) {
        printf("malloc error for istack_smp_map_iso_0\n");
        exit(0);
    }

	double ref_width = 1.5;
	int i, iflag;
	long num_patch = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
		if(iflag > 0){
			num_patch = count_map_PSF_isoline(num_patch, nthreads, psf_s[i], psf_m[i],
                                              istack_smp_map_iso_n, istack_smp_map_iso_p,
                                              istack_smp_map_iso_0);
		};
	};
    
    set_buffer_address_4_patch((ITHREE * num_patch), mline_buf);
    long inum_patch;
    if(mline_buf->num_nod_buf > 0){
        resize_strided_buffer(mline_buf);
        
        inum_patch = 0;
        for(i=0; i<psf_a->nmax_loaded; i++){
            iflag = psf_a->iflag_loaded[i]
            * (psf_m[i]->draw_psf_grid + psf_m[i]->draw_psf_zero);
            if(iflag > 0){
                if(psf_m[i]->isoline_width <= 0.0){
                    psf_m[i]->isoline_width = set_tube_radius_by_view(view_s, ref_width);
                };
                inum_patch = set_map_PSF_isoline_to_buf(inum_patch, psf_s[i], psf_m[i],
                                                        mline_buf, point_buf);
            };
        };
    }
    free(istack_smp_map_iso_n);
    free(istack_smp_map_iso_p);
    free(istack_smp_map_iso_0);
    return;
}
