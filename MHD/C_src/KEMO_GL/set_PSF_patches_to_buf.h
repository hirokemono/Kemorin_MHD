
/* set_PSF_patches_to_buf.h */

#ifndef SET_PSF_PATCHES_TO_BUF_
#define SET_PSF_PATCHES_TO_BUF_

#include <pthread.h>

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "set_new_patch_4_map_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"
#include "icosahedron_c.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    struct psf_data     **psf_s;
    struct psf_menu_val **psf_m;
    struct kemo_array_control *psf_a;
    int shading_mode;
    
    long ist_psf;
    long ied_psf;
    long *num_patch;
} args_pthread_PSF_Patch;

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    struct psf_data     *psf_s;
    struct psf_menu_val *psf_m;
    int ncorner;
    
    long *istack_smp_arrow;
    long nnod_viz;
    long *num_patch;
} args_pthread_PSF_Arrow;

/* prptotypes */

long count_psf_nodes_to_buf(long ist_psf, long ied_psf);

long set_psf_nodes_to_buf(long ipatch_in, long ist_psf, long ied_psf, int shading_mode, 
                          struct psf_data **psf_s, struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *strided_buf);
long set_psf_textures_to_buf(long ist_texture, long ist_psf, long ied_psf,
                             struct psf_data **psf_s,
                             struct kemo_array_control *psf_a,
                             struct gl_strided_buffer *strided_buf);

long set_psf_map_to_buf(long ist_patch, long ist_psf, long ied_psf,
                        struct psf_data **psf_s,
                        struct kemo_array_control *psf_a,
                        struct gl_strided_buffer *strided_buf);

long add_num_psf_arrows(long ist_patch, long ist, long ied, int ncorner,
                        struct psf_data *psf_s, struct psf_menu_val *psf_m);
long set_psf_arrows_to_buf(long ist_patch, long ist, long ied,
                           int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m,
                           struct gl_strided_buffer *strided_buf);


long set_psf_nodes_to_buf_pthread(long ipatch_in, int nthreads,
                                  long ist_psf, long ied_psf, int shading_mode, 
                                  struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                  struct kemo_array_control *psf_a,
                                  struct gl_strided_buffer *strided_buf);
long set_psf_textures_to_buf_pthread(int nthreads, long ist_psf, long ied_psf,
                                     struct psf_data **psf_s, struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *strided_buf);
long set_psf_map_to_buf_pthread(long ipatch_in, int nthreads, long ist_psf, long ied_psf,
                                struct psf_data **psf_s, struct kemo_array_control *psf_a,
                                struct gl_strided_buffer *strided_buf);


long add_num_psf_arrows_pthread(long ist_patch, const int nthreads,
                                long *istack_arrow, int ncorner, 
                                struct psf_data *psf_s, struct psf_menu_val *psf_m);
long set_psf_arrows_to_buf_pthread(long ist_patch, const int nthreads, 
                                   long *istack_smp_arrow, int ncorner, 
                                   struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                   struct gl_strided_buffer *strided_buf);

#endif
