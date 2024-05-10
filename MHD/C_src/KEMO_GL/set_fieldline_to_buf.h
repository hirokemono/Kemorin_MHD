
/* set_fieldline_to_buf.h */

#ifndef SET_FIELDLINE_TO_BUF_
#define SET_FIELDLINE_TO_BUF_

#include <pthread.h>

#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "icosahedron_c.h"


typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    int ncorner;
    struct psf_data       *fline_s;
    struct fline_menu_val *fline_m;
    
    long *num_patch;
} args_pthread_fieldline;



/* prototypes */
long count_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s);
long count_fieldlines_to_buf(struct psf_data *fline_s);

long set_fieldtubes_to_buf(long ist_patch, long ist_line, long ied_line,
                           int ncorner, struct psf_data *fline_s, 
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf);
long set_fieldlines_to_buf(long ist_patch, long ist_line, long ied_line,
                           struct psf_data *fline_s,
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf);

long set_fieldtubes_to_buf_pthread(long ist_patch, const int nthreads, 
                                   int ncorner, struct psf_data *fline_s, 
                                   struct fline_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf);
long set_fieldlines_to_buf_pthread(long ist_patch, const int nthreads, 
                                   struct psf_data *fline_s, 
                                   struct fline_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf);

#endif
