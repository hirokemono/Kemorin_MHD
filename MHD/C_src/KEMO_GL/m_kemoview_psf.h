
/* m_kemoview_psf.h */

#ifndef M_KEMOVIEWER_PSF_
#define M_KEMOVIEWER_PSF_


#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "read_data_4_kemoviewer.h"
#include "psf_data_array_manager.h"

struct kemoview_psf{
    struct kemo_array_control   *psf_a;
    struct psf_data            **psf_d;
    struct psf_menu_val        **psf_m;
};

/* prototypes */ 
struct kemoview_psf * init_kemoview_psf();
void dealloc_kemoview_psf(struct kemoview_psf *kemo_psf);

void init_draw_psf(struct kemoview_psf *kemo_psf, struct psf_data *ucd_tmp,
			int iflag_fileformat, int istep, const char *ucd_header);
void close_PSF_view(struct kemoview_psf *kemo_psf, 
			struct psf_data *psf_current_data, struct psf_menu_val*psf_current_menu);

void evolution_psf_viewer(struct psf_data *psf_ucd_tmp, struct kemoview_psf *kemo_psf);
#endif
