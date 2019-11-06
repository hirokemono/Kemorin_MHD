
/* m_kemoview_psf.h */

#ifndef M_KEMOVIEWER_PSF_
#define M_KEMOVIEWER_PSF_


#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "numbers_to_bin_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "read_data_4_kemoviewer.h"
#include "psf_data_array_manager.h"
#include "set_each_psf_parameters.h"

struct kemoview_psf{
    struct kemo_array_control   *psf_a;
    struct psf_data            **psf_d;
    struct psf_menu_val        **psf_m;
};

/* prototypes */ 
struct kemoview_psf * init_kemoview_psf(void);
void dealloc_kemoview_psf(struct kemoview_psf *kemo_psf);

void init_draw_psf(struct kemoview_psf *kemo_psf, struct psf_data *ucd_tmp,
			int iflag_fileformat, int istep, const char *ucd_header);
void close_PSF_view(struct kemoview_psf *kemo_psf);

void evolution_psf_viewer(struct psf_data *psf_ucd_tmp, struct kemoview_psf *kemo_psf);

void set_PSF_loaded_params(int selected, int input, struct kemoview_psf *kemo_psf);
int get_PSF_loaded_params(struct kemoview_psf *kemo_psf, int selected);

void set_each_PSF_field_param(int selected, int input, struct kemoview_psf *kemo_psf);
int get_each_PSF_field_param(int selected, struct kemoview_psf *kemo_psf);

int toggle_each_PSF_draw_switch(int selected, struct kemoview_psf *kemo_psf);
int get_each_PSF_draw_switch(int selected, struct kemoview_psf *kemo_psf);

void set_each_PSF_color_param(int selected, int input, struct kemoview_psf *kemo_psf);
int get_each_PSF_color_param(int selected, struct kemoview_psf *kemo_psf);

void set_each_PSF_color_w_exp(int selected, double value, int i_digit, 
							  struct kemoview_psf *kemo_psf);
void get_each_PSF_color_w_exp(int selected, struct kemoview_psf *kemo_psf,
							  double *value, int *i_digit);

double get_each_PSF_data_range(int selected, int icomp, struct kemoview_psf *kemo_psf);
double get_each_PSF_colormap_range(int selected, struct kemoview_psf *kemo_psf);
#endif
