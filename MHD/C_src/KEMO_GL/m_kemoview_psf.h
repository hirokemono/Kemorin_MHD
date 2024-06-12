
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
    struct psf_data      *psf_d;
    struct psf_menu_val  *psf_m;
};

struct kemoview_mul_psf{
    struct kemo_array_control   *psf_a;
    struct psf_data            **psf_d;
    struct psf_normals         **psf_n;
    struct psf_menu_val        **psf_m;
};

/* prototypes */ 
struct kemoview_mul_psf * init_kemoview_mul_psf(void);
void dealloc_kemoview_mul_psf(struct kemoview_mul_psf *kemo_mul_psf);

void init_draw_mul_psf(struct kemoview_mul_psf *kemo_mul_psf, struct psf_data *ucd_tmp,
                       int iflag_fileformat, int istep, double time, const char *ucd_header);
void close_PSF_view(struct kemoview_mul_psf *kemo_mul_psf);

void evolution_psf_viewer(struct psf_data *psf_ucd_tmp, struct kemoview_mul_psf *kemo_mul_psf);

void set_PSF_loaded_params(int selected, int input, struct kemoview_mul_psf *kemo_mul_psf);
int get_PSF_loaded_params(struct kemoview_mul_psf *kemo_mul_psf, int selected);

void get_PSF_full_path_file_name(struct kemoview_mul_psf *kemo_mul_psf,
                                 struct kv_string *ucd_m);
int get_PSF_full_path_file_prefix(struct kemoview_mul_psf *kemo_mul_psf,
                                  struct kv_string *psf_filehead, int *iflag);

void set_each_PSF_field_param(int selected, int input, struct kemoview_mul_psf *kemo_mul_psf);
int get_each_PSF_field_param(int selected, struct kemoview_mul_psf *kemo_mul_psf);

int toggle_each_PSF_draw_switch(int selected, struct kemoview_mul_psf *kemo_mul_psf);
void set_each_PSF_draw_switch(int selected, int iflag, struct kemoview_mul_psf *kemo_mul_psf);
int get_each_PSF_draw_switch(int selected, struct kemoview_mul_psf *kemo_mul_psf);

void update_PSF_textured_id(struct kemoview_mul_psf *kemo_mul_psf);
void set_each_PSF_color_param(int selected, int input, struct kemoview_mul_psf *kemo_mul_psf);
int get_each_PSF_color_param(int selected, struct kemoview_mul_psf *kemo_mul_psf);

void set_viz_colormap_param(int selected, int input,
                            struct psf_menu_val *viz_menu);
int get_viz_colormap_param(int selected,
                           struct psf_menu_val *viz_menu);



void set_each_PSF_color_w_exp(int selected, double value, int i_digit, 
							  struct kemoview_mul_psf *kemo_mul_psf);
void get_each_PSF_color_w_exp(int selected, struct kemoview_mul_psf *kemo_mul_psf,
							  double *value, int *i_digit);

double get_each_PSF_data_range(int selected, int icomp, struct kemoview_mul_psf *kemo_mul_psf);
double get_each_PSF_colormap_range(int selected, struct kemoview_mul_psf *kemo_mul_psf);

void delete_PSF_opacity_list(int i_delete, struct kemoview_mul_psf *kemo_mul_psf);

void add_PSF_opacity_list(double add_value, double add_opacity,
                          struct kemoview_mul_psf *kemo_mul_psf);
void set_PSF_linear_colormap(double minvalue, int i_min_digit,
                             double maxvalue, int i_max_digit,
                             struct kemoview_mul_psf *kemo_mul_psf);

void set_draw_time_flag(int iflag, struct kemoview_mul_psf *kemo_mul_psf);
int toggle_draw_time_flag(struct kemoview_mul_psf *kemo_mul_psf);
int get_draw_time_flag(struct kemoview_mul_psf *kemo_mul_psf);
int get_avail_time_flag(struct kemoview_mul_psf *kemo_mul_psf);

void set_draw_file_step_flag(int iflag, struct kemoview_mul_psf *kemo_mul_psf);
int toggle_draw_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf);
int get_draw_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf);
int get_avail_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf);

long send_PSF_num_component(struct kemoview_mul_psf *kemo_mul_psf, int i);
void send_PSF_field_name(struct kemoview_mul_psf *kemo_mul_psf,
                         struct kv_string *colorname, int i);
void set_PSF_polygon_mode(int iflag, struct kemoview_mul_psf *kemo_mul_psf);
void set_PSF_tangential_vec_mode(int iflag, struct kemoview_mul_psf *kemo_mul_psf);
int get_PSF_draw_refv(struct kemoview_mul_psf *kemo_mul_psf);

void set_PSF_fixed_color(double *rgba, struct kemoview_mul_psf *kemo_mul_psf);
void set_PSF_constant_opacity(double opacity,
                              struct kemoview_mul_psf *kemo_mul_psf);

void get_PSF_rgb_at_value(struct kemoview_mul_psf *kemo_mul_psf, double value,
                          double *red, double *green, double *blue);
double get_PSF_opacity_at_value(struct kemoview_mul_psf *kemo_mul_psf, double value);
void set_PSF_opacity_data(int i_point, double value, double opacity,
                          struct kemoview_mul_psf *kemoview_mul_psf);

void get_PSF_opacity_items(struct kemoview_mul_psf *kemo_mul_psf,
                           int i_point, double *value, double *opacity);

void write_PSF_colormap_file(struct kv_string *filename, int iflag_draw_axis,
                             struct kemoview_mul_psf *kemo_mul_psf);
void read_PSF_colormap_file(struct kv_string *filename,
                            struct kemoview_mul_psf *kemo_mul_psf);


int send_psf_file_dir_prefix(struct kemoview_mul_psf *kemo_mul_psf,
                             struct kv_string *stripped_dir,
                             struct kv_string *stripped_filehead);
struct colormap_params * link_active_colormap_param(struct kemoview_mul_psf *kemo_mul_psf);
#endif
