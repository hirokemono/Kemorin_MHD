!> @file  kemorin_MHD_C_linkage.f03
!!      module kemorin_MHD_C_linkage
!!
!! @author  H. Matsui
!! @date Programmed in May, 2017
!
!> @brief Linkage to C routines by using Fotran 2003 C binding
!!
      module kemorin_MHD_C_linkage
!
      use iso_c_binding
      use m_precision
!
! -----------------------------------------------------------------------
!
      interface
!
! -----------------------------------------------------------------------
!
!void open_wt_rawfile(const char *file_name, int *ierr);
!void open_ad_rawfile(const char *file_name, int *ierr);
!void open_rd_rawfile(const char *file_name, int *ierr);
!void close_rawfile();
!
!void rawseek_go_fwd_f(int *ioffset, int *ierr);
!void rawread_f(int *iflag_swap, int *ilength, char *textbuf, int *lenchara);
!void rawwrite_f(int *ilength, char *textbuf, int *lenchara);
!
!void open_wt_gzfile(const char *gz_file_name);
!void open_ad_gzfile(const char *gz_file_name);
!void open_rd_gzfile(const char *gz_file_name);
!void close_gzfile();
!
!int check_gzfile_eof();
!
!void write_compress_txt(int *num_buffer, char *input_txt);
!void write_compress_txt_nolf(int *num_buffer, char *input_txt);
!
!void gzseek_go_fwd_f(int *ioffset, int *ierr);
!void gzread_f(int *iflag_swap, int *ilength, char *textbuf, int *ierr);
!void gzwrite_f(int *ilength, char *textbuf, int *ierr);
!
!void get_one_line_from_gz(int *num_buffer, int *num_word, int *nchara, char *line_buf);
!
!void gzip_defleat_once(int *len_buf, const char *buf, int *len_gzipbuf, 
!                       int *len_gzipped, char *gzipbuf);
!void gzip_defleat_begin(int *len_buf, const char *buf, int *len_gzipbuf, 
!                        int *len_gzipped, char *gzipbuf);
!void gzip_defleat_cont(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);
!void gzip_defleat_last(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);
!
!void gzip_infleat_once(int *len_gzipbuf, const char *gzipbuf, int *len_buf, 
!                       char *buf, int *len_gzipped);
!void gzip_infleat_begin(int *len_gzipbuf, const char *gzipbuf, int *len_buf, 
!                        char *buf, int *len_gzipped);
!void gzip_infleat_cont(int *len_gzipbuf, int *len_buf, const char *buf, int *len_gzipped);
!void gzip_infleat_last(int *len_gzipbuf, int *len_buf, const char *buf, int *len_gzipped);
!
!
!void compress_file(const char *txt_file_name, const char *gz_file_name);
!void decompress_file(const char *gz_file_name, const char *txt_file_name);
!!
!
!void write_png_rgba_c(const char *fhead, int *num_x, int *num_y, 
!			const unsigned char *cimage);
!void write_png_rgb_c(const char *fhead, int *num_x, int *num_y, 
!			const unsigned char *cimage);

!
!!
!void kemo_fftw_plan_dft_r2c_1d(fftw_plan *plan, int *n_size,
!			double *dble_in, fftw_complex *cplx_out, unsigned *flags);
!void kemo_fftw_plan_dft_c2r_1d(fftw_plan *plan, int *n_size, 
!			fftw_complex *cplx_in, double *dble_out, unsigned *flags);
!
!void kemo_fftw_plan_many_dft_r2c(fftw_plan *plan, int rank,
!                                 int *n_size, int howmany,
!                                 double *dble_in, const int *inembed,
!                                 int istride, int idist,
!                                 fftw_complex *cplx_out, int *onembed,
!                                 int ostride, int odist,
!                                 unsigned *flags);
!void kemo_fftw_plan_many_dft_c2r(fftw_plan *plan, int rank, 
!                                 int *n_size, int howmany,
!                                 fftw_complex *cplx_in, const int *inembed,
!                                 int istride, int idist,
!                                 double *dble_out, int *onembed,
!                                 int ostride, int odist,
!                                 unsigned *flags);
!
!void kemo_fftw_destroy_plan(fftw_plan *plan);
!void kemo_fftw_cleanup();
!
!void kemo_fftw_execute(fftw_plan *plan);
!void kemo_fftw_execute_dft_r2c(fftw_plan *plan, double *dble_in, fftw_complex *cplx_out);
!void kemo_fftw_execute_dft_c2r(fftw_plan *plan, fftw_complex *cplx_in, !double *dble_out);
!
!
!
!    void allocate_kemoviewer_work();
!    void allocate_kemoviwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh);
!    void allocate_single_kemoviwewer_struct(int iflag_dmesh);
!    void allocate_kemoviwewer_pointers();
!    void deallocate_kemoviwewer_pointers();
!    
!    int send_nlimit_load_psf();
!    int add_kemoview_array();
!    void close_kemoview_array();
!    
!    void set_num_loaded_kemoview(int num);
!    void set_max_loaded_kemoview(int num);
!    void set_to_loaded_kemoview_flag(int id_psf, int iflag);
!    void set_current_kemoview(int id_psf);
!    
!    int send_nlimit_load_kemoview();
!    int send_num_loaded_kemoview();
!    int send_max_loaded_kemoview();
!    int send_loaded_kemoview_flag(int id_psf);
!    int send_current_kemoview();
!    
!    int send_iflag_current_kemoview();
!    
!    void draw_kemoviewer_c();
!    void draw_kemoviewer_to_ps();
!    void kemoviewer_initial_lighting();
!    
!    void init_kemoview_background_color();
!    void set_kemoview_background_color(GLfloat color[4]);
!    void send_background_color(GLfloat color[4]);
!    
!    void evolution_viewer(int istep);
!    
!    void write_modelview_file_glut(const char *file_name);
!    void load_modelview_file_glut(const char *file_name);
!    int kemoview_open_data_glut(const char *file_name);
!    
!    void close_mesh_view();
!    int  close_psf_view();
!    void close_fline_view();
!    
!    void set_to_pick_surface_command(const char *command);
!    void send_pick_surface_command(char *command);
!    
!    void set_viewtype_glut(int sel);
!    
!    
!    void draw_modified_object_distance();
!    
!    void set_to_coastline_radius(double radius);
!    double send_coastline_radius();
!    
!    void set_object_property_flags(int selected, int iflag);
!    int send_object_property_flags(int selected);
!    int object_properties_toggle(int selected);
!    
!    void set_to_mesh_color_mode(int icolor);
!    void set_to_num_of_color_loop(int icolor);
!    
!    void set_to_node_diam(double diam);
!    void set_to_dist_domains(double dist);
!    
!    
!    void set_to_domain_surface_opacity(double opacity_in);
!    void set_to_ele_surface_opacity(double opacity_in);
!    void set_to_surf_surface_opacity(double opacity_in);
!    
!    void set_domain_color_flag(int selected, int icolor);
!    void set_node_grp_color_flag(int icolor);
!    void set_ele_grp_color_flag(int selected, int icolor);
!    void set_surf_grp_color_flag(int selected, int icolor);
!    
!    void set_domain_color_code(int selected, GLfloat color_code4[4]);
!    void set_to_node_grp_color_code(GLfloat color_code4[4]);
!    void set_ele_grp_color_code(int selected, GLfloat color_code4[4]);
!    void set_surf_grp_color_code(int selected, GLfloat color_code4[4]);
!    
!    double send_domain_surface_opacity();
!    double send_ele_surface_opacity();
!    double send_surf_surface_opacity();
!    
!    
!    int send_draw_surface_nod();
!    int send_draw_surface_grid();
!    int send_draw_surface_solid();
!    
!    void set_kemoview_mesh_draw(int selected, int iflag);
!    void kemoview_mesh_draw_toggle(int selected);
!    
!    void set_to_draw_domain_nod(int iflag, int i);
!    void set_to_draw_domain_grid(int iflag, int i);
!    void set_to_draw_domain_solid(int iflag, int i);
!    
!    void set_to_draw_nodgrp_nod(int iflag, int i);
!    int send_draw_nodgrp_nod(int i);
!    
!    void set_to_draw_elegrp_grid(int iflag, int i);
!    int send_draw_elegrp_grid(int i);
!    
!    void set_to_draw_elegrp_nod(int iflag, int i);
!    int send_draw_elegrp_nod(int i);
!    
!    void set_to_draw_elegrp_solid(int iflag, int i);
!    int send_draw_elegrp_solid(int i);
!    
!	
!	
!    void set_to_draw_surfgrp_nod(int iflag, int i);
!    int send_draw_surfgrp_nod(int i);
!    
!    void set_to_draw_surfgrp_grid(int iflag, int i);
!    int send_draw_surfgrp_grid(int i);
!    
!    void set_to_draw_surfgrp_solid(int iflag, int i);
!    int send_draw_surfgrp_solid(int i);
!    
!    void kemoview_nod_grp_toggle(int selected);
!    
!    void kemoview_ele_grp_toggle(int selected);
!    void kemoview_ele_grp_nod_toggle(int selected);
!    void kemoview_ele_grp_grid_toggle(int selected);
!    
!    void kemoview_surf_grp_toggle(int selected);
!    void kemoview_surf_grp_nod_toggle(int selected);
!    void kemoview_surf_grp_grid_toggle(int selected);
!    
!    int send_iflag_draw_mesh();
!    
!    int send_num_pe_sf();
!    int send_ngrp_nod_sf();
!    int send_ngrp_ele_sf();
!    int send_ngrp_surf_sf();
!    
!    void send_nod_gp_name_sf(char *name, int i);
!    void send_ele_gp_name_sf(char *name, int i);
!    void send_surf_gp_name_sf(char *name, int i);
!    
!    
!    int send_iflag_draw_type();
!    int send_iflag_view_type();
!    
!    int send_num_of_color_loop();
!    
!    
!    double send_node_diam();
!    double send_dist_domains();
!    
!    
!    void get_ext_from_file_name(const char *file_head, char *stripped_fhead, char *stripped_ext);
!    void add_ext_to_file_name(const char *file_head, const char *added_ext, char *file_name);
!    
!    
!    void set_to_text_color_code(float c_code[4]);
!    void send_text_color_code(float c_code[4]);
!    
!    int set_image_file_format_id(char *image_fmt);
!    void get_kemoviewer_fliped_img(int npixel_x, int npixel_y,
!                                   unsigned char *glimage, unsigned char *fliped_img);
!    void write_kemoviewer_window_to_file(int iflag_img, const char *fhead);
!    void write_kemoviewer_window_step_file(int iflag_img, int istep, const char *fhead);
!    
!    void modify_view_kemoview();
!    void rotate_kemoview();
!    
!    void reset_kemoviewer_to_init_angle();
!    
!    void set_kemoview_retinamode(int i_retina);
!    void set_kemoview_windowsize(GLint npixel_x, GLint npixel_y);
!    void update_projection_by_kemoviewer_size(GLint npixel_x, GLint npixel_y);
!    
!    void update_kemoviewer_distance();
!    
!    void set_kemoview_rotation_parameter(GLdouble rot_vect[4]);
!    void set_kemoview_dragging_rotation(GLdouble rot_vect[4]);
!    void set_kemoview_animation_rot_axis(int iaxis);
!    void set_kemoview_animation_rot_angle(int int_degree);
!    void set_kemoview_shift_vector(GLdouble position[3]);
!    void set_kemoview_scale_factor(GLdouble scale_s);
!    void set_kemoview_projection_aperture(GLdouble aperture_s);
!    void set_kemoview_stereo_parameter(GLdouble focus, GLdouble eye_sep);
!    
!    void send_kemoview_windowsize(GLint *npixel_x, GLint *npixel_y);
!    void send_kemoview_rotation_parameter(GLdouble rot_vect[4]);
!    void send_kemoview_shift_vector(GLdouble position[3]);
!    void send_kemoview_lookat_vector(GLdouble position[3]);
!    GLdouble send_kemoview_scale_factor();
!    GLdouble send_kemoview_projection_aperture();
!    void send_kemoview_projection_parameters(GLdouble *aperture_s, GLdouble *near_s,
!                                             GLdouble *far_s, GLdouble *aspect_s);
!    GLdouble send_kemoview_stereo_parameters();
!    GLdouble send_kemoview_stereo_eyeseparation();
!    
!    void kemoviewer_mousedolly(GLdouble start[2], GLdouble x_dolly, GLdouble y_dolly);
!    void kemoviewer_mousepan(GLdouble start[2], GLdouble x_pan, GLdouble y_pan);
!    void kemoviewer_zooming(GLdouble wheelDelta);
!    
!    void kemoview_startTrackball(GLdouble x, GLdouble y);
!    /* calculated rotation based on current mouse position */
!    void kemoview_rollToTrackball(GLdouble x, GLdouble y);
!    /* add a GL rotation (dA) to an existing GL rotation (A) */
!    void drugging_addToRotationTrackball();
!    void add_kemoview_animation_rot(GLdouble dt);
!    void reset_kemoviewer_animation();
!    
!    void set_to_stereo_shutter(int iflag);
!    void set_to_iflag_anaglyph(int iflag);
!    int send_stereo_shutter();
!    int send_iflag_anaglyph();
!    
!    void draw_menubottun_glut();
!    
!    /* subroutines for surafces */
!    void set_num_loaded_PSF(int num);
!    void set_max_loaded_PSF(int num);
!    void set_to_loaded_PSF_flag(int id_psf, int iflag);
!    void set_to_current_PSF(int id_psf);
!    int send_num_loaded_PSF();
!    int send_max_loaded_PSF();
!    int send_loaded_PSF_flag(int id_psf);
!    int send_current_PSF();
!    
!    int send_current_psf_full_path_header(char *file_head, int *iflag);
!    void send_current_psf_file_header(char *file_head);
!    
!    void set_current_psf_field_flag(int sel);
!    void set_current_psf_component_flag(int sel);
!    
!    int send_nfield_current_psf();
!    int send_ncomptot_current_psf();
!    int send_ncomp_current_psf(int i);
!    int send_istack_comp_current_psf(int i);
!    void send_current_psf_data_name(char *name, int i);
!    
!    int send_iflag_draw_current_psf();
!    
!    int send_draw_field_current_psf();
!    int send_draw_comp_id_current_psf();
!    int send_draw_component_current_psf();
!    int send_coordinate_id_current_psf();
!    
!    void set_texture_current_psf(int img_fmt, const char *img_head);
!    
!    void set_current_psf_polygon_mode(int iflag);
!    void set_current_psf_tanvec_mode(int iflag);
!    
!    int send_draw_current_psf_refv();
!    int toggle_draw_current_psf_refv();
!    
!    void set_current_psf_patch_color_mode(int iflag);
!    void set_current_isoline_color(int iflag);
!    void set_current_n_isoline(int nlline);
!    void set_current_increment_vect(int increment);
!    void set_current_scale_vect(double scale);
!    void set_current_vector_thick(double size);
!    
!    int send_current_psf_patch_color();
!    int send_current_isoline_color();
!    int send_current_num_isoline();
!    int send_current_vector_patch_color();
!    int send_current_increment_vect();
!    double send_current_scale_vect();
!    double send_current_vector_thick();
!    
!    int send_kemoview_psf_draw_flags(int selected);
!    int kemoview_psf_draw_switch_select(int selected);
!    
!    void set_current_PSF_color_mode_id(int isel);
!    
!    double send_current_psf_data_min(int i);
!    double send_current_psf_data_max(int i);
!    
!	void delete_current_PSF_color_idx_list(int i_delete);
!	void delete_current_PSF_opacity_idx_list(int i_delete);
!	void add_current_PSF_color_idx_list(double add_value, double add_color);
!	void add_current_PSF_opacity_idx_list(double add_value, double add_opacity);
!	
!    void set_current_PSF_linear_colormap(double minvalue, double maxvalue);
!    void set_current_PSF_fixed_color(double *rgba);
!    void set_current_PSF_constant_opacity(double opacity);
!    
!    void set_current_PSF_rgb_from_value(double value, double *red, double *green, double *blue);
!    void set_current_PSF_opacity_from_value(double value, double *opacity);
!    void set_current_PSF_color_point(int i_point, double value, double color);
!    void set_current_PSF_opacity_point(int i_point, double value, double opacity);
!    
!    double send_current_PSF_color_table_min();
!    double send_current_PSF_color_table_max();
!    double send_current_PSF_minimum_opacity();
!    double send_current_PSF_maximum_opacity();
!    int send_current_PSF_color_table_num();
!    int send_current_PSF_opacity_table_num();
!    
!    void send_current_PSF_color_table_items(int i_point, double *value, double *color);
!    void send_current_PSF_opacity_table_items(int i_point, double *value, double *opacity);
!    
!    void write_current_PSF_colormap_control_file(const char *file_name);
!	void check_current_PSF_colormap_control();
!    
!    
!    /* Subroutines for field lines */
!    
!    int send_fline_file_header(char *file_head);
!    void set_to_fline_file_step(int istep);
!    
!    void set_fline_color_field_flag(int sel);
!    void set_fline_color_comp_flag(int sel);    
!    
!    int send_nfield_fline();
!    int send_ncomptot_fline();
!    int send_ncomp_fline(int i);
!    int send_istack_comp_fline(int i);
!    void send_fline_data_name(char *name, int i);
!    
!    
!    void set_to_draw_fline(int iflag);
!    void set_to_if_draw_fline(int ifield);
!    void set_to_ic_draw_fline(int icomp);
!    void set_to_fieldline_color(int iflag);
!    
!    int send_iflag_draw_fline();
!    int send_if_draw_fline();
!    int send_ic_draw_fline();
!    int send_icomp_draw_fline();
!    int send_fieldline_color();
!    
!	void set_to_fline_type_flag(int iflag);
!	int send_fline_type_flag();
!	int toggle_fline_type_flag();
!	
!	void set_to_fline_thickness(double thick);
!	double send_fline_thickness();
!    
!    double send_fline_data_min(int i);
!    double send_fline_data_max(int i);
!    
!    void input_fline_linear_colormap(double minvalue, double maxvalue);
!    void set_fline_constant_opacitymap(double opacity);
!    
!    void realloc_fline_color_index_list(int id_cmode, int num);
!    void realloc_flie_opacity_index_list(int num);
!    
!    void set_fline_rgb_from_value(double value, double *red, double *green, double *blue);
!    void set_fline_opacity_from_value(double value, double *opacity);
!    void set_each_fline_color_point(int i_point, double value, double color);
!    void set_each_fline_opacity_point(int i_point, double value, double opacity);
!    
!    void set_fline_color_mode_id(int isel);
!    
!    double send_fline_color_table_min();
!    double send_fline_color_table_max();
!    double send_fline_minimum_opacity();
        function send_fline_minimum_opacity()                           &
     &          BIND(C, name = "send_fline_minimum_opacity")
!          IMPORT C_DOUBLE, c_int
          integer(c_int) :: send_fline_minimum_opacity
        END function
!    
!    double send_fline_maximum_opacity();
        function send_fline_maximum_opacity()                           &
     &          BIND(C, name = "send_fline_maximum_opacity")
!          IMPORT C_DOUBLE, c_int
          integer(c_int) :: send_fline_maximum_opacity
        END function
!    
!    int send_fline_color_table_num();
        function send_fline_color_table_num()                           &
     &          BIND(C, name = "send_fline_color_table_num")
!          IMPORT c_int
          integer(c_int) :: send_fline_color_table_num
        END function
!
!    int send_fline_opacity_table_num();
        function send_fline_opacity_table_num()                         &
     &          BIND(C, name = "send_fline_opacity_table_num")
!          IMPORT c_int
          integer(c_int) :: send_fline_opacity_table_num
        END function
!
!    void send_fline_color_table_items(int i_point, double *value, double *color);
        subroutine send_fline_color_table_items                         &
     &           (i_point, value, color)                                &
     &            BIND(C, name = "send_fline_color_table_items")
!          IMPORT C_DOUBLE, c_int
          integer(c_int), VALUE :: i_point
          REAL(C_DOUBLE), intent(inout) :: value(i_point)
          REAL(C_DOUBLE), intent(inout) :: color(i_point)
        END subroutine
!
!    void send_fline_opacity_table_items(int i_point, double *value, double *opacity);
        subroutine send_fline_opacity_table_items                       &
     &           (i_point, value, opacity)                              &
     &            BIND(C, name = "send_fline_opacity_table_items")
!          IMPORT C_DOUBLE, c_int
          integer(c_int), VALUE :: i_point
          REAL(C_DOUBLE), intent(inout) :: value(i_point)
          REAL(C_DOUBLE), intent(inout) :: opacity(i_point)
        END subroutine
!
!    void write_fline_colormap_control_file(const char *file_name);
        subroutine write_fline_colormap_control_file(file_name)         &
     &            BIND(C, name = "write_fline_colormap_control_file")
!          IMPORT C_DOUBLE, C_CHAR
          character(kind=C_CHAR, LEN=kchara) :: file_name
          REAL(C_DOUBLE) :: write_fline_colormap_control_file
        END subroutine
!    
!    double round_to_3digit(double value);
        function round_to_3digit(value)                                 &
     &          BIND(C, name = "round_to_3digit")
!          IMPORT C_DOUBLE
          REAL(C_DOUBLE), VALUE :: value
          REAL(C_DOUBLE) :: round_to_3digit
        END function
!
      end interface
!
! -----------------------------------------------------------------------
!
      end module kemorin_MHD_C_linkage
