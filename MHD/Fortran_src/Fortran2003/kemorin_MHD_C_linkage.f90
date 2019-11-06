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
!void rawread_32bit_f(int *iflag_swap, int *ilength, char *textbuf, int *lenchara);
!void rawread_64bit_f(int *iflag_swap, int *ilength, char *textbuf, int *lenchara);
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
!void gzread_32bit_f(int *iflag_swap, int *ilength, char *textbuf, int *ierr);
!void gzread_64bit_f(int *iflag_swap, int *ilength, char *textbuf, int *ierr);
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
!    void kemoview_allocate_viwewer_struct(struct kemoviewer_type *kemoviewer_data, int iflag_dmesh);
!    void kemoview_allocate_single_viwewer_struct(struct kemoviewer_type *kemoviewer_data);
!    void kemoview_allocate_pointers();
!    void kemoview_deallocate_pointers();
!    
!    int kemoview_get_PSF_maximum_load();
!    
!
!    void kemoview_set_single_viewer_id(int id_window);
!
!     void kemoview_set_current_viewer_id(int id_window, mul_kemoviewer_type *kemoview_array);
!    int kemoview_get_current_viewer_id(mul_kemoviewer_type *kemoview_array);
!    
!    void kemoview_draw_objects_c();
!    void kemoview_init_lighting();
!    
!    void kemoview_init_background_color();
!    void kemoview_set_background_color(GLfloat color[4]);
!    void kemoview_get_background_color(GLfloat color[4]);
!    
!    void kemoview_viewer_evolution(int istep);
!    
!    void kemoview_write_modelview_file(const char *file_name);
!    void kemoview_load_modelview_file(const char *file_name);
!    int kemoview_open_data(const char *file_name);
!    
!    void kemoview_close_mesh_view();
!    int  kemoview_close_PSF_view();
!    void kemoview_close_fieldline_view();
!    
!    void kemoview_set_pick_surface_command(const char *command);
!    void kemoview_get_pick_surface_command(char *command);
!    
!    void kemoview_set_viewtype(int sel);
!    
!    
!    void kemoview_draw_with_modified_domain_distance();
!    
!    void kemoview_set_coastline_radius(double radius);
!    double kemoview_get_coastline_radius();
!    
!    void kemoview_set_object_property_flags(int selected, int iflag);
!    int kemoview_get_object_property_flags(int selected);
!    int kemoview_toggle_object_properties(int selected);
!    
!    void kemoview_set_mesh_color_mode(int icolor);
!    void kemoview_set_num_of_color_loop(int icolor);
!    
!    void kemoview_set_node_diamater(double diam);
!    void kemoview_set_domain_distance(double dist);
!    
!    
!    void kemoview_set_mesh_color_flag(int iflag_group, int selected, int icolor);
!    void kemoview_set_mesh_color_code(int iflag_group, int selected, float color_code4[4]);
!    
!    void kemoview_set_mesh_opacity(int iflag_group, double opacity_in);
!    double kemoview_get_mesh_opacity(int iflag_group);
!    
!    
!    void kemoview_set_mesh_draw_flag(int selected, int iflag);
!    void kemoview_mesh_draw_toggle(int selected);
!    
!    void kemoview_set_draw_mesh_item(int iflag_group, int selected, int igrp, int iflag);
!    void kemoview_toggle_draw_mesh_item(int iflag_group, int selected, int igrp){
!    int kemoview_get_draw_mesh_item(int iflag_group, int selected, int igrp);
!    
!    int kemoview_get_draw_mesh_flag();
!    
!    int kemoview_get_num_subdomain();
!    int kemoview_get_num_node_grp();
!    int kemoview_get_num_ele_grp();
!    int kemoview_get_num_surf_grp();
!    
!    void kemoview_get_node_grp_name(char *name, int i);
!    void kemoview_get_ele_grp_name(char *name, int i);
!    void kemoview_get_surf_grp_name(char *name, int i);
!    
!    
!    int kemoview_get_view_type_flag();
!    
!    int kemoview_get_num_of_color_loop();
!    
!    
!    double kemoview_get_node_diamater();
!    double kemoview_get_domain_distance();
!    
!    
!    void kemoview_get_ext_from_file_name(const char *file_head, char *stripped_fhead, char *stripped_ext);
!    void kemoview_add_ext_to_file_name(const char *file_head, const char *added_ext, char *file_name);
!    
!    
!    void kemoview_set_text_color_code(float c_code[4]);
!    void kemoview_get_text_color_code(float c_code[4]);
!    
!    int kemoview_set_image_file_format_id(char *image_fmt);
!    void kemoview_get_fliped_img(int npixel_x, int npixel_y,
!                                 unsigned char *glimage, unsigned char *fliped_img);
!    void kemoview_write_window_to_file(int iflag_img, const char *fhead);
!    void kemoview_write_window_to_file_w_step(int iflag_img, int istep, const char *fhead);
!    
!    void kemoview_modify_view();
!    void kemoview_rotate();
!    
!    void kemoviewer_reset_to_init_angle();
!    
!    void kemoview_set_retinamode(int i_retina);
!    void kemoview_set_windowsize(int npixel_x, int npixel_y);
!    void kemoview_update_projection_by_viewer_size(int npixel_x, int npixel_y);
!    
!    void kemoview_update_distance();
!    
!    void kemoview_set_rotation_parameter(double rot_vect[4]);
!    void kemoview_set_dragging_rotation(double rot_vect[4]);
!    void kemoview_set_animation_rot_axis(int iaxis);
!    void kemoview_set_animation_rot_angle(int int_degree);
!    void kemoview_set_shift_vector(double position[3]);
!    void kemoview_set_scale_factor(double scale_s);
!    void kemoview_set_projection_aperture(double aperture_s);
!    void kemoview_set_stereo_parameter(double focus, double eye_sep);
!    
!    void kemoview_get_windowsize(int *npixel_x, int *npixel_y);
!    void kemoview_get_rotation_parameter(double rot_vect[4]);
!    void kemoview_get_shift_vector(double position[3]);
!    void kemoview_get_lookat_vector(double position[3]);
!    double kemoview_get_scale_factor();
!    double kemoview_get_projection_aperture();
!    void kemoview_get_projection_parameters(double *aperture_s, double *near_s,
!                                            double *far_s, double *aspect_s);
!    double kemoview_get_stereo_focus();
!    double kemoview_get_stereo_eyeseparation();
!    
!    void kemoview_mousedolly(double start[2], double x_dolly, double y_dolly);
!    void kemoview_mousepan(double start[2], double x_pan, double y_pan);
!    void kemoview_zooming(double wheelDelta);
!    
!    void kemoview_startTrackball(double x, double y);
!    /* calculated rotation based on current mouse position */
!    void kemoview_rollToTrackball(double x, double y);
!    /* add a GL rotation (dA) to an existing GL rotation (A) */
!    void kemoview_drugging_addToRotationTrackball();
!    void kemoview_animation_add_rotation(double dt);
!    void kemoview_reset_animation();
!    
!    void kemoview_set_stereo_shutter(int iflag);
!    void kemoview_set_anaglyph_flag(int iflag);
!    int kemoview_get_stereo_shutter();
!    int kemoview_get_anaglyph_flag();
!    
!    /* subroutines for surafces */
!    void kemoview_set_PSF_num_loaded(int selected, int num);
!    void kemoview_set_loaded_PSF_flag(int id_psf, int iflag);
!    int kemoview_get_PSF_loaded_params(int selected);
!    int kemoview_get_PSF_loaded_flag(int id_psf);
!    
!    int kemoview_get_PSF_full_path_file_prefix(char *file_head, int *iflag);
!    int kemoview_get_PSF_file_prefix(char *file_head);
!    
!    void kemoview_set_each_PSF_field_param(int selected, int input);
!    
!    int kemoview_get_each_PSF_field_param(int selected);
!    int kemoview_get_PSF_num_component(int i);
!    void kemoview_get_PSF_field_name(char *name, int i);
!    
!    void set_texture_current_psf(int img_fmt, const char *img_head);
!    
!    void kemoview_set_PSF_polygon_mode(int iflag);
!    void kemoview_set_PSF_tangential_vec_mode(int iflag);
!    
!    int kemoview_get_PSF_draw_refv();
!    int kemoview_toggle_PSF_draw_refv();
!    
!    void kemoview_set_PSF_color_param(int selected, int input);
!    int kemoview_get_PSF_color_param(int selected){
!
!    void kemoview_set_PSF_vector_scale(double scale);
!    void kemoview_set_PSF_vector_thickness(double value, int i_digit);
!    
!    void kemoview_get_PSF_vector_scale(double *value, int *i_digit);
!    void kemoview_get_PSF_vector_thickness(double *value, int *i_digit);
!    
!    int kemoview_get_PSF_draw_flags(int selected);
!    int kemoview_select_PSF_draw_switch(int selected);
!    
!    double kemoview_get_each_PSF_data_range(int selected, int icomp);
!    
!    void kemoview_delete_PSF_color_list(int i_delete);
!    void kemoview_delete_PSF_opacity_list(int i_delete);
!    void kemoview_add_PSF_color_list(double add_value, double add_color);
!    void kemoview_add_PSF_opacity_list(double add_value, double add_opacity);
!	
!    void kemoview_set_PSF_linear_colormap(double minvalue, int i_min_digit,
!                                          double maxvalue, int i_max_digit);
!
!    void kemoview_set_PSF_single_color(double *rgba);
!    void kemoview_set_PSF_constant_opacity(double opacity);
!    
!    void kemoview_get_PSF_rgb_at_value(double value, double *red, double *green, double *blue);
!    double kemoview_get_PSF_opacity_at_value(double value);
!    void kemoview_set_PSF_color_data(int i_point, double value, double color);
!    void kemoview_set_PSF_opacity_data(int i_point, double value, double opacity);
!    
!    double kemoview_get_each_PSF_colormap_range(int selected);
!    
!    void kemoview_get_PSF_color_items(int i_point, double *value, double *color);
!    void kemoview_get_PSF_opacity_items(int i_point, double *value, double *opacity);
!    
!    void kemoview_write_PSF_colormap_file(const char *file_name);
!    void kemoview_read_PSF_colormap_file(const char *file_name);
!	 void kemoview_check_PSF_colormap_control();
!    
!    
!    /* Subroutines for field lines */
!    
!    int kemoview_get_fline_file_step_prefix(char *file_head);
!    void kemoview_set_fline_file_step(int istep);
!    
!    void kemoview_set_fline_field_param(int selected, int input);
!    
!    void kemoview_set_fline_parameters(int selected, int iflag);
!    int kemoview_get_fline_parameters(int selected);
!
!    void kemoview_set_fline_color_param(int selected, int input);
!    
!    int kemoview_get_fline_color_num_comps(int i);
!    void kemoview_get_fline_color_data_name(char *name, int i);
!    
!	int kemoview_toggle_fline_type();
!	
!	void kemoview_set_fline_thickness(double value, int i_digit);
!	double kemoview_get_fline_thickness();
!    
!    void kemoview_set_fline_linear_colormap(double minvalue, double maxvalue);
!    void kemoview_set_fline_constant_opacity(double opacity);
!    
!    double kemoview_get_fline_opacity_at_value(double value);
!    
!    void kemoview_set_fline_color_data(int i_point, double value, double color);
!    void kemoview_set_fline_opacity_data(int i_point, double value, double opacity);
!    
!    double kemoview_get_fline_data_range(int selected, int icomp){
!    double kemoview_get_fline_colormap_range(int selected);
        function kemoview_get_fline_colormap_range(selected)            &
     &          BIND(C, name = "kemoview_get_fline_colormap_range")
!          IMPORT C_DOUBLE, c_int
          integer(c_int) :: kemoview_get_fline_maximum_opacity
          integer(c_int) :: selected
        END function
!    
!    int kemoview_get_fline_color_param(int selected);
        function kemoview_get_fline_color_param(selected)               &
     &          BIND(C, name = "kemoview_get_fline_color_param")
!          IMPORT c_int
          integer(c_int) :: kemoview_get_ffline_opacity_num
          integer(c_int) :: selected
        END function
!
!    void kemoview_get_fline_color_item(int i_point, double *value, double *color);
        subroutine kemoview_get_fline_color_item                        &
     &           (i_point, value, color)                                &
     &            BIND(C, name = "kemoview_get_fline_color_item")
!          IMPORT C_DOUBLE, c_int
          integer(c_int), VALUE :: i_point
          REAL(C_DOUBLE), intent(inout) :: value(i_point)
          REAL(C_DOUBLE), intent(inout) :: color(i_point)
        END subroutine
!
!    void kemoview_get_fline_opacity_item(int i_point, double *value, double *opacity);
        subroutine kemoview_get_fline_opacity_item                      &
     &           (i_point, value, opacity)                              &
     &            BIND(C, name = "kemoview_get_fline_opacity_item")
!          IMPORT C_DOUBLE, c_int
          integer(c_int), VALUE :: i_point
          REAL(C_DOUBLE), intent(inout) :: value(i_point)
          REAL(C_DOUBLE), intent(inout) :: opacity(i_point)
        END subroutine
!
!    void kemoview_write_fline_colormap_file(const char *file_name);
        subroutine kemoview_write_fline_colormap_file(file_name)        &
     &            BIND(C, name = "kemoview_write_fline_colormap_file")
!          IMPORT C_DOUBLE, C_CHAR
          character(kind=C_CHAR, LEN=kchara) :: file_name
          REAL(C_DOUBLE) :: kemoview_write_fline_colormap_file
        END subroutine
!    
      end interface
!
! -----------------------------------------------------------------------
!
      end module kemorin_MHD_C_linkage
