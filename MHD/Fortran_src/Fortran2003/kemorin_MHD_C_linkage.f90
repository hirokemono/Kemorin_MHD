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
!!
!
!    void kemoview_allocate_single_viwewer_struct(struct kemoviewer_type *kemoviewer_data);
!    void kemoview_deallocate_pointers();
!    
!    int kemoview_get_PSF_maximum_load();
!    
!
!     void kemoview_set_current_viewer_id(int id_window, mul_kemoviewer_type *kemoview_array);
!    
!    void kemoview_draw_objects_c();
!    void kemoview_init_lighting();
!    void kemoview_gl_init_lighting(struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_init_background_color(struct kemoviewer_type *kemoviewer);
!    void kemoview_set_background_color(float color[4],
!                                       struct kemoviewer_type *kemoviewer);
!    void kemoview_get_background_color(struct kemoviewer_type *kemoviewer,
!                                       float color[4]);
!
!    void kemoview_viewer_evolution(int istep, struct kemoviewer_type *kemoviewer);
!
!    void kemoview_write_modelview_file(struct kv_string *filename,
!                                       struct kemoviewer_type *kemoviewer);
!    void kemoview_load_modelview_file(struct kv_string *filename,
!                                      struct kemoviewer_type *kemoviewer);
!
!
!    int kemoview_open_data(struct kv_string *filename,
!                           struct kemoviewer_type *kemoviewer);
!    void kemoview_close_mesh_view(struct kemoviewer_type *kemoviewer);
!    int  kemoview_close_PSF_view(struct kemoviewer_type *kemoviewer);
!    void kemoview_close_fieldline_view(struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_set_viewtype(int sel);
!    
!    
!    void kemoview_draw_with_modified_domain_distance(struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_set_coastline_radius(double radius, kemo_sgl);
!    double kemoview_get_coastline_radius(struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_set_object_property_flags(int selected, int iflag,
!                                            struct kemoviewer_type *kemoviewer);
!    int kemoview_get_object_property_flags(struct kemoviewer_type *kemoviewer, int selected);
!    int kemoview_toggle_object_properties(int selected);
!    
!    void kemoview_set_mesh_color_mode(int icolor, struct kemoviewer_type *kemoviewer);
!    void kemoview_set_num_of_color_loop(int icolor, struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_set_node_diamater(double factor, int i_digit,
!                                    struct kemoviewer_type *kemoviewer);
!    void kemoview_get_node_diamater(struct kemoviewer_type *kemoviewer,
!                                    double *factor, int *i_digit);
!
!    void kemoview_set_domain_distance(double dist, struct kemoviewer_type *kemoviewer);
!    double kemoview_get_domain_distance(struct kemoviewer_type *kemoviewer);
!
!    
!    void kemoview_set_mesh_color_flag(int iflag_group, int selected, int icolor,
!                                      struct kemoviewer_type *kemoviewer);
!    void kemoview_set_mesh_color_code(int iflag_group, int selected, float color_code4[4]);
!    
!    void kemoview_set_mesh_opacity(int iflag_group, double opacity_in,
!                                   struct kemoviewer_type *kemoviewer);
!    double kemoview_get_mesh_opacity(struct kemoviewer_type *kemoviewer,
!                                     int iflag_group);
!
!    
!    void kemoview_set_mesh_draw_flag(int selected, int iflag,
!                                     struct kemoviewer_type *kemoviewer);
!    void kemoview_set_draw_mesh_item(int iflag_group, int selected, int igrp, int iflag);
!    int kemoview_get_draw_mesh_item(int iflag_group, int selected, int igrp);
!    
!    int kemoview_get_draw_mesh_flag(struct kemoviewer_type *kemoviewer);
!    
!    int kemoview_get_num_of_mesh_group(int iflag_group);
!    
!    int kemoview_get_num_of_mesh_group(struct kemoviewer_type *kemoviewer,
!                                       int iflag_group);
!    void kemoview_get_node_grp_name(struct kemoviewer_type *kemoviewer,
!                                   int i, struct kv_string *groupname);
!    void kemoview_get_ele_grp_name(struct kemoviewer_type *kemoviewer,
!                                   int i, struct kv_string *groupname);
!    void kemoview_get_surf_grp_name(struct kemoviewer_type *kemoviewer,
!                                    int i, struct kv_string *groupname);
!    
!    
!    int kemoview_get_view_type_flag();
!    
!    int kemoview_get_num_of_color_loop(struct kemoviewer_type *kemoviewer);
!    
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
!    void kemoview_get_gl_buffer_to_bmp(int npix_x, int npix_y, unsigned char *image);
!    void kemoview_get_fliped_img(int npixel_x, int npixel_y,
!                                 unsigned char *glimage, unsigned char *fliped_img);
!    void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix,
!                                       int npix_x, int npix_y, unsigned char *image);
!    void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix,
!                                              int npix_x, int npix_y, unsigned char *image);
!
!    void kemoview_modify_view();
!    void kemoviewer_reset_to_init_angle();
!    
!    void kemoview_set_retinamode(int i_retina);
!    void kemoview_set_windowsize(int npixel_x, int npixel_y, int nwindow_x, int nwindow_y);
!    void kemoview_update_projection_by_viewer_size(int npixel_x, int npixel_y, int nwindow_x, int nwindow_y);
!    
!    void kemoview_set_view_parameter(int selected, int i, double rot_vect);
!    void kemoview_set_stereo_parameter(int selected, double value);
!
!    int kemoview_get_view_integer(kemo_sgl, int selected);
!    void kemoview_set_view_integer(int selected, int ivalue);
!    double kemoview_get_view_parameter(int selected, int i);
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
!    /* subroutines for surafces */
!    void kemoview_set_PSF_num_loaded(int selected, int num);
!    int kemoview_get_PSF_loaded_params(struct kemoviewer_type *kemoviewer, int selected);
!    int kemoview_get_PSF_loaded_flag(struct kemoviewer_type *kemoviewer, int id_psf);
!
!    int kemoview_get_PSF_full_path_file_prefix(struct kemoviewer_type *kemoviewer,
!                                               struct kv_string *psf_filehead, int *iflag);
!
!    void kemoview_set_each_PSF_field_param(int selected, int input,
!                                           struct kemoviewer_type *kemoviewer);
!
!    int kemoview_get_each_PSF_field_param(struct kemoviewer_type *kemoviewer,
!                                          int selected);
!    int kemoview_get_PSF_num_component(struct kemoviewer_type *kemoviewer, int i);
!    void kemoview_get_PSF_field_name(struct kemoviewer_type *kemoviewer,
!                                     struct kv_string *colorname, int i);
!    
!    void set_texture_current_psf(int img_fmt, const char *img_head);
!    
!    void kemoview_set_PSF_polygon_mode(int iflag, struct kemoviewer_type *kemoviewer);
!    void kemoview_set_PSF_tangential_vec_mode(int iflag, struct kemoviewer_type *kemoviewer);
!    
!    int kemoview_get_PSF_draw_refv(struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_set_PSF_color_param(int selected, int input,
!                                      struct kemoviewer_type *kemoviewer);
!    int kemoview_get_PSF_color_param(struct kemoviewer_type *kemoviewer,
!                                     int selected);
!
!    void kemoview_set_each_PSF_color_w_exp(int selected, double value, int i_digit,
!                                           struct kemoviewer_type *kemoviewer);
!    
!    int kemoview_get_PSF_draw_flags(struct kemoviewer_type *kemoviewer,
!                                    int selected);
!    int kemoview_select_PSF_draw_switch(struct kemoviewer_type *kemoviewer,
!                                        int selected);
!    
!    double kemoview_get_each_PSF_data_range(struct kemoviewer_type *kemoviewer,
!                                            int selected, int icomp);
!    
!    void kemoview_delete_PSF_color_list(int i_delete,
!                                        struct kemoviewer_type *kemoviewer);
!    void kemoview_delete_PSF_opacity_list(int i_delete,
!                                          struct kemoviewer_type *kemoviewer);
!    void kemoview_add_PSF_color_list(double add_value, double add_color,
!                                     struct kemoviewer_type *kemoviewer);
!    void kemoview_add_PSF_opacity_list(double add_value, double add_opacity,
!                                       struct kemoviewer_type *kemoviewer);
!	
!    void kemoview_set_PSF_linear_colormap(double minvalue, int i_min_digit,
!                                          double maxvalue, int i_max_digit,
!                                          struct kemoviewer_type *kemoviewer);
!
!    void kemoview_set_PSF_single_color(double *rgba,
!                                       struct kemoviewer_type *kemoviewer);
!    void kemoview_set_PSF_constant_opacity(double opacity,
!                                           struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_get_PSF_rgb_at_value(struct kemoviewer_type *kemoviewer, double value,
!                                       double *red, double *green, double *blue);
!    double kemoview_get_PSF_opacity_at_value(struct kemoviewer_type *kemoviewer, 
!                                             double value);
!    void kemoview_set_PSF_color_data(int i_point, double value, double color,
!                                     struct kemoviewer_type *kemoviewer);
!    void kemoview_set_PSF_opacity_data(int i_point, double value, double opacity,
!                                       struct kemoviewer_type *kemoviewer);
!    
!    double kemoview_get_each_PSF_colormap_range(struct kemoviewer_type *kemoviewer,
!                                                int selected);
!    
!    void kemoview_get_PSF_color_items(struct kemoviewer_type *kemoviewer,
!                                      int i_point, double *value, double *color);
!    void kemoview_get_PSF_opacity_items(struct kemoviewer_type *kemoviewer,
!                                        int i_point, double *value, double *opacity);
!    
!    void kemoview_write_PSF_colormap_file(struct kv_string *filename,
!                                          struct kemoviewer_type *kemoviewer);
!    void kemoview_read_PSF_colormap_file(struct kv_string *filename,
!                                         struct kemoviewer_type *kemoviewer);
!
!    
!    /* Subroutines for field lines */
!    
!    int kemoview_get_fline_file_step_prefix(struct kemoviewer_type *kemoviewer,
!                                            struct kv_string *fline_filehead);
!    void kemoview_set_fline_file_step(int istep, struct kemoviewer_type *kemoviewer);
!    
!    void kemoview_set_fline_field_param(int selected, int input);
!    
!    void kemoview_set_fline_parameters(int selected, int iflag, 
!                                       struct kemoviewer_type *kemoviewer);
!    int kemoview_get_fline_parameters(int selected);
!
!    void kemoview_set_fline_color_param(int selected, int input,
!                                        struct kemoviewer_type *kemoviewer);
!    
!    int kemoview_get_fline_color_num_comps(struct kemoviewer_type *kemoviewer, int i);
!    void kemoview_get_fline_color_data_name(struct kemoviewer_type *kemoviewer,
!                                            struct kv_string *colorname, int i);
!    
!    void kemoview_set_fline_color_w_exp(int selected, double value, int i_digit,
!                                        struct kemoviewer_type *kemoviewer);
!    void kemoview_set_fline_linear_colormap(double minvalue, int i_min_digit,
!                                            double maxvalue, int i_max_digit,
!                                            struct kemoviewer_type *kemoviewer);
!    void kemoview_get_fline_color_w_exp(struct kemoviewer_type *kemoviewer,
!                                        int selected, double *value, int *i_digit);
!
!    void update_fieldline_menu_hbox(struct kemoviewer_type *kemo_sgl,
!                                    struct fieldline_gtk_menu *fline_menu){
!    double kemoview_get_fline_opacity_at_value(struct kemoviewer_type *kemoviewer,
!                                               double value);
!    
!    void kemoview_set_fline_color_data(int i_point, double value, double color,
!                                       struct kemoviewer_type *kemoviewer);
!    void kemoview_set_fline_opacity_data(int i_point, double value, double opacity,
!                                         struct kemoviewer_type *kemoviewer,);
!    
!    double kemoview_get_fline_data_range(struct kemoviewer_type *kemoviewer,
!                                         int selected, int icomp);
!    double kemoview_get_fline_data_range(struct kemoviewer_type *kemoviewer,
!                                         int selected, int icomp){
!
        function kemoview_get_fline_colormap_range(selected)            &
     &          BIND(C, name = "kemoview_get_fline_colormap_range")
!          IMPORT C_DOUBLE, c_int
          integer(c_int) :: kemoview_get_fline_maximum_opacity
          integer(c_int) :: selected
        END function
!    
!    int kemoview_get_fline_color_param(struct kemoviewer_type *kemoviewer,
!                                       int selected);
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
!    void kemoview_write_fline_colormap_file(struct kv_string *filename,
!                                            struct kemoviewer_type *kemoviewer){
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
