!>@file   t_ctl_data_plot_pg.f90
!!@brief  module t_ctl_data_plot_pg
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief  Structure of control data for viewer with PGplot
!!
!!@verbatim
!!      subroutine read_control_draw_pg(pg_ctl)
!!      subroutine read_control_drmed_grp_pg(pg_ctl)
!!
!!      subroutine dealloc_plot_ctl_data(pg_ctl)
!!        type(controls_with_pgplot), intent(inout) :: pg_ctl
!!@endverbatim
!
      module t_ctl_data_plot_pg
!
      use m_precision
      use m_machine_parameter
!
      use t_read_control_elements
      use t_ctl_data_4_time_steps
      use t_control_elements
      use t_control_array_character3
      use t_control_array_int2real
      use t_control_array_int2real2
      use skip_comment_f
!
      implicit none
!
!
      integer(kind = kint), parameter :: pg_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                 :: fname_pgplot_ctl = 'ctl_draw_pg'
      character(len = kchara), parameter                                &
     &                 :: fname_drmed_grp_ctl = 'ctl_draw_pg_grouped'
!
!
!>      Structure of panel and color information
      type pgplot_panel_ctl
        type(read_character_item)  :: contour_type_ctl
        type(read_character_item)  :: color_mode_ctl
!
        type(read_integer_item) :: num_panels_ctl
!
        integer(kind= kint) :: i_pgplot_param =    0
      end type pgplot_panel_ctl
!
!>      Structure of field to view
      type pgplot_field_ctl
        type(read_character_item) :: psf_data_fmt_ctl
        type(read_character_item) :: psf_file_head_ctl
        type(read_character_item) :: map_grid_file_ctl
!
!>      Structure for list of field
!!@n      plot_field_ctl%c1_tbl: Name of field
!!@n      plot_field_ctl%c2_tbl: component to plot
!!@n      plot_field_ctl%c3_tbl: label for plot
        type(ctl_array_c3) :: plot_field_ctl
!
!>      Structure for range data input
!!@n      contour_range_ctl%int1: Component ID for plot
!!@n      contour_range_ctl%int2: Number of Contour lines
!!@n      contour_range_ctl%vec1: Minimum value
!!@n      contour_range_ctl%vec2: Maximum value
        type(ctl_array_i2r2) :: contour_range_ctl
!
!>      Structure for range data input
!!@n      vector_scale_ctl%int1: Component ID for plot
!!@n      vector_scale_ctl%int2: Incrememnt for vedctor data to draw
!!@n      vector_scale_ctl%vect: Scale factor for vectror
        type(ctl_array_i2r) :: vector_scale_ctl
!
        integer(kind= kint) :: i_sf_plotting =     0
      end type pgplot_field_ctl
!
!>      Structure of section plot
      type pgpolot_section_ctl
        type(read_real_item) :: outer_radius_ctl
        type(read_real_item) :: ro_ri_ratio_ctl
        type(read_real2_item) :: pg_plane_size_ctl
!
        integer(kind= kint) :: i_z_plane_ctl =     0
      end type pgpolot_section_ctl
!
!>      Structure of map plot
      type pgpolot_map_ctl
        type(read_integer_item) :: radial_ID_ctl
        type(read_character_item) :: pg_grid_type_ctl
!
        integer(kind= kint) :: i_sphere_map_ctl =  0
      end type pgpolot_map_ctl
!
!
!>      Structure of control data using pgplot
      type controls_with_pgplot
!>        Structure for time stepping control
        type(time_data_control) :: t_pg_ctl
!
!>        Structure of panel and color information
        type(pgplot_panel_ctl) :: pg_panel_ctl
!>        Structure of field to view
        type(pgplot_field_ctl) :: pg_fld_ctl
!>        Structure of section plot
        type(pgpolot_section_ctl) :: pg_section_ctl
!>        Structure of map plot
        type(pgpolot_map_ctl) :: pg_map_ctl
!
        integer(kind= kint) :: i_draw_pgplot =  0
        integer(kind= kint) :: i_drmed_grp =    0
      end type controls_with_pgplot
!
!     top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_draw_pgplot = 'drawing_pgplot_ctl'
!
!     top level for drmed_grp
!
      character(len=kchara), parameter                                  &
     &                    :: hd_drmed_grp =   'draw_grouping_pg_ctl'
!
!     flags for 2nd level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_pgplot_param =   'pgplot_param_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_sf_plotting =    'surf_plotting_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_z_plane_ctl =    'z_plane_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_sphere_map_ctl = 'sphere_map_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_grouping_plot = 'grouping_plot_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
!     flags for pgplot paramter
!
      character(len=kchara), parameter                                  &
     &                    :: hd_contour_type_ctl = 'contour_type_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_color_mode_ctl =   'color_mode_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_num_panels_ctl =   'num_panels_ctl'
!
!     flags for plotting paramteres
!
      character(len=kchara), parameter                                  &
     &                    :: hd_psf_data_fmt_ctl =  'psf_data_fmt_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_psf_data_ctl =      'psf_file_name_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_map_grid_file =     'map_grid_file_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_field_2_plot =      'plot_field_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_ctr_range_ctl =    'contour_range_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_vec_scale_ctl =    'vector_scale_ctl'
      integer(kind= kint) :: i_psf_data_ctl =      0
      integer(kind= kint) :: i_ntot_range_ctl =    0
      integer(kind= kint) :: i_ntot_scale_ctl =    0
!
!     flags for z-plane
!
      character(len=kchara), parameter                                  &
     &                    :: hd_outer_radius_ctl = 'outer_radius_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_ro_ri_ratio_ctl =  'ro_ri_ratio_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_plane_size_ctl =   'plane_size_ctl'
!
!     flags for sphere map
!
      character(len=kchara), parameter                                  &
     &                    :: hd_radial_ID_ctl = 'radial_ID_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_sph_grid_type = 'sph_grid_type_ctl'
!
!     flags for grouping plot
!
      integer(kind= kint) :: i_start_ele_grp =      0
!
!
      private :: fname_pgplot_ctl, fname_drmed_grp_ctl
!
      private :: hd_draw_pgplot, hd_drmed_grp
      private :: hd_pgplot_param, hd_sf_plotting, hd_z_plane_ctl
      private :: hd_sphere_map_ctl, hd_grouping_plot, hd_time_step
      private :: hd_contour_type_ctl, hd_color_mode_ctl
      private :: hd_num_panels_ctl, hd_psf_data_fmt_ctl
      private :: hd_map_grid_file, hd_psf_data_ctl, hd_vec_scale_ctl
      private :: hd_ctr_range_ctl,   hd_field_2_plot
      private :: hd_outer_radius_ctl, hd_ro_ri_ratio_ctl
      private :: hd_plane_size_ctl
      private :: hd_radial_ID_ctl, hd_sph_grid_type
!
      private :: read_ctl_data_draw_pgplot
      private :: read_ctl_data_draw_grped_pg
      private :: read_ctl_data_4_pgplot_param
      private :: read_ctl_data_4_surf_plot
      private :: read_ctl_data_4_zplane_plot, read_ctl_data_4_sph_map
      private :: dealloc_plot_field_ctl
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_control_draw_pg(pg_ctl)
!
      type(controls_with_pgplot), intent(inout) :: pg_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      open(pg_ctl_file_code, file=fname_pgplot_ctl)
!
      do
        call load_one_line_from_control(pg_ctl_file_code, c_buf1)
        call read_ctl_data_draw_pgplot                                  &
     &     (pg_ctl_file_code, hd_draw_pgplot, pg_ctl, c_buf1)
        if(pg_ctl%i_draw_pgplot .gt. 0) exit
      end do
      close(pg_ctl_file_code)
!
      end subroutine read_control_draw_pg
!
!   --------------------------------------------------------------------
!
      subroutine read_control_drmed_grp_pg(pg_ctl)
!
      type(controls_with_pgplot), intent(inout) :: pg_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      open(pg_ctl_file_code, file=fname_drmed_grp_ctl)
!
      do
        call load_one_line_from_control(pg_ctl_file_code, c_buf1)
        call read_ctl_data_draw_grped_pg                                &
     &     (pg_ctl_file_code, hd_drmed_grp, pg_ctl, c_buf1)
        if(pg_ctl%i_drmed_grp .gt. 0) exit
      end do
      close(pg_ctl_file_code)
!
      end subroutine read_control_drmed_grp_pg
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_plot_ctl_data(pg_ctl)
!
      type(controls_with_pgplot), intent(inout) :: pg_ctl
!
!
      call dealloc_plot_field_ctl(pg_ctl%pg_fld_ctl)
!
      end subroutine dealloc_plot_ctl_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_draw_pgplot                              &
     &         (id_control, hd_block, pg_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(controls_with_pgplot), intent(inout) :: pg_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pg_ctl%i_draw_pgplot .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, pg_ctl%t_pg_ctl, c_buf)
!
        call read_ctl_data_4_pgplot_param                               &
     &     (id_control, hd_pgplot_param, pg_ctl%pg_panel_ctl, c_buf)
        call read_ctl_data_4_surf_plot(id_control, hd_sf_plotting,      &
     &      pg_ctl%pg_fld_ctl, c_buf)
        call read_ctl_data_4_zplane_plot(id_control, hd_z_plane_ctl,    &
     &      pg_ctl%pg_section_ctl, c_buf)
        call read_ctl_data_4_sph_map(id_control, hd_sphere_map_ctl,     &
     &      pg_ctl%pg_map_ctl, c_buf)
      end do
      pg_ctl%i_draw_pgplot = 1
!
      end subroutine read_ctl_data_draw_pgplot
!
!-----------------------------------------------------------------------
!
      subroutine read_ctl_data_draw_grped_pg                            &
     &         (id_control, hd_block, pg_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(controls_with_pgplot), intent(inout) :: pg_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pg_ctl%i_drmed_grp .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_ctl_data_4_pgplot_param                               &
     &     (id_control, hd_pgplot_param, pg_ctl%pg_panel_ctl, c_buf)
        call read_ctl_data_4_surf_plot(id_control, hd_sf_plotting,      &
     &     pg_ctl%pg_fld_ctl, c_buf)
      end do
      pg_ctl%i_drmed_grp = 1
!
      end subroutine read_ctl_data_draw_grped_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_pgplot_param                           &
     &         (id_control, hd_block, pg_panel_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pgplot_panel_ctl), intent(inout) :: pg_panel_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pg_panel_ctl%i_pgplot_param .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_contour_type_ctl, pg_panel_ctl%contour_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_color_mode_ctl, pg_panel_ctl%color_mode_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_panels_ctl, pg_panel_ctl%num_panels_ctl)
      end do
      pg_panel_ctl%i_pgplot_param = 1
!
      end subroutine read_ctl_data_4_pgplot_param
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_surf_plot                              &
     &         (id_control, hd_block, pg_fld_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pgplot_field_ctl), intent(inout) :: pg_fld_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pg_fld_ctl%i_sf_plotting .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c3(id_control,                          &
     &      hd_field_2_plot, pg_fld_ctl%plot_field_ctl, c_buf)
!
        call read_control_array_i2_r2(id_control,                       &
     &      hd_ctr_range_ctl, pg_fld_ctl%contour_range_ctl, c_buf)
!
        call read_control_array_i2_r(id_control,                        &
     &      hd_vec_scale_ctl, pg_fld_ctl%vector_scale_ctl, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_psf_data_fmt_ctl, pg_fld_ctl%psf_data_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_psf_data_ctl, pg_fld_ctl%psf_file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_map_grid_file, pg_fld_ctl%map_grid_file_ctl)
      end do
      pg_fld_ctl%i_sf_plotting = 1
!
      end subroutine read_ctl_data_4_surf_plot
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_zplane_plot                            &
     &         (id_control, hd_block, pg_section_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pgpolot_section_ctl), intent(inout) :: pg_section_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pg_section_ctl%i_z_plane_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_real_ctl_type(c_buf, hd_outer_radius_ctl,             &
     &      pg_section_ctl%outer_radius_ctl)
        call read_real_ctl_type(c_buf, hd_ro_ri_ratio_ctl,              &
     &      pg_section_ctl%ro_ri_ratio_ctl)
!
        call read_real2_ctl_type(c_buf,                                 &
     &      hd_plane_size_ctl, pg_section_ctl%pg_plane_size_ctl)
      end do
      pg_section_ctl%i_z_plane_ctl = 1
!
      end subroutine read_ctl_data_4_zplane_plot
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_sph_map                                &
     &         (id_control, hd_block, pg_map_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pgpolot_map_ctl), intent(inout) :: pg_map_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pg_map_ctl%i_sphere_map_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_sph_grid_type, pg_map_ctl%pg_grid_type_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_radial_ID_ctl, pg_map_ctl%radial_ID_ctl)
      end do
      pg_map_ctl%i_sphere_map_ctl = 1
!
      end subroutine read_ctl_data_4_sph_map
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_plot_field_ctl(pg_fld_ctl)
!
      type(pgplot_field_ctl), intent(inout) :: pg_fld_ctl
!
!
      call dealloc_control_array_i2_r(pg_fld_ctl%vector_scale_ctl)
      call dealloc_control_array_i2_r2(pg_fld_ctl%contour_range_ctl)
      call dealloc_control_array_c3(pg_fld_ctl%plot_field_ctl)
!
      end subroutine dealloc_plot_field_ctl
!
!-----------------------------------------------------------------------
!
      end module t_ctl_data_plot_pg
