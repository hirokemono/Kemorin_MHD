!
!      module m_ctl_data_plot_pg
!
!      Written by H. Matsui
!
!      subroutine read_control_draw_pg
!      subroutine read_control_drmed_grp_pg
!
!      subroutine deallocate_plot_ctl_data
!
      module m_ctl_data_plot_pg
!
      use m_precision
      use t_read_control_arrays
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
      character(len=kchara) :: start_ele_grp_name_ctl
!
      character(len=kchara) :: contour_type_ctl = 'Line'
      character(len=kchara) :: color_mode_ctl =   'Rainbow'
!
      real(kind = kreal) :: time_pg
!
      integer(kind = kint) :: num_panels_ctl = 1
!
!    parameter for z_plane
!
      real(kind = kreal) :: outer_radius_ctl
      real(kind = kreal) :: ro_ri_ratio_ctl
!
!   parameter for plane model
!
      real(kind = kreal) :: plane_size_ctl(2)
!
!   parameter for map
!
      integer(kind = kint) :: radial_ID_ctl
      character(len=kchara) :: sph_grid_type_ctl = 'no_pole'
!
      integer(kind = kint) :: ntot_plotting_ctl
!
      character(len=kchara) :: psf_data_fmt_ctl =   'ucd'
      character(len=kchara) :: psf_file_head_ctl
      character(len=kchara) :: map_grid_file_ctl
      character(len=kchara), allocatable :: image_datahead_ctl(:)
!
      character(len=kchara), allocatable :: plot_field_ctl(:)
      character(len=kchara), allocatable :: plot_comp_ctl(:)
      character(len=kchara), allocatable :: plot_label_ctl(:)
!
!!      Structure for range data input
!!@n      contour_range_ctl%int1: Component ID for plot
!!@n      contour_range_ctl%int2: Number of Contour lines
!!@n      contour_range_ctl%vec1: Minimum value
!!@n      contour_range_ctl%vec2: Maximum value
      type(ctl_array_i2r2) :: contour_range_ctl
!
!!      Structure for range data input
!!@n      vector_scale_ctl%int1: Component ID for plot
!!@n      vector_scale_ctl%int2: Incrememnt for vedctor data to draw
!!@n      vector_scale_ctl%vect: Scale factor for vectror
      type(ctl_array_i2r) :: vector_scale_ctl
!
!    parameter for grouping plots
!
      character(len=kchara) :: group_mesh_head_ctl = 'grouping_mesh'
      character(len=kchara) :: group_data_name_ctl = 'correlation.dat'
      integer(kind = kint) :: istep_drmd_grp_ctl
      integer(kind = kint) :: istart_ele_grp_ctl, ngrp_ele_grp_ctl
!
      character(len=kchara) :: time_average_data_ctl = 'NO'
!
!     top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_draw_pgplot = 'drawing_pgplot_ctl'
      integer(kind= kint) :: i_draw_pgplot =    0
!
!     top level for drmed_grp
!
      character(len=kchara), parameter                                  &
     &                    :: hd_drmed_grp =   'draw_grouping_pg_ctl'
      integer(kind= kint) :: i_drmed_grp =    0
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
!
      integer(kind= kint) :: i_pgplot_param =    0
      integer(kind= kint) :: i_sf_plotting =     0
      integer(kind= kint) :: i_z_plane_ctl =     0
      integer(kind= kint) :: i_sphere_map_ctl =  0
      integer(kind= kint) :: i_grouping_plot =   0
!
!     flags for pgplot paramter
!
      character(len=kchara), parameter                                  &
     &                    :: hd_contour_type_ctl = 'contour_type_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_color_mode_ctl =   'color_mode_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_num_panels_ctl =   'num_panels_ctl'
      integer(kind= kint) :: i_contour_type_ctl = 0
      integer(kind= kint) :: i_color_mode_ctl =   0
      integer(kind= kint) :: i_num_panels_ctl =   0
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
     &                    :: hd_ntot_plotting_ctl = 'plot_field_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_ctr_range_ctl =    'contour_range_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_vec_scale_ctl =    'vector_scale_ctl'
      integer(kind= kint) :: i_psf_data_fmt_ctl =  0
      integer(kind= kint) :: i_psf_data_ctl =      0
      integer(kind= kint) :: i_map_grid_file =     0
      integer(kind= kint) :: i_ntot_plotting_ctl = 0
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
      integer(kind= kint) :: i_outer_radius_ctl = 0
      integer(kind= kint) :: i_ro_ri_ratio_ctl =  0
      integer(kind= kint) :: i_plane_size_ctl =   0
!
!     flags for sphere map
!
      character(len=kchara), parameter                                  &
     &                    :: hd_radial_ID_ctl = 'radial_ID_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_sph_grid_type = 'sph_grid_type_ctl'
      integer(kind= kint) :: i_radial_ID_ctl = 0
      integer(kind= kint) :: i_sph_grid_type = 0
!
!     flags for grouping plot
!
      character(len=kchara), parameter                                  &
     &                    :: hd_group_mesh_head = 'group_mesh_head_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_group_data_name = 'group_data_name_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_istep_drmd_grp =  'step_to_plot_ctl'
      character(len=kchara), parameter :: hd_start_ele_grp_name         &
     &                     = 'start_element_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &                    :: hd_ngrp_ele_grp = 'num_element_grp_ctl'
      character(len=kchara), parameter :: hd_time_average_data          &
     &                     = 'time_average_data_ctl'
      integer(kind= kint) :: i_group_mesh_head = 0
      integer(kind= kint) :: i_group_data_name = 0
      integer(kind= kint) :: i_istep_drmd_grp = 0
      integer(kind= kint) :: i_start_ele_grp =      0
      integer(kind= kint) :: i_start_ele_grp_name = 0
      integer(kind= kint) :: i_ngrp_ele_grp = 0
      integer(kind= kint) :: i_time_average_data = 0
!
!
      private :: fname_pgplot_ctl, fname_drmed_grp_ctl
!
      private :: hd_draw_pgplot, i_draw_pgplot
      private :: hd_drmed_grp,     i_drmed_grp
      private :: hd_pgplot_param,   i_pgplot_param
      private :: hd_sf_plotting,    i_sf_plotting
      private :: hd_z_plane_ctl,    i_z_plane_ctl
      private :: hd_sphere_map_ctl, i_sphere_map_ctl
      private :: hd_grouping_plot,  i_grouping_plot
      private :: hd_contour_type_ctl, hd_color_mode_ctl
      private :: hd_num_panels_ctl
      private :: hd_psf_data_fmt_ctl, hd_map_grid_file
      private :: hd_psf_data_ctl, hd_vec_scale_ctl
      private :: hd_ctr_range_ctl,   hd_ntot_plotting_ctl
      private :: hd_outer_radius_ctl, hd_ro_ri_ratio_ctl
      private :: hd_plane_size_ctl
      private :: hd_radial_ID_ctl, hd_sph_grid_type
      private :: hd_group_mesh_head, hd_group_data_name
      private :: hd_istep_drmd_grp, hd_start_ele_grp_name
      private :: hd_ngrp_ele_grp
!
      private :: read_ctl_data_draw_pgplot
      private :: read_ctl_data_4_pgplot_param
      private :: read_ctl_data_4_surf_plot
      private :: read_ctl_data_4_zplane_plot, read_ctl_data_4_sph_map
      private :: read_ctl_data_4_drmd_grp
!
      private :: allocate_plot_ctl_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_control_draw_pg
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = pg_ctl_file_code
      open ( ctl_file_code, file=fname_pgplot_ctl )
!
      call load_ctl_label_and_line
      call read_ctl_data_draw_pgplot
!
      close(ctl_file_code)
!
      end subroutine read_control_draw_pg
!
!   --------------------------------------------------------------------
!
      subroutine read_control_drmed_grp_pg
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = pg_ctl_file_code
      open ( ctl_file_code, file=fname_drmed_grp_ctl)
!
      call load_ctl_label_and_line
      call read_ctl_data_draw_grped_pg
!
      close(ctl_file_code)
!
      end subroutine read_control_drmed_grp_pg
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_draw_pgplot
!
      use m_machine_parameter
      use m_ctl_data_4_time_steps
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_draw_pgplot) .eq. 0) return
      if (i_draw_pgplot .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_draw_pgplot, i_draw_pgplot)
        if(i_draw_pgplot .gt. 0) exit
!
!
        call read_time_step_ctl
!
        call read_ctl_data_4_pgplot_param
        call read_ctl_data_4_surf_plot
        call read_ctl_data_4_zplane_plot
        call read_ctl_data_4_sph_map
      end do
!
      end subroutine read_ctl_data_draw_pgplot
!
!-----------------------------------------------------------------------
!
      subroutine read_ctl_data_draw_grped_pg
!
      use m_machine_parameter
      use m_ctl_data_4_time_steps
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_drmed_grp) .eq. 0) return
      if (i_drmed_grp .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_drmed_grp,  i_drmed_grp)
        if(i_drmed_grp .gt. 0) exit
!
        call read_ctl_data_4_pgplot_param
        call read_ctl_data_4_surf_plot
        call read_ctl_data_4_drmd_grp
      end do
!
      end subroutine read_ctl_data_draw_grped_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_pgplot_param
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_pgplot_param) .eq. 0) return
      if (i_pgplot_param .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pgplot_param, i_pgplot_param)
        if(i_pgplot_param .gt. 0) exit
!
!
        call read_character_ctl_item(hd_contour_type_ctl,               &
     &        i_contour_type_ctl, contour_type_ctl)
        call read_character_ctl_item(hd_color_mode_ctl,                 &
     &        i_color_mode_ctl, color_mode_ctl)
!
        call read_integer_ctl_item(hd_num_panels_ctl,                   &
     &        i_num_panels_ctl, num_panels_ctl)
      end do
!
      end subroutine read_ctl_data_4_pgplot_param
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_surf_plot
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_sf_plotting) .eq. 0) return
      if (i_sf_plotting .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sf_plotting, i_sf_plotting)
        if(i_sf_plotting .gt. 0) exit
!
!
        call find_control_array_flag(hd_ntot_plotting_ctl,              &
     &      ntot_plotting_ctl)
        if(ntot_plotting_ctl.gt.0 .and. i_ntot_plotting_ctl.eq.0) then
          call allocate_plot_ctl_data
          call read_control_array_chara3_list(hd_ntot_plotting_ctl,     &
     &        ntot_plotting_ctl, i_ntot_plotting_ctl,                   &
     &        plot_field_ctl, plot_comp_ctl, plot_label_ctl)
        end if
!
        call read_control_array_i2_r2                                   &
     &     (hd_ctr_range_ctl, contour_range_ctl)
!
        call read_control_array_i2_r                                    &
     &     (hd_vec_scale_ctl, vector_scale_ctl)
!
        call read_character_ctl_item(hd_psf_data_fmt_ctl,               &
     &          i_psf_data_fmt_ctl, psf_data_fmt_ctl)
        call read_character_ctl_item(hd_psf_data_ctl,                   &
     &          i_psf_data_ctl, psf_file_head_ctl)
        call read_character_ctl_item(hd_map_grid_file,                  &
     &          i_map_grid_file, map_grid_file_ctl)
      end do
!
      end subroutine read_ctl_data_4_surf_plot
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_zplane_plot
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_z_plane_ctl) .eq. 0) return
      if (i_z_plane_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_z_plane_ctl, i_z_plane_ctl)
        if(i_z_plane_ctl .gt. 0) exit
!
!
        call read_real_ctl_item(hd_outer_radius_ctl,                    &
     &        i_outer_radius_ctl, outer_radius_ctl)
        call read_real_ctl_item(hd_ro_ri_ratio_ctl,                     &
     &        i_ro_ri_ratio_ctl, ro_ri_ratio_ctl)
!
        call read_real2_ctl_item(hd_plane_size_ctl,                     &
     &        i_plane_size_ctl, plane_size_ctl)
      end do
!
      end subroutine read_ctl_data_4_zplane_plot
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_sph_map
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_sphere_map_ctl) .eq. 0) return
      if (i_sphere_map_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sphere_map_ctl, i_sphere_map_ctl)
        if(i_sphere_map_ctl .gt. 0) exit
!
!
        call read_character_ctl_item(hd_sph_grid_type,                  &
     &        i_sph_grid_type, sph_grid_type_ctl)
!
        call read_integer_ctl_item(hd_radial_ID_ctl,                    &
     &        i_radial_ID_ctl, radial_ID_ctl)
      end do
!
      end subroutine read_ctl_data_4_sph_map
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_drmd_grp
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_grouping_plot) .eq. 0) return
      if (i_grouping_plot .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_grouping_plot, i_grouping_plot)
        if(i_grouping_plot .gt. 0) exit
!
!
        call read_character_ctl_item(hd_group_mesh_head,                &
     &        i_group_mesh_head, group_mesh_head_ctl)
        call read_character_ctl_item(hd_group_data_name,                &
     &        i_group_data_name, group_data_name_ctl)
        call read_character_ctl_item(hd_start_ele_grp_name,             &
     &        i_start_ele_grp_name, start_ele_grp_name_ctl)
        call read_character_ctl_item(hd_time_average_data,              &
     &        i_time_average_data, time_average_data_ctl)
!
        call read_integer_ctl_item(hd_istep_drmd_grp,                   &
     &        i_istep_drmd_grp, istep_drmd_grp_ctl)
        call read_integer_ctl_item(hd_ngrp_ele_grp,                     &
     &        i_ngrp_ele_grp, ngrp_ele_grp_ctl)
      end do
!
      end subroutine read_ctl_data_4_drmd_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_plot_ctl_data
!
!
      allocate( image_datahead_ctl(ntot_plotting_ctl) )
!
      allocate( plot_field_ctl(ntot_plotting_ctl) )
      allocate( plot_comp_ctl(ntot_plotting_ctl) )
      allocate( plot_label_ctl(ntot_plotting_ctl) )
!
      end subroutine allocate_plot_ctl_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_plot_ctl_data
!
!
      deallocate( image_datahead_ctl )
!
      deallocate( plot_field_ctl )
      deallocate( plot_comp_ctl )
      deallocate( plot_label_ctl )
!
      end subroutine deallocate_plot_ctl_data
!
!-----------------------------------------------------------------------
!
      end module m_ctl_data_plot_pg
