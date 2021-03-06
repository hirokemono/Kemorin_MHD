!>@file   t_control_data_vizs.f90
!!@brief  module t_control_data_vizs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine bcast_viz_controls(viz_ctls)
!!      subroutine dealloc_viz_controls(viz_ctls)
!!       type(visualization_controls), intent(inout) :: viz_ctls
!!       type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine add_fields_4_vizs_to_fld_ctl(viz_ctls, field_ctl)
!!        type(visualization_controls), intent(in) :: viz_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    array  cross_section_ctl
!!      ....
!!    end array cross_section_ctl
!!
!!    array  isosurface_ctl
!!      ....
!!    end array isosurface_ctl
!!
!!    array  volume_rendering
!!      ....
!!    end array volume_rendering
!!
!!    array  fieldline
!!      ....
!!    end array fieldline
!!
!!    array  LIC_rendering
!!      ....
!!    end array LIC_rendering
!!
!!    delta_t_sectioning_ctl   1.0e-3
!!    i_step_sectioning_ctl    400
!!    delta_t_isosurface_ctl   1.0e-3
!!    i_step_isosurface_ctl    400
!!    delta_t_pvr_ctl          1.0e-2
!!    i_step_pvr_ctl           400
!!    delta_t_fline_ctl        1.0e-1
!!    i_step_fline_ctl         400
!!    delta_t_LIC_ctl          1.0e-1
!!    i_step_LIC_ctl           400
!!    delta_t_field_ctl        1.0e-3
!!    i_step_field_ctl         800
!!    output_field_file_fmt_ctl   'VTK'
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_vizs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_data_pvrs
      use t_control_data_flines
      use t_control_data_LIC_pvrs
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!>        Structures of visualization controls
      type visualization_controls
!>        Structures of setioning controls
        type(section_controls) :: psf_ctls
!>        Structures of isosurface controls
        type(isosurf_controls) :: iso_ctls
!>        Structures of volume rendering controls
        type(volume_rendering_controls) :: pvr_ctls
!>        Structures of fieldline controls
        type(fieldline_controls) :: fline_ctls
!>        Structures of LIC rendering controls
        type(lic_rendering_controls) :: lic_ctls
!
!>   Increment for sectioning
        type(read_integer_item) :: i_step_psf_v_ctl
!>   Increment for isosurface
        type(read_integer_item) :: i_step_iso_v_ctl
!>   Increment for volume rendering
        type(read_integer_item) :: i_step_pvr_v_ctl
!>   Increment for LIC rendering
        type(read_integer_item) :: i_step_lic_v_ctl
!>   Increment for field line
        type(read_integer_item) :: i_step_fline_v_ctl
!>   Increment for field data output
        type(read_integer_item) :: i_step_ucd_v_ctl
!
!>   time interval for sectioning
        type(read_real_item) :: delta_t_psf_v_ctl
!>   time interval for isosurface
        type(read_real_item) :: delta_t_iso_v_ctl
!>   time interval for volume rendering
        type(read_real_item) :: delta_t_pvr_v_ctl
!>   time interval for LIC rendering
        type(read_real_item) :: delta_t_lic_v_ctl
!>   time interval for field line
        type(read_real_item) :: delta_t_fline_v_ctl
!>   time interval for field data output
        type(read_real_item) :: delta_t_ucd_v_ctl
!
!>   File format for field data output
        type(read_character_item) :: output_field_file_fmt_ctl
!
!
        integer (kind=kint) :: i_viz_control = 0
      end type visualization_controls
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_viz_controls(viz_ctls)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call bcast_files_4_psf_ctl(viz_ctls%psf_ctls)
      call bcast_files_4_iso_ctl(viz_ctls%iso_ctls)
      call bcast_files_4_pvr_ctl(viz_ctls%pvr_ctls)
      call bcast_files_4_fline_ctl(viz_ctls%fline_ctls)
      call bcast_files_4_lic_ctl(viz_ctls%lic_ctls)
!
      call bcast_ctl_type_r1(viz_ctls%delta_t_psf_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_iso_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_pvr_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_fline_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_lic_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_ucd_v_ctl)
!
      call bcast_ctl_type_i1(viz_ctls%i_step_psf_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_iso_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_pvr_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_lic_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_fline_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_ucd_v_ctl)
!
      call bcast_ctl_type_c1(viz_ctls%output_field_file_fmt_ctl)
!
      call calypso_mpi_bcast_one_int(viz_ctls%i_viz_control, 0)
!
      end subroutine bcast_viz_controls
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_viz_controls(viz_ctls)
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call dealloc_psf_ctl_stract(viz_ctls%psf_ctls)
      call dealloc_iso_ctl_stract(viz_ctls%iso_ctls)
      call dealloc_pvr_ctl_struct(viz_ctls%pvr_ctls)
      call dealloc_fline_fhead_ctl(viz_ctls%fline_ctls)
      call dealloc_lic_ctl_struct(viz_ctls%lic_ctls)
!
      viz_ctls%delta_t_psf_v_ctl%iflag =   0
      viz_ctls%delta_t_iso_v_ctl%iflag =   0
      viz_ctls%delta_t_pvr_v_ctl%iflag =   0
      viz_ctls%delta_t_fline_v_ctl%iflag = 0
      viz_ctls%delta_t_lic_v_ctl%iflag =   0
      viz_ctls%delta_t_ucd_v_ctl%iflag =   0
!
      viz_ctls%i_step_psf_v_ctl%iflag =   0
      viz_ctls%i_step_iso_v_ctl%iflag =   0
      viz_ctls%i_step_pvr_v_ctl%iflag =   0
      viz_ctls%i_step_lic_v_ctl%iflag =   0
      viz_ctls%i_step_fline_v_ctl%iflag = 0
      viz_ctls%i_step_ucd_v_ctl%iflag =   0
!
      viz_ctls%i_viz_control = 0
!
      end subroutine dealloc_viz_controls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_vizs_to_fld_ctl(viz_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(visualization_controls), intent(in) :: viz_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(viz_ctls%psf_ctls%num_psf_ctl .gt. 0) then
        call add_fields_4_psfs_to_fld_ctl(viz_ctls%psf_ctls, field_ctl)
      end if
!
      if(viz_ctls%iso_ctls%num_iso_ctl .gt. 0) then
        call add_fields_4_isos_to_fld_ctl(viz_ctls%iso_ctls, field_ctl)
      end if
!
!
      if(viz_ctls%pvr_ctls%num_pvr_ctl .gt. 0) then
        call add_fields_4_pvrs_to_fld_ctl(viz_ctls%pvr_ctls, field_ctl)
      end if
!
      if(viz_ctls%lic_ctls%num_lic_ctl .gt. 0) then
        call add_fields_4_lics_to_fld_ctl(viz_ctls%lic_ctls, field_ctl)
      end if
!
      if(viz_ctls%fline_ctls%num_fline_ctl .gt. 0) then
        call add_fields_4_flines_to_fld_ctl(viz_ctls%fline_ctls,        &
     &                                      field_ctl)
      end if
!
      end subroutine add_fields_4_vizs_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_vizs
