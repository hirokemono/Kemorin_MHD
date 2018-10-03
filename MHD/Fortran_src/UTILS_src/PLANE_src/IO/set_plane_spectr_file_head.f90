!set_plane_spectr_file_head.f90
!      module set_plane_spectr_file_head
!
!      Written by H. Matsui on Oct., 2008
!
!!      subroutine s_set_plane_spectr_file_head(mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
!
      module set_plane_spectr_file_head
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_plane_spectr_file_head(mesh_file)
!
      use m_control_plane_fft
      use set_spectr_file_name
      use set_parallel_file_name
      use set_control_platform_data
!
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_control_mesh_def(new_p_plt, mesh_file)
!
      if (new_p_plt%field_file_prefix%iflag .gt. 0) then
        plane_udt_header = new_p_plt%field_file_prefix%charavalue
      end if
!
!
      if (plane_spectr_mode_head_ctl%iflag .gt. 0) then
        spec_mode_file_name                                             &
     &      = add_dat_extension(plane_spectr_mode_head_ctl%charavalue)
      else
        spec_mode_file_name = spec_mode_def_name
      end if
!
      if (plane_spectr_data_head_ctl%iflag .gt. 0) then
        spec_header = plane_spectr_data_head_ctl%charavalue
      else
        spec_header = spec_def_header
      end if
!
      if (plane_spectr_ene_head_ctl%iflag .gt. 0) then
        ene_header = plane_spectr_ene_head_ctl%charavalue
      else
        ene_header = ene_spec_def_header
      end if
!
      if (plane_spectr_h_ene_head_ctl%iflag .gt. 0) then
        ene_h_header = plane_spectr_h_ene_head_ctl%charavalue
      else
        ene_h_header = ene_h_spec_def_header
      end if
!
      end subroutine s_set_plane_spectr_file_head
!
! -----------------------------------------------------------------------
!
      end module set_plane_spectr_file_head
