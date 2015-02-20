!set_plane_spectr_file_head.f90
!      module set_plane_spectr_file_head
!
      module set_plane_spectr_file_head
!
!      Written by H. Matsui on Oct., 2008
!
      use m_precision
!
      implicit none
!
!      subroutine s_set_plane_spectr_file_head
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_plane_spectr_file_head
!
      use m_ctl_data_4_platforms
      use m_ctl_data_plane_spec_file
      use set_spectr_file_name
      use set_parallel_file_name
      use m_read_mesh_data
      use set_control_platform_data
!
!
      call set_control_mesh_def
!
      if (udt_file_head_ctl%iflag .gt. 0) then
        plane_udt_header = udt_file_head_ctl%charavalue
      end if
!
!
      if (i_plane_spec_mode_head .gt. 0) then
        call add_dat_extension(plane_spectr_mode_head_ctl,              &
     &                      spec_mode_file_name)
      else
        spec_mode_file_name = spec_mode_def_name
      end if
!
      if (i_plane_spec_data_head .gt. 0) then
        spec_header = plane_spectr_data_head_ctl
      else
        spec_header = spec_def_header
      end if
!
      if (i_plane_spec_ene_head .gt. 0) then
        ene_header = plane_spectr_ene_head_ctl
      else
        ene_header = ene_spec_def_header
      end if
!
      if (i_plane_sp_h_ene_head .gt. 0) then
        ene_h_header = plane_spectr_h_ene_head_ctl
      else
        ene_h_header = ene_h_spec_def_header
      end if
!
      end subroutine s_set_plane_spectr_file_head
!
! -----------------------------------------------------------------------
!
      end module set_plane_spectr_file_head
