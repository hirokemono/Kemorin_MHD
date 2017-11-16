!
!      module m_control_data_sections
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_psf_ctl_stract
!      subroutine allocate_iso_ctl_stract
!
!      subroutine deallocate_psf_ctl_stract
!      subroutine deallocate_iso_ctl_stract
!
!      subroutine read_sections_control_data
!
!      subroutine read_files_4_psf_ctl
!      subroutine read_files_4_iso_ctl
!
!      subroutine bcast_files_4_psf_ctl
!      subroutine bcast_files_4_iso_ctl
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_sections
!
      implicit  none
!
!
      type(section_controls), save :: psf_ctls1
      type(isosurf_controls), save :: iso_ctls1
!
      end module m_control_data_sections
