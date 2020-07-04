!>@file   t_ctl_data_sph_MHD_psf.f90
!!@brief  module t_ctl_data_sph_MHD_psf
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_control_4_sph_MHD_w_psf(file_name, DMHD_ctl)
!!      subroutine read_control_4_sph_MHD_noviz(file_name, DMHD_ctl)
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!@endverbatim
!
      module t_ctl_data_sph_MHD_psf
!
      use m_precision
!
      use t_ctl_data_MHD
      use m_machine_parameter
      use t_read_control_elements
      use calypso_mpi
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: ctl_file_code = 11
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
!
      private :: hd_mhd_ctl
      private :: ctl_file_code
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_w_psf(file_name, DMHD_ctl)
!
      use t_control_data_surfacings
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(ctl_file_code, file = file_name, status='old' )
!
        do
          call load_one_line_from_control(ctl_file_code, c_buf1)
          call read_sph_mhd_ctl_w_psf                                   &
     &       (ctl_file_code, hd_mhd_ctl, DMHD_ctl, c_buf1)
          if(DMHD_ctl%i_mhd_ctl .gt. 0) exit
        end do
        close(ctl_file_code)
!
        call section_step_ctls_to_time_ctl                              &
     &     (DMHD_ctl%surfacing_ctls, DMHD_ctl%smctl_ctl%tctl)
      end if
!
      call bcast_sph_mhd_ctl_w_psf(DMHD_ctl)
!
      end subroutine read_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_noviz(file_name, DMHD_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(ctl_file_code, file = file_name, status='old' )
!
        do
          call load_one_line_from_control(ctl_file_code, c_buf1)
          call read_sph_mhd_ctl_noviz                                   &
     &       (ctl_file_code, hd_mhd_ctl, DMHD_ctl, c_buf1)
          if(DMHD_ctl%i_mhd_ctl .gt. 0) exit
        end do
        close(ctl_file_code)
      end if
!
      call bcast_sph_mhd_ctl_data(DMHD_ctl)
!
      end subroutine read_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_sph_MHD_psf
