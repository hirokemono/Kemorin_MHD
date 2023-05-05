!>@file   ctl_file_sph_MHD_IO.f90
!!@brief  module ctl_file_sph_MHD_IO
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
!!      subroutine read_control_4_sph_MHD_w_psf                         &
!!     &         (file_name, MHD_ctl, surfacing_ctls, zm_ctls)
!!      subroutine read_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(surfacing_controls), intent(inout) :: surfacing_ctls
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!      subroutine write_control_4_sph_MHD_w_psf(file_name,             &
!!     &          MHD_ctl, surfacing_ctls, zm_ctls)
!!      subroutine write_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(mhd_simulation_control), intent(in) :: MHD_ctl
!!        type(surfacing_controls), intent(in) :: surfacing_ctls
!!        type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!!@endverbatim
!
      module ctl_file_sph_MHD_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_MHD
      use t_read_control_elements
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
      subroutine read_control_4_sph_MHD_w_psf                           &
     &         (file_name, MHD_ctl, surfacing_ctls, zm_ctls)
!
      use t_control_data_surfacings
      use t_control_data_dynamo_vizs
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(surfacing_controls), intent(inout) :: surfacing_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
      type(buffer_for_control) :: c_buf1
!
!
      open(ctl_file_code, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
        call read_sph_mhd_ctl_w_psf(ctl_file_code, hd_mhd_ctl,          &
     &      MHD_ctl, surfacing_ctls, zm_ctls, c_buf1)
        if(MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(ctl_file_code)
!
      call section_step_ctls_to_time_ctl(surfacing_ctls,                &
     &                                   MHD_ctl%smctl_ctl%tctl)
      call add_fields_4_scts_to_fld_ctl(surfacing_ctls,                 &
     &    MHD_ctl%model_ctl%fld_ctl%field_ctl)
!
      end subroutine read_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      open(ctl_file_code, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
        call read_sph_mhd_ctl_noviz                                     &
     &     (ctl_file_code, hd_mhd_ctl, MHD_ctl, c_buf1)
        if(MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(ctl_file_code)
!
      end subroutine read_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_control_4_sph_MHD_w_psf(file_name,               &
     &          MHD_ctl, surfacing_ctls, zm_ctls)
!
      use t_control_data_surfacings
      use t_control_data_dynamo_vizs
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(in) :: MHD_ctl
      type(surfacing_controls), intent(in) :: surfacing_ctls
      type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write MHD control file: ', trim(file_name)
      open(ctl_file_code, file = file_name)
      level1 = 0
      call write_sph_mhd_ctl_w_psf(ctl_file_code, hd_mhd_ctl,           &
     &    MHD_ctl, surfacing_ctls, zm_ctls, level1)
      close(ctl_file_code)
!
      end subroutine write_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine write_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(in) :: MHD_ctl
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write MHD control file: ', trim(file_name)
      open(ctl_file_code, file = file_name)
      level1 = 0
      call write_sph_mhd_ctl_noviz                                      &
     &   (ctl_file_code, hd_mhd_ctl, MHD_ctl, level1)
      close(ctl_file_code)
!
      end subroutine write_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      end module ctl_file_sph_MHD_IO
