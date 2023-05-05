!>@file   ctl_file_SGS_MHD_IO.f90
!!@brief  module ctl_file_SGS_MHD_IO
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!@n        Modified by H. Matsui on Apr., 2023
!!
!!@verbatim
!!      subroutine read_control_4_sph_SGS_MHD(file_name, MHD_ctl,       &
!!     &          sgs_ctl, viz_ctls, zm_ctls)
!!        character(len=kchara), intent(in) :: file_name
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!      subroutine write_control_file_sph_SGS_MHD(file_name, MHD_ctl,   &
!!     &          sgs_ctl, viz_ctls, zm_ctls)
!!        character(len=kchara), intent(in) :: file_name
!!        type(mhd_simulation_control), intent(in) :: MHD_ctl
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        type(visualization_controls), intent(in) :: viz_ctls
!!        type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!!@endverbatim
!
      module ctl_file_SGS_MHD_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_ctl_data_SGS_model
      use t_control_data_vizs
      use t_control_data_dynamo_vizs
!
      implicit none
!
!
      integer(kind=kint), parameter, private :: ctl_file_code = 11
!
!   Top level of label
      character(len=kchara), parameter, private                         &
     &                    :: hd_mhd_ctl = 'MHD_control'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_SGS_MHD(file_name, MHD_ctl,         &
     &          sgs_ctl, viz_ctls, zm_ctls)
!
      use t_ctl_data_SPH_MHD_control
      use viz_step_ctls_to_time_ctl
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
      type(buffer_for_control) :: c_buf1
!
!
      open(ctl_file_code, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
        call read_sph_mhd_control_data(ctl_file_code, hd_mhd_ctl,       &
     &      MHD_ctl, sgs_ctl, viz_ctls, zm_ctls, c_buf1)
        if(MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(ctl_file_code)
!
      call s_viz_step_ctls_to_time_ctl                                  &
     &   (viz_ctls, MHD_ctl%smctl_ctl%tctl)
      call add_fields_4_vizs_to_fld_ctl                                 &
     &   (viz_ctls, MHD_ctl%model_ctl%fld_ctl%field_ctl)
!
      end subroutine read_control_4_sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine write_control_file_sph_SGS_MHD(file_name, MHD_ctl,     &
     &          sgs_ctl, viz_ctls, zm_ctls)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(in) :: MHD_ctl
      type(SGS_model_control), intent(in) :: sgs_ctl
      type(visualization_controls), intent(in) :: viz_ctls
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
      level1 = 0
      open(ctl_file_code, file = file_name)
      call write_sph_mhd_control_data(ctl_file_code, hd_mhd_ctl,        &
     &    MHD_ctl, sgs_ctl, viz_ctls, zm_ctls, level1)
      close(ctl_file_code)
!
      end subroutine write_control_file_sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      end module ctl_file_SGS_MHD_IO
