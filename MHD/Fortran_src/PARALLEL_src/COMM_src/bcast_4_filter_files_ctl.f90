!>@file   bcast_4_filter_files_ctl.f90
!!@brief  module bcast_4_filter_files_ctl
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief  Structure for reading parameters for filtering files
!!
!!@verbatim
!!      subroutine bcast_filter_fnames_control(ffile_ctl)
!!        type(filter_file_control), intent(inout) :: ffile_ctl
!!      subroutine bcast_ele_layers_control(elayer_ctl)
!!        type(layering_control), intent(inout) :: elayer_ctl
!!@endverbatim
!
      module bcast_4_filter_files_ctl
!
      use m_precision
      use t_ctl_data_filter_files
      use t_ctl_data_ele_layering
!
      implicit  none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_filter_fnames_control(ffile_ctl)
!
      use bcast_control_arrays
!
      type(filter_file_control), intent(inout) :: ffile_ctl
!
!
      call bcast_ctl_type_c1(ffile_ctl%filter_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_coef_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_elen_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_moms_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_wide_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%model_coef_ini_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%commute_coef_ini_head_ctl)
!
      call bcast_ctl_type_c1(ffile_ctl%filter_elen_format)
      call bcast_ctl_type_c1(ffile_ctl%filter_3d_format)
      call bcast_ctl_type_c1(ffile_ctl%filter_wide_format)
      call bcast_ctl_type_c1(ffile_ctl%model_coef_rst_format)
      call bcast_ctl_type_c1(ffile_ctl%commute_coef_rst_format)
!
      call MPI_BCAST(ffile_ctl%i_filter_fnames, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_filter_fnames_control
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_ele_layers_control(elayer_ctl)
!
      use bcast_control_arrays
!
      type(layering_control), intent(inout) :: elayer_ctl
!
!
      call bcast_ctl_array_c1(elayer_ctl%layer_grp_name_ctl)
      call bcast_ctl_array_i1(elayer_ctl%igrp_stack_layer_ctl)
!
!
      call bcast_ctl_type_i1(elayer_ctl%num_layering_grp_ctl)
      call bcast_ctl_type_i1(elayer_ctl%num_fl_layer_grp_ctl)
!
      call bcast_ctl_type_i1(elayer_ctl%ngrp_SGS_on_sphere_ctl)
!
!
      call bcast_ctl_type_c1(elayer_ctl%layering_grp_type_ctl)
      call bcast_ctl_type_c1(elayer_ctl%start_layering_grp_name_ctl)
      call bcast_ctl_type_c1(elayer_ctl%start_fl_layer_grp_name_ctl)
!
      call MPI_BCAST(elayer_ctl%i_dynamic_layers, 1,                    &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ele_layers_control
!
!   --------------------------------------------------------------------
!
      end module bcast_4_filter_files_ctl
