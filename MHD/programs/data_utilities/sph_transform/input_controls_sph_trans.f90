!>@file   input_controls_sph_trans.f90
!!@brief  module input_controls_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Control data of node monitoring
!!
!!@verbatim
!!      subroutine load_control_data_sph_trans(spt_ctl)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!
!!      subroutine input_control_sph_fwd_trans(ctl_file_name,           &
!!     &          spt_ctl, time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!!      subroutine s_input_control_sph_trans(ctl_file_name,             &
!!     &          spt_ctl, time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!!      subroutine input_control_zm_trans(ctl_file_name, spt_ctl,       &
!!     &          time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!!      subroutine input_control_pick_zm(ctl_file_name, spt_ctl,        &
!!     &          time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!        type(time_step_param), intent(inout) :: time_STR
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!!        type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!!@endverbatim
!
      module input_controls_sph_trans
!
      use m_precision
      use calypso_mpi
!
      use t_ctl_params_sph_trans
      use t_ctl_data_4_sph_trans
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_control_data_sph_trans(ctl_file_name, spt_ctl)
!
      use ctl_data_sph_trans_IO
      use bcast_ctl_data_4_sph_trans
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_data_sph_trans(ctl_file_name, spt_ctl)
      end if
!
      call bcast_sph_trans_control_data(spt_ctl)
!
      if(spt_ctl%i_sph_trans_ctl .ne. 1) then
        call calypso_MPI_abort(spt_ctl%i_sph_trans_ctl,                 &
     &                             'control file is broken')
      end if
!
      end subroutine load_control_data_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine input_control_sph_fwd_trans(ctl_file_name,             &
     &          spt_ctl, time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
!
      if (iflag_debug.gt.0) write(*,*) 'load_control_data_sph_trans'
      call load_control_data_sph_trans(ctl_file_name, spt_ctl)
!
      call set_control_sph_fwd_trans(spt_ctl, time_STR, SPH_TRNS,       &
     &                               FEM_STR, SPH_STR)
!
      end subroutine input_control_sph_fwd_trans
!
! -----------------------------------------------------------------------
!
      subroutine s_input_control_sph_trans(ctl_file_name,               &
     &          spt_ctl, time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
!
      if (iflag_debug.gt.0) write(*,*) 'load_control_data_sph_trans'
      call load_control_data_sph_trans(ctl_file_name, spt_ctl)
!
      call s_set_ctl_data_4_sph_trans(spt_ctl, time_STR, SPH_TRNS,      &
     &                                FEM_STR, SPH_STR)
!
      end subroutine s_input_control_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_control_zm_trans(ctl_file_name, spt_ctl,         &
     &          time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
!
      if (iflag_debug.gt.0) write(*,*) 'load_control_data_sph_trans'
      call load_control_data_sph_trans(ctl_file_name, spt_ctl)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(spt_ctl, time_STR, SPH_TRNS,      &
     &                                FEM_STR, SPH_STR)
      call set_ctl_data_4_zm_trans(spt_ctl, SPH_STR)
!
      end subroutine input_control_zm_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_control_pick_zm(ctl_file_name, spt_ctl,          &
     &          time_STR, SPH_TRNS, FEM_STR, SPH_STR)
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
!
      if (iflag_debug.gt.0) write(*,*) 'load_control_data_sph_trans'
      call load_control_data_sph_trans(ctl_file_name, spt_ctl)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(spt_ctl, time_STR, SPH_TRNS,      &
     &                                FEM_STR, SPH_STR)
      call set_ctl_data_4_pick_zm(spt_ctl, FEM_STR)
!
      end subroutine input_control_pick_zm
!
! -----------------------------------------------------------------------
!
      end module input_controls_sph_trans
