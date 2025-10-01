!>@file   input_control_sph_utils.f90
!!@brief  module input_control_sph_utils
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each isosurface
!!
!!@verbatim
!!      subroutine s_input_control_sph_utils                            &
!!     &         (ctl_file_name, spu_ctl, time_SHR, rj_fld, pwr)
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!!        type(time_step_param), intent(inout) :: time_SHR
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!@verbatim
      module input_control_sph_utils
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_step_parameter
      use t_ctl_data_4_sph_utils
      use t_phys_data
      use t_rms_4_sph_spectr
!
      implicit  none
!
      character (len = kchara), parameter, private                      &
     &        :: control_file_name='ctl_sph_transform'
!
      private :: bcast_control_data_sph_utils
      private :: bcast_sph_trans_model_ctl, bcast_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_input_control_sph_utils                              &
     &         (ctl_file_name, spu_ctl, time_SHR, rj_fld, pwr)
!
      use m_ctl_params_sph_utils
      use ctl_file_sph_utils_IO
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
      type(time_step_param), intent(inout) :: time_SHR
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      if(my_rank .eq. 0) then
       call read_control_data_sph_utils(ctl_file_name, spu_ctl)
      end if
      call bcast_control_data_sph_utils(spu_ctl)
!
      if(spu_ctl%i_sph_trans_ctl .ne. 1) then
        call calypso_MPI_abort(spu_ctl%i_sph_trans_ctl,                 &
     &                             'control file is broken')
      end if
!
      call set_ctl_data_4_sph_utils(spu_ctl, time_SHR, rj_fld, pwr)
!
      end subroutine s_input_control_sph_utils
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_control_data_sph_utils(spu_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call bcast_ctl_data_4_platform(spu_ctl%plt)
      call bcast_ctl_data_4_platform(spu_ctl%org_plt)
      call bcast_FEM_mesh_control(spu_ctl%Fmesh_ctl)
!
      call bcast_sph_monitoring_ctl(spu_ctl%smonitor_ctl)
!
      call bcast_sph_trans_model_ctl(spu_ctl)
      call bcast_sph_trans_params_ctl(spu_ctl)
!
      call calypso_mpi_bcast_one_int(spu_ctl%i_sph_trans_ctl, 0)
!
      end subroutine bcast_control_data_sph_utils
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_model_ctl(spu_ctl)
!
      use calypso_mpi_int
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call bcast_ctl_type_r1(spu_ctl%buoyancy_ratio_ctl)
      call bcast_ctl_type_r1(spu_ctl%thermal_buoyancy_ctl)
!
      call bcast_phys_data_ctl(spu_ctl%fld_ctl)
      call bcast_ctl_data_4_time_step(spu_ctl%tstep_ctl)
!
      call calypso_mpi_bcast_one_int(spu_ctl%i_sph_trans_model, 0)
!
      end subroutine bcast_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_params_ctl(spu_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
!
!
      call bcast_ctl_type_c1(spu_ctl%zm_spec_file_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%tave_ene_spec_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%ene_spec_head_ctl)
      call bcast_ctl_type_c1(spu_ctl%vol_ene_spec_head_ctl)
!
      call calypso_mpi_bcast_one_int(spu_ctl%i_sph_trans_params, 0)
!
      end subroutine bcast_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module input_control_sph_utils
