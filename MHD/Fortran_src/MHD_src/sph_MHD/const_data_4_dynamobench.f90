!>@file   const_data_4_dynamobench.f90
!!@brief  module const_data_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine s_const_data_4_dynamobench                           &
!!     &         (sph_params, sph_rj, rj_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module const_data_4_dynamobench
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_data_4_dynamobench                             &
     &         (sph_params, sph_rj, rj_fld)
!
      use m_boundary_params_sph_MHD
      use m_field_at_mid_equator
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
!
      use calypso_mpi
      use cal_rms_fields_by_sph
      use global_field_4_dynamobench
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
!
      if(iflag_debug.gt.0)  write(*,*) 'mid_eq_transfer_dynamobench'
      call mid_eq_transfer_dynamobench(rj_fld)
!
      call cal_mean_squre_in_shell                                      &
     &   (sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_params%l_truncation, sph_rj, rj_fld)
      if(my_rank .eq. 0) call copy_energy_4_dynamobench
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call pick_inner_core_rotation(sph_rj%idx_rj_degree_one,         &
     &      sph_rj%nidx_rj, sph_params%nlayer_ICB, sph_rj%ar_1d_rj,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_mean_squre_in_shell                                    &
     &     (izero, sph_params%nlayer_ICB, sph_params%l_truncation,      &
     &      sph_rj, rj_fld)
        if(my_rank .eq. 0) call copy_icore_energy_4_dbench
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &   .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call pick_mag_torque_inner_core                                 &
     &     (sph_rj%idx_rj_degree_one,  sph_rj%nidx_rj,                  &
     &      sph_params%nlayer_ICB, sph_rj%radius_1d_rj_r,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine s_const_data_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module const_data_4_dynamobench
