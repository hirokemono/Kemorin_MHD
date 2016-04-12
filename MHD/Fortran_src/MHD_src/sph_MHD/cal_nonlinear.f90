!>@file   cal_nonlinear.f90
!!@brief  module cal_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear
!!      subroutine licv_exp
!!@endverbatim
!
!
      module cal_nonlinear
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
!
      implicit none
!
      private :: nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear
!
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
!
      use m_work_time
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        call add_reftemp_advect_sph_MHD                                 &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out)
      end if
!
!*  ----  copy coriolis term for inner core rotation
!*
      call start_eleps_time(13)
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call copy_icore_rot_to_tor_coriolis(sph_bc_U%kr_in)
      end if
      call end_eleps_time(13)
!
      call sum_forces_by_explicit
!
      end subroutine nonlinear
!*
!*   ------------------------------------------------------------------
!
      subroutine sum_forces_by_explicit
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use cal_vorticity_terms_adams
!
!
!$omp parallel
      if(      iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .ne. id_turn_OFF) then
        call set_MHD_terms_to_force(itor%i_rot_buoyancy,                &
     &      nnod_rj, ntot_phys_rj, d_rj)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_force(itor%i_rot_comp_buo,                &
     &      nnod_rj, ntot_phys_rj, d_rj)
      else if( iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_rot_cv_terms_to_force(itor%i_rot_buoyancy,             &
     &      nnod_rj, ntot_phys_rj, d_rj)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .eq.     id_turn_OFF) then
        call set_rot_cv_terms_to_force(itor%i_rot_comp_buo,             &
     &      nnod_rj, ntot_phys_rj, d_rj)
      else
        call set_rot_advection_to_force(nnod_rj, ntot_phys_rj, d_rj)
        if(iflag_4_coriolis .ne. id_turn_OFF) then
          call add_coriolis_to_vort_force(nnod_rj, ntot_phys_rj, d_rj)
        end if
        if(iflag_4_lorentz .ne.  id_turn_OFF) then
          call add_lorentz_to_vort_force(nnod_rj, ntot_phys_rj, d_rj)
        end if
        if(iflag_4_gravity .ne.  id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_buoyancy,          &
     &        nnod_rj, ntot_phys_rj, d_rj)
        else if(iflag_4_composit_buo .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_comp_buo,          &
     &        nnod_rj, ntot_phys_rj, d_rj)
        else if(iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_filter_buo,        &
     &        nnod_rj, ntot_phys_rj, d_rj)
        end if
      end if
!$omp end parallel
!
      end subroutine sum_forces_by_explicit
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_momentum_eq_explicit
!
      use m_work_time
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      call start_eleps_time(14)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD
      call end_eleps_time(14)
!
      call start_eleps_time(15)
      if (iflag_debug.ge.1) write(*,*) 's_cal_nonlinear_sph_MHD'
      call s_cal_nonlinear_sph_MHD
      call end_eleps_time(15)
!
      call start_eleps_time(16)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD
      call end_eleps_time(16)
!
      call start_eleps_time(17)
      if (iflag_debug.ge.1) write(*,*) 'cal_momentum_eq_exp_sph'
      call cal_momentum_eq_exp_sph
      call end_eleps_time(17)
!
      end subroutine nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_exp
!
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_vorticity_terms_adams
!
!*  ----  copy velocity for coriolis term ------------------
!*
      if (iflag_debug.eq.1) write(*,*) 'sph_transform_4_licv'
      if(iflag_4_coriolis .ne. id_turn_OFF) call sph_transform_4_licv
!
!   ----  lead nonlinear terms by phesdo spectrum
!
!$omp parallel workshare
      d_rj(1:nnod_rj,ipol%i_h_advect) = zero
!$omp end parallel workshare
!
      if(ipol%i_forces .gt. 0) then
!$omp parallel workshare
        d_rj(1:nnod_rj,ipol%i_forces) = zero
        d_rj(1:nnod_rj,itor%i_forces) = zero
!$omp end parallel workshare
      end if
!
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        call add_reftemp_advect_sph_MHD                                 &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out)
      end if
!
!$omp parallel
      if(iflag_4_coriolis .ne. id_turn_OFF) then
        call add_coriolis_to_vort_force(nnod_rj, ntot_phys_rj, d_rj)
      end if
      if(iflag_4_gravity .ne.  id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor%i_rot_buoyancy,            &
     &      nnod_rj, ntot_phys_rj, d_rj)
      else if(iflag_4_composit_buo .ne. id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor%i_rot_comp_buo,            &
     &      nnod_rj, ntot_phys_rj, d_rj)
      end if
!$omp end parallel
!
!
      end subroutine licv_exp
!*
!*   ------------------------------------------------------------------
!
      end module cal_nonlinear
