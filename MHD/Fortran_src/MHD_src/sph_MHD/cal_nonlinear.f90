!>@file   cal_nonlinear.f90
!!@brief  module cal_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear                                            &
!!     &         (reftemp_rj, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, rj_fld)
!!      subroutine licv_exp(reftemp_rj,                                 &
!!     &          sph_rlm, sph_rj, comm_rlm, comm_rj, rj_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module cal_nonlinear
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_control_parameter
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_data
!
      implicit none
!
      private :: sum_forces_by_explicit, nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear                                              &
     &         (reftemp_rj, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, rj_fld)
!
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
!
      use m_work_time
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
      real(kind = kreal), intent(in)                                    &
     &      :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph(sph_rtp, sph_rtm, sph_rlm, sph_rj,   &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, rj_fld)
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        call add_reftemp_advect_sph_MHD                                 &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, reftemp_rj, rj_fld%d_fld)
      end if
!
!*  ----  copy coriolis term for inner core rotation
!*
      call start_eleps_time(13)
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call copy_icore_rot_to_tor_coriolis(sph_bc_U%kr_in,             &
     &     sph_rj%idx_rj_degree_one, sph_rj%nnod_rj, sph_rj%nidx_rj(2), &
     &     rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      call end_eleps_time(13)
!
      call sum_forces_by_explicit(sph_rj, rj_fld)
!
      end subroutine nonlinear
!*
!*   ------------------------------------------------------------------
!
      subroutine sum_forces_by_explicit(sph_rj, rj_fld)
!
      use cal_vorticity_terms_adams
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(      iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .ne. id_turn_OFF) then
        call set_MHD_terms_to_force(itor%i_rot_buoyancy,                &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_force(itor%i_rot_comp_buo,                &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_rot_cv_terms_to_force(itor%i_rot_buoyancy,             &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .eq.     id_turn_OFF) then
        call set_rot_cv_terms_to_force(itor%i_rot_comp_buo,             &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call set_rot_advection_to_force                                 &
     &     (sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        if(iflag_4_coriolis .ne. id_turn_OFF) then
          call add_coriolis_to_vort_force                               &
     &       (sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(iflag_4_lorentz .ne.  id_turn_OFF) then
          call add_lorentz_to_vort_force                                &
     &       (sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(iflag_4_gravity .ne.  id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_buoyancy,          &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        else if(iflag_4_composit_buo .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_comp_buo,          &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        else if(iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_filter_buo,        &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      end if
!$omp end parallel
!
      end subroutine sum_forces_by_explicit
!
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph                                &
     &         (sph_rtp, sph_rtm, sph_rlm, sph_rj,                      &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_momentum_eq_explicit
!
      use m_work_time
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      call start_eleps_time(14)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD(sph_rtp, sph_rtm, sph_rlm,              &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, rj_fld)
      call end_eleps_time(14)
!
      call start_eleps_time(15)
      if (iflag_debug.ge.1) write(*,*) 'nonlinear_terms_in_rtp'
      call nonlinear_terms_in_rtp(sph_rtp)
      call end_eleps_time(15)
!
      call start_eleps_time(16)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD(sph_rtp, sph_rtm, sph_rlm,           &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, rj_fld)
      call end_eleps_time(16)
!
      call start_eleps_time(17)
      if (iflag_debug.ge.1) write(*,*) 'cal_momentum_eq_exp_sph'
      call cal_momentum_eq_exp_sph(sph_rj, rj_fld)
      call end_eleps_time(17)
!
      end subroutine nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_exp(reftemp_rj,                                   &
     &          sph_rlm, sph_rj, comm_rlm, comm_rj, rj_fld)
!
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_vorticity_terms_adams
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
      real(kind = kreal), intent(in)                                    &
     &                   :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
      type(phys_data), intent(inout) :: rj_fld
!
!*  ----  copy velocity for coriolis term ------------------
!*
      if (iflag_debug.eq.1) write(*,*) 'sph_transform_4_licv'
      if(iflag_4_coriolis .ne. id_turn_OFF) then
        call sph_transform_4_licv                                       &
     &     (sph_rlm, comm_rlm, comm_rj, rj_fld)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
!
!$omp parallel workshare
      rj_fld%d_fld(1:sph_rj%nnod_rj,ipol%i_h_advect) = zero
!$omp end parallel workshare
!
      if(ipol%i_forces .gt. 0) then
!$omp parallel workshare
        rj_fld%d_fld(1:sph_rj%nnod_rj,ipol%i_forces) = zero
        rj_fld%d_fld(1:sph_rj%nnod_rj,itor%i_forces) = zero
!$omp end parallel workshare
      end if
!
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        call add_reftemp_advect_sph_MHD                                 &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, reftemp_rj, rj_fld%d_fld)
      end if
!
!$omp parallel
      if(iflag_4_coriolis .ne. id_turn_OFF) then
        call add_coriolis_to_vort_force                                 &
     &     (sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(iflag_4_gravity .ne.  id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor%i_rot_buoyancy,            &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(iflag_4_composit_buo .ne. id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor%i_rot_comp_buo,            &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
!
      end subroutine licv_exp
!*
!*   ------------------------------------------------------------------
!
      end module cal_nonlinear
