!>@file   m_field_at_mid_equator.f90
!!@brief  module m_field_at_mid_equator
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  data at mid-depth of the shell at equator for dynamo benchmark
!!
!!@verbatim
!!      subroutine deallocate_mid_eq_field
!!      subroutine set_mid_equator_point_global
!!      subroutine mid_eq_transfer_dynamobench
!!@endverbatim
!
      module m_field_at_mid_equator
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_field_4_dynamobench
!
      implicit none
!
!>      radius at mid depth of fluid shell
      real(kind = kreal) :: r_MID
!
!>      Inner closest point of mid depth of fluid shell
      integer(kind = kint) :: kr_gl_rmid_in
!>      Outer closest point of mid depth of fluid shell
      integer(kind = kint) :: kr_gl_rmid_out
!>      Inner closest radius of mid depth of fluid shell
      real(kind = kreal) :: coef_gl_rmid_in
!>      Outer closest radius of mid depth of fluid shell
      real(kind = kreal) :: coef_gl_rmid_out
!
!>      Spectr data for mid depth for each domain
      real(kind = kreal), allocatable :: d_rj_mid_lc(:,:)
!>      Spectr data for mid depth collected to 0 process
      real(kind = kreal), allocatable :: d_rj_mid_eq(:,:)
!
!>      Number of gird points at mid equator
      integer(kind = kint) :: mphi_mid_eq
!>      Field data for mid depth at equator
      real(kind = kreal), allocatable :: d_rtp_eq_mid(:,:)
!
      private :: r_MID
      private :: kr_gl_rmid_in, kr_gl_rmid_out
      private :: coef_gl_rmid_in, coef_gl_rmid_out
      private :: d_rj_mid_lc, d_rj_mid_eq, d_rtp_eq_mid
!
      private :: allocate_mid_eq_field
      private :: collect_mid_eq_spectr, cal_field_4_dynamobench
      private :: cal_drift_by_v44
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_mid_eq_field
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      if(mphi_mid_eq .le. izero) mphi_mid_eq = nidx_rtp(3)
!
      num = nidx_global_rj(2)
      allocate( d_rj_mid_lc(0:num,7) )
      d_rj_mid_lc = 0.0d0
!
      if(my_rank .eq. 0) then
        num = nidx_global_rj(2)
        allocate( d_rj_mid_eq(0:num,7) )
!
        allocate( d_rtp_eq_mid(mphi_mid_eq,7) )
!
        d_rj_mid_eq = 0.0d0
        d_rtp_eq_mid = 0.0d0
      end if
!
      end subroutine allocate_mid_eq_field
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_mid_eq_field
!
!
      deallocate(d_rj_mid_lc)
      if(my_rank .eq. 0) then
        deallocate(d_rtp_eq_mid, d_rj_mid_eq)
      end if
!
      end subroutine deallocate_mid_eq_field
!
! ----------------------------------------------------------------------
!
      subroutine set_mid_equator_point_global
!
      use mid_eq_transform_single
!
      integer(kind = kint) :: kr
!
!
      r_MID = half * (radius_1d_rj_r(nlayer_ICB)                        &
     &              + radius_1d_rj_r(nlayer_CMB) )
!
      call allocate_mid_eq_field
      call initialize_mid_eq_transform(l_truncation,                    &
     &    mphi_mid_eq, r_MID)
!
      kr_gl_rmid_in =  izero
      kr_gl_rmid_out = izero
      do kr = 1, nidx_rj(1) - 1
        if(radius_1d_rj_r(kr) .eq. r_MID) then
          kr_gl_rmid_in =  kr
          kr_gl_rmid_out = izero
          coef_gl_rmid_in =  one
          coef_gl_rmid_out = zero
          exit
        end if
        if(radius_1d_rj_r(kr) .lt. r_MID                                &
     &      .and. radius_1d_rj_r(kr+1) .gt. r_MID) then
          kr_gl_rmid_in =  kr
          kr_gl_rmid_out = kr + 1
          coef_gl_rmid_in =  (radius_1d_rj_r(kr+1) - r_MID)             &
     &                      / dr_1d_rj(kr,0)
          coef_gl_rmid_out = one - coef_gl_rmid_in
          exit
        end if
      end do
!
      end subroutine set_mid_equator_point_global
!
! ----------------------------------------------------------------------
!
      subroutine mid_eq_transfer_dynamobench
!
      use m_parallel_var_dof
      use m_sph_phys_address
!
      use mid_eq_transform_single
!
!
      call collect_mid_eq_spectr
!
!    spherical transfer
!
      if(my_rank .gt. 0) return
!
      call equator_transfer_scalar(nidx_global_rj(2), mphi_mid_eq,      &
     &    d_rj_mid_eq(0,ibench_temp), d_rtp_eq_mid(1,ibench_temp))
      call equator_transfer_vector(nidx_global_rj(2), mphi_mid_eq,      &
     &    d_rj_mid_eq(0,ibench_velo), d_rtp_eq_mid(1,ibench_velo))
!
      if(ipol%i_magne .gt. 0) then
        call equator_transfer_vector(nidx_global_rj(2), mphi_mid_eq,    &
     &    d_rj_mid_eq(0,ibench_magne), d_rtp_eq_mid(1,ibench_magne))
      end if
!
!   Evaluate drift frequencty by velocity 
!
      call cal_drift_by_v44
!
!   find local point for dynamobench
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_field_4_dynamobench'
      call cal_field_4_dynamobench
!
      end subroutine mid_eq_transfer_dynamobench
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine collect_mid_eq_spectr
!
      use m_parallel_var_dof
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: j, j_gl, i_in, i_ot, num
!
!
!    pickup spectrum for mid depth
!
      do j = 1, nidx_rj(2)
        j_gl = idx_gl_1d_rj_j(j,1)
        i_in = j + (kr_gl_rmid_in-1) *  nidx_rj(2)
        i_ot = j + (kr_gl_rmid_out-1) * nidx_rj(2)
!
        d_rj_mid_lc(j_gl,ibench_temp)                                   &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_temp)              &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_temp)
!
        d_rj_mid_lc(j_gl,ibench_velo)                                   &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_velo)              &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_velo)
        d_rj_mid_lc(j_gl,ibench_velo+1)                                 &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_velo+1)            &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_velo+1)
        d_rj_mid_lc(j_gl,ibench_velo+2)                                 &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_velo+2)            &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_velo+2)
      end do
!
      num = 4 * (nidx_global_rj(2) + 1)
!
      if(ipol%i_magne .gt. 0) then
!
        do j = 1, nidx_rj(2)
          j_gl = idx_gl_1d_rj_j(j,1)
          i_in = j + (kr_gl_rmid_in-1) *  nidx_rj(2)
          i_ot = j + (kr_gl_rmid_out-1) * nidx_rj(2)
!
          d_rj_mid_lc(j_gl,ibench_magne)                                &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_magne)             &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_magne)
          d_rj_mid_lc(j_gl,ibench_magne+1)                              &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_magne+1)           &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_magne+1)
          d_rj_mid_lc(j_gl,ibench_magne+2)                              &
     &          = coef_gl_rmid_in * d_rj(i_in,ipol%i_magne+2)           &
     &           + coef_gl_rmid_out * d_rj(i_ot,ipol%i_magne+2)
        end do
!
        num = 7 * (nidx_global_rj(2) + 1)
      end if
!
!    collect data to rank 0
!
      if(my_rank .eq. 0) d_rj_mid_eq =   zero
      call MPI_Reduce(d_rj_mid_lc(0,1), d_rj_mid_eq(0,1), num,          &
     &    MPI_DOUBLE_PRECISION, MPI_SUM, izero, SOLVER_COMM, ierr)
!
      end subroutine collect_mid_eq_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_drift_by_v44
!
      use m_t_step_parameter
!
      integer(kind = kint) :: j4c, j4s
      real(kind = kreal) :: vp44c, vp44s
      real(kind = kreal) :: vt54c, vt54s
!
!
      if(time .eq. t_prev) return
!
      j4c = ifour*(ifour+1) + ifour
      j4s = ifour*(ifour+1) - ifour
      vp44c = d_rj_mid_eq(j4c,ibench_velo  )
      vp44s = d_rj_mid_eq(j4s,ibench_velo  )
!
      j4c = ifive*(ifive+1) + ifour
      j4s = ifive*(ifive+1) - ifour
      vt54c = d_rj_mid_eq(j4c,ibench_velo+2)
      vt54s = d_rj_mid_eq(j4s,ibench_velo+2)
!
      phase_vm4(1) = atan2(vp44s,vp44c)
      phase_vm4(2) = atan2(vt54s,vt54c)
!
      omega_vm4(1:2) = quad * (phase_vm4(1:2) - phase_vm4_prev(1:2))    &
     &                / (time - t_prev)
!
      t_prev = time
      phase_vm4_prev(1:2) = phase_vm4(1:2)
!
      end subroutine cal_drift_by_v44
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_4_dynamobench
!
      use m_parallel_var_dof
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: nd, mphi, mp_next, icou
      real(kind = kreal) :: coef
!
!
      icou = 0
      do mphi = 1, mphi_mid_eq
        mp_next = mod(mphi,mphi_mid_eq) + 1
        if(      d_rtp_eq_mid(mphi,ibench_velo)  .le.  zero             &
     &     .and. d_rtp_eq_mid(mp_next,ibench_velo) .gt. zero) then
          icou = icou + 1
          coef = d_rtp_eq_mid(mp_next,ibench_velo)                      &
     &          / (d_rtp_eq_mid(mp_next,ibench_velo)                    &
     &              - d_rtp_eq_mid(mphi,ibench_velo))
!
          phi_zero(icou) = two*four*atan(one)                           &
     &                    * (dble(mphi) - coef) / dble(mphi_mid_eq)
          do nd = 1, 7
            d_zero(icou,nd) = coef * d_rtp_eq_mid(mphi,nd)              &
     &                       + (one - coef) * d_rtp_eq_mid(mp_next,nd)
          end do
!
          if(icou .eq. 4) exit
        end if
      end do
!
      phi_prev(1:4) = phi_zero(1:4)
!
      do nd = 1, 7
        d_zero(0,nd) = quad * (d_zero(1,nd) + d_zero(2,nd)              &
     &                       + d_zero(3,nd) + d_zero(4,nd))
      end do
!
      end subroutine cal_field_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module m_field_at_mid_equator
