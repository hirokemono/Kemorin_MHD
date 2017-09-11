!
!      module fem_vol_average_energy
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine fem_vol_all_energy                                   &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, aw, k2, vect_e,             &
!!     &          rms_local, ave_local)
!!      subroutine fem_vol_angular_momentum                             &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, aw, k2, xe, vect_e,         &
!!     &          amom_local)
!!
!!      subroutine fem_vol_ave_rms_4_scalar                             &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, aw, k2, phi_e,              &
!!     &          rms_local, ave_local)
!!      subroutine fem_ave_rms_4_scalar                                 &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, aw, k2, phi_e,                    &
!!     &          rms_local, ave_local)
!
      module fem_vol_average_energy
!
      use m_precision
!
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
      subroutine fem_vol_all_energy                                     &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, aw, k2, vect_e,               &
     &          rms_local, ave_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: aw(nnod_4_ele,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: vect_e(numele,3)
!
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(3)
!
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
      real (kind=kreal) :: rms_smp(np_smp)
      real (kind=kreal) :: ave_smp(np_smp,3)
!
!
      rms_smp = zero
      ave_smp = zero
!$omp parallel do private(ii,ix,iele,istart,iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
!
         do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
          do iele = istart, iend
!
           rms_smp(iproc) = rms_smp(iproc)                              &
     &    + dble(interior_ele(iele))                                    &
     &     * ( vect_e(iele,1) * vect_e(iele,1)                          &
     &       + vect_e(iele,2) * vect_e(iele,2)                          &
     &       + vect_e(iele,3) * vect_e(iele,3) )                        &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
           ave_smp(iproc,1) = ave_smp(iproc,1)                          &
     &    + dble(interior_ele(iele)) * vect_e(iele,1)                   &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
           ave_smp(iproc,2) = ave_smp(iproc,2)                          &
     &    + dble(interior_ele(iele)) * vect_e(iele,2)                   &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
           ave_smp(iproc,3) = ave_smp(iproc,3)                          &
     &    + dble(interior_ele(iele)) * vect_e(iele,3)                   &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
      do iproc = 1, np_smp
        rms_local  = rms_local + rms_smp(iproc)
        ave_local(1) = ave_local(1) + ave_smp(iproc,1)
        ave_local(2) = ave_local(2) + ave_smp(iproc,2)
        ave_local(3) = ave_local(3) + ave_smp(iproc,3)
      end do
!
      end subroutine fem_vol_all_energy
!
! ----------------------------------------------------------------------
!
      subroutine fem_vol_angular_momentum                               &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, aw, k2, xe, vect_e,           &
     &          amom_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: aw(nnod_4_ele,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: xe(numele,3)
      real (kind=kreal), intent(in) :: vect_e(numele,3)
!
      real (kind=kreal), intent(inout) :: amom_local(3)
!
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
      real (kind=kreal) :: a_mom_smp(np_smp,3)
!
!
      a_mom_smp = 0.0d0
!$omp parallel do private(ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
!voption, indep, vec
          do iele = istart, iend
            a_mom_smp(iproc,1) = a_mom_smp(iproc,1)                     &
     &                   + dble(interior_ele(iele))                     &
     &                   * (xe(iele,2)*vect_e(iele,3)                   &
     &                    - xe(iele,3)*vect_e(iele,2))                  &
     &                  * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
             a_mom_smp(iproc,2) = a_mom_smp(iproc,2)                    &
     &                    + dble(interior_ele(iele))                    &
     &                   * (xe(iele,3)*vect_e(iele,1)                   &
     &                    - xe(iele,1)*vect_e(iele,3))                  &
     &                  * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
             a_mom_smp(iproc,3) = a_mom_smp(iproc,3)                    &
     &                    + dble(interior_ele(iele))                    &
     &                   * (xe(iele,1)*vect_e(iele,2)                   &
     &                    - xe(iele,2)*vect_e(iele,1))                  &
     &                  * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
          end do
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        amom_local(1) = amom_local(1) + a_mom_smp(iproc,1)
        amom_local(2) = amom_local(2) + a_mom_smp(iproc,2)
        amom_local(3) = amom_local(3) + a_mom_smp(iproc,3)
      end do
!
      end subroutine fem_vol_angular_momentum
!
! ----------------------------------------------------------------------
!
      subroutine fem_vol_ave_rms_4_scalar                               &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, aw, k2, phi_e,                &
     &          rms_local, ave_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: aw(nnod_4_ele,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: phi_e(numele)
!
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
      real (kind=kreal) :: bulk_e_smp(np_smp)
      real (kind=kreal) :: average_smp(np_smp)
!
!
      bulk_e_smp(1:np_smp) =  zero
      average_smp(1:np_smp) = zero
!
!$omp parallel do private(ii,ix,iele,ist,ied)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
          do iele = ist, ied
            bulk_e_smp(iproc) = bulk_e_smp(iproc)                       &
     &                           + dble(interior_ele(iele))             &
     &                             * ( phi_e(iele) * phi_e(iele) )      &
     &                             * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
            average_smp(iproc) = average_smp(iproc)                     &
     &                           + dble(interior_ele(iele))             &
     &                             * phi_e(iele)                        &
     &                             * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
          end do
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        rms_local = rms_local + bulk_e_smp(iproc)
        ave_local = ave_local + average_smp(iproc)
      end do
!
      end subroutine fem_vol_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
!
      subroutine fem_ave_rms_4_scalar                                   &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, aw, k2, phi_e,                      &
     &          rms_local, ave_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real(kind=kreal), intent(in) :: aw(nnod_4_ele,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: phi_e(numele)
!
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
      real (kind=kreal) :: bulk_e_smp(np_smp)
      real (kind=kreal) :: average_smp(np_smp)
!
!
      bulk_e_smp(1:np_smp) =  zero
      average_smp(1:np_smp) = zero
!
!$omp parallel do private(ii,ix,iele,ist,ied)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
          do iele = ist, ied
            bulk_e_smp(iproc) = bulk_e_smp(iproc)                       &
     &                           + dble(interior_ele(iele))             &
     &                             * ( phi_e(iele) * phi_e(iele) )      &
     &                             * aw(k2,ix)*owe3d(ix)
!
            average_smp(iproc) = average_smp(iproc)                     &
     &                           + dble(interior_ele(iele))             &
     &                             * phi_e(iele)                        &
     &                             * aw(k2,ix)*owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        rms_local = rms_local + bulk_e_smp(iproc)
        ave_local = ave_local + average_smp(iproc)
      end do
!
      end subroutine fem_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
!
      end module fem_vol_average_energy
