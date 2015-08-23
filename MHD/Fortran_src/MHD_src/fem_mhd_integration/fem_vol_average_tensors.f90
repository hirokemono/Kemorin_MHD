!
!      module fem_vol_average_tensors
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine fem_vol_ave_sym_tensor_1                             &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          ntot_int_3d, n_int, xjac, aw, k2,                     &
!!     &          vect_e, rms_local, ave_local)
!!      subroutine fem_vol_ave_sym_tensor_2                             &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          ntot_int_3d, n_int, xjac, aw, k2,                     &
!!     &          vect_e, rms_local, ave_local)
!!
!!      subroutine fem_vol_ave_asym_tensor                              &
!!     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,    &
!!     &          ntot_int_3d, n_int, xjac, aw, k2,                     &
!!     &          vect_e, rms_local, ave_local)
!
      module fem_vol_average_tensors
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_fem_gauss_int_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_vol_ave_sym_tensor_1                               &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          ntot_int_3d, n_int, xjac, aw, k2,                       &
     &          vect_e, rms_local, ave_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
     &     * (       vect_e(iele,1) * vect_e(iele,1)                    &
     &       + two * vect_e(iele,2) * vect_e(iele,2)                    &
     &       + two * vect_e(iele,3) * vect_e(iele,3) )                  &
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
      end subroutine fem_vol_ave_sym_tensor_1
!
! ----------------------------------------------------------------------
!
      subroutine fem_vol_ave_sym_tensor_2                               &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          ntot_int_3d, n_int, xjac, aw, k2,                       &
     &          vect_e, rms_local, ave_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: k2
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: aw(nnod_4_ele,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: vect_e(numele,3)
!
      real (kind=kreal), intent(inout) :: rms_local
      real (kind=kreal), intent(inout) :: ave_local(6)
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
     &     * (       vect_e(iele,1) * vect_e(iele,1)                    &
     &       + two * vect_e(iele,2) * vect_e(iele,2)                    &
     &       +       vect_e(iele,3) * vect_e(iele,3) )                  &
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
        ave_local(4) = ave_local(4) + ave_smp(iproc,1)
        ave_local(5) = ave_local(5) + ave_smp(iproc,2)
        ave_local(6) = ave_local(6) + ave_smp(iproc,3)
      end do
!
      end subroutine fem_vol_ave_sym_tensor_2
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_vol_ave_asym_tensor                                &
     &         (numele, nnod_4_ele, iele_fsmp_stack, interior_ele,      &
     &          ntot_int_3d, n_int, xjac, aw, k2,                       &
     &          vect_e, rms_local, ave_local)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: interior_ele(numele)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
     &     * (  vect_e(iele,1) * vect_e(iele,1)                         &
     &       +  vect_e(iele,2) * vect_e(iele,2)                         &
     &       +  vect_e(iele,3) * vect_e(iele,3) )                       &
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
!
      do iproc = 1, np_smp
        rms_local  = rms_local + rms_smp(iproc)
        ave_local(1) = ave_local(1) + ave_smp(iproc,1)
        ave_local(2) = ave_local(2) + ave_smp(iproc,2)
        ave_local(3) = ave_local(3) + ave_smp(iproc,3)
      end do
      rms_local  = two * rms_local
!
      end subroutine fem_vol_ave_asym_tensor
!
! ----------------------------------------------------------------------
!
      end module fem_vol_average_tensors
