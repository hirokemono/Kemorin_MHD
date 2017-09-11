!
!      module fem_skv_div_normal
!
!      Written by H. Matsui
!      Modified by H. Matsui on Aug, 2006
!
!!      subroutine fem_skv_div_normal_pg                                &
!!     &         (numele, nnod_4_e2, np_smp, iele_fsmp_stack,           &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, dnx2, k2, vect_e, sk1)
!!      subroutine fem_skv_rms_flux_pg                                  &
!!     &         (numele, nnod_4_e2, np_smp, iele_fsmp_stack,           &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, xjac, dnx2, k2, vect_e, sk1)
!
      module fem_skv_div_normal
!
      use m_precision
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_div_normal_pg                                  &
     &         (numele, nnod_4_e2, np_smp, iele_fsmp_stack,             &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, dnx2, k2, vect_e, sk1)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e2, np_smp
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: vect_e(numele,3)
!
      real (kind=kreal), intent(inout) :: sk1(numele)
!
!
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(ii,ix,iele,istart, iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
          do iele = istart, iend
!
            sk1(iele) = sk1(iele) + (dnx2(iele,k2,ix,1)*vect_e(iele,1)  &
     &                             + dnx2(iele,k2,ix,2)*vect_e(iele,2)  &
     &                             + dnx2(iele,k2,ix,3)*vect_e(iele,3)) &
     &                             *xjac(iele,ix)*owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_div_normal_pg
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_rms_flux_pg                                    &
     &         (numele, nnod_4_e2, np_smp, iele_fsmp_stack,             &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, xjac, dnx2, k2, vect_e, sk1)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e2, np_smp
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: vect_e(numele,3)
      real (kind=kreal), intent(inout)  :: sk1(numele)
!
!
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(ii,ix,iele,istart, iend)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
          do iele = istart, iend
!
            sk1(iele) = sk1(iele)                                       &
                       + (dnx2(iele,k2,ix,1)*vect_e(iele,1)             &
     &                  + dnx2(iele,k2,ix,2)*vect_e(iele,2)             &
     &                  + dnx2(iele,k2,ix,3)*vect_e(iele,3) )**2        &
     &                   * xjac(iele,ix)*owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_rms_flux_pg
!
! ----------------------------------------------------------------------
!
      end module fem_skv_div_normal
