!
!     module cal_rotation_on_element
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine fem_rotation_on_element(iele_fsmp_stack,              &
!     &          numnod, numele, nnod_4_ele, ie, a_vol_ele,             &
!     &          ntot_int_3d, n_int, dnx, xjac, d_ele, d_nod)
!      subroutine fem_rotation_grp_on_element(iele_fsmp_stack,          &
!     &          numnod, numele, nnod_4_ele, ie, a_vol_ele,             &
!     &          nele_grp, iele_grp, ntot_int_3d, n_int, dnx, xjac,     &
!     &          d_ele, d_nod)
!
      module cal_rotation_on_element
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_rotation_on_element(iele_fsmp_stack,               &
     &          numnod, numele, nnod_4_ele, ie, a_vol_ele,              &
     &          ntot_int_3d, n_int, dnx, xjac, d_ele, d_nod)
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
!
!
      integer (kind = kint), intent(in) :: ntot_int_3d, n_int
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
!
      real(kind = kreal), intent(in) :: d_nod(numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(numele,3)
!
      integer (kind = kint) :: iproc, inod, iele
      integer (kind = kint) :: k1, ii, ix
      integer (kind = kint) :: ist, ied
!
! --------- lead average velocity in a element
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        d_ele(ist:ied,1) = zero
        d_ele(ist:ied,2) = zero
        d_ele(ist:ied,3) = zero
!
        do k1 = 1, nnod_4_ele
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
              inod = ie(iele,k1)
!
              d_ele(iele,1) = d_ele(iele,1)                             &
     &                       + ( dnx(iele,k1,ix,2) * d_nod(inod,3)      &
     &                         - dnx(iele,k1,ix,3) * d_nod(inod,2) )    &
     &                        * xjac(iele,ix) * owe3d(ix)               &
     &                        * a_vol_ele(iele)
!
              d_ele(iele,2) = d_ele(iele,2)                             &
     &                       + ( dnx(iele,k1,ix,3) * d_nod(inod,1)      &
     &                         - dnx(iele,k1,ix,1) * d_nod(inod,3) )    &
     &                        * xjac(iele,ix) * owe3d(ix)               &
     &                        * a_vol_ele(iele)
!
              d_ele(iele,3) = d_ele(iele,3)                             &
     &                       + ( dnx(iele,k1,ix,1) * d_nod(inod,2)      &
     &                         - dnx(iele,k1,ix,2) * d_nod(inod,1) )    &
     &                        * xjac(iele,ix) * owe3d(ix)               &
     &                        * a_vol_ele(iele)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_rotation_on_element
!
! ----------------------------------------------------------------------
!
      subroutine fem_rotation_grp_on_element(iele_fsmp_stack,           &
     &          numnod, numele, nnod_4_ele, ie, a_vol_ele,              &
     &          nele_grp, iele_grp, ntot_int_3d, n_int, dnx, xjac,      &
     &          d_ele, d_nod)
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
!
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind = kint), intent(in) :: ntot_int_3d, n_int
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
!
      real(kind = kreal), intent(in) :: d_nod(numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(numele,3)
!
      integer (kind = kint) :: iproc, inod, inum, iele
      integer (kind = kint) :: k1, ii, ix
      integer (kind = kint) :: ist, ied
!
! --------- lead average velocity in a element
!
!$omp parallel do private(k1,ii,ix,inum,iele,ist,ied)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        d_ele(ist:ied,1) = zero
        d_ele(ist:ied,2) = zero
        d_ele(ist:ied,3) = zero
!
        do k1 = 1, nnod_4_ele
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do inum = ist, ied
              iele = iele_grp(inum)
              inod = ie(iele,k1)
!
              d_ele(iele,1) = d_ele(iele,1)                             &
     &                       + ( dnx(iele,k1,ix,2) * d_nod(inod,3)      &
     &                         - dnx(iele,k1,ix,3) * d_nod(inod,2) )    &
     &                        * xjac(iele,ix) * owe3d(ix)               &
     &                        * a_vol_ele(iele)
!
              d_ele(iele,2) = d_ele(iele,2)                             &
     &                       + ( dnx(iele,k1,ix,3) * d_nod(inod,1)      &
     &                         - dnx(iele,k1,ix,1) * d_nod(inod,3) )    &
     &                        * xjac(iele,ix) * owe3d(ix)               &
     &                        * a_vol_ele(iele)
!
              d_ele(iele,3) = d_ele(iele,3)                             &
     &                       + ( dnx(iele,k1,ix,1) * d_nod(inod,2)      &
     &                         - dnx(iele,k1,ix,2) * d_nod(inod,1) )    &
     &                        * xjac(iele,ix) * owe3d(ix)               &
     &                        * a_vol_ele(iele)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_rotation_grp_on_element
!
! ----------------------------------------------------------------------
!
      end module cal_rotation_on_element
