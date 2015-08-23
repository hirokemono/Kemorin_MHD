!
!     module int_differenciate_on_ele
!
!     Written by H.Matsui
!
!!      subroutine int_diff_scalar_on_ele                               &
!!     &         (numnod, numele, nnod_4_ele, ie, a_vol_ele,            &
!!     &          iele_fsmp_stack, ntot_int_3d, n_int, xjac, dnx,       &
!!     &          d_nod, dvx)
!!      subroutine int_diff_vector_on_ele                               &
!!     &         (numnod, numele, nnod_4_ele, ie, a_vol_ele,            &
!!     &          iele_fsmp_stack, ntot_int_3d, n_int, xjac, dnx,       &
!!     &          d_nod, dvx)
!
      module int_differenciate_on_ele
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine int_diff_scalar_on_ele                                 &
     &         (numnod, numele, nnod_4_ele, ie, a_vol_ele,              &
     &          iele_fsmp_stack, ntot_int_3d, n_int, xjac, dnx,         &
     &          d_nod, dvx)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in)                                      &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
!
      real(kind=kreal), intent(inout) :: d_nod(numnod)
!
      real(kind=kreal), intent(inout) :: dvx(numele,9)
!
       integer(kind = kint) :: iproc, inod, iele, k1, ii, ix
       integer(kind = kint) :: istart, iend, nd
!
!
      do nd = 1, 9
!$omp parallel do
        do iele = 1, numele
          dvx(iele,nd) = 0.0d0
        end do
!$omp end parallel do
      end do
!
! --------- lead average gradient of magntic field in a element
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_ele
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir concur
!cdir nodep
            do iele = istart, iend
                inod = ie(iele,k1)
!
                dvx(iele,1) = dvx(iele,1)                               &
     &              +   dnx(iele,k1,ix,1) * d_nod(inod)                 &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,2) = dvx(iele,2)                               &
     &              +   dnx(iele,k1,ix,2) * d_nod(inod)                 &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,3) = dvx(iele,3)                               &
     &              +   dnx(iele,k1,ix,3) * d_nod(inod)                 &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
             end do
           end do
!
         end do
       end do
!$omp end parallel do
!
      end subroutine int_diff_scalar_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine int_diff_vector_on_ele                                 &
     &         (numnod, numele, nnod_4_ele, ie, a_vol_ele,              &
     &          iele_fsmp_stack, ntot_int_3d, n_int, xjac, dnx,         &
     &          d_nod, dvx)
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in)                                      &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
!
      real(kind=kreal), intent(inout) :: d_nod(numnod,3)
!
      real(kind=kreal), intent(inout) :: dvx(numele,9)
!
       integer(kind = kint) :: iproc, inod, iele, k1, ii, ix
       integer(kind = kint) :: istart, iend, nd
!
!
      do nd = 1, 9
!$omp parallel do
        do iele = 1, numele
          dvx(iele,nd) = 0.0d0
        end do
!$omp end parallel do
      end do
!
! --------- lead average gradient of magntic field in a element
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_ele
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir concur
!cdir nodep
            do iele = istart, iend
                inod = ie(iele,k1)
!
                dvx(iele,1) = dvx(iele,1)                               &
     &              +   dnx(iele,k1,ix,1) * d_nod(inod,1)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,2) = dvx(iele,2)                               &
     &              +   dnx(iele,k1,ix,2) * d_nod(inod,1)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,3) = dvx(iele,3)                               &
     &              +   dnx(iele,k1,ix,3) * d_nod(inod,1)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
!
                dvx(iele,4) = dvx(iele,4)                               &
     &              +   dnx(iele,k1,ix,1) * d_nod(inod,2)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,5) = dvx(iele,5)                               &
     &              +   dnx(iele,k1,ix,2) * d_nod(inod,2)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,6) = dvx(iele,6)                               &
     &              +   dnx(iele,k1,ix,3) * d_nod(inod,2)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
!
                dvx(iele,7) = dvx(iele,7)                               &
     &              +   dnx(iele,k1,ix,1) * d_nod(inod,3)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,8) = dvx(iele,8)                               &
     &              +   dnx(iele,k1,ix,2) * d_nod(inod,3)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
!
                dvx(iele,9) = dvx(iele,9)                               &
     &              +   dnx(iele,k1,ix,3) * d_nod(inod,3)               &
     &               * xjac(iele,ix) * owe3d(ix) * a_vol_ele(iele)
             end do
           end do
!
         end do
       end do
!$omp end parallel do
!
      end subroutine int_diff_vector_on_ele
!
! -----------------------------------------------------------------------
!
      end module int_differenciate_on_ele
