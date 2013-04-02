!fem_skv_poisson_bc.f90
!     module fem_skv_poisson_bc
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine fem_skv_scalar_diffuse_fixed                          &
!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp,                  &
!     &          num_index_ibc, ele_bc_id, ibc_stack_smp,               &
!     &          k2, nd, n_int, ntot_int_3d, xjac, dnx1, dnx2,          &
!     &          ak_d, phi_e, sk_v)
!
!      subroutine fem_skv_poisson_fixed(numele, nnod_4_e1, nnod_4_e2,   &
!     &          np_smp, num_index_ibc, ele_bc_id, ibc_stack_smp,       &
!     &          k2, n_int, ntot_int_3d, xjac, dnx1, dnx2, phi_e, sk_v)
!
      module fem_skv_poisson_bc
!
      use m_precision
      use m_phys_constants
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
      subroutine fem_skv_scalar_diffuse_fixed                           &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp,                   &
     &          num_index_ibc, ele_bc_id, ibc_stack_smp,                &
     &          k2, nd, n_int, ntot_int_3d, xjac, dnx1, dnx2,           &
     &          ak_d, phi_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      integer (kind=kint), intent(in) :: n_int, k2, nd, np_smp
      real (kind=kreal), intent(in) :: phi_e(numele)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, inum, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,inum,iele,istart,iend)
      do iproc = 1, np_smp
        istart = ibc_stack_smp( iproc-1 ) + 1
        iend   = ibc_stack_smp( iproc   )
!
        do k1 = 1, nnod_4_e1
          do ii = 1, n_int * n_int * n_int
            ix = int_start3(n_int) + ii
!
!cdir nodep
!VOPTION INDEP, VEC
            do inum = istart, iend
              iele = ele_bc_id(inum)
!
              sk_v(iele,nd,k1) = sk_v(iele,nd,k1)                       &
     &                     - ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)    &
     &                       + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)    &
     &                       + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )  &
     &                     * xjac(iele,ix) * owe3d(ix) * phi_e(iele)    &
     &                     * ak_d(iele)
!
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_skv_scalar_diffuse_fixed
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_fixed(numele, nnod_4_e1, nnod_4_e2,    &
     &          np_smp, num_index_ibc, ele_bc_id, ibc_stack_smp,        &
     &          k2, n_int, ntot_int_3d, xjac, dnx1, dnx2, phi_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: num_index_ibc
      integer (kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer (kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      integer (kind=kint), intent(in) :: n_int, k2, np_smp
      real (kind=kreal), intent(in) :: phi_e(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind=kint) :: k1
!
      integer (kind=kint) :: iproc, iele, inum, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,inum,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = ibc_stack_smp( iproc-1 ) + 1
        iend   = ibc_stack_smp( iproc   )
!
        do k1=1, nnod_4_e1
          do ii = 1, n_int * n_int * n_int
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do inum = istart, iend
              iele = ele_bc_id(inum)
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     - ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)    &
     &                       + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)    &
     &                       + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )  &
     &                      * xjac(iele,ix) * owe3d(ix) * phi_e(iele)
!
            end do
!
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_skv_poisson_fixed
!
!-----------------------------------------------------------------------
!
      end module fem_skv_poisson_bc
