!fem_skv_div_asym_t.f90
!      module fem_skv_div_asym_t
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_all_div_asym_t                                &
!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack, &
!     &          n_int, k2, ntot_int_3d, xjac, an, dnx, flux_1, sk_v)
!      subroutine fem_skv_grp_div_asym_t                                &
!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack, &
!     &          nele_grp, iele_grp, n_int, k2, ntot_int_3d,            &
!     &          xjac, an, dnx, flux_1, sk_v)
!
      module fem_skv_div_asym_t
!
      use m_precision
!
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
      subroutine fem_skv_all_div_asym_t                                 &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          n_int, k2, ntot_int_3d, xjac, an, dnx, flux_1, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_e2,ntot_int_3d,3)
      real(kind=kreal), intent(in)    :: flux_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer(kind=kint) :: k1
      integer(kind=kint) :: iproc, iele, ii, ix
      integer(kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do iele = istart, iend
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + an(k1,ix)             &
     &                     * ( dnx(iele,k2,ix,2) * flux_1(iele,1)       &
     &                       - dnx(iele,k2,ix,3) * flux_1(iele,3) )     &
     &                        * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1) + an(k1,ix)             &
     &                     * (-dnx(iele,k2,ix,1) * flux_1(iele,1)       &
     &                       + dnx(iele,k2,ix,3) * flux_1(iele,2) )     &
     &                        * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1) + an(k1,ix)             &
     &                     * ( dnx(iele,k2,ix,1) * flux_1(iele,3)       &
     &                       - dnx(iele,k2,ix,2) * flux_1(iele,2) )     &
     &                        * xjac(iele,ix)*owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_all_div_asym_t
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_asym_t                                 &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          nele_grp, iele_grp, n_int, k2, ntot_int_3d,             &
     &          xjac, an, dnx, flux_1, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_e2,ntot_int_3d,3)
      real(kind=kreal), intent(in)    :: flux_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer(kind=kint) :: k1
      integer(kind=kint) :: iproc, inum, iele, ii, ix
      integer(kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,inum,iele,istart,iend) 
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
            do inum = istart, iend
              iele = iele_grp(inum)
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1) + an(k1,ix)             &
     &                     * ( dnx(iele,k2,ix,2) * flux_1(iele,1)       &
     &                       - dnx(iele,k2,ix,3) * flux_1(iele,3) )     &
     &                        * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1) + an(k1,ix)             &
     &                     * (-dnx(iele,k2,ix,1) * flux_1(iele,1)       &
     &                       + dnx(iele,k2,ix,3) * flux_1(iele,2) )     &
     &                        * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1) + an(k1,ix)             &
     &                     * ( dnx(iele,k2,ix,1) * flux_1(iele,3)       &
     &                       - dnx(iele,k2,ix,2) * flux_1(iele,2) )     &
     &                        * xjac(iele,ix)*owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_grp_div_asym_t
!
!-----------------------------------------------------------------------
!
      end module fem_skv_div_asym_t
