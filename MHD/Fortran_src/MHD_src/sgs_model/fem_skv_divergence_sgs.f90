!fem_skv_divergence_sgs.f90
!     module fem_skv_divergence_sgs
!
!        programmed by H.Matsui on July, 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_div_sgs_pg(numele, nnod_4_e1, nnod_4_e2,      &
!     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,       &
!     &          xjac, an1, dnx1, dnx2, xmom_order2, nele_fmom,         &
!     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,   &
!     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,  &
!     &          ak_diff, vector_1, sk_v)
!
      module fem_skv_divergence_sgs
!
      use m_precision
!
      use m_constants
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
      subroutine fem_skv_div_sgs_pg(numele, nnod_4_e1, nnod_4_e2,       &
     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,        &
     &          xjac, an1, dnx1, dnx2, xmom_order2, nele_fmom,          &
     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,    &
     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,   &
     &          ak_diff, vector_1, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d, n_int
      integer(kind=kint), intent(in) :: k2
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real(kind=kreal), intent(in) :: xmom_order2
!
      integer(kind=kint), intent(in) :: nele_fmom
      real(kind=kreal), intent(in) :: elen_dx2_ele_dx(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dy2_ele_dx(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dz2_ele_dx(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dxdy_ele_dx(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dydz_ele_dx(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dzdx_ele_dx(nele_fmom,3)
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind=kreal) :: div_x, div_y, div_z
      integer(kind=kint) :: k1
      integer(kind=kint) :: iproc, iele, ii, ix
      integer(kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,div_x,div_y,div_z)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = istart, iend
              div_x = half * xmom_order2                                &
     &         * ( ( elen_dx2_ele_dx(iele,1) *dnx1(iele,k1,ix,1)        &
     &             + elen_dxdy_ele_dx(iele,1)*dnx1(iele,k1,ix,2)        &
     &             + elen_dzdx_ele_dx(iele,1)*dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,1)                                   &
     &           + ( elen_dxdy_ele_dx(iele,1)*dnx1(iele,k1,ix,1)        &
     &             + elen_dy2_ele_dx(iele,1)* dnx1(iele,k1,ix,2)        &
     &             + elen_dydz_ele_dx(iele,1)*dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,2)                                   &
     &           + ( elen_dzdx_ele_dx(iele,1)*dnx1(iele,k1,ix,1)        &
     &             + elen_dydz_ele_dx(iele,1)*dnx1(iele,k1,ix,2)        &
     &             + elen_dz2_ele_dx(iele,1)* dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,3) )
!
              div_y = half * xmom_order2                                &
     &         * ( ( elen_dx2_ele_dx(iele,2) *dnx1(iele,k1,ix,1)        &
     &             + elen_dxdy_ele_dx(iele,2)*dnx1(iele,k1,ix,2)        &
     &             + elen_dzdx_ele_dx(iele,2)*dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,1)                                   &
     &           + ( elen_dxdy_ele_dx(iele,2)*dnx1(iele,k1,ix,1)        &
     &             + elen_dy2_ele_dx(iele,2)* dnx1(iele,k1,ix,2)        &
     &             + elen_dydz_ele_dx(iele,2)*dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,2)                                   &
     &           + ( elen_dzdx_ele_dx(iele,2)*dnx1(iele,k1,ix,1)        &
     &             + elen_dydz_ele_dx(iele,2)*dnx1(iele,k1,ix,2)        &
     &             + elen_dz2_ele_dx(iele,2)* dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,3) )
!
              div_z = half * xmom_order2                                &
     &         * ( ( elen_dx2_ele_dx(iele,3) *dnx1(iele,k1,ix,1)        &
     &             + elen_dxdy_ele_dx(iele,3)*dnx1(iele,k1,ix,2)        &
     &             + elen_dzdx_ele_dx(iele,3)*dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,1)                                   &
     &           + ( elen_dxdy_ele_dx(iele,3)*dnx1(iele,k1,ix,1)        &
     &             + elen_dy2_ele_dx(iele,3)* dnx1(iele,k1,ix,2)        &
     &             + elen_dydz_ele_dx(iele,3)*dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,2)                                   &
     &           + ( elen_dzdx_ele_dx(iele,3)*dnx1(iele,k1,ix,1)        &
     &             + elen_dydz_ele_dx(iele,3)*dnx1(iele,k1,ix,2)        &
     &             + elen_dz2_ele_dx(iele,3)* dnx1(iele,k1,ix,3) )      &
     &           * dnx2(iele,k2,ix,3) )
!
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &            + ( (an1(k1,ix) * dnx2(iele,k2,ix,1)                  &
     &             + ak_diff(iele) * div_x ) * vector_1(iele,1)         &
     &              + (an1(k1,ix) * dnx2(iele,k2,ix,2)                  &
     &             + ak_diff(iele) * div_y ) * vector_1(iele,2)         &
     &              + (an1(k1,ix) * dnx2(iele,k2,ix,3)                  &
     &             + ak_diff(iele) * div_z ) * vector_1(iele,3))        &
     &           * xjac(iele,ix)*owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_div_sgs_pg
!
!-----------------------------------------------------------------------
!
      end module fem_skv_divergence_sgs
