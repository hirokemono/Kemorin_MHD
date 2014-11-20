!fem_skv_div_ast_w_sgs_upw.f90
!      module fem_skv_div_ast_w_sgs_upw
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on Aug., 2006
!
!      subroutine fem_skv_div_as_tsr_w_sgs_upw                          &
!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp,                  &
!     &          iele_fsmp_stack, n_int, k2, dt, ntot_int_3d,           &
!     &          xjac, an1, dnx1, dnx2, xmom_order2, nele_fmom,         &
!     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,   &
!     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,  &
!     &          ak_diff, vxe, sgs_1, as_tsr_1, sk_v)
!
      module fem_skv_div_ast_w_sgs_upw
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_as_tsr_w_sgs_upw                           &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp,                   &
     &          iele_fsmp_stack, n_int, k2, dt, ntot_int_3d,            &
     &          xjac, an1, dnx1, dnx2, xmom_order2, nele_fmom,          &
     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,    &
     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,   &
     &          ak_diff, vxe, sgs_1, as_tsr_1, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d, n_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: sgs_1(numele,3)
      real(kind=kreal), intent(in) :: as_tsr_1(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: dt
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
!
      real(kind=kreal) :: tau
      real(kind=kreal) :: div_ox, div_oy, div_oz
      real(kind=kreal) :: div_x,  div_y,  div_z
      integer(kind=kint) :: k1
      integer(kind=kint) :: iproc, iele, ii, ix
      integer(kind=kint) :: ist, ied
!
!
!$omp  parallel do private(k1,ii,ix,iele,ist,ied,div_ox,div_oy,div_oz,  &
!$omp&                     tau,div_x,div_y,div_z)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = ist, ied
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe(iele,1)*dnx1(iele,k1,ix,1)              &
     &                    + vxe(iele,2)*dnx1(iele,k1,ix,2)              &
     &                    + vxe(iele,3)*dnx1(iele,k1,ix,3) )
!
!
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
              div_ox = tau * ( dnx2(iele,k2,ix,2)*sgs_1(iele,1)         &
     &                       - dnx2(iele,k2,ix,3)*sgs_1(iele,3) )
              div_oy = tau * (-dnx2(iele,k2,ix,1)*sgs_1(iele,1)         &
     &                       + dnx2(iele,k2,ix,3)*sgs_1(iele,2) )
              div_oz = tau * ( dnx2(iele,k2,ix,1)*sgs_1(iele,3)         &
     &                       - dnx2(iele,k2,ix,2)*sgs_1(iele,2) )
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + ( div_ox + ak_diff(iele)               &
     &                           * ( div_y * as_tsr_1(iele,1)           &
     &                            -  div_z * as_tsr_1(iele,3) ) )       &
     &                           * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                         + ( div_oy + ak_diff(iele)               &
     &                           * (-div_x * as_tsr_1(iele,1)           &
     &                            +  div_z * as_tsr_1(iele,2) ) )       &
     &                           * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                         + ( div_oz + ak_diff(iele)               &
     &                           * ( div_x * as_tsr_1(iele,3)           &
     &                            +  div_y * as_tsr_1(iele,2) ) )       &
     &                           * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_div_as_tsr_w_sgs_upw
!
!   --------------------------------------------------------------------
!
      end module fem_skv_div_ast_w_sgs_upw
