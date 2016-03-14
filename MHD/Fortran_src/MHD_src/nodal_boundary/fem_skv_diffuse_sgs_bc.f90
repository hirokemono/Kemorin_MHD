!fem_skv_diffuse_sgs_bc.f90
!     module fem_skv_diffuse_sgs_bc
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!      subroutine fem_skv_diffuse_sgs_fixed                             &
!     &        (numele, nnod_4_e1, nnod_4_e2, np_smp,                   &
!     &         num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int, &
!     &         ntot_int_3d, xjac, dnx1, dnx2, xmom_order2, nele_fmom,  &
!     &         elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2, &
!     &         elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2,&
!     &         ak_diff, ak_d, phi_e, sk_v)
!
      module fem_skv_diffuse_sgs_bc
!
      use m_precision
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
      subroutine fem_skv_diffuse_sgs_fixed                              &
     &        (numele, nnod_4_e1, nnod_4_e2, np_smp,                    &
     &         num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int,  &
     &         ntot_int_3d, xjac, dnx1, dnx2, xmom_order2, nele_fmom,   &
     &         elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,  &
     &         elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2, &
     &         ak_diff, ak_d, phi_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: n_int, k2, nd, np_smp
      integer(kind=kint), intent(in) :: num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real(kind=kreal), intent(in) :: xmom_order2
!
      integer(kind=kint), intent(in) :: nele_fmom
      real(kind=kreal), intent(in) :: elen_dx2_ele_dx2(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dy2_ele_dx2(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dz2_ele_dx2(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dxdy_ele_dx2(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dydz_ele_dx2(nele_fmom,3)
      real(kind=kreal), intent(in) :: elen_dzdx_ele_dx2(nele_fmom,3)
!
      real (kind=kreal), intent(in) :: phi_e(numele)
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
!
      real (kind=kreal) :: diffuse
      real (kind=kreal) :: div_x, div_y, div_z
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: ip, iele, inum, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do                                                       &
!$omp&  private(k1,ii,ix,inum,iele,ist,ied,diffuse,div_x,div_y,div_z)
      do ip = 1, np_smp
        ist = ibc_stack_smp(ip-1) + 1
        ied = ibc_stack_smp(ip)
!
        do k1=1, nnod_4_e1
          do ii = 1, n_int * n_int * n_int
            ix = int_start3(n_int) + ii
!
!cdir nodep
!OCL VECTOR, NOVREC
!VOPTION INDEP, VEC
            do inum = ist, ied
              iele = ele_bc_id(inum)
!
!  ------  set diffusion term
!
              diffuse =  ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)        &
     &                   + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)        &
     &                   + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )
!
              div_x = half * xmom_order2                                &
     &               * ((elen_dx2_ele_dx2(iele,1)* dnx1(iele,k1,ix,1)   &
     &                 + elen_dxdy_ele_dx2(iele,1)*dnx1(iele,k1,ix,2)   &
     &                 + elen_dzdx_ele_dx2(iele,1)*dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,1)                              &
     &               +  (elen_dxdy_ele_dx2(iele,1)*dnx1(iele,k1,ix,1)   &
     &                 + elen_dy2_ele_dx2(iele,1)* dnx1(iele,k1,ix,2)   &
     &                 + elen_dydz_ele_dx2(iele,1)*dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,2)                              &
     &               +  (elen_dzdx_ele_dx2(iele,1)*dnx1(iele,k1,ix,1)   &
     &                 + elen_dydz_ele_dx2(iele,1)*dnx1(iele,k1,ix,2)   &
     &                 + elen_dz2_ele_dx2(iele,1)* dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,3) )
!
              div_y = half * xmom_order2                                &
     &               * ((elen_dx2_ele_dx2(iele,2)* dnx1(iele,k1,ix,1)   &
     &                 + elen_dxdy_ele_dx2(iele,2)*dnx1(iele,k1,ix,2)   &
     &                 + elen_dzdx_ele_dx2(iele,2)*dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,1)                              &
     &               +  (elen_dxdy_ele_dx2(iele,2)*dnx1(iele,k1,ix,1)   &
     &                 + elen_dy2_ele_dx2(iele,2)* dnx1(iele,k1,ix,2)   &
     &                 + elen_dydz_ele_dx2(iele,2)*dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,2)                              &
     &               +  (elen_dzdx_ele_dx2(iele,3)*dnx1(iele,k1,ix,1)   &
     &                 + elen_dydz_ele_dx2(iele,3)*dnx1(iele,k1,ix,2)   &
     &                 + elen_dz2_ele_dx2(iele,3)* dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,3) )
!
              div_z = half * xmom_order2                                &
     &               * ((elen_dx2_ele_dx2(iele,3)* dnx1(iele,k1,ix,1)   &
     &                 + elen_dxdy_ele_dx2(iele,3)*dnx1(iele,k1,ix,2)   &
     &                 + elen_dzdx_ele_dx2(iele,3)*dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,1)                              &
     &               +  (elen_dxdy_ele_dx2(iele,3)*dnx1(iele,k1,ix,1)   &
     &                 + elen_dy2_ele_dx2(iele,3)* dnx1(iele,k1,ix,2)   &
     &                 + elen_dydz_ele_dx2(iele,3)*dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,2)                              &
     &               +  (elen_dzdx_ele_dx2(iele,3)*dnx1(iele,k1,ix,1)   &
     &                 + elen_dydz_ele_dx2(iele,3)*dnx1(iele,k1,ix,2)   &
     &                 + elen_dz2_ele_dx2(iele,3)* dnx1(iele,k1,ix,3))  &
     &                * dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,nd,k1) = sk_v(iele,nd,k1)                       &
     &           - (diffuse + ak_diff(iele) * (div_x+div_y+div_z))      &
     &            * ak_d(iele) * phi_e(iele) * xjac(iele,ix)*owe3d(ix)
!
            end do
!
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_skv_diffuse_sgs_fixed
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffuse_sgs_bc
