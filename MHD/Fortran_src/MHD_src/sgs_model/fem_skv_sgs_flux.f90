!
!      module fem_skv_sgs_flux
!
!     programmed by H. Matsui in April, 2012
!
!      subroutine fem_skv_sgs_flux_pg(numele, nnod_4_e1, nnod_4_e2,     &
!     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx,&
!     &          xmom_order2, nele_filter_mom,                          &
!     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,            &
!     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,           &
!     &          scalar_1, dvx, nd_t, sk_v)
!
!      subroutine fem_skv_sgs_flux_upw(numele, nnod_4_e1, ,nnod_4_e2,   &
!     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx,&
!     &          dt, xmom_order2, nele_filter_mom,                      &
!     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,            &
!     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,           &
!     &          scalar_1, vxe, dvx, nd_t, sk_v)
!
      module fem_skv_sgs_flux
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
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
      subroutine fem_skv_sgs_flux_pg(numele, nnod_4_e1, nnod_4_e2,      &
     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx, &
     &          xmom_order2, nele_filter_mom,                           &
     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,             &
     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,            &
     &          scalar_1, dvx, nd_t, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd_t, k2
      integer (kind=kint), intent(in) :: n_int, ntot_int_3d
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx(numele,nnod_4_e2,ntot_int_3d,3)
!
      real(kind=kreal), intent(in) :: xmom_order2
!
      integer (kind=kint), intent(in) :: nele_filter_mom
      real (kind=kreal), intent(in) :: elen_dx2_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dy2_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dz2_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dxdy_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dydz_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dzdx_ele(nele_filter_mom)
!
      real (kind=kreal), intent(in) :: scalar_1(numele)
      real (kind=kreal), intent(in) :: dvx(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia(np_smp)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: ip, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
      do ip = 1, np_smp
        ist = iele_fsmp_stack(ip-1)+1
        ied = iele_fsmp_stack(ip)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = ist, ied
!
              inertia(ip) = an(k1,ix) * xmom_order2                     &
                          * ((elen_dx2_ele(iele) *  dvx(iele,1)         &
     &                      + elen_dxdy_ele(iele) * dvx(iele,2)         &
     &                      + elen_dzdx_ele(iele) * dvx(iele,3))        &
     &                       * dnx(iele,k2,ix,1)                        &
     &                     + (elen_dxdy_ele(iele) * dvx(iele,1)         &
     &                      + elen_dy2_ele(iele) *  dvx(iele,2)         &
     &                      + elen_dydz_ele(iele) * dvx(iele,3))        &
     &                      * dnx(iele,k2,ix,2)                         &
     &                     + (elen_dzdx_ele(iele) * dvx(iele,1)         &
     &                      + elen_dydz_ele(iele) * dvx(iele,2)         &
     &                      + elen_dz2_ele(iele) *  dvx(iele,3))        &
     &                       * dnx(iele,k2,ix,3) )
!
!
              sk_v(iele,nd_t,k1) = sk_v(iele,nd_t,k1)                   &
     &                            + inertia(ip) * scalar_1(iele)        &
     &                             * xjac(iele,ix) * owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!
      end subroutine fem_skv_sgs_flux_pg
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_upw(numele, nnod_4_e1, nnod_4_e2,     &
     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx, &
     &          dt, xmom_order2, nele_filter_mom,                       &
     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,             &
     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,            &
     &          scalar_1, vxe, dvx, nd_t, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, ntot_int_3d
      integer (kind=kint), intent(in) :: nd_t, k2
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx(numele,nnod_4_e2,ntot_int_3d,3)
!
      real(kind=kreal), intent(in) :: xmom_order2
!
      integer (kind=kint), intent(in) :: nele_filter_mom
      real (kind=kreal), intent(in) :: elen_dx2_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dy2_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dz2_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dxdy_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dydz_ele(nele_filter_mom)
      real (kind=kreal), intent(in) :: elen_dzdx_ele(nele_filter_mom)
!
      real (kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: scalar_1(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: dvx(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: tau(np_smp)
      real (kind=kreal) :: inertia(np_smp)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: ip, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
      do ip = 1, np_smp
        ist = iele_fsmp_stack(ip-1)+1
        ied = iele_fsmp_stack(ip)
!
        do k1 = 1, nnod_4_e1
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
!voption, indep, vec
            do iele = ist, ied
!
!  -----  set weighting function
!
              tau(ip) = an(k1,ix)                                       &
     &                   + half * dt                                    &
     &                 * ( vxe(iele,1)*dnx(iele,k1,ix,1)                &
     &                   + vxe(iele,2)*dnx(iele,k1,ix,2)                &
     &                   + vxe(iele,3)*dnx(iele,k1,ix,3) )
!
              inertia(ip) = tau(ip) * xmom_order2                       &
                          * ((elen_dx2_ele(iele) *  dvx(iele,1)         &
     &                      + elen_dxdy_ele(iele) * dvx(iele,2)         &
     &                      + elen_dzdx_ele(iele) * dvx(iele,3))        &
     &                       * dnx(iele,k2,ix,1)                        &
     &                     + (elen_dxdy_ele(iele) * dvx(iele,1)         &
     &                      + elen_dy2_ele(iele) *  dvx(iele,2)         &
     &                      + elen_dydz_ele(iele) * dvx(iele,3))        &
     &                      * dnx(iele,k2,ix,2)                         &
     &                     + (elen_dzdx_ele(iele) * dvx(iele,1)         &
     &                      + elen_dydz_ele(iele) * dvx(iele,2)         &
     &                      + elen_dz2_ele(iele) *  dvx(iele,3))        &
     &                       * dnx(iele,k2,ix,3) )
!
!
              sk_v(iele,nd_t,k1) = sk_v(iele,nd_t,k1)                   &
     &                            + inertia(ip) * scalar_1(iele)        &
     &                             * xjac(iele,ix) * owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_sgs_flux_upw
!
! ----------------------------------------------------------------------
!
      end module fem_skv_sgs_flux
