!fem_skv_sgs_induct_t.f90
!     module fem_skv_sgs_induct_t
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!
!      subroutine fem_skv_sgs_induct_t_pg(numele, nnod_4_e1, nnod_4_e2, &
!     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx,&
!     &          xmom_order2, nele_filter_mom,                          &
!     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,            &
!     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,           &
!     &          vect_sgs, dvx, dbx, nd, sk_v)
!      subroutine fem_skv_sgs_induct_t_upw(numele, nnod_4_e1, nnod_4_e2,&
!     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx,&
!     &          dt, xmom_order2, nele_filter_mom,                      &
!     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,            &
!     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,           &
!     &          vect_sgs, vxe, dvx, dbx, nd, sk_v)
!
      module fem_skv_sgs_induct_t
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_pg(numele, nnod_4_e1, nnod_4_e2,  &
     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx, &
     &          xmom_order2, nele_filter_mom,                           &
     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,             &
     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,            &
     &          vect_sgs, dvx, dbx, nd, sk_v)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: nd, k2
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
      real(kind=kreal), intent(in) :: vect_sgs(numele,3)
      real(kind=kreal), intent(in) :: dvx(numele,3)
      real(kind=kreal), intent(in) :: dbx(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia(np_smp), induct2(np_smp)
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
      do iproc = 1, np_smp
       ist = iele_fsmp_stack(iproc-1)+1
       ied   = iele_fsmp_stack(iproc)
!
       do ii= 1, n_int * n_int * n_int 
        ix = int_start3(n_int) + ii
        do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
         do iele = ist, ied
!
!  -----  set weighting function
!
          inertia(iproc) = an(k1,ix) * xmom_order2                      &
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
          induct2(iproc) = an(k1,ix) * xmom_order2                      &
                          * ((elen_dx2_ele(iele) *  dbx(iele,1)         &
     &                      + elen_dxdy_ele(iele) * dbx(iele,2)         &
     &                      + elen_dzdx_ele(iele) * dbx(iele,3))        &
     &                       * dnx(iele,k2,ix,1)                        &
     &                     + (elen_dxdy_ele(iele) * dbx(iele,1)         &
     &                      + elen_dy2_ele(iele) *  dbx(iele,2)         &
     &                      + elen_dydz_ele(iele) * dbx(iele,3))        &
     &                      * dnx(iele,k2,ix,2)                         &
     &                     + (elen_dzdx_ele(iele) * dbx(iele,1)         &
     &                      + elen_dydz_ele(iele) * dbx(iele,2)         &
     &                      + elen_dz2_ele(iele) *  dbx(iele,3))        &
     &                       * dnx(iele,k2,ix,3) )
!
              sk_v(iele,nd,k1) = sk_v(iele,nd,k1)                       &
     &            + ( induct2(iproc) * vect_sgs(iele,2)                 &
     &            -   inertia(iproc) * vect_sgs(iele,1)  )              &
     &            * xjac(iele,ix) * owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_sgs_induct_t_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_upw(numele, nnod_4_e1, nnod_4_e2, &
     &          iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, an, dnx, &
     &          dt, xmom_order2, nele_filter_mom,                       &
     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,             &
     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,            &
     &          vect_sgs, vxe, dvx, dbx, nd, sk_v)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: nd, k2
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
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vect_sgs(numele,3)
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: dvx(numele,3)
      real(kind=kreal), intent(in) :: dbx(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind=kreal) :: tau(np_smp)
      real(kind=kreal) :: inertia(np_smp), induct2(np_smp)

!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied) 
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
!
!  -----  set weighting function
!
              tau(iproc) = an(k1,ix)                                    &
     &                   + half * dt                                    &
     &                  * ( vxe(iele,1)*dnx(iele,k1,ix,1)               &
     &                   + vxe(iele,2)*dnx(iele,k1,ix,2)                &
     &                   + vxe(iele,3)*dnx(iele,k1,ix,3) )
!
              inertia(iproc) = tau(iproc) * xmom_order2                 &
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
              induct2(iproc) = tau(iproc) * xmom_order2                 &
                          * ((elen_dx2_ele(iele) *  dbx(iele,1)         &
     &                      + elen_dxdy_ele(iele) * dbx(iele,2)         &
     &                      + elen_dzdx_ele(iele) * dbx(iele,3))        &
     &                       * dnx(iele,k2,ix,1)                        &
     &                     + (elen_dxdy_ele(iele) * dbx(iele,1)         &
     &                      + elen_dy2_ele(iele) *  dbx(iele,2)         &
     &                      + elen_dydz_ele(iele) * dbx(iele,3))        &
     &                      * dnx(iele,k2,ix,2)                         &
     &                     + (elen_dzdx_ele(iele) * dbx(iele,1)         &
     &                      + elen_dydz_ele(iele) * dbx(iele,2)         &
     &                      + elen_dz2_ele(iele) *  dbx(iele,3))        &
     &                       * dnx(iele,k2,ix,3) )
!
              sk_v(iele,nd,k1) = sk_v(iele,nd,k1)                       &
     &            + ( induct2(iproc) * vect_sgs(iele,2)                 &
     &            -   inertia(iproc) * vect_sgs(iele,1) )               &
     &             * xjac(iele,ix) * owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_sgs_induct_t_upw
!
!-----------------------------------------------------------------------
!
      end module fem_skv_sgs_induct_t
