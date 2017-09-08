!fem_skv_inertia3_sgsmod_upw.f90
!     module fem_skv_inertia3_sgsmod_upw
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine fem_skv_vect_inertia_modsgs_upw                      &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &         max_int_point, maxtot_int_3d, int_start3, owe3d,       &
!!     &         n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, dnx2,     &
!!     &         xmom_order2, nele_fmom,                                &
!!     &         elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,   &
!!     &         elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,  &
!!     &         ak_diff, vector_e, sgs_e, flux_e, vxe, vxe_up, sk_v)
!
      module fem_skv_inertia3_sgsmod_upw
!
      use m_precision
!
      use m_constants
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vect_inertia_modsgs_upw                        &
     &          (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack, &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, dnx2,      &
     &          xmom_order2, nele_fmom,                                 &
     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,    &
     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,   &
     &          ak_diff, vector_e, sgs_e, flux_e, vxe, vxe_up, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
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
      real (kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: flux_e(numele,6)
      real (kind=kreal), intent(in) :: sgs_e(numele,6)
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: vxe_up(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind=kreal) :: inertia, tau
      real(kind=kreal) :: div_x,  div_y,  div_z
      real(kind=kreal) :: div_cx, div_cy, div_cz
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,tau,inertia,        &
!$omp&                    div_x,div_y,div_z,div_cx,div_cy,div_cz)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  ------  set inertia term
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe_up(iele,1)*dnx1(iele,k1,ix,1)           &
     &                    + vxe_up(iele,2)*dnx1(iele,k1,ix,2)           &
     &                    + vxe_up(iele,3)*dnx1(iele,k1,ix,3) )
!
              div_cx = half * xmom_order2                               &
     &          * ( ( elen_dx2_ele_dx(iele,1)* dnx1(iele,k1,ix,1)       &
     &             +  elen_dxdy_ele_dx(iele,1)*dnx1(iele,k1,ix,2)       &
     &             +  elen_dzdx_ele_dx(iele,1)*dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,1)                                   &
     &            + ( elen_dxdy_ele_dx(iele,1)*dnx1(iele,k1,ix,1)       &
     &             +  elen_dy2_ele_dx(iele,1)* dnx1(iele,k1,ix,2)       &
     &             +  elen_dydz_ele_dx(iele,1)*dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,2)                                   &
     &            + ( elen_dzdx_ele_dx(iele,1)*dnx1(iele,k1,ix,1)       &
     &             +  elen_dydz_ele_dx(iele,1)*dnx1(iele,k1,ix,2)       &
     &             +  elen_dz2_ele_dx(iele,1)* dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,3) )

              div_cy = half * xmom_order2                               &
     &          * ( ( elen_dx2_ele_dx(iele,2)* dnx1(iele,k1,ix,1)       &
     &             +  elen_dxdy_ele_dx(iele,2)*dnx1(iele,k1,ix,2)       &
     &             +  elen_dzdx_ele_dx(iele,2)*dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,1)                                   &
     &            + ( elen_dxdy_ele_dx(iele,2)*dnx1(iele,k1,ix,1)       &
     &             +  elen_dy2_ele_dx(iele,2)* dnx1(iele,k1,ix,2)       &
     &             +  elen_dydz_ele_dx(iele,2)*dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,2)                                   &
     &            + ( elen_dzdx_ele_dx(iele,2)*dnx1(iele,k1,ix,1)       &
     &             +  elen_dydz_ele_dx(iele,2)*dnx1(iele,k1,ix,2)       &
     &             +  elen_dz2_ele_dx(iele,2)* dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,3) )

              div_cz = half * xmom_order2                               &
     &          * ( ( elen_dx2_ele_dx(iele,3)* dnx1(iele,k1,ix,1)       &
     &             +  elen_dxdy_ele_dx(iele,3)*dnx1(iele,k1,ix,2)       &
     &             +  elen_dzdx_ele_dx(iele,3)*dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,1)                                   &
     &            + ( elen_dxdy_ele_dx(iele,3)*dnx1(iele,k1,ix,1)       &
     &             +  elen_dy2_ele_dx(iele,3)* dnx1(iele,k1,ix,2)       &
     &             +  elen_dydz_ele_dx(iele,3)*dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,2)                                   &
     &            + ( elen_dzdx_ele_dx(iele,3)*dnx1(iele,k1,ix,1)       &
     &             +  elen_dydz_ele_dx(iele,3)*dnx1(iele,k1,ix,2)       &
     &             +  elen_dz2_ele_dx(iele,3)* dnx1(iele,k1,ix,3) )     &
     &           * dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              div_x =    tau * (dnx2(iele,k2,ix,1)*sgs_e(iele,1)        &
     &                        + dnx2(iele,k2,ix,2)*sgs_e(iele,2)        &
     &                        + dnx2(iele,k2,ix,3)*sgs_e(iele,3) )
!
              div_y =    tau * (dnx2(iele,k2,ix,1)*sgs_e(iele,2)        &
     &                        + dnx2(iele,k2,ix,2)*sgs_e(iele,4)        &
     &                        + dnx2(iele,k2,ix,3)*sgs_e(iele,5) ) 
!
              div_z =    tau * (dnx2(iele,k2,ix,1)*sgs_e(iele,3)        &
     &                        + dnx2(iele,k2,ix,2)*sgs_e(iele,5)        &
     &                        + dnx2(iele,k2,ix,3)*sgs_e(iele,6) ) 
!
              inertia =    tau * (vxe(iele,1)*dnx2(iele,k2,ix,1)        &
     &                          + vxe(iele,2)*dnx2(iele,k2,ix,2)        &
     &                          + vxe(iele,3)*dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                 + ( inertia*vector_e(iele,1) + div_x             &
     &                  + ak_diff(iele) * ( div_cx * flux_e(iele,1)     &
     &                                   +  div_cy * flux_e(iele,2)     &
     &                                   +  div_cz * flux_e(iele,3)) )  &
     &                          * xjac(iele,ix)*owe3d(ix)
!
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                        + ( inertia*vector_e(iele,2) + div_y      &
     &                  + ak_diff(iele) * ( div_cx * flux_e(iele,2)     &
     &                                   +  div_cy * flux_e(iele,4)     &
     &                                   +  div_cz * flux_e(iele,5)) )  &
     &                          * xjac(iele,ix)*owe3d(ix)
!
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                        + ( inertia*vector_e(iele,3) + div_z      &
     &                  + ak_diff(iele) * ( div_cx * flux_e(iele,3)     &
     &                                   +  div_cy * flux_e(iele,5)     &
     &                                   +  div_cz * flux_e(iele,6)) )  &
     &                          * xjac(iele,ix)*owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_vect_inertia_modsgs_upw
!
!-----------------------------------------------------------------------
!
      end module fem_skv_inertia3_sgsmod_upw
