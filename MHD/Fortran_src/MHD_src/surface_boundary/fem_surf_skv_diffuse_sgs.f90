!fem_surf_skv_diffuse_sgs.f90
!      module fem_surf_skv_diffuse_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine fem_surf_skv_poisson_sgs                             &
!!     &       (np_smp, numele, nnod_4_e1, nnod_4_e2, nnod_4_sf1,       &
!!     &        node_on_sf, num_surf_bc, surf_item,                     &
!!     &        num_surf_smp, isurf_grp_smp_stack,                      &
!!     &         max_int_point, maxtot_int_3d, int_start3, owe3d,       &
!!     &        ntot_int_3d, xjac, dnx1, dnx2, xmom_order2, nele_fmom,  &
!!     &        elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2, &
!!     &        elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2,&
!!     &        igrp, k2, n_int, ak_diff, phi_sf, sk_v)
!!      subroutine fem_surf_skv_diffusion_sgs                           &
!!     &       (np_smp, numele, nnod_4_e1, nnod_4_e2, nnod_4_sf1,       &
!!     &        node_on_sf, num_surf_bc, surf_item,                     &
!!     &        num_surf_smp, isurf_grp_smp_stack,                      &
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &        ntot_int_3d, xjac, dnx1, dnx2, xmom_order2, nele_fmom,  &
!!     &        elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2, &
!!     &        elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2,&
!!     &        igrp, k2, n_int, ak_diff, vect_sf, ak_d, nd_v, sk_v)
!
      module fem_surf_skv_diffuse_sgs
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
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
      subroutine fem_surf_skv_poisson_sgs                               &
     &        (np_smp, numele, nnod_4_e1, nnod_4_e2, nnod_4_sf1,        &
     &         node_on_sf, num_surf_bc, surf_item,                      &
     &         num_surf_smp, isurf_grp_smp_stack,                       &
     &         max_int_point, maxtot_int_3d, int_start3, owe3d,         &
     &         ntot_int_3d, xjac, dnx1, dnx2, xmom_order2, nele_fmom,   &
     &         elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,  &
     &         elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2, &
     &         igrp, k2, n_int, ak_diff, phi_sf, sk_v)
!
      integer(kind = kint), intent(in) :: np_smp, numele
      integer(kind = kint), intent(in) :: nnod_4_e1, nnod_4_e2
      integer(kind = kint), intent(in) :: nnod_4_sf1
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf(nnod_4_sf1,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: n_int, ntot_int_3d
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
      real (kind=kreal), intent(in) :: phi_sf(num_surf_bc)
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: iproc, id_sf, ist, ied
      integer (kind = kint) :: inum, iele, isf
      integer (kind = kint) :: k1, kk2, ix, ii
      real (kind=kreal) :: diffuse
      real (kind=kreal) :: div_x, div_y, div_z
!
!
!$omp parallel do  private(id_sf,ist,ied,inum,iele,isf,k1,kk2,          &
!$omp&                     ix,ii,diffuse,div_x, div_y, div_z)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
        do ii = 1, n_int * n_int * n_int
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!OCL VECTOR, NOVREC
!VOPTION INDEP, VEC
            do inum = ist, ied
              iele = surf_item(1,inum)
              iele = surf_item(1,inum)
              isf = surf_item(2,inum)
              kk2 =   node_on_sf(k2,isf)
!
              diffuse =   ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)       &
     &                    + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)       &
     &                    + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )
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
              sk_v(iele,1,k1) = sk_v(iele,1,k1) - ( diffuse             &
     &                      + ak_diff(iele) * (div_x + div_y + div_z))  &
     &                     * phi_sf(inum) * xjac(iele,ix)*owe3d(ix)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_skv_poisson_sgs
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffusion_sgs                             &
     &        (np_smp, numele, nnod_4_e1, nnod_4_e2, nnod_4_sf1,        &
     &         node_on_sf, num_surf_bc, surf_item,                      &
     &         num_surf_smp, isurf_grp_smp_stack,                       &
     &         max_int_point, maxtot_int_3d, int_start3, owe3d,         &
     &         ntot_int_3d, xjac, dnx1, dnx2, xmom_order2, nele_fmom,   &
     &         elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,  &
     &         elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2, &
     &         igrp, k2, n_int, ak_diff, vect_sf, ak_d, nd_v, sk_v)
!
      integer(kind = kint), intent(in) :: np_smp, numele
      integer(kind = kint), intent(in) :: nnod_4_e1, nnod_4_e2
      integer(kind = kint), intent(in) :: nnod_4_sf1
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf(nnod_4_sf1,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: igrp, k2, nd_v
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: n_int, ntot_int_3d
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
      real (kind=kreal), intent(in) :: vect_sf(num_surf_bc,3)
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: iproc, id_sf, ist, ied
      integer (kind = kint) :: inum, iele, isf
      integer (kind = kint) :: k1, kk2, ix, ii
      real (kind=kreal) :: diffuse
      real (kind=kreal) :: div_x, div_y, div_z
!
!
!$omp parallel do  private(id_sf,ist,ied,inum,iele,isf,k1,kk2,          &
!$omp&                     ix,ii,diffuse,div_x, div_y, div_z)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
        do ii = 1, n_int * n_int * n_int
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!OCL VECTOR, NOVREC
!VOPTION INDEP, VEC
            do inum = ist, ied
              iele = surf_item(1,inum)
              iele = surf_item(1,inum)
              isf = surf_item(2,inum)
              kk2 =   node_on_sf(k2,isf)
!
              diffuse =   ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)       &
     &                    + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)       &
     &                    + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )
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
              sk_v(iele,nd_v,k1) = sk_v(iele,nd_v,k1)                   &
     &                      - ak_d(iele) * ( diffuse                    &
     &                      + ak_diff(iele) * (div_x + div_y + div_z))  &
     &                     * vect_sf(inum,1) * xjac(iele,ix)*owe3d(ix)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_skv_diffusion_sgs
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_diffuse_sgs
