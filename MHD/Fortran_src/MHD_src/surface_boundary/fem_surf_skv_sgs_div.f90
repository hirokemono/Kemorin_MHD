!fem_surf_skv_sgs_div.f90
!      module fem_surf_skv_sgs_div
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine fem_sf_skv_div_flux_commute_p                        &
!!     &         (np_smp, numele, nnod_4_e1, nnod_4_sf1, nnod_4_sf2,    &
!!     &          node_on_sf, num_surf_bc, surf_item,                   &
!!     &          num_surf_smp,  isurf_grp_smp_stack,                   &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_sf_grp, xsf_sf, axj_sf, an1_sf, an2_sf,      &
!!     &          xmom_order2, nele_fmom,                               &
!!     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,  &
!!     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx, &
!!     &          igrp, k2, nd, n_int, dxe_sf, vect_sf, sk_v)
!!
!!      subroutine fem_sf_skv_sgs_div_flux_posi                         &
!!     &         (np_smp, numele, nnod_4_e1, nnod_4_sf1, nnod_4_sf2,    &
!!     &          node_on_sf, num_surf_bc, surf_item,                   &
!!     &          num_surf_smp, isurf_grp_smp_stack,                    &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_sf_grp, xsf_sf, axj_sf, an1_sf, an2_sf,      &
!!     &          xmom_order2, nele_fmom,                               &
!!     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,  &
!!     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx, &
!!     &          igrp, k2, nd, n_int, dxe_sf, vect_sf, ak_diff,        &
!!     &          coef, sk_v)
!
      module fem_surf_skv_sgs_div
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
      subroutine fem_sf_skv_div_flux_commute_p                          &
     &         (np_smp, numele, nnod_4_e1, nnod_4_sf1, nnod_4_sf2,      &
     &          node_on_sf, num_surf_bc, surf_item,                     &
     &          num_surf_smp,  isurf_grp_smp_stack,                     &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_sf_grp, xsf_sf, axj_sf, an1_sf, an2_sf,        &
     &          xmom_order2, nele_fmom,                                 &
     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,    &
     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,   &
     &          igrp, k2, nd, n_int, dxe_sf, vect_sf, sk_v)
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_e1
      integer(kind = kint), intent(in) :: nnod_4_sf1, nnod_4_sf2
      integer (kind = kint), intent(in)                                 &
     &                      :: node_on_sf(nnod_4_sf1,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      integer (kind = kint), intent(in) :: ntot_int_sf_grp, n_int
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: an2_sf(nnod_4_sf2,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xsf_sf(num_surf_bc,ntot_int_sf_grp,3)
      real (kind=kreal), intent(in)                                     &
     &                  :: axj_sf(num_surf_bc,ntot_int_sf_grp)
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
      integer(kind = kint), intent(in) :: igrp, k2, nd
!
      real (kind=kreal), intent(in) :: dxe_sf(num_surf_bc,4,nnod_4_sf2)
      real (kind=kreal), intent(in) :: vect_sf(num_surf_bc,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind=kreal) :: diff_x, diff_y, diff_z
!
      integer (kind = kint) :: iproc, id_sf, isf
      integer (kind = kint) :: ist, ied, inum, iele
      integer (kind = kint) :: ii, ix, k1, kk1
!
!
!$omp parallel do private(id_sf,ist,ied,inum,iele,isf,k1,kk1,ii,ix,     &
!$omp&                    diff_x,diff_y,diff_z)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
        do ii = 1, n_int * n_int
          ix = int_start2(n_int) + ii
          do k1 = 1, nnod_4_sf1
!
!cdir nodep
!VOPTION INDEP, VEC
            do inum = ist, ied
              iele = surf_item(1,inum)
              isf = surf_item(2,inum)
              kk1 = node_on_sf(k1,isf)
!
              diff_x =    elen_dx2_ele_dx(iele,1)                       &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,1)          &
     &                  + elen_dxdy_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,2)          &
     &                  + elen_dzdx_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,3)          &
     &                  + elen_dxdy_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,1)          &
     &                  +  elen_dy2_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,2)          &
     &                  + elen_dydz_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,3)          &
     &                  + elen_dzdx_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,1)          &
     &                  + elen_dydz_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,2)          &
     &                  +  elen_dz2_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,3)
!
              diff_y =    elen_dx2_ele_dx(iele,2)                       &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,1)          &
     &                  + elen_dxdy_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,2)          &
     &                  + elen_dzdx_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,3)          &
     &                  + elen_dxdy_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,1)          &
     &                  +  elen_dy2_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,2)          &
     &                  + elen_dydz_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,3)          &
     &                  + elen_dzdx_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,1)          &
     &                  + elen_dydz_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,2)          &
     &                  +  elen_dz2_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,3)
!
              diff_z =    elen_dx2_ele_dx(iele,3)                       &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,1)          &
     &                  + elen_dxdy_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,2)          &
     &                  + elen_dzdx_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,3)          &
     &                  + elen_dxdy_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,1)          &
     &                  +  elen_dy2_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,2)          &
     &                  + elen_dydz_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,3)          &
     &                  + elen_dzdx_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,1)          &
     &                  + elen_dydz_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,2)          &
     &                  +  elen_dz2_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,3)
!
              sk_v(iele,nd,kk1) = sk_v(iele,nd,kk1)                     &
     &                  - an1_sf(k1,ix) * an2_sf(k2,ix)                 &
     &                    * half * xmom_order2 * dxe_sf(inum,4,k2)      &
     &                    * (diff_x * vect_sf(inum,1)                   &
     &                     + diff_y * vect_sf(inum,2)                   &
     &                     + diff_z * vect_sf(inum,3) )                 &
     &                    * owe2d(ix) * axj_sf(inum,ix)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_sf_skv_div_flux_commute_p
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_div_flux_posi                           &
     &         (np_smp, numele, nnod_4_e1, nnod_4_sf1, nnod_4_sf2,      &
     &          node_on_sf, num_surf_bc, surf_item,                     &
     &          num_surf_smp, isurf_grp_smp_stack,                      &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_sf_grp, xsf_sf, axj_sf, an1_sf, an2_sf,        &
     &          xmom_order2, nele_fmom,                                 &
     &          elen_dx2_ele_dx,  elen_dy2_ele_dx,  elen_dz2_ele_dx,    &
     &          elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,   &
     &          igrp, k2, nd, n_int, dxe_sf, vect_sf, ak_diff,          &
     &          coef, sk_v)
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_e1
      integer(kind = kint), intent(in) :: nnod_4_sf1, nnod_4_sf2
      integer (kind = kint), intent(in)                                 &
     &                      :: node_on_sf(nnod_4_sf1,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      integer (kind = kint), intent(in) :: ntot_int_sf_grp, n_int
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: an2_sf(nnod_4_sf2,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xsf_sf(num_surf_bc,ntot_int_sf_grp,3)
      real (kind=kreal), intent(in)                                     &
     &                  :: axj_sf(num_surf_bc,ntot_int_sf_grp)
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
      integer(kind = kint), intent(in) :: igrp, k2, nd
!
      real (kind=kreal), intent(in) :: dxe_sf(num_surf_bc,4,nnod_4_sf2)
      real (kind=kreal), intent(in) :: vect_sf(num_surf_bc,3)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real(kind=kreal) :: diff_x, diff_y, diff_z
!
      integer (kind = kint) :: iproc, id_sf, isf
      integer (kind = kint) :: ist, ied, inum, iele
      integer (kind = kint) :: ii, ix, k1, kk1
!
!
!$omp parallel do private(id_sf,ist,ied,inum,iele,isf,k1,kk1,ii,ix,     &
!$omp&                    diff_x,diff_y,diff_z)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
        do ii = 1, n_int * n_int
          ix = int_start2(n_int) + ii
          do k1 = 1, nnod_4_sf1
!
!cdir nodep
!VOPTION INDEP, VEC
            do inum = ist, ied
              iele = surf_item(1,inum)
              isf = surf_item(2,inum)
              kk1 = node_on_sf(k1,isf)
!
              diff_x =    elen_dx2_ele_dx(iele,1)                       &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,1)          &
     &                  + elen_dxdy_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,2)          &
     &                  + elen_dzdx_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,3)          &
     &                  + elen_dxdy_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,1)          &
     &                  +  elen_dy2_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,2)          &
     &                  + elen_dydz_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,3)          &
     &                  + elen_dzdx_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,1)          &
     &                  + elen_dydz_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,2)          &
     &                  +  elen_dz2_ele_dx(iele,1)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,3)
!
              diff_y =    elen_dx2_ele_dx(iele,2)                       &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,1)          &
     &                  + elen_dxdy_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,2)          &
     &                  + elen_dzdx_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,3)          &
     &                  + elen_dxdy_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,1)          &
     &                  +  elen_dy2_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,2)          &
     &                  + elen_dydz_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,3)          &
     &                  + elen_dzdx_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,1)          &
     &                  + elen_dydz_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,2)          &
     &                  +  elen_dz2_ele_dx(iele,2)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,3)
!
              diff_z =    elen_dx2_ele_dx(iele,3)                       &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,1)          &
     &                  + elen_dxdy_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,2)          &
     &                  + elen_dzdx_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,1)*xsf_sf(inum,ix,3)          &
     &                  + elen_dxdy_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,1)          &
     &                  +  elen_dy2_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,2)          &
     &                  + elen_dydz_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,2)*xsf_sf(inum,ix,3)          &
     &                  + elen_dzdx_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,1)          &
     &                  + elen_dydz_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,2)          &
     &                  +  elen_dz2_ele_dx(iele,3)                      &
     &                   * xsf_sf(inum,ix,3)*xsf_sf(inum,ix,3)
!
              sk_v(iele,nd,kk1) = sk_v(iele,nd,kk1)                     &
     &                  - coef * ak_diff(iele)                          &
     &                    * an1_sf(k1,ix) * an2_sf(k2,ix)               &
     &                    * half * xmom_order2 * dxe_sf(inum,4,k2)      &
     &                    * (diff_x * vect_sf(inum,1)                   &
     &                     + diff_y * vect_sf(inum,2)                   &
     &                     + diff_z * vect_sf(inum,3) )                 &
     &                    *  owe2d(ix) * axj_sf(inum,ix)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_sf_skv_sgs_div_flux_posi
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_sgs_div
