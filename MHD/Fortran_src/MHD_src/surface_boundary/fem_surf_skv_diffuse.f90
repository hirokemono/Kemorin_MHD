!fem_surf_skv_diffuse.f90
!      module fem_surf_skv_diffuse
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine fem_surf_skv_current_diffuse(np_smp, numele,         &
!!     &          nnod_4_e1, nnod_4_sf1, nnod_4_sf2, node_on_sf,        &
!!     &          num_surf_bc, surf_item, num_surf_smp,                 &
!!     &          isurf_grp_smp_stack, igrp, k2, nd, n_int,             &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_sf_grp, xjq_sf, an1_sf, an2_sf,              &
!!     &          dxe_sf, scalar_sf, sk_v)
!!      subroutine fem_surf_skv_diffuse_term(np_smp, numele,            &
!!     &          nnod_4_e1, nnod_4_sf1, nnod_4_sf2, node_on_sf,        &
!!     &          num_surf_bc, surf_item, num_surf_smp,                 &
!!     &          isurf_grp_smp_stack, igrp, k2, nd, n_int,             &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_sf_grp, xjq_sf, an1_sf, an2_sf, ak,          &
!!     &          dxe_sf, scalar_sf, sk_v)
!
      module fem_surf_skv_diffuse
!
      use m_precision
!
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
      subroutine fem_surf_skv_current_diffuse(np_smp, numele,           &
     &          nnod_4_e1, nnod_4_sf1, nnod_4_sf2, node_on_sf,          &
     &          num_surf_bc, surf_item, num_surf_smp,                   &
     &          isurf_grp_smp_stack, igrp, k2, nd, n_int,               &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_sf_grp, xjq_sf, an1_sf, an2_sf,                &
     &          dxe_sf, scalar_sf, sk_v)
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
      real (kind=kreal), intent(in) :: owe2d(maxtot_int_2d)
!
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
      integer (kind = kint), intent(in) :: ntot_int_sf_grp
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: an2_sf(nnod_4_sf2,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xjq_sf(num_surf_bc,ntot_int_sf_grp)
!
      real (kind=kreal), intent(in) :: dxe_sf(num_surf_bc,4,nnod_4_sf2)
      real (kind=kreal), intent(in) :: scalar_sf(num_surf_bc)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: iproc, id_sf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: k1, kk1, ii, ix
!
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,ix,ii)
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
!
              iele = surf_item(1,inum)
              isf = surf_item(2,inum)
              kk1 =   node_on_sf(k1,isf)
!
              sk_v(iele,nd,kk1) = sk_v(iele,nd,kk1)                     &
     &               -  an1_sf(k1,ix) * an2_sf(k2,ix)                   &
     &                * scalar_sf(inum) * dxe_sf(inum,4,k2)             &
     &                * xjq_sf(inum,ix) * owe2d(ix)
!
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_skv_current_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffuse_term(np_smp, numele,              &
     &          nnod_4_e1, nnod_4_sf1, nnod_4_sf2, node_on_sf,          &
     &          num_surf_bc, surf_item, num_surf_smp,                   &
     &          isurf_grp_smp_stack, igrp, k2, nd, n_int,               &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_sf_grp, xjq_sf, an1_sf, an2_sf, ak,            &
     &          dxe_sf, scalar_sf, sk_v)
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
      real (kind=kreal), intent(in) :: owe2d(maxtot_int_2d)
!
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
      integer (kind = kint), intent(in) :: ntot_int_sf_grp
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: an2_sf(nnod_4_sf2,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xjq_sf(num_surf_bc,ntot_int_sf_grp)
!
      real (kind = kreal), intent(in) :: ak(numele)
      real (kind=kreal), intent(in) :: dxe_sf(num_surf_bc,4,nnod_4_sf2)
      real (kind=kreal), intent(in) :: scalar_sf(num_surf_bc)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: iproc, id_sf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: k1, kk1, ii, ix
!
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,ix,ii)
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
              isf =  surf_item(2,inum)
              kk1 =  node_on_sf(k1,isf)
!
              sk_v(iele,nd,kk1) = sk_v(iele,nd,kk1)                     &
     &                          - ak(iele)*an1_sf(k1,ix)*an2_sf(k2,ix)  &
     &                            * scalar_sf(inum) * dxe_sf(inum,4,k2) &
     &                            * xjq_sf(inum,ix) * owe2d(ix)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_skv_diffuse_term
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_diffuse
