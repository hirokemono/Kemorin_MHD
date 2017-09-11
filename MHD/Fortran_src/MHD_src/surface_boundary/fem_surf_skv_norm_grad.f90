!
!      module fem_surf_skv_norm_grad
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine fem_surf_skv_norm_grad_pg(np_smp, numele,            &
!!     &          nnod_4_e1, nnod_4_sf1, node_on_sf,                    &
!!     &          num_surf, num_surf_bc, surf_istack, surf_item,        &
!!     &          num_surf_smp, isurf_grp_smp_stack,  nmax_surf,        &
!!     &          nmax_ele_surf, ngrp_sf, id_grp_sf, ist_surf, sf_apt,  &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          n_int, nd, ntot_int_sf_grp, xj_sf, an1_sf, ak_d, sk_v)
!!      subroutine fem_surf_skv_norm_poisson(np_smp, numele,            &
!!     &          nnod_4_e1, nnod_4_sf1, node_on_sf,                    &
!!     &          num_surf, num_surf_bc, surf_istack, surf_item,        &
!!     &          num_surf_smp, isurf_grp_smp_stack, nmax_surf,         &
!!     &          nmax_ele_surf, ngrp_sf, id_grp_sf, ist_surf, sf_apt,  &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          n_int, ntot_int_sf_grp, xj_sf, an1_sf, sk_v)
!!
!!      subroutine fem_surf_skv_trq_sph_out_pg                          &
!!     &         (np_smp, numele, nnod_4_e1, nnod_4_sf1, nnod_4_sf2,    &
!!     &          node_on_sf, num_surf_bc, surf_item,                   &
!!     &          num_surf_smp,  isurf_grp_smp_stack,                   &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          igrp, k2, n_int, ntot_int_sf_grp, xj_sf,              &
!!     &          an1_sf, an2_sf, ak_d_velo, xe_sf, vect_sf, sk_v)
!
      module fem_surf_skv_norm_grad
!
      use m_precision
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
      subroutine fem_surf_skv_norm_grad_pg(np_smp, numele,              &
     &          nnod_4_e1, nnod_4_sf1, node_on_sf,                      &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          num_surf_smp, isurf_grp_smp_stack,  nmax_surf,          &
     &          nmax_ele_surf, ngrp_sf, id_grp_sf, ist_surf, sf_apt,    &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          n_int, nd, ntot_int_sf_grp, xj_sf, an1_sf, ak_d, sk_v)
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_e1
      integer(kind = kint), intent(in) :: nnod_4_sf1
      integer (kind = kint), intent(in)                                 &
     &                      :: node_on_sf(nnod_4_sf1,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf, num_surf_bc
      integer (kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in) :: num_surf_smp
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: nmax_surf, ngrp_sf
      integer (kind = kint), intent(in) :: nmax_ele_surf
      integer (kind = kint), intent(in) :: id_grp_sf(nmax_surf)
      integer (kind = kint), intent(in) :: ist_surf(0:nmax_surf)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      integer (kind = kint), intent(in) :: ntot_int_sf_grp, n_int
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xj_sf(num_surf_bc,ntot_int_sf_grp)
!
      integer (kind = kint), intent(in) :: nd
      real (kind=kreal), intent(in) :: sf_apt(nmax_ele_surf)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf, isf
      integer (kind = kint) :: ist, ied, inum, iele, idat
      integer (kind = kint) :: k1, kk1, ii, ix
!
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
!
        nsf = surf_istack(igrp) - surf_istack(igrp-1)
        if (nsf.gt.0) then
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,idat)
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
!OCL VECTOR, NOVREC
!VOPTION INDEP, VEC
                do inum = ist, ied
                  iele = surf_item(1,inum)
                  isf = surf_item(2,inum)
                  kk1 = node_on_sf(k1,isf)
                  idat = ist_surf(i-1) + inum - surf_istack(igrp-1)
!
! -------  caliculate
!
                  sk_v(iele,nd,kk1) = sk_v(iele,nd,kk1)                 &
     &                          + ak_d(iele) * sf_apt(idat)             &
     &                         * an1_sf(k1,ix)*xj_sf(inum,ix)*owe2d(ix)
                end do
              end do
            end do
!
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine fem_surf_skv_norm_grad_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_poisson(np_smp, numele,              &
     &          nnod_4_e1, nnod_4_sf1, node_on_sf,                      &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          num_surf_smp, isurf_grp_smp_stack, nmax_surf,           &
     &          nmax_ele_surf, ngrp_sf, id_grp_sf, ist_surf, sf_apt,    &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          n_int, ntot_int_sf_grp, xj_sf, an1_sf, sk_v)
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_e1
      integer(kind = kint), intent(in) :: nnod_4_sf1
      integer (kind = kint), intent(in)                                 &
     &                      :: node_on_sf(nnod_4_sf1,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf, num_surf_bc
      integer (kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in) :: num_surf_smp
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: nmax_surf, ngrp_sf
      integer (kind = kint), intent(in) :: nmax_ele_surf
      integer (kind = kint), intent(in) :: id_grp_sf(nmax_surf)
      integer (kind = kint), intent(in) :: ist_surf(0:nmax_surf)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      integer (kind = kint), intent(in) :: ntot_int_sf_grp, n_int
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xj_sf(num_surf_bc,ntot_int_sf_grp)
!
      real (kind=kreal), intent(in) :: sf_apt(nmax_ele_surf)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf, isf
      integer (kind = kint) :: ist, ied, inum, iele, idat
      integer (kind = kint) :: k1, kk1, ii, ix
!
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
!
        nsf = surf_istack(igrp) - surf_istack(igrp-1)
        if (nsf.gt.0) then
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,idat)
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
!OCL VECTOR, NOVREC
!VOPTION INDEP, VEC
                do inum = ist, ied
                  iele = surf_item(1,inum)
                  isf = surf_item(2,inum)
                  kk1 = node_on_sf(k1,isf)
                  idat = ist_surf(i-1) + inum - surf_istack(igrp-1)
!
! -------  caliculate
!
                  sk_v(iele,1,kk1) = sk_v(iele,1,kk1)                   &
     &                          + sf_apt(idat) * an1_sf(k1,ix)          &
     &                           * xj_sf(inum,ix)*owe2d(ix)
                end do
              end do
            end do
!
          end do
!$omp end parallel do
!
        end if
      end do
!
      end subroutine fem_surf_skv_norm_poisson
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_surf_skv_trq_sph_out_pg                            &
     &         (np_smp, numele, nnod_4_e1, nnod_4_sf1, nnod_4_sf2,      &
     &          node_on_sf, num_surf_bc, surf_item,                     &
     &          num_surf_smp,  isurf_grp_smp_stack,                     &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          igrp, k2, n_int, ntot_int_sf_grp, xj_sf,                &
     &          an1_sf, an2_sf, ak_d_velo, xe_sf, vect_sf, sk_v)
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
      integer (kind = kint), intent(in) :: igrp, k2
      integer (kind = kint), intent(in) :: ntot_int_sf_grp, n_int
      real (kind=kreal), intent(in)                                     &
     &                  :: an1_sf(nnod_4_sf1,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: an2_sf(nnod_4_sf2,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xj_sf(num_surf_bc,ntot_int_sf_grp)
!
      real (kind=kreal), intent(in) :: xe_sf(num_surf_bc,4,nnod_4_sf2)
      real (kind=kreal), intent(in) :: vect_sf(num_surf_bc,3)
      real (kind=kreal), intent(in) :: ak_d_velo(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      integer (kind = kint) :: id_sf, iproc
      integer (kind = kint) :: ist, ied, iele, isf, inum
      integer (kind = kint) :: ii, ix, k1, kk1
!
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,ii,ix) 
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
              sk_v(iele,1,kk1) = sk_v(iele,1,kk1)                       &
     &              + ak_d_velo(iele) * an1_sf(k1,ix) * an2_sf(k2,ix)   &
     &              * vect_sf(inum,1) * xe_sf(inum,4,k2)                &
     &              * xj_sf(id_sf,ix) * owe2d(ix) * half
              sk_v(iele,2,kk1) = sk_v(iele,2,kk1)                       &
     &              + ak_d_velo(iele) * an1_sf(k1,ix) * an2_sf(k2,ix)   &
     &              * vect_sf(inum,2) * xe_sf(inum,4,k2)                &
     &              * xj_sf(id_sf,ix) * owe2d(ix) * half
              sk_v(iele,3,kk1) = sk_v(iele,3,kk1)                       &
     &              + ak_d_velo(iele) * an1_sf(k1,ix) * an2_sf(k2,ix)   &
     &              * vect_sf(inum,3) * xe_sf(inum,4,k2)                &
     &              * xj_sf(id_sf,ix) * owe2d(ix) * half
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_skv_trq_sph_out_pg
!
! ----------------------------------------------------------------------
!
      end module fem_surf_skv_norm_grad
