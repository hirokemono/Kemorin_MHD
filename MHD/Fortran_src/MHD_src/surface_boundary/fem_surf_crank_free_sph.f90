!fem_surf_crank_free_sph.f90
!      module fem_surf_crank_free_sph
!
!      stress free boundary in a spherical shell
!     Written by H. Matsui on Sep. 2005
!
!      subroutine fem_surf_crank_free_inside(igrp, k2, num_int, dt,     &
!     &          numele, nnod_4_ele, nnod_4_surf, node_on_sf,           &
!     &          num_surf_bc, num_surf_smp, isurf_grp_smp_stack,        &
!     &          surf_item, ntot_int_sf_grp, aw_sf, xjq_sf, xe_sf,      &
!     &          ak_d_velo, coef_imp, sk_v)
!      subroutine fem_surf_crank_free_outside(igrp, k2, num_int, dt,    &
!     &          numele, nnod_4_ele, nnod_4_surf, node_on_sf,           &
!     &          num_surf_bc, num_surf_smp, isurf_grp_smp_stack,        &
!     &          surf_item, ntot_int_sf_grp, aw_sf, xjq_sf, xe_sf,      &
!    &           ak_d_velo, coef_imp, sk_v)
!
      module fem_surf_crank_free_sph
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_fem_gauss_int_coefs
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
      subroutine fem_surf_crank_free_inside(igrp, k2, num_int, dt,      &
     &          numele, nnod_4_ele, nnod_4_surf, node_on_sf,            &
     &          num_surf_bc, num_surf_smp, isurf_grp_smp_stack,         &
     &          surf_item, ntot_int_sf_grp, aw_sf, xjq_sf, xe_sf,       &
     &          ak_d_velo, coef_imp, sk_v)
!
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer (kind = kint), intent(in) :: igrp, k2, num_int
      integer (kind = kint), intent(in) :: num_surf_bc
      integer (kind = kint), intent(in) :: num_surf_smp
      integer (kind = kint), intent(in)                                 &
     &             :: isurf_grp_smp_stack(0:num_surf_smp)
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      real (kind=kreal), intent(in) :: ak_d_velo(numele)
      real (kind=kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: dt
!
      integer (kind = kint), intent(in) :: ntot_int_sf_grp
      real (kind=kreal), intent(in)                                     &
     &                  :: aw_sf(nnod_4_surf,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &                  :: xjq_sf(num_surf_bc,ntot_int_sf_grp)
      real (kind=kreal), intent(in) :: xe_sf(num_surf_bc,4,nnod_4_surf)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      integer (kind = kint) :: ip, id_sf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: k1, kk1
!
!
!$omp parallel do private(id_sf,ist,ied,inum,iele,isf,k1,kk1) 
      do ip = 1, np_smp
        id_sf = np_smp*(igrp-1) + ip
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
        do ii = 1, num_int * num_int
          ix = int_start2(num_int) + ii
          do k1 = 1, nnod_4_surf
!
!cdir nodep
!VOPTION INDEP, VEC
             do inum = ist, ied
               iele = surf_item(1,inum)
               isf =  surf_item(2,inum)
               kk1 =  node_on_sf(k1,isf)
!
               sk_v(iele,1,kk1) = sk_v(iele,1,kk1)                      &
     &                       - ak_d_velo(iele)                          &
     &                         * aw_sf(k1,ix) * aw_sf(k2,ix)            &
     &                         * xe_sf(inum,4,k2)* xjq_sf(id_sf,ix)     &
     &                         * owe2d(ix) * coef_imp * dt
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_crank_free_inside
!
! -----------------------------------------------------------------------
!
      subroutine fem_surf_crank_free_outside(igrp, k2, num_int, dt,     &
     &          numele, nnod_4_ele, nnod_4_surf, node_on_sf,            &
     &          num_surf_bc, num_surf_smp, isurf_grp_smp_stack,         &
     &          surf_item, ntot_int_sf_grp, aw_sf, xjq_sf, xe_sf,       &
     &          ak_d_velo, coef_imp, sk_v)
!
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer (kind = kint), intent(in) :: igrp, k2, num_int
      integer (kind = kint), intent(in) :: num_surf_bc
      integer (kind = kint), intent(in) :: num_surf_smp
      integer (kind = kint), intent(in)                                 &
     &             :: isurf_grp_smp_stack(0:num_surf_smp)
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      real (kind=kreal), intent(in) :: ak_d_velo(numele)
      real (kind=kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: dt
!
      integer (kind = kint), intent(in) :: ntot_int_sf_grp
      real (kind=kreal), intent(in)                                     &
     &             :: aw_sf(nnod_4_surf,ntot_int_sf_grp)
      real (kind=kreal), intent(in)                                     &
     &             :: xjq_sf(num_surf_bc,ntot_int_sf_grp)
      real (kind=kreal), intent(in) :: xe_sf(num_surf_bc,4,nnod_4_surf)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      integer (kind = kint) :: ip, id_sf
      integer (kind = kint) :: ist, ied, inum, iele, isf
      integer (kind = kint) :: ii, ix
      integer (kind = kint) :: k1, kk1
!
!
!$omp parallel do private(id_sf,ist,ied,inum,iele,isf,k1,kk1) 
      do ip = 1, np_smp
        id_sf = np_smp*(igrp-1) + ip
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
        do ii = 1, num_int * num_int
          ix = int_start2(num_int) + ii
          do k1 = 1, nnod_4_surf
!
!cdir nodep
!VOPTION INDEP, VEC
             do inum = ist, ied
               iele = surf_item(1,inum)
               isf =  surf_item(2,inum)
               kk1 =  node_on_sf(k1,isf)
!
               sk_v(iele,1,kk1) = sk_v(iele,1,kk1)                      &
     &                       + ak_d_velo(iele)                          &
     &                         * aw_sf(k1,ix) * aw_sf(k2,ix)            &
     &                         * xe_sf(inum,4,k2)* xjq_sf(id_sf,ix)     &
     &                         * owe2d(ix) * coef_imp * dt
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine fem_surf_crank_free_outside
!
! -----------------------------------------------------------------------
!
      end module fem_surf_crank_free_sph
