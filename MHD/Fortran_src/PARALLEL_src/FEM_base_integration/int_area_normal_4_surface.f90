!int_area_normal_4_surface.f90
!     module int_area_normal_4_surface
!
!      Written by H. Matsui on Aug., 2006
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine int_normal_all_surf(numsurf, isurf_smp_stack,         &
!     &           ntot_int_2d, n_int, xj_surf, xsf_surf, area_surf,     &
!     &           a_area_surf, vnorm_surf)
!
!      subroutine int_surf_area_1_surf_grp(numele, numsurf,             &
!     &           isf_4_ele, e_multi, ntot_int_2d, num_int,             &
!     &           xj_surf, num_sgrp, isurf_grp, area)
!
      module int_area_normal_4_surface
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
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
      subroutine int_normal_all_surf(numsurf, isurf_smp_stack,          &
     &           ntot_int_2d, n_int, xj_surf, xsf_surf, area_surf,      &
     &           a_area_surf, vnorm_surf)
!
      integer (kind = kint), intent(in) :: ntot_int_2d, n_int
      integer (kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numsurf
      real(kind = kreal), intent(in)                                    &
     &           :: xj_surf(numsurf,ntot_int_2d)
      real(kind = kreal), intent(in)                                    &
     &           :: xsf_surf(numsurf,ntot_int_2d,3)
!
      real(kind = kreal), intent(inout) :: area_surf(numsurf)
      real(kind = kreal), intent(inout) :: a_area_surf(numsurf)
      real(kind = kreal), intent(inout) :: vnorm_surf(numsurf,3)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: isurf, ix, ii
!
!
      vnorm_surf =  0.0d0
      area_surf =   0.0d0
      a_area_surf = 0.0d0
!
!$omp parallel do private(ist,ied,ii,ix,isurf)
      do ip = 1, np_smp
        ist = isurf_smp_stack(ip-1) + 1
        ied = isurf_smp_stack(ip)
!
        do ii = 1, n_int * n_int
          ix = int_start2(n_int) + ii
!
!cdir noloopchg
          do isurf = ist, ied
!
            area_surf(isurf) = area_surf(isurf)                         &
     &                        + xj_surf(isurf,ix) * owe2d(ix)
!
            vnorm_surf(isurf,1) = vnorm_surf(isurf,1)                   &
     &                         + xsf_surf(isurf,ix,1) * owe2d(ix)
            vnorm_surf(isurf,2) = vnorm_surf(isurf,2)                   &
     &                         + xsf_surf(isurf,ix,2) * owe2d(ix)
            vnorm_surf(isurf,3) = vnorm_surf(isurf,3)                   &
     &                         + xsf_surf(isurf,ix,3) * owe2d(ix)
!
          end do
        end do
!
!cdir noloopchg
        do isurf = ist, ied
          if (area_surf(isurf) .eq. 0.0d0) then
            a_area_surf(isurf) = 1.0d60
          else
            a_area_surf(isurf) = 1.0d0 / area_surf(isurf)
          end if
        end do
!
!cdir noloopchg
        do isurf = ist, ied
          vnorm_surf(isurf,1) = vnorm_surf(isurf,1)*a_area_surf(isurf)
          vnorm_surf(isurf,2) = vnorm_surf(isurf,2)*a_area_surf(isurf)
          vnorm_surf(isurf,3) = vnorm_surf(isurf,3)*a_area_surf(isurf)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine int_normal_all_surf
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_area_1_surf_grp(numele, numsurf,              &
     &           isf_4_ele, e_multi, ntot_int_2d, num_int,              &
     &           xj_surf, num_sgrp, isurf_grp, area)
!
      integer (kind = kint), intent(in) :: numele, numsurf
      integer (kind = kint), intent(in) :: ntot_int_2d, num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer (kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in) :: e_multi(numele)
      real(kind = kreal), intent(in) :: xj_surf(numsurf,ntot_int_2d)
!
      real(kind = kreal), intent(inout) :: area
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: ii, ix
!
!
      area = 0.0d0
      do ii= 1, num_int * num_int
        ix = int_start2(num_int) + ii
!
!$cdir nodep
        do inum = 1, num_sgrp
          iele = isurf_grp(1,inum)
          isf =  isurf_grp(2,inum)
          isurf = abs(isf_4_ele(iele,isf))
!
          area = area + e_multi(iele)*xj_surf(isurf,ix)*owe2d(ix)
        end do
      end do
!
      end subroutine int_surf_area_1_surf_grp
!
!  ---------------------------------------------------------------------
!
      end module int_area_normal_4_surface
