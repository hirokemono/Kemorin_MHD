!int_area_normal_4_surface.f90
!     module int_area_normal_4_surface
!
!      Written by H. Matsui on Aug., 2006
!      Modified by H. Matsui on Jan., 2009
!
!!      subroutine int_normal_all_surf(numsurf, isurf_smp_stack,        &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_2d, n_int, xj_surf, xsf_surf, area_surf,     &
!!     &          a_area_surf, vnorm_surf)
!!      subroutine int_normal_surf_groups(num_surf, num_surf_bc,        &
!!     &          num_surf_smp, isurf_grp_smp_stack,                    &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_2d, n_int, xj_sf_grp, xsf_sf_grp, area_surf, &
!!     &          a_area_surf, vnorm_surf)
!!      subroutine int_surf_area_1_surf_grp                             &
!!     &         (numele, numsurf, isf_4_ele, interior_ele,             &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_2d, num_int, xj_surf, num_sgrp, isurf_grp,   &
!!     &          area)
!
      module int_area_normal_4_surface
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
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
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_2d, n_int, xj_surf, xsf_surf, area_surf,       &
     &          a_area_surf, vnorm_surf)
!
      integer (kind = kint), intent(in) :: ntot_int_2d, n_int
      integer (kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numsurf
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
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
!$omp parallel workshare
      vnorm_surf(1:numsurf,1) =  0.0d0
      vnorm_surf(1:numsurf,2) =  0.0d0
      vnorm_surf(1:numsurf,3) =  0.0d0
      area_surf(1:numsurf) =     0.0d0
      a_area_surf(1:numsurf) =   0.0d0
!$omp end parallel workshare
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
      subroutine int_normal_surf_groups(num_surf, num_surf_bc,          &
     &          num_surf_smp, isurf_grp_smp_stack,                      &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_2d, n_int, xj_sf_grp, xsf_sf_grp, area_surf,   &
     &          a_area_surf, vnorm_surf)
!
      integer (kind = kint), intent(in) :: ntot_int_2d, n_int
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      real(kind = kreal), intent(in)                                    &
     &           :: xj_sf_grp(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(in)                                    &
     &           :: xsf_sf_grp(num_surf_bc,ntot_int_2d,3)
!
      real(kind = kreal), intent(inout) :: area_surf(num_surf_bc)
      real(kind = kreal), intent(inout) :: a_area_surf(num_surf_bc)
      real(kind = kreal), intent(inout) :: vnorm_surf(num_surf_bc,3)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: i_grp, inum, ix, ii, i
!
!
!$omp parallel workshare
      vnorm_surf(1:num_surf_bc,1) =  0.0d0
      vnorm_surf(1:num_surf_bc,2) =  0.0d0
      vnorm_surf(1:num_surf_bc,3) =  0.0d0
      area_surf(1:num_surf_bc) =     0.0d0
      a_area_surf(1:num_surf_bc) =   0.0d0
!$omp end parallel workshare
!
      do i_grp = 1, num_surf
!$omp parallel do private(i,ist,ied,inum)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = isurf_grp_smp_stack(i-1) + 1
          ied = isurf_grp_smp_stack(i)
!
          do ii = 1, n_int * n_int
            ix = int_start2(n_int) + ii
!
            do inum = ist, ied
              area_surf(inum) = area_surf(inum)                         &
     &                         + xj_sf_grp(inum,ix) * owe2d(ix)
!
              vnorm_surf(inum,1) = vnorm_surf(inum,1)                   &
     &                            + xsf_sf_grp(inum,ix,1) * owe2d(ix)
              vnorm_surf(inum,2) = vnorm_surf(inum,2)                   &
     &                            + xsf_sf_grp(inum,ix,2) * owe2d(ix)
              vnorm_surf(inum,3) = vnorm_surf(inum,3)                   &
     &                            + xsf_sf_grp(inum,ix,3) * owe2d(ix)
            end do
          end do
!
          do inum = ist, ied
            if(area_surf(inum) .eq. 0.0d0) then
              a_area_surf(inum) = 1.0d60
            else
              a_area_surf(inum) = 1.0d0 / area_surf(inum)
            end if
!
            vnorm_surf(inum,1) = vnorm_surf(inum,1)*a_area_surf(inum)
            vnorm_surf(inum,2) = vnorm_surf(inum,2)*a_area_surf(inum)
            vnorm_surf(inum,3) = vnorm_surf(inum,3)*a_area_surf(inum)
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine int_normal_surf_groups
!
!  ---------------------------------------------------------------------
!
      subroutine int_surf_area_1_surf_grp                               &
     &         (numele, numsurf, isf_4_ele, interior_ele,               &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_2d, num_int, xj_surf, num_sgrp, isurf_grp,     &
     &          area)
!
      integer (kind = kint), intent(in) :: numele, numsurf
      integer (kind = kint), intent(in) :: ntot_int_2d, num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer (kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
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
          area = area + dble(interior_ele(iele))                        &
     &                 * xj_surf(isurf,ix)*owe2d(ix)
        end do
      end do
!
      end subroutine int_surf_area_1_surf_grp
!
!  ---------------------------------------------------------------------
!
      end module int_area_normal_4_surface
