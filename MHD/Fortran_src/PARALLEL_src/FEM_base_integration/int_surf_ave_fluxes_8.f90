!int_surf_ave_fluxes_8.f90
!      module int_surf_ave_fluxes_8
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!
!!      subroutine int_surf_ave_1sgrp_8(numnod, numele, numsurf,        &
!!     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,        &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_2d, num_int, an_surf, xj_surf,               &
!!     &          num_sgrp, isurf_grp, istack_sf_grp_smp, d1_nod, ave_l)
!!
!!      subroutine int_vec_flux_1sgrp_8(numnod, numele, numsurf,        &
!!     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,        &
!!     &          num_sgrp, isurf_grp, istack_sf_grp_smp,               &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_2d, num_int, an_surf, xsf_surf, d1_nod, flux)
!!      subroutine int_vec_tflux_1sgrp_8(numnod, numele, numsurf,       &
!!     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,        &
!!     &          num_sgrp, isurf_grp, istack_sf_grp_smp,               &
!!     &          max_int_point, maxtot_int_2d, int_start2, owe2d,      &
!!     &          ntot_int_2d, num_int, an_surf, xsf_surf, d1_nod,      &
!!     &          flux_l)
!
      module int_surf_ave_fluxes_8
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_surf_ave_1sgrp_8(numnod, numele, numsurf,          &
     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,          &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_2d, num_int, an_surf, xj_surf,                 &
     &          num_sgrp, isurf_grp, istack_sf_grp_smp, d1_nod, ave_l)
!
      integer (kind = kint), intent(in) :: numnod, numele, numsurf
      integer (kind = kint), intent(in) :: nnod_4_surf
      integer (kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer (kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer (kind = kint), intent(in) :: ntot_int_2d, num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer (kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      real(kind= kreal), intent(in) :: an_surf(nnod_4_surf,ntot_int_2d)
      real(kind= kreal), intent(in) :: xj_surf(numsurf,ntot_int_2d)
      real(kind= kreal), intent(in) :: d1_nod(numnod)
!
      real(kind = kreal), intent(inout) :: ave_l
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: ii, ix, ist, ied, ip
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      real(kind = kreal) :: ave_smp(np_smp)
!
!
      ave_smp(1:np_smp) = 0.0d0
!
!$omp parallel do private(ii,ix,inum,iele,isf,isurf,ist,ied,            &
!$omp&                    i1,i2,i3,i4,i5,i6,i7,i8)
      do ip = 1, np_smp
        ist = istack_sf_grp_smp(ip-1) + 1
        ied = istack_sf_grp_smp(ip)
!
        do ii= 1, num_int * num_int
        ix = int_start2(num_int) + ii
!
!$cdir nodep
          do inum = ist, ied
            iele = isurf_grp(1,inum)
            isf =  isurf_grp(2,inum)
            isurf = abs(isf_4_ele(iele,isf))
!
            i1 =  ie_surf(isurf, 1)
            i2 =  ie_surf(isurf, 2)
            i3 =  ie_surf(isurf, 3)
            i4 =  ie_surf(isurf, 4)
            i5 =  ie_surf(isurf, 5)
            i6 =  ie_surf(isurf, 6)
            i7 =  ie_surf(isurf, 7)
            i8 =  ie_surf(isurf, 8)
!
            ave_smp(ip) = ave_smp(ip) + (an_surf(1, ix)*d1_nod(i1 )     &
     &                                 + an_surf(2, ix)*d1_nod(i2 )     &
     &                                 + an_surf(3, ix)*d1_nod(i3 )     &
     &                                 + an_surf(4, ix)*d1_nod(i4 )     &
     &                                 + an_surf(5, ix)*d1_nod(i5 )     &
     &                                 + an_surf(6, ix)*d1_nod(i6 )     &
     &                                 + an_surf(7, ix)*d1_nod(i7 )     &
     &                                 + an_surf(8, ix)*d1_nod(i8 ))    &
     &                               * dble(interior_ele(iele))         &
     &                               * xj_surf(isurf,ix)*owe2d(ix)
          end do
        end do
      end do
!$omp end parallel do
!
      ave_l = 0.0d0
      do ip = 1, np_smp
        ave_l = ave_l + ave_smp(ip)
      end do
!
      end subroutine int_surf_ave_1sgrp_8
!
!  ---------------------------------------------------------------------
!
      subroutine int_vec_flux_1sgrp_8(numnod, numele, numsurf,          &
     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,          &
     &          num_sgrp, isurf_grp, istack_sf_grp_smp,                 &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_2d, num_int, an_surf, xsf_surf, d1_nod, flux)
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: ntot_int_2d, num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      real(kind= kreal), intent(in) :: an_surf(nnod_4_surf,ntot_int_2d)
      real(kind = kreal), intent(in)                                    &
     &           :: xsf_surf(numsurf,ntot_int_2d,3)
      real(kind = kreal), intent(in) :: d1_nod(numnod,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: ii, ix, ip, ist, ied
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      real(kind = kreal) :: sign_surf, flux_ele(3)
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp parallel do private(ii,ix,inum,iele,isf,isurf,ist,ied,            &
!$omp&                    i1,i2,i3,i4,i5,i6,i7,i8,sign_surf,flux_ele)
      do ip = 1, np_smp
        ist = istack_sf_grp_smp(ip-1) + 1
        ied = istack_sf_grp_smp(ip)
        do ii= 1, num_int * num_int
          ix = int_start2(num_int) + ii
!
!$cdir nodep
          do inum = ist, ied
            iele = isurf_grp(1,inum)
            isf =  isurf_grp(2,inum)
            isurf = abs(isf_4_ele(iele,isf))
            sign_surf = dble(isf_4_ele(iele,isf) / isurf)
!
            i1 =  ie_surf(isurf, 1)
            i2 =  ie_surf(isurf, 2)
            i3 =  ie_surf(isurf, 3)
            i4 =  ie_surf(isurf, 4)
            i5 =  ie_surf(isurf, 5)
            i6 =  ie_surf(isurf, 6)
            i7 =  ie_surf(isurf, 7)
            i8 =  ie_surf(isurf, 8)
!
            flux_ele(1) =   (an_surf(1, ix)*d1_nod(i1,1)                &
     &                     + an_surf(2, ix)*d1_nod(i2,1)                &
     &                     + an_surf(3, ix)*d1_nod(i3,1)                &
     &                     + an_surf(4, ix)*d1_nod(i4,1)                &
     &                     + an_surf(5, ix)*d1_nod(i5,1)                &
     &                     + an_surf(6, ix)*d1_nod(i6,1)                &
     &                     + an_surf(7, ix)*d1_nod(i7,1)                &
     &                     + an_surf(8, ix)*d1_nod(i8,1))               &
     &                     * xsf_surf(isurf,ix,1)
!
            flux_ele(2) =   (an_surf(1, ix)*d1_nod(i1,2)                &
     &                     + an_surf(2, ix)*d1_nod(i2,2)                &
     &                     + an_surf(3, ix)*d1_nod(i3,2)                &
     &                     + an_surf(4, ix)*d1_nod(i4,2)                &
     &                     + an_surf(5, ix)*d1_nod(i5,2)                &
     &                     + an_surf(6, ix)*d1_nod(i6,2)                &
     &                     + an_surf(7, ix)*d1_nod(i7,2)                &
     &                     + an_surf(8, ix)*d1_nod(i8,2))               &
     &                    * xsf_surf(isurf,ix,2)
!
            flux_ele(3) =   (an_surf(1, ix)*d1_nod(i1,3)                &
     &                     + an_surf(2, ix)*d1_nod(i2,3)                &
     &                     + an_surf(3, ix)*d1_nod(i3,3)                &
     &                     + an_surf(4, ix)*d1_nod(i4,3)                &
     &                     + an_surf(5, ix)*d1_nod(i5,3)                &
     &                     + an_surf(6, ix)*d1_nod(i6,3)                &
     &                     + an_surf(7, ix)*d1_nod(i7,3)                &
     &                     + an_surf(8, ix)*d1_nod(i8,3))               &
     &                   * xsf_surf(isurf,ix,3) 
!
            flux(inum) = flux(inum)                                     &
     &                  + sign_surf * dble(interior_ele(iele))          &
     &                   * (flux_ele(1) + flux_ele(2) + flux_ele(3))    &
     &                   * owe2d(ix)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine int_vec_flux_1sgrp_8
!
!  ---------------------------------------------------------------------
!
      subroutine int_vec_tflux_1sgrp_8(numnod, numele, numsurf,         &
     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,          &
     &          num_sgrp, isurf_grp, istack_sf_grp_smp,                 &
     &          max_int_point, maxtot_int_2d, int_start2, owe2d,        &
     &          ntot_int_2d, num_int, an_surf, xsf_surf, d1_nod,        &
     &          flux_l)
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: ntot_int_2d, num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      integer (kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_2d
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
      real(kind = kreal),   intent(in) :: owe2d(maxtot_int_2d)
!
      real(kind= kreal), intent(in) :: an_surf(nnod_4_surf,ntot_int_2d)
      real(kind = kreal), intent(in)                                    &
     &           :: xsf_surf(numsurf,ntot_int_2d,3)
      real(kind = kreal), intent(in) :: d1_nod(numnod,3)
!
      real(kind = kreal), intent(inout) :: flux_l
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: ii, ix, ist, ied, ip
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      real(kind = kreal) :: sign_surf, flux_ele(3), flux_smp(np_smp)
!
!
      flux_smp(1:np_smp) = 0.0d0
!
!$omp parallel do private(ii,ix,inum,iele,isf,isurf,ist,ied,            &
!$omp&                    i1,i2,i3,i4,i5,i6,i7,i8,sign_surf,flux_ele)
      do ip = 1, np_smp
        ist = istack_sf_grp_smp(ip-1) + 1
        ied = istack_sf_grp_smp(ip)
!
        do ii= 1, num_int * num_int
        ix = int_start2(num_int) + ii
!
!$cdir nodep
          do inum = ist, ied
            iele = isurf_grp(1,inum)
            isf =  isurf_grp(2,inum)
            isurf = abs(isf_4_ele(iele,isf))
            sign_surf = dble(isf_4_ele(iele,isf) / isurf)
!
            i1 =  ie_surf(isurf, 1)
            i2 =  ie_surf(isurf, 2)
            i3 =  ie_surf(isurf, 3)
            i4 =  ie_surf(isurf, 4)
            i5 =  ie_surf(isurf, 5)
            i6 =  ie_surf(isurf, 6)
            i7 =  ie_surf(isurf, 7)
            i8 =  ie_surf(isurf, 8)
!
            flux_ele(1) =   (an_surf(1, ix)*d1_nod(i1,1)                &
     &                     + an_surf(2, ix)*d1_nod(i2,1)                &
     &                     + an_surf(3, ix)*d1_nod(i3,1)                &
     &                     + an_surf(4, ix)*d1_nod(i4,1)                &
     &                     + an_surf(5, ix)*d1_nod(i5,1)                &
     &                     + an_surf(6, ix)*d1_nod(i6,1)                &
     &                     + an_surf(7, ix)*d1_nod(i7,1)                &
     &                     + an_surf(8, ix)*d1_nod(i8,1))               &
     &                     * xsf_surf(isurf,ix,1)
!
            flux_ele(2) =   (an_surf(1, ix)*d1_nod(i1,2)                &
     &                     + an_surf(2, ix)*d1_nod(i2,2)                &
     &                     + an_surf(3, ix)*d1_nod(i3,2)                &
     &                     + an_surf(4, ix)*d1_nod(i4,2)                &
     &                     + an_surf(5, ix)*d1_nod(i5,2)                &
     &                     + an_surf(6, ix)*d1_nod(i6,2)                &
     &                     + an_surf(7, ix)*d1_nod(i7,2)                &
     &                     + an_surf(8, ix)*d1_nod(i8,2))               &
     &                    * xsf_surf(isurf,ix,2)
!
            flux_ele(3) =   (an_surf(1, ix)*d1_nod(i1,3)                &
     &                     + an_surf(2, ix)*d1_nod(i2,3)                &
     &                     + an_surf(3, ix)*d1_nod(i3,3)                &
     &                     + an_surf(4, ix)*d1_nod(i4,3)                &
     &                     + an_surf(5, ix)*d1_nod(i5,3)                &
     &                     + an_surf(6, ix)*d1_nod(i6,3)                &
     &                     + an_surf(7, ix)*d1_nod(i7,3)                &
     &                     + an_surf(8, ix)*d1_nod(i8,3))               &
     &                   * xsf_surf(isurf,ix,3) 
!
            flux_smp(ip) = flux_smp(ip)                                 &
     &                    + sign_surf * dble(interior_ele(iele))        &
     &                     * (flux_ele(1) + flux_ele(2) + flux_ele(3))  &
     &                     * owe2d(ix)
          end do
        end do
      end do
!$omp end parallel do
!
      flux_l = 0.0d0
      do ip = 1, np_smp
        flux_l = flux_l + flux_smp(ip)
      end do
!
      end subroutine int_vec_tflux_1sgrp_8
!
!  ---------------------------------------------------------------------
!
      end module int_surf_ave_fluxes_8
