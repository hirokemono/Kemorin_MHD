!>@file   cal_radial_filtering_data.f90
!!@brief  module cal_radial_filtering_data
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate radial filtering
!!
!!@verbatim
!!      subroutine count_radial_point_4_filter(sph_rj, r_filter)
!!      subroutine count_fiiltering_area(num_moms, sph_rj,              &
!!     &          radial_rj_grp, r_filter, num_OC, kmin_OC, kmax_OC)
!!      subroutine set_filtering_points(num_OC, kmin_OC, kmax_OC,       &
!!     &           num_moms, num_sides, sph_rj, r_filter)
!!      subroutine cal_radial_fileters(kmin_OC, kmax_OC,                &
!!     &          num_moms, num_sides, filter_mom, sph_rj, r_filter)
!!      subroutine cal_each_radial_filter_coefs                         &
!!     &         (r_point, dr_point, num_moms, num_sides, radius,       &
!!     &         filter_mom, func, weight)
!!@endverbatim
!
!
      module cal_radial_filtering_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_filter_coefficients
      use t_spheric_mesh
      use t_spheric_parameter
!
      implicit none
!
      private :: set_filter_size_by_ave_dr, set_filter_size_by_min_dr
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_radial_point_4_filter(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
!
      r_filter%num_node(1) =    sph_rj%nidx_rj(1)
      r_filter%istack_node(0) = 0
      r_filter%istack_node(1) = r_filter%num_node(1)
      r_filter%ntot_nod =       r_filter%istack_node(1)
!
      end subroutine count_radial_point_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine count_fiiltering_area(num_moms, sph_rj, radial_rj_grp, &
     &          r_filter, num_OC, kmin_OC, kmax_OC)
!
      integer(kind = kint), intent(in)  :: num_moms
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(filter_coefficients_type), intent(inout) :: r_filter
      integer(kind = kint), intent(inout)  :: num_OC, kmin_OC, kmax_OC
!
      integer(kind = kint) :: inum, ist, ied, num, igrp, i
      integer(kind = kint) :: igrp_ocore
!
!
      igrp_ocore = 0
      do igrp = 1, radial_rj_grp%num_grp
        if(r_filter%group_name(1)                                       &
     &         .eq. radial_rj_grp%grp_name(igrp)) then
          igrp_ocore = igrp
          exit
        end if
      end do
!
      ist = radial_rj_grp%istack_grp(igrp_ocore-1) + 1
      ied = radial_rj_grp%istack_grp(igrp_ocore)
      kmin_OC = radial_rj_grp%item_grp(ist)
      kmax_OC = radial_rj_grp%item_grp(ist)
      do inum = ist+1, ied
        kmin_OC = min(kmin_OC, radial_rj_grp%item_grp(inum))
        kmax_OC = max(kmax_OC, radial_rj_grp%item_grp(inum))
      end do
      num_OC = ied - ist + 1
!
      i = 0
      r_filter%istack_near_nod(0) = 0
      do inum = kmin_OC+1, kmax_OC-1
        i = i + 1
        r_filter%inod_filter(i) = inum
        r_filter%nnod_near(i) =   num_moms
      end do
!
      do inum = 1, kmin_OC
        i = i + 1
        r_filter%inod_filter(i) = inum
        r_filter%nnod_near(i) =   ione
      end do
!
      do inum = kmax_OC, sph_rj%nidx_rj(1)
        i = i + 1
        r_filter%inod_filter(i) = inum
        r_filter%nnod_near(i) =   ione
      end do
!
      num = sph_rj%nidx_rj(1)
      do i = 1, num
        r_filter%istack_near_nod(i)                                     &
     &    = r_filter%istack_near_nod(i-1) + r_filter%nnod_near(i)
      end do
      r_filter%ntot_near_nod = r_filter%istack_near_nod(num)
!
      end subroutine count_fiiltering_area
!
! ----------------------------------------------------------------------
!
      subroutine set_filtering_points(num_OC, kmin_OC, kmax_OC,         &
     &           num_moms, num_sides, sph_rj, r_filter)
!
      integer(kind = kint), intent(in)  :: num_OC, kmin_OC, kmax_OC
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in)  :: num_moms, num_sides
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: inum, inod, ist, jstart, item, i, neib
!
!
      i = 0
      do inum = kmin_OC+1, kmax_OC-1
        i = i + 1
        inod = r_filter%inod_filter(i)
        ist =  r_filter%istack_near_nod(i-1)
        neib = r_filter%nnod_near(i)
!
        if((inod-num_sides) .lt. kmin_OC) then
          jstart = kmin_OC - 1
        else if((inod+num_sides) .gt. kmax_OC) then
          jstart = kmax_OC - num_moms
        else
          jstart = inod - num_sides
        end if
!
        do item = 1, neib
          r_filter%inod_near(item+ist) = jstart + item
        end do
      end do
!
      do inum = num_OC-1, sph_rj%nidx_rj(1)
        ist = r_filter%istack_near_nod(inum-1) + 1
        r_filter%inod_near(ist) = r_filter%inod_filter(inum)
        r_filter%func(ist) =   one
        r_filter%weight(ist) = one
      end do
!
      end subroutine set_filtering_points
!
! -----------------------------------------------------------------------
!
      subroutine cal_radial_fileters(kmin_OC, kmax_OC,                  &
     &          num_moms, num_sides, filter_mom, sph_rj, r_filter)
!
      integer(kind = kint), intent(in)  :: kmin_OC, kmax_OC
      integer(kind = kint), intent(in)  :: num_moms, num_sides
      real(kind = kreal), intent(in) :: filter_mom(0:num_moms-1)
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: inum, inod, ist, jstart, i
      real(kind = kreal) :: dr_point
!
!
      i = 0
      do inum = kmin_OC+1, kmax_OC-1
        i = i + 1
        inod =   r_filter%inod_filter(i)
        ist =    r_filter%istack_near_nod(i-1) + 1
        jstart = r_filter%inod_near(ist)
!
!        call set_filter_size_by_ave_dr(inod, sph_rj, dr_point)
        call set_filter_size_by_min_dr(inod, sph_rj, dr_point)
!
        call cal_each_radial_filter_coefs                               &
     &     (sph_rj%radius_1d_rj_r(inod), dr_point, num_moms, num_sides, &
     &      sph_rj%radius_1d_rj_r(jstart), filter_mom,                  &
     &      r_filter%func(ist), r_filter%weight(ist))
      end do
!
      end subroutine cal_radial_fileters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_filter_size_by_ave_dr(kr, sph_rj, dr_point)
!
      integer(kind = kint), intent(in)  :: kr
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(inout) :: dr_point
!
!
        if(kr .eq. 1) then
          dr_point =         sph_rj%radius_1d_rj_r(kr+1)                &
     &                     - sph_rj%radius_1d_rj_r(kr)
        else if(kr .eq. sph_rj%nidx_rj(1)) then
          dr_point =         sph_rj%radius_1d_rj_r(kr)                  &
     &                     - sph_rj%radius_1d_rj_r(kr-1)
        else
          dr_point = half * (sph_rj%radius_1d_rj_r(kr+1)                &
     &                     - sph_rj%radius_1d_rj_r(kr-1))
        end if
!
      end subroutine set_filter_size_by_ave_dr
!
! -----------------------------------------------------------------------
!
      subroutine set_filter_size_by_min_dr(kr, sph_rj, dr_point)
!
      integer(kind = kint), intent(in)  :: kr
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(inout) :: dr_point
!
!
        if(kr .eq. 1) then
          dr_point =         sph_rj%radius_1d_rj_r(kr+1)                &
     &                     - sph_rj%radius_1d_rj_r(kr)
        else if(kr .eq. sph_rj%nidx_rj(1)) then
          dr_point =         sph_rj%radius_1d_rj_r(kr)                  &
     &                     - sph_rj%radius_1d_rj_r(kr-1)
        else
          dr_point = min((sph_rj%radius_1d_rj_r(kr+1)                   &
     &                     - sph_rj%radius_1d_rj_r(kr)),                &
     &                   (sph_rj%radius_1d_rj_r(kr)                     &
     &                     - sph_rj%radius_1d_rj_r(kr-1)))
        end if
!
      end subroutine set_filter_size_by_min_dr
!
! -----------------------------------------------------------------------
!
      subroutine cal_each_radial_filter_coefs                           &
     &         (r_point, dr_point, num_moms, num_sides, radius,         &
     &          filter_mom, func, weight)
!
      use radial_int_for_sph_spec
      use m_ludcmp
!
      integer(kind = kint), intent(in)  :: num_moms
      real(kind = kreal), intent(in) :: r_point, dr_point
      real(kind = kreal), intent(in) :: radius(num_moms)
      real(kind = kreal), intent(in) :: filter_mom(0:num_moms-1)
      real(kind = kreal), intent(inout) :: func(num_moms)
      real(kind = kreal), intent(inout) :: weight(num_moms)
!
      integer(kind = kint) :: num_sides
      integer(kind = kint) :: indx(num_moms)
      real(kind = kreal) :: a_mat(num_moms,num_moms), d
      real(kind = kreal) :: a_int(num_moms)
      real(kind = kreal) :: a_ctr
      real(kind = kreal) :: b(num_moms)
      real(kind = kreal) :: dr(num_moms)
!
      integer(kind = kint) :: imom
!
!
      call radial_int_matrix_by_trapezoid                               &
     &    (num_moms, ione, num_moms, radius, a_int, a_ctr)
!
      do imom = 1, num_moms
        dr(imom) = radius(imom) - r_point
      end do
!
      a_mat(num_sides,1:num_moms)                                       &
     &              = a_int(1:num_moms)
      b(num_sides) = filter_mom(0) 
!
      do imom = 1, num_sides-1
        a_mat(num_sides-imom,1:num_moms)                                &
     &        = a_int(1:num_moms) * r_point**2                          &
     &         * dr(1:num_moms)**(2*imom-1)
        a_mat(num_sides+imom,1:num_moms)                                &
     &        = a_int(1:num_moms) * r_point**2                          &
     &         * dr(1:num_moms)**(2*imom)
!
        b(num_sides-imom) = 0.0d0
        b(num_sides+imom) = filter_mom(2*imom  ) * r_point**2           &
     &                     * dr_point**(2*imom  ) 
      end do
!
      call ludcmp(a_mat, num_moms, num_moms, indx, d)
      call lubksb(a_mat, num_moms, num_moms, indx, b)
!
      func(1:num_moms) = b(1:num_moms)
      weight(1:num_moms) = b(1:num_moms) * a_int(1:num_moms)
!
      end subroutine cal_each_radial_filter_coefs
!
! -----------------------------------------------------------------------
!
      end module cal_radial_filtering_data
