!>@file   t_radial_filtering_data.f90
!!@brief  module t_radial_filtering_data
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate radial filtering
!!
!!@verbatim
!!      subroutine check_radial_filter(sph_rj, r_filter)
!!      subroutine check_radial_filter_func(sph_rj, r_filter)
!!      subroutine const_radial_filter(sph_rj, sph_grps, r_filters)
!!      subroutine dealloc_radial_filter_moms(r_filter)
!!@endverbatim
!
!
      module t_radial_filtering_data
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
      type radial_filters_type
!> filter width
        real(kind = kreal) :: width = 1.0d0
!> Number of moments
        integer(kind = kint) :: num_filter_moments = 5
        integer(kind = kint) :: nfilter_sides = 3
!
        real(kind = kreal), allocatable :: filter_mom(:)
!
!> data structure for filter coefficients table
        type(filter_coefficients_type) :: r_filter
!> data structure for filter coefficients table
        type(filter_coefficients_type) :: wide_filter
!> data structure for filter coefficients table
        type(filter_coefficients_type) :: wide2_filter
      end type radial_filters_type
!
      private :: count_radial_point_4_filter, count_fiiltering_area
      private :: set_filtering_points, cal_radial_fileters
      private :: set_filter_size_by_ave_dr, set_filter_size_by_min_dr
      private :: cal_each_radial_filter_coefs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_radial_filter(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: i, ist, ied
!
!
      if(my_rank .ne. 0) return
        write(*,*)  'r_filter%inod_filter(i)',  r_filter%istack_node
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) i, r_filter%inod_filter(i),                        &
     &                  r_filter%inod_near(ist:ied)
        end do
        write(*,*)  'r_filter%weight(i)'
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),    &
     &               i, r_filter%inod_filter(i),                        &
     &                  r_filter%weight(ist:ied)
        end do
!
      end subroutine check_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine check_radial_filter_func(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: i, ist, ied
!
!
      if(my_rank .ne. 0) return
        write(*,*)  'r_filter%func(i)'
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),    &
     &               i, r_filter%inod_filter(i),                        &
     &                  r_filter%func(ist:ied)
        end do
!
      end subroutine check_radial_filter_func
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_radial_filter(sph_rj, sph_grps, r_filters)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(radial_filters_type), intent(inout) :: r_filters
!
      integer(kind = kint) :: num_OC, kmin_OC, kmax_OC
!
!
      r_filters%r_filter%ngrp_node = 1
      call alloc_num_filtering_comb(np_smp, r_filters%r_filter)
      r_filters%r_filter%group_name(1) = 'outer_core'
!
      call count_radial_point_4_filter(sph_rj, r_filters%r_filter)
!
      call alloc_inod_filter_comb(r_filters%r_filter)
!
      call count_fiiltering_area(r_filters%num_filter_moments, sph_rj,  &
     &    sph_grps%radial_rj_grp, r_filters%r_filter,                   &
     &    num_OC, kmin_OC, kmax_OC)
!
      call alloc_3d_filter_comb(r_filters%r_filter)
      call alloc_3d_filter_func(r_filters%r_filter)
!
      call set_filtering_points(num_OC, kmin_OC, kmax_OC,               &
     &    r_filters%num_filter_moments, r_filters%nfilter_sides,        &
     &    sph_rj, r_filters%r_filter)

      call cal_radial_fileters(kmin_OC, kmax_OC,                        &
     &    r_filters%num_filter_moments, r_filters%nfilter_sides,        &
     &    r_filters%width, r_filters%filter_mom, sph_rj,                &
     &    r_filters%r_filter)
!
      end subroutine const_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine alloc_radial_filter_moms(r_filters)
!
      type(radial_filters_type), intent(inout) :: r_filters
!
!
      r_filters%nfilter_sides = (r_filters%num_filter_moments + 1) / 2
      allocate(r_filters%filter_mom(0:r_filters%num_filter_moments-1))
      r_filters%filter_mom = 0.0d0
!
      end subroutine alloc_radial_filter_moms
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_radial_filter_moms(r_filters)
!
      type(radial_filters_type), intent(inout) :: r_filters
!
!
      deallocate(r_filters%filter_mom)
!
      end subroutine dealloc_radial_filter_moms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_radial_moments(num_moms, num_sides, filter_mom)
!
      integer(kind = kint), intent(in)  :: num_moms, num_sides
      real(kind = kreal), intent(inout) :: filter_mom(0:num_moms-1)
!
      integer(kind = kint) :: imom
!
!
      filter_mom(0) = one
      do imom = 1, num_sides-1
        filter_mom(2*imom-1) = zero
        filter_mom(2*imom  ) = real(2*imom-1) * filter_mom(2*imom-2)
      end do
!
      end subroutine cal_radial_moments
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
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
     &          num_moms, num_sides, width, filter_mom,                 &
     &          sph_rj, r_filter)
!
      integer(kind = kint), intent(in)  :: kmin_OC, kmax_OC
      integer(kind = kint), intent(in)  :: num_moms, num_sides
      real(kind = kreal), intent(in) :: width
      real(kind = kreal), intent(in) :: filter_mom(0:num_moms-1)
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: inum, inod, ist, jstart, imom, i
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
        call cal_each_radial_filter_coefs(sph_rj%radius_1d_rj_r(inod),  &
     &      dr_point, width, num_moms, num_sides,                       &
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
     &         (r_point, dr_point, filter_length, num_moms, num_sides,  &
     &          radius, filter_mom, func, weight)
!
      use radial_int_for_sph_spec
      use m_ludcmp
!
      integer(kind = kint), intent(in)  :: num_moms
      real(kind = kreal), intent(in) :: r_point, dr_point
      real(kind = kreal), intent(in) :: radius(num_moms)
      real(kind = kreal), intent(in) :: filter_mom(0:num_moms-1)
      real(kind = kreal), intent(in) :: filter_length
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
    &        = a_int(1:num_moms) * r_point**2                           &
    &         * dr(1:num_moms)**(2*imom-1)
        a_mat(num_sides+imom,1:num_moms)                                &
    &        = a_int(1:num_moms) * r_point**2                           &
    &         * dr(1:num_moms)**(2*imom)
!
        b(num_sides-imom) = filter_mom(2*imom-1) * r_point**2           &
    &                      * (filter_length*dr_point)**(2*imom-1)
        b(num_sides+imom) = filter_mom(2*imom  ) * r_point**2           &
    &                      * (filter_length*dr_point)**(2*imom  ) 
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
      end module t_radial_filtering_data
