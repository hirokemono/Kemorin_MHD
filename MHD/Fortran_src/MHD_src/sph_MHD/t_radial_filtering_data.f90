!t_radial_filtering_data.f90
!      module t_radial_filtering_data
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine alloc_nod_data_4_filter(numnod, filtering_data)
!!      subroutine dealloc_nod_data_4_filter(wk_filter)
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
!> data structure for filter coefficients table
        type(filter_coefficients_type) :: r_filter
!> data structure for filter coefficients table
        type(filter_coefficients_type) :: wide_filter
!> data structure for filter coefficients table
        type(filter_coefficients_type) :: wide2_filter
      end type radial_filters_type
!
      integer(kind = kint), parameter :: num_filter_moments = 5
      integer(kind = kint), parameter                                   &
     &              :: num_filter_sides = (num_filter_moments + 1) / 2
      real(kind = kreal) :: filter_mom(0:num_filter_moments-1)
      real(kind = kreal) :: filter_length = 1.0d0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_SGS_model_sph_mhd(sph_rj, sph_grps, r_filters)
!
      use wider_radial_filter_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(radial_filters_type), intent(inout) :: r_filters
!
!
      call const_radial_filter(sph_rj, sph_grps, r_filters%r_filter)
!
      call cal_wider_fileters(sph_rj, r_filters%r_filter,               &
     &    r_filters%wide_filter, r_filters%wide2_filter)
!
      end subroutine init_SGS_model_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine const_radial_filter(sph_rj, sph_grps, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: igrp_ocore
      integer(kind = kint) :: i, ist, ied
!
!
      r_filter%ngrp_node = 1
      call alloc_num_filtering_comb(np_smp, r_filter)
      r_filter%group_name(1) = 'outer_core'
!
      call count_radial_point_4_filter                                  &
     &   (sph_grps%radial_rj_grp, r_filter, igrp_ocore)
!
      call alloc_inod_filter_comb(r_filter)
!
      call count_fiiltering_area(igrp_ocore, num_filter_moments,        &
     &    sph_grps%radial_rj_grp, r_filter)
!
      call alloc_3d_filter_comb(r_filter)
      call alloc_3d_filter_func(r_filter)
!
      call set_filtering_points                                         &
     &   (num_filter_moments, num_filter_sides, r_filter)
!
      call cal_radial_fileters                                          &
     &   (num_filter_moments, num_filter_sides, sph_rj, r_filter)
!
!      if(my_rank .eq. 0) then
!        write(*,*)  'r_filter%inod_filter(i)'
!        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
!          ist = r_filter%istack_near_nod(i-1) + 1
!          ied = r_filter%istack_near_nod(i)
!          write(*,*) i, r_filter%inod_filter(i),                       &
!     &                  r_filter%inod_near(ist:ied)
!        end do
!        write(*,*)  'r_filter%func(i)'
!        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
!          ist = r_filter%istack_near_nod(i-1) + 1
!          ied = r_filter%istack_near_nod(i)
!          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),   &
!     &               i, r_filter%inod_filter(i),                       &
!     &                  r_filter%func(ist:ied)
!        end do
!        write(*,*)  'r_filter%weight(i)'
!        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
!          ist = r_filter%istack_near_nod(i-1) + 1
!          ied = r_filter%istack_near_nod(i)
!          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),   &
!     &               i, r_filter%inod_filter(i),                       &
!     &                  r_filter%weight(ist:ied)
!        end do
!      end if
!
      end subroutine const_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine count_radial_point_4_filter                            &
     &         (radial_rj_grp, r_filter, igrp_ocore)
!
      type(group_data), intent(in) :: radial_rj_grp
      type(filter_coefficients_type), intent(inout) :: r_filter
      integer(kind = kint), intent(inout)  :: igrp_ocore
!
      integer(kind = kint) :: igrp
!
!
      r_filter%istack_node(0) = 0
      do igrp = 1, radial_rj_grp%num_grp
        if(r_filter%group_name(1)                                       &
     &         .eq. radial_rj_grp%grp_name(igrp)) then
          r_filter%num_node(1) =  radial_rj_grp%istack_grp(igrp)        &
     &                          - radial_rj_grp%istack_grp(igrp-1)
          igrp_ocore = igrp
          exit
        end if
      end do
      r_filter%istack_node(1) = r_filter%num_node(1)
      r_filter%ntot_nod =       r_filter%istack_node(1)
!
      end subroutine count_radial_point_4_filter
!
! ----------------------------------------------------------------------
!
      subroutine count_fiiltering_area(igrp_ocore,                      &
     &          num_moms, radial_rj_grp, r_filter)
!
      use quicksort
!
      integer(kind = kint), intent(in)  :: igrp_ocore
      integer(kind = kint), intent(in)  :: num_moms
      type(group_data), intent(in) :: radial_rj_grp
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: inum, ist, num
!
!
      ist = radial_rj_grp%istack_grp(igrp_ocore-1)
      num = r_filter%num_node(1)
      r_filter%istack_near_nod(0) = 0
      do inum = 1, num
        r_filter%inod_filter(inum) = radial_rj_grp%item_grp(inum+ist)
        r_filter%nnod_near(inum) =   num_moms
        r_filter%istack_near_nod(inum)                                  &
     &    = r_filter%istack_near_nod(inum-1) + r_filter%nnod_near(inum)
      end do
      r_filter%ntot_near_nod = r_filter%istack_near_nod(num)
!
      call quicksort_int(r_filter%ntot_nod, r_filter%inod_filter,       &
     &    ione, r_filter%ntot_nod)
!
      end subroutine count_fiiltering_area
!
! ----------------------------------------------------------------------
!
      subroutine set_filtering_points(num_moms, num_sides, r_filter)
!
      integer(kind = kint), intent(in)  :: num_moms, num_sides
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: num, neib
      integer(kind = kint) :: inum, inod, ist, jstart, item
!
!
      num = r_filter%num_node(1)
      do inum = 1, num
        inod = r_filter%inod_filter(inum)
        ist =  r_filter%istack_near_nod(inum-1)
        neib = r_filter%nnod_near(inum)
!
        if((inod-num_sides) .lt. r_filter%inod_filter(1)) then
          jstart = r_filter%inod_filter(1) - 1
        else if((inod+num_sides) .gt. r_filter%inod_filter(num)) then
          jstart = r_filter%inod_filter(num) - num_moms
        else
          jstart = inod - num_sides
        end if
!
        do item = 1, neib
          r_filter%inod_near(item+ist) = jstart + item
        end do
      end do
!
      end subroutine set_filtering_points
!
! -----------------------------------------------------------------------
!
      subroutine cal_radial_fileters                                    &
     &         (num_moms, num_sides, sph_rj, r_filter)
!
      integer(kind = kint), intent(in)  :: num_moms, num_sides
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: inum, inod, ist, jstart, imom
      real(kind = kreal) :: dr_point
!
!
      filter_mom(0) = one
      do imom = 1, num_sides-1
        filter_mom(2*imom-1) = zero
        filter_mom(2*imom  ) = real(2*imom-1) * filter_mom(2*imom-2)
      end do
!
      do inum = 1, r_filter%num_node(1)
        inod =   r_filter%inod_filter(inum)
        ist =    r_filter%istack_near_nod(inum-1) + 1
        jstart = r_filter%inod_near(ist)
!
!        if(inod .eq. 1) then
!          dr_point =         sph_rj%radius_1d_rj_r(inod+1)             &
!     &                     - sph_rj%radius_1d_rj_r(inod)
!        else if(inod .eq. sph_rj%nidx_rj(1)) then
!          dr_point =         sph_rj%radius_1d_rj_r(inod)               &
!     &                     - sph_rj%radius_1d_rj_r(inod-1)
!        else
!          dr_point = half * (sph_rj%radius_1d_rj_r(inod+1)              &
!     &                     - sph_rj%radius_1d_rj_r(inod-1))
!        end if
!
        if(inod .eq. 1) then
          dr_point =         sph_rj%radius_1d_rj_r(inod+1)              &
     &                     - sph_rj%radius_1d_rj_r(inod)
        else if(inod .eq. sph_rj%nidx_rj(1)) then
          dr_point =         sph_rj%radius_1d_rj_r(inod)                &
     &                     - sph_rj%radius_1d_rj_r(inod-1)
        else
          dr_point = min((sph_rj%radius_1d_rj_r(inod+1)                 &
     &                     - sph_rj%radius_1d_rj_r(inod)),              &
     &                   (sph_rj%radius_1d_rj_r(inod)                   &
     &                     - sph_rj%radius_1d_rj_r(inod-1)))
        end if
!
        call cal_each_radial_filter_coefs(sph_rj%radius_1d_rj_r(inod),  &
     &      dr_point, filter_length, num_moms, num_sides,               &
     &      sph_rj%radius_1d_rj_r(jstart), filter_mom,                  &
     &      r_filter%func(ist), r_filter%weight(ist))
      end do
!
      end subroutine cal_radial_fileters
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
!      call radial_int_matrix_by_simpson                               &
    &    (num_moms, ione, num_moms, radius, a_int, a_ctr)
      if(my_rank .eq. 0) write(*,*) one * dr_point, 'a_int', a_int
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
      if(my_rank .eq. 0) then
        write(*,*) r_point, 'B', b
        do imom = 1, num_moms
          write(*,*) imom, 'A',  a_mat(imom,1:num_moms)  
        end do
      end if

      call ludcmp(a_mat, num_moms, num_moms, indx, d)
      call lubksb(a_mat, num_moms, num_moms, indx, b)
      if(my_rank .eq. 0)  write(*,*) r_point, 'Solution', b
!
      func(1:num_moms) = b(1:num_moms)
      weight(1:num_moms) = b(1:num_moms) * a_int(1:num_moms)
!
      end subroutine cal_each_radial_filter_coefs
!
! -----------------------------------------------------------------------
!
      end module t_radial_filtering_data
