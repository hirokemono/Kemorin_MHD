!set_new_2d_element_group.f90
!      module set_new_2d_element_group
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine alloc_r_ele_cubed_sph(numele)
!      subroutine dealloc_r_ele_cubed_sph
!      subroutine count_new_2d_element_group
!      subroutine set_rele_cubed_sph(numnod, numele, ie, radius, r_ele)
!      subroutine set_new_2d_ele_group(ele_grp)
!
      module set_new_2d_element_group
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
!
      implicit none
!
      real(kind = kreal), allocatable, private :: r_ele_sph(:)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_r_ele_cubed_sph(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      allocate(r_ele_sph(numele))
      if(numele .gt. 0) r_ele_sph = 0.0d0
!
      end subroutine alloc_r_ele_cubed_sph
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_r_ele_cubed_sph
!
      deallocate(r_ele_sph)
!
      end subroutine dealloc_r_ele_cubed_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_rele_cubed_sph(numnod, numele, ie, radius, r_ele)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele,num_t_linear)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: r_ele(numele)
!
      integer(kind = kint) :: i,i1,i2,i3,i4,i5,i6,i7,i8
      real(kind = kreal) :: d2,d3,d4,d6,d7,d8
!
!$omp parallel do private(i,i1,i2,i3,i4,i5,i6,i7,i8,d2,d3,d4,d6,d7,d8)
      do i = 1, numele
        i1 = ie(i,1)
        i2 = ie(i,2)
        i3 = ie(i,3)
        i4 = ie(i,4)
        i5 = ie(i,5)
        i6 = ie(i,6)
        i7 = ie(i,7)
        i8 = ie(i,8)
!
        d2 = abs(radius(i2) - radius(i1))
        d3 = abs(radius(i3) - radius(i1))
        d4 = abs(radius(i4) - radius(i1))
        d6 = abs(radius(i6) - radius(i5))
        d7 = abs(radius(i7) - radius(i5))
        d8 = abs(radius(i8) - radius(i5))
        if(      d2.lt.1.0d-10 .and. d3.lt.1.0d-10 .and. d4.lt.1.0d-10  &
     &     .and. d6.lt.1.0d-10 .and. d7.lt.1.0d-10 .and. d8.lt.1.0d-10) &
     &    then
          r_ele_sph(i) = half * (radius(i1) + radius(i5))
        else
          r_ele_sph(i) = r_ele(i)
        end if
      end do
!$omp end parallel do
!
      end subroutine set_rele_cubed_sph
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_new_2d_element_group
!
      use calypso_mpi
      use m_geometry_data
      use m_add_ele_grp_parameter
      use set_ele_grp2_by_2d
!
!
      if (iflag_grping_direction .eq. 0) then
        call count_added_egrp_item                                      &
     &     (ele1%numele, r_ele_sph, ele1%theta_ele,                     &
     &      num_r_ele_grp, minmax_r_ele_grping,                         &
     &      num_t_ele_grp, minmax_t_ele_grping)
!
      else if (iflag_grping_direction .eq. 1) then
        call count_added_egrp_item(ele1%numele, r_ele_sph,  s_ele,      &
     &      num_r_ele_grp, minmax_r_ele_grping,                         &
     &      num_s_ele_grp, minmax_s_ele_grping)
!
      else if (iflag_grping_direction .eq. 2) then
        call count_added_egrp_item                                      &
     &     (ele1%numele, s_ele,  ele1%x_ele(1:ele1%numele,3),           &
     &      num_s_ele_grp, minmax_s_ele_grping,                         &
     &      num_z_ele_grp, minmax_z_ele_grping)
      else if (iflag_grping_direction .eq. 3) then
        call count_added_egrp_item                                      &
     &     (ele1%numele, ele1%x_ele(1:ele1%numele,3), ele1%theta_ele,   &
     &      num_z_ele_grp, minmax_z_ele_grping,                         &
     &      num_t_ele_grp, minmax_t_ele_grping)
      end if
!
      call MPI_allREDUCE(nitem_added_lc, nitem_added_gl, ngrp_added,    &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine count_new_2d_element_group
!
!   --------------------------------------------------------------------
!
      subroutine set_new_2d_ele_group(ele_grp)
!
      use m_add_ele_grp_parameter
      use m_geometry_data
      use t_group_data
      use set_ele_grp2_by_2d
!
      type(group_data), intent(inout) :: ele_grp
!
!
      if (iflag_grping_direction .eq. 0) then
        call const_ele_grp_item_by_2d                                   &
     &     (ele1%numele, r_ele_sph, ele1%theta_ele,                     &
     &      num_r_ele_grp, r_ele_grp_name, minmax_r_ele_grping,         &
     &      num_t_ele_grp, t_ele_grp_name, minmax_t_ele_grping,         &
     &      ele_grp)
!
      else if (iflag_grping_direction .eq. 1) then
        call const_ele_grp_item_by_2d(ele1%numele, r_ele_sph,  s_ele,   &
     &      num_r_ele_grp, r_ele_grp_name, minmax_r_ele_grping,         &
     &      num_s_ele_grp, s_ele_grp_name, minmax_s_ele_grping,         &
     &      ele_grp)

!
      else if (iflag_grping_direction .eq. 2) then
        call const_ele_grp_item_by_2d                                   &
     &     (ele1%numele, s_ele, ele1%x_ele(1:ele1%numele,3),            &
     &      num_s_ele_grp, s_ele_grp_name, minmax_s_ele_grping,         &
     &      num_z_ele_grp, z_ele_grp_name, minmax_z_ele_grping,         &
     &      ele_grp)

      else if (iflag_grping_direction .eq. 3) then
        call const_ele_grp_item_by_2d                                   &
     &     (ele1%numele, ele1%x_ele(1:ele1%numele,3), ele1%theta_ele,   &
     &      num_z_ele_grp, z_ele_grp_name, minmax_z_ele_grping,         &
     &      num_t_ele_grp, t_ele_grp_name, minmax_t_ele_grping,         &
     &      ele_grp)
      end if
!
      end subroutine set_new_2d_ele_group
!
!   --------------------------------------------------------------------
!
      end module set_new_2d_element_group
