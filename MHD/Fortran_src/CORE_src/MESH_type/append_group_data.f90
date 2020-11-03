!>@file   append_group_data.f90
!!@brief  module append_group_data
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!> @brief Append group data structure
!
!!@verbatim
!!      subroutine s_append_group_data(add_group, target_group)
!!        type(group_data), intent(in) :: add_group
!!        type(group_data), intent(inout) :: target_group
!!      subroutine append_surface_group_data                            &
!!     &         (add_sf_group, target_sf_group)
!!        type(surface_group_data), intent(in) :: add_sf_group
!!        type(surface_group_data), intent(inout) :: target_sf_group
!!@end verbatim
!
      module append_group_data
!
      use m_precision
      use t_group_data
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine s_append_group_data(add_group, target_group)
!
      use copy_mesh_structures
!
      type(group_data), intent(in) :: add_group
      type(group_data), intent(inout) :: target_group
!
      type(group_data) :: grp_tmp
      integer(kind = kint) :: i
!
!
      call copy_group_data(target_group, grp_tmp)
      call dealloc_group(target_group)
!
      target_group%num_grp = grp_tmp%num_grp + add_group%num_grp
      call alloc_group_num(target_group)
!
      target_group%istack_grp(0) =  grp_tmp%istack_grp(0)
!$omp parallel do
      do i = 1, grp_tmp%num_grp
        target_group%grp_name(i) =   grp_tmp%grp_name(i)
        target_group%istack_grp(i) = grp_tmp%istack_grp(i)
      end do
!$omp end parallel do
!$omp parallel do
      do i = 1, add_group%num_grp
        target_group%grp_name(i+grp_tmp%num_grp)                        &
     &      = add_group%grp_name(i)
        target_group%istack_grp(i+grp_tmp%num_grp)                      &
     &      = add_group%istack_grp(i) + grp_tmp%num_item
      end do
!$omp end parallel do
!
      target_group%num_item                                             &
     &     = target_group%istack_grp(target_group%num_grp)
      call alloc_group_item(target_group)
!
!$omp parallel do
      do i = 1, grp_tmp%num_item
        target_group%item_grp(i) =   grp_tmp%item_grp(i)
      end do
!$omp end parallel do
!$omp parallel do
      do i = 1, add_group%num_item
        target_group%item_grp(i+grp_tmp%num_item)                       &
     &     =   add_group%item_grp(i)
      end do
!$omp end parallel do
!
      end subroutine s_append_group_data
!
! ----------------------------------------------------------------------
!
      subroutine append_surface_group_data                              &
     &         (add_sf_group, target_sf_group)
!
      use copy_mesh_structures
!
      type(surface_group_data), intent(in) :: add_sf_group
      type(surface_group_data), intent(inout) :: target_sf_group
!
      type(surface_group_data) :: sf_grp_tmp
      integer(kind = kint) :: i
!
!
      call copy_surface_group(target_sf_group, sf_grp_tmp)
      call dealloc_sf_group(target_sf_group)
!
      target_sf_group%num_grp                                           &
     &         = sf_grp_tmp%num_grp + add_sf_group%num_grp
      call dealloc_sf_group_num(target_sf_group)
!
      target_sf_group%istack_grp(0) =  sf_grp_tmp%istack_grp(0)
!$omp parallel do
      do i = 1, sf_grp_tmp%num_grp
        target_sf_group%grp_name(i) =   sf_grp_tmp%grp_name(i)
        target_sf_group%istack_grp(i) = sf_grp_tmp%istack_grp(i)
      end do
!$omp end parallel do
!$omp parallel do
      do i = 1, add_sf_group%num_grp
        target_sf_group%grp_name(i+sf_grp_tmp%num_grp)                  &
     &      = add_sf_group%grp_name(i)
        target_sf_group%istack_grp(i+sf_grp_tmp%num_grp)                &
     &      = add_sf_group%istack_grp(i) + sf_grp_tmp%num_item
      end do
!$omp end parallel do
!
      target_sf_group%num_item                                          &
     &   = target_sf_group%istack_grp(target_sf_group%num_grp)
      call dealloc_sf_group_item(target_sf_group)
!
!$omp parallel do
      do i = 1, sf_grp_tmp%num_item
        target_sf_group%item_sf_grp(1:2,i)                              &
     &     = sf_grp_tmp%item_sf_grp(1:2,i)
      end do
!$omp end parallel do
!$omp parallel do
      do i = 1, add_sf_group%num_item
        target_sf_group%item_sf_grp(1:2,i+sf_grp_tmp%num_item)          &
     &     =   add_sf_group%item_sf_grp(1:2,i)
      end do
!$omp end parallel do
!
      end subroutine append_surface_group_data
!
! ----------------------------------------------------------------------
!
      end module append_group_data
