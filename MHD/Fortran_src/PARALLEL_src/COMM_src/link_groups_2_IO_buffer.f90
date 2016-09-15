!>@file   link_groups_2_IO_buffer.f90
!!@brief  module link_groups_2_IO_buffer
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!!
!!@verbatim
!!      subroutine link_group_stack_4_mpi_IO(nloop, group_IO, i_array)
!!      subroutine link_group_item_4_mpi_IO(nloop, group_IO, i_array)
!!      subroutine link_sf_group_stack_4_mpi_IO                         &
!!     &         (nloop, surf_grp_IO, i_array)
!!      subroutine link_sf_group_item_4_mpi_IO                          &
!!     &         (nloop, surf_grp_IO, iv_array)
!!        type(group_data), intent(in) :: group_IO(nloop)
!!        type(surface_group_data), intent(in) :: surf_grp_IO(nloop)
!!        type(intarray_IO), intent(inout) :: i_array(nloop)
!!        type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!!@endverbatim
!
      module link_groups_2_IO_buffer
!
      use m_precision
      use calypso_mpi
!
      use t_group_data
      use t_calypso_mpi_IO_param
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine link_group_stack_4_mpi_IO(nloop, group_IO, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(group_data), intent(in) :: group_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i, ip
!
!
      do i = 1, nloop
        ip = 1 + rank_in_multi_domain(i)
        i_array(i)%num =   group_IO(i)%num_grp
        i_array(i)%i_IO => group_IO(i)%istack_grp
      end do
!
      end subroutine link_group_stack_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_group_item_4_mpi_IO(nloop, group_IO, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(group_data), intent(in) :: group_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i, ip
!
!
      do i = 1, nloop
        ip = 1 + rank_in_multi_domain(i)
        i_array(i)%num =   group_IO(i)%num_item
        i_array(i)%i_IO => group_IO(i)%item_grp
      end do
!
      end subroutine link_group_item_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sf_group_stack_4_mpi_IO                           &
     &         (nloop, surf_grp_IO, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(surface_group_data), intent(in) :: surf_grp_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i, ip
!
!
      do i = 1, nloop
        ip = 1 + rank_in_multi_domain(i)
        i_array(i)%num =   surf_grp_IO(i)%num_grp
        i_array(i)%i_IO => surf_grp_IO(i)%istack_grp
      end do
!
      end subroutine link_sf_group_stack_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sf_group_item_4_mpi_IO                            &
     &         (nloop, surf_grp_IO, iv_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(surface_group_data), intent(in) :: surf_grp_IO(nloop)
      type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!
      integer(kind = kint) :: i, ip
!
!
      do i = 1, nloop
        ip = 1 + rank_in_multi_domain(i)
        iv_array(i)%n1 =    2
        iv_array(i)%n2 =     surf_grp_IO(i)%num_item
        iv_array(i)%iv_IO => surf_grp_IO(i)%item_sf_grp
      end do
!
      end subroutine link_sf_group_item_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      end module link_groups_2_IO_buffer
