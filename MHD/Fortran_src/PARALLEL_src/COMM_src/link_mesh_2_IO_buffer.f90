!>@file   link_mesh_2_IO_buffer.f90
!!@brief  module link_mesh_2_IO_buffer
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!!
!!@verbatim
!!      subroutine link_node_position_4_mpi_IO(nloop, nod_IO,           &
!!     &          i8_array, v_array)
!!      subroutine link_element_4_mpi_IO(nloop, ele_IO,                 &
!!     &          i8_array, i_array, iv_array)
!!        type(sph_IO_data), intent(in) :: sph_IO(nloop)
!!        type(int8array_IO), intent(inout) :: i8_array(nloop)
!!        type(intarray_IO),  intent(inout) :: i_array(nloop)
!!        type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!!        type(vectarray_IO), intent(inout) :: v_array(nloop)
!!@endverbatim
!
      module link_mesh_2_IO_buffer
!
      use m_precision
      use calypso_mpi
!
      use t_geometry_data
      use t_calypso_mpi_IO_param
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_node_position_4_mpi_IO(nloop, nod_IO,             &
     &          i8_array, v_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(node_data), intent(in) :: nod_IO(nloop)
!
      type(int8array_IO), intent(inout) :: i8_array(nloop)
      type(vectarray_IO), intent(inout) :: v_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i8_array(i)%num =    nod_IO(i)%numnod
        i8_array(i)%i8_IO => nod_IO(i)%inod_global
!
        v_array(i)%n1 = nod_IO(i)%numnod
        v_array(i)%n2 = 3
        v_array(i)%v_IO => nod_IO(i)%xx
      end do
!
      end subroutine link_node_position_4_mpi_IO
!
!------------------------------------------------------------------
!
      subroutine link_element_4_mpi_IO(nloop, ele_IO,                   &
     &          i8_array, i_array, iv_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(element_data), intent(in) :: ele_IO(nloop)
!
      type(int8array_IO), intent(inout) :: i8_array(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
      type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i8_array(i)%num =    ele_IO(i)%numele
        i8_array(i)%i8_IO => ele_IO(i)%iele_global
!
        i_array(i)%num =   ele_IO(i)%numele
        i_array(i)%i_IO => ele_IO(i)%elmtyp
!
        iv_array(i)%n1 = ele_IO(i)%numele
        iv_array(i)%n2 = ele_IO(i)%nnod_4_ele
        iv_array(i)%iv_IO => ele_IO(i)%ie
      end do
!
      end subroutine link_element_4_mpi_IO
!
!------------------------------------------------------------------
!
      end module link_mesh_2_IO_buffer
