!>@file   node_geometry_IO_b.f90
!!@brief  module node_geometry_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for Binary mesh data IO
!!
!!@verbatim
!!      subroutine write_geometry_info_b(nod_IO)
!!        type(node_data), intent(in) :: nod_IO
!!      subroutine write_scalar_in_element_b(nod_IO, sfed_IO)
!!      subroutine write_vector_in_element_b(nod_IO, sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine read_number_of_node_b(bin_flags, nod_IO)
!!      subroutine read_geometry_info_b(bin_flags, nod_IO)
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(node_data), intent(inout) :: nod_IO
!!
!!      subroutine read_scalar_in_element_b(bin_flags, nod_IO, sfed_IO)
!!      subroutine read_vector_in_element_b(bin_flags, nod_IO, sfed_IO)
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module node_geometry_IO_b
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_info_b(nod_IO)
!
      use binary_IO
!
      type(node_data), intent(in) :: nod_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(nod_IO%numnod)
      call write_one_integer_b(nod_IO%internal_node)
!
      num64 = nod_IO%numnod
      call write_mul_int8_b(nod_IO%numnod, nod_IO%inod_global)
      call write_2d_vector_b(num64, n_vector, nod_IO%xx)
!
      end subroutine write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine write_scalar_in_element_b(nod_IO, sfed_IO)
!
      use binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(nod_IO%numnod)
      call write_one_integer_b(nod_IO%internal_node)
!
      num64 = nod_IO%numnod
      call write_1d_vector_b(num64, sfed_IO%ele_scalar)
!
      end subroutine write_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine write_vector_in_element_b(nod_IO, sfed_IO)
!
      use binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(nod_IO%numnod)
      call write_one_integer_b(nod_IO%internal_node)
!
      num64 = nod_IO%numnod
      call write_2d_vector_b                                            &
     &   (num64, n_vector, sfed_IO%ele_vector)
!
      end subroutine write_vector_in_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node_b(bin_flags, nod_IO)
!
      use binary_IO
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(node_data), intent(inout) :: nod_IO
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nod_IO%numnod, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nod_IO%internal_node, bin_flags%ierr_IO)
!
      end subroutine read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info_b(bin_flags, nod_IO)
!
      use binary_IO
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(node_data), intent(inout) :: nod_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call read_mul_int8_b(bin_flags%iflag_bin_swap,                    &
     &    nod_IO%numnod, nod_IO%inod_global, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      num64 = nod_IO%numnod
      call read_2d_vector_b(bin_flags%iflag_bin_swap,                   &
     &    num64, n_vector, nod_IO%xx, bin_flags%ierr_IO)
!
      end subroutine read_geometry_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_scalar_in_element_b(bin_flags, nod_IO, sfed_IO)
!
      use binary_IO
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nod_IO%numnod, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nod_IO%internal_node, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      num64 = nod_IO%numnod
      call read_1d_vector_b(bin_flags%iflag_bin_swap,                   &
     &    num64, sfed_IO%ele_scalar, bin_flags%ierr_IO)
!
      end subroutine read_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine read_vector_in_element_b(bin_flags, nod_IO, sfed_IO)
!
      use binary_IO
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nod_IO%numnod, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nod_IO%internal_node, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      num64 = nod_IO%numnod
      call read_2d_vector_b(bin_flags%iflag_bin_swap,                   &
     &    num64, n_vector, sfed_IO%ele_vector,                          &
     &    bin_flags%ierr_IO)
!
      end subroutine read_vector_in_element_b
!
!------------------------------------------------------------------
!
      end module node_geometry_IO_b
