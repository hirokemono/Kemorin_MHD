!> @file  gz_node_geometry_IO_b.f90
!!      module gz_node_geometry_IO_b
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief Node data IO using zlib
!!
!!@verbatim
!!      subroutine gz_write_geometry_info_b(nod_IO)
!!      subroutine gz_write_scalar_in_element_b(nod_IO, sfed_IO)
!!      subroutine gz_write_vector_in_element_b(nod_IO, sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine gz_read_number_of_node_b(gz_flags, nod_IO)
!!      subroutine gz_read_geometry_info_b(gz_flags, nod_IO)
!!      subroutine gz_read_scalar_in_element_b(gz_flags, nod_IO, sfed_IO)
!!      subroutine gz_read_vector_in_element_b                          &
!!     &         (gz_flags, nod_IO, sfed_IO)
!!        type(file_IO_flags), intent(inout) :: gz_flags
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_node_geometry_IO_b
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_surf_edge_IO
      use binary_IO
      use skip_gz_comment
      use transfer_to_long_integers
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_info_b(nod_IO)
!
      use gz_binary_IO
!
      type(node_data), intent(in) :: nod_IO
!
!
      call gz_write_one_integer_b(nod_IO%numnod)
      call gz_write_one_integer_b(nod_IO%internal_node)
!
      call gz_write_mul_int8_b                                          &
     &   (cast_long(nod_IO%numnod), nod_IO%inod_global)
      call gz_write_2d_vector_b(nod_IO%numnod, ithree, nod_IO%xx)
!
      end subroutine gz_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_scalar_in_element_b(nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call gz_write_one_integer_b(nod_IO%numnod)
      call gz_write_one_integer_b(nod_IO%internal_node)
!
      call gz_write_1d_vector_b(nod_IO%numnod, sfed_IO%ele_scalar)
!
      end subroutine gz_write_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_vector_in_element_b(nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call gz_write_one_integer_b(nod_IO%numnod)
      call gz_write_one_integer_b(nod_IO%internal_node)
!
      call gz_write_2d_vector_b                                         &
     &   (nod_IO%numnod, n_vector, sfed_IO%ele_scalar)
!
      end subroutine gz_write_vector_in_element_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_node_b(gz_flags, nod_IO)
!
      use gz_binary_IO
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_read_one_integer_b                                        &
     &   (gz_flags%iflag_bin_swap, nod_IO%numnod, gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      call gz_read_one_integer_b(gz_flags%iflag_bin_swap,               &
     &    nod_IO%internal_node, gz_flags%ierr_IO)
!
      end subroutine gz_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_info_b(gz_flags, nod_IO)
!
      use gz_binary_IO
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call gz_read_mul_int8_b(gz_flags%iflag_bin_swap,                  &
     &    cast_long(nod_IO%numnod), nod_IO%inod_global,                 &
     &    gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      call gz_read_2d_vector_b(gz_flags%iflag_bin_swap,                 &
     &    nod_IO%numnod, ithree, nod_IO%xx, gz_flags%ierr_IO)
!
      end subroutine gz_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_scalar_in_element_b(gz_flags, nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_read_number_of_node_b(gz_flags, nod_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call gz_write_1d_vector_b(nod_IO%numnod, sfed_IO%ele_scalar)
!
      end subroutine gz_read_scalar_in_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_vector_in_element_b                            &
     &         (gz_flags, nod_IO, sfed_IO)
!
      use gz_binary_IO
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_read_number_of_node_b(gz_flags, nod_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call gz_read_2d_vector_b(gz_flags%iflag_bin_swap, nod_IO%numnod,  &
     &    n_vector, sfed_IO%ele_scalar, gz_flags%ierr_IO)
!
      end subroutine gz_read_vector_in_element_b
!
!------------------------------------------------------------------
!
      end module gz_node_geometry_IO_b
