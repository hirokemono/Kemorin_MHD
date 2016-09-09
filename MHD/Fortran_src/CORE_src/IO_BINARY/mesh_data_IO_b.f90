!>@file   mesh_data_IO_b.f90
!!@brief  module mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for Binary mesh data IO
!!
!!@verbatim
!!      subroutine write_geometry_data_b
!!      subroutine write_geometry_info_b
!!      subroutine write_element_info_b
!!
!!      subroutine read_geometry_data_b
!!      subroutine read_number_of_node_b
!!      subroutine read_geometry_info_b
!!      subroutine read_number_of_element_b
!!@endverbatim
!
      module mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use m_read_mesh_data
!
      implicit  none
!
      private :: write_element_info_b, read_element_info_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_geometry_data_b
!
      use domain_data_IO_b
!
!
      call write_domain_info_b
!
      call write_geometry_info_b
      call write_element_info_b
!
      call write_import_data_b
      call write_export_data_b
!
      end subroutine write_geometry_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geometry_info_b
!
      use binary_IO
!
!
      call write_one_integer_b(nod_IO%numnod)
      call write_one_integer_b(nod_IO%internal_node)
!
      call write_mul_int8_b(nod_IO%numnod, nod_IO%inod_global)
      call write_2d_vector_b(nod_IO%numnod, ithree, xx_dummy)
!
      call deallocate_node_data_dummy
!
      end subroutine write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine write_element_info_b
!
      use binary_IO
!
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call write_one_integer_b(ele_IO%numele)
!
      call write_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
      call write_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        ie_tmp(1:ele_IO%nodelm(i)) = ele_IO%ie(i,1:ele_IO%nodelm(i))
        call write_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine write_element_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_geometry_data_b
!
      use domain_data_IO_b
!
!
      call read_domain_info_b
      call read_number_of_node_b
      call read_geometry_info_b
!
!  ----  read element data -------
!
      call read_number_of_element_b
      call read_element_info_b
!
! ----  import & export 
!
      call read_import_data_b
      call read_export_data_b
!
      end subroutine read_geometry_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_number_of_node_b
!
      use binary_IO
!
!
      call read_one_integer_b(nod_IO%numnod)
      call read_one_integer_b(nod_IO%internal_node)
!
      end subroutine read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine read_geometry_info_b
!
      use binary_IO
!
!
      call allocate_node_data_dummy
!
      call read_mul_int8_b(nod_IO%numnod, nod_IO%inod_global)
      call read_2d_vector_b(nod_IO%numnod, ithree, xx_dummy)
!
      end subroutine read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine read_number_of_element_b
!
      use binary_IO
!
      call read_one_integer_b(ele_IO%numele)
!
      end subroutine read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine read_element_info_b
!
      use binary_IO
      use set_nnod_4_ele_by_type
!
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call alloc_element_types(ele_IO)
      call read_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call alloc_ele_connectivity(ele_IO)
!
      call read_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        call read_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
        ele_IO%ie(i,1:ele_IO%nodelm(i)) = ie_tmp(1:ele_IO%nodelm(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine read_element_info_b
!
!------------------------------------------------------------------
!
      end module mesh_data_IO_b
