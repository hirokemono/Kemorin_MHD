!>@file   gz_mesh_data_IO_b.f90
!!@brief  module gz_mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine gz_write_geometry_data_b
!!      subroutine gz_write_geometry_info_b
!!      subroutine gz_write_element_info_b
!!
!!      subroutine gz_read_geometry_data_b
!!      subroutine gz_read_number_of_node_b
!!      subroutine gz_read_geometry_info_b
!!      subroutine gz_read_number_of_element_b
!!@endverbatim
!
      module gz_mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use m_read_mesh_data
!
      implicit  none
!
      private :: gz_write_element_info_b, gz_read_element_info_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_data_b
!
      use gz_domain_data_IO_b
!
!
      call gz_write_domain_info_b
!
      call gz_write_geometry_info_b
      call gz_write_element_info_b
!
      call gz_write_import_data_b
      call gz_write_export_data_b
!
      end subroutine gz_write_geometry_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geometry_info_b
!
      use gz_binary_IO
!
!
      call gz_write_one_integer_b(numnod_dummy)
      call gz_write_one_integer_b(internal_node_dummy)
!
      call gz_write_mul_int8_b(numnod_dummy, globalnodid_dummy)
      call gz_write_2d_vector_b(numnod_dummy, ithree, xx_dummy)
!
      call deallocate_node_data_dummy
!
      end subroutine gz_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_element_info_b
!
      use gz_binary_IO
!
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call gz_write_one_integer_b(ele_IO%numele)
!
      call gz_write_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
      call gz_write_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        ie_tmp(1:ele_IO%nodelm(i)) = ie_dummy(i,1:ele_IO%nodelm(i))
        call gz_write_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call deallocate_ele_info_dummy
!
      end subroutine gz_write_element_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_data_b
!
      use gz_domain_data_IO_b
!
!
      call gz_read_domain_info_b
      call gz_read_number_of_node_b
      call gz_read_geometry_info_b
!
!  ----  read element data -------
!
      call gz_read_number_of_element_b
      call gz_read_element_info_b
!
! ----  import & export 
!
      call gz_read_import_data_b
      call gz_read_export_data_b
!
      end subroutine gz_read_geometry_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_node_b
!
      use gz_binary_IO
!
!
      call gz_read_one_integer_b(numnod_dummy)
      call gz_read_one_integer_b(internal_node_dummy)
!
      end subroutine gz_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geometry_info_b
!
      use gz_binary_IO
!
!
      call allocate_node_data_dummy
!
      call gz_read_mul_int8_b(numnod_dummy, globalnodid_dummy)
      call gz_read_2d_vector_b(numnod_dummy, ithree, xx_dummy)
!
      end subroutine gz_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_number_of_element_b
!
      use gz_binary_IO
!
      call gz_read_one_integer_b(ele_IO%numele)
!
      end subroutine gz_read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_element_info_b
!
      use gz_binary_IO
      use set_nnod_4_ele_by_type
!
      integer (kind = kint) :: i
      integer (kind = kint), allocatable :: ie_tmp(:)
!
!
      call allocate_ele_info_dummy
      call gz_read_mul_integer_b(ele_IO%numele, ele_IO%elmtyp)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call allocate_connect_dummy
!
      call gz_read_mul_int8_b(ele_IO%numele, ele_IO%iele_global)
!
      allocate(ie_tmp(ele_IO%nnod_4_ele))
      do i = 1, ele_IO%numele
        call gz_read_mul_integer_b(ele_IO%nodelm(i), ie_tmp)
        ie_dummy(i,1:ele_IO%nodelm(i)) = ie_tmp(1:ele_IO%nodelm(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine gz_read_element_info_b
!
!------------------------------------------------------------------
!
      end module gz_mesh_data_IO_b
