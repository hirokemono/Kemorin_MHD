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
      call write_fld_inthead_b(numnod_dummy)
      call write_fld_inthead_b(internal_node_dummy)
!
      call write_fld_mul_i8head_b(numnod_dummy, globalnodid_dummy)
      call write_fld_realarray2_b                                       &
     &   (numnod_dummy, ithree, xx_dummy)
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
      call write_fld_inthead_b(numele_dummy)
!
      call write_fld_mul_inthead_b(numele_dummy, i_ele_dummy)
      call write_fld_mul_i8head_b(numele_dummy, globalelmid_dummy)
!
      allocate(ie_tmp(nnod_4_ele_dummy))
      do i = 1, numele_dummy
        ie_tmp(1:nodelm_dummy(i)) = ie_dummy(i,1:nodelm_dummy(i))
        call write_fld_mul_inthead_b(nodelm_dummy(i), ie_tmp)
      end do
      deallocate(ie_tmp)
!
      call deallocate_ele_info_dummy
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
      call read_fld_inthead_b(numnod_dummy)
      call read_fld_inthead_b(internal_node_dummy)
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
      call read_fld_mul_i8head_b(numnod_dummy, globalnodid_dummy)
      call read_fld_realarray2_b                                        &
     &   (numnod_dummy, ithree, xx_dummy)
!
      end subroutine read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine read_number_of_element_b
!
      use binary_IO
!
      call read_fld_inthead_b(numele_dummy)
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
      call allocate_ele_info_dummy
      call read_fld_mul_inthead_b(numele_dummy, i_ele_dummy)
!
      nnod_4_ele_dummy = 0
      do i = 1, numele_dummy
        call s_set_nnod_4_ele_by_type(i_ele_dummy(i), nodelm_dummy(i))
        nnod_4_ele_dummy = max(nnod_4_ele_dummy,nodelm_dummy(i))
      end do
!
      call allocate_connect_dummy
!
      call read_fld_mul_i8head_b(numele_dummy, globalelmid_dummy)
!
      allocate(ie_tmp(nnod_4_ele_dummy))
      do i = 1, numele_dummy
        call read_fld_mul_inthead_b(nodelm_dummy(i), ie_tmp)
        ie_dummy(i,1:nodelm_dummy(i)) = ie_tmp(1:nodelm_dummy(i))
      end do
      deallocate(ie_tmp)
!
      end subroutine read_element_info_b
!
!------------------------------------------------------------------
!
      end module mesh_data_IO_b
