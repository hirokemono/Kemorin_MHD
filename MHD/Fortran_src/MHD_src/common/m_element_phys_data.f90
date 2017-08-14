!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!!      subroutine initialize_ele_field_data(numele)
!!      subroutine deallocate_ele_data_arrays
!
!
      module m_element_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
      use t_material_property
!
      implicit  none
!
!
!>       Structure for field data on element
      type(phys_data), save :: fld_ele1
!
!>   address for element fields
      type(phys_address), save :: iphys_ele
!
!>      Strucutre of coefficients for each element
      type(coefs_4_MHD_type), save :: ak_MHD
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine initialize_ele_field_data(numele)
!
      use initialize_element_field
!
      integer(kind = kint), intent(in) :: numele
!
!  allocatie element field
!
      call alloc_phys_data_type(numele, fld_ele1)
      call set_element_field_address(fld_ele1, iphys_ele)
!
      end subroutine initialize_ele_field_data
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_ele_data_arrays
!
!
      call dealloc_phys_data_type(fld_ele1)
      call dealloc_phys_name_type(fld_ele1)
!
      end subroutine deallocate_ele_data_arrays
!
!  --------------------------------------------------------------------
!
      end module m_element_phys_data
