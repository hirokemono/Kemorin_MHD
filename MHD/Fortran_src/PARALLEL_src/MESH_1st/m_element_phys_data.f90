!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!      subroutine initialize_ele_field_data
!      subroutine deallocate_ele_data_arrays
!
!
      module m_element_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
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
      private ::  set_element_field_address
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine initialize_ele_field_data
!
      use m_geometry_data
!
!  allocatie element field
!
      call alloc_phys_data_type(ele1%numele, fld_ele1)
      call set_element_field_address
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
!  --------------------------------------------------------------------
!
      subroutine set_element_field_address
!
      use m_phys_labels
!
!   set address of elemental values
!
      integer(kind = kint) :: i, i0
!
       i0 = 1
       do i = 1, fld_ele1%num_phys
!
        if      (fld_ele1%phys_name(i) .eq. fhd_velo) then
          iphys_ele%i_velo = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_vort) then
          iphys_ele%i_vort = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_v) then
          iphys_ele%i_filter_velo = i0
!
        else if (fld_ele1%phys_name(i) .eq. fhd_magne) then
          iphys_ele%i_magne = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_current) then
          iphys_ele%i_current = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_b) then
          iphys_ele%i_filter_magne = i0
!
        else if (fld_ele1%phys_name(i) .eq. fhd_temp) then
          iphys_ele%i_temp = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_temp) then
          iphys_ele%i_filter_temp = i0
!
        else if (fld_ele1%phys_name(i) .eq. fhd_light) then
          iphys_ele%i_light = i0
        else if (fld_ele1%phys_name(i) .eq. fhd_filter_comp) then
          iphys_ele%i_filter_comp = i0
        end if
!
        i0 = i0 + fld_ele1%num_component(i)
      end do
!
      end subroutine set_element_field_address
!
!  --------------------------------------------------------------------
!
      end module m_element_phys_data
