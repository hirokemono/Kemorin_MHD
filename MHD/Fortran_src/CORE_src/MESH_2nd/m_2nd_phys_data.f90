!
!     module m_2nd_phys_data
!.......................................................................
!
!     Written by H. Matsui
!
!       subroutine deallocate_2nd_phys_name
!       subroutine deallocate_2nd_data_arrays
!
!       subroutine disconnect_2nd_phys_name
!       subroutine disconnect_2nd_data_arrays
!
!
      module m_2nd_phys_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
!
      type(phys_data), save :: phys_2nd
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine deallocate_2nd_phys_name
!
      call dealloc_phys_name_type(phys_2nd)
!
      end subroutine deallocate_2nd_phys_name
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_2nd_data_arrays
!
      call dealloc_phys_data_type(phys_2nd)
!
      end subroutine deallocate_2nd_data_arrays
!
!  --------------------------------------------------------------------
!
      subroutine disconnect_2nd_phys_name
!
      call disconnect_phys_name_type(phys_2nd)
!
      end subroutine disconnect_2nd_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine disconnect_2nd_data_arrays
!
       call disconnect_phys_data_type(phys_2nd)
!
       end subroutine disconnect_2nd_data_arrays
!
!  --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_nodal_field_names
!
      use link_data_type_to_1st_mesh
!
!
      call link_nodal_fld_type_names(phys_2nd)
!
      end subroutine link_nodal_field_names
!
! -------------------------------------------------------------------
!
      subroutine link_nodal_field_data
!
      use link_data_type_to_1st_mesh
!
!
      call link_nodal_fld_type(phys_2nd)
!
      end subroutine link_nodal_field_data
!
! -------------------------------------------------------------------
!
      end module m_2nd_phys_data
