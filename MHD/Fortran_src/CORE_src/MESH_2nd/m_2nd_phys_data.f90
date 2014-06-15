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
      end module m_2nd_phys_data
