!set_2nd_field_to_restart.f90
!     module set_2nd_field_to_restart
!
!      subroutine count_2nd_field_num_to_rst
!      subroutine copy_2nd_field_name_to_rst
!      subroutine copy_2nd_field_data_to_rst
!      subroutine copy_2nd_field_data_from_rst
!
!
      module set_2nd_field_to_restart
!
      use m_precision
!
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use m_field_data_IO
!
      use set_restart_data
!
      implicit  none
! 
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_2nd_field_num_to_rst
!
!
      call count_field_num_to_rst_IO(phys_2nd%num_phys,                 &
     &    phys_2nd%phys_name, num_phys_data_IO)
!
      end subroutine count_2nd_field_num_to_rst
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_field_name_to_rst
!
!
      call copy_field_name_to_rst_IO(nnod_2nd, phys_2nd%num_phys,       &
     &    phys_2nd%istack_component, phys_2nd%phys_name,                      &
     &    num_phys_data_IO, ntot_phys_data_IO, num_phys_comp_IO,        &
     &    istack_phys_comp_IO, phys_data_name_IO, numgrid_phys_IO)
!
      end subroutine copy_2nd_field_name_to_rst
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_field_data_to_rst
!
!
      call copy_field_data_to_rst_IO                                    &
     &   (nnod_2nd, phys_2nd%num_phys, phys_2nd%ntot_phys,              &
     &    phys_2nd%istack_component, phys_2nd%phys_name, phys_2nd%d_fld,           &
     &    num_phys_data_IO, ntot_phys_data_IO, istack_phys_comp_IO,     &
     &    phys_data_name_IO, numgrid_phys_IO, phys_data_IO)
!
      end subroutine copy_2nd_field_data_to_rst
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_field_data_from_rst
!
!
      call copy_field_data_from_rst_IO                                  &
     &   (nnod_2nd, phys_2nd%num_phys, phys_2nd%ntot_phys,              &
     &    phys_2nd%istack_component, phys_2nd%phys_name,                &
     &    phys_2nd%d_fld, num_phys_data_IO, ntot_phys_data_IO,          &
     &    istack_phys_comp_IO, phys_data_name_IO, numgrid_phys_IO, phys_data_IO)
!
      end subroutine copy_2nd_field_data_from_rst
!
!------------------------------------------------------------------
!
      end module set_2nd_field_to_restart
