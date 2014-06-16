!set_field_type_to_restart.f90
!     module set_field_type_to_restart
!
!      subroutine count_field_type_num_to_rst(phys)
!      subroutine copy_field_type_name_to_rst(node, phys)
!      subroutine copy_field_type_to_rst(node, phys)
!      subroutine copy_field_type_from_rst(node, phys)
!
!
      module set_field_type_to_restart
!
      use m_precision
!
      use t_geometry_data
      use t_phys_data
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
      subroutine count_field_type_num_to_rst(phys)
!
      type(phys_data), intent(in) :: phys
!
!
      call count_field_num_to_rst_IO(phys%num_phys,                 &
     &    phys%phys_name, num_phys_data_IO)
!
      end subroutine count_field_type_num_to_rst
!
!------------------------------------------------------------------
!
      subroutine copy_field_type_name_to_rst(node, phys)
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys
!
!
      call copy_field_name_to_rst_IO(node%numnod, phys%num_phys,        &
     &    phys%istack_component, phys%phys_name,                        &
     &    num_phys_data_IO, ntot_phys_data_IO, num_phys_comp_IO,        &
     &    istack_phys_comp_IO, phys_data_name_IO, numgrid_phys_IO)
!
      end subroutine copy_field_type_name_to_rst
!
!------------------------------------------------------------------
!
      subroutine copy_field_type_to_rst(node, phys)
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys
!
!
      call copy_field_data_to_rst_IO                                    &
     &   (node%numnod, phys%num_phys, phys%ntot_phys,                   &
     &    phys%istack_component, phys%phys_name, phys%d_fld,            &
     &    num_phys_data_IO, ntot_phys_data_IO, istack_phys_comp_IO,     &
     &    phys_data_name_IO, numgrid_phys_IO, phys_data_IO)
!
      end subroutine copy_field_type_to_rst
!
!------------------------------------------------------------------
!
      subroutine copy_field_type_from_rst(node, phys)
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys
!
!
      call copy_field_data_from_rst_IO                                  &
     &   (node%numnod, phys%num_phys, phys%ntot_phys,                   &
     &    phys%istack_component, phys%phys_name, phys%d_fld,            &
     &    num_phys_data_IO, ntot_phys_data_IO, istack_phys_comp_IO,     &
     &    phys_data_name_IO, numgrid_phys_IO, phys_data_IO)
!
      end subroutine copy_field_type_from_rst
!
!------------------------------------------------------------------
!
      end module set_field_type_to_restart
