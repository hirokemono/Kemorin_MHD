!> @file  set_field_to_restart.f90
!!      module set_field_to_restart
!!
!! @author  H. Matsui
!! @date Programmed in 2001
!
!> @brief Copy between field data structure and IO data
!!
!!@verbatim
!!      subroutine init_field_name_by_restart(fld_IO, phys)
!!
!!      subroutine count_field_num_to_restart(phys, fld_IO)
!!      subroutine copy_field_name_to_restart(node, phys, fld_IO)
!!
!!      subroutine copy_field_data_to_restart(node, phys, fld_IO)
!!      subroutine copy_field_data_from_restart(node, fld_IO, phys)
!!
!!      subroutine simple_copy_fld_name_to_rst(phys, fld_IO)
!!      subroutine simple_copy_fld_data_to_rst(node, phys, fld_IO)
!!@endverbatim
!
!
      module set_field_to_restart
!
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_field_data_IO
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
      subroutine init_field_name_by_restart(fld_IO, phys)
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: phys
!
!
      phys%num_phys =  fld_IO%num_field_IO
      phys%ntot_phys = fld_IO%ntot_comp_IO
      call alloc_phys_name_type(phys)
!
      phys%phys_name(1:phys%num_phys)                                   &
     &    = fld_IO%fld_name(1:phys%num_phys)
      phys%num_component(1:phys%num_phys)                               &
     &    = fld_IO%num_comp_IO(1:phys%num_phys)
      phys%istack_component(0:phys%num_phys)                            &
     &    = fld_IO%istack_comp_IO(0:phys%num_phys)
!
      end subroutine init_field_name_by_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_field_num_to_restart(phys, fld_IO)
!
      type(phys_data), intent(in) :: phys
      type(field_IO), intent(inout) :: fld_IO
!
!
      call count_field_num_to_rst_IO(phys%num_phys,                 &
     &    phys%phys_name, fld_IO%num_field_IO)
!
      end subroutine count_field_num_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_name_to_restart(node, phys, fld_IO)
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_field_name_to_rst_IO(node%numnod, phys%num_phys,        &
     &    phys%istack_component, phys%phys_name,                        &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name, fld_IO%nnod_IO)
!
      end subroutine copy_field_name_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_data_to_restart(node, phys, fld_IO)
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_field_data_to_rst_IO                                    &
     &   (node%numnod, phys%num_phys, phys%ntot_phys,                   &
     &    phys%istack_component, phys%phys_name, phys%d_fld,            &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name, fld_IO%nnod_IO,       &
     &    fld_IO%d_IO)
!
      end subroutine copy_field_data_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_data_from_restart(node, fld_IO, phys)
!
      type(node_data), intent(in) :: node
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: phys
!
!
      call copy_field_data_from_rst_IO                                  &
     &   (node%numnod, phys%num_phys, phys%ntot_phys,                   &
     &    phys%istack_component, phys%phys_name, phys%d_fld,            &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name, fld_IO%nnod_IO,       &
     &    fld_IO%d_IO)
!
      end subroutine copy_field_data_from_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine simple_copy_fld_name_to_rst(phys, fld_IO)
!
      type(phys_data), intent(in) :: phys
      type(field_IO), intent(inout) :: fld_IO
!
!
      call simple_copy_fld_name_to_rst_IO                               &
     &   (phys%num_phys, phys%istack_component, phys%phys_name,         &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO,                      &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name)
!
      end subroutine simple_copy_fld_name_to_rst
!
!------------------------------------------------------------------
!
      subroutine simple_copy_fld_data_to_rst(node, phys, fld_IO)
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys
      type(field_IO), intent(inout) :: fld_IO
!
!
      call simple_copy_fld_dat_to_rst_IO                                &
     &   (node%numnod, phys%ntot_phys, phys%d_fld,                      &
     &    fld_IO%ntot_comp_IO, fld_IO%nnod_IO, fld_IO%d_IO)
!
      end subroutine simple_copy_fld_data_to_rst
!
!------------------------------------------------------------------
!
      end module set_field_to_restart
