!> @file  set_field_to_restart.f90
!!      module set_field_to_restart
!!
!! @author  H. Matsui
!! @date Programmed in 2001
!
!> @brief Copy between field data and IO data
!!
!!@verbatim
!!      subroutine init_field_name_by_restart(fld_IO)
!!
!!      subroutine count_field_num_to_restart(fld_IO)
!!      subroutine copy_field_name_to_restart(fld_IO)
!!
!!      subroutine copy_field_data_to_restart(fld_IO)
!!      subroutine copy_field_data_from_restart(fld_IO)
!!
!!      subroutine simple_copy_fld_name_to_rst(fld_IO)
!!      subroutine simple_copy_fld_data_to_rst(fld_IO)
!!@endverbatim
!
!
      module set_field_to_restart
!
      use m_precision
!
      use m_geometry_data
      use m_node_phys_data
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
      subroutine init_field_name_by_restart(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      nod_fld1%num_phys =  fld_IO%num_field_IO
      nod_fld1%ntot_phys = fld_IO%ntot_comp_IO
      call allocate_phys_name
!
      phys_nod_name(1:nod_fld1%num_phys)                                &
     &    = fld_IO%fld_name(1:nod_fld1%num_phys)
      nod_fld1%num_component(1:nod_fld1%num_phys)                       &
     &    = fld_IO%num_comp_IO(1:nod_fld1%num_phys)
      nod_fld1%istack_component(0:nod_fld1%num_phys)                    &
     &    = fld_IO%istack_comp_IO(0:nod_fld1%num_phys)
!
      end subroutine init_field_name_by_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_field_num_to_restart(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call count_field_num_to_rst_IO(nod_fld1%num_phys, phys_nod_name,  &
     &    fld_IO%num_field_IO)
!
      end subroutine count_field_num_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_name_to_restart(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_field_name_to_rst_IO(node1%numnod, nod_fld1%num_phys,   &
     &    nod_fld1%istack_component, phys_nod_name,                     &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name, fld_IO%nnod_IO)
!
      end subroutine copy_field_name_to_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_field_data_to_restart(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_field_data_to_rst_IO(node1%numnod, nod_fld1%num_phys,   &
     &    nod_fld1%ntot_phys, nod_fld1%istack_component,                &
     &    phys_nod_name, d_nod,                                         &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name, fld_IO%nnod_IO,       &
     &    fld_IO%d_IO)
!
      end subroutine copy_field_data_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_data_from_restart(fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
!
!
      call copy_field_data_from_rst_IO(node1%numnod, nod_fld1%num_phys, &
     &    nod_fld1%ntot_phys, nod_fld1%istack_component,                &
     &    phys_nod_name, d_nod,                                         &
     &    fld_IO%num_field_IO, fld_IO%ntot_comp_IO,                     &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name, fld_IO%nnod_IO,       &
     &    fld_IO%d_IO)
!
      end subroutine copy_field_data_from_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine simple_copy_fld_name_to_rst(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call simple_copy_fld_name_to_rst_IO(nod_fld1%num_phys,            &
     &    nod_fld1%istack_component, phys_nod_name,                     &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO,                      &
     &    fld_IO%istack_comp_IO, fld_IO%fld_name)
!
      end subroutine simple_copy_fld_name_to_rst
!
!------------------------------------------------------------------
!
      subroutine simple_copy_fld_data_to_rst(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call simple_copy_fld_dat_to_rst_IO                                &
     &   (node1%numnod, nod_fld1%ntot_phys, d_nod,                      &
     &    fld_IO%ntot_comp_IO, fld_IO%nnod_IO, fld_IO%d_IO)
!
      end subroutine simple_copy_fld_data_to_rst
!
!------------------------------------------------------------------
!
      end module set_field_to_restart
