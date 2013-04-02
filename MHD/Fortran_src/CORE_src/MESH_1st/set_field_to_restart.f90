!set_field_to_restart.f90
!     module set_field_to_restart
!
!      subroutine init_field_name_by_restart
!
!      subroutine count_field_num_to_restart
!      subroutine copy_field_name_to_restart
!
!      subroutine copy_field_data_to_restart
!      subroutine copy_field_data_from_restart
!
!
      module set_field_to_restart
!
      use m_precision
!
      use m_geometry_parameter
      use m_node_phys_data
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
      subroutine init_field_name_by_restart
!
!
      num_nod_phys =     num_phys_data_IO
      num_tot_nod_phys = ntot_phys_data_IO
      call allocate_phys_name
!
      phys_nod_name(1:num_nod_phys) = phys_data_name_IO(1:num_nod_phys)
      num_nod_component(1:num_nod_phys)                                 &
     &    = num_phys_comp_IO(1:num_nod_phys)
      istack_nod_component(0:num_nod_phys)                              &
     &    = istack_phys_comp_IO(0:num_nod_phys)
!
      call deallocate_phys_data_IO
!
      end subroutine init_field_name_by_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_field_num_to_restart
!
!
      call count_field_num_to_rst_IO(num_nod_phys, phys_nod_name,       &
     &    num_phys_data_IO)
!
      end subroutine count_field_num_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_name_to_restart
!
!
      call copy_field_name_to_rst_IO(numnod, num_nod_phys,              &
     &    istack_nod_component, phys_nod_name,                          &
     &    num_phys_data_IO, ntot_phys_data_IO, num_phys_comp_IO,        &
     &    istack_phys_comp_IO, phys_data_name_IO, numgrid_phys_IO)
!
      end subroutine copy_field_name_to_restart
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_field_data_to_restart
!
!
      call copy_field_data_to_rst_IO(numnod, num_nod_phys,              &
     &    num_tot_nod_phys, istack_nod_component, phys_nod_name, d_nod, &
     &    num_phys_data_IO, ntot_phys_data_IO, istack_phys_comp_IO,     &
     &    phys_data_name_IO, numgrid_phys_IO, phys_data_IO)
!
      end subroutine copy_field_data_to_restart
!
!------------------------------------------------------------------
!
      subroutine copy_field_data_from_restart
!
!
      call copy_field_data_from_rst_IO(numnod, num_nod_phys,            &
     &    num_tot_nod_phys, istack_nod_component, phys_nod_name, d_nod, &
     &    num_phys_data_IO, ntot_phys_data_IO, istack_phys_comp_IO,     &
     &    phys_data_name_IO, numgrid_phys_IO, phys_data_IO)
!
      end subroutine copy_field_data_from_restart
!
!------------------------------------------------------------------
!
      end module set_field_to_restart
