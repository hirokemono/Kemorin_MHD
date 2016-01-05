!
!      module input_control_gen_z_filter
!
!     Written by H. Matsui on June, 2007
!
!!      subroutine s_input_control_4_z_commute                          &
!!     &         (nod_comm, node, ele, surf, edge)
!
      module input_control_gen_z_filter
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_4_z_commute                            &
     &         (nod_comm, node, ele, surf, edge)
!
      use m_machine_parameter
      use calypso_mpi
!
      use m_ctl_data_gen_z_filter
      use set_ctl_gen_z_filter
      use const_geometry_z_commute
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_z_filter'
      call read_control_4_z_filter
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_gen_z_filter'
      call set_ctl_params_4_gen_z_filter
!
!  --  set geometry
!
      if (iflag_debug.eq.1) write(*,*) 'set_geometry_z_commute'
      call set_geometry_z_commute(nod_comm, node, ele, surf, edge)
!
      end subroutine s_input_control_4_z_commute
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_z_filter
