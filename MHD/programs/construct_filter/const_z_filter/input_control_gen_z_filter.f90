!>@file   input_control_gen_z_filter.f90
!!@brief  module input_control_gen_z_filter
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!!
!>@brief Data input for vertical filter
!!
!!@verbatim
!!      subroutine s_input_control_4_z_commute                          &
!!     &         (nod_comm, node, ele, surf, edge, edge_z_gl,           &
!!     &          mat_crs, CG_param, DJDS_param)
!!        type(communication_table), intent(inout) :: nod_comm
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(inout) :: DJDS_param
!!        type(global_edge_data), intent(inout) :: edge_z_gl
!!@endverbatim
      module input_control_gen_z_filter
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_iccg_parameter
      use t_crs_matrix
      use t_ctl_data_gen_z_filter
!
      implicit none
!
      type(ctl_data_gen_z_filter), save, private :: z_filter_ctl1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_4_z_commute                            &
     &         (nod_comm, node, ele, surf, edge, edge_z_gl,             &
     &          mat_crs, CG_param, DJDS_param)
!
      use m_machine_parameter
      use calypso_mpi
!
      use set_ctl_gen_z_filter
      use const_geometry_z_commute
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
      type(global_edge_data), intent(inout) :: edge_z_gl
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_z_filter'
      call read_control_4_z_filter(z_filter_ctl1)
      call set_ctl_params_4_gen_z_filter                                &
     &   (z_filter_ctl1, mat_crs, CG_param, DJDS_param)
      call dealloc_ctl_data_gen_z_filter(z_filter_ctl1)
!
!  --  set geometry
!
      if (iflag_debug.eq.1) write(*,*) 'set_geometry_z_commute'
      call set_geometry_z_commute(nod_comm, node, ele, surf,            &
     &                            edge, edge_z_gl)
!
      end subroutine s_input_control_4_z_commute
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_z_filter
