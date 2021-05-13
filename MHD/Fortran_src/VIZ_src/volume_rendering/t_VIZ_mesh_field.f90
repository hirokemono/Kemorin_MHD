!>@file   t_VIZ_mesh_field.f90
!!@brief  module t_VIZ_mesh_field
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine unlink_FEM_field_4_viz(VIZ_DAT)
!!      subroutine unlink_jacobians_4_viz(VIZ_DAT)
!!        type(mesh_data), intent(inout), target :: geofem
!!        type(next_nod_ele_table), intent(in), target :: next_tbl
!!        type(jacobians_type), intent(in), target :: jacobians
!!        type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!!@endverbatim
!
      module t_VIZ_mesh_field
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_VIZ_step_parameter
      use t_control_param_vol_grping
      use t_calypso_comm_table
!
      implicit none
!
!
!>      Structure of data for visualization
      type VIZ_mesh_field
!>         Structure for mesh data for visualization
        type(mesh_data), pointer :: viz_fem
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: repart_p
!>        Transfer table to visualization mesh
        type(calypso_comm_table) :: mesh_to_viz_tbl
!
!!>        Structure of shape function for PVR and fieldline
!        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type), pointer :: jacobians
!>        Structure of neighboring list for each node
        type(next_nod_ele_table), pointer :: next_tbl
!
!>        Structure of edge communication table
        type(communication_table) :: edge_comm
      end type VIZ_mesh_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine unlink_FEM_field_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      nullify(VIZ_DAT%viz_fem)
!
      end subroutine unlink_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine unlink_jacobians_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      nullify(VIZ_DAT%jacobians, VIZ_DAT%next_tbl)
!
      end subroutine unlink_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      end module t_VIZ_mesh_field
