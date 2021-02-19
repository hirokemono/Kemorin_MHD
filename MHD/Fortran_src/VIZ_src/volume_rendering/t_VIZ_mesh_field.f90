!>@file   t_VIZ_mesh_field.f90
!!@brief  module t_VIZ_mesh_field
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine link_FEM_field_4_viz(geofem, nod_fld, VIZ_DAT)
!!      subroutine link_jacobians_4_viz(ele_4_nod, jacobians, VIZ_DAT)
!!      subroutine unlink_FEM_field_4_viz(VIZ_DAT)
!!      subroutine unlink_jacobians_4_viz(VIZ_DAT)
!!      subroutine alloc_FEM_field_4_viz(VIZ_DAT)
!!      subroutine alloc_jacobians_4_viz(VIZ_DAT)
!!      subroutine dealloc_FEM_field_4_viz(VIZ_DAT)
!!      subroutine dealloc_jacobians_4_viz(VIZ_DAT)
!!        type(mesh_data), intent(inout), target :: geofem
!!        type(phys_data), intent(inout), target :: nod_fld
!!        type(element_around_node), intent(in), target :: ele_4_nod
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
!>         Structure for nodal field data
        type(phys_data), pointer :: viz_fld
!>        Structure for repartitioning parameters
        type(volume_partioning_param) :: repart_p
!>        Transfer table to visualization mesh
        type(calypso_comm_table) :: mesh_to_viz_tbl
!
!!>        Structure of shape function for PVR and fieldline
!        type(shape_finctions_at_points) :: spfs
!>        Stracture for Jacobians
        type(jacobians_type), pointer :: jacobians
!>        Structure of included element list for each node
        type(element_around_node), pointer :: ele_4_nod
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
      subroutine link_FEM_field_4_viz(geofem, nod_fld, VIZ_DAT)
!
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in), target :: nod_fld
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      VIZ_DAT%viz_fem => geofem
      VIZ_DAT%viz_fld => nod_fld
!
      end subroutine link_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine link_jacobians_4_viz(ele_4_nod, jacobians, VIZ_DAT)
!
      type(element_around_node), intent(in), target :: ele_4_nod
      type(jacobians_type), intent(in), target :: jacobians
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      VIZ_DAT%ele_4_nod => ele_4_nod
      VIZ_DAT%jacobians => jacobians
!
      end subroutine link_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine unlink_FEM_field_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      nullify(VIZ_DAT%viz_fem, VIZ_DAT%viz_fld)
!
      end subroutine unlink_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine unlink_jacobians_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      nullify(VIZ_DAT%jacobians, VIZ_DAT%ele_4_nod)
!
      end subroutine unlink_jacobians_4_viz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_FEM_field_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      allocate(VIZ_DAT%viz_fem)
      allocate(VIZ_DAT%viz_fld)
!
      end subroutine alloc_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine alloc_jacobians_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      allocate(VIZ_DAT%ele_4_nod)
      allocate(VIZ_DAT%jacobians)
!
      end subroutine alloc_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_FEM_field_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      deallocate(VIZ_DAT%viz_fem, VIZ_DAT%viz_fld)
!
      end subroutine dealloc_FEM_field_4_viz
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_jacobians_4_viz(VIZ_DAT)
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
!
      deallocate(VIZ_DAT%ele_4_nod, VIZ_DAT%jacobians)
!
      end subroutine dealloc_jacobians_4_viz
!
! ----------------------------------------------------------------------
!
      end module t_VIZ_mesh_field
