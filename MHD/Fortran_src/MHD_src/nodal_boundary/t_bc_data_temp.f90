!
!     module t_bc_data_temp
!!
!!      Written by Kemorin on Feb., 2004
!!
!!      subroutine set_bc_temp_id                                       &
!!     &         (IO_bc, node, ele, fluid, nod_grp, temp_nod, Snod_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(group_data), intent(in) :: nod_grp
!!        type(boundary_condition_list), intent(in) :: temp_nod
!!        type(nodal_bcs_4_scalar_type), intent(inout) :: Snod_bcs
!
!
      module t_bc_data_temp
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
!
!>      Structure for nodal boudnary for scalar
      type nodal_bcs_4_scalar_type
        type(scaler_fixed_nod_bc_type) :: nod_bc_s
!
        type(scaler_fixed_nod_bc_type) :: sgs_bc_s
      end type nodal_bcs_4_scalar_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_temp_id                                         &
     &         (IO_bc, node, ele, fluid, nod_grp, temp_nod, Snod_bcs)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use t_boundary_field_IO
      use t_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_scalars
      use set_ele_nod_bc_vectors
      use set_nodal_boundary
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: nod_grp
      type(boundary_condition_list), intent(in) :: temp_nod
!
      type(nodal_bcs_4_scalar_type), intent(inout) :: Snod_bcs
!
!
      call count_num_bc_scl_w_SGS                                       &
     &   (nod_grp, temp_nod, Snod_bcs%nod_bc_s, Snod_bcs%sgs_bc_s)
!
      call alloc_scalar_nod_bc_type(node%numnod, Snod_bcs%nod_bc_s)
      call alloc_scalar_nod_bc_type(node%numnod, Snod_bcs%sgs_bc_s)
!
      Snod_bcs%nod_bc_s%scalar_bc_name = fhd_temp
      call set_bc_fixed_temp_id(IO_bc, node, nod_grp, temp_nod,         &
     &    Snod_bcs%nod_bc_s, Snod_bcs%sgs_bc_s)
!
!   set node id in an element for the temperature boundary 
!
      call ele_nodal_bc_scalar_layer                                    &
     &   (node, ele, fluid, Snod_bcs%nod_bc_s)
      call ele_nodal_bc_scalar_layer                                    &
     &   (node, ele, fluid, Snod_bcs%sgs_bc_s)
!
      end subroutine set_bc_temp_id
!
!  ---------------------------------------------------------------------
!
      end module t_bc_data_temp
