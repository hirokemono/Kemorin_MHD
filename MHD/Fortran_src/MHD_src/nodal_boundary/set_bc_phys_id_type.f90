!
!      module set_bc_phys_id_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov., 2003
!        modified by H.Matsui on Oct., 2005
!        modified by H.Matsui on Jan., 2009
!
!      subroutine set_bc_velo_id_type(mesh, MHD_mesh, nod_grp, nodal_bc)
!      subroutine set_bc_vect_p_id_type(mesh, nod_grp, nodal_bc)
!      subroutine set_bc_magne_id_type(mesh, nod_grp, nodal_bc)
!      subroutine set_bc_current_id_type(mesh, nod_grp, nodal_bc)
!      subroutine set_bc_temp_id_type(mesh, MHD_mesh, nod_grp, nodal_bc)
!      subroutine set_bc_press_id_type(mesh, MHD_mesh, nod_grp, nodal_bc)
!      subroutine set_bc_m_potential_id_type(mesh, MHD_mesh, nod_grp,   &
!     &          nodal_bc)
!      subroutine set_bc_d_scalar_id_type(mesh, MHD_mesh, nod_grp,      &
!     &          nodal_bc)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(mesh_data_MHD),       intent(in) :: MHD_mesh
!        type(group_data),          intent(in) :: nod_grp
!        type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      module set_bc_phys_id_type
!
      use m_precision
!
      use m_boundary_field_IO
      use t_mesh_data
      use t_group_data
      use t_nodal_bc_data
      use set_ele_nod_bc_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_velo_id_type(mesh, MHD_mesh, nod_grp, nodal_bc)
!
      use t_geometry_data_MHD
      use set_bc_type_vectors
      use set_bc_type_scalars
!
      type(mesh_geometry),       intent(in) :: mesh
      type(mesh_data_MHD),       intent(in) :: MHD_mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_fixed_velo_type_id(nod_grp, nodal_bc)
!
      call set_bc_type_velo_4_sphere_id(nod_grp, nodal_bc)
!
!   set node id in an element for velocity boundary 
!
      call set_ele_nod_bc_type_vect_layer(mesh, MHD_mesh%fluid,         &
     &    nodal_bc%velocity)
      call set_ele_nod_bc_type_vect_layer(mesh, MHD_mesh%fluid,         &
     &    nodal_bc%sgs_velo)
!
      call set_ele_nodal_bc_type_rotate(mesh, MHD_mesh%fluid,           &
     &    nodal_bc%rotation)
      call set_ele_nod_bc_type_layer(mesh, MHD_mesh%fluid,              &
     &    nodal_bc%free_plane)
      call set_ele_nod_bc_type_layer(mesh, MHD_mesh%fluid,              &
     &    nodal_bc%no_radial_v)
      call set_ele_nod_bc_type_layer(mesh, MHD_mesh%fluid,              &
     &    nodal_bc%free_sphere)
!
      end subroutine set_bc_velo_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_vect_p_id_type(mesh, nod_grp, nodal_bc)
!
      use set_bc_type_vectors
!
      type(mesh_geometry),       intent(in) :: mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_fixed_vect_p_type_id(nod_grp, nodal_bc)
!
!   set node id in an element for magnetic boundary 
!
      call set_ele_nod_bc_type_vect(mesh, nodal_bc%vector_p)
      call set_ele_nod_bc_type_vect(mesh, nodal_bc%sgs_vect_p)
!
      end subroutine set_bc_vect_p_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_magne_id_type(mesh, nod_grp, nodal_bc)
!
      use set_bc_type_vectors
!
      type(mesh_geometry),       intent(in) :: mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_fixed_magne_type_id(nod_grp, nodal_bc)
!
!   set node id in an element for magnetic boundary 
!
      call set_ele_nod_bc_type_vect(mesh, nodal_bc%magne)
      call set_ele_nod_bc_type_vect(mesh, nodal_bc%sgs_magne)
!
      end subroutine set_bc_magne_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_current_id_type(nod_grp, nodal_bc)
!
      use set_bc_type_vectors
!
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_fixed_current_type_id(nod_grp, nodal_bc)
      call dealloc_vector_nod_bc_type(nodal_bc%current)
!
      end subroutine set_bc_current_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_temp_id_type(mesh, MHD_mesh, nod_grp, nodal_bc)
!
      use t_geometry_data_MHD
      use set_bc_type_scalars
!
      type(mesh_geometry),       intent(in) :: mesh
      type(mesh_data_MHD),       intent(in) :: MHD_mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      call set_bc_type_fixed_temp_id(nod_grp, nodal_bc)
!
!   set node id in an element for the temperature boundary 
!
      call set_ele_nod_bc_type_layer(mesh, MHD_mesh%fluid,              &
     &    nodal_bc%temp)
      call set_ele_nod_bc_type_layer(mesh, MHD_mesh%fluid,              &
     &    nodal_bc%sgs_temp)
!
!
      end subroutine set_bc_temp_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_press_id_type(mesh, MHD_mesh, nod_grp,          &
     &          nodal_bc)
!
      use t_geometry_data_MHD
      use set_bc_type_scalars
!
      type(mesh_geometry),       intent(in) :: mesh
      type(mesh_data_MHD),       intent(in) :: MHD_mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_type_fixed_press_id(nod_grp, nodal_bc)
!
!   set node id in an element for the pressure boundary 
!
      call set_ele_nodal_bc_type_part_pot(mesh, MHD_mesh%fluid,         &
     &    nodal_bc%press)
      call set_ele_nodal_bc_type_part_pot(mesh, MHD_mesh%fluid,         &
     &    nodal_bc%sgs_press)
!
      end subroutine set_bc_press_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_m_potential_id_type(mesh, MHD_mesh, nod_grp,    &
     &          nodal_bc)
!
      use set_bc_type_scalars
      use t_geometry_data_MHD
!
      type(mesh_geometry),       intent(in) :: mesh
      type(mesh_data_MHD),       intent(in) :: MHD_mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_type_fixed_mag_p_id(nod_grp, nodal_bc)
!
      call alloc_scalar_nod_bc_type(mesh%node%numnod,                   &
     &                              nodal_bc%mag_p_ins)
      nodal_bc%mag_p_ins%ibc(1:mesh%node%numnod)                        &
     &      = nodal_bc%magne_p%ibc(1:mesh%node%numnod) 
!
      call alloc_scalar_nod_bc_type(mesh%node%numnod,                   &
     &                              nodal_bc%mag_p_cd)
          nodal_bc%mag_p_cd%ibc(1:mesh%node%numnod)                     &
     &      = nodal_bc%magne_p%ibc(1:mesh%node%numnod) 
!
!   set node id in an element for the pressure boundary 
!
      call set_ele_nodal_bc_type_potential(mesh, nodal_bc%magne_p)
      call set_ele_nodal_bc_type_potential(mesh, nodal_bc%sgs_mag_p)
      call set_ele_nodal_bc_type_part_pot(mesh, MHD_mesh%insulate,      &
     &    nodal_bc%mag_p_ins)
      call set_ele_nodal_bc_type_part_pot(mesh, MHD_mesh%conduct,       &
     &    nodal_bc%mag_p_cd)
!
      end subroutine set_bc_m_potential_id_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_d_scalar_id_type(mesh, MHD_mesh, nod_grp,       &
     &          nodal_bc)
!
      use t_geometry_data_MHD
      use set_bc_type_scalars
!
      type(mesh_geometry),       intent(in) :: mesh
      type(mesh_data_MHD),       intent(in) :: MHD_mesh
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      call set_bc_type_fixed_composit_id(nod_grp, nodal_bc)
!
!   set node id in an element for composition boundary 
!
      call set_ele_nod_bc_type_layer(mesh, MHD_mesh%fluid,              &
     &    nodal_bc%composition)
!
      end subroutine set_bc_d_scalar_id_type
!
!  ---------------------------------------------------------------------
!
      end module set_bc_phys_id_type
