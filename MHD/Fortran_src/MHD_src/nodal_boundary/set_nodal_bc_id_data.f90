!set_nodal_bc_id_data.f90
!     module set_nodal_bc_id_data
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine set_bc_id_data                                       &
!!     &         (node, ele, nod_grp, MHD_mesh, iphys, nod_fld)
!!      subroutine set_boundary_velo(node, i_velo, nod_fld)
!!      subroutine set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!!      subroutine delete_field_by_fixed_v_bc(Vnod_bcs, i_field, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: nod_grp
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module set_nodal_bc_id_data
!
      use m_precision
      use m_constants
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_group_data
      use t_phys_data
      use t_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_id_data                                         &
     &         (node, ele, nod_grp, MHD_mesh, iphys, nod_fld)
!
      use m_machine_parameter
      use m_control_parameter
!
      use m_bc_data_ene
      use m_bc_data_velo
      use m_bc_data_magne
!
      use m_boundary_condition_IDs
      use m_bc_data_list
!
      use count_num_nod_bc_MHD
      use set_boundary_scalars
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
      type(mesh_data_MHD), intent(in) :: MHD_mesh
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 v'
        call set_bc_velo_id                                             &
     &     (node, ele, MHD_mesh%fluid, nod_grp, Vnod1_bcs)
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 P'
        call set_bc_press_id(node, ele, MHD_mesh%fluid, nod_grp,        &
     &      iphys, nod_fld, Vnod1_bcs)
        if ( iflag_debug .eq.1) write(*,*)  'set boundary values 4 v'
        call set_boundary_velo(node, Vnod1_bcs, iphys%i_velo, nod_fld)
      end if
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_bc_temp_id                                             &
     &     (node, ele, MHD_mesh%fluid, nod_grp, iphys, nod_fld)
!
        call set_boundary_scalar(nod_bc1_t, iphys%i_temp, nod_fld)
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_bc_composition_id(node, ele, MHD_mesh%fluid, nod_grp)
!
        call set_boundary_scalar(nod_bc1_c, iphys%i_light, nod_fld)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &  .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*)  'set boundary ID 4 magne'
        call set_bc_magne_id(node, ele, nod_grp, iphys, nod_fld)
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 magne_p'
        call set_bc_m_potential_id                                      &
     &     (node, ele, MHD_mesh%conduct, MHD_mesh%insulate, nod_grp)
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 current'
        call set_bc_current_id(node, ele, nod_grp)
!
        if (iflag_debug.eq.1)  write(*,*) 'set_boundary_vect magne'
        call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld)
!
        if (iflag_debug.eq.1) write(*,*) 'set boundary value 4 magne'
        call set_boundary_scalar(nod_bc1_f, iphys%i_m_phi, nod_fld)
!
        if (iflag_debug.eq.1) write(*,*) 'set_boundary_vect current'
        call set_boundary_vect(nod_bc1_j, iphys%i_current, nod_fld)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 vect_p'
        call set_bc_vect_p_id(node, ele, nod_grp)
!
        if (iflag_debug .eq.1) write(*,*) 'set_boundary_vect vect_p'
        call set_boundary_vect(nod_bc1_a, iphys%i_vecp, nod_fld)
      end if
!
      end subroutine set_bc_id_data
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo(node, Vnod_bcs, i_velo, nod_fld)
!
      use m_control_parameter
!
      use t_bc_data_velo
!
      use set_nodal_bc_4_velo
      use set_boundary_scalars
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_velo
      type(node_data), intent(in) :: node
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_data), intent(inout) :: nod_fld
!
!
!     set fixed velocity
!
      call set_boundary_vect(Vnod_bcs%nod_bc_v, i_velo, nod_fld)
!
!   set rotation boundary
      call set_boundary_rot_vect                                        &
     &   (node, Vnod_bcs%nod_bc_rot, i_velo, nod_fld)
!
!   boundary condition for special case
!     ( please write every time!!)
      call set_boundary_specific_vect                                   &
     &   (node, Vnod_bcs%nod_bc_vsp, i_velo, nod_fld)
!
!
      call delete_radial_vector_on_bc                                   &
     &   (node, Vnod_bcs%nod_bc_vr0, i_velo, nod_fld)
!
      end subroutine set_boundary_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      use t_finite_element_mat
      use t_bc_data_velo
!
      use set_boundary_scalars
      use set_fixed_boundaries
!
      type(node_data), intent(in) :: node
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      call delete_vector_ffs_on_bc(node, Vnod_bcs%nod_bc_v, f_l, f_nl)
      call delete_vector_ffs_rot_bc                                     &
     &   (node, Vnod_bcs%nod_bc_rot, f_l, f_nl)
      call set_vector_ffs_special_bc(node, Vnod_bcs%nod_bc_vsp, f_l)
!
      end subroutine set_boundary_velo_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_v_bc(Vnod_bcs, i_field, nod_fld)
!
      use t_bc_data_velo
      use set_boundary_scalars
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_data), intent(inout) :: nod_fld
!
!
      call delete_vector_on_bc(Vnod_bcs%nod_bc_v, i_field, nod_fld)
      call delete_vector_by_rot_v_bc                                    &
     &   (Vnod_bcs%nod_bc_rot, i_field, nod_fld)
      call delete_vector_by_fixed_t_bc                                  &
     &   (Vnod_bcs%nod_bc_vsp, i_field, nod_fld)
!
      end subroutine delete_field_by_fixed_v_bc
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_bc_id_data
