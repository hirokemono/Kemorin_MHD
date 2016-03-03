!
!     module t_bc_data_magne
!.......................................................................
!
!     Written by Kemorin
!
!!      subroutine set_bc_vect_p_id(node, ele, nod_grp, Bnod_bcs)
!!      subroutine set_bc_magne_id                                      &
!!     &         (node, ele, nod_grp, iphys, nod_fld, Bnod_bcs)
!!      subroutine set_bc_current_id(node, ele, nod_grp, Bnod_bcs)
!!      subroutine set_bc_m_potential_id                                &
!!     &         (node, ele, conduct, insulate, nod_grp, Bnod_bcs)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: conduct, insulate
!!        type(group_data), intent(in) :: nod_grp
!!        type(nodal_bcs_4_induction_type), intent(inout) :: Bnod_bcs
!
      module t_bc_data_magne
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
      type nodal_bcs_4_induction_type
        type(vect_fixed_nod_bc_type) :: nod_bc_a
!
        type(vect_fixed_nod_bc_type) :: nod_bc_b
!
        type(vect_fixed_nod_bc_type) :: nod_bc_j
!
        type(scaler_fixed_nod_bc_type) :: nod_bc_f
!
        type(scaler_fixed_nod_bc_type) :: nod_bc_fcd
!
        type(scaler_fixed_nod_bc_type) :: nod_bc_fins
!
        type(vect_fixed_nod_bc_type) :: sgs_bc_a
!
        type(vect_fixed_nod_bc_type) :: sgs_bc_b
!
        type(scaler_fixed_nod_bc_type) :: sgs_bc_f
      end type nodal_bcs_4_induction_type
!Bnod_bcs%nod_bc_a
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_vect_p_id(node, ele, nod_grp, Bnod_bcs)
!
      use t_geometry_data
      use t_group_data
      use m_bc_data_list
      use m_boundary_condition_IDs
      use count_num_nod_bc_MHD
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
!
      type(nodal_bcs_4_induction_type), intent(inout) :: Bnod_bcs
!
!
      call count_num_bc_vect(iflag_bc_fixed, nod_grp,                   &
     &    a_potential_nod, Bnod_bcs%nod_bc_a)
      call count_num_bc_vect(iflag_bc_sgs, nod_grp,                     &
     &    a_potential_nod, Bnod_bcs%sgs_bc_a)
!
      call alloc_vector_nod_bc_type(node%numnod, Bnod_bcs%nod_bc_a)
      call alloc_vector_nod_bc_type(node%numnod, Bnod_bcs%sgs_bc_a)
!
      Bnod_bcs%nod_bc_a%vect_bc_name(1) = 'vector_potential_x'
      Bnod_bcs%nod_bc_a%vect_bc_name(2) = 'vector_potential_y'
      Bnod_bcs%nod_bc_a%vect_bc_name(3) = 'vector_potential_z'
!
      call set_bc_fixed_vect_p_id(node, nod_grp, a_potential_nod,       &
     &    Bnod_bcs%nod_bc_a, Bnod_bcs%sgs_bc_a)
!
!   set node id in an element
      call ele_nodal_bc_vector_whole(node, ele, Bnod_bcs%nod_bc_a)
      call ele_nodal_bc_vector_whole(node, ele, Bnod_bcs%sgs_bc_a)
!
      end subroutine set_bc_vect_p_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_magne_id                                        &
     &         (node, ele, nod_grp, iphys, nod_fld, Bnod_bcs)
!
      use t_geometry_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use m_bc_data_list
      use m_boundary_condition_IDs
      use count_num_nod_bc_MHD
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
      type(nodal_bcs_4_induction_type), intent(inout) :: Bnod_bcs
!
!
      call count_num_bc_magne(iflag_bc_fixed, nod_grp,                  &
     &    magne_nod, Bnod_bcs%nod_bc_b)
      call count_num_bc_vect(iflag_bc_sgs, nod_grp,                     &
     &    magne_nod, Bnod_bcs%sgs_bc_b)
!
      call alloc_vector_nod_bc_type(node%numnod, Bnod_bcs%nod_bc_b)
      call alloc_vector_nod_bc_type(node%numnod, Bnod_bcs%sgs_bc_b)
!
      Bnod_bcs%nod_bc_b%vect_bc_name(1) = 'magnetic_x'
      Bnod_bcs%nod_bc_b%vect_bc_name(2) = 'magnetic_y'
      Bnod_bcs%nod_bc_b%vect_bc_name(3) = 'magnetic_z'
!
      call set_bc_fixed_magne_id(node, nod_grp, magne_nod,              &
     &    iphys%i_magne, Bnod_bcs%nod_bc_b, Bnod_bcs%sgs_bc_b, nod_fld)
!
!   set node id in an element for magnetic boundary 
!
      call ele_nodal_bc_vector_whole(node, ele, Bnod_bcs%nod_bc_b)
      call ele_nodal_bc_vector_whole(node, ele, Bnod_bcs%sgs_bc_b)
!
      end subroutine set_bc_magne_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_current_id(node, ele, nod_grp, Bnod_bcs)
!
      use t_geometry_data
      use t_group_data
      use m_boundary_condition_IDs
      use m_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
!
      type(nodal_bcs_4_induction_type), intent(inout) :: Bnod_bcs
!
!
      call count_num_bc_vect (iflag_bc_fixed, nod_grp,                  &
     &    current_nod, Bnod_bcs%nod_bc_j)
!
      call alloc_vector_nod_bc_type(node%numnod, Bnod_bcs%nod_bc_j)
!
      Bnod_bcs%nod_bc_j%vect_bc_name(1) = 'current_x'
      Bnod_bcs%nod_bc_j%vect_bc_name(2) = 'current_y'
      Bnod_bcs%nod_bc_j%vect_bc_name(3) = 'current_z'
!
      call set_bc_fixed_current_id(node, nod_grp, current_nod,          &
     &    Bnod_bcs%nod_bc_j)
!
      call ele_nodal_bc_vector_whole(node, ele, Bnod_bcs%nod_bc_j)
!
      end subroutine set_bc_current_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_m_potential_id                                  &
     &         (node, ele, conduct, insulate, nod_grp, Bnod_bcs)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use m_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct, insulate
      type(group_data), intent(in) :: nod_grp
!
      type(nodal_bcs_4_induction_type), intent(inout) :: Bnod_bcs
!
!
      call count_num_bc_magp                                            &
     &   (nod_grp, Bnod_bcs%nod_bc_f, Bnod_bcs%sgs_bc_f)
!
      call alloc_scalar_nod_bc_type(node%numnod, Bnod_bcs%nod_bc_f)
      call alloc_scalar_nod_bc_type(node%numnod, Bnod_bcs%sgs_bc_f)
!
      Bnod_bcs%nod_bc_f%scalar_bc_name = fhd_mag_potential
!
      call set_bc_fixed_m_potential_id(node, nod_grp,                   &
     &    e_potential_nod, Bnod_bcs%nod_bc_f, Bnod_bcs%sgs_bc_f)
!
!   set node id in an element for the pressure boundary 
!
      call ele_nodal_bc_potential_whole(node, ele, Bnod_bcs%nod_bc_f)
!
      call set_ele_nodal_bc_mag_p_layer                                 &
     &   (node, ele, insulate, Bnod_bcs%nod_bc_f, Bnod_bcs%nod_bc_fins)
      call set_ele_nodal_bc_mag_p_layer                                 &
     &   (node, ele, conduct,  Bnod_bcs%nod_bc_f, Bnod_bcs%nod_bc_fcd)
      call dealloc_scalar_ibc_type(Bnod_bcs%nod_bc_f)
!
!
      call ele_nodal_bc_potential_whole(node, ele, Bnod_bcs%sgs_bc_f)
      call dealloc_scalar_ibc_type(Bnod_bcs%sgs_bc_f)
!
      end subroutine set_bc_m_potential_id
!
!  ---------------------------------------------------------------------
!
      end module t_bc_data_magne
