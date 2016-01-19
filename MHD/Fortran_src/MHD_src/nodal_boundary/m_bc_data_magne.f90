!
!     module m_bc_data_magne
!.......................................................................
!
!     Written by Kemorin
!
!!      subroutine set_bc_vect_p_id(node, ele, nod_grp)
!!      subroutine set_bc_magne_id(node, ele, nod_grp, nod_fld)
!!      subroutine set_bc_current_id(node, ele, nod_grp)
!!      subroutine set_bc_m_potential_id(node, ele, nod_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: nod_grp
!
      module m_bc_data_magne
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
      type(vect_fixed_nod_bc_type), save :: nod_bc1_a
!
      type(vect_fixed_nod_bc_type), save :: nod_bc1_b
!
      type(vect_fixed_nod_bc_type), save :: nod_bc1_j
!
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_f
!
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_fcd
!
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_fins
!
      type(vect_fixed_nod_bc_type), save :: sgs_bc1_a
!
      type(vect_fixed_nod_bc_type), save :: sgs_bc1_b
!
      type(scaler_fixed_nod_bc_type), save :: sgs_bc1_f
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_vect_p_id(node, ele, nod_grp)
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
!
      call count_num_bc_vect(iflag_bc_fixed, nod_grp,                   &
     &    a_potential_nod, nod_bc1_a)
      call count_num_bc_vect(iflag_bc_sgs, nod_grp,                     &
     &    a_potential_nod, sgs_bc1_a)
!
      call alloc_vector_nod_bc_type(node%numnod, nod_bc1_a)
      call alloc_vector_nod_bc_type(node%numnod, sgs_bc1_a)
!
      nod_bc1_a%vect_bc_name(1) = 'vector_potential_x'
      nod_bc1_a%vect_bc_name(2) = 'vector_potential_y'
      nod_bc1_a%vect_bc_name(3) = 'vector_potential_z'
!
      call set_bc_fixed_vect_p_id(node, nod_grp, a_potential_nod,       &
     &    nod_bc1_a, sgs_bc1_a)
!
!   set node id in an element
      call ele_nodal_bc_vector_whole(node, ele, nod_bc1_a)
      call ele_nodal_bc_vector_whole(node, ele, sgs_bc1_a)
!
      end subroutine set_bc_vect_p_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_magne_id(node, ele, nod_grp, iphys, nod_fld)
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
      type(phys_data), intent(inout) :: nod_fld
!
!
      call count_num_bc_magne(iflag_bc_fixed, nod_grp,                  &
     &    magne_nod, nod_bc1_b)
      call count_num_bc_vect(iflag_bc_sgs, nod_grp,                     &
     &    magne_nod, sgs_bc1_b)
!
      call alloc_vector_nod_bc_type(node%numnod, nod_bc1_b)
      call alloc_vector_nod_bc_type(node%numnod, sgs_bc1_b)
!
      nod_bc1_b%vect_bc_name(1) = 'magnetic_x'
      nod_bc1_b%vect_bc_name(2) = 'magnetic_y'
      nod_bc1_b%vect_bc_name(3) = 'magnetic_z'
!
      call set_bc_fixed_magne_id(node, nod_grp, magne_nod,              &
     &    iphys%i_magne, nod_bc1_b, sgs_bc1_b, nod_fld)
!
!   set node id in an element for magnetic boundary 
!
      call ele_nodal_bc_vector_whole(node, ele, nod_bc1_b)
      call ele_nodal_bc_vector_whole(node, ele, sgs_bc1_b)
!
      end subroutine set_bc_magne_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_current_id(node, ele, nod_grp)
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
!
      call count_num_bc_vect (iflag_bc_fixed, nod_grp,                  &
     &    current_nod, nod_bc1_j)
!
      call alloc_vector_nod_bc_type(node%numnod, nod_bc1_j)
!
      nod_bc1_j%vect_bc_name(1) = 'current_x'
      nod_bc1_j%vect_bc_name(2) = 'current_y'
      nod_bc1_j%vect_bc_name(3) = 'current_z'
!
      call set_bc_fixed_current_id(node, nod_grp, current_nod,          &
     &    nod_bc1_j)
!
      call ele_nodal_bc_vector_whole(node, ele, nod_bc1_j)
!
      end subroutine set_bc_current_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_m_potential_id(node, ele, nod_grp)
!
      use t_geometry_data
      use t_group_data
      use m_geometry_data_MHD
      use m_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_magp(nod_grp, nod_bc1_f, sgs_bc1_f)
!
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_f)
      call alloc_scalar_nod_bc_type(node%numnod, sgs_bc1_f)
!
      nod_bc1_f%scalar_bc_name = fhd_mag_potential
!
      call set_bc_fixed_m_potential_id(node, nod_grp,                   &
     &    e_potential_nod, nod_bc1_f, sgs_bc1_f)
!
!   set node id in an element for the pressure boundary 
!
      call ele_nodal_bc_potential_whole(node, ele, nod_bc1_f)
!
      call set_ele_nodal_bc_mag_p_layer                                 &
     &   (node, ele, insulate1, nod_bc1_f, nod_bc1_fins)
      call set_ele_nodal_bc_mag_p_layer                                 &
     &   (node, ele, conduct1,  nod_bc1_f, nod_bc1_fcd)
      call dealloc_scalar_ibc_type(nod_bc1_f)
!
!
      call ele_nodal_bc_potential_whole(node, ele, sgs_bc1_f)
      call dealloc_scalar_ibc_type(sgs_bc1_f)
!
      end subroutine set_bc_m_potential_id
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_magne
