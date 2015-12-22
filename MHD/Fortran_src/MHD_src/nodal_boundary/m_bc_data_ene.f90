!
!     module m_bc_data_ene
!.......................................................................
!
!      Written by Kemorin on Feb., 2004
!
!!      subroutine set_bc_temp_id(node, ele, nod_grp, iphys, nod_fld)
!!      subroutine set_bc_composition_id(node, ele, nod_grp)
!
!
      module m_bc_data_ene
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
!
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_t
!
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_c
!
!
      type(scaler_fixed_nod_bc_type), save :: sgs_bc1_t
!
      type(scaler_fixed_nod_bc_type), save :: sgs_bc1_c
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_temp_id(node, ele, nod_grp, iphys, nod_fld)
!
      use t_geometry_data
      use t_group_data
      use t_phys_address
      use t_phys_data
      use m_control_parameter
      use m_bc_data_list
      use m_geometry_data_MHD
      use count_num_nod_bc_MHD
      use set_bc_scalars
      use set_ele_nod_bc_vectors
      use set_nodal_boundary
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call count_num_bc_scl_w_SGS                                     &
     &   (nod_grp, temp_nod, nod_bc1_t, sgs_bc1_t)
!
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_t)
      call alloc_scalar_nod_bc_type(node%numnod, sgs_bc1_t)
!
      nod_bc1_t%scalar_bc_name = fhd_temp
      call set_bc_fixed_temp_id(node, nod_grp, temp_nod,                &
     &    nod_bc1_t, sgs_bc1_t)
!
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
        call set_fixed_bc_4_par_temp(node%numnod, nod_fld%ntot_phys,    &
     &      iphys%i_ref_t, nod_fld%d_fld, nod_bc1_t)
      end if
!
!   set node id in an element for the temperature boundary 
!
      call ele_nodal_bc_scalar_layer(node, ele, fluid1, nod_bc1_t)
      call ele_nodal_bc_scalar_layer(node, ele, fluid1, sgs_bc1_t)
!
      end subroutine set_bc_temp_id
!
!  ---------------------------------------------------------------------
 !
      subroutine set_bc_composition_id(node, ele, nod_grp)
!
      use t_geometry_data
      use t_group_data
      use m_bc_data_list
      use m_geometry_data_MHD
      use count_num_nod_bc_MHD
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_scl_w_SGS                                       &
     &   (nod_grp, light_nod, nod_bc1_c, sgs_bc1_c)
!
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_c)
      call alloc_scalar_nod_bc_type(node%numnod, sgs_bc1_c)
!
      nod_bc1_c%scalar_bc_name = fhd_light
      call set_bc_fixed_temp_id(node, nod_grp, light_nod,               &
     &    nod_bc1_c, sgs_bc1_c)
!
!   set node id in an element for composition boundary
!
      call ele_nodal_bc_scalar_layer(node, ele, fluid1, nod_bc1_c)
      call ele_nodal_bc_scalar_layer(node, ele, fluid1, sgs_bc1_c)
!
      end subroutine set_bc_composition_id
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_ene
