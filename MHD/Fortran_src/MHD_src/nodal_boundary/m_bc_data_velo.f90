!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_bc_velo_id(node, ele, fluid, nod_grp)
!!      subroutine set_bc_press_id                                      &
!!     &         (node, ele, fluid, nod_grp, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(group_data), intent(in) :: nod_grp
!
      module m_bc_data_velo
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
!>      Structure for nodal boudnary for fixed velocity
      type(vect_fixed_nod_bc_type), save :: nod_bc1_v
!>      Structure for nodal boudnary for SGS fixed velocity
      type(vect_fixed_nod_bc_type), save :: sgs_bc1_v
!>      Structure for nodal boudnary for fixed pressure
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_p
!>      Structure for nodal boudnary for SGS fixed presure
      type(scaler_fixed_nod_bc_type), save :: sgs_bc1_p
!
!
!>      Structure for nodal boudnary for non-radial velocity
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_vr0
!>      Structure for nodal boudnary for free-slip velocity on plane
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_vfree
!>      Structure for nodal boudnary for special velocity on plane
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_vsp
!
!>      Structure for nodal boudnary for rotation
      type(scaler_rotaion_nod_bc_type), save :: nod_bc1_rot
!
!>      Structure for nodal boudnary for fixed vorticity
      type(vect_fixed_nod_bc_type), save :: nod_bc1_w
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_velo_id(node, ele, fluid, nod_grp)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use m_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_vectors
      use set_bc_scalars
!
      use set_ele_nod_bc_vectors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: nod_grp
!
!
      call count_num_bc_velo(nod_grp, nod_bc1_v, sgs_bc1_v,             &
     &    nod_bc1_rot, nod_bc1_vfree, nod_bc1_vsp, nod_bc1_vr0)
!
      nod_bc1_w%num_bc_nod(1:3) = 0
      nod_bc1_w%nmax_bc = 0
!
      if ( iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 v'
      call alloc_vector_nod_bc_type(node%numnod, nod_bc1_v)
      call alloc_vector_nod_bc_type(node%numnod, sgs_bc1_v)
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_vr0)
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_vfree)
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_vsp)
      call alloc_rotate_nod_bc_type(node%numnod, nod_bc1_rot)
!
      if (iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 P'
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_p)
      call alloc_scalar_nod_bc_type(node%numnod, sgs_bc1_p)
!
      nod_bc1_v%vect_bc_name(1) = 'velocity_x'
      nod_bc1_v%vect_bc_name(2) = 'velocity_y'
      nod_bc1_v%vect_bc_name(3) = 'velocity_z'
!
      if(iflag_debug .gt. 0) write(*,*) 'set_bc_fixed_velo_id'
      call set_bc_fixed_velo_id(node, nod_grp, velo_nod,                &
     &    nod_bc1_v, sgs_bc1_v, nod_bc1_rot)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_bc_velo_4_sphere_id'
      call set_bc_velo_4_sphere_id(node, nod_grp, velo_nod,             &
     &    nod_bc1_vfree, nod_bc1_vr0, nod_bc1_vsp)
!
!   set node id in an element for velocity boundary 
!
      if(iflag_debug .gt. 0) write(*,*) 'ele_nodal_bc_vector_layer'
      call ele_nodal_bc_vector_layer(node, ele, fluid, nod_bc1_v)
      call ele_nodal_bc_vector_layer(node, ele, fluid, sgs_bc1_v)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_rotate'
      call set_ele_nodal_bc_4_rotate(node, ele, fluid, nod_bc1_rot)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vfree'
      call ele_nodal_bc_scalar_layer(node, ele, fluid, nod_bc1_vfree)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vr0'
      call ele_nodal_bc_scalar_layer(node, ele, fluid, nod_bc1_vr0)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_velo_sph'
      call ele_nodal_bc_scalar_layer(node, ele, fluid, nod_bc1_vsp)
!
!
      end subroutine set_bc_velo_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_press_id                                        &
     &         (node, ele, fluid, nod_grp, iphys, nod_fld)
!
      use t_geometry_data
      use t_group_data
      use t_phys_address
      use t_phys_data
      use t_geometry_data_MHD
      use m_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_scalars
      use set_ele_nod_bc_vectors
      use set_nodal_boundary
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: nod_grp
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call count_num_bc_scl_w_SGS                                       &
     &   (nod_grp, press_nod, nod_bc1_p, sgs_bc1_p)
!
      if (iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 P'
      call alloc_scalar_nod_bc_type(node%numnod, nod_bc1_p)
      call alloc_scalar_nod_bc_type(node%numnod, sgs_bc1_p)
!
      nod_bc1_p%scalar_bc_name = fhd_press
      call set_bc_fixed_temp_id(node, nod_grp, press_nod,               &
     &    nod_bc1_p, sgs_bc1_p)
!
      call set_potential_4_fixed_press(node%numnod, nod_fld%ntot_phys,  &
     &    iphys%i_press, iphys%i_p_phi, nod_fld%d_fld, nod_bc1_p)

      call set_potential_4_fixed_press(node%numnod, nod_fld%ntot_phys,  &
     &    iphys%i_press, iphys%i_p_phi, nod_fld%d_fld, sgs_bc1_p)
!
!   set node id in an element for the pressure boundary
!
      call ele_nodal_bc_potential_layer(node, ele, fluid, nod_bc1_p)
      call ele_nodal_bc_potential_layer(node, ele, fluid, sgs_bc1_p)
!
      end subroutine set_bc_press_id
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_velo
