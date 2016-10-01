!
!     module t_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_bc_velo_id                                       &
!!     &          (IO_bc, node, ele, fluid, nod_grp, Vnod_bcs)
!!      subroutine set_bc_press_id                                      &
!!     &          (IO_bc, node, ele, fluid, nod_grp, Vnod_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(group_data), intent(in) :: nod_grp
!!        type(nodal_bcs_4_momentum_type), intent(inout) :: Vnod_bcs
!
      module t_bc_data_velo
!
      use m_precision
      use t_nodal_bc_data
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use t_boundary_field_IO
!
      implicit  none
!
!>      Structure for nodal boudnary for momentum equaiton
      type nodal_bcs_4_momentum_type
!>      Structure for nodal boudnary for fixed velocity
        type(vect_fixed_nod_bc_type) :: nod_bc_v
!>      Structure for nodal boudnary for SGS fixed velocity
        type(vect_fixed_nod_bc_type) :: sgs_bc_v
!>      Structure for nodal boudnary for fixed pressure
        type(scaler_fixed_nod_bc_type) :: nod_bc_p
!>      Structure for nodal boudnary for SGS fixed presure
        type(scaler_fixed_nod_bc_type) :: sgs_bc_p
!
!
!>      Structure for nodal boudnary for non-radial velocity
        type(scaler_fixed_nod_bc_type) :: nod_bc_vr0
!>      Structure for nodal boudnary for free-slip velocity on sphere
        type(scaler_fixed_nod_bc_type) :: nod_bc_vfree
!>      Structure for nodal boudnary for special velocity on plane
        type(scaler_fixed_nod_bc_type) :: nod_bc_vsp
!
!>      Structure for nodal boudnary for rotation
        type(scaler_rotaion_nod_bc_type) :: nod_bc_rot
!
!>      Structure for nodal boudnary for fixed vorticity
        type(vect_fixed_nod_bc_type) :: nod_bc_w
      end type nodal_bcs_4_momentum_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_velo_id                                         &
     &          (IO_bc, node, ele, fluid, nod_grp, Vnod_bcs)
!
      use m_bc_data_list
      use count_num_nod_bc_MHD
      use set_bc_vectors
      use set_bc_scalars
!
      use set_ele_nod_bc_vectors
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: nod_grp
!
      type(nodal_bcs_4_momentum_type), intent(inout) :: Vnod_bcs
!
!
      call count_num_bc_velo(nod_grp,                                   &
     &    Vnod_bcs%nod_bc_v, Vnod_bcs%sgs_bc_v,                         &
     &    Vnod_bcs%nod_bc_rot, Vnod_bcs%nod_bc_vfree,                   &
     &    Vnod_bcs%nod_bc_vsp, Vnod_bcs%nod_bc_vr0)
!
      Vnod_bcs%nod_bc_w%num_bc_nod(1:3) = 0
      Vnod_bcs%nod_bc_w%nmax_bc = 0
!
      if ( iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 v'
      call alloc_vector_nod_bc_type(node%numnod, Vnod_bcs%nod_bc_v)
      call alloc_vector_nod_bc_type(node%numnod, Vnod_bcs%sgs_bc_v)
      call alloc_scalar_nod_bc_type(node%numnod, Vnod_bcs%nod_bc_vr0)
      call alloc_scalar_nod_bc_type(node%numnod, Vnod_bcs%nod_bc_vfree)
      call alloc_scalar_nod_bc_type(node%numnod, Vnod_bcs%nod_bc_vsp)
      call alloc_rotate_nod_bc_type(node%numnod, Vnod_bcs%nod_bc_rot)
!
      Vnod_bcs%nod_bc_v%vect_bc_name(1) = 'velocity_x'
      Vnod_bcs%nod_bc_v%vect_bc_name(2) = 'velocity_y'
      Vnod_bcs%nod_bc_v%vect_bc_name(3) = 'velocity_z'
!
      if(iflag_debug .gt. 0) write(*,*) 'set_bc_fixed_velo_id'
      call set_bc_fixed_velo_id(IO_bc, node, nod_grp, velo_nod,         &
     &    Vnod_bcs%nod_bc_v, Vnod_bcs%sgs_bc_v, Vnod_bcs%nod_bc_rot)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_bc_velo_4_sphere_id'
      call set_bc_velo_4_sphere_id(node, nod_grp, velo_nod,             &
     &    Vnod_bcs%nod_bc_vfree,                                        &
     &    Vnod_bcs%nod_bc_vr0, Vnod_bcs%nod_bc_vsp)
!
!   set node id in an element for velocity boundary 
!
      if(iflag_debug .gt. 0) write(*,*) 'ele_nodal_bc_vector_layer'
      call ele_nodal_bc_vector_layer                                    &
     &   (node, ele, fluid, Vnod_bcs%nod_bc_v)
      call ele_nodal_bc_vector_layer                                    &
     &   (node, ele, fluid, Vnod_bcs%sgs_bc_v)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_rotate'
      call set_ele_nodal_bc_4_rotate                                    &
     &   (node, ele, fluid, Vnod_bcs%nod_bc_rot)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vfree'
      call ele_nodal_bc_scalar_layer                                    &
     &   (node, ele, fluid, Vnod_bcs%nod_bc_vfree)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vr0'
      call ele_nodal_bc_scalar_layer                                    &
     &   (node, ele, fluid, Vnod_bcs%nod_bc_vr0)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_velo_sph'
      call ele_nodal_bc_scalar_layer                                    &
     &   (node, ele, fluid, Vnod_bcs%nod_bc_vsp)
!
      end subroutine set_bc_velo_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_press_id                                        &
     &          (IO_bc, node, ele, fluid, nod_grp, Vnod_bcs)
!
      use m_bc_data_list
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
!
      type(nodal_bcs_4_momentum_type), intent(inout) :: Vnod_bcs
!
!
      call count_num_bc_scl_w_SGS                                       &
     &   (nod_grp, press_nod, Vnod_bcs%nod_bc_p, Vnod_bcs%sgs_bc_p)
!
      if (iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 P'
      call alloc_scalar_nod_bc_type(node%numnod, Vnod_bcs%nod_bc_p)
      call alloc_scalar_nod_bc_type(node%numnod, Vnod_bcs%sgs_bc_p)
!
      Vnod_bcs%nod_bc_p%scalar_bc_name = fhd_press
      call set_bc_fixed_temp_id(IO_bc, node, nod_grp, press_nod,        &
     &    Vnod_bcs%nod_bc_p, Vnod_bcs%sgs_bc_p)
!
      call set_potential_4_fixed_press(Vnod_bcs%nod_bc_p)
      call set_potential_4_fixed_press(Vnod_bcs%sgs_bc_p)
!
!   set node id in an element for the pressure boundary
!
      call ele_nodal_bc_potential_layer                                 &
     &   (node, ele, fluid, Vnod_bcs%nod_bc_p)
      call ele_nodal_bc_potential_layer                                 &
     &   (node, ele, fluid, Vnod_bcs%sgs_bc_p)
!
      end subroutine set_bc_press_id
!
!  ---------------------------------------------------------------------
!
      end module t_bc_data_velo
