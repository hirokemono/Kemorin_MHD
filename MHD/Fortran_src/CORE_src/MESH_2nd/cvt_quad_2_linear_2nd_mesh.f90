!
!      module cvt_quad_2_linear_2nd_mesh
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine generate_2nd_nod_on_surf
!      subroutine connect_quad_4_sim_2_linear_2nd
!      subroutine connect_lag_4_sim_2_linear_2nd
!      subroutine generate_2nd_linear_group
!      subroutine init_2nd_data_on_surf
!      subroutine copy_2nd_data_on_vertex
!      subroutine generate_2nd_data_on_surf
!
      module cvt_quad_2_linear_2nd_mesh
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: ie_4_333(:,:)
      private :: ie_4_333
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine generate_2nd_nod_on_surf
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      use set_geometry_4_quad27
      use coordinate_converter
!
!
      nnod_2nd = numnod + numsurf + numele
!
      call allocate_2nd_node_position
!
      call set_position_on_surf(numnod, numsurf, numele,                &
     &    xx, x_ele, x_surf, nnod_2nd, xx_2nd)
!
      call position_2_sph(nnod_2nd, xx_2nd,                             &
     &    radius_2nd, theta_2nd, phi_2nd, a_radius_2nd,                 &
     &    s_cyl_2nd, a_s_cyl_2nd)
!
      end subroutine generate_2nd_nod_on_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine connect_quad_4_sim_2_linear_2nd
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      use m_27quad_2_8x8linear
!
!
       nele_2nd = 8*numele
!
      call allocate_2nd_element_connect
      call allocate_2nd_element_data
      allocate(ie_4_333(numele,27) )
!
      call init_27quad_2_8x8linear
      call gen_connect_quad27_from_quad20(numnod, numele, numsurf, ie,  &
     &    isf_4_ele, ie_4_333)
!
      call set_27quad_2_8x8linear(numele, ie_4_333,                     &
     &    nnod_2nd, ie_2nd)
!
      deallocate(ie_4_333)
!
      end subroutine connect_quad_4_sim_2_linear_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine connect_lag_4_sim_2_linear_2nd
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      use m_27quad_2_8x8linear
!
!
       nele_2nd = 8*numele
!
      call allocate_2nd_element_connect
      call allocate_2nd_element_data
!
      call init_27quad_2_8x8linear
      call set_27quad_2_8x8linear(numele, ie, nnod_2nd, ie_2nd)
!
      end subroutine connect_lag_4_sim_2_linear_2nd
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine generate_2nd_linear_group
!
      use m_element_group
      use m_surface_group
      use m_2nd_group_data
!
      use convert_group_2_linear
      use link_group_to_1st_mesh
!
!
      call link_node_group
!
      num_mat_2nd = num_mat
      num_mat_bc_2nd = 8*num_mat_bc
      call allocate_2nd_element_group
!
      call convert_ele_group_2_linear(num_mat, num_mat_bc,              &
     &          mat_name, mat_istack, mat_item, num_mat_bc_2nd,         &
     &          mat_name_2nd, mat_istack_2nd, mat_item_2nd)
!
      num_surf_2nd = num_surf
      num_surf_bc_2nd = 4*num_surf_bc
      call allocate_2nd_surface_group
!
      call convert_surf_group_2_linear(num_surf, num_surf_bc,           &
     &          surf_name, surf_istack, surf_item, num_surf_bc_2nd,     &
     &          surf_name_2nd, surf_istack_2nd, surf_item_2nd)
!
      end subroutine generate_2nd_linear_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_2nd_data_on_surf
!
      use m_2nd_geometry_param
      use m_2nd_phys_data
!
!
      call link_nodal_field_names
      call alloc_phys_data_type(nnod_2nd, phys_2nd)
!
      end subroutine init_2nd_data_on_surf
!
!  ---------------------------------------------------------------------
!
      subroutine copy_2nd_data_on_vertex
!
      use m_geometry_parameter
      use m_node_phys_data
      use m_2nd_geometry_param
      use m_2nd_phys_data
!
      use set_data_4_quad27
!
!
      call copy_original_data(numnod, num_tot_nod_phys,                 &
     &    d_nod, nnod_2nd, phys_2nd%d_fld)
!
      end subroutine copy_2nd_data_on_vertex
!
!  ---------------------------------------------------------------------
!
      subroutine generate_2nd_data_on_surf
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      use set_data_4_quad27
!
!
      call set_fields_on_surf(numnod, numsurf, numele,                  &
     &    ie, ie_surf, num_tot_nod_phys, nnod_2nd, phys_2nd%d_fld)
!
      end subroutine generate_2nd_data_on_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module cvt_quad_2_linear_2nd_mesh
