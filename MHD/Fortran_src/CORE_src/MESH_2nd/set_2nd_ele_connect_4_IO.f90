!
!      module set_2nd_ele_connect_4_IO
!
!     Written by H. Matsui on June, 2007
!
!      subroutine copy_2nd_ele_connect_to_IO
!      subroutine copy_2nd_ele_connect_from_IO
!
      module set_2nd_ele_connect_4_IO
!
      use m_precision
!
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_ele_connect_to_IO
!
!
      numele_dummy =     nele_2nd
      nnod_4_ele_dummy = nnod_4_ele_2nd
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
      globalelmid_dummy = globalelmid_2nd
      i_ele_dummy =       elmtyp_2nd
      nodelm_dummy =      nodelm_2nd
      ie_dummy =          ie_2nd
!
      end subroutine copy_2nd_ele_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_ele_connect_from_IO
!
      use m_geometry_constants
!
      nele_2nd = numele_dummy
      first_ele_type_2nd = i_ele_dummy(1)
!
      if (first_ele_type_2nd .eq. 332) then
        nnod_4_ele_2nd =  num_t_quad
        nnod_4_surf_2nd = num_quad_sf
        nnod_4_edge_2nd = num_quad_edge
      else if (first_ele_type_2nd .eq. 331) then
        nnod_4_ele_2nd =  num_t_linear
        nnod_4_surf_2nd = num_linear_sf
        nnod_4_edge_2nd = num_linear_edge
      else if (first_ele_type_2nd .eq. 333) then
        nnod_4_ele_2nd =  num_t_lag
        nnod_4_surf_2nd = num_lag_sf
        nnod_4_edge_2nd = num_quad_edge
      end if
!
      call allocate_2nd_element_connect
!
      globalelmid_2nd = globalelmid_dummy
      elmtyp_2nd =      i_ele_dummy
      nodelm_2nd =      nodelm_dummy
      ie_2nd =          ie_dummy
!
      call deallocate_ele_info_dummy
!
      end subroutine copy_2nd_ele_connect_from_IO
!
!------------------------------------------------------------------
!
      end module set_2nd_ele_connect_4_IO
