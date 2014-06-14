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
      numele_dummy =     ele_2nd%numele
      nnod_4_ele_dummy = ele_2nd%nnod_4_ele
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
      globalelmid_dummy = ele_2nd%iele_global
      i_ele_dummy =       ele_2nd%elmtyp
      nodelm_dummy =      ele_2nd%nodelm
      ie_dummy =          ele_2nd%ie
!
      end subroutine copy_2nd_ele_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_ele_connect_from_IO
!
      use m_geometry_constants
!
      ele_2nd%numele = numele_dummy
      ele_2nd%first_ele_type = i_ele_dummy(1)
!
      if (ele_2nd%first_ele_type .eq. 332) then
        ele_2nd%nnod_4_ele =  num_t_quad
        surf_2nd%nnod_4_surf = num_quad_sf
        edge_2nd%nnod_4_edge = num_quad_edge
      else if (ele_2nd%first_ele_type .eq. 331) then
        ele_2nd%nnod_4_ele =  num_t_linear
        surf_2nd%nnod_4_surf = num_linear_sf
        edge_2nd%nnod_4_edge = num_linear_edge
      else if (ele_2nd%first_ele_type .eq. 333) then
        ele_2nd%nnod_4_ele =  num_t_lag
        surf_2nd%nnod_4_surf = num_lag_sf
        edge_2nd%nnod_4_edge = num_quad_edge
      end if
!
      call allocate_ele_connect_type(ele_2nd)
!
      ele_2nd%iele_global = globalelmid_dummy
      ele_2nd%elmtyp =      i_ele_dummy
      ele_2nd%nodelm =      nodelm_dummy
      ele_2nd%ie =          ie_dummy
!
      call deallocate_ele_info_dummy
!
      end subroutine copy_2nd_ele_connect_from_IO
!
!------------------------------------------------------------------
!
      end module set_2nd_ele_connect_4_IO
