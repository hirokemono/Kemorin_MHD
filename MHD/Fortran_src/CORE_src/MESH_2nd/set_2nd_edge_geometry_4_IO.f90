!set_2nd_edge_geometry_4_IO.f90
!      module set_2nd_edge_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_2nd_edge_connect_to_IO
!      subroutine copy_2nd_edge_geom_to_IO
!      subroutine copy_2nd_edge_geom_to_IO_sph
!      subroutine copy_2nd_edge_geom_to_IO_cyl
!
!      subroutine copy_2nd_edge_connect_from_IO
!      subroutine copy_2nd_edge_geom_from_IO
!      subroutine copy_2nd_edge_geom_from_IO_sph
!      subroutine copy_2nd_edge_geom_from_IO_cyl
!
      module set_2nd_edge_geometry_4_IO
!
      use m_precision
!
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_edge_geometry_data
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
      subroutine copy_2nd_edge_connect_to_IO
!
      use m_geometry_constants
!
!
      numele_dummy = nele_2nd
!
      nsf_4_ele_IO = nsurf_2nd
      nsurf_in_ele_IO = nedge_4_surf
!
      ned_4_ele_IO = nele_2nd
!
      numele_dummy = nedge_2nd
      nnod_4_ele_dummy = nnod_4_edge_2nd
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
      call allocate_surface_connect_IO
      call allocate_edge_connect_IO
!
      if      (nnod_4_edge_2nd .eq. num_linear_edge) then
        i_ele_dummy(1:nedge_2nd) = 111
      else if (nnod_4_edge_2nd .eq. num_quad_edge) then
        i_ele_dummy(1:nedge_2nd) = 112
      end if
!
      globalelmid_dummy(1:nedge_2nd) = globaledgeid_2nd(1:nedge_2nd)
      nodelm_dummy(1:nedge_2nd) =      nnod_4_edge_2nd
      ie_dummy(1:nedge_2nd,1:nnod_4_edge_2nd)                           &
     &        = ie_edge_2nd(1:nedge_2nd,1:nnod_4_edge_2nd)
!
      isf_4_ele_IO(1:nsurf_2nd,1:nedge_4_surf)                          &
     &        = iedge_4_sf_2nd(1:nsurf_2nd,1:nedge_4_surf)
      iedge_4_ele_IO(1:nele_2nd,1:nedge_4_ele)                          &
     &        = iedge_4_ele_2nd(1:nele_2nd,1:nedge_4_ele)
!
      end subroutine copy_2nd_edge_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_to_IO
!
      numnod_dummy =        nedge_2nd
      internal_node_dummy = internal_edge
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:nedge_2nd) = globaledgeid_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,1:3) =      x_edge_2nd(1:nedge_2nd,1:3)
      ele_scalar_IO(1:nedge_2nd) =     edge_length_2nd(1:nedge_2nd)
      ele_vector_IO(1:nedge_2nd,1:3) = edge_vect_2nd(1:nedge_2nd,1:3)
!
      end subroutine copy_2nd_edge_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_to_IO_sph
!
      numnod_dummy = nedge_2nd
      internal_node_dummy = internal_edge
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:nedge_2nd) = globaledgeid_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,1) = r_edge_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,2) = theta_edge_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,3) = phi_edge_2nd(1:nedge_2nd)
      ele_scalar_IO(1:nedge_2nd) =     edge_length_2nd(1:nedge_2nd)
      ele_vector_IO(1:nedge_2nd,1:3)                                    &
     &      = edge_vect_2nd_sph(1:nedge_2nd,1:3)
!
      end subroutine copy_2nd_edge_geom_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_to_IO_cyl
!
      numnod_dummy = nedge_2nd
      internal_node_dummy = internal_edge
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:nedge_2nd) = globaledgeid_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,1) = s_edge_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,2) = phi_edge_2nd(1:nedge_2nd)
      xx_dummy(1:nedge_2nd,3) = x_edge_2nd(1:nedge_2nd,3)
      ele_scalar_IO(1:nedge_2nd) =     edge_length_2nd(1:nedge_2nd)
      ele_vector_IO(1:nedge_2nd,1:3)                                    &
     &      = edge_vect_2nd_cyl(1:nedge_2nd,1:3)
!
      end subroutine copy_2nd_edge_geom_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_connect_from_IO
!
      use m_geometry_constants
!
!
      nedge_2nd = numele_dummy
!
      call allocate_2nd_edge_connect
      call allocate_2nd_edge_4_ele
!
      globaledgeid_2nd(1:nedge_2nd) = globalelmid_dummy(1:nedge_2nd)
      ie_edge_2nd(1:nedge_2nd,1:nnod_4_edge_2nd)                        &
     &        = ie_dummy(1:nedge_2nd,1:nnod_4_edge_2nd)
!
      iedge_4_sf_2nd(1:nsurf_2nd,1:nedge_4_surf)                        &
     &        = isf_4_ele_IO(1:nsurf_2nd,1:nedge_4_surf)
!
      iedge_4_ele_2nd(1:nele_2nd,1:nedge_4_ele)                         &
     &        = iedge_4_ele_IO(1:nele_2nd,1:nedge_4_ele)
!
      call deallocate_surface_connect_IO
      call deallocate_ele_info_dummy
      call deallocate_edge_connect_IO
!
      end subroutine copy_2nd_edge_connect_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_from_IO
!
      call allocate_2nd_edge_geometry
      call allocate_2nd_edge_vects
!
      x_edge_2nd(1:nedge_2nd,1:3) =    xx_dummy(1:nedge_2nd,1:3)
      edge_length_2nd(1:nedge_2nd) =   ele_scalar_IO(1:nedge_2nd)
      edge_vect_2nd(1:nedge_2nd,1:3) = ele_vector_IO(1:nedge_2nd,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_edge_geom_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_from_IO_sph
!
      call allocate_2nd_edge_geometry
      call allocate_2nd_edge_vects
      call allocate_2nd_edge_vect_sph
!
      r_edge_2nd(1:nedge_2nd) =      xx_dummy(1:nedge_2nd,1)
      theta_edge_2nd(1:nedge_2nd) =  xx_dummy(1:nedge_2nd,2)
      phi_edge_2nd(1:nedge_2nd) =    xx_dummy(1:nedge_2nd,3)
      edge_length_2nd(1:nedge_2nd) = ele_scalar_IO(1:nedge_2nd)
      edge_vect_2nd_sph(1:nedge_2nd,1:3)                                &
     &      = ele_vector_IO(1:nedge_2nd,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_edge_geom_from_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_from_IO_cyl
!
      call allocate_2nd_edge_geometry
      call allocate_2nd_edge_vects
      call allocate_2nd_edge_vect_cyl
!
      s_edge_2nd(1:nedge_2nd) =   xx_dummy(1:nedge_2nd,1)
      phi_edge_2nd(1:nedge_2nd) = xx_dummy(1:nedge_2nd,2)
      x_edge_2nd(1:nedge_2nd,3) = xx_dummy(1:nedge_2nd,3)
      edge_length_2nd(1:nedge_2nd) =       ele_scalar_IO(1:nedge_2nd)
      edge_vect_2nd_cyl(1:nedge_2nd,1:3)                                &
     &      = ele_vector_IO(1:nedge_2nd,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_edge_geom_from_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_2nd_edge_geometry_4_IO
