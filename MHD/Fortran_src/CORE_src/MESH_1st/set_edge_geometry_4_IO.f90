!
!      module set_edge_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_edge_connect_to_IO
!      subroutine copy_edge_connect_from_IO
!      subroutine copy_edge_geometry_to_IO
!      subroutine copy_edge_geometry_to_IO_sph
!      subroutine copy_edge_geometry_to_IO_cyl
!      subroutine copy_edge_geometry_from_IO
!      subroutine copy_edge_geometry_from_IO_sph
!      subroutine copy_edge_geometry_from_IO_cyl
!
      module set_edge_geometry_4_IO
!
      use m_precision
!
      use m_geometry_data
      use m_edge_geometry_data
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
      subroutine copy_edge_connect_to_IO
!
      use m_geometry_constants
!
!
      numele_dummy = ele1%numele
!
      nsf_4_ele_IO =    surf1%numsurf
      nsurf_in_ele_IO = nedge_4_surf
!
      ned_4_ele_IO = ele1%numele
!
      numele_dummy =     edge1%numedge
      nnod_4_ele_dummy = nnod_4_edge
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
      call allocate_surface_connect_IO
      call allocate_edge_connect_IO
!
      if      (nnod_4_edge .eq. num_linear_edge) then
        i_ele_dummy(1:edge1%numedge) = 111
      else if (nnod_4_edge .eq. num_quad_edge) then
        i_ele_dummy(1:edge1%numedge) = 112
      end if
!
      globalelmid_dummy(1:edge1%numedge)                                &
     &        = iedge_global(1:edge1%numedge)
      nodelm_dummy(1:edge1%numedge) = nnod_4_edge
      ie_dummy(1:edge1%numedge,1:nnod_4_edge)                           &
     &        = ie_edge(1:edge1%numedge,1:nnod_4_edge)
!
      isf_4_ele_IO(1:surf1%numsurf,1:nedge_4_surf)                      &
     &        = iedge_4_sf(1:surf1%numsurf,1:nedge_4_surf)
      iedge_4_ele_IO(1:ele1%numele,1:nedge_4_ele)                       &
     &        = iedge_4_ele(1:ele1%numele,1:nedge_4_ele)
!
      end subroutine copy_edge_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO
!
!
      numnod_dummy =        edge1%numedge
      internal_node_dummy = internal_edge
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:edge1%numedge)                                &
     &      = iedge_global(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,1:3) =  x_edge(1:edge1%numedge,1:3)
      ele_scalar_IO(1:edge1%numedge) = edge_length(1:edge1%numedge)
      ele_vector_IO(1:edge1%numedge,1:3)                                &
     &      = edge_vect(1:edge1%numedge,1:3)
!
      end subroutine copy_edge_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO_sph
!
      numnod_dummy =        edge1%numedge
      internal_node_dummy = internal_edge
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:edge1%numedge)                                &
     &       = iedge_global(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,1) = r_edge(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,2) = theta_edge(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,3) = phi_edge(1:edge1%numedge)
      ele_scalar_IO(1:edge1%numedge) = edge_length(1:edge1%numedge)
      ele_vector_IO(1:edge1%numedge,1:3)                                &
     &       = edge_vect_sph(1:edge1%numedge,1:3)
!
      end subroutine copy_edge_geometry_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_to_IO_cyl
!
      numnod_dummy =        edge1%numedge
      internal_node_dummy = internal_edge
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:edge1%numedge)                                &
     &      = iedge_global(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,1) = s_edge(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,2) = phi_edge(1:edge1%numedge)
      xx_dummy(1:edge1%numedge,3) = x_edge(1:edge1%numedge,3)
      ele_scalar_IO(1:edge1%numedge) = edge_length(1:edge1%numedge)
      ele_vector_IO(1:edge1%numedge,1:3)                                &
     &      = edge_vect_cyl(1:edge1%numedge,1:3)
!
      end subroutine copy_edge_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_edge_connect_from_IO
!
      use m_geometry_constants
!
!
      edge1%numedge = numele_dummy
      numedge =       edge1%numedge
!
      call allocate_edge_connect
      call allocate_edge_4_ele
!
      iedge_global(1:edge1%numedge)                                     &
     &        = globalelmid_dummy(1:edge1%numedge)
      ie_edge(1:edge1%numedge,1:nnod_4_edge)                            &
     &        = ie_dummy(1:edge1%numedge,1:nnod_4_edge)
!
      iedge_4_sf(1:surf1%numsurf,1:nedge_4_surf)                        &
     &        = isf_4_ele_IO(1:surf1%numsurf,1:nedge_4_surf)
!
      iedge_4_ele(1:ele1%numele,1:nedge_4_ele)                          &
     &        = iedge_4_ele_IO(1:ele1%numele,1:nedge_4_ele)
!
      call deallocate_surface_connect_IO
      call deallocate_ele_info_dummy
      call deallocate_edge_connect_IO
!
      end subroutine copy_edge_connect_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_from_IO
!
      call allocate_edge_geometry
      call allocate_edge_vectors
!
      x_edge(1:edge1%numedge,1:3) =  xx_dummy(1:edge1%numedge,1:3)
      edge_length(1:edge1%numedge) = ele_scalar_IO(1:edge1%numedge)
      edge_vect(1:edge1%numedge,1:3)                                    &
     &          = ele_vector_IO(1:edge1%numedge,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_edge_geometry_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_from_IO_sph
!
      call allocate_edge_geometry
      call allocate_edge_vectors
      call allocate_edge_vector_sph
!
      r_edge(1:edge1%numedge) =     xx_dummy(1:edge1%numedge,1)
      theta_edge(1:edge1%numedge) = xx_dummy(1:edge1%numedge,2)
      phi_edge(1:edge1%numedge) =   xx_dummy(1:edge1%numedge,3)
      edge_length(1:edge1%numedge) = ele_scalar_IO(1:edge1%numedge)
      edge_vect_sph(1:edge1%numedge,1:3) =                              &
     &               ele_vector_IO(1:edge1%numedge,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_edge_geometry_from_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_edge_geometry_from_IO_cyl
!
      call allocate_edge_geometry
      call allocate_edge_vectors
      call allocate_edge_vector_cyl
!
      s_edge(1:edge1%numedge) =   xx_dummy(1:edge1%numedge,1)
      phi_edge(1:edge1%numedge) = xx_dummy(1:edge1%numedge,2)
      x_edge(1:edge1%numedge,3) = xx_dummy(1:edge1%numedge,3)
      edge_length(1:edge1%numedge) = ele_scalar_IO(1:edge1%numedge)
      edge_vect_cyl(1:edge1%numedge,1:3)                                &
     &         = ele_vector_IO(1:edge1%numedge,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_edge_geometry_from_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_edge_geometry_4_IO
