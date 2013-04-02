!
!      module set_2nd_surf_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_2nd_surf_connect_to_IO
!      subroutine copy_2nd_surf_geom_to_IO
!      subroutine copy_2nd_surf_geom_to_IO_sph
!      subroutine copy_2nd_surf_geom_to_IO_cyl
!
!      subroutine copy_2nd_surf_connect_from_IO
!      subroutine copy_2nd_surf_geom_from_IO
!      subroutine copy_2nd_surf_geom_from_IO_sph
!      subroutine copy_2nd_surf_geom_from_IO_cyl
!
      module set_2nd_surf_geometry_4_IO
!
      use m_precision
!
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_surface_geometry_data
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
      subroutine copy_2nd_surf_connect_to_IO
!
      use m_geometry_constants
!
!
      numele_dummy =        nsurf_2nd
      nnod_4_ele_dummy =    nnod_4_surf_2nd
!
      nsf_4_ele_IO =    nele_2nd
      nsurf_in_ele_IO = nsurf_4_ele
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
      call allocate_surface_connect_IO
!
      if      (nnod_4_surf_2nd .eq. 4) then
        i_ele_dummy(1:nsurf_2nd) = 221
      else if (nnod_4_surf_2nd .eq. 8) then
        i_ele_dummy(1:nsurf_2nd) = 222
      else if (nnod_4_surf_2nd .eq. 9) then
        i_ele_dummy(1:nsurf_2nd) = 223
      end if
!
      nodelm_dummy(1:nsurf_2nd) = nnod_4_surf_2nd
      globalelmid_dummy(1:nsurf_2nd) = globalsurfid_2nd(1:nsurf_2nd)
      ie_dummy(1:nsurf_2nd,1:nnod_4_surf_2nd)                           &
     &        = ie_surf_2nd(1:nsurf_2nd,1:nnod_4_surf_2nd)
!
      isf_4_ele_IO(1:nele_2nd,1:nsurf_4_ele)                            &
     &        = isf_4_ele_2nd(1:nele_2nd,1:nsurf_4_ele)
!
      end subroutine copy_2nd_surf_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_to_IO
!
!
      numnod_dummy =        nsurf_2nd
      internal_node_dummy = internal_surf_2nd
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!
      globalnodid_dummy(1:nsurf_2nd) = globalsurfid_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,1:3) =      x_surf_2nd(1:nsurf_2nd,1:3)
!
      ele_scalar_IO(1:nsurf_2nd) =     area_surf_2nd(1:nsurf_2nd)
      ele_vector_IO(1:nsurf_2nd,1:3) = vnorm_surf_2nd(1:nsurf_2nd,1:3)
!
      end subroutine copy_2nd_surf_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_to_IO_sph
!
      numnod_dummy =        nsurf_2nd
      internal_node_dummy = internal_surf_2nd
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:nsurf_2nd) = globalsurfid_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,1) = r_surf_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,2) = theta_surf_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,3) = phi_surf_2nd(1:nsurf_2nd)
!
      ele_scalar_IO(1:nsurf_2nd) =     area_surf_2nd(1:nsurf_2nd)
      ele_vector_IO(1:nsurf_2nd,1:3)                                    &
     &                           = vnorm_surf_2nd_sph(1:nsurf_2nd,1:3)
!
      end subroutine copy_2nd_surf_geom_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_to_IO_cyl
!
      numnod_dummy =        nsurf_2nd
      internal_node_dummy = internal_surf_2nd
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:nsurf_2nd) = globalsurfid_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,1) = s_surf_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,2) = phi_surf_2nd(1:nsurf_2nd)
      xx_dummy(1:nsurf_2nd,3) = x_surf_2nd(1:nsurf_2nd,3)
      ele_scalar_IO(1:nsurf_2nd) = area_surf_2nd(1:nsurf_2nd)
      ele_vector_IO(1:nsurf_2nd,1:3)                                    &
     &                           = vnorm_surf_2nd_cyl(1:nsurf_2nd,1:3)
!
      end subroutine copy_2nd_surf_geom_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_connect_from_IO
!
      use m_geometry_constants
!
!
      nsurf_2nd = numele_dummy
!
      call allocate_2nd_surface_connect
!
      globalsurfid_2nd(1:nsurf_2nd) = nodelm_dummy(1:nsurf_2nd)
      ie_surf_2nd(1:nsurf_2nd,1:nnod_4_surf_2nd)                        &
     &        = ie_dummy(1:nsurf_2nd,1:nnod_4_surf_2nd)
!
      isf_4_ele_2nd(1:nele_2nd,1:nsurf_4_ele)                           &
     &        = isf_4_ele_IO(1:nele_2nd,1:nsurf_4_ele)
!
      call deallocate_surface_connect_IO
      call deallocate_ele_info_dummy
!
      end subroutine copy_2nd_surf_connect_from_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_from_IO
!
      call allocate_2nd_surf_geom
      call allocate_2nd_norm_vects
!
      x_surf_2nd(1:nsurf_2nd,1:3) = xx_dummy(1:nsurf_2nd,1:3)
!
      area_surf_2nd(1:nsurf_2nd) =      ele_scalar_IO(1:nsurf_2nd)
      vnorm_surf_2nd(1:nsurf_2nd,1:3) = ele_vector_IO(1:nsurf_2nd,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_surf_geom_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_from_IO_sph
!
      call allocate_2nd_surf_geom
      call allocate_2nd_norm_vects
      call allocate_2nd_norm_vect_sph
!
      r_surf_2nd(1:nsurf_2nd) =     xx_dummy(1:nsurf_2nd,1)
      theta_surf_2nd(1:nsurf_2nd) = xx_dummy(1:nsurf_2nd,2)
      phi_surf_2nd(1:nsurf_2nd) =   xx_dummy(1:nsurf_2nd,3)
!
      area_surf_2nd(1:nsurf_2nd) =          ele_scalar_IO(1:nsurf_2nd)
      vnorm_surf_2nd_sph(1:nsurf_2nd,1:3)                               &
     &                           = ele_vector_IO(1:nsurf_2nd,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_surf_geom_from_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_from_IO_cyl
!
      call allocate_2nd_surf_geom
      call allocate_2nd_norm_vects
      call allocate_2nd_norm_vect_cyl
!
      s_surf_2nd(1:nsurf_2nd) =   xx_dummy(1:nsurf_2nd,1)
      phi_surf_2nd(1:nsurf_2nd) = xx_dummy(1:nsurf_2nd,2)
      x_surf_2nd(1:nsurf_2nd,3) = xx_dummy(1:nsurf_2nd,3)
!
      area_surf_2nd(1:nsurf_2nd) = ele_scalar_IO(1:nsurf_2nd)
      vnorm_surf_2nd_cyl(1:nsurf_2nd,1:3)                               &
     &                           = ele_vector_IO(1:nsurf_2nd,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_surf_geom_from_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_2nd_surf_geometry_4_IO
