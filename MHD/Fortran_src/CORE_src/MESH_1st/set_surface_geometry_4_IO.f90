!
!      module set_surface_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_surf_connect_to_IO
!      subroutine copy_surf_geometry_to_IO
!      subroutine copy_surf_geometry_to_IO_sph
!      subroutine copy_surf_geometry_to_IO_cyl
!
!      subroutine copy_surf_connect_from_IO
!      subroutine copy_surf_geometry_from_IO
!      subroutine copy_surf_geometry_from_IO_sph
!      subroutine copy_surf_geometry_from_IO_cyl
!
      module set_surface_geometry_4_IO
!
      use m_precision
!
      use m_geometry_data
      use m_surface_geometry_data
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
      subroutine copy_surf_connect_to_IO
!
      use m_geometry_constants
!
!
      numele_dummy =        surf1%numsurf
      nnod_4_ele_dummy =    surf1%nnod_4_surf
!
      nsf_4_ele_IO = ele1%numele
      nsurf_in_ele_IO = nsurf_4_ele
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
      call allocate_surface_connect_IO
!
      if      (surf1%nnod_4_surf .eq. 4) then
        i_ele_dummy(1:surf1%numsurf) = 221
      else if (surf1%nnod_4_surf .eq. 8) then
        i_ele_dummy(1:surf1%numsurf) = 222
      else if (surf1%nnod_4_surf .eq. 9) then
        i_ele_dummy(1:surf1%numsurf) = 223
      end if
!
      nodelm_dummy(1:surf1%numsurf) = surf1%nnod_4_surf
      globalelmid_dummy(1:surf1%numsurf)                                &
     &        = isurf_global(1:surf1%numsurf)
      ie_dummy(1:surf1%numsurf,1:surf1%nnod_4_surf)                     &
     &        = surf1%ie_surf(1:surf1%numsurf,1:surf1%nnod_4_surf)
!
      isf_4_ele_IO(1:ele1%numele,1:nsurf_4_ele)                         &
     &        = isf_4_ele(1:ele1%numele,1:nsurf_4_ele)
!
      end subroutine copy_surf_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO
!
!
      numnod_dummy =        surf1%numsurf
      internal_node_dummy = surf1%internal_surf
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!
      globalnodid_dummy(1:surf1%numsurf)                                &
     &      = isurf_global(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,1:3) = x_surf(1:surf1%numsurf,1:3)
!
      ele_scalar_IO(1:surf1%numsurf) =     area_surf(1:surf1%numsurf)
      ele_vector_IO(1:surf1%numsurf,1:3)                                &
     &      = vnorm_surf(1:surf1%numsurf,1:3)
!
      end subroutine copy_surf_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO_sph
!
      numnod_dummy =        surf1%numsurf
      internal_node_dummy = surf1%internal_surf
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:surf1%numsurf)                                &
     &     = isurf_global(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,1) = r_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,2) = theta_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,3) = phi_surf(1:surf1%numsurf)
!
      ele_scalar_IO(1:surf1%numsurf) =     area_surf(1:surf1%numsurf)
      ele_vector_IO(1:surf1%numsurf,1:3)                                &
     &      = vnorm_surf_sph(1:surf1%numsurf,1:3)
!
      end subroutine copy_surf_geometry_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO_cyl
!
      numnod_dummy =        surf1%numsurf
      internal_node_dummy = surf1%internal_surf
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
      globalnodid_dummy(1:surf1%numsurf)                                &
     &      = isurf_global(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,1) = s_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,2) = phi_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,3) = x_surf(1:surf1%numsurf,3)
      ele_scalar_IO(1:surf1%numsurf) =     area_surf(1:surf1%numsurf)
      ele_vector_IO(1:surf1%numsurf,1:3)                                &
     &      = vnorm_surf_cyl(1:surf1%numsurf,1:3)
!
      end subroutine copy_surf_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_surf_connect_from_IO
!
      use m_geometry_constants
!
!
      surf1%numsurf = numele_dummy
!
      call allocate_surface_connect
!
      isurf_global(1:surf1%numsurf)                                     &
     &        = globalelmid_dummy(1:surf1%numsurf)
      surf1%ie_surf(1:surf1%numsurf,1:surf1%nnod_4_surf)                &
     &        = ie_dummy(1:surf1%numsurf,1:surf1%nnod_4_surf)
!
      isf_4_ele(1:ele1%numele,1:nsurf_4_ele)                            &
     &        = isf_4_ele_IO(1:ele1%numele,1:nsurf_4_ele)
!
      call deallocate_surface_connect_IO
      call deallocate_ele_info_dummy
!
      end subroutine copy_surf_connect_from_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_from_IO
!
      call allocate_surface_geometry
      call allocate_normal_vectors
!
      x_surf(1:surf1%numsurf,1:3) = xx_dummy(1:surf1%numsurf,1:3)
!
      area_surf(1:surf1%numsurf) =      ele_scalar_IO(1:surf1%numsurf)
      vnorm_surf(1:surf1%numsurf,1:3)                                   &
     &         = ele_vector_IO(1:surf1%numsurf,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_surf_geometry_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_from_IO_sph
!
      call allocate_surface_geometry
      call allocate_normal_vectors
      call allocate_normal_vector_sph
!
      r_surf(1:surf1%numsurf) =     xx_dummy(1:surf1%numsurf,1)
      theta_surf(1:surf1%numsurf) = xx_dummy(1:surf1%numsurf,2)
      phi_surf(1:surf1%numsurf) =   xx_dummy(1:surf1%numsurf,3)
!
      area_surf(1:surf1%numsurf) = ele_scalar_IO(1:surf1%numsurf)
      vnorm_surf_sph(1:surf1%numsurf,1:3)                               &
     &      = ele_vector_IO(1:surf1%numsurf,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_surf_geometry_from_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_from_IO_cyl
!
      call allocate_surface_geometry
      call allocate_normal_vectors
      call allocate_normal_vector_cyl
!
      s_surf(1:surf1%numsurf) =   xx_dummy(1:surf1%numsurf,1)
      phi_surf(1:surf1%numsurf) = xx_dummy(1:surf1%numsurf,2)
      x_surf(1:surf1%numsurf,3) = xx_dummy(1:surf1%numsurf,3)
!
      area_surf(1:surf1%numsurf) = ele_scalar_IO(1:surf1%numsurf)
      vnorm_surf_cyl(1:surf1%numsurf,1:3)                               &
     &      = ele_vector_IO(1:surf1%numsurf,1:3)
!
      call deallocate_ele_scalar_IO
      call deallocate_ele_vector_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_surf_geometry_from_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_surface_geometry_4_IO
