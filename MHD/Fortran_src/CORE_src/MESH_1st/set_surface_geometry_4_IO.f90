!
!      module set_surface_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_surf_geometry_to_IO
!      subroutine copy_surf_geometry_to_IO_sph
!      subroutine copy_surf_geometry_to_IO_cyl
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
     &      = surf1%isurf_global(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,1:3) = surf1%x_surf(1:surf1%numsurf,1:3)
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
     &     = surf1%isurf_global(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,1) = surf1%r_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,2) = surf1%theta_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,3) = surf1%phi_surf(1:surf1%numsurf)
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
     &      = surf1%isurf_global(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,1) = surf1%s_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,2) = surf1%phi_surf(1:surf1%numsurf)
      xx_dummy(1:surf1%numsurf,3) = surf1%x_surf(1:surf1%numsurf,3)
      ele_scalar_IO(1:surf1%numsurf) =     area_surf(1:surf1%numsurf)
      ele_vector_IO(1:surf1%numsurf,1:3)                                &
     &      = vnorm_surf_cyl(1:surf1%numsurf,1:3)
!
      end subroutine copy_surf_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_surface_geometry_4_IO
