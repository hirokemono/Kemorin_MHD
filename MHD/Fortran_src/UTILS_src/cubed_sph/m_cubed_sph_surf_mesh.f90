!m_cubed_sph_surf_mesh.f90
!      module m_cubed_sph_surf_mesh
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine deallocate_surface_geometries
!      subroutine deallocate_coarsing_stack
!      subroutine deallocate_surface_connect
!      subroutine deallocate_coarse_surf_connect
!
      module m_cubed_sph_surf_mesh
!
      use m_precision
      use t_cubed_sph_surf_mesh
!
      implicit none
!
      type(cubed_sph_surf_mesh), save :: c_sphere1
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_surface_geometries
!
      call dealloc_surface_geometries(c_sphere1)
!
      end subroutine deallocate_surface_geometries
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coarsing_stack
!
      call dealloc_coarsing_stack(c_sphere1)
!
      end subroutine deallocate_coarsing_stack
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_surface_connect
!
      call dealloc_surface_connect(c_sphere1)
!
      end subroutine deallocate_surface_connect
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coarse_surf_connect
!
      call dealloc_coarse_surf_connect(c_sphere1)
!
      end subroutine deallocate_coarse_surf_connect
!
!   --------------------------------------------------------------------
!
      end module m_cubed_sph_surf_mesh
