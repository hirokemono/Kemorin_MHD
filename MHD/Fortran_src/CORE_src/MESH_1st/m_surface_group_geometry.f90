!
!      module m_surface_group_geometry
!
!> @brief Geometry data for surface group
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_normal_4_sf_grp_sph
!      subroutine allocate_normal_4_sf_grp_cyl
!       subroutine deallocate_vector_4_surface
!      subroutine deallocate_normal_4_sf_grp_sph
!      subroutine deallocate_normal_4_sf_grp_cyl
!
      module m_surface_group_geometry
!
      use m_precision
!
      use t_surface_group_geometry
!
      implicit none
!
!
!>   Structure of geometry data for surface group
      type(surface_group_geometry), save :: sf_grp_v1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_normal_4_sf_grp_sph
!
      use m_surface_group
!
      call alloc_normal_sf_grp_type_sph(num_surf_bc, sf_grp_v1)
!
      end subroutine allocate_normal_4_sf_grp_sph
!
!-----------------------------------------------------------------------
!
      subroutine allocate_normal_4_sf_grp_cyl
!
      use m_surface_group
!
      call alloc_normal_sf_grp_type_cyl(num_surf_bc, sf_grp_v1)
!
      end subroutine allocate_normal_4_sf_grp_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_geomtetry
!
      call dealloc_surf_grp_type_geom(sf_grp_v1)
!
      end subroutine deallocate_surf_grp_geomtetry
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_vector_4_surface
!
!
      call dealloc_vectors_surf_grp_type(sf_grp_v1)
!
      end subroutine deallocate_vector_4_surface
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_normal_4_sf_grp_sph
!
      call dealloc_normal_sf_grp_type_sph(sf_grp_v1)
!
      end subroutine deallocate_normal_4_sf_grp_sph
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_normal_4_sf_grp_cyl
!
      call dealloc_normal_sf_grp_type_cyl(sf_grp_v1)
!
      end subroutine deallocate_normal_4_sf_grp_cyl
!
! -----------------------------------------------------------------------!
      end module m_surface_group_geometry
