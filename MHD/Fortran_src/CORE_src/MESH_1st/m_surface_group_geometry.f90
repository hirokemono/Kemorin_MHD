!
!      module m_surface_group_geometry
!
!> @brief Geometry data for surface group
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine deallocate_surf_grp_geometry
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
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_geometry
!
      call dealloc_surf_grp_type_geom(sf_grp_v1)
!
      end subroutine deallocate_surf_grp_geometry
!
!-----------------------------------------------------------------------
!
      end module m_surface_group_geometry
