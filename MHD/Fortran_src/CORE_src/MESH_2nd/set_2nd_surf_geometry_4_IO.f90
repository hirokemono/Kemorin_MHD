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
!
      use set_surface_geom_type_IO
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
      call copy_surf_conn_type_to_IO(surf_2nd, ele_2nd%numele)
!
      end subroutine copy_2nd_surf_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_to_IO
!
      call copy_surf_geom_type_to_IO(surf_2nd)
!
      end subroutine copy_2nd_surf_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_to_IO_sph
!
      call copy_surf_geom_type_to_IO_sph(surf_2nd)
!
      end subroutine copy_2nd_surf_geom_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_to_IO_cyl
!
      call copy_surf_geom_type_to_IO_cyl(surf_2nd)
!
      end subroutine copy_2nd_surf_geom_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_connect_from_IO
!
      call copy_surf_conn_type_from_IO(surf_2nd, ele_2nd%numele)
!
      end subroutine copy_2nd_surf_connect_from_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_from_IO
!
      call copy_surf_geom_type_from_IO(surf_2nd)
!
      end subroutine copy_2nd_surf_geom_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_from_IO_sph
!
      call copy_surf_geom_type_from_IO_sph(surf_2nd)
!
      end subroutine copy_2nd_surf_geom_from_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_surf_geom_from_IO_cyl
!
      call copy_surf_geom_type_from_IO_cyl(surf_2nd)
!
      end subroutine copy_2nd_surf_geom_from_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_2nd_surf_geometry_4_IO
