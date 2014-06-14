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
!
      use set_edge_geom_type_IO
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
!
      call copy_edge_conn_type_to_IO(edge_2nd, nele_2nd, surf_2nd%numsurf)
!
      end subroutine copy_2nd_edge_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_to_IO
!
!
      call copy_edge_geom_type_to_IO(edge_2nd)
!
      end subroutine copy_2nd_edge_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_to_IO_sph
!
      call copy_edge_geom_type_to_IO_sph(edge_2nd)
!
      end subroutine copy_2nd_edge_geom_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_to_IO_cyl
!
      call copy_edge_geom_type_to_IO_cyl(edge_2nd)
!
      end subroutine copy_2nd_edge_geom_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_connect_from_IO
!
      call copy_edge_conn_type_from_IO(edge_2nd, nele_2nd, surf_2nd%numsurf)
!
      end subroutine copy_2nd_edge_connect_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_from_IO
!
      call copy_edge_geom_type_from_IO(edge_2nd)
!
      end subroutine copy_2nd_edge_geom_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_from_IO_sph
!
      call copy_edge_geom_type_from_IO_sph(edge_2nd)
!
      end subroutine copy_2nd_edge_geom_from_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_2nd_edge_geom_from_IO_cyl
!
      call copy_edge_geom_type_from_IO_cyl(edge_2nd)
!
      end subroutine copy_2nd_edge_geom_from_IO_cyl
!
!------------------------------------------------------------------
!
      end module set_2nd_edge_geometry_4_IO
