!>@file   m_vertical_filter_utils.f90
!!@brief  module m_vertical_filter_utils
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for vertical filter construction
!!
!!@verbatim
!!@endverbatim
!
      module m_vertical_filter_utils
!
      use m_precision
      use t_mesh_data
      use t_surface_data
      use t_edge_data
      use t_jacobians
!
      implicit none
!
!>  structure for node data (position)
      type(mesh_geometry), save :: z_filter_mesh
!
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf_z_filter
!
!>     Structure for edge data
      type(edge_data), save :: edge_z_filter
!
!
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacs_z
!
      end module m_vertical_filter_utils
