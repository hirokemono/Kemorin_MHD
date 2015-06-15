!
!      module const_metis_input
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine s_const_metis_input
!
      module const_metis_input
!
      use m_precision
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_metis_input
!
      use m_geometry_graph
      use m_metis_IO
      use const_geometry_graph
      use copy_4_metis_IO
!
!
      call s_const_geometry_graph
      call const_internal_geometry_graph
!
      call deallocate_geometry_graph
!
      call copy_graph_4_metis_IO
!
      call output_graph_4_metis
!
      end subroutine s_const_metis_input
!
! ----------------------------------------------------------------------
!
      end module const_metis_input
