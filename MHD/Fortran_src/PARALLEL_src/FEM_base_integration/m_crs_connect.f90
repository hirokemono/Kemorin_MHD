!
!      module m_crs_connect
!
!        programmed by H.Matsui on Oct., 2006
!
!      subroutine allocate_crs_stack(numnod)
!      subroutine allocate_crs_connect
!
!
      module m_crs_connect
!
      use m_precision
      use t_crs_connect
!
      implicit none
!
!>  Structures for index table for compressed raw strage matrix
      type(CRS_matrix_connect), save :: tbl1_crs
!
      end module m_crs_connect
