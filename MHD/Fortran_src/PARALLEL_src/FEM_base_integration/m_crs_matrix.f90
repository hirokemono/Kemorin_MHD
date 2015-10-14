!
!     module m_crs_matrix
!
!      Written by Kemorin
!
      module m_crs_matrix
!
      use m_precision
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
!>  Structures for index table for compressed raw strage matrix
      type(CRS_matrix_connect), save :: tbl1_crs
!
!>  Structures for compressed raw strage matrix
      type(CRS_matrix), save :: mat1_crs
!
      end module m_crs_matrix
