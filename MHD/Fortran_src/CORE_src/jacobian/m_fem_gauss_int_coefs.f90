!m_fem_gauss_int_coefs
!   module   m_fem_gauss_int_coefs
!.......................................................................
!
!
!      Written by H. Matsui on Dec. 2003
!      Modified by H. Matsui on Oct., 2006
!
!      subroutine max_int_point_by_etype(nnod_4_ele)
!      subroutine maximum_integration_points(num_int)
!
      module   m_fem_gauss_int_coefs
!
      use m_precision
      use m_constants
      use t_fem_gauss_int_coefs
!
      implicit  none
!
      integer (kind=kint), save :: max_int_point = 4
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!> Set maximum number for integration points of FEM
!
      subroutine max_int_point_by_etype(nnod_4_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
!
      if(nnod_4_ele.eq.num_t_quad  .or. nnod_4_ele.eq.num_t_lag) then
        call maximum_integration_points(ithree)
      else if(nnod_4_ele .eq. ione) then
        call maximum_integration_points(ione)
      else
        call maximum_integration_points(itwo)
      end if
!
      end subroutine max_int_point_by_etype
!
! ----------------------------------------------------------------------
!
      subroutine maximum_integration_points(num_int)
!
      integer(kind = kint), intent(in) :: num_int
!
      max_int_point = num_int
!
      end subroutine maximum_integration_points
!
!-----------------------------------------------------------------------
!
      end module   m_fem_gauss_int_coefs
