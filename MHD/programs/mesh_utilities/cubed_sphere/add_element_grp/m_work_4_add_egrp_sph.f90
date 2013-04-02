!
!      module m_work_4_add_egrp_sph
!
      module  m_work_4_add_egrp_sph
!
!     Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: iele_4_sort(:)
!
      real(kind = kreal), allocatable :: wk1_4_sort(:)
      real(kind = kreal), allocatable :: wk2_4_sort(:)
!
!      subroutine allocate_work_4_add_egrp_sph
!      subroutine deallocate_work_4_add_egrp_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_work_4_add_egrp_sph
!
      use m_geometry_parameter
!
      allocate( iele_4_sort(numele) )
      allocate( wk1_4_sort(numele) )
      allocate( wk2_4_sort(numele) )
!
      iele_4_sort = 0
      wk1_4_sort = 0.0d0
      wk2_4_sort = 0.0d0
!
      end subroutine allocate_work_4_add_egrp_sph
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_work_4_add_egrp_sph
!
      deallocate( iele_4_sort )
      deallocate( wk1_4_sort )
      deallocate( wk2_4_sort )
!
      end subroutine deallocate_work_4_add_egrp_sph
!
!   --------------------------------------------------------------------
!
      end module m_work_4_add_egrp_sph
