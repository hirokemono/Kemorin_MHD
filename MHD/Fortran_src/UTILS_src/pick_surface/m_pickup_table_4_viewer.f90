!
!      module m_pickup_table_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_imark_node(numnod)
!!      subroutine deallocate_imark_node
!
      module m_pickup_table_4_viewer
!
      use m_precision
      use t_surface_data
!
      implicit none
!
!
      integer(kind = kint), allocatable :: imark_node(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_imark_node(numnod)
!
      integer(kind = kint) :: numnod
!
      allocate( imark_node(numnod) )
      imark_node = 0
!
      end subroutine allocate_imark_node
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_imark_node
!
      deallocate( imark_node )
!
      end subroutine deallocate_imark_node
!
!------------------------------------------------------------------
!
      end module m_pickup_table_4_viewer
