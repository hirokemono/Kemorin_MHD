!
!     module m_offset_size_cube
!
      module m_offset_size_cube
!
!     modified by Kemorin
!
      use m_precision
!
      implicit none
!
! ......................................................................
!
      integer(kind=kint)  ::  ioff, joff, koff

!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!
!      subroutine init_node_para_4_each_pe
!
      subroutine init_node_para_4_each_pe(ipe, jpe, kpe)
!
! *****  initialization to construct node information
!
      use m_size_of_cube
      implicit none
!
      integer(kind=kint)  ::  ipe, jpe, kpe
!
! ***** set nodal position off set (i,j,k starting position -1)
!
                        ioff = (ipe-1)*nxi - ndepth
!
                        joff = (jpe-1)*nyi - ndepth
!
                        koff = (kpe-1)*nzi
            if (kpe/=1) koff = koff - ndepth
!
       end subroutine init_node_para_4_each_pe
!
! ----------------------------------------------------------------------
!
      end module m_offset_size_cube
