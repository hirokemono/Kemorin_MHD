!
!  t_hanging_nodes.f90
!  Fortran_codes
!
!  Created by Hiroaki Matsui on 12/04/12.
!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
!
!      module t_hanging_nodes
!
!      subroutine alloc_hanging_nodes(hang)
!      subroutine alloc_hanging_surf(hang)
!      subroutine alloc_hanging_edge(hang)
!
!      subroutine alloc_hanging_nodes(hang)
!      subroutine alloc_hanging_surf(hang)
!      subroutine alloc_hanging_edge(hang)
!        type(hanging_data), intent(inout) :: hang
!
      module t_hanging_nodes
!
      use m_precision
!
      implicit  none
!
!
!>  structure for node data (position)
      type hanging_data
        integer( kind=kint )  ::  nnod_sf
!>       number of hangning node on surface
        integer( kind=kint )  ::  nnod_ed
!>       number of hangning node on edge
!
        integer(kind=kint), allocatable  ::  inod_sf(:,:)
!>       local hanging node ID for surface (inum,j)
!>           (j=1: hanging node, j=2-5:pearents nodes)
        integer(kind=kint), allocatable  ::  inod_ed(:,:)
!>       local hanging node ID for surface (inum,j)
!>           (j=1: hanging node, j=2-3:pearents nodes)
!
        integer( kind=kint )  ::  nsurf_4
!>       number of hangning surface into four surface
        integer( kind=kint )  ::  nsurf_2
!>       number of hangning surface into two surface
!
        integer(kind=kint), allocatable  ::  isurf_4(:,:)
!>       local hanging node ID for surface (inum,j)
!>           (j=1: hanging node, j=2-5:pearents nodes)
        integer(kind=kint), allocatable  ::  isurf_2(:,:)
!>       local hanging node ID for surface (inum,j)
!>           (j=1: hanging node, j=2-3:pearents nodes)
!
!
        integer( kind=kint )  ::  nedge_2
!>       number of hangning edge
        integer(kind=kint), allocatable  ::  iedge_2(:,:)
!>       local hanging node ID for surface (inum,j)
!>           (j=1: coarser edge, j=2-3:finer edge)
      end type hanging_data
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_hanging_node(hang)
!
      type(hanging_data), intent(inout) :: hang
!
!
      allocate(hang%inod_sf(hang%nnod_sf,5))
      allocate(hang%inod_ed(hang%nnod_ed,3))
!
      if(hang%nnod_sf .gt. 0) hang%inod_sf = 0
      if(hang%nnod_ed .gt. 0) hang%inod_ed = 0
!
      end subroutine alloc_hanging_node
!
!   --------------------------------------------------------------------
!
      subroutine alloc_hanging_surf(hang)
!
      type(hanging_data), intent(inout) :: hang
!
!
      allocate(hang%isurf_4(hang%nsurf_4,5))
      allocate(hang%isurf_2(hang%nsurf_2,3))
!
      if(hang%nsurf_4 .gt. 0) hang%isurf_4 = 0
      if(hang%nsurf_2 .gt. 0) hang%isurf_2 = 0
!
      end subroutine alloc_hanging_surf
!
!   --------------------------------------------------------------------
!
      subroutine alloc_hanging_edge(hang)
!
      type(hanging_data), intent(inout) :: hang
!
!
      allocate(hang%iedge_2(hang%nedge_2,3))
      if(hang%nedge_2 .gt. 0) hang%iedge_2 = 0
!
      end subroutine alloc_hanging_edge
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_hanging_nodes(hang)
!
      type(hanging_data), intent(inout) :: hang
!
!
      deallocate(hang%inod_sf, hang%inod_ed)
!
      end subroutine dealloc_hanging_nodes
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_hanging_surf(hang)
!
      type(hanging_data), intent(inout) :: hang
!
!
      deallocate(hang%isurf_4, hang%isurf_2)
!
      end subroutine dealloc_hanging_surf
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_hanging_edge(hang)
!
      type(hanging_data), intent(inout) :: hang
!
!
      deallocate(hang%iedge_2)
!
      end subroutine dealloc_hanging_edge
!
!   --------------------------------------------------------------------
!
      end module t_hanging_nodes
