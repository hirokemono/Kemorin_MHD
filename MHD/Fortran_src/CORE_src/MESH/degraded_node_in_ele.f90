!>@file   degraded_node_in_ele.f90
!!@brief  module degraded_node_in_ele
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!> @brief Search belonged element list for each node
!!
!!@verbatim
!!      subroutine find_degraded_node(iele, numele, nnod_4_ele, ie,     &
!!     &                              ie_degrade)
!!        integer (kind=kint), intent(in) :: iele, nnod_4_ele
!!        integer (kind=kint), intent(in) :: numele, nnod_4_ele
!!        integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
!!        integer(kind = kint), intent(inout) :: ie_degrade(nnod_4_ele)
!!@endverbatim
!
      module degraded_node_in_ele
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: find_degraded_on_ele8, find_degraded_on_surf4
      private :: find_degraded_on_tri3, find_degraded_on_edge2
      private :: find_degraded_on_ele20, find_degraded_on_surf8
      private :: find_degraded_on_tri6
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_node(iele, numele, nnod_4_ele, ie,       &
     &                              ie_degrade)
!
      use m_geometry_constants
!
      integer (kind=kint), intent(in) :: iele
      integer (kind=kint), intent(in) :: numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: ie_degrade(nnod_4_ele)
!
!
      if(nnod_4_ele .eq. num_t_linear) then
        call find_degraded_on_ele8(iele, numele, ie, ie_degrade)
      else if(nnod_4_ele .eq. num_linear_sf) then
        call find_degraded_on_surf4(iele, numele, ie, ie_degrade)
      else if(nnod_4_ele .eq. num_triangle) then
        call find_degraded_on_tri3(iele, numele, ie, ie_degrade)
      else if(nnod_4_ele .eq. num_linear_edge) then
        call find_degraded_on_edge2(iele, numele, ie, ie_degrade)
!
      else if(nnod_4_ele .eq. num_t_quad) then
        call find_degraded_on_ele20(iele, numele, ie, ie_degrade)
      else if(nnod_4_ele .eq. num_quad_sf) then
        call find_degraded_on_surf8(iele, numele, ie, ie_degrade)
      else if(nnod_4_ele .eq. 6) then
        call find_degraded_on_tri6(iele, numele, ie, ie_degrade)
!
      else if(nnod_4_ele .eq. num_t_lag) then
        call find_degraded_on_ele20(iele, numele, ie(1,1),              &
     &                              ie_degrade(1))
      else if(nnod_4_ele .eq. num_lag_sf) then
        call find_degraded_on_surf8(iele, numele, ie(1,1),              &
     &                              ie_degrade(1))
      end if
!
      end subroutine find_degraded_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_ele8(iele, numele, ie, ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele,8)
      integer(kind = kint), intent(inout) :: ie_degrade(8)
!
      ie_degrade(1:8) = 0
      if(ie(iele,1) .eq. ie(iele,2)) ie_degrade(2) = 1
      if(ie(iele,2) .eq. ie(iele,3)) ie_degrade(3) = 2
      if(ie(iele,3) .eq. ie(iele,4)) ie_degrade(4) = 3
      if(ie(iele,4) .eq. ie(iele,1)) ie_degrade(4) = 1
      if(ie(iele,5) .eq. ie(iele,6)) ie_degrade(6) = 5
      if(ie(iele,6) .eq. ie(iele,7)) ie_degrade(7) = 6
      if(ie(iele,7) .eq. ie(iele,8)) ie_degrade(8) = 7
      if(ie(iele,8) .eq. ie(iele,5)) ie_degrade(8) = 5
      if(ie(iele,1) .eq. ie(iele,5)) ie_degrade(5) = 1
      if(ie(iele,2) .eq. ie(iele,6)) ie_degrade(6) = 2
      if(ie(iele,3) .eq. ie(iele,7)) ie_degrade(7) = 3
      if(ie(iele,4) .eq. ie(iele,8)) ie_degrade(8) = 4
!
      end subroutine find_degraded_on_ele8
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_surf4(iele, numele, ie_surf,          &
     &                                  ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie_surf(numele,4)
      integer(kind = kint), intent(inout) :: ie_degrade(4)
!
      ie_degrade(1:4) = 0
      if(ie_surf(iele,1) .eq. ie_surf(iele,2)) ie_degrade(2) = 1
      if(ie_surf(iele,2) .eq. ie_surf(iele,3)) ie_degrade(3) = 2
      if(ie_surf(iele,3) .eq. ie_surf(iele,4)) ie_degrade(4) = 3
      if(ie_surf(iele,4) .eq. ie_surf(iele,1)) ie_degrade(4) = 1
!
      end subroutine find_degraded_on_surf4
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_tri3(iele, numele, ie_tri,            &
     &                                 ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie_tri(numele,3)
      integer(kind = kint), intent(inout) :: ie_degrade(3)
!
      ie_degrade(1:3) = 0
      if(ie_tri(iele,1) .eq. ie_tri(iele,2)) ie_degrade(2) = 1
      if(ie_tri(iele,2) .eq. ie_tri(iele,3)) ie_degrade(3) = 2
      if(ie_tri(iele,3) .eq. ie_tri(iele,1)) ie_degrade(3) = 1
!
      end subroutine find_degraded_on_tri3
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_edge2(iele, numele, ie_edge,          &
     &                                 ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie_edge(numele,2)
      integer(kind = kint), intent(inout) :: ie_degrade(2)
!
      ie_degrade(1:2) = 0
      if(ie_edge(iele,1) .eq. ie_edge(iele,2)) ie_degrade(2) = 1
!
      end subroutine find_degraded_on_edge2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_ele20(iele, numele, ie, ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele,20)
      integer(kind = kint), intent(inout) :: ie_degrade(20)
!
      call find_degraded_on_ele8(iele, numele, ie(1,1), ie_degrade(1))
      ie_degrade(9:20) = 0
      if(ie_degrade(2) .eq. 1) ie_degrade( 9) = 1
      if(ie_degrade(3) .eq. 2) ie_degrade(10) = 2
      if(ie_degrade(4) .eq. 3) ie_degrade(11) = 3
      if(ie_degrade(4) .eq. 1) ie_degrade(12) = 1
      if(ie_degrade(6) .eq. 1) ie_degrade(13) = 5
      if(ie_degrade(7) .eq. 2) ie_degrade(14) = 6
      if(ie_degrade(8) .eq. 3) ie_degrade(15) = 7
      if(ie_degrade(8) .eq. 1) ie_degrade(16) = 5
      if(ie_degrade(5) .eq. 1) ie_degrade(17) = 1
      if(ie_degrade(6) .eq. 2) ie_degrade(18) = 2
      if(ie_degrade(7) .eq. 3) ie_degrade(19) = 3
      if(ie_degrade(8) .eq. 4) ie_degrade(20) = 4
!
      end subroutine find_degraded_on_ele20
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_surf8(iele, numele, ie_surf,          &
     &                                  ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie_surf(numele,8)
      integer(kind = kint), intent(inout) :: ie_degrade(8)
!
!
      call find_degraded_on_tri3(iele, numele, ie_surf(1,1),            &
     &                           ie_degrade(1))
      ie_degrade(5:8) = 0
      if(ie_degrade(2) .eq. 1) ie_degrade(5) = 1
      if(ie_degrade(3) .eq. 2) ie_degrade(6) = 2
      if(ie_degrade(4) .eq. 3) ie_degrade(7) = 3
      if(ie_degrade(4) .eq. 1) ie_degrade(8) = 1
!
      end subroutine find_degraded_on_surf8
!
! -----------------------------------------------------------------------
!
      subroutine find_degraded_on_tri6(iele, numele, ie_tri,            &
     &                                 ie_degrade)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie_tri(numele,6)
      integer(kind = kint), intent(inout) :: ie_degrade(6)
!
!
      call find_degraded_on_tri3(iele, numele, ie_tri(1,1),             &
     &                           ie_degrade(1))
      ie_degrade(4:6) = 0
      if(ie_degrade(2) .eq. 1) ie_degrade(4) = 1
      if(ie_degrade(3) .eq. 2) ie_degrade(5) = 2
      if(ie_degrade(3) .eq. 1) ie_degrade(6) = 1
!
      end subroutine find_degraded_on_tri6
!
! -----------------------------------------------------------------------
!
      end module degraded_node_in_ele
