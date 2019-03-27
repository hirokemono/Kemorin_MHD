!cal_refined_nod_near_pole.f90
!      module cal_refined_nod_near_pole
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine s_cal_refined_nod_near_pole                          &
!!     &         (node, surf, edge, refine_surf, refine_edge)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(in) :: edge
!!        type(surface_data), intent(in) :: surf
!!        type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
      module cal_refined_nod_near_pole
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_refined_nod_near_pole                            &
     &         (node, surf, edge, refine_surf, refine_edge)
!
      use m_constants
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_refined_node_id
      use cal_sph_4_refine
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
      integer(kind = kint) :: isurf, iedge
      integer(kind = kint) :: inod, inod1, inod2, k1
      real(kind = kreal) :: diff, phi_min, phi_max
      real(kind = kreal) :: pi
!
!
      pi = four * atan (one)
!
      do iedge = 1, edge%numedge
        inod1 = edge%ie_edge(iedge,1)
        inod2 = edge%ie_edge(iedge,2)
!
        diff = abs( node%phi(inod1) - node%phi(inod2) )
!
        if( diff .ge. pi ) then
          call cal_sph_xing_med_edge_refine(node%numnod, edge%numedge,  &
     &        edge%ie_edge(1,1), node%phi, iedge,                       &
     &        refine_edge%ntot_nod_refine,                              &
     &        refine_edge%istack_nod_refine,                            &
     &        refine_edge%xi_refine, refine_edge%sph_refine)
        end if
      end do
!
      do isurf = 1, surf%numsurf
        phi_max = 0.0d0
        phi_min = two*pi
        do k1 = 1, 4
          inod = surf%ie_surf(isurf,k1)
          phi_max = max(phi_max,node%phi(inod))
          phi_min = min(phi_min,node%phi(inod))
        end do
        diff = abs(phi_max - phi_min)
!
        if( diff .ge. pi ) then
          call cal_sph_xing_med_surf_refine(node%numnod, surf%numsurf,  &
     &        surf%ie_surf(1,1), node%phi, isurf, phi_max,              &
     &        refine_surf%ntot_nod_refine,                              &
     &        refine_surf%istack_nod_refine,                            &
     &        refine_surf%xi_refine, refine_surf%sph_refine)
        end if
      end do
!
      end subroutine s_cal_refined_nod_near_pole
!
!  ---------------------------------------------------------------------
!
      end module cal_refined_nod_near_pole
