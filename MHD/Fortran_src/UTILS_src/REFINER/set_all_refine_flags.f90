!set_all_refine_flags.f90
!      module set_all_refine_flags
!
!      Written by Kemorin on Oct., 2007
!
!!      subroutine s_set_all_refine_flags(ele, surf, edge, refine_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_refine_table), intent(inout) :: refine_tbl
!
      module set_all_refine_flags
!
      use m_precision
!
      implicit    none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_all_refine_flags(ele, surf, edge, refine_tbl)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_refined_element_data
      use set_surf_edge_refine_flags
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(element_refine_table), intent(inout) :: refine_tbl
!
      integer(kind = kint) :: iele, k1, isurf, iedge
!
!
      do iele = 1, ele%numele
        call s_set_surf_edge_refine_flags                               &
     &     (refine_tbl%iflag_refine_ele(iele),                          &
     &      refine_tbl%iflag_refine_sf_lcl(1,iele),                     &
     &      refine_tbl%iflag_refine_ed_lcl(1,iele) )
      end do
!
      do iele = 1, ele%numele
        do k1 = 1, nsurf_4_ele
          isurf = abs( surf%isf_4_ele(iele,k1) )
          if (isurf .eq. surf%isf_4_ele(iele,k1) ) then
            refine_tbl%iflag_refine_surf(isurf)                         &
     &            = refine_tbl%iflag_refine_sf_lcl(k1,iele)
          end if
        end do
      end do
!
      do iele = 1, ele%numele
        do k1 = 1, nedge_4_ele
          iedge = abs(edge%iedge_4_ele(iele,k1))
          if (iedge .eq. edge%iedge_4_ele(iele,k1) ) then
            refine_tbl%iflag_refine_edge(iedge)                         &
     &            = refine_tbl%iflag_refine_ed_lcl(k1,iele)
          end if
        end do
      end do
!
      end subroutine s_set_all_refine_flags
!
! ----------------------------------------------------------------------
!
      end module set_all_refine_flags
