!set_all_refine_flags.f90
!      module set_all_refine_flags
!
!      Written by Kemorin on Oct., 2007
!
!      subroutine s_set_all_refine_flags
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
      subroutine s_set_all_refine_flags(ele, surf, edge)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use m_refined_element_data
      use set_surf_edge_refine_flags
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint) :: iele, k1, isurf, iedge
!
!
      do iele = 1, ele%numele
        call s_set_surf_edge_refine_flags(iflag_refine_ele(iele),       &
     &      iflag_refine_sf_lcl(1,iele), iflag_refine_ed_lcl(1,iele) )
      end do
!
      do iele = 1, ele%numele
        do k1 = 1, nsurf_4_ele
          isurf = abs( surf%isf_4_ele(iele,k1) )
          if (isurf .eq. surf%isf_4_ele(iele,k1) ) then
            iflag_refine_surf(isurf) = iflag_refine_sf_lcl(k1,iele)
          end if
        end do
      end do
!
      do iele = 1, ele%numele
        do k1 = 1, nedge_4_ele
          iedge = abs(edge%iedge_4_ele(iele,k1))
          if (iedge .eq. edge%iedge_4_ele(iele,k1) ) then
            iflag_refine_edge(iedge) = iflag_refine_ed_lcl(k1,iele)
          end if
        end do
      end do
!
      end subroutine s_set_all_refine_flags
!
! ----------------------------------------------------------------------
!
      end module set_all_refine_flags
