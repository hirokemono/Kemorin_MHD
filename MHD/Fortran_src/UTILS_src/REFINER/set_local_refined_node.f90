!set_local_refined_node.f90
!      module set_local_refined_node
!
!      Writen by H. Matsui on Oct., 2007
!
!!      subroutine refined_node_on_ele_2_local                          &
!!     &         (iele, ele, iflag_refine_ele,                          &
!!     &          ntot_nod_refine_ele, num_nod_refine_ele,              &
!!     &          istack_nod_refine_ele, inod_refine_ele,               &
!!     &          inod_refine_nod_local, inod_refine_ele_local)
!!      subroutine refined_node_on_surf_2_local                         &
!!     &         (iele, surf, iflag_refine_surf,                        &
!!     &          ntot_nod_refine_surf, num_nod_refine_surf,            &
!!     &          istack_nod_refine_surf, inod_refine_surf,             &
!!     &          inod_refine_surf_local)
!!      subroutine refined_node_on_edge_2_local(iele, edge,             &
!!     &          ntot_nod_refine_edge, num_nod_refine_edge,            &
!!     &          istack_nod_refine_edge, inod_refine_edge,             &
!!     &          inod_refine_edge_local)
!
      module set_local_refined_node
!
      use m_precision
      use m_refine_flag_parameters
!
      implicit none
!
      integer(kind = kint), private :: inod_sf_rev(4)
!
      private :: rotate_refined_surface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine refined_node_on_ele_2_local                            &
     &         (iele, ele, iflag_refine_ele,                            &
     &          ntot_nod_refine_ele, num_nod_refine_ele,                &
     &          istack_nod_refine_ele, inod_refine_ele,                 &
     &          inod_refine_nod_local, inod_refine_ele_local)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: iele
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in)                                  &
     &           :: iflag_refine_ele(ele%numele)
      integer(kind = kint), intent(in) :: ntot_nod_refine_ele
      integer(kind = kint), intent(in)                                  &
     &           :: num_nod_refine_ele(ele%numele)
      integer(kind = kint), intent(in)                                  &
     &           :: istack_nod_refine_ele(0:ele%numele)
      integer(kind = kint), intent(in)                                  &
     &           :: inod_refine_ele(ntot_nod_refine_ele)
!
      integer(kind = kint), intent(inout) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(inout) :: inod_refine_ele_local(8)
!
      integer(kind = kint) :: k1, icou
!
!
      inod_refine_nod_local = 0
      inod_refine_ele_local = 0
!
      do k1 = 1, ele%nnod_4_ele
        inod_refine_nod_local(k1) = ele%ie(iele,k1)
      end do
!
      icou = istack_nod_refine_ele(iele-1)
      if ( num_nod_refine_ele(iele) .eq. 1) then
        inod_refine_ele_local(1) = inod_refine_ele(icou+1)
      else if ( num_nod_refine_ele(iele) .eq. 2) then
        inod_refine_ele_local(1:8) = 0
        if      (iflag_refine_ele(iele) .eq. iflag_stri_e1 ) then
          inod_refine_ele_local(7) = inod_refine_ele(icou+1)
          inod_refine_ele_local(8) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e2 ) then
          inod_refine_ele_local(8) = inod_refine_ele(icou+1)
          inod_refine_ele_local(5) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e3 ) then
          inod_refine_ele_local(5) = inod_refine_ele(icou+1)
          inod_refine_ele_local(6) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e4 ) then
          inod_refine_ele_local(6) = inod_refine_ele(icou+1)
          inod_refine_ele_local(7) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e5 ) then
          inod_refine_ele_local(3) = inod_refine_ele(icou+1)
          inod_refine_ele_local(4) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e6 ) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(1) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e7 ) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(2) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e8 ) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(3) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e9 ) then
          inod_refine_ele_local(3) = inod_refine_ele(icou+1)
          inod_refine_ele_local(7) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e10) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(8) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e11) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(5) = inod_refine_ele(icou+2)
        else if (iflag_refine_ele(iele) .eq. iflag_stri_e12) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(6) = inod_refine_ele(icou+2)
        end if
!
!
      else if ( num_nod_refine_ele(iele) .eq. 4) then
        inod_refine_ele_local(1:8) = 0
        if      (iflag_refine_ele(iele) .eq. iflag_tri_e1 ) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(2) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(8) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e2 ) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(3) = inod_refine_ele(icou+2)
          inod_refine_ele_local(8) = inod_refine_ele(icou+3)
          inod_refine_ele_local(5) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e3 ) then
          inod_refine_ele_local(3) = inod_refine_ele(icou+1)
          inod_refine_ele_local(4) = inod_refine_ele(icou+2)
          inod_refine_ele_local(5) = inod_refine_ele(icou+3)
          inod_refine_ele_local(6) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e4 ) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(1) = inod_refine_ele(icou+2)
          inod_refine_ele_local(6) = inod_refine_ele(icou+3)
          inod_refine_ele_local(7) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e5 ) then
          inod_refine_ele_local(3) = inod_refine_ele(icou+1)
          inod_refine_ele_local(4) = inod_refine_ele(icou+2)
          inod_refine_ele_local(5) = inod_refine_ele(icou+3)
          inod_refine_ele_local(6) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e6 ) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(1) = inod_refine_ele(icou+2)
          inod_refine_ele_local(6) = inod_refine_ele(icou+3)
          inod_refine_ele_local(7) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e7 ) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(2) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(8) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e8 ) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(3) = inod_refine_ele(icou+2)
          inod_refine_ele_local(8) = inod_refine_ele(icou+3)
          inod_refine_ele_local(5) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e9 ) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(3) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(5) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e10) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(4) = inod_refine_ele(icou+2)
          inod_refine_ele_local(8) = inod_refine_ele(icou+3)
          inod_refine_ele_local(6) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e11) then
          inod_refine_ele_local(3) = inod_refine_ele(icou+1)
          inod_refine_ele_local(1) = inod_refine_ele(icou+2)
          inod_refine_ele_local(5) = inod_refine_ele(icou+3)
          inod_refine_ele_local(7) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_e12) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(2) = inod_refine_ele(icou+2)
          inod_refine_ele_local(6) = inod_refine_ele(icou+3)
          inod_refine_ele_local(8) = inod_refine_ele(icou+4)
!
        else if (iflag_refine_ele(iele) .eq. iflag_tri_xs1) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(3) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(6) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_xs2) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(4) = inod_refine_ele(icou+2)
          inod_refine_ele_local(8) = inod_refine_ele(icou+3)
          inod_refine_ele_local(5) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_ys3) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(8) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(3) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_ys4) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(5) = inod_refine_ele(icou+2)
          inod_refine_ele_local(6) = inod_refine_ele(icou+3)
          inod_refine_ele_local(2) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_zs5) then
          inod_refine_ele_local(5) = inod_refine_ele(icou+1)
          inod_refine_ele_local(6) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(8) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_tri_zs6) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(2) = inod_refine_ele(icou+2)
          inod_refine_ele_local(3) = inod_refine_ele(icou+3)
          inod_refine_ele_local(4) = inod_refine_ele(icou+4)
!
        else if (iflag_refine_ele(iele) .eq. iflag_five_s1) then
          inod_refine_ele_local(2) = inod_refine_ele(icou+1)
          inod_refine_ele_local(3) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(6) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_five_s2) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(4) = inod_refine_ele(icou+2)
          inod_refine_ele_local(8) = inod_refine_ele(icou+3)
          inod_refine_ele_local(5) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_five_s3) then
          inod_refine_ele_local(4) = inod_refine_ele(icou+1)
          inod_refine_ele_local(8) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(3) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_five_s4) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(5) = inod_refine_ele(icou+2)
          inod_refine_ele_local(6) = inod_refine_ele(icou+3)
          inod_refine_ele_local(2) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_five_s5) then
          inod_refine_ele_local(5) = inod_refine_ele(icou+1)
          inod_refine_ele_local(6) = inod_refine_ele(icou+2)
          inod_refine_ele_local(7) = inod_refine_ele(icou+3)
          inod_refine_ele_local(8) = inod_refine_ele(icou+4)
        else if (iflag_refine_ele(iele) .eq. iflag_five_s6) then
          inod_refine_ele_local(1) = inod_refine_ele(icou+1)
          inod_refine_ele_local(2) = inod_refine_ele(icou+2)
          inod_refine_ele_local(3) = inod_refine_ele(icou+3)
          inod_refine_ele_local(4) = inod_refine_ele(icou+4)
        end if
!
      else if ( num_nod_refine_ele(iele) .eq. 8) then
        inod_refine_ele_local(1) = inod_refine_ele(icou+1)
        inod_refine_ele_local(2) = inod_refine_ele(icou+2)
        inod_refine_ele_local(3) = inod_refine_ele(icou+3)
        inod_refine_ele_local(4) = inod_refine_ele(icou+4)
        inod_refine_ele_local(5) = inod_refine_ele(icou+5)
        inod_refine_ele_local(6) = inod_refine_ele(icou+6)
        inod_refine_ele_local(7) = inod_refine_ele(icou+7)
        inod_refine_ele_local(8) = inod_refine_ele(icou+8)
      end if
!
      end subroutine refined_node_on_ele_2_local
!
!  ---------------------------------------------------------------------
!
      subroutine refined_node_on_surf_2_local                           &
     &         (iele, surf, iflag_refine_surf,                          &
     &          ntot_nod_refine_surf, num_nod_refine_surf,              &
     &          istack_nod_refine_surf, inod_refine_surf,               &
     &          inod_refine_surf_local)
!
      use m_geometry_constants
      use t_surface_data
!
      integer(kind = kint), intent(in) :: iele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &           :: iflag_refine_surf(surf%numsurf)
      integer(kind = kint), intent(in) :: ntot_nod_refine_surf
      integer(kind = kint), intent(in)                                  &
     &           :: num_nod_refine_surf(surf%numsurf)
      integer(kind = kint), intent(in)                                  &
     &           :: istack_nod_refine_surf(0:surf%numsurf)
      integer(kind = kint), intent(in)                                  &
     &           :: inod_refine_surf(ntot_nod_refine_surf)
!
      integer(kind = kint), intent(inout)                               &
     &           :: inod_refine_surf_local(6,4)
!
      integer(kind = kint) :: isurf, k1, icou
!
!
      inod_refine_surf_local = 0
!
      do k1 = 1, nsurf_4_ele
        isurf = abs( surf%isf_4_ele(iele,k1) )
        icou = istack_nod_refine_surf(isurf-1)
        if ( num_nod_refine_surf(isurf) .eq. 1) then
          inod_refine_surf_local(k1,1) = inod_refine_surf(icou+1)
!
        else if (num_nod_refine_surf(isurf) .eq. 2) then
          inod_refine_surf_local(k1,1:4) = 0
!
          if (isurf .eq. surf%isf_4_ele(iele,k1) ) then
!
            if     (iflag_refine_surf(isurf).eq.iflag_tri_xe1_sf) then
              inod_refine_surf_local(k1,4) = inod_refine_surf(icou+1)
              inod_refine_surf_local(k1,3) = inod_refine_surf(icou+2)
            else if(iflag_refine_surf(isurf).eq.iflag_tri_xe3_sf) then
              inod_refine_surf_local(k1,1) = inod_refine_surf(icou+1)
              inod_refine_surf_local(k1,2) = inod_refine_surf(icou+2)
            else if(iflag_refine_surf(isurf).eq.iflag_tri_ye2_sf) then
              inod_refine_surf_local(k1,1) = inod_refine_surf(icou+1)
              inod_refine_surf_local(k1,4) = inod_refine_surf(icou+2)
            else if(iflag_refine_surf(isurf).eq.iflag_tri_ye4_sf) then
              inod_refine_surf_local(k1,2) = inod_refine_surf(icou+1)
              inod_refine_surf_local(k1,3) = inod_refine_surf(icou+2)
            end if
!
          else
!
            inod_sf_rev(1:4) = 0
            if     (iflag_refine_surf(isurf).eq.iflag_tri_xe1_sf) then
              inod_sf_rev(4) = inod_refine_surf(icou+1)
              inod_sf_rev(3) = inod_refine_surf(icou+2)
            else if(iflag_refine_surf(isurf).eq.iflag_tri_xe3_sf) then
              inod_sf_rev(1) = inod_refine_surf(icou+1)
              inod_sf_rev(2) = inod_refine_surf(icou+2)
            else if(iflag_refine_surf(isurf).eq.iflag_tri_ye2_sf) then
              inod_sf_rev(1) = inod_refine_surf(icou+1)
              inod_sf_rev(4) = inod_refine_surf(icou+2)
            else if(iflag_refine_surf(isurf).eq.iflag_tri_ye4_sf) then
              inod_sf_rev(2) = inod_refine_surf(icou+1)
              inod_sf_rev(3) = inod_refine_surf(icou+2)
            end if
!
            call rotate_refined_surface(surf%isf_rot_ele(iele,k1),      &
     &          inod_sf_rev, k1, inod_refine_surf_local)
!
          end if
!
        else if (num_nod_refine_surf(isurf) .eq. 4) then
          if (isurf .eq. surf%isf_4_ele(iele,k1) ) then
            inod_refine_surf_local(k1,1) = inod_refine_surf(icou+1)
            inod_refine_surf_local(k1,2) = inod_refine_surf(icou+2)
            inod_refine_surf_local(k1,3) = inod_refine_surf(icou+3)
            inod_refine_surf_local(k1,4) = inod_refine_surf(icou+4)
          else
!
            call rotate_refined_surface(surf%isf_rot_ele(iele,k1),      &
     &          inod_refine_surf(icou+1), k1, inod_refine_surf_local)
!
          end if
        end if
      end do
!
      end subroutine refined_node_on_surf_2_local
!
!  ---------------------------------------------------------------------
!
      subroutine refined_node_on_edge_2_local(iele, edge,               &
     &          ntot_nod_refine_edge, num_nod_refine_edge,              &
     &          istack_nod_refine_edge, inod_refine_edge,               &
     &          inod_refine_edge_local)
!
      use m_geometry_constants
      use t_edge_data
!
      integer(kind = kint), intent(in) :: iele
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: ntot_nod_refine_edge
      integer(kind = kint), intent(in)                                  &
     &           :: num_nod_refine_edge(edge%numedge)
      integer(kind = kint), intent(in)                                  &
     &           :: istack_nod_refine_edge(0:edge%numedge)
      integer(kind = kint), intent(in)                                  &
     &           :: inod_refine_edge(ntot_nod_refine_edge)
!
      integer(kind = kint), intent(inout)                               &
     &           :: inod_refine_edge_local(12,2)
!
      integer(kind = kint) :: iedge, k1, icou
!
!
      inod_refine_edge_local = 0
!
      do k1 = 1, nedge_4_ele
        iedge = abs( edge%iedge_4_ele(iele,k1) )
        icou = istack_nod_refine_edge(iedge-1)
        if ( num_nod_refine_edge(iedge) .eq. 1) then
          inod_refine_edge_local(k1,1) = inod_refine_edge(icou+1)
        else if (num_nod_refine_edge(iedge) .eq. 2) then
          if (iedge .eq. edge%iedge_4_ele(iele,k1)) then
            inod_refine_edge_local(k1,1) = inod_refine_edge(icou+1)
            inod_refine_edge_local(k1,2) = inod_refine_edge(icou+2)
          else
            inod_refine_edge_local(k1,1) = inod_refine_edge(icou+2)
            inod_refine_edge_local(k1,2) = inod_refine_edge(icou+1)
          end if
        end if
      end do
!
      end subroutine refined_node_on_edge_2_local
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rotate_refined_surface(irot_flag, inod_sf_rev, k1,     &
     &          inod_surf_local)
!
      integer(kind = kint), intent(in) :: k1
      integer(kind = kint), intent(in) :: irot_flag
      integer(kind = kint), intent(in) :: inod_sf_rev(4)
!
      integer(kind = kint), intent(inout) :: inod_surf_local(6,4)
!
!
      if      (irot_flag .eq. 1) then
        inod_surf_local(k1,1) = inod_sf_rev(1)
        inod_surf_local(k1,2) = inod_sf_rev(4)
        inod_surf_local(k1,3) = inod_sf_rev(3)
        inod_surf_local(k1,4) = inod_sf_rev(2)
      else if (irot_flag .eq. 2) then
        inod_surf_local(k1,1) = inod_sf_rev(2)
        inod_surf_local(k1,2) = inod_sf_rev(1)
        inod_surf_local(k1,3) = inod_sf_rev(4)
        inod_surf_local(k1,4) = inod_sf_rev(3)
      else if (irot_flag .eq. 3) then
        inod_surf_local(k1,1) = inod_sf_rev(3)
        inod_surf_local(k1,2) = inod_sf_rev(2)
        inod_surf_local(k1,3) = inod_sf_rev(1)
        inod_surf_local(k1,4) = inod_sf_rev(4)
      else if (irot_flag .eq. 4) then
        inod_surf_local(k1,1) = inod_sf_rev(4)
        inod_surf_local(k1,2) = inod_sf_rev(3)
        inod_surf_local(k1,3) = inod_sf_rev(2)
        inod_surf_local(k1,4) = inod_sf_rev(1)
      end if
!
      end subroutine rotate_refined_surface
!
!  ---------------------------------------------------------------------
!
      end module set_local_refined_node
