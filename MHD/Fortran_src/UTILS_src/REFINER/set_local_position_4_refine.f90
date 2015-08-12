!set_local_position_4_refine.f90
!      module set_local_position_4_refine
!
      module set_local_position_4_refine
!
!      Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      use m_refine_flag_parameters
      use m_local_refiened_position
      use m_refined_node_id
      use m_refined_element_data
!
      implicit none
!
      private :: set_local_posi_refine_edge
      private :: istack_nod_refine_surf
      private :: set_local_posi_refine_ele
!
!      subroutine s_set_local_position_4_refine
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_local_position_4_refine
!
      use m_geometry_data
!
      integer(kind = kint) :: iele, isurf, iedge, ist
!
!
      do iedge = 1, numedge
        ist = istack_nod_refine_edge(iedge-1)
        call set_local_posi_refine_edge(iedge, ist)
      end do
!
      do isurf = 1, numsurf
        ist = istack_nod_refine_surf(isurf-1)
        call set_local_posi_refine_surf(isurf, ist)
      end do
!
      do iele = 1, ele1%numele
        ist = istack_nod_refine_ele(iele-1)
        call set_local_posi_refine_ele(iele, ist)
      end do
!
      end subroutine s_set_local_position_4_refine
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_posi_refine_edge(iedge, ist)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iedge, ist
!
!
      if      (iflag_refine_edge(iedge) .eq. iflag_2_to_3_ed            &
     &    .or. iflag_refine_edge(iedge) .eq. iflag_dbl_ed    ) then
        xi_refine_edge(ist+1,1) = xi_dbl_ed(1)
      else if (iflag_refine_edge(iedge) .eq. iflag_tri_n1_ed) then
        xi_refine_edge(ist+1,1) = xi_tri_n1_ed(1)
      else if (iflag_refine_edge(iedge) .eq. iflag_tri_n2_ed) then
        xi_refine_edge(ist+1,1) = xi_tri_n2_ed(1)
      else if (iflag_refine_edge(iedge) .eq. iflag_tri_full_ed) then
        xi_refine_edge(ist+1,1) = xi_tri_full_ed(1)
        xi_refine_edge(ist+2,1) = xi_tri_full_ed(2)
      else if (iflag_refine_edge(iedge) .eq. iflag_tri_full_ed_eq) then
        xi_refine_edge(ist+1,1) = xi_tri_full_ed_eq(1)
        xi_refine_edge(ist+2,1) = xi_tri_full_ed_eq(2)
      end if
!
      end subroutine set_local_posi_refine_edge
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_posi_refine_surf(isurf, ist)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: isurf, ist
      integer(kind= kint) :: i
!
!
      if      (iflag_refine_surf(isurf) .eq. iflag_4_to_9_sf            &
     &    .or. iflag_refine_surf(isurf) .eq. iflag_dbl_sf    ) then
        xi_refine_surf(ist+1,1) = xei_dbl_sf(1)
        xi_refine_surf(ist+1,2) = xei_dbl_sf(2)
!
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_xe1_sf ) then
        xi_refine_surf(ist+1,1) = xei_tri_xe1_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_xe1_sf(2)
        xi_refine_surf(ist+2,1) = xei_tri_xe1_sf(3)
        xi_refine_surf(ist+2,2) = xei_tri_xe1_sf(4)
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_xe3_sf ) then
        xi_refine_surf(ist+1,1) = xei_tri_xe3_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_xe3_sf(2)
        xi_refine_surf(ist+2,1) = xei_tri_xe3_sf(3)
        xi_refine_surf(ist+2,2) = xei_tri_xe3_sf(4)
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_ye2_sf ) then
        xi_refine_surf(ist+1,1) = xei_tri_ye2_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_ye2_sf(2)
        xi_refine_surf(ist+2,1) = xei_tri_ye2_sf(3)
        xi_refine_surf(ist+2,2) = xei_tri_ye2_sf(4)
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_ye4_sf ) then
        xi_refine_surf(ist+1,1) = xei_tri_ye4_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_ye4_sf(2)
        xi_refine_surf(ist+2,1) = xei_tri_ye4_sf(3)
        xi_refine_surf(ist+2,2) = xei_tri_ye4_sf(4)
!
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_full_sf          &
     &    .or. iflag_refine_surf(isurf) .eq. iflag_tri_e1_sf            &
     &    .or. iflag_refine_surf(isurf) .eq. iflag_tri_e2_sf            &
     &    .or. iflag_refine_surf(isurf) .eq. iflag_tri_e3_sf            &
     &    .or. iflag_refine_surf(isurf) .eq. iflag_tri_e4_sf) then
        do i = 1, 4
          xi_refine_surf(ist+i,1) = xei_tri_full_sf(2*i-1)
          xi_refine_surf(ist+i,2) = xei_tri_full_sf(2*i  )
        end do
!
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_full_sf_eq       &
     &       ) then
        do i = 1, 4
          xi_refine_surf(ist+i,1) = xei_tri_full_sf_eq(2*i-1)
          xi_refine_surf(ist+i,2) = xei_tri_full_sf_eq(2*i  )
        end do
!
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_n1_sf) then
        xi_refine_surf(ist+1,1) = xei_tri_n1_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_n1_sf(2)
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_n2_sf) then
        xi_refine_surf(ist+1,1) = xei_tri_n2_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_n2_sf(2)
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_n3_sf) then
        xi_refine_surf(ist+1,1) = xei_tri_n3_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_n3_sf(2)
      else if (iflag_refine_surf(isurf) .eq. iflag_tri_n4_sf) then
        xi_refine_surf(ist+1,1) = xei_tri_n4_sf(1)
        xi_refine_surf(ist+1,2) = xei_tri_n4_sf(2)
!
      else if (iflag_refine_surf(isurf) .eq. iflag_five_sf) then
        do i = 1, 4
          xi_refine_surf(ist+i,1) = xei_tri_full_sf_eq(2*i-1)
          xi_refine_surf(ist+i,2) = xei_tri_full_sf_eq(2*i  )
        end do
!
      end if
!
      end subroutine set_local_posi_refine_surf
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_posi_refine_ele(iele, ist)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iele, ist
      integer(kind= kint) :: i
!
!
      if      (iflag_refine_ele(iele) .eq. iflag_8_to_27                &
     &    .or. iflag_refine_ele(iele) .eq. iflag_double ) then
        xi_refine_ele(ist+1,1) = xezi_double(1)
        xi_refine_ele(ist+1,2) = xezi_double(2)
        xi_refine_ele(ist+1,3) = xezi_double(3)
!
!
      else if (iflag_refine_ele(iele) .eq. iflag_tri_xs1 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_xs1(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_xs1(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_xs1(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_xs2 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_xs2(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_xs2(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_xs2(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_ys3 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_ys3(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_ys3(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_ys3(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_ys4 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_ys4(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_ys4(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_ys4(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_zs5 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_zs5(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_zs5(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_zs5(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_zs6 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_zs6(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_zs6(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_zs6(3*i  )
        end do
!
      else if (iflag_refine_ele(iele) .eq. iflag_tri_full ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_full(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_full(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_full(3*i  )
        end do
!
      else if (iflag_refine_ele(iele) .eq. iflag_tri_full_eq ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_full_eq(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_full_eq(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_full_eq(3*i  )
        end do
!
      else if (iflag_refine_ele(iele) .eq. iflag_tri_s1 ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_s1(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_s1(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_s1(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_s2 ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_s2(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_s2(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_s2(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_s3 ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_s3(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_s3(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_s3(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_s4 ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_s4(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_s4(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_s4(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_s5 ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_s5(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_s5(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_s5(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_s6 ) then
        do i = 1, 8
          xi_refine_ele(ist+i,1) = xezi_tri_s6(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_s6(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_s6(3*i  )
        end do
!
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e1 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e1(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e1(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e1(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e2 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e2(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e2(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e2(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e3 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e3(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e3(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e3(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e4 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e4(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e4(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e4(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e5 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e5(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e5(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e5(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e6 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e6(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e6(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e6(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e7 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e7(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e7(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e7(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e8 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e8(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e8(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e8(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e9 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e9(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e9(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e9(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e10 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e10(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e10(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e10(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e11 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e11(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e11(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e11(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_tri_e12 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_e12(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_e12(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_e12(3*i  )
        end do
!
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n1 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n1(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n1(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n1(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n2 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n2(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n2(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n2(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n3 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n3(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n3(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n3(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n4 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n4(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n4(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n4(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n5 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n5(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n5(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n5(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n6 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n6(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n6(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n6(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n7 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n7(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n7(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n7(3)
      else if (iflag_refine_ele(iele) .eq. iflag_tri_n8 ) then
        xi_refine_ele(ist+1,1) = xezi_tri_n8(1)
        xi_refine_ele(ist+1,2) = xezi_tri_n8(2)
        xi_refine_ele(ist+1,3) = xezi_tri_n8(3)
!
!
      else if (iflag_refine_ele(iele) .eq. iflag_five_s1 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_fxs1(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_fxs1(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_fxs1(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_five_s2 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_fxs2(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_fxs2(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_fxs2(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_five_s3 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_fys3(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_fys3(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_fys3(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_five_s4 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_fys4(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_fys4(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_fys4(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_five_s5 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_fzs5(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_fzs5(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_fzs5(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_five_s6 ) then
        do i = 1, 4
          xi_refine_ele(ist+i,1) = xezi_tri_fzs6(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_tri_fzs6(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_tri_fzs6(3*i  )
        end do
!
!
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e1 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e1(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e1(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e1(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e2 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e2(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e2(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e2(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e3 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e3(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e3(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e3(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e4 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e4(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e4(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e4(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e5 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e5(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e5(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e5(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e6 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e6(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e6(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e6(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e7 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e7(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e7(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e7(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e8 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e8(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e8(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e8(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e9 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e9(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e9(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e9(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e10 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e10(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e10(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e10(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e11 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e11(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e11(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e11(3*i  )
        end do
      else if (iflag_refine_ele(iele) .eq. iflag_stri_e12 ) then
        do i = 1, 2
          xi_refine_ele(ist+i,1) = xezi_stri_e12(3*i-2)
          xi_refine_ele(ist+i,2) = xezi_stri_e12(3*i-1)
          xi_refine_ele(ist+i,3) = xezi_stri_e12(3*i  )
        end do
!
!
      end if
!
      end subroutine set_local_posi_refine_ele
!
!  ---------------------------------------------------------------------
!
      end module set_local_position_4_refine
