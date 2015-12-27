!set_refined_node_id.f90
!      module set_refined_node_id
!
!      subroutine s_set_refined_node_id                                 &
!     &         (numnod, numele, numsurf, numedge)
!
      module set_refined_node_id
!
      use m_precision
!
      implicit none
!
      private :: set_inod_refine_edge
      private :: set_inod_refine_surf
      private :: set_inod_refine_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_refined_node_id                                  &
     &         (numnod, numele, numsurf, numedge)
!
      use m_refined_element_data
      use m_refined_node_id
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: numsurf, numedge
!
      integer(kind = kint) :: inod, iedge, isurf, iele
      integer(kind = kint) :: ist, jst, jed
!
      do inod = 1, numnod
        inod_refine_nod(inod) = inod
      end do
!
      do iedge = 1, numedge
        ist = ntot_nod_refine_nod + istack_nod_refine_edge(iedge-1)
        jst = istack_nod_refine_edge(iedge-1) + 1
        jed = istack_nod_refine_edge(iedge)
!
        call set_inod_refine_edge(iflag_refine_edge(iedge), ist,        &
     &      num_nod_refine_edge(iedge), inod_refine_edge(jst) )
!
      end do
!
      do isurf = 1, numsurf
        ist = ntot_nod_refine_nod + ntot_nod_refine_edge                &
     &       + istack_nod_refine_surf(isurf-1)
        jst = istack_nod_refine_surf(isurf-1) + 1
        jed = istack_nod_refine_surf(isurf)
!
        call set_inod_refine_surf(iflag_refine_surf(isurf), ist,        &
     &      num_nod_refine_surf(isurf), inod_refine_surf(jst) )
!
      end do
!
      do iele = 1, numele
        ist = ntot_nod_refine_nod + ntot_nod_refine_edge                &
     &       + ntot_nod_refine_surf + istack_nod_refine_ele(iele-1)
        jst = istack_nod_refine_ele(iele-1) + 1
        jed = istack_nod_refine_ele(iele)
!
        call set_inod_refine_ele(iflag_refine_ele(iele), ist,           &
     &      num_nod_refine_ele(iele), inod_refine_ele(jst) )
!
      end do
!
      end subroutine s_set_refined_node_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_inod_refine_edge(iflag_refine_edge, ist,           &
     &          num_nod_refine_edge, inod_refine_edge)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: ist, num_nod_refine_edge
      integer(kind = kint), intent(in) :: iflag_refine_edge
      integer(kind = kint), intent(inout)                               &
     &              :: inod_refine_edge(num_nod_refine_edge)
!
!
!      if      (iflag_refine_edge .eq. iflag_nothing_ed) then
      if      (iflag_refine_edge .eq. iflag_2_to_3_ed                   &
     &    .or. iflag_refine_edge .eq. iflag_dbl_ed                      &
     &    .or. iflag_refine_edge .eq. iflag_tri_n1_ed                   &
     &    .or. iflag_refine_edge .eq. iflag_tri_n2_ed) then
        inod_refine_edge(1) = ist + 1
      else if (iflag_refine_edge .eq. iflag_tri_full_ed                 &
     &    .or. iflag_refine_edge .eq. iflag_tri_full_ed_eq) then
        inod_refine_edge(1) = ist + 1
        inod_refine_edge(2) = ist + 2
      end if
!
      end subroutine set_inod_refine_edge
!
!  ---------------------------------------------------------------------
!
      subroutine set_inod_refine_surf(iflag_refine_surf, ist,           &
     &          num_nod_refine_surf, inod_refine_surf)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: ist, num_nod_refine_surf
      integer(kind = kint), intent(in) :: iflag_refine_surf
      integer(kind = kint), intent(inout)                               &
     &              :: inod_refine_surf(num_nod_refine_surf)
!
!
!      if      (iflag_refine_surf .eq. iflag_nothing_sf                 &
!     &    .or. iflag_refine_surf .eq. iflag_4_to_8_sf                  &
!     &    .or. iflag_refine_surf .eq. iflag_tri_x_sf                   &
!     &    .or. iflag_refine_surf .eq. iflag_tri_y_sf) then
      if      (iflag_refine_surf .eq. iflag_4_to_9_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_dbl_sf                      &
     &    .or. iflag_refine_surf .eq. iflag_tri_n1_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_n2_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_n3_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_n4_sf) then
        inod_refine_surf(1) = ist + 1
      else if (iflag_refine_surf .eq. iflag_tri_xe1_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_tri_xe3_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_tri_ye2_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_tri_ye4_sf) then
        inod_refine_surf(1) = ist + 1
        inod_refine_surf(2) = ist + 2
      else if (iflag_refine_surf .eq. iflag_tri_full_sf_eq              &
     &    .or. iflag_refine_surf .eq. iflag_tri_full_sf                 &
     &    .or. iflag_refine_surf .eq. iflag_tri_e1_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_e2_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_e3_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_e4_sf) then
        inod_refine_surf(1) = ist + 1
        inod_refine_surf(2) = ist + 2
        inod_refine_surf(3) = ist + 3
        inod_refine_surf(4) = ist + 4
      else if (iflag_refine_surf .eq. iflag_five_sf) then
        inod_refine_surf(1) = ist + 1
        inod_refine_surf(2) = ist + 2
        inod_refine_surf(3) = ist + 3
        inod_refine_surf(4) = ist + 4
      end if
!
      end subroutine set_inod_refine_surf
!
!  ---------------------------------------------------------------------
!
      subroutine  set_inod_refine_ele(iflag_refine_ele, ist,            &
     &          num_nod_refine_ele, inod_refine_ele )
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: ist, num_nod_refine_ele
      integer(kind = kint), intent(in) :: iflag_refine_ele
      integer(kind = kint), intent(inout)                               &
     &              :: inod_refine_ele(num_nod_refine_ele)
!
!
!      if      (iflag_refine_ele .eq. iflag_nothing                     &
!     &    .or. iflag_refine_ele .eq. iflag_8_to_20                     &
!     &    .or. iflag_refine_ele .eq. iflag_tri_x                       &
!     &    .or. iflag_refine_ele .eq. iflag_tri_y                       &
!     &    .or. iflag_refine_ele .eq. iflag_tri_z) then
!        num_nod_refine_ele = 0
      if      (iflag_refine_ele .eq. iflag_8_to_27                      &
     &    .or. iflag_refine_ele .eq. iflag_double                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n1                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n2                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n3                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n4                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n5                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n6                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n7                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n8) then
        inod_refine_ele(1) = ist + 1
      else if (iflag_refine_ele .eq. iflag_tri_xs1                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_xs2                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_ys3                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_ys4                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_zs5                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_zs6                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_e1                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e2                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e3                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e4                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e5                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e6                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e7                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e8                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e9                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_e10                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_e11                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_e12) then
        inod_refine_ele(1) = ist + 1
        inod_refine_ele(2) = ist + 2
        inod_refine_ele(3) = ist + 3
        inod_refine_ele(4) = ist + 4
      else if (iflag_refine_ele .eq. iflag_tri_full                     &
     &    .or. iflag_refine_ele .eq. iflag_tri_full_eq                  &
     &    .or. iflag_refine_ele .eq. iflag_tri_s1                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s2                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s3                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s4                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s5                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s6) then
        inod_refine_ele(1) = ist + 1
        inod_refine_ele(2) = ist + 2
        inod_refine_ele(3) = ist + 3
        inod_refine_ele(4) = ist + 4
        inod_refine_ele(5) = ist + 5
        inod_refine_ele(6) = ist + 6
        inod_refine_ele(7) = ist + 7
        inod_refine_ele(8) = ist + 8
      else if (iflag_refine_ele .eq. iflag_five_s1                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s2                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s3                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s4                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s5                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s6) then
        inod_refine_ele(1) = ist + 1
        inod_refine_ele(2) = ist + 2
        inod_refine_ele(3) = ist + 3
        inod_refine_ele(4) = ist + 4
      else if(iflag_refine_ele .eq. iflag_stri_e1                       &
 &   .or. iflag_refine_ele .eq. iflag_stri_e2                           &
     &   .or. iflag_refine_ele .eq. iflag_stri_e3                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e4                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e5                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e6                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e7                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e8                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e9                       &
     &   .or. iflag_refine_ele .eq. iflag_stri_e10                      &
     &   .or. iflag_refine_ele .eq. iflag_stri_e11                      &
     &   .or. iflag_refine_ele .eq. iflag_stri_e12) then
        inod_refine_ele(1) = ist + 1
        inod_refine_ele(2) = ist + 2
      end if
!
      end subroutine  set_inod_refine_ele
!
!  ---------------------------------------------------------------------
!
      end module set_refined_node_id
