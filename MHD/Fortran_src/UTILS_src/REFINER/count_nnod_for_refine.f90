!count_nnod_for_refine.f90
!      module count_nnod_for_refine
!
!!      subroutine s_count_nnod_for_refine                              &
!!     &         (node, ele, surf, edge, refine_tbl,                    &
!!     &          refine_nod, refine_ele, refine_surf, refine_edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_refine_table), intent(in) :: refine_tbl
!!        type(table_4_refine), intent(inout) :: refine_nod, refine_ele
!!        type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
      module count_nnod_for_refine
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: count_num_nod_refine_ele
      private :: count_num_nod_refine_surf
      private :: count_num_nod_refine_edge
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_count_nnod_for_refine                                &
     &         (node, ele, surf, edge, refine_tbl,                      &
     &          refine_nod, refine_ele, refine_surf, refine_edge)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_refined_node_id
      use t_refined_element_data
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(element_refine_table), intent(in) :: refine_tbl
      type(table_4_refine), intent(inout) :: refine_nod, refine_ele
      type(table_4_refine), intent(inout) :: refine_surf, refine_edge
!
      integer(kind = kint) :: iedge, isurf, iele
!
!
      refine_nod%num_nod_refine(1:node%numnod) = 1
!
      do iedge = 1, edge%numedge
        call count_num_nod_refine_edge                                  &
     &     (refine_tbl%iflag_refine_edge(iedge),                        &
     &      refine_edge%num_nod_refine(iedge))
      end do
!
      do isurf = 1, surf%numsurf
        call count_num_nod_refine_surf                                  &
     &     (refine_tbl%iflag_refine_surf(isurf),                        &
     &      refine_surf%num_nod_refine(isurf) )
      end do
!
      do iele = 1, ele%numele
        call count_num_nod_refine_ele                                   &
     &     (refine_tbl%iflag_refine_ele(iele),                          &
     &      refine_ele%num_nod_refine(iele) )
      end do
!
      call cal_stacks_4_refine_list(node%numnod, refine_nod)
      call cal_stacks_4_refine_list(edge%numedge, refine_edge)
      call cal_stacks_4_refine_list(surf%numsurf, refine_surf)
      call cal_stacks_4_refine_list(ele%numele, refine_ele)
!
      end subroutine s_count_nnod_for_refine
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_nod_refine_edge(iflag_refine_edge,           &
     &          num_nod_refine_edge)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iflag_refine_edge
      integer(kind = kint), intent(inout) :: num_nod_refine_edge
!
!
      if      (iflag_refine_edge .eq. iflag_nothing_ed) then
        num_nod_refine_edge = 0
      else if (iflag_refine_edge .eq. iflag_2_to_3_ed                   &
     &    .or. iflag_refine_edge .eq. iflag_dbl_ed                      &
     &    .or. iflag_refine_edge .eq. iflag_tri_n1_ed                   &
     &    .or. iflag_refine_edge .eq. iflag_tri_n2_ed) then
        num_nod_refine_edge = 1
      else if (iflag_refine_edge .eq. iflag_tri_full_ed                 &
     &    .or. iflag_refine_edge .eq. iflag_tri_full_ed_eq ) then
        num_nod_refine_edge = 2
      end if
!
      end subroutine count_num_nod_refine_edge
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_nod_refine_surf(iflag_refine_surf,           &
     &          num_nod_refine_surf )
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iflag_refine_surf
      integer(kind = kint), intent(inout) :: num_nod_refine_surf
!
!
      if      (iflag_refine_surf .eq. iflag_nothing_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_4_to_8_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_x_sf                    &
     &    .or. iflag_refine_surf .eq. iflag_tri_y_sf) then
        num_nod_refine_surf = 0
      else if (iflag_refine_surf .eq. iflag_4_to_9_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_dbl_sf                      &
     &    .or. iflag_refine_surf .eq. iflag_tri_n1_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_n2_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_n3_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_n4_sf) then
        num_nod_refine_surf = 1
      else if (iflag_refine_surf .eq. iflag_tri_xe1_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_tri_xe3_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_tri_ye2_sf                  &
     &    .or. iflag_refine_surf .eq. iflag_tri_ye4_sf) then
        num_nod_refine_surf = 2
      else if (iflag_refine_surf .eq. iflag_tri_full_sf_eq              &
     &    .or. iflag_refine_surf .eq. iflag_tri_full_sf                 &
     &    .or. iflag_refine_surf .eq. iflag_tri_e1_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_e2_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_e3_sf                   &
     &    .or. iflag_refine_surf .eq. iflag_tri_e4_sf) then
        num_nod_refine_surf = 4
      else if (iflag_refine_surf .eq. iflag_five_sf) then
        num_nod_refine_surf = 4
      end if
!
      end subroutine count_num_nod_refine_surf
!
!  ---------------------------------------------------------------------
!
      subroutine  count_num_nod_refine_ele(iflag_refine_ele,            &
     &          num_nod_refine_ele )
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iflag_refine_ele
      integer(kind = kint), intent(inout) :: num_nod_refine_ele
!
!
      if      (iflag_refine_ele .eq. iflag_nothing                      &
     &    .or. iflag_refine_ele .eq. iflag_8_to_20                      &
     &    .or. iflag_refine_ele .eq. iflag_tri_x                        &
     &    .or. iflag_refine_ele .eq. iflag_tri_y                        &
     &    .or. iflag_refine_ele .eq. iflag_tri_z) then
        num_nod_refine_ele = 0
      else if (iflag_refine_ele .eq. iflag_8_to_27                      &
     &    .or. iflag_refine_ele .eq. iflag_double                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n1                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n2                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n3                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n4                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n5                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n6                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n7                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_n8) then
        num_nod_refine_ele = 1
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
        num_nod_refine_ele = 4
      else if (iflag_refine_ele .eq. iflag_stri_e1                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e2                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e3                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e4                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e5                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e6                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e7                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e8                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e9                      &
     &    .or. iflag_refine_ele .eq. iflag_stri_e10                     &
     &    .or. iflag_refine_ele .eq. iflag_stri_e11                     &
     &    .or. iflag_refine_ele .eq. iflag_stri_e12) then
        num_nod_refine_ele = 2
      else if (iflag_refine_ele .eq. iflag_tri_full                     &
     &    .or. iflag_refine_ele .eq. iflag_tri_full_eq                  &
     &    .or. iflag_refine_ele .eq. iflag_tri_s1                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s2                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s3                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s4                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s5                       &
     &    .or. iflag_refine_ele .eq. iflag_tri_s6) then
        num_nod_refine_ele = 8
      else if (iflag_refine_ele .eq. iflag_five_s1                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s2                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s3                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s4                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s5                      &
     &    .or. iflag_refine_ele .eq. iflag_five_s6) then
        num_nod_refine_ele = 4
      end if
!
      end subroutine  count_num_nod_refine_ele
!
!  ---------------------------------------------------------------------
!
      end module count_nnod_for_refine
