!set_refined_connection.f90
!      module set_refined_connection
!
      module set_refined_connection
!
!      Writen by H. Matsui on Sep., 2007
!
!      subroutine count_refined_connection(iele, iflag_refine)
!      subroutine s_set_refined_connection(iele, iflag_refine)
!
      use m_precision
!
      use m_refine_flag_parameters
      use m_refined_element_data
      use m_refined_node_id
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_refined_connection(iele, iflag_refine)
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: iflag_refine
!
!
      if(iflag_refine .eq. iflag_nothing) then
        num_ele_refined(iele) = 1
!
      else if(iflag_refine .eq. iflag_8_to_20) then
        num_ele_refined(iele) = 1
      else if(iflag_refine .eq. iflag_8_to_27) then
        num_ele_refined(iele) = 1
!
!
      else if(iflag_refine .eq. iflag_double_x                          &
     &   .or. iflag_refine .eq. iflag_double_y                          &
     &   .or. iflag_refine .eq. iflag_double_z) then
        num_ele_refined(iele) = 4
!
      else if(iflag_refine .eq. iflag_double) then
        num_ele_refined(iele) = 8
!
      else if(iflag_refine .eq. iflag_tri_x                             &
     &   .or. iflag_refine .eq. iflag_tri_y                             &
     &   .or. iflag_refine .eq. iflag_tri_z) then
        num_ele_refined(iele) = 9
!
!
      else if(iflag_refine .eq. iflag_tri_xs1                           &
     &   .or. iflag_refine .eq. iflag_tri_xs2                           &
     &   .or. iflag_refine .eq. iflag_tri_ys3                           &
     &   .or. iflag_refine .eq. iflag_tri_ys4                           &
     &   .or. iflag_refine .eq. iflag_tri_zs5                           &
     &   .or. iflag_refine .eq. iflag_tri_zs6) then
        num_ele_refined(iele) = 13
!
!
      else if(iflag_refine .eq. iflag_tri_full                          &
     &   .or. iflag_refine .eq. iflag_tri_full_eq) then
        num_ele_refined(iele) = 27
!
!
      else if(iflag_refine .eq. iflag_tri_s1                            &
     &   .or. iflag_refine .eq. iflag_tri_s2                            &
     &   .or. iflag_refine .eq. iflag_tri_s3                            &
     &   .or. iflag_refine .eq. iflag_tri_s4                            &
     &   .or. iflag_refine .eq. iflag_tri_s5                            &
     &   .or. iflag_refine .eq. iflag_tri_s6) then
        num_ele_refined(iele) = 22
!
      else if(iflag_refine .eq. iflag_tri_e1                            &
     &   .or. iflag_refine .eq. iflag_tri_e2                            &
     &   .or. iflag_refine .eq. iflag_tri_e3                            &
     &   .or. iflag_refine .eq. iflag_tri_e4                            &
     &   .or. iflag_refine .eq. iflag_tri_e5                            &
     &   .or. iflag_refine .eq. iflag_tri_e6                            &
     &   .or. iflag_refine .eq. iflag_tri_e7                            &
     &   .or. iflag_refine .eq. iflag_tri_e8                            &
     &   .or. iflag_refine .eq. iflag_tri_e9                            &
     &   .or. iflag_refine .eq. iflag_tri_e10                           &
     &   .or. iflag_refine .eq. iflag_tri_e11                           &
     &   .or. iflag_refine .eq. iflag_tri_e12) then
        num_ele_refined(iele) = 11
!
      else if(iflag_refine .eq. iflag_tri_n1                            &
     &   .or. iflag_refine .eq. iflag_tri_n2                            &
     &   .or. iflag_refine .eq. iflag_tri_n3                            &
     &   .or. iflag_refine .eq. iflag_tri_n4                            &
     &   .or. iflag_refine .eq. iflag_tri_n5                            &
     &   .or. iflag_refine .eq. iflag_tri_n6                            &
     &   .or. iflag_refine .eq. iflag_tri_n7                            &
     &   .or. iflag_refine .eq. iflag_tri_n8) then
        num_ele_refined(iele) = 4
!
      else if(iflag_refine .eq. iflag_stri_e1                           &
     &   .or. iflag_refine .eq. iflag_stri_e2                           &
     &   .or. iflag_refine .eq. iflag_stri_e3                           &
     &   .or. iflag_refine .eq. iflag_stri_e4                           &
     &   .or. iflag_refine .eq. iflag_stri_e5                           &
     &   .or. iflag_refine .eq. iflag_stri_e6                           &
     &   .or. iflag_refine .eq. iflag_stri_e7                           &
     &   .or. iflag_refine .eq. iflag_stri_e8                           &
     &   .or. iflag_refine .eq. iflag_stri_e9                           &
     &   .or. iflag_refine .eq. iflag_stri_e10                          &
     &   .or. iflag_refine .eq. iflag_stri_e11                          &
     &   .or. iflag_refine .eq. iflag_stri_e12) then
        num_ele_refined(iele) = 5
!
      else if(iflag_refine .eq. iflag_five_x                            &
     &   .or. iflag_refine .eq. iflag_five_y                            &
     &   .or. iflag_refine .eq. iflag_five_z) then
        num_ele_refined(iele) = 5
!
      else if(iflag_refine .eq. iflag_five_s1                           &
     &   .or. iflag_refine .eq. iflag_five_s2                           &
     &   .or. iflag_refine .eq. iflag_five_s3                           &
     &   .or. iflag_refine .eq. iflag_five_s4                           &
     &   .or. iflag_refine .eq. iflag_five_s5                           &
     &   .or. iflag_refine .eq. iflag_five_s6) then
        num_ele_refined(iele) = 6
      end if
!
      end subroutine count_refined_connection
!
! ----------------------------------------------------------------------
!
      subroutine s_set_refined_connection(iele, iflag_refine)
!
      use copy_refined_nod_quad
      use copy_refined_nod_dbl
!
      use copy_refined_nod_tri_xyz
      use copy_refined_nod_tri_1dsurf
!
      use copy_refined_nod_tri_full
      use copy_refined_nod_tri_surf
      use copy_refined_nod_tri_edge
      use copy_refined_nod_tri_node
      use copy_refined_nod_stri_edge
!
      use copy_refined_nod_for_five
      use copy_refined_nod_five_sf
!
      use set_each_refined_connect
!
      integer(kind = kint), intent(in) :: iele
      integer(kind = kint), intent(in) :: iflag_refine
      integer(kind = kint) :: ist
!
!
      if     (iflag_refine .eq. iflag_nothing) then
!
        ist = istack_ele_refined(iele-1)
        call set_no_refined_connect(ntot_ele_refined, ist,              &
     &      inod_refine_nod_local, ie_refined )
!
      else if(iflag_refine .eq. iflag_8_to_20) then
        call s_copy_refined_nod_quad20(inod_refine_local,               &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local)
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_quad20(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
      else if(iflag_refine .eq. iflag_8_to_27) then
        call s_copy_refined_nod_quad27(inod_refine_local,               &
     &      inod_refine_nod_local, inod_refine_ele_local,               &
     &      inod_refine_surf_local, inod_refine_edge_local)
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_quad27(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_double_x) then
        call s_copy_refined_nod_dbl_x(inod_refine_local,                &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_dbl_wx(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
      else if(iflag_refine .eq. iflag_double_y) then
        call s_copy_refined_nod_dbl_y(inod_refine_local,                &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_dbl_wy(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
      else if(iflag_refine .eq. iflag_double_z) then
        call s_copy_refined_nod_dbl_z(inod_refine_local,                &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_dbl_wz(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_double) then
        call s_copy_refined_nod_dbl_full(inod_refine_local,             &
     &      inod_refine_nod_local, inod_refine_ele_local,               &
     &      inod_refine_surf_local, inod_refine_edge_local)
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_dbl_w(ntot_ele_refined, ist,           &
     &      inod_refine_local, ie_refined )
!
!
!
      else if(iflag_refine .eq. iflag_tri_x) then
        call s_copy_refined_nod_tri_x(inod_refine_local,                &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_wx(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_tri_y) then
        call s_copy_refined_nod_tri_y(inod_refine_local,                &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_wy(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_tri_z) then
        call s_copy_refined_nod_tri_z(inod_refine_local,                &
     &      inod_refine_nod_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_wz(ntot_ele_refined, ist,          &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_tri_xs1                           &
     &   .or. iflag_refine .eq. iflag_tri_xs2                           &
     &   .or. iflag_refine .eq. iflag_tri_ys3                           &
     &   .or. iflag_refine .eq. iflag_tri_ys4                           &
     &   .or. iflag_refine .eq. iflag_tri_zs5                           &
     &   .or. iflag_refine .eq. iflag_tri_zs6) then
!
        call copy_refined_nod_tri_1ds(iflag_refine,                     &
     &      inod_refine_local, inod_refine_nod_local,                   &
     &      inod_refine_ele_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_1ds(ntot_ele_refined, ist,         &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_tri_full                          &
     &   .or. iflag_refine .eq. iflag_tri_full_eq) then
        call s_copy_refined_nod_tri_full(inod_refine_local,             &
     &      inod_refine_nod_local, inod_refine_ele_local,               &
     &      inod_refine_surf_local, inod_refine_edge_local)
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_w(ntot_ele_refined, ist,           &
     &      inod_refine_local, ie_refined )
!
!
!
      else if(iflag_refine .eq. iflag_tri_s1                            &
     &   .or. iflag_refine .eq. iflag_tri_s2                            &
     &   .or. iflag_refine .eq. iflag_tri_s3                            &
     &   .or. iflag_refine .eq. iflag_tri_s4                            &
     &   .or. iflag_refine .eq. iflag_tri_s5                            &
     &   .or. iflag_refine .eq. iflag_tri_s6) then
!
        call copy_refined_nod_tri_s(iflag_refine, inod_refine_local,    &
     &      inod_refine_nod_local, inod_refine_ele_local,               &
     &      inod_refine_surf_local, inod_refine_edge_local)
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_s(ntot_ele_refined, ist,           &
     &      inod_refine_local, ie_refined )
!
!
      else if(iflag_refine .eq. iflag_tri_e1                            &
     &   .or. iflag_refine .eq. iflag_tri_e2                            &
     &   .or. iflag_refine .eq. iflag_tri_e3                            &
     &   .or. iflag_refine .eq. iflag_tri_e4                            &
     &   .or. iflag_refine .eq. iflag_tri_e5                            &
     &   .or. iflag_refine .eq. iflag_tri_e6                            &
     &   .or. iflag_refine .eq. iflag_tri_e7                            &
     &   .or. iflag_refine .eq. iflag_tri_e8                            &
     &   .or. iflag_refine .eq. iflag_tri_e9                            &
     &   .or. iflag_refine .eq. iflag_tri_e10                           &
     &   .or. iflag_refine .eq. iflag_tri_e11                           &
     &   .or. iflag_refine .eq. iflag_tri_e12) then
!
        call copy_refined_nod_tri_e(iflag_refine, inod_refine_local,    &
     &      inod_refine_nod_local, inod_refine_ele_local,               &
     &      inod_refine_surf_local, inod_refine_edge_local)
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_e(ntot_ele_refined, ist,           &
     &      inod_refine_local, ie_refined )
!
!
!
      else if(iflag_refine .eq. iflag_tri_n1                            &
     &   .or. iflag_refine .eq. iflag_tri_n2                            &
     &   .or. iflag_refine .eq. iflag_tri_n3                            &
     &   .or. iflag_refine .eq. iflag_tri_n4                            &
     &   .or. iflag_refine .eq. iflag_tri_n5                            &
     &   .or. iflag_refine .eq. iflag_tri_n6                            &
     &   .or. iflag_refine .eq. iflag_tri_n7                            &
     &   .or. iflag_refine .eq. iflag_tri_n8) then
!
        call copy_refined_nod_tri_n(iflag_refine, inod_refine_local,    &
     &      inod_refine_nod_local, inod_refine_ele_local,               &
     &      inod_refine_surf_local, inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
        call set_refined_connect_tri_n(ntot_ele_refined, ist,           &
     &      inod_refine_local, ie_refined )
!
      else if(iflag_refine .eq. iflag_stri_e1                           &
     &   .or. iflag_refine .eq. iflag_stri_e2                           &
     &   .or. iflag_refine .eq. iflag_stri_e3                           &
     &   .or. iflag_refine .eq. iflag_stri_e4                           &
     &   .or. iflag_refine .eq. iflag_stri_e5                           &
     &   .or. iflag_refine .eq. iflag_stri_e6                           &
     &   .or. iflag_refine .eq. iflag_stri_e7                           &
     &   .or. iflag_refine .eq. iflag_stri_e8                           &
     &   .or. iflag_refine .eq. iflag_stri_e9                           &
     &   .or. iflag_refine .eq. iflag_stri_e10                          &
     &   .or. iflag_refine .eq. iflag_stri_e11                          &
     &   .or. iflag_refine .eq. iflag_stri_e12) then
        call s_copy_refined_nod_stri_edge(iflag_refine,                 &
     &      inod_refine_local, inod_refine_nod_local,                   &
     &          inod_refine_ele_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
!
        if     (iflag_refine .eq. iflag_stri_e1) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e1, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e2) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e2, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e3) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e3, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e4) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e4, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e5) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e5, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e6) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e6, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e7) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e7, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e8) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e8, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e9) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e9, ntot_ele_refined, ist,                  &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e10) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e10, ntot_ele_refined, ist,                 &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e11) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e11, ntot_ele_refined, ist,                 &
              inod_refine_local, ie_refined)
!
        else if(iflag_refine .eq. iflag_stri_e12) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_sm_tri_e12, ntot_ele_refined, ist,                 &
              inod_refine_local, ie_refined)
        end if
!
      else if(iflag_refine .eq. iflag_five_x                            &
     &   .or. iflag_refine .eq. iflag_five_y                            &
     &   .or. iflag_refine .eq. iflag_five_z) then
        call s_copy_refined_nod_five(iflag_refine,                      &
     &      inod_refine_local, inod_refine_nod_local,                   &
     &      inod_refine_ele_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
!
        if      (iflag_refine .eq. iflag_five_z) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_wz, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_y) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_wy, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_x) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_wx, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        end if
!
      else if(iflag_refine .eq. iflag_five_s1                           &
     &   .or. iflag_refine .eq. iflag_five_s2                           &
     &   .or. iflag_refine .eq. iflag_five_s3                           &
     &   .or. iflag_refine .eq. iflag_five_s4                           &
     &   .or. iflag_refine .eq. iflag_five_s5                           &
     &   .or. iflag_refine .eq. iflag_five_s6) then
        call s_copy_refined_nod_five_sf(iflag_refine,                   &
     &      inod_refine_local, inod_refine_nod_local,                   &
     &      inod_refine_ele_local, inod_refine_surf_local,              &
     &      inod_refine_edge_local )
!
        ist = istack_ele_refined(iele-1)
!
        if      (iflag_refine .eq. iflag_five_s6) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_s6, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_s5) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_s5, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_s4) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_s4, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_s3) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_s3, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_s2) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_s2, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        else if (iflag_refine .eq. iflag_five_s1) then
          call set_refined_connect(num_ele_refined(iele),               &
     &        ie_new_five_s1, ntot_ele_refined, ist,                    &
              inod_refine_local, ie_refined)
!
        end if
!
      end if
!
      end subroutine s_set_refined_connection
!
!  ---------------------------------------------------------------------
!
      end module set_refined_connection
