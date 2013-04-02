!set_surf_edge_refine_flags.f90
!      module set_surf_edge_refine_flags
!
      module set_surf_edge_refine_flags
!
!      Writen by H. Matsui on Oct., 2007
!
      use m_precision
!
      use m_refine_flag_parameters
!
      implicit none
!
      private :: surf_edge_refine_flag_4_5seg
      private :: surf_edge_refine_flag_4_double
!
!      subroutine s_set_surf_edge_refine_flags(iflag_refine,            &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_surf_edge_refine_flags(iflag_refine,             &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      use set_surf_edge_tri_flag
      use set_surf_edge_stri_flag
!
      integer(kind = kint), intent(in) :: iflag_refine
!
      integer(kind = kint), intent(inout) :: iflag_refine_sf_l(6)
      integer(kind = kint), intent(inout) :: iflag_refine_ed_l(12)
!
!
      if (iflag_refine .eq. iflag_nothing) then
        iflag_refine_sf_l(1:6 ) = iflag_nothing_sf
        iflag_refine_ed_l(1:12) = iflag_nothing_ed
      end if
!
!
      if (iflag_refine .eq. iflag_8_to_20) then
        iflag_refine_sf_l(1:6 ) = iflag_4_to_8_sf
        iflag_refine_ed_l(1:12) = iflag_2_to_3_ed
      end if
!
!
      if (iflag_refine .eq. iflag_8_to_27) then
        iflag_refine_sf_l(1:6 ) = iflag_4_to_9_sf
        iflag_refine_ed_l(1:12) = iflag_2_to_3_ed
      end if
!
!
      if (iflag_refine .eq. iflag_double) then
        iflag_refine_sf_l(1:6 ) = iflag_dbl_sf
        iflag_refine_ed_l(1:12) = iflag_dbl_ed
!
      else if(iflag_refine .eq. iflag_double_x                          &
     &   .or. iflag_refine .eq. iflag_double_y                          &
     &   .or. iflag_refine .eq. iflag_double_z) then
        call surf_edge_refine_flag_4_double(iflag_refine,               &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
      end if
!
!
      if(iflag_refine .eq. iflag_tri_x                                  &
     &   .or. iflag_refine .eq. iflag_tri_y                             &
     &   .or. iflag_refine .eq. iflag_tri_z) then
        call surf_edge_refine_flag_tri_1d(iflag_refine,                 &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      else if(iflag_refine .eq. iflag_tri_xs1                           &
     &   .or. iflag_refine .eq. iflag_tri_xs2                           &
     &   .or. iflag_refine .eq. iflag_tri_ys3                           &
     &   .or. iflag_refine .eq. iflag_tri_ys4                           &
     &   .or. iflag_refine .eq. iflag_tri_zs5                           &
     &   .or. iflag_refine .eq. iflag_tri_zs6) then
        call surf_edge_refine_flag_stri_surf(iflag_refine,              &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
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
        call surf_edge_refine_flag_stri_edge(iflag_refine,              &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      else if(iflag_refine .eq. iflag_tri_s1                            &
     &   .or. iflag_refine .eq. iflag_tri_s2                            &
     &   .or. iflag_refine .eq. iflag_tri_s3                            &
     &   .or. iflag_refine .eq. iflag_tri_s4                            &
     &   .or. iflag_refine .eq. iflag_tri_s5                            &
     &   .or. iflag_refine .eq. iflag_tri_s6) then
        call surf_edge_refine_flag_tri_surf(iflag_refine,               &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
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
        call surf_edge_refine_flag_tri_edge(iflag_refine,               &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      else if(iflag_refine .eq. iflag_tri_n1                            &
     &   .or. iflag_refine .eq. iflag_tri_n2                            &
     &   .or. iflag_refine .eq. iflag_tri_n3                            &
     &   .or. iflag_refine .eq. iflag_tri_n4                            &
     &   .or. iflag_refine .eq. iflag_tri_n5                            &
     &   .or. iflag_refine .eq. iflag_tri_n6                            &
     &   .or. iflag_refine .eq. iflag_tri_n7                            &
     &   .or. iflag_refine .eq. iflag_tri_n8) then
        call surf_edge_refine_flag_tri_node(iflag_refine,               &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
      end if
!
      if (iflag_refine .eq. iflag_tri_full) then
        iflag_refine_sf_l(1:6 ) = iflag_tri_full_sf
        iflag_refine_ed_l(1:12) = iflag_tri_full_ed
      end if
!
      if (iflag_refine .eq. iflag_tri_full_eq) then
        iflag_refine_sf_l(1:6 ) = iflag_tri_full_sf_eq
        iflag_refine_ed_l(1:12) = iflag_tri_full_ed_eq
      end if
!
      if     (iflag_refine .eq. iflag_five_x                            &
     &   .or. iflag_refine .eq. iflag_five_y                            &
     &   .or. iflag_refine .eq. iflag_five_z                            &
     &   .or. iflag_refine .eq. iflag_five_s1                           &
     &   .or. iflag_refine .eq. iflag_five_s2                           &
     &   .or. iflag_refine .eq. iflag_five_s3                           &
     &   .or. iflag_refine .eq. iflag_five_s4                           &
     &   .or. iflag_refine .eq. iflag_five_s5                           &
     &   .or. iflag_refine .eq. iflag_five_s6) then
        call surf_edge_refine_flag_4_5seg(iflag_refine,                 &
     &      iflag_refine_sf_l, iflag_refine_ed_l)
      end if
!
      end subroutine s_set_surf_edge_refine_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_4_double(iflag_refine,           &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      integer(kind = kint), intent(in) :: iflag_refine
!
      integer(kind = kint), intent(inout) :: iflag_refine_sf_l(6)
      integer(kind = kint), intent(inout) :: iflag_refine_ed_l(12)
!
!
      iflag_refine_ed_l(1:12) = iflag_nothing_ed
!
      if (iflag_refine .eq. iflag_double_x) then
        iflag_refine_sf_l(1) = iflag_dbl_sf
        iflag_refine_sf_l(2) = iflag_dbl_sf
        iflag_refine_sf_l(3) = iflag_dbl_y_sf
        iflag_refine_sf_l(4) = iflag_dbl_x_sf
        iflag_refine_sf_l(5) = iflag_dbl_x_sf
        iflag_refine_sf_l(6) = iflag_dbl_y_sf
!
        iflag_refine_ed_l(2) =  iflag_dbl_ed
        iflag_refine_ed_l(4) =  iflag_dbl_ed
        iflag_refine_ed_l(6) =  iflag_dbl_ed
        iflag_refine_ed_l(8) =  iflag_dbl_ed
        iflag_refine_ed_l(9) =  iflag_dbl_ed
        iflag_refine_ed_l(10) = iflag_dbl_ed
        iflag_refine_ed_l(11) = iflag_dbl_ed
        iflag_refine_ed_l(12) = iflag_dbl_ed
      end if
!
      if (iflag_refine .eq. iflag_double_y) then
        iflag_refine_sf_l(1) = iflag_dbl_x_sf
        iflag_refine_sf_l(2) = iflag_dbl_y_sf
        iflag_refine_sf_l(3) = iflag_dbl_sf
        iflag_refine_sf_l(4) = iflag_dbl_sf
        iflag_refine_sf_l(5) = iflag_dbl_y_sf
        iflag_refine_sf_l(6) = iflag_dbl_x_sf
!
        iflag_refine_ed_l(1) =  iflag_dbl_ed
        iflag_refine_ed_l(3) =  iflag_dbl_ed
        iflag_refine_ed_l(5) =  iflag_dbl_ed
        iflag_refine_ed_l(7) =  iflag_dbl_ed
        iflag_refine_ed_l(9) =  iflag_dbl_ed
        iflag_refine_ed_l(10) = iflag_dbl_ed
        iflag_refine_ed_l(11) = iflag_dbl_ed
        iflag_refine_ed_l(12) = iflag_dbl_ed
      end if
!
!
      if (iflag_refine .eq. iflag_double_z) then
        iflag_refine_sf_l(1) = iflag_dbl_y_sf
        iflag_refine_sf_l(2) = iflag_dbl_x_sf
        iflag_refine_sf_l(3) = iflag_dbl_x_sf
        iflag_refine_sf_l(4) = iflag_dbl_y_sf
        iflag_refine_sf_l(5) = iflag_dbl_sf
        iflag_refine_sf_l(6) = iflag_dbl_sf
!
        iflag_refine_ed_l(1) =  iflag_dbl_ed
        iflag_refine_ed_l(2) =  iflag_dbl_ed
        iflag_refine_ed_l(3) =  iflag_dbl_ed
        iflag_refine_ed_l(4) =  iflag_dbl_ed
        iflag_refine_ed_l(5) =  iflag_dbl_ed
        iflag_refine_ed_l(6) =  iflag_dbl_ed
        iflag_refine_ed_l(7) =  iflag_dbl_ed
        iflag_refine_ed_l(8) =  iflag_dbl_ed
      end if
!
      end subroutine surf_edge_refine_flag_4_double
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_4_5seg(iflag_refine,             &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      integer(kind = kint), intent(in) :: iflag_refine
!
      integer(kind = kint), intent(inout) :: iflag_refine_sf_l(6)
      integer(kind = kint), intent(inout) :: iflag_refine_ed_l(12)
!
!
      iflag_refine_sf_l(1:6) =  iflag_nothing_sf
      iflag_refine_ed_l(1:12) = iflag_nothing_ed
!
      if      (iflag_refine .eq. iflag_five_x) then
        iflag_refine_sf_l(1) = iflag_five_sf
        iflag_refine_sf_l(2) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_y) then
        iflag_refine_sf_l(3) = iflag_five_sf
        iflag_refine_sf_l(4) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_z) then
        iflag_refine_sf_l(5) = iflag_five_sf
        iflag_refine_sf_l(6) = iflag_five_sf
!
      else if (iflag_refine .eq. iflag_five_s1) then
        iflag_refine_sf_l(1) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_s2) then
        iflag_refine_sf_l(2) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_s3) then
        iflag_refine_sf_l(3) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_s4) then
        iflag_refine_sf_l(4) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_s5) then
        iflag_refine_sf_l(5) = iflag_five_sf
      else if (iflag_refine .eq. iflag_five_s6) then
        iflag_refine_sf_l(6) = iflag_five_sf
      end if
!
      end subroutine surf_edge_refine_flag_4_5seg
!
!  ---------------------------------------------------------------------
!
      end module set_surf_edge_refine_flags
