!m_local_refiened_position.f90
!      module m_local_refiened_position
!
      module m_local_refiened_position
!
      use m_constants
!
      use m_precision
!
      implicit none
!
!
!      real(kind = kreal), parameter, private :: zero =    0.0d0
!      real(kind = kreal), parameter, private :: one =     1.0d0
!      real(kind = kreal), parameter, private :: two =     2.0d0
!      real(kind = kreal), parameter, private :: three =   3.0d0
!      real(kind = kreal), parameter, private :: six =   two*three
!      real(kind = kreal), parameter, private :: ten =    10.0d0
!      real(kind = kreal), parameter, private :: twenty = 20.0d0
!      real(kind = kreal), parameter, private :: half =  one/two
!      real(kind = kreal), parameter, private :: third = one/three
!      real(kind = kreal), parameter, private :: deci =  one/ten
!
      real(kind = kreal), parameter, private :: r13 =    13.0d0
!
      real(kind = kreal), parameter, private :: r0_2 = two * deci
      real(kind = kreal), parameter, private :: r0_3 = three * deci
      real(kind = kreal), parameter, private :: r0_4 = two * two* deci
      real(kind = kreal), parameter, private :: r0_6 = six * deci
!
!
      real(kind = kreal), parameter :: xezi_double(3)                   &
     &      = (/  zero,  zero,  zero/)
!       position for iflag_refine = iflag_double, iflag_8_to_27
!
      real(kind = kreal), parameter :: xezi_tri_xs1(12)                 &
     &      = (/-r0_6,-third,-third,    -r0_6, third,-third,            &
     &          -r0_6, third, third,    -r0_6,-third, third /)
!       position for iflag_refine = iflag_tri_xs1  (2,3,7,6)
      real(kind = kreal), parameter :: xezi_tri_xs2(12)                 &
     &      = (/ r0_6,-third,-third,     r0_6, third,-third,            &
     &           r0_6, third, third,     r0_6,-third, third /)
!       position for iflag_refine = iflag_tri_xs2  (1,4,8,5)
      real(kind = kreal), parameter :: xezi_tri_ys3(12)                 &
     &      = (/-third,-r0_6,-third,    -third,-r0_6, third,            &
     &           third,-r0_6, third,     third,-r0_6,-third /)
!       position for iflag_refine = iflag_tri_ys3  (4,8,7,3)
      real(kind = kreal), parameter :: xezi_tri_ys4(12)                 &
     &      = (/-third, r0_6,-third,    -third, r0_6, third,            &
     &           third, r0_6, third,     third, r0_6,-third /)
!       position for iflag_refine = iflag_tri_ys4  (1,5,6,2)
      real(kind = kreal), parameter :: xezi_tri_zs5(12)                 &
     &      = (/-third,-third,-r0_6,     third,-third,-r0_6,            &
     &           third, third,-r0_6,    -third, third,-r0_6 /)
!       position for iflag_refine = iflag_tri_zs5  (5,6,7,8)
      real(kind = kreal), parameter :: xezi_tri_zs6(12)                 &
     &      = (/-third,-third, r0_6,     third,-third, r0_6,            &
     &           third, third, r0_6,    -third, third, r0_6 /)
!       position for iflag_refine = iflag_tri_zs6  (1,2,3,4)
!       position for iflag_refine = iflag_five_s6  (1,2,3,4)
!
!
      real(kind = kreal), parameter :: xezi_tri_fxs1(12)                &
     &      = (/ third,-third,-third,     third, third,-third,          &
     &           third, third, third,     third,-third, third /)
!       position for iflag_refine = iflag_five_s1  (2,3,7,6)
      real(kind = kreal), parameter :: xezi_tri_fxs2(12)                &
     &      = (/-third,-third,-third,    -third, third,-third,          &
     &          -third, third, third,    -third,-third, third /)
!       position for iflag_refine = iflag_five_s2  (1,4,8,5)
      real(kind = kreal), parameter :: xezi_tri_fys3(12)                &
     &      = (/-third, third,-third,    -third, third, third,          &
     &           third, third, third,     third, third,-third /)
!       position for iflag_refine = iflag_five_s3  (4,8,7,3)
      real(kind = kreal), parameter :: xezi_tri_fys4(12)                &
     &      = (/-third,-third,-third,    -third,-third, third,          &
     &           third,-third, third,     third,-third,-third /)
!       position for iflag_refine = iflag_five_s4  (1,5,6,2)
      real(kind = kreal), parameter :: xezi_tri_fzs5(12)                &
     &      = (/-third,-third, third,     third,-third, third,          &
     &           third, third, third,    -third, third, third /)
!       position for iflag_refine = iflag_five_s5  (5,6,7,8)
      real(kind = kreal), parameter :: xezi_tri_fzs6(12)                &
     &      = (/-third,-third,-third,     third,-third,-third,          &
     &           third, third,-third,    -third, third,-third /)
!       position for iflag_refine = iflag_five_s6  (1,2,3,4)
!
!
      real(kind = kreal), parameter :: xezi_tri_full_eq(24)             &
     &      = (/-third,-third,-third,     third,-third,-third,          &
     &           third, third,-third,    -third, third,-third,          &
     &          -third,-third, third,     third,-third, third,          &
     &           third, third, third,    -third, third, third /)
!       position for iflag_refine = iflag_tri_full_eq
!
!
      real(kind = kreal), parameter :: xezi_tri_full(24)                &
     &      = (/-r0_4,-r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4, r0_4,-r0_4,    -r0_4, r0_4,-r0_4,                &
     &          -r0_4,-r0_4, r0_4,     r0_4,-r0_4, r0_4,                &
     &           r0_4, r0_4, r0_4,    -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_full
!
      real(kind = kreal), parameter :: xezi_tri_s1(24)                  &
     &      = (/-r0_4,-r0_4,-r0_4,     -r0_2,-r0_4,-r0_4,               &
     &          -r0_2, r0_4,-r0_4,     -r0_4, r0_4,-r0_4,               &
     &          -r0_4,-r0_4, r0_4,     -r0_2,-r0_4, r0_4,               &
     &          -r0_2, r0_4, r0_4,     -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_s1
      real(kind = kreal), parameter :: xezi_tri_s2(24)                  &
     &      = (/ r0_2,-r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4, r0_4,-r0_4,     r0_2, r0_4,-r0_4,                &
     &           r0_2,-r0_4, r0_4,     r0_4,-r0_4, r0_4,                &
     &           r0_4, r0_4, r0_4,     r0_2, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_s2
      real(kind = kreal), parameter :: xezi_tri_s3(24)                  &
     &      = (/-r0_4,-r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4,-r0_2,-r0_4,    -r0_4,-r0_2,-r0_4,                &
     &          -r0_4,-r0_4, r0_4,     r0_4,-r0_4, r0_4,                &
     &           r0_4,-r0_2, r0_4,    -r0_4,-r0_2, r0_4 /)
!       position for iflag_refine = iflag_tri_s3
      real(kind = kreal), parameter :: xezi_tri_s4(24)                  &
     &      = (/-r0_4, r0_2,-r0_4,     r0_4, r0_2,-r0_4,                &
     &           r0_4, r0_4,-r0_4,    -r0_4, r0_4,-r0_4,                &
     &          -r0_4, r0_2, r0_4,     r0_4, r0_2, r0_4,                &
     &           r0_4, r0_4, r0_4,    -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_s4
      real(kind = kreal), parameter :: xezi_tri_s5(24)                  &
     &      = (/-r0_4,-r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4, r0_4,-r0_4,    -r0_4, r0_4,-r0_4,                &
     &          -r0_4,-r0_4, -r0_2,    r0_4,-r0_4,-r0_2,                &
     &           r0_4, r0_4, -r0_2,   -r0_4, r0_4,-r0_2 /)
!       position for iflag_refine = iflag_tri_s5
      real(kind = kreal), parameter :: xezi_tri_s6(24)                  &
     &      = (/-r0_4,-r0_4, r0_2,     r0_4,-r0_4, r0_2,                &
     &           r0_4, r0_4, r0_2,    -r0_4, r0_4, r0_2,                &
     &          -r0_4,-r0_4, r0_4,     r0_4,-r0_4, r0_4,                &
     &           r0_4, r0_4, r0_4,    -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_s6
!
      real(kind = kreal), parameter :: xezi_tri_e1(12)                  &
     &      = (/-r0_4,-r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4, r0_4, r0_4,    -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e1  (1,2,7,8)
      real(kind = kreal), parameter :: xezi_tri_e2(12)                  &
     &      = (/ r0_4,-r0_4,-r0_4,     r0_4, r0_4,-r0_4,                &
     &          -r0_4, r0_4, r0_4,    -r0_4,-r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e2  (2,3,8,5)
      real(kind = kreal), parameter :: xezi_tri_e3(12)                  &
     &      = (/ r0_4, r0_4,-r0_4,    -r0_4, r0_4,-r0_4,                &
     &          -r0_4,-r0_4, r0_4,     r0_4,-r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e3  (3,4,5,6)
      real(kind = kreal), parameter :: xezi_tri_e4(12)                  &
     &      = (/-r0_4, r0_4,-r0_4,    -r0_4,-r0_4,-r0_4,                &
     &           r0_4,-r0_4, r0_4,     r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e4  (4,1,6,7)
      real(kind = kreal), parameter :: xezi_tri_e5(12)                  &
     &      = (/ r0_4, r0_4,-r0_4,    -r0_4, r0_4,-r0_4,                &
     &          -r0_4,-r0_4, r0_4,     r0_4,-r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e5  (3,4,5,6)
      real(kind = kreal), parameter :: xezi_tri_e6(12)                  &
     &      = (/-r0_4, r0_4,-r0_4,    -r0_4,-r0_4,-r0_4,                &
     &           r0_4,-r0_4, r0_4,     r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e6  (4,1,6,7)
      real(kind = kreal), parameter :: xezi_tri_e7(12)                  &
     &      = (/-r0_4,-r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4, r0_4, r0_4,    -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e7  (1,2,7,8)
      real(kind = kreal), parameter :: xezi_tri_e8(12)                  &
     &      = (/ r0_4,-r0_4,-r0_4,     r0_4, r0_4,-r0_4,                &
     &          -r0_4, r0_4, r0_4,    -r0_4,-r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e8  (2,3,8,5)
      real(kind = kreal), parameter :: xezi_tri_e9(12)                  &
     &      = (/-r0_4,-r0_4,-r0_4,     r0_4, r0_4,-r0_4,                &
     &           r0_4, r0_4, r0_4,    -r0_4,-r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e9  (1,3,7,5)
      real(kind = kreal), parameter :: xezi_tri_e10(12)                 &
     &      = (/ r0_4,-r0_4,-r0_4,    -r0_4, r0_4,-r0_4,                &
     &          -r0_4, r0_4, r0_4,     r0_4,-r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e10  (2,4,8,6)
      real(kind = kreal), parameter :: xezi_tri_e11(12)                 &
     &      = (/ r0_4, r0_4,-r0_4,    -r0_4,-r0_4,-r0_4,                &
     &          -r0_4,-r0_4, r0_4,     r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e11  (3,1,5,7)
      real(kind = kreal), parameter :: xezi_tri_e12(12)                 &
     &      = (/-r0_4, r0_4,-r0_4,     r0_4,-r0_4,-r0_4,                &
     &           r0_4,-r0_4, r0_4,    -r0_4, r0_4, r0_4 /)
!       position for iflag_refine = iflag_tri_e12  (4,2,6,8)
!
      real(kind = kreal), parameter :: xezi_tri_n1(3)                   &
     &      = (/-r0_4,-r0_4,-r0_4/)
!       position for iflag_refine = iflag_tri_n1
      real(kind = kreal), parameter :: xezi_tri_n2(3)                   &
     &      = (/ r0_4,-r0_4,-r0_4/)
!       position for iflag_refine = iflag_tri_n2
      real(kind = kreal), parameter :: xezi_tri_n3(3)                   &
     &      = (/ r0_4, r0_4,-r0_4/)
!       position for iflag_refine = iflag_tri_n3
      real(kind = kreal), parameter :: xezi_tri_n4(3)                   &
     &      = (/-r0_4, r0_4,-r0_4/)
!       position for iflag_refine = iflag_tri_n4
      real(kind = kreal), parameter :: xezi_tri_n5(3)                   &
     &      = (/-r0_4,-r0_4, r0_4/)
!       position for iflag_refine = iflag_tri_n5
      real(kind = kreal), parameter :: xezi_tri_n6(3)                   &
     &      = (/ r0_4,-r0_4, r0_4/)
!       position for iflag_refine = iflag_tri_n6
      real(kind = kreal), parameter :: xezi_tri_n7(3)                   &
     &      = (/ r0_4, r0_4, r0_4/)
!       position for iflag_refine = iflag_tri_n7
      real(kind = kreal), parameter :: xezi_tri_n8(3)                   &
     &      = (/-r0_4, r0_4, r0_4/)
!       position for iflag_refine = iflag_tri_n8
!
!
      real(kind = kreal), parameter :: xezi_stri_e1( 6)                 &
     &      = (/ third, third, third,    -third, third, third /)
!       position for iflag_refine = iflag_stri_e1  (7,8)
      real(kind = kreal), parameter :: xezi_stri_e2( 6)                 &
     &      = (/-third, third, third,    -third,-third, third /)
!       position for iflag_refine = iflag_stri_e2  (8,5)
      real(kind = kreal), parameter :: xezi_stri_e3( 6)                 &
     &      = (/-third,-third, third,     third,-third, third /)
!       position for iflag_refine = iflag_stri_e3  (5,6)
      real(kind = kreal), parameter :: xezi_stri_e4( 6)                 &
     &      = (/ third,-third, third,     third, third, third /)
!       position for iflag_refine = iflag_stri_e4  (6,7)
      real(kind = kreal), parameter :: xezi_stri_e5( 6)                 &
     &      = (/ third, third,-third,    -third, third,-third /)
!       position for iflag_refine = iflag_stri_e5  (3,4)
      real(kind = kreal), parameter :: xezi_stri_e6( 6)                 &
     &      = (/-third, third,-third,    -third,-third,-third /)
!       position for iflag_refine = iflag_stri_e6  (4,1)
      real(kind = kreal), parameter :: xezi_stri_e7( 6)                 &
     &      = (/-third,-third,-third,     third,-third,-third /)
!       position for iflag_refine = iflag_stri_e7  (1,2)
      real(kind = kreal), parameter :: xezi_stri_e8( 6)                 &
     &      = (/ third,-third,-third,     third, third,-third /)
!       position for iflag_refine = iflag_stri_e8  (2,3)
      real(kind = kreal), parameter :: xezi_stri_e9( 6)                 &
     &      = (/ third, third,-third,     third, third, third /)
!       position for iflag_refine = iflag_stri_e9  (3,7)
      real(kind = kreal), parameter :: xezi_stri_e10( 6)                &
     &      = (/-third, third,-third,    -third, third, third /)
!       position for iflag_refine = iflag_stri_e10  (4,8)
      real(kind = kreal), parameter :: xezi_stri_e11( 6)                &
     &      = (/-third,-third,-third,    -third,-third, third /)
!       position for iflag_refine = iflag_stri_e11  (1,5)
      real(kind = kreal), parameter :: xezi_stri_e12( 6)                &
     &      = (/ third,-third,-third,     third,-third, third /)
!       position for iflag_refine = iflag_stri_e12  (2,6)
!
!
!   refine for surface
!
!
      real(kind = kreal), parameter :: xei_dbl_sf(2)                    &
     &      = (/  zero,  zero /)
!       position for iflag_refine_surf = iflag_4_to_9_sf, 
!                                        iflag_dbl_sf
!
      real(kind = kreal), parameter :: xei_tri_xe1_sf(4)                &
     &      = (/-third,  r0_2,     third,  r0_2 /)
!       position for iflag_refine_surf = iflag_tri_xe1_sf  (4,3)
      real(kind = kreal), parameter :: xei_tri_xe3_sf(4)                &
     &      = (/-third, -r0_2,     third, -r0_2 /)
!       position for iflag_refine_surf = iflag_tri_xe3_sf  (1,2)
      real(kind = kreal), parameter :: xei_tri_ye2_sf(4)                &
     &      = (/ -r0_2,-third,     -r0_2,  third/)
!       position for iflag_refine_surf = iflag_tri_ye2_sf  (1,4)
      real(kind = kreal), parameter :: xei_tri_ye4_sf(4)                &
     &      = (/  r0_2,-third,      r0_2,  third/)
!       position for iflag_refine_surf = iflag_tri_ye4_sf  (2,3)
!
      real(kind = kreal), parameter :: xei_tri_full_sf_eq(8)            &
     &      = (/-third, -third,    third,-third,                        &
     &           third,  third,   -third, third/)
!       position for iflag_refine_surf = iflag_tri_full_sf_eq
!
      real(kind = kreal), parameter :: xei_tri_full_sf(8)               &
     &      = (/-r0_4, -r0_4,    r0_4,-r0_4,                            &
     &           r0_4,  r0_4,   -r0_4, r0_4/)
!       position for iflag_refine_surf = iflag_tri_full_sf,
!                                        iflag_tri_e1_sf,
!                                        iflag_tri_e2_sf,
!                                        iflag_tri_e3_sf,
!                                        iflag_tri_e4_sf
      real(kind = kreal), parameter :: xei_tri_n1_sf(2)                 &
     &      = (/-r0_4, -r0_4/)
!       position for iflag_refine_surf = iflag_tri_n1_sf
      real(kind = kreal), parameter :: xei_tri_n2_sf(2)                 &
     &      = (/ r0_4, -r0_4/)
!       position for iflag_refine_surf = iflag_tri_n2_sf
      real(kind = kreal), parameter :: xei_tri_n3_sf(2)                 &
     &      = (/ r0_4,  r0_4/)
!       position for iflag_refine_surf = iflag_tri_n3_sf
      real(kind = kreal), parameter :: xei_tri_n4_sf(2)                 &
     &      = (/-r0_4,  r0_4/)
!       position for iflag_refine_surf = iflag_tri_n4_sf
!
!
!   refine for edge
!
      real(kind = kreal), parameter :: xi_dbl_ed(1) =    (/ zero/)
!       position for iflag_refine_edge = iflag_dbl_ed, iflag_2_to_3_ed
      real(kind = kreal), parameter :: xi_tri_full_ed_eq(2)             &
     &      = (/-third, third/)
!       position for iflag_refine_edge = iflag_tri_full_ed_eq
      real(kind = kreal), parameter :: xi_tri_full_ed(2)                &
     &      = (/-r0_4, r0_4/)
!       position for iflag_refine_edge = iflag_tri_full_ed
      real(kind = kreal), parameter :: xi_tri_n1_ed(1) = (/-r0_4/)
!       position for iflag_refine_edge = iflag_tri_n1_ed
      real(kind = kreal), parameter :: xi_tri_n2_ed(1) = (/ r0_4/)
!       position for iflag_refine_edge = iflag_tri_n2_ed
!
      end module m_local_refiened_position
