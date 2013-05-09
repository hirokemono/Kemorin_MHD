!>@file   m_taylor_exp_matrix.f90
!!@brief  module m_taylor_exp_matrix
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief Matrix for Taylor expansion in grid space N
!
      module m_taylor_exp_matrix
!
      use m_precision
!
      implicit none
!
!
!>     Taylor expansion around k=th grid with 3-points
!!@verbatim
!!          (f(k), f(k+1), f(k-1))^T
!!        =   (Taylor_exp_fdm4) (f0, df/dn, d2f/dn2)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_exp_fdm2(3,3)             &
     &           = reshape( (/1.0d0,  1.0d0,  1.0d0,                    &
     &                        0.0d0,  1.0d0, -1.0d0,                    &
     &                        0.0d0,  0.5d0,  0.5d0 /),                 &
     &             shape=(/3,3/) )
!
!
!>     Taylor expansion around k=th grid with 5-points
!!@verbatim
!!          (f(k), f(k+1), f(k-1), f(k+2), f(k-2))^T
!!        =   (Taylor_exp_fdm4) (f0, df/dn, d2f/dn2, d3f/dn3, d4f/dn4)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_exp_fdm4(5,5)             &
     &      = reshape( (/1.0d0, 1.0d0,  1.0d0,  1.0d0,  1.0d0,          &
     &                   0.0d0, 1.0d0, -1.0d0,  2.0d0, -2.0d0,          &
     &                   0.0d0, 0.5d0,  0.5d0,  2.0d0,  2.0d0,          &
     &                   0.0d0, 1.0d0/6.0d0, -1.0d0/6.0d0,              &
     &                          4.0d0/3.0d0, -4.0d0/3.0d0,              &
     &                   0.0d0, 1.0d0/24.0d0,   1.0d0/24.0d0,           &
     &                          2.0d0/3.0d0,    2.0d0/3.0d0/),          &
     &                  shape=(/5,5/) )
!
!
!
!
!>     Taylor expansion at inner boundary with fixed dfdn
!!@verbatim
!!          (f(ICB),  df/dn(ICB), f(k+1))^T
!!        =   (Taylor_fixed_dn_ICB_fdm2) (f0, df/dn, d2f/dn2)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn_ICB_fdm2(3,3)    &
     &           = reshape( (/1.0d0,  0.0d0,  1.0d0,                    &
     &                        0.0d0,  1.0d0,  1.0d0,                    &
     &                        0.0d0,  0.0d0,  0.5d0 /),                 &
     &             shape=(/3,3/) )
!
!>     Taylor expansion at inner boundary with fixed d2fdn2
!!@verbatim
!!          (f(ICB), f(k+1), d2f/dn2(ICB))^T
!!        =   (Taylor_fixed_d2n_ICB_fdm2) (f0, df/dn, d2f/dn2)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_d2n_ICB_fdm2(3,3)   &
     &           = reshape( (/1.0d0,  1.0d0,  0.0d0,                    &
     &                        0.0d0,  1.0d0,  0.0d0,                    &
     &                        0.0d0,  0.5d0,  1.0d0 /),                 &
     &             shape=(/3,3/) )
!
!
!>     Taylor expansion at outer boundary with fixed dfdn
!!@verbatim
!!          (f(CMB),  df/dn(CMB), f(k-1))^T
!!        =   (Taylor_fixed_dn_CMB_fdm2) (f0, df/dn, d2f/dn2)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn_CMB_fdm2(3,3)    &
     &           = reshape( (/1.0d0,  0.0d0,  1.0d0,                    &
     &                        0.0d0,  1.0d0, -1.0d0,                    &
     &                        0.0d0,  0.0d0,  0.5d0 /),                 &
     &             shape=(/3,3/) )
!
!>     Taylor expansion at outer boundary with fixed d2fdn2
!!@verbatim
!!          (f(CMB), f(k-1), d2f/dn2(CMB))^T
!!        =   (Taylor_fixed_d2n_CMB_fdm2) (f0, df/dn, d2f/dn2)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_d2n_CMB_fdm2(3,3)   &
     &           = reshape( (/1.0d0,  1.0d0,  0.0d0,                    &
     &                        0.0d0, -1.0d0,  0.0d0,                    &
     &                        0.0d0,  0.5d0,  1.0d0 /),                 &
     &             shape=(/3,3/) )
!
!
!
!>     Taylor expansion at inner boundary with fixed df/dn
!!@verbatim
!!          (f(ICB), df/dn(ICB), f(ICB+1), f(ICB+2))^T
!!        =   (Taylor_fixed_dn_ICB_fdm4) (f0, df/dn, d2f/dn2, d3f/dn3)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn_ICB_fdm4(4,4)    &
     &      = reshape( (/1.0d0,  0.0d0,  1.0d0,        1.0d0,           &
     &                   0.0d0,  1.0d0,  1.0d0,        2.0d0,           &
     &                   0.0d0,  0.0d0,  0.5d0,        2.0d0,           &
     &                   0.0d0,  0.0d0,  1.0d0/6.0d0,  4.0d0/3.0d0/),   &
     &                  shape=(/4,4/) )
!
!>     Taylor expansion at next of inner boundary with fixed df/dn
!!@verbatim
!!          (f(ICB+1), f(ICB), f(ICB+2), df/dr(ICB), f(ICB+3))^T
!!        =   (Taylor_fixed_dn_ICB1_fdm4)
!!               (f0, df/dn, d2f/dn2, d3f/dn3, d4f/dn4)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn_ICB1_fdm4(5,5)   &
     &      = reshape( (/1.0d0,  1.0d0, 1.0d0,  0.0d0,  1.0d0,          &
     &                   0.0d0, -1.0d0, 1.0d0,  1.0d0,  2.0d0,          &
     &                   0.0d0,  0.5d0, 0.5d0, -1.0d0,  2.0d0,          &
     &                   0.0d0, -1.0d0/6.0d0, 1.0d0/6.0d0,              &
     &                           0.5d0,         4.0d0/3.0d0,            &
     &                   0.0d0,  1.0d0/24.0d0,  1.0d0/24.0d0,           &
     &                          -1.0d0/6.0d0,   2.0d0/3.0d0/),          &
     &                  shape=(/5,5/) )
!
!>     Taylor expansion at next of outer boundary with fixed df/dn
!!@verbatim
!!          (f(CMB-1), f(CMB-2), f(CMB), f(CMB-3), df/dr(CMB))^T
!!        =   (Taylor_fixed_dn_CMB1_fdm4)
!!               (f0, df/dn, d2f/dn2, d3f/dn3, d4f/dn4)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn_CMB1_fdm4(5,5)   &
     &      = reshape( (/1.0d0,  1.0d0, 1.0d0,  1.0d0,  0.0d0,          &
     &                   0.0d0, -1.0d0, 1.0d0, -2.0d0,  1.0d0,          &
     &                   0.0d0,  0.5d0, 0.5d0,  2.0d0, -1.0d0,          &
     &                   0.0d0, -1.0d0/6.0d0, 1.0d0/6.0d0,              &
     &                          -4.0d0/3.0d0, 0.5d0,                    &
     &                   0.0d0,  1.0d0/24.0d0,  1.0d0/24.0d0,           &
     &                           2.0d0/3.0d0,  -1.0d0/6.0d0/),          &
     &                  shape=(/5,5/) )
!
!>     Taylor expansion at inner boundary with fixed df/dn
!!@verbatim
!!          (f(CMB), df/dn(CMB), f(CMB-1), f(CMB-2))^T
!!        =   (Taylor_fixed_dn_CMB_fdm4) (f0, df/dn, d2f/dn2, d3f/dn3)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn_CMB_fdm4(4,4)    &
     &      = reshape( (/1.0d0,  0.0d0,  1.0d0,        1.0d0,           &
     &                   0.0d0,  1.0d0, -1.0d0,       -2.0d0,           &
     &                   0.0d0,  0.0d0,  0.5d0,        2.0d0,           &
     &                   0.0d0,  0.0d0, -1.0d0/6.0d0, -4.0d0/3.0d0/),   &
     &                  shape=(/4,4/) )
!
!
!
!>     Taylor expansion at inner boundary with fixed d2f/dn2
!!@verbatim
!!          (f(ICB), f(ICB+1), d2f/dn2(ICB), f(ICB+2))^T
!!        =   (Taylor_fixed_dn2_ICB_fdm4) (f0, df/dn, d2f/dn2, d3f/dn3)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn2_ICB_fdm4(4,4)   &
     &      = reshape( (/1.0d0,  1.0d0,        0.0d0,  1.0d0,           &
     &                   0.0d0,  1.0d0,        0.0d0,  2.0d0,           &
     &                   0.0d0,  0.5d0,        1.0d0,  2.0d0,           &
     &                   0.0d0,  1.0d0/6.0d0,  0.0d0,  4.0d0/3.0d0/),   &
     &                  shape=(/4,4/) )
!
!>     Taylor expansion at next of inner boundary with fixed d2f/dn2
!!@verbatim
!!          (f(ICB+1), f(ICB), f(ICB+2), d2f/dn2(ICB), f(ICB+3))^T
!!        =   (Taylor_fixed_dn2_ICB1_fdm4)
!!               (f0, df/dn, d2f/dn2, d3f/dn3, d4f/dn4)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn2_ICB1_fdm4(5,5)  &
     &      = reshape( (/1.0d0,  1.0d0, 1.0d0,  0.0d0,  1.0d0,          &
     &                   0.0d0, -1.0d0, 1.0d0,  0.0d0,  2.0d0,          &
     &                   0.0d0,  0.5d0, 0.5d0,  1.0d0,  2.0d0,          &
     &                   0.0d0, -1.0d0/6.0d0, 1.0d0/6.0d0,              &
     &                          -1.0d0,         4.0d0/3.0d0,            &
     &                   0.0d0,  1.0d0/24.0d0,  1.0d0/24.0d0,           &
     &                           0.5d0,         2.0d0/3.0d0/),          &
     &                  shape=(/5,5/) )
!
!>     Taylor expansion at next of outer boundary with fixed d2f/dn2
!!@verbatim
!!          (f(CMB-1), f(CMB-2), f(CMB), f(CMB-2), d2f/dn2(CMB))^T
!!        =   (Taylor_fixed_dn2_CMB1_fdm4)
!!               (f0, df/dn, d2f/dn2, d3f/dn3, d4f/dn4)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn2_CMB1_fdm4(5,5)  &
     &      = reshape( (/1.0d0,  1.0d0, 1.0d0,  1.0d0,  0.0d0,          &
     &                   0.0d0, -1.0d0, 1.0d0, -2.0d0,  0.0d0,          &
     &                   0.0d0,  0.5d0, 0.5d0,  2.0d0,  1.0d0,          &
     &                   0.0d0, -1.0d0/6.0d0, 1.0d0/6.0d0,              &
     &                          -4.0d0/3.0d0, -1.0d0,                   &
     &                   0.0d0,  1.0d0/24.0d0,  1.0d0/24.0d0,           &
     &                           2.0d0/3.0d0,  0.5d0/),                 &
     &                  shape=(/5,5/) )
!
!>     Taylor expansion at inner boundary with fixed d2f/dn2
!!@verbatim
!!          (f(CMB), f(CMB-1), d2f/dn2(CMB), f(CMB-2))^T
!!        =   (Taylor_fixed_dn2_CMB_fdm4) (f0, df/dn, d2f/dn2, d3f/dn3)^T
!!@endverbatim
      real(kind = kreal), parameter :: Taylor_fixed_dn2_CMB_fdm4(4,4)   &
     &      = reshape( (/1.0d0,  1.0d0,        0.0d0,  1.0d0,           &
     &                   0.0d0, -1.0d0,        0.0d0, -2.0d0,           &
     &                   0.0d0,  0.5d0,        1.0d0,  2.0d0,           &
     &                   0.0d0, -1.0d0/6.0d0,  0.0d0, -4.0d0/3.0d0/),   &
     &                  shape=(/4,4/) )
!
      end module m_taylor_exp_matrix
