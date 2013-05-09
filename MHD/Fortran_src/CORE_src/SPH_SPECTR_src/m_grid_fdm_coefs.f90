!>@file   m_grid_fdm_coefs.f90
!!@brief  module m_grid_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief  Coefficients to obtain derivatives by grid space
!
      module m_grid_fdm_coefs
!
      use m_precision
!
      implicit none
!
!
!>      Coeffeicients for df/dn by 3-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_base_fdm2(-1) *  d_nod(k-1)
!!              + dn_coef_base_fdm2( 0) *  d_nod(k  )
!!              + dn_coef_base_fdm2( 1) *  d_nod(k+1)
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_base_fdm2(-1:1)  &
     &        = (/ -1.0d0/2.0d0, 0.0d0, 1.0d0/2.0d0/)
!
!>      Coeffeicients for d^2f/dn^2 by 3-point stencil FDM
!!@verbatim
!!      dfdn =    d2n_coef_base_fdm2(-1) *  d_nod(k-1)
!!              + d2n_coef_base_fdm2( 0) *  d_nod(k  )
!!              + d2n_coef_base_fdm2( 1) *  d_nod(k+1)
!!@endverbatim
      real(kind = kreal), parameter  :: d2n_coef_base_fdm2(-1:1)  &
     &        = (/1.0d0, -2.0d0, 1.0d0/)
!
!>      Coeffeicients for d^2f/dn^2 at ICB
!!      with fixed df/dn by 3-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_dn_ICB_fdm2(-1) *  dfdn(ICB)
!!              + d2n_coef_fix_dn_ICB_fdm2( 0) *  d_nod(ICB  )
!!              + d2n_coef_fix_dn_ICB_fdm2( 1) *  d_nod(ICB+1)
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_dn_ICB_fdm2(-1:1) &
     &        = (/-2.0d0, -2.0d0, 2.0d0/)
!>      Coeffeicients for d^2f/dn^2 at CMB
!!      with fixed df/dn by 3-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_dn_CMB_fdm2(-1) *  d_nod(CMB-1)
!!              + d2n_coef_fix_dn_CMB_fdm2( 0) *  d_nod(CMB  )
!!              + d2n_coef_fix_dn_CMB_fdm2( 1) *  dfdn(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_dn_CMB_fdm2(-1:1) &
     &        = (/ 2.0d0, -2.0d0, 2.0d0/)
!
!
!>      Coeffeicients for df/dn at ICB
!!      with fixed d^2f/dn^2 by 3-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_d2n_ICB_fdm2(-1) *  d2fdn2(ICB)
!!              + dn_coef_fix_d2n_ICB_fdm2( 0) *  d_nod(ICB  )
!!              + dn_coef_fix_d2n_ICB_fdm2( 1) *  d_nod(ICB+1)
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_d2n_ICB_fdm2(-1:1) &
     &        = (/ -1.0d0/2.0d0, -1.0d0, 1.0d0/)
!>      Coeffeicients for df/dn at CMB
!!      with fixed d^2f/dn^2 by 3-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_d2n_CMB_fdm2(-1) *  d_nod(CMB-1)
!!              + dn_coef_fix_d2n_CMB_fdm2( 0) *  d_nod(CMB  )
!!              + dn_coef_fix_d2n_CMB_fdm2( 1) *  d2fdn2(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_d2n_CMB_fdm2(-1:1) &
     &        = (/-1.0d0,  1.0d0, 1.0d0/2.0d0/)
!
!
!
!>      Coeffeicients for df/dn by 5-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_base_fdm2(-2) *  d_nod(k-2)
!!              + dn_coef_base_fdm2(-1) *  d_nod(k-1)
!!              + dn_coef_base_fdm2( 0) *  d_nod(k  )
!!              + dn_coef_base_fdm2( 1) *  d_nod(k+1)
!!              + dn_coef_base_fdm2( 2) *  d_nod(k+2)
!!@endverbatim
      real(kind = kreal), parameter  :: dn_coef_base_fdm4(-2:2)         &
     &        = (/-1.0d0/12.0d0, -2.0d0/3.0d0, 0.0d0,                   &
     &            2.0d0/3.0d0,  1.0d0/12.0d0/)
!
!>      Coeffeicients for d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      dfdn =    d2n_coef_base_fdm4(-2) *  d_nod(k-2)
!!              + d2n_coef_base_fdm4(-1) *  d_nod(k-1)
!!              + d2n_coef_base_fdm4( 0) *  d_nod(k  )
!!              + d2n_coef_base_fdm4( 1) *  d_nod(k+1)
!!              + d2n_coef_base_fdm4( 2) *  d_nod(k+2)
!!@endverbatim
      real(kind = kreal), parameter  :: d2n_coef_base_fdm4(-2:2)        &
     &        = (/-1.0d0/12.0d0, 4.0d0/3.0d0,-5.0d0/2.0d0,              &
     &            4.0d0/3.0d0, -1.0d0/12.0d0/)
!
!
!
!>      Coeffeicients for d^2f/dn^2 at ICB
!!      with fixed df/dn by 5-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_dn_ICB_fdm4(-1) *  dfdn(ICB)
!!              + d2n_coef_fix_dn_ICB_fdm4( 0) *  d_nod(ICB  )
!!              + d2n_coef_fix_dn_ICB_fdm4( 1) *  d_nod(ICB+1)
!!              + d2n_coef_fix_dn_ICB_fdm4( 2) *  d_nod(ICB+2)
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_dn_ICB_fdm4(-1:2)   &
     &        = (/-3.0d0, -7.0d0/2.0d0, 4.0d0, -1.0d0/2.0d0/)
!
!>      Coeffeicients for d^2f/dn^2 at next of ICB
!!      with fixed df/dn by 5-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_dn_ICB1_fdm4(-2) *  dfdn(ICB)
!!              + d2n_coef_fix_dn_ICB1_fdm4(-1) *  d_nod(ICB  )
!!              + d2n_coef_fix_dn_ICB1_fdm4( 0) *  d_nod(ICB+1)
!!              + d2n_coef_fix_dn_ICB1_fdm4( 1) *  d_nod(ICB+2)
!!              + d2n_coef_fix_dn_ICB1_fdm4( 2) *  d_nod(ICB+3)
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_dn_ICB1_fdm4(-2:2)  &
     &        = (/ 1.0d0/3.0d0,  29.0d0/18.0d0, -3.0d0,                 &
     &            3.0d0/2.0d0, -1.0d0/9.0d0/)
!
!>      Coeffeicients for df/dn at next of ICB
!!      with fixed df/dn by 5-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_dn_ICB1_fdm4(-2) *  dfdn(ICB)
!!              + dn_coef_fix_dn_ICB1_fdm4(-1) *  d_nod(ICB  )
!!              + dn_coef_fix_dn_ICB1_fdm4( 0) *  d_nod(ICB+1)
!!              + dn_coef_fix_dn_ICB1_fdm4( 1) *  d_nod(ICB+2)
!!              + dn_coef_fix_dn_ICB1_fdm4( 2) *  d_nod(ICB+3)
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_dn_ICB1_fdm4(-2:2)   &
     &        = (/-1.0d0/3.0d0, -17.0d0/18.0d0, 1.0d0/2.0d0,            &
     &             1.0d0/2.0d0, -1.0d0/18.0d0/)
!
!>      Coeffeicients for df/dn at next of CMB
!!      with fixed df/dn by 5-point stencil FDM
!!@verbatim
!!      dfdn =    d2n_coef_fix_dn_CMB1_fdm4(-2) *  d_nod(CMB-3)
!!              + d2n_coef_fix_dn_CMB1_fdm4(-1) *  d_nod(CMB-2)
!!              + d2n_coef_fix_dn_CMB1_fdm4( 0) *  d_nod(CMB-1)
!!              + d2n_coef_fix_dn_CMB1_fdm4( 1) *  d_nod(CMB  )
!!              + d2n_coef_fix_dn_CMB1_fdm4( 2) *  dfdn(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_dn_CMB1_fdm4(-2:2)   &
     &        = (/1.0d0/18.0d0, -1.0d0/2.0d0, -1.0d0/2.0d0,             &
     &          17.0d0/18.0d0, -1.0d0/3.0d0/)
!
!>      Coeffeicients for d^2f/dn^2 at next of CMB
!!      with fixed df/dn by 5-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_dn_CMB1_fdm4(-2) *  d_nod(CMB-3)
!!              + d2n_coef_fix_dn_CMB1_fdm4(-1) *  d_nod(CMB-2)
!!              + d2n_coef_fix_dn_CMB1_fdm4( 0) *  d_nod(CMB-1)
!!              + d2n_coef_fix_dn_CMB1_fdm4( 1) *  d_nod(CMB  )
!!              + d2n_coef_fix_dn_CMB1_fdm4( 2) *  dfdn(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_dn_CMB1_fdm4(-2:2)  &
     &        = (/-1.0d0/9.0d0, 3.0d0/2.0d0, -3.0d0,                    &
     &          29.0d0/18.0d0,  -1.0d0/3.0d0/)
!
!>      Coeffeicients for d^2f/dn^2 at CMB
!!      with fixed df/dn by 5-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_dn_CMB_fdm4(-2) *  d_nod(CMB-2)
!!              + d2n_coef_fix_dn_CMB_fdm4(-1) *  d_nod(CMB-1)
!!              + d2n_coef_fix_dn_CMB_fdm4( 0) *  d_nod(CMB  )
!!              + d2n_coef_fix_dn_CMB_fdm4( 1) *  dfdn(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_dn_CMB_fdm4(-2:1)   &
     &        = (/ -1.0d0/2.0d0, 4.0d0, 3.0d0, -7.0d0/2.0d0/)
!
!
!
!>      Coeffeicients for df/dn at ICB
!!      with fixed d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_d2n_ICB_fdm4(-1) *  d2fdn2(ICB)
!!              + dn_coef_fix_d2n_ICB_fdm4( 0) *  d_nod(ICB  )
!!              + dn_coef_fix_d2n_ICB_fdm4( 1) *  d_nod(ICB+1)
!!              + dn_coef_fix_d2n_ICB_fdm4( 2) *  d_nod(ICB+2)
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_d2n_ICB_fdm4(-1:2)   &
     &        = (/ 4.0d0/3.0d0, -7.0d0/6.0d0,                           &
     &            -1.0d0/3.0d0, -1.0d0/6.0d0 /)
!
!>      Coeffeicients for df/dn at next of ICB
!!      with fixed d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_d2n_ICB1_fdm4(-2) *  d2fdn2(ICB)
!!              + dn_coef_fix_d2n_ICB1_fdm4(-1) *  d_nod(ICB  )
!!              + dn_coef_fix_d2n_ICB1_fdm4( 0) *  d_nod(ICB+1)
!!              + dn_coef_fix_d2n_ICB1_fdm4( 1) *  d_nod(ICB+2)
!!              + dn_coef_fix_d2n_ICB1_fdm4( 2) *  d_nod(ICB+3)
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_d2n_ICB1_fdm4(-2:2)  &
     &        = (/ 1.0d0/11.0d0, -17.0d0/33.0d0, -1.0d0/22.0d0,         &
     &             7.0d0/11.0d0, -5.0d0/66.0d0 /)
!
!>      Coeffeicients for d^2f/dn^2 at next of ICB
!!      with fixed d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_d2n_ICB1_fdm4(-2) *  d2fdn2(ICB)
!!              + d2n_coef_fix_d2n_ICB1_fdm4(-1) *  d_nod(ICB  )
!!              + d2n_coef_fix_d2n_ICB1_fdm4( 0) *  d_nod(ICB+1)
!!              + d2n_coef_fix_d2n_ICB1_fdm4( 1) *  d_nod(ICB+2)
!!              + d2n_coef_fix_d2n_ICB1_fdm4( 2) *  d_nod(ICB+3)
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_d2n_ICB1_fdm4(-2:2) &
     &        = (/-1.0d0/11.0d0,  13.0d0/11.0d0, -27.0d0/11.0d0,        &
     &            15.0d0/11.0d0, -1.0d0/11.0d0 /)
!
!>      Coeffeicients for d^2f/dn^2 at next of CMB
!!      with fixed d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      d2fdn2 =  d2n_coef_fix_d2n_CMB1_fdm4(-2) *  d_nod(CMB-3)
!!              + d2n_coef_fix_d2n_CMB1_fdm4(-1) *  d_nod(CMB-2)
!!              + d2n_coef_fix_d2n_CMB1_fdm4( 0) *  d_nod(CMB-1)
!!              + d2n_coef_fix_d2n_CMB1_fdm4( 1) *  d_nod(CMB  )
!!              + d2n_coef_fix_d2n_CMB1_fdm4( 2) *  d2fdn2(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: d2n_coef_fix_d2n_CMB1_fdm4(-2:2) &
     &        = (/-1.0d0/11.0d0, 15.0d0/11.0d0, -27.0d0/11.0d0,         &
     &            13.0d0/11.0d0,  -1.0d0/11.0d0 /)
!
!>      Coeffeicients for df/dn at next of CMB
!!      with fixed d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_d2n_CMB1_fdm4(-2) *  d_nod(CMB-3)
!!              + dn_coef_fix_d2n_CMB1_fdm4(-1) *  d_nod(CMB-2)
!!              + dn_coef_fix_d2n_CMB1_fdm4( 0) *  d_nod(CMB-1)
!!              + dn_coef_fix_d2n_CMB1_fdm4( 1) *  d_nod(CMB  )
!!              + dn_coef_fix_d2n_CMB1_fdm4( 2) *  d2fdn2(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_d2n_CMB1_fdm4(-2:2)  &
     &        = (/ 5.0d0/66.0d0, -7.0d0/11.0d0,  1.0d0/22.0d0,          &
     &            17.0d0/33.0d0, -1.0d0/11.0d0 /)
!
!>      Coeffeicients for df/dn at CMB
!!      with fixed d^2f/dn^2 by 5-point stencil FDM
!!@verbatim
!!      dfdn =    dn_coef_fix_d2n_CMB_fdm4(-2) *  d_nod(CMB-2)
!!              + dn_coef_fix_d2n_CMB_fdm4(-1) *  d_nod(CMB-1)
!!              + dn_coef_fix_d2n_CMB_fdm4( 0) *  d_nod(CMB  )
!!              + dn_coef_fix_d2n_CMB_fdm4( 1) *  d2fdn2(CMB )
!!@endverbatim
      real(kind = kreal), parameter :: dn_coef_fix_d2n_CMB_fdm4(-2:1)   &
     &        = (/ 1.0d0/6.0d0, 1.0d0/3.0d0,                            &
     &             7.0d0/6.0d0, -4.0d0/3.0d0 /)
!
      end module m_grid_fdm_coefs
