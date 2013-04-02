!
!      module calcs_4_crs_CG11
!
!     Written by Kemorin
!
!      subroutine crs_z_plus_beta_p_11(N, NP, WP, WZ, RHO, RHO1)
!C +-----------------------------+
!C | {p} = {z} + BETA*{p}        |
!C +-----------------------------+
!
      module calcs_4_crs_CG11
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine crs_z_plus_beta_p_11(N, NP, WP, WZ, RHO, RHO1)
!
      integer(kind = kint), intent(in) :: N, NP
      real(kind = kreal), intent(in) :: RHO, RHO1
      real(kind = kreal), intent(in) :: WZ(NP)
      real(kind = kreal), intent(inout) :: WP(NP)
!
!
      real(kind = kreal) :: BETA
!
!
      BETA= RHO / RHO1
      WP(1:N)= WZ(1:N) + BETA*WP(1:N)
!
      end subroutine crs_z_plus_beta_p_11
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_crs_CG11
