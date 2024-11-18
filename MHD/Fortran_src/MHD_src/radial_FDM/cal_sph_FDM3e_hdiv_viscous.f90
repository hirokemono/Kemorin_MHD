!>@file   cal_sph_FDM3e_hdiv_viscous.f90
!!@brief  module cal_sph_FDM3e_hdiv_viscous
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine set_sph_FDM_fix_hdiv_viscs_mat(kr, n_in, n_out,      &
!!     &          jmax, a2r_ele_rj, a3r_ele_rj, g_sph_rj,               &
!!     &          nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat,    &
!!     &          hdiv_visous_mat)
!!        integer(kind = kint), intent(in) :: n_in, n_out
!!        integer(kind = kint), intent(in) :: kr, nri_fdm
!!        integer(kind = kint), intent(in) :: jmax
!!        real(kind = kreal), intent(in) :: a2r_ele_rj
!!        real(kind = kreal), intent(in) :: a3r_ele_rj
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d3_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!!      subroutine add_sph_FDM_val_hdiv_viscs_mat(kr, n_in, n_out,      &
!!     &         flag_viscous_variation, flag_ref_density_valiation,    &
!!     &         jmax, a1r_ele_rj, a2r_ele_rj, g_sph_rj,                &
!!     &         relative_d, h_nu, h_rho, h_drhodr,                     &
!!     &         nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,     &
!!     &         hdiv_visous_mat)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: n_in, n_out
!!        integer(kind = kint), intent(in) :: kr, nri_fdm
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: a1r_ele_rj
!!        real(kind = kreal), intent(in) :: a2r_ele_rj
!!        real(kind = kreal), intent(in) :: a3r_ele_rj
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
!!        real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_visous_mat(jmax,-n_in:n_out)
!!@endverbatim
!
      module cal_sph_FDM3e_hdiv_viscous
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: add_sph_hdiv_viscous_rho_depend
      private :: add_sph_hdiv_viscous_nu_depend
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_FDM_fix_hdiv_viscs_mat(kr, n_in, n_out,        &
     &          jmax, a2r_ele_rj, a3r_ele_rj, g_sph_rj,                 &
     &          nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat,      &
     &          hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: a3r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d3_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      integer(kind = kint) :: i_next
!
!
      do i_next = n_in, n_out
        hdiv_visous_mat(1:jmax,i_next) =      - fdm3e_d3_mat(kr,i_next) &
     &    +     g_sph_rj(1:jmax,3)*a2r_ele_rj * fdm3e_d1_mat(kr,i_next) &
     &    - two*g_sph_rj(1:jmax,3)*a3r_ele_rj * fdm3e_d0_mat(kr,i_next)
      end do
!
      end subroutine set_sph_FDM_fix_hdiv_viscs_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_FDM_val_hdiv_viscs_mat(kr, n_in, n_out,        &
     &         flag_viscous_variation, flag_ref_density_valiation,      &
     &         jmax, a1r_ele_rj, a2r_ele_rj, g_sph_rj,                  &
     &         relative_d, h_nu, h_rho, h_drhodr,                       &
     &         nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,       &
     &         hdiv_visous_mat)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
      real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      real(kind = kreal) :: mat_tmp(n_in:n_out)
      real(kind = kreal) :: relative_de, h_nu_e
      real(kind = kreal) :: h_rho_e, h_drhodr_e
      integer(kind = kint) :: i_next
!
!
      relative_de = half * (relative_d(1) + relative_d(2))
      h_nu_e =      half * (h_nu(1) +     h_nu(2))
      h_rho_e =     half * (h_rho(1) +    h_rho(2))
      h_drhodr_e =  half * (h_drhodr(1) + h_drhodr(2))
!
      if(flag_ref_density_valiation) then
        call add_sph_hdiv_viscous_rho_depend(kr, n_in, n_out, jmax,     &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho_e, h_drhodr_e,      &
     &      nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,          &
     &      hdiv_visous_mat)
!
        if(flag_viscous_variation) then
          mat_tmp(n_in:n_out)                                           &
     &       = h_nu_e * h_rho_e * fdm3e_d1_mat(kr,-n_in:n_out:1)
          do i_next = n_in, n_out
            hdiv_visous_mat(1:jmax,i_next)                              &
     &        = hdiv_visous_mat(1:jmax,i_next) + mat_tmp(i_next)
          end do
        end if
      end if
      if(flag_viscous_variation) then
        call add_sph_hdiv_viscous_nu_depend(kr, n_in, n_out, jmax,      &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_de, h_nu_e,      &
     &      nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,          &
     &      hdiv_visous_mat)
      end if
!
      end subroutine add_sph_FDM_val_hdiv_viscs_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_sph_hdiv_viscous_rho_depend(kr, n_in, n_out, jmax, &
     &          a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho, h_drhodr,      &
     &          nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,      &
     &          hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: h_rho, h_drhodr
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      integer(kind = kint) :: i_next
      real(kind = kreal) :: c_d1
!
!
      c_d1 = two * a1r_ele_rj * h_rho + h_drhodr
      do i_next =  n_in, n_out
        hdiv_visous_mat(1:jmax,i_next) = hdiv_visous_mat(1:jmax,i_next) &
     &                             + h_rho * fdm3e_d2_mat(kr,i_next)    &
     &                             + c_d1 *  fdm3e_d1_mat(kr,i_next)    &
     &                             - (g_sph_rj(1:jmax,3)*a2r_ele_rj     &
     &                              * h_rho * two / three)              &
     &                                     * fdm3e_d0_mat(kr,i_next)
      end do
!
      end subroutine add_sph_hdiv_viscous_rho_depend
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_hdiv_viscous_nu_depend(kr, n_in, n_out, jmax,  &
     &          a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_d, h_nu,     &
     &          nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,      &
     &          hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: relative_d, h_nu
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      integer(kind = kint) :: i_next
      real(kind = kreal) :: c_d2, c_d1
!
!
      do i_next = n_in, n_out
        c_d2 = - h_nu
        c_d1 = two * a1r_ele_rj * h_nu
        hdiv_visous_mat(1:jmax,i_next) = hdiv_visous_mat(1:jmax,i_next) &
     &                             + c_d2 * fdm3e_d2_mat(kr,i_next)     &
     &                             + c_d1 * fdm3e_d1_mat(kr,i_next)     &
     &                         - g_sph_rj(1:jmax,3) * a2r_ele_rj * h_nu &
     &                                    * fdm3e_d0_mat(kr,i_next)
        hdiv_visous_mat(1:jmax,i_next) = relative_d                     &
     &                             * hdiv_visous_mat(1:jmax,i_next)
      end do
!
      end subroutine add_sph_hdiv_viscous_nu_depend
!
! -----------------------------------------------------------------------
!
      end module cal_sph_FDM3e_hdiv_viscous
