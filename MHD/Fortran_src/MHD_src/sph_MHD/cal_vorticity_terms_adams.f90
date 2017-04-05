!>@file   cal_vorticity_terms_adams.f90
!!@brief  module cal_vorticity_terms_adams
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evoluve the vorticity equation by explicit scheme 
!!
!!@verbatim
!!      subroutine cal_vorticity_eq_adams(ipol, itor, kr_in, kr_out,    &
!!     &          dt, coef_exp, nnod_rj, jmax, ntot_phys_rj, d_rj)
!!      subroutine cal_vorticity_eq_euler(ipol, itor, kr_in, kr_out,    &
!!     &          dt, coef_exp, nnod_rj, jmax, ntot_phys_rj, d_rj)
!!
!!      subroutine set_MHD_terms_to_force                               &
!!     &         (ipol, itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine set_rot_cv_terms_to_force                            &
!!     &         (ipol, itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_SGS_MHD_terms_to_force                           &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!
!!      subroutine set_rot_advection_to_force                           &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_coriolis_to_vort_force                           &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_buoyancy_to_vort_force                           &
!!     &         (itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_lorentz_to_vort_force                            &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!
!!      subroutine add_SGS_inertia_to_vort_force                        &
!!     &         (ipol, itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_SGS_lorentz_to_vort_force                        &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!
!!      subroutine set_ini_adams_inertia                                &
!!     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol, itor
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!!
!!@n @param it_rot_buo  Spectr field address
!!                       for toroidal curl of buodyancy
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module cal_vorticity_terms_adams
!
      use m_precision
      use m_t_step_parameter
      use t_phys_address
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_adams(ipol, itor, kr_in, kr_out,      &
     &          dt, coef_exp, nnod_rj, jmax, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: coef_exp
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nnod_rj, jmax, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_in-1)*jmax + 1
      ied = kr_out * jmax
!$omp workshare
        d_rj(ist:ied,ipol%i_vort) = d_rj(ist:ied,ipol%i_vort)           &
     &             + dt * (coef_exp * d_rj(ist:ied,ipol%i_w_diffuse)    &
     &                     + adam_0 * d_rj(ist:ied,ipol%i_forces)       &
     &                     + adam_1 * d_rj(ist:ied,ipol%i_pre_mom))
        d_rj(ist:ied,itor%i_vort) = d_rj(ist:ied,itor%i_vort)           &
     &             + dt * (coef_exp * d_rj(ist:ied,itor%i_w_diffuse)    &
     &                     + adam_0 * d_rj(ist:ied,itor%i_forces)       &
     &                     + adam_1 * d_rj(ist:ied,itor%i_pre_mom))
!
        d_rj(ist:ied,ipol%i_pre_mom) = d_rj(ist:ied,ipol%i_forces)
        d_rj(ist:ied,itor%i_pre_mom) = d_rj(ist:ied,itor%i_forces)
!$omp end workshare
!
      end subroutine cal_vorticity_eq_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_euler(ipol, itor, kr_in, kr_out,      &
     &          dt, coef_exp, nnod_rj, jmax, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: coef_exp
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nnod_rj, jmax, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_in-1)*jmax + 1
      ied = kr_out * jmax
!$omp workshare
        d_rj(ist:ied,ipol%i_vort) = d_rj(ist:ied,ipol%i_vort)           &
     &             + dt * (coef_exp *  d_rj(ist:ied,ipol%i_w_diffuse)   &
     &                               + d_rj(ist:ied,ipol%i_forces) )
!
        d_rj(ist:ied,itor%i_vort) = d_rj(ist:ied,itor%i_vort)           &
     &             + dt * (coef_exp *  d_rj(ist:ied,itor%i_w_diffuse)   &
     &                                 + d_rj(ist:ied,itor%i_forces) )
!$omp end workshare
!
      end subroutine cal_vorticity_eq_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_MHD_terms_to_force                                 &
     &         (ipol, itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: it_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces)                                   &
     &                         = - d_rj(1:nnod_rj,ipol%i_rot_inertia)   &
     &                           + d_rj(1:nnod_rj,ipol%i_rot_Coriolis)  &
     &                           + d_rj(1:nnod_rj,ipol%i_rot_Lorentz)
        d_rj(1:nnod_rj,itor%i_forces)                                   &
     &                         = - d_rj(1:nnod_rj,itor%i_rot_inertia)   &
     &                           + d_rj(1:nnod_rj,itor%i_rot_Coriolis)  &
     &                           + d_rj(1:nnod_rj,itor%i_rot_Lorentz)   &
     &                           + d_rj(1:nnod_rj,it_rot_buo)
!$omp end workshare
!
      end subroutine set_MHD_terms_to_force
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_cv_terms_to_force                              &
     &         (ipol, itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: it_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces)                                   &
     &                          = - d_rj(1:nnod_rj,ipol%i_rot_inertia)  &
     &                            + d_rj(1:nnod_rj,ipol%i_rot_Coriolis)
        d_rj(1:nnod_rj,itor%i_forces)                                   &
     &                          = - d_rj(1:nnod_rj,itor%i_rot_inertia)  &
     &                            + d_rj(1:nnod_rj,itor%i_rot_Coriolis) &
     &                            + d_rj(1:nnod_rj,it_rot_buo)
!$omp end workshare
!
      end subroutine set_rot_cv_terms_to_force
!
! ----------------------------------------------------------------------
!
      subroutine add_SGS_MHD_terms_to_force                             &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces) = d_rj(1:nnod_rj,ipol%i_forces)   &
     &                        - d_rj(1:nnod_rj,ipol%i_SGS_rot_inertia)  &
     &                        + d_rj(1:nnod_rj,ipol%i_SGS_rot_Lorentz)
        d_rj(1:nnod_rj,itor%i_forces) = d_rj(1:nnod_rj,itor%i_forces)   &
     &                        - d_rj(1:nnod_rj,itor%i_SGS_rot_inertia)  &
     &                        + d_rj(1:nnod_rj,itor%i_SGS_rot_Lorentz)
!$omp end workshare
!
      end subroutine add_SGS_MHD_terms_to_force
!
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_advection_to_force                             &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces)                                   &
     &                          = - d_rj(1:nnod_rj,ipol%i_rot_inertia)
        d_rj(1:nnod_rj,itor%i_forces)                                   &
     &                          = - d_rj(1:nnod_rj,itor%i_rot_inertia)
!$omp end workshare
!
      end subroutine set_rot_advection_to_force
!
! ----------------------------------------------------------------------
!
      subroutine add_coriolis_to_vort_force                             &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces) = d_rj(1:nnod_rj,ipol%i_forces)   &
     &                           + d_rj(1:nnod_rj,ipol%i_rot_Coriolis)
        d_rj(1:nnod_rj,itor%i_forces) = d_rj(1:nnod_rj,itor%i_forces)   &
     &                           + d_rj(1:nnod_rj,itor%i_rot_Coriolis)
!$omp end workshare
!
      end subroutine add_coriolis_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_buoyancy_to_vort_force                             &
     &         (itor, it_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: itor
      integer(kind = kint), intent(in) :: it_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,itor%i_forces) = d_rj(1:nnod_rj,itor%i_forces)   &
     &                            + d_rj(1:nnod_rj,it_rot_buo)
!$omp end workshare
!
      end subroutine add_buoyancy_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_lorentz_to_vort_force                              &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces) = d_rj(1:nnod_rj,ipol%i_forces)   &
     &                           + d_rj(1:nnod_rj,ipol%i_rot_Lorentz)
        d_rj(1:nnod_rj,itor%i_forces) = d_rj(1:nnod_rj,itor%i_forces)   &
     &                           + d_rj(1:nnod_rj,itor%i_rot_Lorentz)
!$omp end workshare
!
      end subroutine add_lorentz_to_vort_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_SGS_inertia_to_vort_force                          &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces) = d_rj(1:nnod_rj,ipol%i_forces)   &
     &                        - d_rj(1:nnod_rj,ipol%i_SGS_rot_inertia)
        d_rj(1:nnod_rj,itor%i_forces) = d_rj(1:nnod_rj,itor%i_forces)   &
     &                        - d_rj(1:nnod_rj,itor%i_SGS_rot_inertia)
!$omp end workshare
!
      end subroutine add_SGS_inertia_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_SGS_lorentz_to_vort_force                          &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_forces) = d_rj(1:nnod_rj,ipol%i_forces)   &
     &                     + d_rj(1:nnod_rj,ipol%i_SGS_rot_Lorentz)
        d_rj(1:nnod_rj,itor%i_forces) = d_rj(1:nnod_rj,itor%i_forces)   &
     &                     + d_rj(1:nnod_rj,itor%i_SGS_rot_Lorentz)
!$omp end workshare
!
      end subroutine add_SGS_lorentz_to_vort_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_inertia                                  &
     &         (ipol, itor, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
!$omp workshare
        d_rj(1:nnod_rj,ipol%i_pre_mom) = d_rj(1:nnod_rj,ipol%i_forces)
        d_rj(1:nnod_rj,itor%i_pre_mom) = d_rj(1:nnod_rj,itor%i_forces)
!$omp end workshare
!
      end subroutine set_ini_adams_inertia
!
! ----------------------------------------------------------------------
!
      end module cal_vorticity_terms_adams
