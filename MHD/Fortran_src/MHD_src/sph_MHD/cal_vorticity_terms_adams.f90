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
      use t_phys_address
!
      implicit  none
!
!>      Coefficient of terms at current step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_0 =  three / two
!>      Coefficient of terms at previous step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_1 = -one / two
!>      1 / adam_0
      real(kind=kreal), parameter :: adam_r =  two / three
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
      use m_physical_property
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: coef_exp
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nnod_rj, jmax, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_in-1)*jmax + 1
      ied = kr_out * jmax
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_vort) = d_rj(inod,ipol%i_vort)                 &
     &                 + dt * (coef_exp * d_rj(inod,ipol%i_w_diffuse)   &
     &                         + adam_0 * d_rj(inod,ipol%i_forces)      &
     &                         + adam_1 * d_rj(inod,ipol%i_pre_mom))
        d_rj(inod,itor%i_vort) = d_rj(inod,itor%i_vort)                 &
     &                 + dt * (coef_exp * d_rj(inod,itor%i_w_diffuse)   &
     &                         + adam_0 * d_rj(inod,itor%i_forces)      &
     &                         + adam_1 * d_rj(inod,itor%i_pre_mom))
!
        d_rj(inod,ipol%i_pre_mom) = d_rj(inod,ipol%i_forces)
        d_rj(inod,itor%i_pre_mom) = d_rj(inod,itor%i_forces)
      end do
!$omp end do nowait
!
      end subroutine cal_vorticity_eq_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_euler(ipol, itor, kr_in, kr_out,      &
     &          dt, coef_exp, nnod_rj, jmax, ntot_phys_rj, d_rj)
!
      use m_physical_property
!
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: coef_exp
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nnod_rj, jmax, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_in-1)*jmax + 1
      ied = kr_out * jmax
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_vort) = d_rj(inod,ipol%i_vort)                 &
     &               + dt * (coef_exp *  d_rj(inod,ipol%i_w_diffuse)    &
     &                                 + d_rj(inod,ipol%i_forces) )
!
        d_rj(inod,itor%i_vort) = d_rj(inod,itor%i_vort)                 &
     &               + dt * (coef_exp *  d_rj(inod,itor%i_w_diffuse)    &
     &                                 + d_rj(inod,itor%i_forces) )
       end do
!$omp end do
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = - d_rj(inod,ipol%i_rot_inertia)      &
     &                             + d_rj(inod,ipol%i_rot_Coriolis)     &
     &                             + d_rj(inod,ipol%i_rot_Lorentz)
        d_rj(inod,itor%i_forces) = - d_rj(inod,itor%i_rot_inertia)      &
     &                             + d_rj(inod,itor%i_rot_Coriolis)     &
     &                             + d_rj(inod,itor%i_rot_Lorentz)      &
     &                             + d_rj(inod,it_rot_buo)
      end do
!$omp end do nowait
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = - d_rj(inod,ipol%i_rot_inertia)      &
     &                             + d_rj(inod,ipol%i_rot_Coriolis)
        d_rj(inod,itor%i_forces) = - d_rj(inod,itor%i_rot_inertia)      &
     &                             + d_rj(inod,itor%i_rot_Coriolis)     &
     &                             + d_rj(inod,it_rot_buo)
      end do
!$omp end do nowait
!
      end subroutine set_rot_cv_terms_to_force
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = - d_rj(inod,ipol%i_rot_inertia)
        d_rj(inod,itor%i_forces) = - d_rj(inod,itor%i_rot_inertia)
      end do
!$omp end do nowait
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = d_rj(inod,ipol%i_forces)             &
     &                           + d_rj(inod,ipol%i_rot_Coriolis)
        d_rj(inod,itor%i_forces) = d_rj(inod,itor%i_forces)             &
     &                           + d_rj(inod,itor%i_rot_Coriolis)
      end do
!$omp end do nowait
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,itor%i_forces) = d_rj(inod,itor%i_forces)             &
     &                           + d_rj(inod,it_rot_buo)
       end do
!$omp end do nowait
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = d_rj(inod,ipol%i_forces)             &
     &                           + d_rj(inod,ipol%i_rot_Lorentz)
        d_rj(inod,itor%i_forces) = d_rj(inod,itor%i_forces)             &
     &                           + d_rj(inod,itor%i_rot_Lorentz)
       end do
!$omp end do nowait
!
      end subroutine add_lorentz_to_vort_force
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
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_pre_mom) = d_rj(inod,ipol%i_forces)
        d_rj(inod,itor%i_pre_mom) = d_rj(inod,itor%i_forces)
      end do
!$omp end do nowait
!
      end subroutine set_ini_adams_inertia
!
! ----------------------------------------------------------------------
!
      end module cal_vorticity_terms_adams
