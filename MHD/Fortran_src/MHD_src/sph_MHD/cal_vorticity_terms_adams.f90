!>@file   cal_vorticity_terms_adams.f90
!!@brief  module cal_vorticity_terms_adams
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evoluve the vorticity equation by explicit scheme 
!!
!!@verbatim
!!      subroutine cal_vorticity_eq_adams(ipol_bse, ipol_exp, ipol_dif, &
!!     &          ist, ied, dt, coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_vorticity_eq_euler(ipol_bse, ipol_exp, ipol_dif, &
!!     &          ist, ied, dt, coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!!        type(base_field_address), intent(in) :: ipol_bse
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(diffusion_address), intent(in) :: ipol_dif
!!
!!      subroutine set_MHD_terms_to_force                               &
!!     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                   &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine set_rot_cv_terms_to_force                            &
!!     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                   &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!
!!      subroutine set_rot_advection_to_force                           &
!!     &         (ipol_exp, ipol_rot_frc, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_coriolis_to_vort_force                           &
!!     &         (ipol_exp, ipol_rot_frc, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_buoyancy_to_vort_force                           &
!!     &         (ipol_exp, is_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine add_lorentz_to_vort_force                            &
!!     &         (ipol_exp, nnod_rj, ntot_phys_rj, d_rj)
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!
!!      subroutine set_ini_adams_inertia                                &
!!     &         (ipol_exp, nnod_rj, ntot_phys_rj, d_rj)
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!!
!!@n @param is_rot_buo  Spectr field address
!!                       for toroidal curl of buodyancy
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module cal_vorticity_terms_adams
!
      use m_precision
      use m_t_step_parameter
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_adams(ipol_bse, ipol_exp, ipol_dif,   &
     &          ist, ied, dt, coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(base_field_address), intent(in) :: ipol_bse
      type(explicit_term_address), intent(in) :: ipol_exp
      type(diffusion_address), intent(in) :: ipol_dif
      real(kind = kreal), intent(in) :: coef_exp
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_bse%i_vort  ) = d_rj(inod,ipol_bse%i_vort  )     &
     &             + dt * (coef_exp * d_rj(inod,ipol_dif%i_w_diffuse  ) &
     &                     + adam_0 * d_rj(inod,ipol_exp%i_forces  )    &
     &                     + adam_1 * d_rj(inod,ipol_exp%i_pre_mom  ))
        d_rj(inod,ipol_bse%i_vort+2) = d_rj(inod,ipol_bse%i_vort+2)     &
     &             + dt * (coef_exp * d_rj(inod,ipol_dif%i_w_diffuse+2) &
     &                     + adam_0 * d_rj(inod,ipol_exp%i_forces+2)    &
     &                     + adam_1 * d_rj(inod,ipol_exp%i_pre_mom+2))
!
        d_rj(inod,ipol_exp%i_pre_mom  )                                 &
     &        = d_rj(inod,ipol_exp%i_forces  )
        d_rj(inod,ipol_exp%i_pre_mom+2)                                 &
     &        = d_rj(inod,ipol_exp%i_forces+2)
      end do
!$omp end parallel do
!
      end subroutine cal_vorticity_eq_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_euler(ipol_bse, ipol_exp, ipol_dif,   &
     &          ist, ied, dt, coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(base_field_address), intent(in) :: ipol_bse
      type(explicit_term_address), intent(in) :: ipol_exp
      type(diffusion_address), intent(in) :: ipol_dif
      real(kind = kreal), intent(in) :: coef_exp
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_bse%i_vort  ) = d_rj(inod,ipol_bse%i_vort  )     &
     &            + dt * (coef_exp *  d_rj(inod,ipol_dif%i_w_diffuse  ) &
     &                              + d_rj(inod,ipol_exp%i_forces  ))
!
        d_rj(inod,ipol_bse%i_vort+2) = d_rj(inod,ipol_bse%i_vort+2)     &
     &            + dt * (coef_exp *  d_rj(inod,ipol_dif%i_w_diffuse+2) &
     &                              + d_rj(inod,ipol_exp%i_forces+2))
       end do
!$omp end parallel do
!
      end subroutine cal_vorticity_eq_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_MHD_terms_to_force                                 &
     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                     &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: is_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect  )                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis  )                  &
     &          + d_rj(inod,ipol_rot_frc%i_lorentz  )
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+2)                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis+2)                  &
     &          + d_rj(inod,ipol_rot_frc%i_lorentz+2)                   &
     &          + d_rj(inod,is_rot_buo+2)
      end do
!$omp end parallel do
!
      end subroutine set_MHD_terms_to_force
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_cv_terms_to_force                              &
     &         (ipol_exp, ipol_rot_frc, is_rot_buo,                     &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: is_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect  )                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis  )
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+2)                  &
     &          + d_rj(inod,ipol_rot_frc%i_Coriolis+2)                  &
     &          + d_rj(inod,is_rot_buo+2)
      end do
!$omp end parallel do
!
      end subroutine set_rot_cv_terms_to_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_advection_to_force                             &
     &         (ipol_exp, ipol_rot_frc, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect  )
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        = - d_rj(inod,ipol_rot_frc%i_m_advect+2)
      end do
!$omp end do nowait
!
      end subroutine set_rot_advection_to_force
!
! ----------------------------------------------------------------------
!
      subroutine add_coriolis_to_vort_force                             &
     &         (ipol_exp, ipol_rot_frc, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        =  d_rj(inod,ipol_exp%i_forces  )                         &
     &         + d_rj(inod,ipol_rot_frc%i_Coriolis  )
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        =  d_rj(inod,ipol_exp%i_forces+2)                         &
     &         + d_rj(inod,ipol_rot_frc%i_Coriolis+2)
      end do
!$omp end do nowait
!
      end subroutine add_coriolis_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_buoyancy_to_vort_force                             &
     &         (ipol_exp, is_rot_buo, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      integer(kind = kint), intent(in) :: is_rot_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        =  d_rj(inod,ipol_exp%i_forces+2)                         &
     &         + d_rj(inod,is_rot_buo+2)
       end do
!$omp end do nowait
!
      end subroutine add_buoyancy_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_lorentz_to_vort_force                              &
     &         (ipol_exp, ipol_rot_frc, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        =  d_rj(inod,ipol_exp%i_forces  )                         &
     &         + d_rj(inod,ipol_rot_frc%i_lorentz  )
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        =  d_rj(inod,ipol_exp%i_forces+2)                         &
     &         + d_rj(inod,ipol_rot_frc%i_lorentz+2)
       end do
!$omp end do nowait
!
      end subroutine add_lorentz_to_vort_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_inertia                                  &
     &         (ipol_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_pre_mom  )                                 &
     &        = d_rj(inod,ipol_exp%i_forces  )
        d_rj(inod,ipol_exp%i_pre_mom+2)                                 &
     &        = d_rj(inod,ipol_exp%i_forces+2)
      end do
!$omp end parallel do
!
      end subroutine set_ini_adams_inertia
!
! ----------------------------------------------------------------------
!
      end module cal_vorticity_terms_adams
