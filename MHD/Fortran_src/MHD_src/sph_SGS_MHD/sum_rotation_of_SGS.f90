!>@file   sum_rotation_of_SGS.f90
!!@brief  module sum_rotation_of_SGS
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine SGS_forces_to_explicit(SGS_param, sph_rj,            &
!!     &                                  ipol_exp, SGS_term, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(SGS_term_address), intent(in) :: SGS_term
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module sum_rotation_of_SGS
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_spheric_parameter
      use t_phys_data
      use t_explicit_term_labels
      use t_SGS_term_labels
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine SGS_forces_to_explicit(SGS_param, sph_rj,              &
     &                                  ipol_exp, SGS_term, rj_fld)
!
      use t_SGS_control_parameter
      use cal_vorticity_terms_adams
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(explicit_term_address), intent(in) :: ipol_exp
      type(SGS_term_address), intent(in) :: SGS_term
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(SGS_param%iflag_SGS .eq. id_SGS_none) return
!$omp parallel
      if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_turn_OFF) then
        call subtract_advection_to_force                                &
     &     (ipol_exp%i_forces, SGS_term%i_SGS_inertia,                  &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(SGS_param%iflag_SGS_lorentz  .ne. id_turn_OFF) then
        call add_each_force_to_forces                                   &
     &    (ipol_exp%i_forces, SGS_term%i_SGS_Lorentz,                   &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine SGS_forces_to_explicit
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_SGS_MHD_terms_to_force(ipol_exp, ipol_rot_SGS,     &
     &          ist, ied, nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(SGS_term_address), intent(in) :: ipol_rot_SGS
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      integer(kind = kint), intent(in) :: ist, ied
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol_exp%i_forces  )                                  &
     &        =  d_rj(inod,ipol_exp%i_forces  )                         &
     &         - d_rj(inod,ipol_rot_SGS%i_SGS_inertia  )                &
     &         + d_rj(inod,ipol_rot_SGS%i_SGS_Lorentz  )
        d_rj(inod,ipol_exp%i_forces+1)                                  &
     &        =  d_rj(inod,ipol_exp%i_forces+1)                         &
     &         - d_rj(inod,ipol_rot_SGS%i_SGS_inertia+1)                &
     &         + d_rj(inod,ipol_rot_SGS%i_SGS_Lorentz+1)
        d_rj(inod,ipol_exp%i_forces+2)                                  &
     &        =  d_rj(inod,ipol_exp%i_forces+2)                         &
     &         - d_rj(inod,ipol_rot_SGS%i_SGS_inertia+2)                &
     &         + d_rj(inod,ipol_rot_SGS%i_SGS_Lorentz+2)
      end do
!$omp end do nowait
!
      end subroutine add_SGS_MHD_terms_to_force
!
! ----------------------------------------------------------------------
!
      end module sum_rotation_of_SGS
