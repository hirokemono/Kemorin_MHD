!>@file   sum_rotation_of_SGS.f90
!!@brief  module sum_rotation_of_SGS
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine SGS_forces_to_explicit                               &
!!     &         (SGS_param, sph_rj, sph_bc_U, ipol, itor, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: ipol, itor
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
      use t_phys_address
      use t_phys_data
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine SGS_forces_to_explicit                                 &
     &         (SGS_param, sph_rj, sph_bc_U, ipol, itor, rj_fld)
!
      use t_SGS_control_parameter
      use t_boundary_data_sph_MHD
      use cal_vorticity_terms_adams
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist, ied
!
!
      ist = (sph_bc_U%kr_in-1)*sph_rj%nidx_rj(2) + 1
      ied =  sph_bc_U%kr_out * sph_rj%nidx_rj(2)
!
!$omp parallel
      if(      SGS_param%iflag_SGS_m_flux  .ne. id_turn_OFF             &
     &   .and. SGS_param%iflag_SGS_lorentz .ne. id_turn_OFF) then
        call add_SGS_MHD_terms_to_force(ipol, itor, ist, ied,           &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(SGS_param%iflag_SGS_m_flux  .ne. id_turn_OFF) then
        call add_SGS_inertia_to_vort_force(ipol, itor, ist, ied,        &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(SGS_param%iflag_SGS_lorentz  .ne. id_turn_OFF) then
        call add_SGS_lorentz_to_vort_force(ipol, itor, ist, ied,        &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine SGS_forces_to_explicit
!
!*   ------------------------------------------------------------------
!
      end module sum_rotation_of_SGS
