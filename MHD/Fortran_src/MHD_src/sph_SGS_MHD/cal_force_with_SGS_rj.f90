!> @file  cal_force_with_SGS_rj.f90
!!      module cal_force_with_SGS_rj
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate forces including SGS terms
!!
!!@verbatim
!!      subroutine s_cal_force_with_SGS_rj                              &
!!     &         (ipol_frc, ipol_SGS, ipol_frc_SGS, rj_fld)
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_force_with_SGS_rj
!
      use m_precision
      use m_constants
!
      use t_base_force_labels
      use t_SGS_term_labels
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_force_with_SGS_rj                                &
     &         (ipol_frc, ipol_SGS, ipol_frc_SGS, rj_fld)
!
      use copy_nodal_fields
!
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(SGS_term_address), intent(in) :: ipol_frc_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_frc_SGS%i_SGS_h_flux .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol_frc%i_h_flux, ipol_SGS%i_SGS_h_flux,           &
     &      ipol_frc_SGS%i_SGS_h_flux)
      end if
!
      if(ipol_frc_SGS%i_SGS_c_flux .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol_frc%i_c_flux, ipol_SGS%i_SGS_c_flux,           &
     &      ipol_frc_SGS%i_SGS_c_flux)
      end if
!
      if(ipol_frc_SGS%i_SGS_inertia .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol_frc%i_m_advect, ipol_SGS%i_SGS_inertia,        &
     &      ipol_frc_SGS%i_SGS_inertia)
      end if
!
      if(ipol_frc_SGS%i_SGS_Lorentz .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol_frc%i_lorentz, ipol_SGS%i_SGS_Lorentz,         &
     &      ipol_frc_SGS%i_SGS_Lorentz)
      end if
!
      if(ipol_frc_SGS%i_SGS_vp_induct .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol_frc%i_vp_induct, ipol_SGS%i_SGS_vp_induct,     &
     &      ipol_frc_SGS%i_SGS_vp_induct)
      end if
!
      if(ipol_frc_SGS%i_SGS_induction .gt. 0) then
        call add_2_nod_vectors                                          &
     &     (rj_fld, ipol_frc%i_induction, ipol_SGS%i_SGS_induction,     &
     &      ipol_frc_SGS%i_SGS_induction)
      end if
!
      if(ipol_frc_SGS%i_SGS_m_flux .gt. 0) then
        call add_2_nod_tensors                                          &
     &     (rj_fld, ipol_frc%i_m_flux, ipol_SGS%i_SGS_m_flux,           &
     &      ipol_frc_SGS%i_SGS_m_flux)
      end if
!
      if(ipol_frc_SGS%i_SGS_maxwell .gt. 0) then
        call add_2_nod_tensors                                          &
     &     (rj_fld, ipol_frc%i_maxwell, ipol_SGS%i_SGS_maxwell,         &
     &      ipol_frc_SGS%i_SGS_maxwell)
      end if
!
      if(ipol_frc_SGS%i_SGS_induct_t .gt. 0) then
        call add_2_nod_tensors                                          &
     &     (rj_fld, ipol_frc%i_induct_t, ipol_SGS%i_SGS_induct_t,       &
     &      ipol_frc_SGS%i_SGS_induct_t)
      end if
!
      end subroutine s_cal_force_with_SGS_rj
!
!-----------------------------------------------------------------------
!
      end module cal_force_with_SGS_rj
