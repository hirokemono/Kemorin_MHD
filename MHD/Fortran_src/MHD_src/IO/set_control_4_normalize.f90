!>@file   set_control_4_normalize.f90
!!@brief  module set_control_4_normalize
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set normalizatios for MHD simulation from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_normalize                            &
!!     &        (fl_prop, cd_prop, ht_prop, cp_prop,                    &
!!     &         dless_ctl, eqs_ctl, MHD_coef_list)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(dimless_control), intent(in) :: dless_ctl
!!        type(equations_control), intent(in) :: eqs_ctl
!!        type(coef_parameters_list), intent(inout) :: MHD_coef_list
!!      subroutine set_coefs_4_magnetic_scale                           &
!!     &         (bscale_ctl, MHD_coef_list)
!!        type(magnetic_field_scale_control), intent(in) :: bscale_ctl
!!        type(coef_parameters_list), intent(inout) :: MHD_coef_list
!!@endverbatim
!
      module set_control_4_normalize
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_error_IDs
!
      use t_physical_property
      use t_normalize_parameter
      use t_ctl_data_mhd_normalize
      use t_normalize_parameter
!
      implicit  none
!
      private :: set_dimensionless_numbers
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_normalize                              &
     &        (fl_prop, cd_prop, ht_prop, cp_prop,                      &
     &         dless_ctl, eqs_ctl, MHD_coef_list)
!
      use set_control_4_MHD_coefs
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(dimless_control), intent(in) :: dless_ctl
      type(equations_control), intent(in) :: eqs_ctl
!
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
      integer (kind = kint) :: i
!
!
!   set dimensionless numbers
!
      call set_dimensionless_numbers(dless_ctl, MHD_coef_list)
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'num_dimless ', MHD_coef_list%dimless_list%num
        do i = 1, MHD_coef_list%dimless_list%num
          write(*,*) i, trim(MHD_coef_list%dimless_list%name(i)),       &
     &              ': ', MHD_coef_list%dimless_list%value(i)
        end do
      end if
!
!    set normalization for thermal
!
      if (ht_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_termal%num =    0
        MHD_coef_list%coefs_t_diffuse%num = 0
        MHD_coef_list%coefs_h_source%num =  0
      else
        call set_coefs_4_thermal_eq(eqs_ctl%heat_ctl, MHD_coef_list)
      end if
!
!    set coefficients for momentum equation
!
      if (fl_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_momentum%num =  0
        MHD_coef_list%coefs_pressure%num =  0
        MHD_coef_list%coefs_v_diffuse%num = 0
        MHD_coef_list%coefs_buoyancy%num =  0
        MHD_coef_list%coefs_comp_buo%num =  0
        MHD_coef_list%coefs_Coriolis%num =  0
        MHD_coef_list%coefs_Lorentz%num =   0
      else
        call set_coefs_4_momentum_eq                                    &
     &     (fl_prop, eqs_ctl%mom_ctl, MHD_coef_list)
      end if
!
!
!    coefficients for inducition equation
!
      if     (cd_prop%iflag_Bevo_scheme .eq. id_no_evolution            &
     &  .and. cd_prop%iflag_Aevo_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_magnetic%num =  0
        MHD_coef_list%coefs_magne_p%num =   0
        MHD_coef_list%coefs_m_diffuse%num = 0
        MHD_coef_list%coefs_induction%num = 0
      else
        call set_coefs_4_induction_eq                                   &
     &     (cd_prop, eqs_ctl%induct_ctl, MHD_coef_list)
      end if
!
!    set normalization for composition
!
      if (cp_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_composition%num = 0
        MHD_coef_list%coefs_c_diffuse%num =   0
        MHD_coef_list%coefs_c_source%num =    0
      else
        call set_coefs_4_composition_eq                                 &
     &     (eqs_ctl%comp_ctl, MHD_coef_list)
      end if
!
      end subroutine s_set_control_4_normalize
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_dimensionless_numbers(dless_ctl, MHD_coef_list)
!
      type(dimless_control), intent(in) :: dless_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if (dless_ctl%dimless%icou .eq. 0) then
          e_message =                                                   &
     &     'Set dimensionless numbers'
          call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%dimless_list%num = dless_ctl%dimless%num
      end if
!
      call copy_dimless_from_ctl                                        &
     &   (dless_ctl%dimless, MHD_coef_list%dimless_list)
!
      end subroutine set_dimensionless_numbers
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_magnetic_scale                             &
     &         (bscale_ctl, MHD_coef_list)
!
      use t_ctl_data_mhd_magne
!
      type(magnetic_field_scale_control), intent(in) :: bscale_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if(bscale_ctl%mag_to_kin_energy_ctl%icou .eq. 0) then
        MHD_coef_list%coefs_me_to_ke%num = 0
      else
        MHD_coef_list%coefs_me_to_ke%num                                &
     &            = bscale_ctl%mag_to_kin_energy_ctl%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (bscale_ctl%mag_to_kin_energy_ctl,                             &
     &    MHD_coef_list%coefs_me_to_ke)
!
      end subroutine set_coefs_4_magnetic_scale
!
! -----------------------------------------------------------------------
!
      end module set_control_4_normalize
