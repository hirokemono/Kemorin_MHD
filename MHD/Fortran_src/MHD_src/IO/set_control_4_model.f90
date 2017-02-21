!>@file   set_control_4_model.f90
!!@brief  module set_control_4_model
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    Mmodified by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set models for MHD simulation from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_model                                &
!!     &          (reft_ctl, refc_ctl, mevo_ctl, evo_ctl, nmtr_ctl,     &
!!     &           evo_V, evo_B, evo_A, evo_T, evo_C)
!!      subroutine s_set_control_4_crank                                &
!!     &         (mevo_ctl, evo_V, evo_B, evo_A, evo_T, evo_C)
!!        type(reference_temperature_ctl), intent(in) :: reft_ctl
!!        type(reference_temperature_ctl), intent(in) :: refc_ctl
!!        type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!!        type(mhd_evolution_control), intent(inout) :: evo_ctl
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!        type(time_evolution_params), intent(inout) :: evo_V, evo_B
!!        type(time_evolution_params), intent(inout) :: evo_A
!!        type(time_evolution_params), intent(inout) :: evo_T, evo_C
!!@endverbatim
!
      module set_control_4_model
!
      use m_precision
      use m_constants
      use m_error_IDs
!
      use m_machine_parameter
      use m_physical_property
      use m_t_int_parameter
      use t_ctl_data_mhd_evo_scheme
      use t_time_stepping_parameter
!
      implicit  none
!
      private :: set_implicit_coefs
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_model                                  &
     &          (reft_ctl, refc_ctl, mevo_ctl, evo_ctl, nmtr_ctl,       &
     &           evo_V, evo_B, evo_A, evo_T, evo_C)
!
      use calypso_mpi
      use m_t_step_parameter
      use m_phys_labels
      use t_ctl_data_mhd_evolution
      use t_ctl_data_temp_model
      use t_ctl_data_node_monitor
      use t_reference_scalar_param
      use node_monitor_IO
!
      type(reference_temperature_ctl), intent(in) :: reft_ctl
      type(reference_temperature_ctl), intent(in) :: refc_ctl
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(mhd_evolution_control), intent(inout) :: evo_ctl
      type(node_monitor_control), intent(inout) :: nmtr_ctl
      type(time_evolution_params), intent(inout) :: evo_V, evo_B, evo_A
      type(time_evolution_params), intent(inout) :: evo_T, evo_C
!
      integer (kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
!  set time_evolution scheme
!
      if (mevo_ctl%scheme_ctl%iflag .eq. 0) then
        e_message = 'Set time integration scheme'
        call calypso_MPI_abort(ierr_evo, e_message)
      else
        if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,                 &
     &                    'explicit_Euler')) then
          iflag_scheme = id_explicit_euler
        else if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,            &
     &                         '2nd_Adams_Bashforth')) then
          iflag_scheme = id_explicit_adams2
        else if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,            &
     &                         'Crank_Nicolson')) then
          iflag_scheme = id_Crank_nicolson
        else if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,            &
     &                         'Crank_Nicolson_consist')) then
          iflag_scheme = id_Crank_nicolson_cmass
        end if
      end if
!
!   set control for time evolution
!
      if (evo_ctl%t_evo_field_ctl%icou .eq. 0) then
        e_message = 'Set field for time integration'
        call calypso_MPI_abort(ierr_evo, e_message)
      end if
!
      do i = 1, evo_ctl%t_evo_field_ctl%num
        tmpchara = evo_ctl%t_evo_field_ctl%c_tbl(i)
        if (tmpchara .eq. fhd_velo ) then
          evo_V%iflag_scheme =   iflag_scheme
        else if (tmpchara .eq. fhd_temp ) then
          evo_T%iflag_scheme =   iflag_scheme
        else if (tmpchara .eq. fhd_light ) then
          evo_C%iflag_scheme =   iflag_scheme
        else if (tmpchara .eq. fhd_magne ) then
          evo_B%iflag_scheme =  iflag_scheme
        else if (tmpchara .eq. fhd_vecp ) then
          evo_A%iflag_scheme = iflag_scheme
        end if
      end do
!
      if (evo_ctl%t_evo_field_ctl%num .gt. 0 ) then
        call dealloc_t_evo_name_ctl(evo_ctl)
      end if
!
      if       (evo_V%iflag_scheme .eq. id_no_evolution                 &
     &    .and. evo_T%iflag_scheme .eq. id_no_evolution                 &
     &    .and. evo_C%iflag_scheme .eq. id_no_evolution                 &
     &    .and. evo_B%iflag_scheme .eq. id_no_evolution                 &
     &    .and. evo_A%iflag_scheme .eq. id_no_evolution) then
            e_message = 'Turn on field for time integration'
        call calypso_MPI_abort(ierr_evo, e_message)
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_t_evo_4_velo     ', evo_V%iflag_scheme
        write(*,*) 'iflag_t_evo_4_temp     ', evo_T%iflag_scheme
        write(*,*) 'iflag_t_evo_4_composit ', evo_C%iflag_scheme
        write(*,*) 'iflag_t_evo_4_magne    ', evo_B%iflag_scheme
        write(*,*) 'iflag_t_evo_4_vect_p   ', evo_A%iflag_scheme
      end if
!
!   set control for reference temperature 
!
      write(tmpchara,'(a)') 'Reference temperature'
      call set_reference_scalar_ctl                                     &
     &   (tmpchara, reft_ctl, ref_param_T1, takepito_T1)
!
!   set control for reference  
!
      write(tmpchara,'(a)') 'Reference temperature'
      call set_reference_scalar_ctl                                     &
     &   (tmpchara, refc_ctl, ref_param_C1, takepito_C1)
!
!
      if (nmtr_ctl%group_4_monitor_ctl%icou .eq. 0) then
        num_monitor = 0
      else
        num_monitor = nmtr_ctl%group_4_monitor_ctl%num
      end if
!
      if (num_monitor .ne. 0) then
        call allocate_monitor_group
!
        do i = 1, num_monitor
          monitor_grp(i) = nmtr_ctl%group_4_monitor_ctl%c_tbl(i)
        end do
        call dealloc_control_array_chara(nmtr_ctl%group_4_monitor_ctl)
!
        if (iflag_debug .ge. iflag_routine_msg) then
          do i = 1, num_monitor
            write(*,*) 'monitor_grp',i,monitor_grp(i)
          end do
        end if
      end if
!
!
      end subroutine s_set_control_4_model
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_crank                                  &
     &         (mevo_ctl, evo_V, evo_B, evo_A, evo_T, evo_C)
!
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(time_evolution_params), intent(inout) :: evo_V, evo_B, evo_A
      type(time_evolution_params), intent(inout) :: evo_T, evo_C
!
!
      call set_implicit_coefs(mevo_ctl%coef_imp_v_ctl,                  &
     &    evo_V%iflag_scheme, fl_prop1%coef_imp, fl_prop1%coef_exp)
      call set_implicit_coefs(mevo_ctl%coef_imp_t_ctl,                  &
     &    evo_T%iflag_scheme, ht_prop1%coef_imp, ht_prop1%coef_exp)
      call set_implicit_coefs(mevo_ctl%coef_imp_b_ctl,                  &
     &    evo_B%iflag_scheme, cd_prop1%coef_imp, cd_prop1%coef_exp)
      call set_implicit_coefs(mevo_ctl%coef_imp_b_ctl,                  &
     &    evo_A%iflag_scheme, cd_prop1%coef_imp, cd_prop1%coef_exp)
      call set_implicit_coefs(mevo_ctl%coef_imp_c_ctl,                  &
     &    evo_C%iflag_scheme, cp_prop1%coef_imp, cp_prop1%coef_exp)
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'coef_imp_v ', fl_prop1%coef_imp
        write(*,*) 'coef_imp_t ', ht_prop1%coef_imp
        write(*,*) 'coef_imp_b ', cd_prop1%coef_imp
        write(*,*) 'coef_imp_c ', cp_prop1%coef_imp
      end if
!
      end subroutine s_set_control_4_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_implicit_coefs                                     &
     &         (coef_imp_ctl, iflag_scheme, coef_imp, coef_exp)
!
      use t_control_elements
!
      type(read_real_item), intent(in) :: coef_imp_ctl
      integer(kind=kint), intent(in)  :: iflag_scheme
      real(kind = kreal), intent(inout) :: coef_imp, coef_exp
!
!
      if(iflag_scheme .ge. id_Crank_nicolson) then
        if (coef_imp_ctl%iflag .eq. 0) then
          coef_imp = half
        else
          coef_imp = coef_imp_ctl%realvalue
        end if
      else
        coef_imp = zero
      end if
      coef_exp = one - coef_imp
!
      end subroutine set_implicit_coefs
!
! -----------------------------------------------------------------------
!
!
      end module set_control_4_model
