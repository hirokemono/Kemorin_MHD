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
!!     &          (reft_ctl, refc_ctl, mevo_ctl, evo_ctl, nmtr_ctl)
!!     subroutine s_set_control_4_crank(mevo_ctl)
!!        type(reference_temperature_ctl), intent(in) :: reft_ctl
!!        type(reference_temperature_ctl), intent(in) :: refc_ctl
!!        type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!!        type(mhd_evolution_control), intent(inout) :: evo_ctl
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!@endverbatim
!
      module set_control_4_model
!
      use m_precision
      use m_error_IDs
!
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
      use m_t_int_parameter
      use t_ctl_data_mhd_evo_scheme
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_model                                  &
     &          (reft_ctl, refc_ctl, mevo_ctl, evo_ctl, nmtr_ctl)
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
          if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,               &
     &                    'explicit_Euler')) then
            iflag_scheme = id_explicit_euler
            iflag_implicit_correct = 0
          else if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,          &
     &                         '2nd_Adams_Bashforth')) then
            iflag_scheme = id_explicit_adams2
            iflag_implicit_correct = 0
          else if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,          &
     &                         'Crank_Nicolson')) then
            iflag_scheme = id_Crank_nicolson
          else if (cmp_no_case(mevo_ctl%scheme_ctl%charavalue,          &
     &                         'Crank_Nicolson_consist')) then
            iflag_scheme = id_Crank_nicolson_cmass
          end if
        end if
!
        if ( iflag_scheme .eq. id_Crank_nicolson                        &
     &     .or. iflag_scheme .eq. id_Crank_nicolson_cmass) then
          if (mevo_ctl%diffuse_correct%iflag .eq. 0) then
            iflag_implicit_correct = 0
          else
            if (yes_flag(mevo_ctl%diffuse_correct%charavalue)) then
              iflag_implicit_correct = iflag_scheme
            end if
          end if
        end if
!
!   set control for time evolution
!
        if (evo_ctl%t_evo_field_ctl%icou .eq. 0) then
          e_message = 'Set field for time integration'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          num_field_to_evolve = evo_ctl%t_evo_field_ctl%num
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &    write(*,*) 'num_field_to_evolve ',num_field_to_evolve
        end if
!
        if ( num_field_to_evolve .ne. 0 ) then
          allocate( t_evo_name(num_field_to_evolve) )
!
          do i = 1, num_field_to_evolve
            t_evo_name(i)  = evo_ctl%t_evo_field_ctl%c_tbl(i)
          end do
!
          call dealloc_t_evo_name_ctl(evo_ctl)
!
          if (iflag_debug .ge. iflag_routine_msg) then
            write(*,*) 'num_field_to_evolve ',num_field_to_evolve
            do i = 1, num_field_to_evolve
              write(*,*) i, trim(t_evo_name(i))
            end do
          end if
!
         do i = 1, num_field_to_evolve
           if ( t_evo_name(i) .eq. fhd_velo ) then
            evo_velo%iflag_scheme =   iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_temp ) then
            evo_temp%iflag_scheme =   iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_light ) then
            evo_comp%iflag_scheme =   iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_magne ) then
            evo_magne%iflag_scheme =  iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_vecp ) then
            evo_vect_p%iflag_scheme = iflag_scheme
           end if
         end do
!
        end if
!
      if       (evo_velo%iflag_scheme     .eq. id_no_evolution          &
     &    .and. evo_temp%iflag_scheme     .eq. id_no_evolution          &
     &    .and. evo_comp%iflag_scheme     .eq. id_no_evolution          &
     &    .and. evo_magne%iflag_scheme    .eq. id_no_evolution          &
     &    .and. evo_vect_p%iflag_scheme   .eq. id_no_evolution) then
            e_message = 'Turn on field for time integration'
        call calypso_MPI_abort(ierr_evo, e_message)
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_t_evo_4_velo     ', evo_velo%iflag_scheme
        write(*,*) 'iflag_t_evo_4_temp     ', evo_temp%iflag_scheme
        write(*,*) 'iflag_t_evo_4_composit ', evo_comp%iflag_scheme
        write(*,*) 'iflag_t_evo_4_magne    ', evo_magne%iflag_scheme
        write(*,*) 'iflag_t_evo_4_vect_p   ', evo_vect_p%iflag_scheme
        write(*,*) 'iflag_implicit_correct ', iflag_implicit_correct
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
      subroutine s_set_control_4_crank(mevo_ctl)
!
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!
!
      call set_implicit_coefs(mevo_ctl%coef_imp_v_ctl, evo_velo)
      call set_implicit_coefs(mevo_ctl%coef_imp_t_ctl, evo_temp)
      call set_implicit_coefs(mevo_ctl%coef_imp_b_ctl, evo_magne)
      call set_implicit_coefs(mevo_ctl%coef_imp_b_ctl, evo_vect_p)
      call set_implicit_coefs(mevo_ctl%coef_imp_c_ctl, evo_comp)
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'coef_imp_v ', evo_velo%coef_imp
        write(*,*) 'coef_imp_t ', evo_temp%coef_imp
        write(*,*) 'coef_imp_b ', evo_magne%coef_imp
        write(*,*) 'coef_imp_a ', evo_vect_p%coef_imp
        write(*,*) 'coef_imp_c ', evo_comp%coef_imp
      end if
!
      end subroutine s_set_control_4_crank
!
! -----------------------------------------------------------------------
!
      end module set_control_4_model
