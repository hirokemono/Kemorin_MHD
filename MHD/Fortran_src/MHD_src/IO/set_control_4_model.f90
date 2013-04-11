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
!!     subroutine s_set_control_4_model
!!     subroutine s_set_control_4_crank
!!@endverbatim
!
      module set_control_4_model
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_mhd_evo_scheme
      use m_t_int_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_model
!
      use m_parallel_var_dof
      use m_t_step_parameter
      use m_phys_labels
      use m_physical_property
      use m_ctl_data_mhd_evolution
      use m_ctl_data_temp_model
      use m_ctl_data_node_monitor
      use node_monitor_IO
!
      integer (kind = kint) :: i
!
!
!  set time_evolution scheme
!
        if (i_scheme .eq. 0) then
          e_message = 'Set time integration scheme'
          call parallel_abort(90, e_message)
        else
          if ( scheme_ctl .eq. 'explicit_Euler' ) then
            iflag_scheme = 1
            iflag_implicit_correct = 0
          else if ( scheme_ctl .eq. '2nd_Adams_Bashforth' ) then
            iflag_scheme = 2
            iflag_implicit_correct = 0
          else if ( scheme_ctl .eq. 'Crank_Nicolson' ) then
            iflag_scheme = 3
          else if ( scheme_ctl .eq. 'Crank_Nicolson_consist' ) then
            iflag_scheme = 4
          end if
        end if
!
        if ( iflag_scheme.eq.3 .or. iflag_scheme.eq.4) then
          if (i_diff_correct.eq.0) then
            iflag_implicit_correct = 0
          else
            if (   diffuse_correct_ctl .eq. 'On'                        &
     &        .or. diffuse_correct_ctl .eq. 'on'                        &
     &        .or. diffuse_correct_ctl .eq. 'ON') then
              iflag_implicit_correct = iflag_scheme
            end if
          end if
        end if
!
!   set control for time evolution
!
        if (i_num_time_evo .eq. 0) then
          e_message = 'Set field for time integration'
          call parallel_abort(91, e_message)
        else
          num_t_evo_control = num_t_evo_control_ctl
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &    write(*,*) 'num_t_evo_control ',num_t_evo_control
        end if
!
        if ( num_t_evo_control .ne. 0 ) then
          allocate( t_evo_name(num_t_evo_control) )
!
          t_evo_name  = t_evo_name_ctl
!
          call dealloc_t_evo_name_ctl
!
          if (iflag_debug .ge. iflag_routine_msg) then
            write(*,*) 'num_t_evo_control ',num_t_evo_control
            do i = 1, num_t_evo_control
              write(*,*) i, trim(t_evo_name(i))
            end do
          end if
!
         do i = 1, num_t_evo_control
           if ( t_evo_name(i) .eq. fhd_velo ) then
            iflag_t_evo_4_velo = iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_temp ) then
            iflag_t_evo_4_temp = iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_light ) then
            iflag_t_evo_4_composit = iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_magne ) then
            iflag_t_evo_4_magne = iflag_scheme
           else if ( t_evo_name(i) .eq. fhd_vecp ) then
            iflag_t_evo_4_vect_p = iflag_scheme
           end if
         end do
!
        end if
!
      if       (iflag_t_evo_4_velo.eq.0                                 &
     &    .and. iflag_t_evo_4_temp.eq.0                                 &
     &    .and. iflag_t_evo_4_composit.eq.0                             &
     &    .and. iflag_t_evo_4_magne.eq.0                                &
     &    .and. iflag_t_evo_4_vect_p.eq.0) then
            e_message = 'Turn on field for time integration'
        call parallel_abort(90, e_message)
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_t_evo_4_velo ',iflag_t_evo_4_velo
        write(*,*) 'iflag_t_evo_4_temp ',iflag_t_evo_4_temp
        write(*,*) 'iflag_t_evo_4_composit ',iflag_t_evo_4_composit
        write(*,*) 'iflag_t_evo_4_magne ',iflag_t_evo_4_magne
        write(*,*) 'iflag_t_evo_4_vect_p ',iflag_t_evo_4_vect_p
        write(*,*) 'iflag_implicit_correct ',iflag_implicit_correct
      end if
!
!   set control for temperature 
!
         if (i_ref_temp.eq.0) then
           iflag_4_ref_temp = 0
         else
           if (ref_temp_ctl .eq. 'spherical_shell') then
             iflag_4_ref_temp = 100
           else if (ref_temp_ctl .eq. 'sph_constant_heat') then
             iflag_4_ref_temp = 200
           else if (ref_temp_ctl .eq. 'linear_x') then
             iflag_4_ref_temp = 1
           else if (ref_temp_ctl .eq. 'linear_y') then
             iflag_4_ref_temp = 2
           else if (ref_temp_ctl .eq. 'linear_z') then
             iflag_4_ref_temp = 3
           end if
         end if
!
         if ( (i_low_temp_posi*i_low_temp_value) .eq. 0) then
           if (iflag_4_ref_temp .eq. 0) then
             low_temp  = 0.0d0
             depth_low_t  =  0.0d0
           else
              e_message                                                 &
     &          = 'Set lower temperature and its position'
             call parallel_abort(90, e_message)
           end if
         else
           low_temp  = low_temp_ctl
           depth_low_t  = depth_low_t_ctl
         end if
!
         if ( (i_high_temp_posi*i_high_temp_value) .eq. 0) then
           if (iflag_4_ref_temp .eq. 0) then
             high_temp =  0.0d0
             depth_high_t =  0.0d0
           else
              e_message                                                 &
     &          = 'Set lower temperature and its position'
             call parallel_abort(90, e_message)
           end if
         else
           high_temp = high_temp_ctl
           depth_high_t = depth_high_t_ctl
         end if
!
        if (iflag_debug .ge. iflag_routine_msg) then
           write(*,*) 'iflag_4_ref_temp ',iflag_4_ref_temp
           write(*,*) 'low_temp ',low_temp
           write(*,*) 'high_temp ',high_temp
           write(*,*) 'depth_low_t ',depth_low_t
           write(*,*) 'depth_high_t ',depth_high_t
        end if
!
!
!
        iflag_straficate = 0
        if (i_strat_ctl .gt. 0) then
          if(stratified_ctl .eq. 'on' .or. stratified_ctl .eq. 'On'     &
     &    .or. stratified_ctl .eq. 'ON' .or. stratified_ctl .eq. '1')   &
     &     iflag_straficate = 1
        end if
!
        if (iflag_straficate.eq.0) then
          stratified_sigma = 0.0d0
          stratified_width = 0.0d0
          stratified_outer_r = 0.0d0
        else if (iflag_straficate.gt.0) then
          if ( (i_strat_sigma*i_strat_width*i_strat_outer) .eq. 0) then
            e_message                                                   &
     &        = 'Set parameteres for stratification'
            call parallel_abort(90, e_message)
          else
            stratified_sigma = stratified_sigma_ctl
            stratified_width = stratified_width_ctl
            stratified_outer_r = stratified_outer_r_ctl
          end if
        end if
!
        if (iflag_debug .ge. iflag_routine_msg) then
           write(*,*) 'iflag_straficate ',iflag_straficate
           write(*,*) 'stratified_sigma ',stratified_sigma
           write(*,*) 'stratified_width ',stratified_width
           write(*,*) 'stratified_outer_r ',stratified_outer_r
        end if
!
        if (i_monitor_grp.eq.0) then
          num_monitor = 0
        else
          num_monitor = num_monitor_ctl
        end if
!
      if (num_monitor .ne. 0) then
        call allocate_monitor_group
!
        do i = 1, num_monitor
          monitor_grp(i) = monitor_grp_ctl(i)
        end do
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
!
      subroutine s_set_control_4_crank
!
!
        if(iflag_t_evo_4_velo.eq.3 .or. iflag_t_evo_4_velo.eq.4) then
          if (i_coef_imp_v.eq.0) then
            coef_imp_v = 0.5d0
          else
            coef_imp_v = coef_imp_v_ctl
          end if
        else
          coef_imp_v = 0.0d0
        end if
!
        if(iflag_t_evo_4_temp.eq.3 .or. iflag_t_evo_4_temp.eq.4) then
          if (i_coef_imp_t.eq.0) then
            coef_imp_t = 0.5d0
          else
            coef_imp_t = coef_imp_t_ctl
          end if
        else
          coef_imp_t = 0.0d0
        end if
!
        if(iflag_t_evo_4_magne.eq.3 .or. iflag_t_evo_4_magne.eq.4       &
     &   .or. iflag_t_evo_4_vect_p.eq.3 .or. iflag_t_evo_4_vect_p.eq.4) &
     &   then
          if (i_coef_imp_b.eq.0) then
            coef_imp_b = 0.5d0
          else
            coef_imp_b = coef_imp_b_ctl
          end if
        else
          coef_imp_b = 0.0d0
        end if
!
        if(iflag_t_evo_4_composit.eq.3                                  &
     &    .or. iflag_t_evo_4_composit.eq.4) then
          if (i_coef_imp_c.eq.0) then
            coef_imp_c = 0.5d0
          else
            coef_imp_c = coef_imp_c_ctl
          end if
        else
          coef_imp_c = 0.0d0
        end if
!
        coef_exp_v = 1.0d0 - coef_imp_v
        coef_exp_t = 1.0d0 - coef_imp_t
        coef_exp_b = 1.0d0 - coef_imp_b
        coef_exp_c = 1.0d0 - coef_imp_c
!
!
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'coef_imp_v ',coef_imp_v
          write(*,*) 'coef_imp_t ',coef_imp_t
          write(*,*) 'coef_imp_b ',coef_imp_b
          write(*,*) 'coef_imp_c ',coef_imp_c
        end if
!
      end subroutine s_set_control_4_crank
!
! -----------------------------------------------------------------------
!
      end module set_control_4_model
