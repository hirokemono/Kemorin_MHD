!>@file   update_after_evolution.f90
!!        module update_after_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_fields
!!      subroutine reset_update_flag
!!
!!      subroutine update_with_velocity
!!      subroutine update_with_temperature
!!      subroutine update_with_vector_potential
!!      subroutine update_with_magnetic_field
!!      subroutine update_with_dummy_scalar
!!@endverbatim
!
      module update_after_evolution
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_int_vol_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_fields
!
      use m_node_phys_address
      use average_on_elements
!
!
      if (iphys%i_velo .ne. 0) call update_with_velocity
!
      if (iphys%i_temp .ne. 0) call update_with_temperature
!
      if (iphys%i_light .ne. 0) call update_with_dummy_scalar
!
      if (iphys%i_vecp .ne. 0) then
        call update_with_vector_potential
      else if (iphys%i_magne.ne.0) then
        call update_with_magnetic_field
      end if
!
      end subroutine update_fields
!
!-----------------------------------------------------------------------
!
      subroutine reset_update_flag
!
      use m_node_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
!
!     reset monitoring flag
!
      iflag_nod_update(1:num_tot_nod_phys) = 0
      iflag_sgs_coefs(1:num_sgs_kinds) =    0
      iflag_diff_coefs(1:num_diff_kinds) =  0
!
      end subroutine reset_update_flag
!
!-----------------------------------------------------------------------
!
      subroutine update_with_velocity
!
      use m_t_step_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_vectors
      use cal_diff_vector_on_ele
      use cal_diff_coef_velo
      use cal_w_filtering_vectors
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
!
      if (iphys_ele%i_velo .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'velocity_on_element'
        call velocity_on_element
      end if
!
       if ( iflag_4_rotate .eq. id_turn_ON                              &
     &      .and. iphys_ele%i_vort .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'vorticity_on_element'
        call vorticity_on_element
       end if
!
!   required field for explicit filtering
!
      if (iphys%i_filter_velo .ne. 0) then
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &      .and. iflag_dynamic.eq.0) then
          iflag2 = 1
        else if (iflag_SGS_model .eq. id_SGS_similarity) then
          iflag2 = 1
        else
          iflag2 = 0
        end if
!
        if (iflag2 .eq. 1) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &      write(*,*) 'cal_filtered_vector', iphys%i_filter_velo
          call cal_filtered_vector(iphys%i_filter_velo, iphys%i_velo)
          iflag_nod_update(iphys%i_filter_velo  ) = 1
          iflag_nod_update(iphys%i_filter_velo+1) = 1
          iflag_nod_update(iphys%i_filter_velo+2) = 1
        end if
      end if
!
      if (iphys%i_wide_fil_velo.ne.0 .and. iflag_dynamic.eq.0) then
        if (iflag_SGS_model.eq.id_SGS_similarity                        &
     &    .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          call cal_w_filtered_vector(iphys%i_wide_fil_velo,             &
     &        iphys%i_filter_velo)
          iflag_nod_update(iphys%i_wide_fil_velo  ) = 1
          iflag_nod_update(iphys%i_wide_fil_velo+1) = 1
          iflag_nod_update(iphys%i_wide_fil_velo+2) = 1
        end if
      end if
!
!    required field for vector potential
!
       if (iflag_SGS_model.eq.id_SGS_NL_grad                            &
     &    .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF               &
     &    .and. iflag_dynamic.eq.0) then
!
         if (iphys_ele%i_filter_velo.ne.0) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_filter_v_on_ele'
           call diff_filter_v_on_ele
         end if
!
         if (iflag_commute_velo .eq. id_SGS_commute_ON                  &
     &         .and. iflag_diff_coefs(iak_diff_v) .eq. 0) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 's_cal_diff_coef_velo'
           call s_cal_diff_coef_velo
         end if
!
       end if
!
!   required field for gradient model
!
       if ( i_dvx .ne. 0) then
         if ( iflag_SGS_model .eq. id_SGS_NL_grad) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_velocity_on_ele'
            call diff_velocity_on_ele
         end if
       end if
!
       end subroutine update_with_velocity
!
!-----------------------------------------------------------------------
!
      subroutine update_with_temperature
!
      use m_t_step_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_scalar_on_ele
      use add_nodal_fields
      use cal_diff_coef_temp
      use cal_w_filtering_scalars
      use copy_nodal_fields
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!
      if (i_step_sgs_coefs .eq. 0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
!
      if (iphys%i_sgs_temp .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg) write(*,*)               &
     &          'iflag_SGS_parterbuation', iflag_SGS_parterbuation
        if(iflag_SGS_parterbuation .eq. 1) then
          call copy_scalar_component(iphys%i_sgs_temp,                  &
     &        iphys%i_par_temp)
        else
          call copy_scalar_component(iphys%i_sgs_temp, iphys%i_temp)
        end if
      end if
!
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
     &            'i_filter_temp', iphys%i_filter_temp
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
                  'iflag_SGS_heat', iflag_SGS_heat
      if (iphys%i_filter_temp .gt. 0) then
        if(iflag_SGS_heat .ne. id_SGS_none) then
!
          if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                 &
     &         .and. iflag_dynamic.eq.0) then
            iflag2 = 1
          else if (iflag_SGS_model .eq. id_SGS_similarity) then
            iflag2 = 1
          else
            iflag2 = 0
          end if
          if (iflag_debug .gt. 0)   write(*,*) 'iflag2', iflag2
!
          if (iflag2 .eq. 1) then
            if (iflag_debug.gt.0) write(*,*) 'cal_filtered_temperature'
            call cal_filtered_scalar(iphys%i_filter_temp,               &
     &          iphys%i_sgs_temp)
            iflag_nod_update(iphys%i_filter_temp) = 1
          end if
!
          if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dynamic.eq.0) then
            if (iflag_debug.gt.0)                                       &
     &        write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
            call cal_w_filtered_scalar(iphys%i_wide_fil_temp,           &
     &          iphys%i_filter_temp)
                iflag_nod_update(iphys%i_wide_fil_temp) = 1
          end if
        end if
!
        if( (iphys%i_filter_buo+iphys%i_f_buo_gen) .gt. 0) then
          if (iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar(iphys%i_filter_temp, iphys%i_temp)
          iflag_nod_update(iphys%i_filter_temp) = 1
        end if
      end if
!
!
!       if ( i_dtx .ne. 0 ) then
!         if (iflag_debug.gt.0) write(*,*) 'diff_temp_on_ele'
!         call diff_temp_on_ele
!       end if
!
       if (iflag_dynamic.eq.0                                           &
     &       .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
!         if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
!           if ( i_dftx .ne. 0) then
!             if (iflag_debug.gt.0) write(*,*) 'diff_filter_t_on_ele'
!             call diff_filter_t_on_ele
!           end if
!         end if
!       end if
!
         if (iflag_commute_temp .eq. id_SGS_commute_ON) then
           if ( iflag_diff_coefs(iak_diff_t) .eq. 0) then
!
             if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
               if (iflag_debug.gt.0)  write(*,*) 's_cal_diff_coef_temp'
               call s_cal_diff_coef_temp
             end if
           end if
!
         end if
       end if
!
       end subroutine update_with_temperature
!
!-----------------------------------------------------------------------
!
      subroutine update_with_vector_potential
!
      use m_t_step_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_vectors
      use cal_rotation_fields
      use cal_diff_vector_on_ele
      use cal_diff_coef_vector_p
      use cal_w_filtering_vectors
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!   set model coefficients for vector potential
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
!
      if (iflag_dynamic.eq.0 .and.  iphys%i_filter_vecp.ne.0            &
     &     .and. iflag_commute_magne .eq. id_SGS_commute_ON) then
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          if (iflag_debug.gt.0) write(*,*) 'cal_filtered_vector_p'
          call cal_filtered_vector(iphys%i_filter_vecp, iphys%i_vecp)
          iflag_nod_update(iphys%i_filter_vecp  ) = 1
          iflag_nod_update(iphys%i_filter_vecp+1) = 1
          iflag_nod_update(iphys%i_filter_vecp+2) = 1
        end if
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &    .and. iflag_SGS_model.eq.id_SGS_similarity                    &
     &    .and. iphys%i_wide_fil_vecp.ne. 0) then
          if (iflag_debug.gt.0)                                         &
     &         write(*,*) 'cal_filtered_vector_p i_wide_fil_vecp'
          call cal_w_filtered_vector(iphys%i_wide_fil_vecp,             &
     &        iphys%i_filter_vecp)
          iflag_nod_update(iphys%i_wide_fil_vecp  ) = 1
          iflag_nod_update(iphys%i_wide_fil_vecp+1) = 1
          iflag_nod_update(iphys%i_wide_fil_vecp+2) = 1
        end if
!
!
        if ( iflag_diff_coefs(iak_diff_b) .eq. 0) then
          if(iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
            if (iflag_SGS_model .eq. id_SGS_NL_grad) then
              if(iflag_debug.gt.0)                                      &
     &                   write(*,*) 's_cal_diff_coef_vector_p'
              call s_cal_diff_coef_vector_p
!
            else if (iflag_SGS_model .eq. id_SGS_similarity) then
              if(iflag_debug.gt.0)                                      &
     &                   write(*,*) 's_cal_diff_coef_vector_p'
              call s_cal_diff_coef_vector_p
            end if
!
          end if
        end if
      end if
!
!   lead magnetic field
!
       if (iphys%i_magne .ne. 0) then
         if (iflag_debug.gt.0) write(*,*) 'cal_magnetic_f_by_vect_p'
         call cal_magnetic_f_by_vect_p
       end if
       if (iphys_ele%i_magne .ne. 0) then
         if (iflag_debug.gt.0) write(*,*) 'rot_magne_on_element'
         call rot_magne_on_element
       end if
!
       if (iphys_ele%i_current .ne. 0                                   &
     &     .and. iflag_4_rotate .eq. id_turn_ON) then
         if (iflag_debug.gt.0) write(*,*) 'current_on_element'
         call current_on_element
       end if
!
!   required field for explicit filtering
!
       if(iflag_dynamic.eq.0                                            &
     &      .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
         if (        iflag_SGS_lorentz .eq. id_SGS_similarity           &
     &       .or.  iflag_SGS_induction .eq. id_SGS_similarity) then
           iflag2 = 3
         else if (   iflag_SGS_lorentz .eq. id_SGS_NL_grad              &
     &       .or.  iflag_SGS_induction .eq. id_SGS_NL_grad) then
           iflag2 = 2
         else
           iflag2 = 2
         end if
       else
         if (        iflag_SGS_lorentz .eq. id_SGS_similarity           &
     &       .or.  iflag_SGS_induction .eq. id_SGS_similarity) then
           iflag2 = 1
         else
           iflag2 = 0
         end if
       end if
!
       if (iflag_debug .ge. iflag_routine_msg) write(*,*)               &
         'flag for magnetic field filtering', iflag2
!
       if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
         if (iphys%i_filter_magne .ne. 0) then
           if (iflag_debug.gt.0)                                        &
     &        write(*,*) 'cal_filtered_vector', iphys%i_filter_magne
           call cal_filtered_vector(iphys%i_filter_magne,               &
     &         iphys%i_magne)
           iflag_nod_update(iphys%i_filter_magne  ) = 1
           iflag_nod_update(iphys%i_filter_magne+1) = 1
           iflag_nod_update(iphys%i_filter_magne+2) = 1
         end if
         call time_prog_barrier
!
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &         'filtered_magne_on_ele', iphys_ele%i_filter_magne
         if (iphys_ele%i_filter_magne .ne. 0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'filtered_magne_on_ele'
           call filtered_magne_on_ele
         end if
!
         if(iflag2.eq.2 .and. i_dfbx.ne.0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'diff_filter_b_on_ele'
           call diff_filter_b_on_ele
         end if
!
         if(iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
           call cal_w_filtered_vector(iphys%i_wide_fil_magne,           &
     &         iphys%i_filter_magne)
           iflag_nod_update(iphys%i_wide_fil_magne  ) = 1
           iflag_nod_update(iphys%i_wide_fil_magne+1) = 1
           iflag_nod_update(iphys%i_wide_fil_magne+2) = 1
         end if
!
       end if
!
!   required field for gradient model
!
!
       if (  iflag_SGS_lorentz .eq.   id_SGS_NL_grad                    &
     &  .or. iflag_SGS_induction .eq. id_SGS_NL_grad) then
         if ( i_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
           call diff_magne_on_ele
        end if
       end if
!
       end subroutine update_with_vector_potential
!
!-----------------------------------------------------------------------
!
       subroutine update_with_magnetic_field
!
      use m_t_step_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
      use average_on_elements
!
      use cal_filtering_vectors
      use cal_diff_vector_on_ele
      use cal_diff_coef_magne
      use cal_w_filtering_vectors
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
      if (iphys_ele%i_magne .ne. 0) then
       call magnetic_on_element
      end if
!
!
       if(iflag_dynamic.eq.0                                            &
     &       .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
         if (        iflag_SGS_lorentz .eq. id_SGS_similarity           &
     &       .or.  iflag_SGS_induction .eq. id_SGS_similarity) then
           iflag2 = 3
         else if (   iflag_SGS_lorentz .eq. id_SGS_NL_grad              &
     &       .or.  iflag_SGS_induction .eq. id_SGS_NL_grad) then
           iflag2 = 2
         else
           iflag2 = 2
         end if
       else
         if (        iflag_SGS_lorentz .eq. id_SGS_similarity           &
     &       .or.  iflag_SGS_induction .eq. id_SGS_similarity) then
           iflag2 = 1
         else
           iflag2 = 0
         end if
       end if
!
       if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
         if (iphys%i_filter_magne .ne. 0) then
           if (iflag_debug.gt.0)                                        &
     &         write(*,*) 'cal_filtered_vector', iphys%i_filter_magne
           call cal_filtered_vector(iphys%i_filter_magne,               &
     &         iphys%i_magne)
           iflag_nod_update(iphys%i_filter_magne  ) = 1
           iflag_nod_update(iphys%i_filter_magne+1) = 1
           iflag_nod_update(iphys%i_filter_magne+2) = 1
         end if
!
         if (iflag2.eq.2 .and. iphys_ele%i_filter_magne.ne.0) then
           if (iflag_debug.gt.0) write(*,*) 'filtered_magne_on_ele'
           call filtered_magne_on_ele
         end if
!
         if (iflag2.eq.2 .and. i_dfbx.ne.0) then
           if (iflag_debug.gt.0) write(*,*) 'diff_filter_b_on_ele'
           call diff_filter_b_on_ele
         end if
!
         if (iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
           call cal_w_filtered_vector(iphys%i_wide_fil_magne,           &
     &         iphys%i_filter_magne)
            iflag_nod_update(iphys%i_wide_fil_magne  ) = 1
            iflag_nod_update(iphys%i_wide_fil_magne+1) = 1
            iflag_nod_update(iphys%i_wide_fil_magne+2) = 1
         end if
       end if
!
!
       if(iflag_commute_magne .eq. id_SGS_commute_ON                    &
     &     .and. iflag_diff_coefs(iak_diff_b) .eq. 0) then
         if (iflag2.eq.2) then
           if (iflag_debug.gt.0) write(*,*) 's_cal_diff_coef_magne'
           call s_cal_diff_coef_magne
!
         else if (iflag2.eq.3) then
           if (iflag_debug.gt.0) write(*,*) 's_cal_diff_coef_magne'
           call s_cal_diff_coef_magne
         end if
       end if
 !
 !
       if (  iflag_SGS_lorentz .eq.   id_SGS_NL_grad                    &
     &  .or. iflag_SGS_induction .eq. id_SGS_NL_grad) then
        if ( i_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
            call diff_magne_on_ele
        end if
       end if
!
       if (iphys_ele%i_current .ne. 0                                   &
     &     .and. iflag_4_rotate .eq. id_turn_ON) then
         if (iflag_debug.gt.0)  write(*,*) 'current_on_element'
         call current_on_element
       end if
!
       end subroutine update_with_magnetic_field
!
!-----------------------------------------------------------------------
!
      subroutine update_with_dummy_scalar
!
      use m_t_step_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_scalar_on_ele
      use add_nodal_fields
      use cal_diff_coef_temp
      use cal_w_filtering_scalars
      use copy_nodal_fields
!
      integer (kind = kint) :: iflag_dynamic, iflag2
!
!
      if (i_step_sgs_coefs.eq.0) then
        iflag_dynamic = 1
      else
        iflag_dynamic = mod(i_step_MHD, i_step_sgs_coefs)
      end if
!
      if (iphys%i_sgs_composit .ne. 0) then
        call copy_scalar_component(iphys%i_sgs_composit,                &
     &      iphys%i_light)
      end if
!
      iflag2 = 0
!
      if (iphys%i_filter_comp .ne. 0                                    &
     &       .and. iflag_SGS_heat.ne.id_SGS_none) then
        if (iflag2.eq.1) then
          if (iflag_debug.gt.0)   write(*,*) 'cal_filtered_composition'
          call cal_filtered_scalar(iphys%i_filter_comp,                 &
     &        iphys%i_sgs_composit)
          iflag_nod_update(iphys%i_filter_comp) = 1
        end if
!
!        if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dynamic.eq.0) then
!          if (iflag_debug.gt.0)                                        &
!     &      write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
!          call cal_w_filtered_scalar(iphys%i_wide_fil_temp,            &
!     &        iphys%i_filter_comp)
!              iflag_nod_update(iphys%i_wide_fil_temp) = 1
!        end if
      end if
!
!
!       if (iflag_dynamic.eq.0                                          &
!     &     .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!         if (iflag_commute_composit .eq. id_SGS_commute_ON) then
!           if ( iflag_diff_coefs(iak_diff_t) .eq. 0) then
!
!             if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
!               if (iflag_debug.gt.0)  write(*,*) 's_cal_diff_coef_temp'
!               call s_cal_diff_coef_temp
!             end if
!
!           end if
!         end if
!       end if
!
       end subroutine update_with_dummy_scalar
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
