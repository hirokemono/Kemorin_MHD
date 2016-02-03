!>@file   update_after_evolution.f90
!!        module update_after_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine fields_evolution
!!      subroutine update_fields(layer_tbl)
!!      subroutine reset_update_flag
!!
!!      subroutine fields_evolution_4_FEM_SPH
!!@endverbatim
!
      module update_after_evolution
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_int_vol_data
      use m_nod_comm_table
      use m_finite_element_matrix
!
      use t_layering_ele_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fields_evolution
!
      use m_layering_ele_list
!
      use cal_temperature
      use cal_part_temperature
      use cal_velocity
      use cal_vector_potential
      use cal_magnetic_field
      use cal_light_element
!
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag
!
!     ---- magnetic field update
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_magne_vector_potential'
        call cal_magne_vector_potential
        call update_with_vector_potential(layer_tbl1)
!
      else if ( iflag_t_evo_4_magne .gt. id_no_evolution) then
!
        if (iflag_debug.eq.1) write(*,*) 's_cal_magnetic_field'
        call s_cal_magnetic_field
        call update_with_magnetic_field(layer_tbl1)
      end if
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field
        end if
!
        call update_with_temperature(layer_tbl1)
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_light_element_variation'
        call cal_light_element_variation
!
        call update_with_dummy_scalar(layer_tbl1)
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(layer_tbl1)
        call update_with_velocity(layer_tbl1)
      end if
!
      end subroutine fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_fields(layer_tbl)
!
      use m_node_phys_data
!
      use average_on_elements
      use update_with_scalars
      use update_with_velo
      use update_with_vector_p
      use update_with_magne
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!
      if (iphys%i_velo .ne. 0) call update_with_velocity(layer_tbl)
!
      if (iphys%i_temp .ne. 0) call update_with_temperature(layer_tbl)
!
      if (iphys%i_light .ne. 0) then
        call update_with_dummy_scalar(layer_tbl)
      end if
!
      if (iphys%i_vecp .ne. 0) then
        call update_with_vector_potential(layer_tbl)
      else if (iphys%i_magne.ne.0) then
        call update_with_magnetic_field(layer_tbl)
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
      nod_fld1%iflag_update(1:nod_fld1%ntot_phys) = 0
      iflag_sgs_coefs(1:num_sgs_kinds) =            0
      iflag_diff_coefs(1:num_diff_kinds) =          0
!
      end subroutine reset_update_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evolution_4_FEM_SPH
!
      use m_layering_ele_list
!
      use cal_temperature
      use cal_part_temperature
      use cal_velocity
      use cal_light_element
!
      use update_with_scalars
      use update_with_velo
!
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field
        end if
!
        call update_with_temperature(layer_tbl1)
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_light_element_variation'
        call cal_light_element_variation
!
        call update_with_dummy_scalar(layer_tbl1)
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution(layer_tbl1)
        call update_with_velocity(layer_tbl1)
      end if
!
      end subroutine fields_evolution_4_FEM_SPH
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
