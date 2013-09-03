!copy_field_data_4_dt_check.f90
!     module copy_field_data_4_dt_check
!
      module copy_field_data_4_dt_check
!
!      Written by H. Matsui on Nov., 2009
!
      use m_constants
!
!      subroutine s_copy_field_data_for_dt_check
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_field_data_for_dt_check
!
      use m_control_parameter
      use m_node_phys_address
      use copy_nodal_fields
!
!
      if((iphys%i_chk_mom_2*iphys%i_chk_mom) .gt. izero) then
        call copy_vector_component(iphys%i_chk_mom_2, iphys%i_chk_mom)
      end if
!
      if((iphys%i_chk_press_2*iphys%i_chk_press) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_press_2,                 &
     &      iphys%i_chk_press)
      end if
!
!
      if((iphys%i_chk_uxb_2*iphys%i_chk_uxb) .gt. izero) then
        call copy_vector_component(iphys%i_chk_uxb_2, iphys%i_chk_uxb)
      end if
!
      if((iphys%i_chk_potential_2*iphys%i_chk_potential) .gt. izero)    &
     &      then
        call copy_scalar_component(iphys%i_chk_potential_2,             &
     &      iphys%i_chk_potential)
      end if
!
!
      if((iphys%i_chk_heat_2*iphys%i_chk_heat) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_heat_2,                  &
     &      iphys%i_chk_heat)
      end if
!
      if((iphys%i_chk_composit_2*iphys%i_chk_composit) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_composit_2,              &
     &      iphys%i_chk_composit)
      end if
!
!
!
      if( (iphys%i_chk_mom*iphys%i_velo) .gt. izero) then
        call copy_vector_component(iphys%i_chk_mom, iphys%i_velo)
      end if
!
      if( (iphys%i_chk_press*iphys%i_press) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_press, iphys%i_press)
      end if
!
!
      if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if( (iphys%i_chk_uxb*iphys%i_vecp) .gt. izero) then
          call copy_vector_component(iphys%i_chk_uxb, iphys%i_vecp)
        end if
      else
        if( (iphys%i_chk_uxb*iphys%i_magne) .gt. izero) then
          call copy_vector_component(iphys%i_chk_uxb, iphys%i_magne)
        end if
      end if
!
      if( (iphys%i_chk_potential*iphys%i_mag_p) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_potential,               &
     &      iphys%i_mag_p)
      end if
!
!
      if( (iphys%i_chk_heat*iphys%i_temp) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_heat, iphys%i_temp)
      end if
!
      if( (iphys%i_chk_composit*iphys%i_light) .gt. izero) then
        call copy_scalar_component(iphys%i_chk_composit, iphys%i_light)
      end if
!
      end subroutine s_copy_field_data_for_dt_check
!
! ----------------------------------------------------------------------
!
      end module copy_field_data_4_dt_check
