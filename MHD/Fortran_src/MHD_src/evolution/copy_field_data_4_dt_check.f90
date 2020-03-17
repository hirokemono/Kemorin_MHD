!copy_field_data_4_dt_check.f90
!     module copy_field_data_4_dt_check
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine s_copy_field_data_for_dt_check                       &
!!     &         (cd_prop, iphys, nod_fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!
      module copy_field_data_4_dt_check
!
      use m_constants
!
      use t_physical_property
      use t_phys_data
      use t_phys_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_field_data_for_dt_check                         &
     &         (cd_prop, iphys, nod_fld)
!
      use copy_nodal_fields
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if((iphys%i_chk_mom_2*iphys%i_chk_mom) .gt. izero) then
        call copy_vector_component(nod_fld,                             &
     &      iphys%i_chk_mom, iphys%i_chk_mom_2)
      end if
!
      if((iphys%i_chk_press_2*iphys%i_chk_press) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%i_chk_press, iphys%i_chk_press_2)
      end if
!
!
      if((iphys%i_chk_uxb_2*iphys%i_chk_uxb) .gt. izero) then
        call copy_vector_component(nod_fld,                             &
     &      iphys%i_chk_uxb, iphys%i_chk_uxb_2)
      end if
!
      if(iphys%check_fld2%i_pre_phi .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%i_chk_potential, iphys%check_fld2%i_pre_phi)
      end if
!
!
      if((iphys%i_chk_heat_2*iphys%i_chk_heat) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%i_chk_heat, iphys%i_chk_heat_2)
      end if
!
      if((iphys%i_chk_composit_2*iphys%i_chk_composit) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%i_chk_composit, iphys%i_chk_composit_2)
      end if
!
!
!
      if( (iphys%i_chk_mom * iphys%base%i_velo) .gt. izero) then
        call copy_vector_component(nod_fld,                             &
     &      iphys%base%i_velo, iphys%i_chk_mom)
      end if
!
      if( (iphys%i_chk_press * iphys%base%i_press) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_press, iphys%i_chk_press)
      end if
!
!
      if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if( (iphys%i_chk_uxb * iphys%base%i_vecp) .gt. izero) then
          call copy_vector_component(nod_fld,                           &
     &        iphys%base%i_vecp, iphys%i_chk_uxb)
        end if
      else
        if( (iphys%i_chk_uxb * iphys%base%i_magne) .gt. izero) then
          call copy_vector_component(nod_fld,                           &
     &        iphys%base%i_magne, iphys%i_chk_uxb)
        end if
      end if
!
      if( (iphys%i_chk_potential * iphys%base%i_mag_p) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_mag_p, iphys%i_chk_potential)
      end if
!
!
      if( (iphys%i_chk_heat*iphys%base%i_temp) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_temp, iphys%i_chk_heat)
      end if
!
      if((iphys%i_chk_composit * iphys%base%i_light) .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_light, iphys%i_chk_composit)
      end if
!
      end subroutine s_copy_field_data_for_dt_check
!
! ----------------------------------------------------------------------
!
      end module copy_field_data_4_dt_check
