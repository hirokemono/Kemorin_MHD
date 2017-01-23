!m_ele_material_property.f90
!     module m_ele_material_property
!
!> @brief coefficients for each element
!
!     Written by H. Matsui
!
!      subroutine init_ele_material_property(numele)
!
      module m_ele_material_property
!
      use m_precision
      use t_material_property
!
      implicit  none
!
!
!>      Strucutre of coefficients for each element
      type(coefs_4_MHD_type), save :: ak_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_ele_material_property(numele)
!
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
!
      integer(kind = kint), intent(in) :: numele
!
!    For thermal
!
      if (evo_temp%iflag_scheme .gt. id_no_evolution) then
        call alloc_temp_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_temp(1:numele) = coef_d_temp
      end if
!
!    For convection
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        call alloc_velo_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_velo(1:numele) = fl_prop1%coef_diffuse
!
        if (iflag_4_gravity .gt. id_turn_OFF                            &
     &     .or. iflag_4_filter_gravity .gt. id_turn_OFF) then
          call alloc_buoyancy_coef_ele(numele, ak_MHD)
          ak_MHD%ak_buo(1:numele) = fl_prop1%coef_buo
        end if
!
        if ( iflag_4_composit_buo .gt. id_turn_OFF) then
          call alloc_comp_buo_coef_ele(numele, ak_MHD)
          ak_MHD%ak_comp_buo(1:numele) = fl_prop1%coef_comp_buo
        end if
      end if
!
!   For Induction
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution                   &
     &    .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call alloc_magne_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_magne(1:numele) = coef_d_magne
      end if
!
!   For dummy scalar
!
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
        call alloc_dscalar_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_composit(1:numele) = coef_d_light
      end if
!
!  check
!
      if (iflag_debug .gt. 0) then
       if ( allocated(ak_MHD%ak_d_velo) ) then
        write(*,*)' coefficient for viscosity:         ',               &
      &            ak_MHD%ak_d_velo(1)
       end if
       if ( allocated(ak_MHD%ak_d_temp) ) then
        write(*,*)' coefficient for thermal diffusion: ',               &
      &            ak_MHD%ak_d_temp(1)
       end if
       if ( allocated(ak_MHD%ak_d_magne) ) then
        write(*,*)' coefficient for magnetic diffusion:',               &
      &            ak_MHD%ak_d_magne(1)
       end if
       if ( allocated(ak_MHD%ak_d_composit) ) then
        write(*,*)' coefficient for chemical diffusion:',               &
      &            ak_MHD%ak_d_composit(1)
       end if
!
       if ( allocated(ak_MHD%ak_buo) ) then
        write(*,*)' coefficient for gravity:          ',                &
      &            ak_MHD%ak_buo(1)
       end if
       if ( allocated(ak_MHD%ak_comp_buo) ) then
        write(*,*)' coefficient for compositional buoyancy: ',          &
     &             ak_MHD%ak_comp_buo(1)
       end if
      end if
!
      end subroutine init_ele_material_property
!
!-----------------------------------------------------------------------
!
      end module m_ele_material_property
