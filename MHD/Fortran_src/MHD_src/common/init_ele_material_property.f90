!init_ele_material_property.f90
!     module init_ele_material_property
!
!> @brief coefficients for each element
!
!     Written by H. Matsui
!
!!      subroutine s_init_ele_material_property                         &
!!     &         (numele, MHD_prop, ak_MHD)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
      module init_ele_material_property
!
      use m_precision
      use m_machine_parameter
      use t_control_parameter
      use t_material_property
      use t_physical_property
!
      implicit  none
!
      private :: ele_viscous_diffusion, ele_magnetic_diffusion
      private :: ele_thermal_diffusion, ele_compositional_diffusion
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_init_ele_material_property                           &
     &         (numele, MHD_prop, ak_MHD)
!
!
      integer(kind = kint), intent(in) :: numele
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
!
!    For thermal
      call ele_thermal_diffusion(numele, MHD_prop%ht_prop, ak_MHD)
!
!    For convection
      call ele_viscous_diffusion(numele, MHD_prop%fl_prop, ak_MHD)
!
!   For Induction
      call ele_magnetic_diffusion(numele, MHD_prop%cd_prop, ak_MHD)
!
!   For dummy scalar
      call ele_compositional_diffusion                                  &
     &   (numele, MHD_prop%cp_prop, ak_MHD)
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
      end subroutine s_init_ele_material_property
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine ele_viscous_diffusion(numele, fl_prop, ak_MHD)
!
      integer(kind = kint), intent(in) :: numele
      type(fluid_property), intent(in) :: fl_prop
!
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_velo_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_velo(1:numele) = fl_prop%coef_diffuse
!
        if     (fl_prop%iflag_4_gravity                                 &
     &     .or. fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
          call alloc_buoyancy_coef_ele(numele, ak_MHD)
          ak_MHD%ak_buo(1:numele) = fl_prop%coef_buo
        end if
!
        if     (fl_prop%iflag_4_composit_buo                            &
     &     .or. fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
          call alloc_comp_buo_coef_ele(numele, ak_MHD)
          ak_MHD%ak_comp_buo(1:numele) = fl_prop%coef_comp_buo
        end if
      end if
!
      end subroutine ele_viscous_diffusion
!
!-----------------------------------------------------------------------
!
      subroutine ele_magnetic_diffusion(numele, cd_prop, ak_MHD)
!
      integer(kind = kint), intent(in) :: numele
      type(conductive_property), intent(in)  :: cd_prop
!
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
!
      if     (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call alloc_magne_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_magne(1:numele) = cd_prop%coef_diffuse
      end if
!
      end subroutine ele_magnetic_diffusion
!
!-----------------------------------------------------------------------
!
      subroutine ele_thermal_diffusion(numele, ht_prop, ak_MHD)
!
      integer(kind = kint), intent(in) :: numele
      type(scalar_property), intent(in) :: ht_prop
!
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_temp_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_temp(1:numele) = ht_prop%coef_diffuse
      end if
!
      end subroutine ele_thermal_diffusion
!
!-----------------------------------------------------------------------
!
      subroutine ele_compositional_diffusion(numele, cp_prop, ak_MHD)
!
      integer(kind = kint), intent(in) :: numele
      type(scalar_property), intent(in) :: cp_prop
!
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
!
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_dscalar_diff_MHD_AMG(numele, ak_MHD)
        ak_MHD%ak_d_composit(1:numele) = cp_prop%coef_diffuse
      end if
!
      end subroutine ele_compositional_diffusion
!
!-----------------------------------------------------------------------
!
      end module init_ele_material_property
