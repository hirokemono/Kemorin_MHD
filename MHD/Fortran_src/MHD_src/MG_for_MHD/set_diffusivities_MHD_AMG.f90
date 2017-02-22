!
!     module set_diffusivities_MHD_AMG
!
!      Written by H.Matsui on Dec., 2008
!
!!      subroutine s_set_diffusivities_MHD_AMG                          &
!!     &         (evo_B, evo_A, evo_T, evo_C, ele,               &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, ak_AMG)
!!        type(time_evolution_params), intent(in) :: evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(element_data), intent(in) :: ele
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(coefs_4_MHD_type), intent(inout) :: ak_AMG
!
      module set_diffusivities_MHD_AMG
!
      use m_precision
!
      use t_time_stepping_parameter
      use t_geometry_data
      use t_coefs_element_4_MHD
      use t_physical_property
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_diffusivities_MHD_AMG                            &
     &         (evo_B, evo_A, evo_T, evo_C, ele,                 &
     &          fl_prop, cd_prop, ht_prop, cp_prop, ak_AMG)
!
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(element_data), intent(in) :: ele
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(coefs_4_MHD_type), intent(inout) :: ak_AMG
!
!  allocate
!    For thermal
!
      if (evo_T%iflag_scheme .gt. id_no_evolution) then
        call alloc_temp_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!    For convection
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_velo_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!   For Induction
!
      if (evo_B%iflag_scheme .gt. id_no_evolution                       &
     &      .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call alloc_magne_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!   For dummy scalar
!
      if (evo_C%iflag_scheme .gt. id_no_evolution) then
        call alloc_dscalar_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!  set data
!    For thermal
!
      if (ele%numele .gt. 0) then
        if (evo_T%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_temp(1:ele%numele) = ht_prop%coef_diffuse
        end if
!
!    For convection
!
        if (fl_prop%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_velo(1:ele%numele) = fl_prop%coef_diffuse
        end if
!
!   For Induction
!
        if (evo_B%iflag_scheme .gt. id_no_evolution                     &
     &      .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_magne(1:ele%numele) = cd_prop%coef_diffuse
        end if
!
!   For dummy scalar
!
        if (evo_C%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_composit(1:ele%numele) = cp_prop%coef_diffuse
        end if
      end if
!
      end subroutine s_set_diffusivities_MHD_AMG
!
! -----------------------------------------------------------------------
!
      end module set_diffusivities_MHD_AMG
