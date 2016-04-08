!
!     module set_diffusivities_MHD_AMG
!
!      Written by H.Matsui on Dec., 2008
!
!!      subroutine s_set_diffusivities_MHD_AMG(ele, ak_AMG)
!!        type(element_data), intent(in) :: ele
!!        type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!!      subroutine s_set_sgs_diff_array_MHD_AMG(ele, ifld_diff, ak_AMG)
!!        type(element_data), intent(in) :: ele
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
      module set_diffusivities_MHD_AMG
!
      use m_precision
!
      use t_geometry_data
      use t_coefs_element_4_MHD
      use m_physical_property
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_diffusivities_MHD_AMG(ele, ak_AMG)
!
      use m_control_parameter
!
      type(element_data), intent(in) :: ele
      type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
!  allocate
!    For thermal
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call alloc_temp_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!    For convection
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_velo_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!   For Induction
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_magne_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!   For dummy scalar
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call alloc_dscalar_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!  set data
!    For thermal
!
      if (ele%numele .gt. 0) then
        if (iflag_t_evo_4_temp .gt. id_no_evolution) then
          ak_AMG%ak_d_temp(1:ele%numele) = coef_d_temp
        end if
!
!    For convection
!
        if (iflag_t_evo_4_velo .gt. id_no_evolution) then
          ak_AMG%ak_d_velo(1:ele%numele) = coef_d_velo
        end if
!
!   For Induction
!
        if (iflag_t_evo_4_magne .gt. id_no_evolution                    &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          ak_AMG%ak_d_magne(1:ele%numele) = coef_d_magne
        end if
!
!   For dummy scalar
!
        if (iflag_t_evo_4_composit .gt. id_no_evolution) then
          ak_AMG%ak_d_composit(1:ele%numele) = coef_d_light
        end if
      end if
!
      end subroutine s_set_diffusivities_MHD_AMG
!
! -----------------------------------------------------------------------
!
      subroutine s_set_sgs_diff_array_MHD_AMG(ele, ifld_diff, ak_AMG)
!
      use t_material_property
!
      type(element_data), intent(in) :: ele
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(coefs_4_MHD_AMG), intent(inout) :: ak_AMG
!
!
      if (ifld_diff%i_velo .gt. 0) then
        call alloc_diff_coefs_velo_type(ele%numele, ak_AMG)
      end if
!
      if (ifld_diff%i_temp .gt. 0) then
        call alloc_diff_coefs_temp_type(ele%numele, ak_AMG)
      end if
!
      if (ifld_diff%i_magne .gt. 0) then
        call alloc_diff_coefs_magne_type(ele%numele, ak_AMG)
      end if
!
      if (ifld_diff%i_light .gt. 0) then
        call alloc_diff_coefs_ds_type(ele%numele, ak_AMG)
      end if
!
      end subroutine s_set_sgs_diff_array_MHD_AMG
!
! -----------------------------------------------------------------------
!
      end module set_diffusivities_MHD_AMG
