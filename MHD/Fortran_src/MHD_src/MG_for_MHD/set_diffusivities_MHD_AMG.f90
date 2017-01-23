!
!     module set_diffusivities_MHD_AMG
!
!      Written by H.Matsui on Dec., 2008
!
!!      subroutine s_set_diffusivities_MHD_AMG(ele, ak_AMG)
!!        type(element_data), intent(in) :: ele
!!        type(coefs_4_MHD_type), intent(inout) :: ak_AMG
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
      type(coefs_4_MHD_type), intent(inout) :: ak_AMG
!
!  allocate
!    For thermal
!
      if (evo_temp%iflag_scheme .gt. id_no_evolution) then
        call alloc_temp_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!    For convection
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        call alloc_velo_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!   For Induction
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution                   &
     &      .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call alloc_magne_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!   For dummy scalar
!
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
        call alloc_dscalar_diff_MHD_AMG(ele%numele, ak_AMG)
      end if
!
!  set data
!    For thermal
!
      if (ele%numele .gt. 0) then
        if (evo_temp%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_temp(1:ele%numele) = ht_prop1%coef_diffuse
        end if
!
!    For convection
!
        if (evo_velo%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_velo(1:ele%numele) = fl_prop1%coef_diffuse
        end if
!
!   For Induction
!
        if (evo_magne%iflag_scheme .gt. id_no_evolution                 &
     &      .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_magne(1:ele%numele) = cd_prop1%coef_diffuse
        end if
!
!   For dummy scalar
!
        if (evo_comp%iflag_scheme .gt. id_no_evolution) then
          ak_AMG%ak_d_composit(1:ele%numele) = coef_d_light
        end if
      end if
!
      end subroutine s_set_diffusivities_MHD_AMG
!
! -----------------------------------------------------------------------
!
      end module set_diffusivities_MHD_AMG
