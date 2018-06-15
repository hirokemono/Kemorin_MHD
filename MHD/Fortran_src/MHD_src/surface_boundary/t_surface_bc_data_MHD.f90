!>@file   t_surface_bc_data_MHD.f90
!!@brief  module t_surface_bc_data_MHD
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_surf_bc_data_type                              &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, surf_bcs)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!!@endverbatim
!
      module t_surface_bc_data_MHD
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
      type surface_boundarty_conditions
        type(scaler_surf_bc_type) :: Tsf_bcs
!
        type(velocity_surf_bc_type) :: Vsf_bcs
        type(velocity_surf_bc_type) :: Asf_bcs
        type(vector_surf_bc_type) ::   Bsf_bcs
        type(vector_surf_bc_type) ::   Jsf_bcs
!
        type(potential_surf_bc_type) :: Psf_bcs
        type(potential_surf_bc_type) :: Fsf_bcs
!
        type(scaler_surf_bc_type) :: Csf_bcs
      end type surface_boundarty_conditions
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_bc_data_type                                &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, surf_bcs)
!
      use t_physical_property
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_scalar(surf_bcs%Tsf_bcs)
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_velo(surf_bcs%Vsf_bcs)
        call alloc_surf_potential(surf_bcs%Psf_bcs)
      end if
!
      if     (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call alloc_surf_vector(surf_bcs%Bsf_bcs)
        call alloc_surf_vector(surf_bcs%Jsf_bcs)
        call alloc_surf_potential(surf_bcs%Fsf_bcs)
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call alloc_surf_data_velo(surf_bcs%Asf_bcs)
      end if
! 
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_scalar(surf_bcs%Csf_bcs)
      end if
! 
      end subroutine alloc_surf_bc_data_type
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_data_MHD
