!>@file   t_MHD_boundary_data.f90
!!@brief  module t_MHD_boundary_data
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_surf_bc_data_type(surf_bcs)
!!
!!      subroutine alloc_surf_data_velo(Vsf_bcs)
!!      subroutine alloc_surf_potential(Fsf_bcs)
!!      subroutine alloc_surf_data_scalar(Ssf_bcs)
!!      subroutine alloc_surf_vector(Bsf_bcs)
!!
!!      subroutine dealloc_surf_data_velo(Vsf_bcs)
!!      subroutine dealloc_surf_potential(Fsf_bcs)
!!      subroutine dealloc_surf_data_scalar(Ssf_bcs)
!!      subroutine dealloc_surf_vector(Bsf_bcs)
!!@endverbatim
!
      module t_MHD_boundary_data
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
      subroutine alloc_surf_bc_data_type(surf_bcs)
!
      use m_control_parameter
!
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      if (evo_temp%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_scalar(surf_bcs%Tsf_bcs)
      end if
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_velo(surf_bcs%Vsf_bcs)
        call alloc_surf_potential(surf_bcs%Psf_bcs)
      end if
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution                   &
     &      .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_vector(surf_bcs%Bsf_bcs)
        call alloc_surf_vector(surf_bcs%Jsf_bcs)
        call alloc_surf_potential(surf_bcs%Fsf_bcs)
      end if
!
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_velo(surf_bcs%Asf_bcs)
      end if
! 
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
        call alloc_surf_data_scalar(surf_bcs%Csf_bcs)
      end if
! 
      end subroutine alloc_surf_bc_data_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_data_velo(Vsf_bcs)
!
      type(velocity_surf_bc_type), intent(inout) :: Vsf_bcs
!
!
      call alloc_surf_vector_num(Vsf_bcs%grad)
      call alloc_surf_scaler_num(Vsf_bcs%normal)
      call alloc_surf_vector_dat_type(Vsf_bcs%sgs)
      call alloc_surf_vector_dat_type(Vsf_bcs%torque_lead)
      call alloc_surf_scaler_dat_type(Vsf_bcs%free_sph_in)
      call alloc_surf_scaler_dat_type(Vsf_bcs%free_sph_out)
!
      call alloc_surf_vector_apt(Vsf_bcs%grad)
      call alloc_surf_scaler_apt(Vsf_bcs%normal)
!
      end subroutine alloc_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_potential(Fsf_bcs)
!
      type(potential_surf_bc_type), intent(inout) :: Fsf_bcs
!
!
      call alloc_surf_scaler_dat_type(Fsf_bcs%sgs)
      call alloc_surf_scaler_dat_type(Fsf_bcs%grad_lead)
!
      call alloc_surf_scaler_num(Fsf_bcs%grad)
      call alloc_surf_scaler_dat_type(Fsf_bcs%wall)
      call alloc_surf_scaler_dat_type(Fsf_bcs%sph_in)
      call alloc_surf_scaler_dat_type(Fsf_bcs%sph_out)
!
      call alloc_surf_scaler_apt(Fsf_bcs%grad)
!
       end subroutine alloc_surf_potential
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_data_scalar(Ssf_bcs)
!
      type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!
!
      call alloc_surf_scaler_num(Ssf_bcs%flux)
      call alloc_surf_scaler_dat_type(Ssf_bcs%sgs)
      call alloc_surf_scaler_dat_type(Ssf_bcs%flux_lead)
!
      call alloc_surf_scaler_apt(Ssf_bcs%flux)
!
      end subroutine alloc_surf_data_scalar
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector(Bsf_bcs)
!
      type(vector_surf_bc_type), intent(inout) :: Bsf_bcs
!
!
      call alloc_surf_vector_num(Bsf_bcs%grad)
      call alloc_surf_vector_dat_type(Bsf_bcs%sgs)
      call alloc_surf_scaler_num(Bsf_bcs%normal)
      call alloc_surf_vector_dat_type(Bsf_bcs%torque_lead)
!
      call alloc_surf_vector_apt(Bsf_bcs%grad)
      call alloc_surf_scaler_apt(Bsf_bcs%normal)
!
      end subroutine alloc_surf_vector
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_data_velo(Vsf_bcs)
!
      type(velocity_surf_bc_type), intent(inout) :: Vsf_bcs
!
!
      call dealloc_surf_vector_type(Vsf_bcs%grad)
      call dealloc_surf_scaler_type(Vsf_bcs%normal)
      call dealloc_surf_vector_dat_type(Vsf_bcs%sgs)
      call dealloc_surf_vector_dat_type(Vsf_bcs%torque_lead)
      call dealloc_surf_scaler_dat_type(Vsf_bcs%free_sph_in)
      call dealloc_surf_scaler_dat_type(Vsf_bcs%free_sph_out)
!
      end subroutine dealloc_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_potential(Fsf_bcs)
!
      type(potential_surf_bc_type), intent(inout) :: Fsf_bcs
!
!
      call dealloc_surf_scaler_dat_type(Fsf_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%grad_lead)
!
      call dealloc_surf_scaler_type(Fsf_bcs%grad)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%wall)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%sph_in)
      call dealloc_surf_scaler_dat_type(Fsf_bcs%sph_out)
!
      end subroutine dealloc_surf_potential
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_data_scalar(Ssf_bcs)
!
      type(scaler_surf_bc_type), intent(inout) :: Ssf_bcs
!
!
      call dealloc_surf_scaler_type(Ssf_bcs%flux)
      call dealloc_surf_scaler_dat_type(Ssf_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Ssf_bcs%flux_lead)
!
      end subroutine dealloc_surf_data_scalar
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector(Bsf_bcs)
!
      type(vector_surf_bc_type), intent(inout) :: Bsf_bcs
!
!
      call dealloc_surf_vector_type(Bsf_bcs%grad)
      call dealloc_surf_vector_dat_type(Bsf_bcs%sgs)
      call dealloc_surf_scaler_type(Bsf_bcs%normal)
      call dealloc_surf_vector_dat_type(Bsf_bcs%torque_lead)
!
      end subroutine dealloc_surf_vector
!
!-----------------------------------------------------------------------
!
      end module t_MHD_boundary_data
