!>@file   set_sym_diffusion_labels.f90
!!        module set_sym_diffusion_labels
!!
!!@author T. kera
!!@date   Programmed in Jan., 2026 by T. Kera (Tohoku Univ.)
!!
!!
!> @brief Labels and addresses for diffusion terms with equatorial symmetry
!!
!!@verbatim
!!      subroutine set_sym_diffusion_addresses                         &
!!     &         (i_phys, field_name, sym_diffusion, flag)
!!        type(diffusion_address), intent(inout) :: sym_diffusion
!!
!! !!!!!  sym viscous diffusion names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   sym_viscous_diffusion             [sym_diffusion%i_v_diffuse]:
!!   sym_vorticity_diffusion           [sym_diffusion%i_w_diffuse]:
!!   sym_vector_potential_diffusion    [sym_diffusion%i_vp_diffuse]:
!!   sym_magnetic_diffusion            [sym_diffusion%i_b_diffuse]:
!!   sym_thermal_diffusion             [sym_diffusion%i_t_diffuse]:
!!   sym_composition_diffusion         [sym_diffusion%i_c_diffuse]:
!!
!!   sym_div_viscousity                [sym_diffusion%i_div_viscous]:
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_sym_diffusion_labels
!
      use m_precision
      use m_phys_constants
!      use t_base_field_labels   not sure but seems unnecessary
      use t_diffusion_term_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_diffusion_addresses                             &
      &         (i_phys, field_name, sym_diffusion, flag)
!
      use m_diffusion_term_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(diffusion_address), intent(inout) :: sym_diffusion
      logical, intent(inout) :: flag
!
!
      flag = check_vector_diffusion_w_symmetry(field_name)                         &
     &      .or. check_scalar_diffusion_w_symmetry(field_name)
      write(*,*)  'set_sym_diffusion_addresses start'
      if(flag) then
            write(*,*)  'set_sym_diffusion_addresses start'
!
            if (field_name .eq. sym_viscous_diffusion%name) then
            sym_diffusion%i_v_diffuse = i_phys
            else if (field_name .eq. sym_vorticity_diffusion%name) then
            sym_diffusion%i_w_diffuse =   i_phys
            else if (field_name .eq. sym_vector_potential_diffusion%name) then
            sym_diffusion%i_vp_diffuse =  i_phys
            else if (field_name .eq. sym_magnetic_diffusion%name) then
            sym_diffusion%i_b_diffuse =   i_phys
            else if (field_name .eq. sym_thermal_diffusion%name) then
            sym_diffusion%i_t_diffuse =   i_phys
            else if (field_name .eq. sym_composition_diffusion%name) then
            sym_diffusion%i_c_diffuse =   i_phys
      !
            else if (field_name .eq. sym_div_viscousity%name) then
            sym_diffusion%i_div_viscous = i_phys
            end if
      !
            write(*,*)  'set_sym_diffusion_addresses end'
!
!
      end if  
!
      end subroutine set_sym_diffusion_addresses
! 
! ----------------------------------------------------------------------
!
      subroutine set_asym_diffusion_addresses                             &
      &         (i_phys, field_name, asym_diffusion, flag)
!
      use m_diffusion_term_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(diffusion_address), intent(inout) :: asym_diffusion
      logical, intent(inout) :: flag
!
!
      flag = check_vector_diffusion_w_symmetry(field_name)                         &
     &      .or. check_scalar_diffusion_w_symmetry(field_name)
      if(flag) then
            if (field_name .eq. asym_viscous_diffusion%name) then
            asym_diffusion%i_v_diffuse = i_phys
            else if (field_name .eq. asym_vorticity_diffusion%name) then
            asym_diffusion%i_w_diffuse =   i_phys
            else if (field_name .eq. asym_vector_potential_diffusion%name) then
            asym_diffusion%i_vp_diffuse =  i_phys
            else if (field_name .eq. asym_magnetic_diffusion%name) then
            asym_diffusion%i_b_diffuse =   i_phys
            else if (field_name .eq. asym_thermal_diffusion%name) then
            asym_diffusion%i_t_diffuse =   i_phys
            else if (field_name .eq. asym_composition_diffusion%name) then
            asym_diffusion%i_c_diffuse =   i_phys
      !
            else if (field_name .eq. asym_div_viscousity%name) then
            asym_diffusion%i_div_viscous = i_phys
            end if
      end if  
!
      end subroutine set_asym_diffusion_addresses
!
! ----------------------------------------------------------------------
!
      end module set_sym_diffusion_labels
