!>@file   sum_total_buoyancy.f90
!!@brief  module sum_total_buoyancy
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Aug., 2025
!!
!>@brief  Summuation of total buoyancy
!!
!!@verbatim
!!      subroutine s_sum_total_buoyancy(id_forces, nod_fld)
!!        type(base_force_address), intent(in) :: id_forces
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine sum_total_buoyancy_flux(id_eflux, nod_fld)
!!        type(energy_flux_address), intent(in) :: id_eflux
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module sum_total_buoyancy
!
      use m_precision
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sum_total_buoyancy(id_forces, nod_fld)
!
      use t_base_force_labels
      use copy_nodal_fields
!
      type(base_force_address), intent(in) :: id_forces
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(id_forces%i_buoyancy .gt. 0) then
        if((id_forces%i_thrm_buo * id_forces%i_comp_buo) .gt. 0) then
          call add_2_nod_vectors(nod_fld,                               &
     &        id_forces%i_thrm_buo, id_forces%i_comp_buo,               &
     &        id_forces%i_buoyancy)
        else if(id_forces%i_thrm_buo .gt. 0) then
          call copy_vector_component(nod_fld,                           &
     &        id_forces%i_thrm_buo, id_forces%i_buoyancy)
        else if(id_forces%i_comp_buo .gt. 0) then
          call copy_vector_component(nod_fld,                           &
     &        id_forces%i_comp_buo, id_forces%i_buoyancy)
        end if
      end if
!
      end subroutine s_sum_total_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine sum_total_buoyancy_flux(id_eflux, nod_fld)
!
      use t_energy_flux_labels
      use copy_nodal_fields
!
      type(energy_flux_address), intent(in) :: id_eflux
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(id_eflux%i_buo_flux .gt. 0) then
        if((id_eflux%i_t_buo_flux * id_eflux%i_c_buo_flux) .gt. 0) then
          call add_2_nod_vectors(nod_fld,                               &
     &        id_eflux%i_t_buo_flux, id_eflux%i_c_buo_flux,             &
     &        id_eflux%i_buo_flux)
        else if(id_eflux%i_t_buo_flux .gt. 0) then
          call copy_vector_component(nod_fld,                           &
     &        id_eflux%i_t_buo_flux, id_eflux%i_buo_flux)
        else if(id_eflux%i_c_buo_flux .gt. 0) then
          call copy_vector_component(nod_fld,                           &
     &        id_eflux%i_c_buo_flux, id_eflux%i_buo_flux)
        end if
      end if
!
      end subroutine sum_total_buoyancy_flux
!
! ----------------------------------------------------------------------
!
      end module sum_total_buoyancy
