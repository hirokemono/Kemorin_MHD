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
!!@endverbatim
!
      module sum_total_buoyancy
!
      implicit none
!
      use m_precision
      use t_base_force_labels
      use t_phys_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!      call s_sum_total_buoyancy(iphys%forces, rj_fld)
      call s_sum_total_buoyancy(iphys%rot_forces, rj_fld)
!      call s_sum_total_buoyancy(iphys%div_forces, rj_fld)
!
!      call s_sum_total_buoyancy(iphys%forces_by_sym_sym, rj_fld)
!      call s_sum_total_buoyancy(iphys%forces_by_sym_asym, rj_fld)
!
!      call s_sum_total_buoyancy(iphys%force_by_filter, rj_fld)
      call s_sum_total_buoyancy(iphys%rot_frc_by_filter, rj_fld)
!      call s_sum_total_buoyancy(iphys%div_frc_by_filter, rj_fld)
!
! ----------------------------------------------------------------------
!
      subroutine s_sum_total_buoyancy(id_forces, nod_fld)
!
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
      end module sum_total_buoyancy
