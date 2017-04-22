!>@file   m_surf_data_list.f90
!!@brief  module m_surf_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine deallocate_surf_bc_lists(MHD_prop)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!@endverbatim
! 
      module m_surf_data_list
!
      use m_precision
      use t_bc_data_list
!
      implicit  none
!
!
!>       Surface group data list for stresses
      type(boundary_condition_list), save :: torque_surf
!>       Surface group data list for pressure
      type(boundary_condition_list), save :: wall_surf
!
!>       Surface group data list for temperature
      type(boundary_condition_list), save :: h_flux_surf
!>       Surface group data list for composition
      type(boundary_condition_list), save :: light_surf
!
!>       Surface group data list for magnetic field
      type(boundary_condition_list), save :: magne_surf
!>       Surface group data list for magnetic vector potential
      type(boundary_condition_list), save :: a_potential_surf
!>       Surface group data list for electrical potential
      type(boundary_condition_list), save :: e_potential_surf
!>       Surface group data list for current density
      type(boundary_condition_list), save :: current_surf
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_bc_lists(MHD_prop)
!
      use t_control_parameter
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
!
      if (MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(h_flux_surf%num_bc .gt. 0)                                   &
     &      call dealloc_bc_type_ctl(h_flux_surf)
      end if
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(torque_surf%num_bc.gt.0)                                     &
     &      call dealloc_bc_type_ctl(torque_surf)
        if(wall_surf%num_bc.gt.0)   call dealloc_bc_type_ctl(wall_surf)
      end if
!
      if    (MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution    &
     &  .or. MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)   &
     & then
        if(magne_surf%num_bc .gt. 0)                                    &
     &        call dealloc_bc_type_ctl(magne_surf)
        if(current_surf%num_bc .gt. 0)                                  &
     &        call dealloc_bc_type_ctl(current_surf)
        if(e_potential_surf%num_bc.gt.0)                                &
     &        call dealloc_bc_type_ctl(e_potential_surf)
      end if
!
      if (MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(a_potential_surf%num_bc.gt.0)                                &
     &        call dealloc_bc_type_ctl(a_potential_surf)
      end if
! 
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(light_surf%num_bc.gt.0) call dealloc_bc_type_ctl(light_surf)
      end if
!
      end subroutine deallocate_surf_bc_lists
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_list
