!>@file   m_surf_data_list.f90
!!@brief  module m_surf_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine allocate_velo_surf_ctl
!!      subroutine allocate_press_surf_ctl
!!      subroutine allocate_temp_surf_ctl
!!      subroutine allocate_magne_surf_ctl
!!      subroutine allocate_vect_p_surf_ctl
!!      subroutine allocate_magp_surf_ctl
!!      subroutine allocate_current_surf_ctl
!!      subroutine allocate_d_scalar_surf_ctl
!!@endverbatim
! 
      module m_surf_data_list
!
      use m_precision
!
      implicit  none
!
!
!>       Structure for surface group data list
      type surface_bc_list_type
!>       number of boundary condition list
        integer (kind=kint) :: num_bc
!>       Value for the boundary condition
        real (kind=kreal), allocatable :: bc_magnitude(:)
!>       Type of the boundary condition
        integer (kind=kint), allocatable :: ibc_type(:)
!>       Name of group to apply the boundary condition
        character (len=kchara), allocatable :: bc_name(:)
      end type surface_bc_list_type
!
!>       Surface group data list for stresses
      type(surface_bc_list_type), save :: torque_surf
!>       Surface group data list for pressure
      type(surface_bc_list_type), save :: wall_surf
!
!>       Surface group data list for temperature
      type(surface_bc_list_type), save :: h_flux_surf
!>       Surface group data list for composition
      type(surface_bc_list_type), save :: light_surf
!
!>       Surface group data list for magnetic field
      type(surface_bc_list_type), save :: magne_surf
!>       Surface group data list for magnetic vector potential
      type(surface_bc_list_type), save :: a_potential_surf
!>       Surface group data list for electrical potential
      type(surface_bc_list_type), save :: e_potential_surf
!>       Surface group data list for current density
      type(surface_bc_list_type), save :: current_surf
!
      private :: deallocate_velo_surf_ctl, deallocate_press_surf_ctl
      private :: deallocate_magp_surf_ctl, deallocate_temp_surf_ctl
      private :: deallocate_magne_surf_ctl, deallocate_vecp_surf_ctl
      private :: deallocate_current_surf_ctl
      private :: deallocate_composit_surf_ctl
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_bc_lists
!
      use m_control_parameter
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        if(h_flux_surf%num_bc .gt. 0) call deallocate_temp_surf_ctl
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if(torque_surf%num_bc.gt.0) call deallocate_velo_surf_ctl
        if(wall_surf%num_bc.gt.0)   call deallocate_press_surf_ctl
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(magne_surf%num_bc .gt. 0)   call deallocate_magne_surf_ctl
        if(current_surf%num_bc .gt. 0) call deallocate_current_surf_ctl
        if(e_potential_surf%num_bc.gt.0) call deallocate_magp_surf_ctl
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(a_potential_surf%num_bc.gt.0) call deallocate_vecp_surf_ctl
      end if
! 
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
        if(light_surf%num_bc.gt.0) call deallocate_composit_surf_ctl
      end if
!
      end subroutine deallocate_surf_bc_lists
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_surface_bc_type_ctl(surf_bc_list)
!
      type(surface_bc_list_type), intent(inout) :: surf_bc_list
!
!
      allocate(surf_bc_list%bc_magnitude(surf_bc_list%num_bc))
      allocate(surf_bc_list%ibc_type(surf_bc_list%num_bc))
      allocate(surf_bc_list%bc_name(surf_bc_list%num_bc))
!
      if(surf_bc_list%num_bc .gt. 0) then
        surf_bc_list%ibc_type =     0
        surf_bc_list%bc_magnitude = 0.0d0
      end if
!
      end subroutine alloc_surface_bc_type_ctl
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surface_bc_type_ctl(surf_bc_list)
!
      type(surface_bc_list_type), intent(inout) :: surf_bc_list
!
!
      deallocate(surf_bc_list%bc_magnitude)
      deallocate(surf_bc_list%ibc_type)
      deallocate(surf_bc_list%bc_name)
!
      end subroutine dealloc_surface_bc_type_ctl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_velo_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(torque_surf)
!
      end subroutine allocate_velo_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_press_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(wall_surf)
!
      end subroutine allocate_press_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_temp_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(h_flux_surf)
!
      end subroutine allocate_temp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_magne_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(magne_surf)
!
      end subroutine allocate_magne_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_vect_p_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(a_potential_surf)
!
      end subroutine allocate_vect_p_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_magp_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(e_potential_surf)
!
      end subroutine allocate_magp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_current_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(current_surf)
!
      end subroutine allocate_current_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_d_scalar_surf_ctl
!
!
      call alloc_surface_bc_type_ctl(light_surf)
!
      end subroutine allocate_d_scalar_surf_ctl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_velo_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(torque_surf)
!
      end subroutine deallocate_velo_surf_ctl
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_press_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(wall_surf)
!
       end subroutine deallocate_press_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_temp_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(h_flux_surf)
!
      end subroutine deallocate_temp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_magne_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(magne_surf)
!
      end subroutine deallocate_magne_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_vecp_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(a_potential_surf)
!
      end subroutine deallocate_vecp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_magp_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(e_potential_surf)
!
      end subroutine deallocate_magp_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_current_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(current_surf)
!
      end subroutine deallocate_current_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_composit_surf_ctl
!
!
      call dealloc_surface_bc_type_ctl(light_surf)
!
      end subroutine deallocate_composit_surf_ctl
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_list
