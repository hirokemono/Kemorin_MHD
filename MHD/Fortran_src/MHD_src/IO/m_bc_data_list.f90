!>@file   m_bc_data_list.f90
!!@brief  module m_bc_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief  Boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!@endverbatim
!
      module m_bc_data_list
!
      use m_precision
      use t_bc_data_list
!
      implicit  none
!
!
!>      Structure for boundary condition lists for MHD
      type(MHD_BC_lists), save :: MHD_BC1
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_bc_lists(MHD_prop, MHD_BC)
!
      use t_control_parameter
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
!
!
      if (MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_BC%temp_BC%surf_BC%num_bc .gt. 0)                        &
     &      call dealloc_surf_bc_list(MHD_BC%temp_BC)
      end if
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_BC%velo_BC%surf_BC%num_bc.gt.0)                          &
     &      call dealloc_surf_bc_list(MHD_BC%velo_BC)
        if(MHD_BC%press_BC%surf_BC%num_bc.gt.0)                         &
     &      call dealloc_surf_bc_list(MHD_BC%press_BC)
      end if
!
      if    (MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution    &
     &  .or. MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)   &
     & then
        if(MHD_BC%magne_BC%surf_BC%num_bc .gt. 0)                       &
     &        call dealloc_surf_bc_list(MHD_BC%magne_BC)
        if(MHD_BC%current_BC%surf_BC%num_bc .gt. 0)                     &
     &        call dealloc_surf_bc_list(MHD_BC%current_BC)
        if(MHD_BC%e_potential_BC%surf_BC%num_bc.gt.0)                   &
     &        call dealloc_surf_bc_list(MHD_BC%e_potential_BC)
      end if
!
      if (MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(MHD_BC%a_potential_BC%surf_BC%num_bc.gt.0)                   &
     &        call dealloc_surf_bc_list(MHD_BC%a_potential_BC)
      end if
! 
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_BC%light_BC%surf_BC%num_bc.gt.0)                         &
     &     call dealloc_surf_bc_list(MHD_BC%light_BC)
      end if
!
      end subroutine deallocate_surf_bc_lists
!
!-----------------------------------------------------------------------
!
!
      end module m_bc_data_list
