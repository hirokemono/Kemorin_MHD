!>@file   t_surface_bc_data_MHD.f90
!!@brief  module t_surface_bc_data_MHD
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine set_bc_surface_data                                  &
!!     &         (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v, &
!!     &          MHD_prop, MHD_BC, surf_bcs)
!!      subroutine dealloc_surf_bc_data(MHD_prop, surf_bcs)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(surface_group_geometry), intent(in) :: sf_grp_v
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!!@endverbatim
!
      module t_surface_bc_data_MHD
!
      use m_precision
      use m_machine_parameter
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
      subroutine set_bc_surface_data                                    &
     &         (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,   &
     &          MHD_prop, MHD_BC, surf_bcs)
!
      use t_control_parameter
      use t_physical_property
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_phys_data
      use t_phys_address
      use t_boundary_field_IO
      use t_bc_data_list
!
      use set_surface_values
      use set_normal_field
!
      use scalar_surf_id
      use vector_surf_id
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      call allocate_work_4_surf_bc_dat(node%numnod)
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (iflag_debug .gt. 0) write(*,*) 'set_surf_grad_velo'
        call set_surf_grad_velo(name_svn, name_vg,                      &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      MHD_BC%velo_BC%surf_BC, surf_bcs%Vsf_bcs)
!
        if (iflag_debug .gt. 0) write(*,*) 'set_wall_potential_id'
        call set_wall_potential_id(name_pg, IO_bc, sf_grp,              &
     &      MHD_BC%press_BC%surf_BC, surf_bcs%Psf_bcs)
      end if
!
      if     (MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution   &
     &   .or. MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution   &
     &       ) then
        if (iflag_debug .gt. 0) write(*,*) 'set_surf_grad_vector'
        call set_surf_grad_vector(name_sbn, name_bg,                    &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      MHD_BC%magne_BC%surf_BC, surf_bcs%Bsf_bcs)
!
        call set_surf_grad_vector(name_sjn, name_jg,                    &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      MHD_BC%current_BC%surf_BC, surf_bcs%Jsf_bcs)
!
        if (iflag_debug .gt. 0) write(*,*) 'set_wall_potential_id'
        call set_wall_potential_id(name_mpg, IO_bc, sf_grp,             &
     &      MHD_BC%e_potential_BC%surf_BC, surf_bcs%Fsf_bcs)
      end if
!
      if (MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (iflag_debug .gt. 0) write(*,*) 'set_surf_grad_velo'
        call set_surf_grad_velo(name_san, name_ag,                      &
     &      IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
     &      MHD_BC%a_potential_BC%surf_BC, surf_bcs%Asf_bcs)
      end if
! 
      if (MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if (iflag_debug .gt. 0) write(*,*) 'set_surf_grad_scalar_id'
        call set_surf_grad_scalar_id(name_hf, IO_bc, sf_grp,            &
     &      MHD_BC%temp_BC%surf_BC, surf_bcs%Tsf_bcs)
      end if
!
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (iflag_debug .gt. 0) write(*,*) 'set_surf_grad_scalar_id'
        call set_surf_grad_scalar_id(name_dsg, IO_bc, sf_grp,           &
     &      MHD_BC%light_BC%surf_BC, surf_bcs%Csf_bcs)
      end if
!
      call deallocate_work_4_surf_bc_dat
! 
      end subroutine set_bc_surface_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_bc_data(MHD_prop, surf_bcs)
!
      use t_control_parameter
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(surface_boundarty_conditions), intent(inout) :: surf_bcs
!
!
      if (MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call dealloc_surf_data_scalar(surf_bcs%Tsf_bcs)
      end if
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call dealloc_surf_data_velo(surf_bcs%Vsf_bcs)
        call dealloc_surf_potential(surf_bcs%Psf_bcs)
      end if
!
      if     (MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution   &
     &   .or. MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution   &
     &       ) then
        call dealloc_surf_vector(surf_bcs%Bsf_bcs)
        call dealloc_surf_vector(surf_bcs%Jsf_bcs)
        call dealloc_surf_potential(surf_bcs%Fsf_bcs)
      end if
!
      if (MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call dealloc_surf_data_velo(surf_bcs%Asf_bcs)
      end if
! 
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call dealloc_surf_data_scalar(surf_bcs%Csf_bcs)
      end if
! 
      end subroutine dealloc_surf_bc_data
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_data_MHD
