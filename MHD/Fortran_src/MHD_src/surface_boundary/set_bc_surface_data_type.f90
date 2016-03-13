!
!      module set_bc_surface_data_type
!
!      Written by H. Matsui on Feb., 2009
!
!      subroutine s_set_bc_surface_data_type(group, sf_dat)
!        type(mesh_groups), intent(in) :: group
!        type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
      module set_bc_surface_data_type
!
      use m_precision
!
      use m_control_parameter
      use t_surface_bc_data
!
!
      implicit none
!
      private :: s_alloc_surf_bc_data_type
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine s_set_bc_surface_data_type(mesh, group, surf, sf_dat)
!
      use m_machine_parameter
      use t_mesh_data
!
      use set_surface_type_id
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
! ---  set boundary conditions
!
      if (iflag_debug.eq.1) write(*,*) 's_count_num_bc_surface_type'
      call s_count_num_bc_surface_type(group, sf_dat)
!
      call s_alloc_surf_bc_data_type(sf_dat)
!
      call s_set_surface_type_id                                        &
     &   (mesh%node, mesh%ele, surf, group, sf_dat)
! 
      end subroutine s_set_bc_surface_data_type
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_alloc_surf_bc_data_type(sf_dat)
!
      type(surface_boundarty_conditions), intent(inout) :: sf_dat
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call alloc_surf_data_scalar(sf_dat%temp)
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_surf_data_velo(sf_dat%velo)
        call alloc_surf_potential(sf_dat%press)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_surf_vector(sf_dat%magne)
        call alloc_surf_vector(sf_dat%current)
        call alloc_surf_potential(sf_dat%magne_p)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_surf_data_velo(sf_dat%vector_p)
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call alloc_surf_data_scalar(sf_dat%comp_sf)
      end if
! 
      end subroutine s_alloc_surf_bc_data_type
!
!-----------------------------------------------------------------------
!
      end module set_bc_surface_data_type
