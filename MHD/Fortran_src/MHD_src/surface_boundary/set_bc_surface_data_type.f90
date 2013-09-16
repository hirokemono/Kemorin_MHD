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
      subroutine s_set_bc_surface_data_type(group, sf_dat)
!
      use m_machine_parameter
      use t_mesh_data
!
      use count_num_surface_type
      use set_surface_type_id
!
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
      call s_set_surface_type_id(group, sf_dat)
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
        call alloc_surf_scaler_dat_type(sf_dat%temp%sgs)
        call alloc_surf_scaler_dat_type(sf_dat%temp%flux_lead)
        call alloc_surf_scaler_type(sf_dat%temp%flux)
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_surf_vector_dat_type(sf_dat%velo%sgs)
        call alloc_surf_scaler_type(sf_dat%velo%normal)
        call alloc_surf_vector_type(sf_dat%velo%grad)
        call alloc_surf_vector_dat_type(sf_dat%velo%torque_lead)
        call alloc_surf_scaler_dat_type(sf_dat%velo%free_sph_in)
        call alloc_surf_scaler_dat_type(sf_dat%velo%free_sph_out)
!
        call alloc_surf_scaler_dat_type(sf_dat%press%sgs)
        call alloc_surf_scaler_type(sf_dat%press%grad)
        call alloc_surf_scaler_dat_type(sf_dat%press%grad_lead)
        call alloc_surf_scaler_dat_type(sf_dat%press%wall)
        call alloc_surf_scaler_dat_type(sf_dat%press%sph_in)
        call alloc_surf_scaler_dat_type(sf_dat%press%sph_out)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_surf_vector_dat_type(sf_dat%magne%sgs)
        call alloc_surf_scaler_type(sf_dat%magne%normal)
        call alloc_surf_vector_type(sf_dat%magne%grad)
        call alloc_surf_vector_dat_type(sf_dat%magne%torque_lead)
!
        call alloc_surf_vector_dat_type(sf_dat%current%sgs)
        call alloc_surf_scaler_type(sf_dat%current%normal)
        call alloc_surf_vector_type(sf_dat%current%grad)
        call alloc_surf_vector_dat_type(sf_dat%current%torque_lead)
!
        call alloc_surf_scaler_dat_type(sf_dat%magne_p%sgs)
        call alloc_surf_scaler_type(sf_dat%magne_p%grad)
        call alloc_surf_scaler_dat_type(sf_dat%magne_p%grad_lead)
        call alloc_surf_scaler_dat_type(sf_dat%magne_p%wall)
        call alloc_surf_scaler_dat_type(sf_dat%magne_p%sph_in)
        call alloc_surf_scaler_dat_type(sf_dat%magne_p%sph_out)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_surf_vector_dat_type(sf_dat%vector_p%sgs)
        call alloc_surf_scaler_type(sf_dat%vector_p%normal)
        call alloc_surf_vector_type(sf_dat%vector_p%grad)
        call alloc_surf_vector_dat_type(sf_dat%vector_p%torque_lead)
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call alloc_surf_scaler_dat_type(sf_dat%comp_sf%sgs)
        call alloc_surf_scaler_dat_type(sf_dat%comp_sf%flux_lead)
        call alloc_surf_scaler_type(sf_dat%comp_sf%flux)
      end if
! 
      end subroutine s_alloc_surf_bc_data_type
!
!-----------------------------------------------------------------------
!
      end module set_bc_surface_data_type
