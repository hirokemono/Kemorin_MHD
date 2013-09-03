!
!      module set_surface_bc_data
!
!        programmed by H.Matsui
!        modified by H. Matsui on Sep., 2007
!
!      subroutine set_surf_bc_data
!      subroutine deallocate_surf_bc_lists
!
      module set_surface_bc_data
!
      use m_precision
!
      implicit  none
!
      private :: allocate_surf_bc_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_bc_data
!
      use m_machine_parameter
      use m_geometry_parameter
!
      use count_num_surface_bc
      use set_surface_id_MHD
      use set_surface_values
!
!
      call allocate_work_4_surf_bc_dat(numnod)
!
! ---  set boundary conditions
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_bc'
      call count_num_surf_bc
!
      call allocate_surf_bc_data
!
      call set_surface_id
! 
      call deallocate_work_4_surf_bc_dat
! 
      end subroutine set_surf_bc_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_bc_data
!
      use m_control_parameter
      use m_surf_data_temp
      use m_surf_data_press
      use m_surf_data_vector_p
      use m_surf_data_magne
      use m_surf_data_magne_p
      use m_surf_data_torque
      use m_surf_data_current
      use m_surf_data_composition
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call allocate_surf_data_temp
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call allocate_surf_data_velo
        call allocate_surf_data_torque
        call allocate_surf_press
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                       &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call allocate_surf_data_magne
        call allocate_surf_data_current
        call allocate_surf_data_magne_p
        call allocate_surf_magp_grad
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call allocate_surf_data_vect_p
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call allocate_surf_data_composit
      end if
!
      end subroutine allocate_surf_bc_data
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surf_bc_lists
!
      use m_control_parameter
      use m_surf_data_list
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        if(num_bc_h_flux.gt.0) call deallocate_temp_surf_ctl
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if(num_bc_tq.gt.0) call deallocate_velo_surf_ctl
        if(num_bc_wall.gt.0) call deallocate_press_surf_ctl
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(num_bc_bs.gt.0) call deallocate_magne_surf_ctl
        if(num_bc_js.gt.0) call deallocate_current_surf_ctl
        if(num_surf_magp.gt.0) call deallocate_magp_surf_ctl
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(num_bc_vps.gt.0) call deallocate_vect_p_surf_ctl
      end if
! 
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        if(num_surf_composition.gt.0) call deallocate_composit_surf_ctl
      end if
!
      end subroutine deallocate_surf_bc_lists
!
!  ---------------------------------------------------------------------
!
      end module set_surface_bc_data
