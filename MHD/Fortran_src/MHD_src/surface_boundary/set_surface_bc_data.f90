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
      use m_geometry_data
      use m_group_data
!
      use count_num_surface_bc
      use set_surface_id_MHD
      use set_surface_values
!
!
      call allocate_work_4_surf_bc_dat(node1%numnod)
!
! ---  set boundary conditions
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_surf_bc'
      call count_num_surf_bc(sf_grp1, sf_grp_nod1)
!
      call allocate_surf_bc_data
!
      call set_surface_id(sf_grp1, sf_grp_nod1, sf_grp_v1)
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
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
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
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        if(light_surf%num_bc.gt.0) call deallocate_composit_surf_ctl
      end if
!
      end subroutine deallocate_surf_bc_lists
!
!  ---------------------------------------------------------------------
!
      end module set_surface_bc_data
