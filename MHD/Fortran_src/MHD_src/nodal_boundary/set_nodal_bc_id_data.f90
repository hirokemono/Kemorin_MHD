!set_nodal_bc_id_data.f90
!     module set_nodal_bc_id_data
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!      subroutine set_bc_id_data
!
      module set_nodal_bc_id_data
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_id_data
!
      use m_machine_parameter
      use m_control_parameter
!
      use m_geometry_data
      use m_group_data
!
      use m_bc_data_ene
      use m_bc_temp_sgs
!
      use m_bc_data_velo
      use m_bc_velo_sgs
      use m_bc_data_vr0
      use m_bc_data_rotate
      use m_bc_data_vfree
      use m_bc_data_vsp
!
      use m_bc_data_magne
      use m_bc_magne_sgs
      use m_bc_data_magne_p
      use m_bc_mag_p_sgs
      use m_bc_data_mag_p_ins
      use m_bc_data_mag_p_cd
!
      use m_bc_data_vect_p
      use m_bc_vecp_sgs
!
      use m_bc_data_current
!
      use m_bc_data_press
      use m_bc_press_sgs
      use m_bc_data_composition
!
      use count_num_nod_bc_MHD
      use set_bc_phys_id
      use set_boundary_potentials
      use set_boundary_scalars
      use set_velocity_boundary
      use set_vecp_boundary
      use set_magne_boundary
!
!
!
      call count_num_bc_nod(nod_grp1)
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call allocate_bc_ene(node1%numnod)
        call allocate_bc_t_sgs(node1%numnod)
        call set_bc_temp_id
        call set_boundary_ene
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
!
        if ( iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 v'
        call allocate_bc_velo(node1%numnod)
        call allocate_bc_v_sgs(node1%numnod)
        call allocate_bc_vr0(node1%numnod)
        call allocate_bc_vfr(node1%numnod)
        call allocate_bc_rot(node1%numnod)
        call allocate_bc_vsp(node1%numnod)
!
        if (iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 P'
        call allocate_bc_press(node1%numnod)
        call allocate_bc_p_sgs(node1%numnod)
!
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 v'
        call set_bc_velo_id
        if ( iflag_debug .eq.1) write(*,*)  'set boundary values 4 v'
        call set_boundary_velo
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 P'
        call set_bc_press_id
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call allocate_bc_composition(node1%numnod)
        call set_bc_composition_id
        call set_boundary_composition
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call allocate_bc_magne(node1%numnod)
        call allocate_bc_b_sgs(node1%numnod)
        call allocate_bc_magne_p(node1%numnod)
        call allocate_bc_magp_sgs(node1%numnod)
        call allocate_bc_current(node1%numnod)
!
        if (iflag_debug.eq.1) write(*,*)  'set boundary ID 4 magne'
        call set_bc_magne_id
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 magne_p'
        call set_bc_m_potential_id
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 current'
        call set_bc_current_id
!
        if (iflag_debug.eq.1)  write(*,*) 'set boundary value 4 magne'
        call set_boundary_magne
        if (iflag_debug.eq.1) write(*,*) 'set boundary value 4 magne'
        call set_boundary_m_phi
        if (iflag_debug.eq.1) write(*,*) 'set boundary value 4 current'
        call set_boundary_current
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call allocate_bc_magne(node1%numnod)
        call allocate_bc_b_sgs(node1%numnod)
        call allocate_bc_vect_p(node1%numnod)
        call allocate_bc_vecp_sgs(node1%numnod)
        call allocate_bc_magne_p(node1%numnod)
        call allocate_bc_magp_sgs(node1%numnod)
        call allocate_bc_current(node1%numnod)
!
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 magne'
        call set_bc_magne_id
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 vect_p'
        call set_bc_vect_p_id
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 magne_p'
        call set_bc_m_potential_id
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 current'
        call set_bc_current_id
!
        if (iflag_debug .eq.1) write(*,*) 'set boundary value 4 magne'
        call set_boundary_magne
        if (iflag_debug .eq.1) write(*,*) 'set boundary value 4 vect_p'
        call set_boundary_vect_p
        if (iflag_debug .eq.1) write(*,*) 'set boundary value 4 magne'
        call set_boundary_m_phi
        if (iflag_debug .eq.1) write(*,*) 'set boundary value 4 current'
        call set_boundary_current
      end if
!
      end subroutine set_bc_id_data
!
!-----------------------------------------------------------------------
!
      end module set_nodal_bc_id_data
