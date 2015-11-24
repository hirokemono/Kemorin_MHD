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
      use m_node_phys_data
      use m_node_phys_address
!
      use m_bc_data_ene
      use m_bc_data_velo
      use m_bc_data_magne
!
      use m_boundary_condition_IDs
      use m_bc_data_list
!
      use count_num_nod_bc_MHD
      use set_bc_phys_id
      use set_boundary_scalars
      use set_velocity_boundary
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call count_num_bc_scl_w_SGS                                     &
     &     (nod_grp1, temp_nod, nod_bc1_t, sgs_bc1_t)
!
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_t)
        call alloc_scalar_nod_bc_type(node1%numnod, sgs_bc1_t)
!
        call set_bc_temp_id
        call set_boundary_scalar(nod_bc1_t, iphys%i_temp, nod_fld1)
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call count_num_bc_velo(nod_grp1, nod_bc1_v, sgs_bc1_v,          &
     &      nod_bc1_rot, nod_bc1_vfree, nod_bc1_vsp, nod_bc1_vr0)
        call count_num_bc_scl_w_SGS                                     &
     &   (nod_grp1, press_nod, nod_bc1_p, sgs_bc1_p)
!
        if ( iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 v'
        call alloc_vector_nod_bc_type(node1%numnod, nod_bc1_v)
        call alloc_vector_nod_bc_type(node1%numnod, sgs_bc1_v)
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_vr0)
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_vfree)
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_vsp)
        call alloc_rotate_nod_bc_type(node1%numnod, nod_bc1_rot)
!
        if (iflag_debug .eq.1)  write(*,*) 'allocate boundary 4 P'
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_p)
        call alloc_scalar_nod_bc_type(node1%numnod, sgs_bc1_p)
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
        call count_num_bc_scl_w_SGS                                     &
     &     (nod_grp1, light_nod, nod_bc1_c, sgs_bc1_c)
!
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_c)
!
        call set_boundary_scalar(nod_bc1_c, iphys%i_light, nod_fld1)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &  .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call count_num_bc_magne(iflag_bc_fixed, nod_grp1,               &
     &      magne_nod, nod_bc1_b)
        call count_num_bc_vect(iflag_bc_sgs, nod_grp1,                  &
     &      magne_nod, sgs_bc1_b)
        call count_num_bc_vect (iflag_bc_fixed, nod_grp1,               &
     &      current_nod, nod_bc1_j)
        call count_num_bc_magp(nod_grp1, nod_bc1_f, sgs_bc1_f)
!
        call alloc_vector_nod_bc_type(node1%numnod, nod_bc1_b)
        call alloc_vector_nod_bc_type(node1%numnod, nod_bc1_j)
        call alloc_vector_nod_bc_type(node1%numnod, sgs_bc1_b)
        call alloc_scalar_nod_bc_type(node1%numnod, nod_bc1_f)
        call alloc_scalar_nod_bc_type(node1%numnod, sgs_bc1_f)
!
        if (iflag_debug.eq.1) write(*,*)  'set boundary ID 4 magne'
        call set_bc_magne_id
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 magne_p'
        call set_bc_m_potential_id
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 current'
        call set_bc_current_id
!
        if (iflag_debug.eq.1)  write(*,*) 'set_boundary_vect magne'
        call set_boundary_vect(nod_bc1_b, iphys%i_magne, nod_fld1)
!
        if (iflag_debug.eq.1) write(*,*) 'set boundary value 4 magne'
        call set_boundary_scalar(nod_bc1_f, iphys%i_m_phi, nod_fld1)
!
        if (iflag_debug.eq.1) write(*,*) 'set_boundary_vect current'
        call set_boundary_vect(nod_bc1_j, iphys%i_current, nod_fld1)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call count_num_bc_vect(iflag_bc_fixed, nod_grp1,                &
     &      a_potential_nod, nod_bc1_a)
        call count_num_bc_vect(iflag_bc_sgs, nod_grp1,                  &
     &      a_potential_nod, sgs_bc1_a)
!
        call alloc_vector_nod_bc_type(node1%numnod, nod_bc1_a)
        call alloc_vector_nod_bc_type(node1%numnod, sgs_bc1_a)
!
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 vect_p'
        call set_bc_vect_p_id
!
        if (iflag_debug .eq.1) write(*,*) 'set_boundary_vect vect_p'
        call set_boundary_vect(nod_bc1_a, iphys%i_vecp, nod_fld1)
      end if
!
      end subroutine set_bc_id_data
!
!-----------------------------------------------------------------------
!
      end module set_nodal_bc_id_data
