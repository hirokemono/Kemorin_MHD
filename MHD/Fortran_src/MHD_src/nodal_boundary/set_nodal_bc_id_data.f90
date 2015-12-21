!set_nodal_bc_id_data.f90
!     module set_nodal_bc_id_data
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine set_bc_id_data
!!
!!      subroutine set_boundary_velo
!!      subroutine set_boundary_velo_4_rhs
!!      subroutine delete_field_by_fixed_v_bc(i_field)
!
      module set_nodal_bc_id_data
!
      use m_precision
      use m_constants
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
!
      use m_bc_data_ene
      use m_bc_data_velo
      use m_bc_data_magne
!
      use m_boundary_condition_IDs
      use m_bc_data_list
!
      use count_num_nod_bc_MHD
      use set_boundary_scalars
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 v'
        call set_bc_velo_id(node1, ele1, nod_grp1)
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 P'
        call set_bc_press_id(node1, ele1, nod_grp1, iphys, nod_fld1)
        if ( iflag_debug .eq.1) write(*,*)  'set boundary values 4 v'
        call set_boundary_velo
      end if
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_bc_temp_id(node1, ele1, nod_grp1, iphys, nod_fld1)
!
        call set_boundary_scalar(nod_bc1_t, iphys%i_temp, nod_fld1)
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_bc_composition_id(node1, ele1, nod_grp1)
!
        call set_boundary_scalar(nod_bc1_c, iphys%i_light, nod_fld1)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &  .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*)  'set boundary ID 4 magne'
        call set_bc_magne_id(node1, ele1, nod_grp1)
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 magne_p'
        call set_bc_m_potential_id(node1, ele1, nod_grp1)
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 current'
        call set_bc_current_id(node1, ele1, nod_grp1)
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
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 vect_p'
        call set_bc_vect_p_id(node1, ele1, nod_grp1)
!
        if (iflag_debug .eq.1) write(*,*) 'set_boundary_vect vect_p'
        call set_boundary_vect(nod_bc1_a, iphys%i_vecp, nod_fld1)
      end if
!
      end subroutine set_bc_id_data
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo
!
      use m_geometry_data
      use m_group_data
!
      use m_control_parameter
      use m_node_phys_data
!
      use m_bc_data_velo
!
      use set_nodal_bc_4_velo
      use set_boundary_scalars
      use set_fixed_boundaries
!
!     set fixed velocity
!
      call set_boundary_vect(nod_bc1_v, iphys%i_velo, nod_fld1)
!
!   set rotation boundary
      call set_boundary_rot_vect                                        &
     &   (node1, nod_bc1_rot, iphys%i_velo, nod_fld1)
!
!   boundary condition for special case
!     ( please write every time!!)
      call set_boundary_specific_vect                                   &
     &   (node1, nod_bc1_vsp, iphys%i_velo, nod_fld1)
!
!
      call delete_radial_vector_on_bc                                   &
     &   (node1, nod_bc1_vr0, iphys%i_velo, nod_fld1)
!
      end subroutine set_boundary_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo_4_rhs
!
      use m_geometry_data
      use m_finite_element_matrix
      use m_bc_data_velo
!
      use set_boundary_scalars
      use set_fixed_boundaries
!
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_v, f1_l, f1_nl)
      call delete_vector_ffs_rot_bc(node1, nod_bc1_rot, f1_l, f1_nl)
      call set_vector_ffs_special_bc(node1, nod_bc1_vsp, f1_l)
!
      end subroutine set_boundary_velo_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_v_bc(i_field)
!
      use m_node_phys_data
      use m_bc_data_velo
      use set_boundary_scalars
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call delete_vector_on_bc(nod_bc1_v, i_field, nod_fld1)
      call delete_vector_by_rot_v_bc(nod_bc1_rot, i_field, nod_fld1)
      call delete_vector_by_fixed_t_bc(nod_bc1_vsp, i_field, nod_fld1)
!
      end subroutine delete_field_by_fixed_v_bc
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_bc_id_data
