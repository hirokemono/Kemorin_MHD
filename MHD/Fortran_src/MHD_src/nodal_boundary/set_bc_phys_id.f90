!set_bc_phys_id.f90
!      module set_bc_phys_id
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov., 2003
!        modified by H.Matsui on Oct., 2005
!
!      subroutine set_bc_velo_id
!      subroutine set_bc_vect_p_id
!      subroutine set_bc_magne_id
!      subroutine set_bc_current_id
!      subroutine set_bc_temp_id
!      subroutine set_bc_press_id
!      subroutine set_bc_m_potential_id
!      subroutine set_bc_composition_id
!
      module set_bc_phys_id
!
      use calypso_mpi
      use m_precision
      use m_machine_parameter
      use m_geometry_data
      use m_group_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_velo_id
!
      use m_bc_data_velo
      use set_bc_vectors
      use set_bc_scalars
!
      use set_ele_nod_bc_scalars
      use set_ele_nod_bc_vectors
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_bc_fixed_velo_id'
      call set_bc_fixed_velo_id(node1, nod_grp1)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_bc_velo_4_sphere_id'
      call set_bc_velo_4_sphere_id(node1, nod_grp1)
!
!   set node id in an element for velocity boundary 
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vect_fl'
      call set_ele_nodal_bc_4_vect_fl(node1, ele1, nod_bc1_v)
      call set_ele_nodal_bc_4_vect_fl(node1, ele1, sgs_bc1_v)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_rotate'
      call set_ele_nodal_bc_4_rotate
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vfree'
      call set_ele_nodal_bc_4_vfree
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vr0'
      call set_ele_nodal_bc_4_vr0
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_velo_sph'
      call set_ele_nodal_bc_4_velo_sph
!
      end subroutine set_bc_velo_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_vect_p_id
!
      use m_bc_data_magne
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_vect_p_id(node1, nod_grp1)
!
!   set node id in an element for magnetic boundary 
!
      call set_ele_nodal_bc_4_vect(node1, ele1, nod_bc1_a)
      call set_ele_nodal_bc_4_vect(node1, ele1, sgs_bc1_a)
!
      end subroutine set_bc_vect_p_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_magne_id
!
      use m_bc_data_magne
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_magne_id(node1, nod_grp1)
!
!   set node id in an element for magnetic boundary 
!
      call set_ele_nodal_bc_4_vect(node1, ele1, nod_bc1_b)
      call set_ele_nodal_bc_4_vect(node1, ele1, sgs_bc1_b)
!
      end subroutine set_bc_magne_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_current_id
!
      use m_bc_data_magne
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_current_id(node1, nod_grp1)
!
      call set_ele_nodal_bc_4_vect(node1, ele1, nod_bc1_j)
!
      end subroutine set_bc_current_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_temp_id
!
      use set_bc_scalars
      use set_ele_nod_bc_scalars
!
!
      call set_bc_fixed_temp_id(node1, nod_grp1)
!
!   set node id in an element for the temperature boundary 
!
      call set_ele_nodal_bc_4_temp
      call set_ele_nodal_bc_4_temp_sgs
!
!
      end subroutine set_bc_temp_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_press_id
!
      use set_bc_scalars
      use set_ele_nod_bc_potential
!
!
      call set_bc_fixed_press_id(node1, nod_grp1)
!
!   set node id in an element for the pressure boundary 
!
      call set_ele_nodal_bc_4_press
      call set_ele_nodal_bc_4_press_sgs
!
      end subroutine set_bc_press_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_m_potential_id
!
      use set_bc_scalars
      use set_ele_nod_bc_potential
!
!
      call set_bc_fixed_m_potential_id(node1, nod_grp1)
!
!   set node id in an element for the pressure boundary 
!
      call set_ele_nodal_bc_4_magne_p
      call set_ele_nodal_bc_4_mag_p_sgs
      call set_ele_nodal_bc_4_mag_p_ins
      call set_ele_nodal_bc_4_mag_p_cd
!
      end subroutine set_bc_m_potential_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_composition_id
!
      use m_bc_data_magne
      use set_bc_scalars
      use set_ele_nod_bc_scalars
!
!
      call set_bc_fixed_composition_id(node1, nod_grp1)
!
!   set node id in an element for composition boundary 
!
      call set_ele_nodal_bc_4_composition
!
      end subroutine set_bc_composition_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_phys_id
