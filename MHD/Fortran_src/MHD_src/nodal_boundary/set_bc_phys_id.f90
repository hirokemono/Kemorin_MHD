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
      use m_precision
      use m_geometry_parameter
      use m_node_group
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
      use set_bc_vectors
      use set_bc_scalars
!
      use set_ele_nod_bc_scalars
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_velo_id(nod_grp1)
!
      call set_bc_velo_4_sphere_id(numnod, nod_grp1)
!
!   set node id in an element for velocity boundary 
!
      call set_ele_nodal_bc_4_velo
      call set_ele_nodal_bc_4_velo_sgs
!
      call set_ele_nodal_bc_4_rotate
      call set_ele_nodal_bc_4_vfree
      call set_ele_nodal_bc_4_vr0
      call set_ele_nodal_bc_4_velo_sph
!
      end subroutine set_bc_velo_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_vect_p_id
!
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_vect_p_id(nod_grp1)
!
!   set node id in an element for magnetic boundary 
!
      call set_ele_nodal_bc_4_vect_p
      call set_ele_nodal_bc_4_vecp_sgs
!
      end subroutine set_bc_vect_p_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_magne_id
!
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_magne_id(nod_grp1)
!
!   set node id in an element for magnetic boundary 
!
      call set_ele_nodal_bc_4_magne
      call set_ele_nodal_bc_4_mag_sgs
!
      end subroutine set_bc_magne_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_current_id
!
      use set_bc_vectors
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_current_id(nod_grp1)
!
      call set_ele_nodal_bc_4_current
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
      call set_bc_fixed_temp_id(numnod, nod_grp1)
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
      call set_bc_fixed_press_id(numnod, nod_grp1)
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
      call set_bc_fixed_m_potential_id(numnod, nod_grp1)
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
      use set_bc_scalars
      use set_ele_nod_bc_scalars
!
!
      call set_bc_fixed_composition_id(numnod, nod_grp1)
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
