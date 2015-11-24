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
      use m_geometry_data_MHD
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
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_vector_layer'
      call set_ele_nodal_bc_vector_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_v)
      call set_ele_nodal_bc_vector_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, sgs_bc1_v)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_rotate'
      call set_ele_nodal_bc_4_rotate                                    &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_rot)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vfree'
      call set_ele_nodal_bc_scalar_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_vfree)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_vr0'
      call set_ele_nodal_bc_scalar_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_vr0)
      if(iflag_debug .gt. 0) write(*,*) 'set_ele_nodal_bc_4_velo_sph'
      call set_ele_nodal_bc_scalar_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_vsp)
!
      call dealloc_vector_ibc_type(nod_bc1_v)
      call dealloc_vector_ibc_type(sgs_bc1_v)
      call dealloc_rotate_ibc_type(nod_bc1_rot)
      call dealloc_scalar_ibc_type(nod_bc1_vfree)
      call dealloc_scalar_ibc_type(nod_bc1_vr0)
      call dealloc_scalar_ibc_type(nod_bc1_vsp)
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
      call set_ele_nodal_bc_vector(node1, ele1, nod_bc1_a)
      call set_ele_nodal_bc_vector(node1, ele1, sgs_bc1_a)
!
      call dealloc_vector_ibc_type(nod_bc1_a)
      call dealloc_vector_ibc_type(sgs_bc1_a)
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
      call set_ele_nodal_bc_vector(node1, ele1, nod_bc1_b)
      call set_ele_nodal_bc_vector(node1, ele1, sgs_bc1_b)
!
      call dealloc_vector_ibc_type(nod_bc1_b)
      call dealloc_vector_ibc_type(sgs_bc1_b)
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
      call set_ele_nodal_bc_vector(node1, ele1, nod_bc1_j)
      call dealloc_vector_ibc_type(nod_bc1_j)
!
      end subroutine set_bc_current_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_temp_id
!
      use m_bc_data_ene
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_temp_id(node1, nod_grp1)
!
!   set node id in an element for the temperature boundary 
!
      call set_ele_nodal_bc_scalar_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_t)
      call set_ele_nodal_bc_scalar_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, sgs_bc1_t)
!
      end subroutine set_bc_temp_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_press_id
!
      use m_bc_data_velo
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_press_id(node1, nod_grp1)
!
!   set node id in an element for the pressure boundary 
!
      call ele_nodal_bc_potential_layer                                 &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_p)
      call dealloc_scalar_ibc_type(nod_bc1_p)
!
      call ele_nodal_bc_potential_layer                                 &
     &   (node1, ele1, iele_fl_start, iele_fl_end, sgs_bc1_p)
      call dealloc_scalar_ibc_type(sgs_bc1_p)
!
      end subroutine set_bc_press_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_m_potential_id
!
      use m_geometry_data_MHD
      use m_bc_data_magne
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_m_potential_id(node1, nod_grp1)
!
!   set node id in an element for the pressure boundary 
!
      call set_ele_nodal_bc_potential(node1, ele1, nod_bc1_f)
!
      call set_ele_nodal_bc_mag_p_layrer(node1, ele1,                   &
     &    iele_ins_start, iele_ins_end, nod_bc1_f, nod_bc1_fins)
      call set_ele_nodal_bc_mag_p_layrer(node1, ele1,                   &
     &    iele_cd_start, iele_cd_end, nod_bc1_f, nod_bc1_fcd)
!
      call dealloc_scalar_ibc_type(nod_bc1_f)
!
!
      call set_ele_nodal_bc_potential(node1, ele1, sgs_bc1_f)
      call dealloc_scalar_ibc_type(sgs_bc1_f)
!
      end subroutine set_bc_m_potential_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_composition_id
!
      use m_bc_data_ene
      use set_bc_scalars
      use set_ele_nod_bc_vectors
!
!
      call set_bc_fixed_composition_id(node1, nod_grp1)
!
!   set node id in an element for composition boundary
!
      call set_ele_nodal_bc_scalar_layer                                &
     &   (node1, ele1, iele_fl_start, iele_fl_end, nod_bc1_c)
!
      end subroutine set_bc_composition_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_phys_id
