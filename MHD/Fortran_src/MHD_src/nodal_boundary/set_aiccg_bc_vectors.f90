!set_aiccg_bc_vectors.f90
!      module set_aiccg_bc_vectors
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on Nov. 2003
!        modified by H. Matsui on Oct. 2005
!        modified by H. Matsui on Feb. 2009
!
!      subroutine set_aiccg_bc_phys
!
      module set_aiccg_bc_vectors
!
      use m_precision
!
      implicit none
!
      private :: set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_phys
!
      use calypso_mpi
      use m_control_parameter
!
      use set_aiccg_bc_fixed
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_aiccg_bc_press_nod
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call set_aiccg_bc_velo
        end if
      end if
!

      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call set_aiccg_bc_temp_nod
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call set_aiccg_bc_composition_nod
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call set_aiccg_bc_mag_p_nod
!
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call set_aiccg_bc_magne_nod
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_aiccg_bc_mag_p_nod
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call set_aiccg_bc_vecp_nod
        end if
      end if
!
!
      end subroutine set_aiccg_bc_phys
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo
!
      use m_control_parameter
      use m_geometry_data
      use m_group_data
      use m_jacobian_sf_grp
      use m_sorted_node
      use m_sorted_node_MHD
      use m_finite_element_matrix
      use m_bc_data_rotate
      use m_velo_matrix
!
      use set_aiccg_free_sph
      use set_aiccg_bc_fixed
!
!
!      matrix setting for free slip on sphere
      if(ngrp_sf_fr_in .gt. 0)  then
        call set_aiccg_bc_free_sph_in(ele1, surf1, sf_grp1,             &
     &      jac1_sf_grp_2d_q, rhs_tbl1, mat_tbl_fl_q,                   &
     &      intg_point_poisson, fem1_wk, Vmat_DJDS)
      end if
      if(ngrp_sf_fr_out .gt. 0) then
        call set_aiccg_bc_free_sph_out(ele1, surf1, sf_grp1,            &
     &      jac1_sf_grp_2d_q, rhs_tbl1, mat_tbl_fl_q,                   &
     &      intg_point_poisson, fem1_wk, Vmat_DJDS)
      end if
!
!      matrix setting for fixed boundaries
      call set_aiccg_bc_velo_nod
!
!
!        write(*,*) '  velo_bc_rotation'
      if ( num_index_ibc_vrot .ne. 0 ) then
       call set_aiccg_bc_velo_rot
      end if
!
      end subroutine set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_vectors
