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
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_nodal_bc_data
      use t_finite_element_mat
      use t_table_FEM_const
      use t_solver_djds
      use t_surface_bc_data
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
      use m_geometry_data
      use m_group_data
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_sorted_node_MHD
      use m_finite_element_matrix
      use m_bc_data_velo
      use m_bc_data_magne
      use m_bc_data_ene
      use m_surf_data_torque
      use m_solver_djds_MHD
!
      use set_aiccg_bc_fixed
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod                                    &
     &   (num_t_linear, ele1, nod_bc1_p, DJDS_fl_l, Pmat_DJDS)
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call set_aiccg_bc_velo(intg_point_t_evo, ele1, surf1,         &
     &        sf_grp1, nod_bc1_v, nod_bc1_rot, sf_bc1_free_sph_in,      &
     &        sf_bc1_free_sph_out, jac1_sf_grp_2d_q,                    &
     &        rhs_tbl1, mat_tbl_fl_q, DJDS_fluid, fem1_wk, Vmat_DJDS)
        end if
      end if
!

      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod                                    &
     &     (ele1%nnod_4_ele, ele1, nod_bc1_t, DJDS_fluid, Tmat_DJDS)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call set_aiccg_bc_scalar_nod                                    &
     &     (ele1%nnod_4_ele, ele1, nod_bc1_c, DJDS_fluid, Cmat_DJDS)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod                                    &
     &     (num_t_linear, ele1, nod_bc1_f, DJDS_linear, Fmat_DJDS)
!
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call set_aiccg_bc_vector_nod                                  &
     &       (ele1, nod_bc1_b, DJDS_entire, Bmat_DJDS)
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call set_aiccg_bc_scalar_nod                                    &
     &     (num_t_linear, ele1, nod_bc1_f, DJDS_linear, Fmat_DJDS)
!
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call set_aiccg_bc_vector_nod                                  &
     &       (ele1, nod_bc1_a, DJDS_entire, Bmat_DJDS)
        end if
      end if
!
!
      end subroutine set_aiccg_bc_phys
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo(num_int, ele, surf, sf_grp,          &
     &          nod_bc_v, nod_bc_rot, free_in_sf, free_out_sf,          &
     &          jac_sf_grp, rhs_tbl, mat_tbl, DJDS_tbl, fem_wk,         &
     &          Vmat_DJDS)
!
      use set_aiccg_free_sph
      use set_aiccg_bc_fixed
!
      integer(kind = kint), intent(in) :: num_int
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc_v
      type(scaler_rotaion_nod_bc_type), intent(in) :: nod_bc_rot
      type(scaler_surf_bc_data_type), intent(in) :: free_in_sf
      type(scaler_surf_bc_data_type), intent(in) :: free_out_sf
!
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: Vmat_DJDS
!
!
!      matrix setting for free slip on sphere
      call set_aiccg_bc_free_sph_in(ele, surf, sf_grp,                  &
     &    free_in_sf, jac_sf_grp, rhs_tbl, mat_tbl,                     &
     &    num_int, fem_wk, Vmat_DJDS)
      call set_aiccg_bc_free_sph_out(ele, surf, sf_grp,                 &
     &    free_out_sf, jac_sf_grp, rhs_tbl, mat_tbl,                    &
     &    num_int, fem_wk, Vmat_DJDS)
!
!      matrix setting for fixed boundaries
      call set_aiccg_bc_vector_nod(ele, nod_bc_v, DJDS_tbl, Vmat_DJDS)
!
!        write(*,*) '  velo_bc_rotation'
      if ( nod_bc_rot%num_idx_ibc .ne. 0 ) then
        call set_aiccg_bc_velo_rot                                      &
     &     (ele, nod_bc_rot, DJDS_tbl, Vmat_DJDS)
      end if
!
      end subroutine set_aiccg_bc_velo
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_vectors
