!allocate_MHD_AMG_array.f90
!     module allocate_MHD_AMG_array
!
!        programmed by H.Matsui on Dec., 2008
!
!      subroutine s_allocate_MHD_AMG_array(femmesh, MG_vector, rhs_mat, &
!     &          MG_mk_MHD)
!      subroutine alloc_aiccg_matrices(femmesh, djds_tbl, djds_tbl_fl,  &
!     &           mat_velo, mat_magne, mat_temp, mat_d_scalar,          &
!     &           mat_press, mat_magp)
!      subroutine alloc_MG_zero_matrices(mat_velo, mat_magne, mat_temp, &
!     &          mat_d_scalar, mat_press, mat_magp)
!
      module allocate_MHD_AMG_array
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_mesh_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_allocate_MHD_AMG_array(femmesh, MG_vector, rhs_mat,  &
     &           mk_MHD)
!
      use m_machine_parameter
      use t_work_FEM_integration
      use t_finite_element_mat
      use t_finite_element_mat_MHD
      use t_vector_for_solver
!
      type(mesh_data), intent(in) :: femmesh
!
      type(vectors_4_solver), intent(inout) ::          MG_vector
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      call alloc_fem_mat_base_type                                      &
     &   (femmesh%mesh%node, femmesh%mesh%ele, rhs_mat)
      call alloc_fem_mat_fluid_type(femmesh%mesh%node%numnod,           &
     &    mk_MHD)
      call alloc_fem_mat_conduct_type(femmesh%mesh%node%numnod,         &
     &    mk_MHD)
!
      if (iflag_debug.ge.1 ) write(*,*) 'alloc_iccgN_vec_type'
      MG_vector%isize_solver_vect = -1
      call alloc_iccgN_vec_type                                         &
     &           (isix, femmesh%mesh%node%numnod, MG_vector)
!
      end subroutine s_allocate_MHD_AMG_array
!
! ----------------------------------------------------------------------
!
      end module allocate_MHD_AMG_array
