!allocate_MHD_AMG_array.f90
!     module allocate_MHD_AMG_array
!
!        programmed by H.Matsui on Dec., 2008
!
!!      subroutine s_allocate_MHD_AMG_array(femmesh, ele_mesh,          &
!!     &          MG_vector, rhs_mat, fem_int, mk_MHD)
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(vectors_4_solver), intent(inout) ::          MG_vector
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(finite_element_integration), intent(inout) :: fem_int
!!        type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
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
      subroutine s_allocate_MHD_AMG_array(femmesh, ele_mesh,            &
     &          MG_vector, rhs_mat, fem_int, mk_MHD)
!
      use m_machine_parameter
      use t_work_FEM_integration
      use t_finite_element_mat
      use t_finite_element_mat_MHD
      use t_vector_for_solver
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
!
      type(vectors_4_solver), intent(inout) ::          MG_vector
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(finite_element_integration), intent(inout) :: fem_int
      type(lumped_mass_mat_layerd), intent(inout) ::    mk_MHD
!
!
      call alloc_fem_mat_base_type                                      &
     &   (femmesh%mesh, ele_mesh%surf, femmesh%group, rhs_mat)
      call alloc_fem_int_base_type(femmesh%mesh, fem_int)
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
