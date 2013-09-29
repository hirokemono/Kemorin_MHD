!allocate_MHD_AMG_array.f90
!     module allocate_MHD_AMG_array
!
!        programmed by H.Matsui on Dec., 2008
!
!      subroutine s_allocate_MHD_AMG_array(femmesh, MG_vector, rhs_mat, &
!     &          MG_mk_MHD)
!      subroutine alloc_MG_AMG_matrices(femmesh, djds_tbl, djds_tbl_fl, &
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
      call alloc_fem_mat_base_type(femmesh%mesh%node%numnod,            &
     &    femmesh%mesh%ele%numele, femmesh%mesh%ele%nnod_4_ele,         &
     &    np_smp, femmesh%mesh%node%max_nod_smp, rhs_mat)
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
      subroutine alloc_MG_AMG_matrices(femmesh, djds_tbl, djds_tbl_fl,  &
     &           mat_velo, mat_magne, mat_temp, mat_d_scalar,           &
     &           mat_press, mat_magp)
!
      use m_control_parameter
      use t_geometry_data_MHD
      use t_solver_djds
!
      type(mesh_data), intent(in) :: femmesh
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_type_djds11_mat(femmesh%mesh%node%numnod,            &
     &      femmesh%mesh%node%internal_node, djds_tbl_fl, mat_press)
!
        if ( iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(femmesh%mesh%node%numnod,          &
     &        femmesh%mesh%node%internal_node, djds_tbl_fl, mat_velo)
        end if
      end if
!
      if ( iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(femmesh%mesh%node%numnod,            &
     &      femmesh%mesh%node%internal_node, djds_tbl_fl, mat_temp)
      end if
!
      if ( iflag_t_evo_4_magne .ge. id_no_evolution) then
        call alloc_type_djds11_mat(femmesh%mesh%node%numnod,            &
     &       femmesh%mesh%node%internal_node, djds_tbl, mat_magp)
!
        if ( iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(femmesh%mesh%node%numnod,          &
     &       femmesh%mesh%node%internal_node, djds_tbl, mat_magne)
        end if
      end if
!
!
      if ( iflag_t_evo_4_vect_p .ge. id_no_evolution) then
        call alloc_type_djds11_mat(femmesh%mesh%node%numnod,            &
     &       femmesh%mesh%node%internal_node, djds_tbl, mat_magp)
!
        if ( iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(femmesh%mesh%node%numnod,          &
     &       femmesh%mesh%node%internal_node, djds_tbl, mat_magne)
        end if
      end if
!
      if ( iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(femmesh%mesh%node%numnod,            &
     &       femmesh%mesh%node%internal_node, djds_tbl_fl,              &
     &       mat_d_scalar)
      end if
!
      end subroutine alloc_MG_AMG_matrices
!
! ----------------------------------------------------------------------
!
      subroutine alloc_MG_zero_matrices(mat_velo, mat_magne, mat_temp,  &
     &          mat_d_scalar, mat_press, mat_magp)
!
      use m_control_parameter
      use t_solver_djds
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_press)
!
        if ( iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call alloc_type_zero_mat(mat_velo)
        end if
      end if
!
      if ( iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_temp)
      end if
!
      if(iflag_t_evo_4_magne .gt. id_no_evolution                       &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_magp)
      end if
!
      if ( iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_magne)
      end if
!
      if ( iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_magne)
      end if
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_d_scalar)
      end if
!
      end subroutine alloc_MG_zero_matrices
!
! ----------------------------------------------------------------------
!
      end module allocate_MHD_AMG_array
