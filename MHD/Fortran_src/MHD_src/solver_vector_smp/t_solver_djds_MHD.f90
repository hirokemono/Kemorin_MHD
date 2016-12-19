!>@file   t_solver_djds_MHD.f90
!!@brief  module t_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!
      module t_solver_djds_MHD
!
      use m_precision
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
      use t_interpolate_table
      use t_sorted_node_MHD
!
      implicit none
!
!
!>        Structure of matrices for MHD dynamo simulation
      type MHD_MG_matrices
!>        Structure of matrix for time evolution of velocity
        type(DJDS_MATRIX), pointer :: Vmat_MG_DJDS(:)
!>        Structure of matrix for time evolution of magnetic field
        type(DJDS_MATRIX), pointer :: Bmat_MG_DJDS(:)
!
!>        Structure of matrix for  pressure
        type(DJDS_MATRIX), pointer :: Pmat_MG_DJDS(:)
!>        Structure of matrix for  scalar potential
        type(DJDS_MATRIX), pointer :: Fmat_MG_DJDS(:)
!
!>        Structure of matrix for time evolution of temperature
        type(DJDS_MATRIX), pointer :: Tmat_MG_DJDS(:)
!>        Structure of matrix for time evolution of composition
        type(DJDS_MATRIX), pointer :: Cmat_MG_DJDS(:)
!
!>        DJDS ordering structures for entire domain
        type(DJDS_ordering_table), pointer :: MG_DJDS_table(:)
!>        DJDS ordering structures for linear entire domain
        type(DJDS_ordering_table), pointer :: MG_DJDS_linear(:)
!
!>        DJDS ordering structures for fluid
        type(DJDS_ordering_table), pointer :: MG_DJDS_fluid(:)
!>        DJDS ordering structures for linear fluid
        type(DJDS_ordering_table), pointer :: MG_DJDS_lin_fl(:)
!>        Communication table structure for entire domain
!
        type(communication_table), pointer :: MG_comm_table(:)
!>        Communication table structure for entire domain
        type(communication_table), pointer :: MG_comm_fluid(:)
!
!>        DJDS ordering structures for fluid
        type(DJDS_ordering_table), pointer :: MG_DJDS_conduct(:)
!
!>        Structures for FEM marix table
        type(tables_MHD_mat_const), pointer :: MG_mat_tbls(:)
!
!>        interpolation table structure for multigrid
        type(MG_itp_table), pointer :: MG_interpolate(:)
      end type MHD_MG_matrices
!
      type MHD_MG_matrix
        integer(kind = kint) :: nlevel_MG
!>        Structure of matrix for time evolution of velocity
        type(DJDS_MATRIX), pointer :: mat_MG_DJDS(:)
!>        DJDS ordering structures for entire domain
        type(DJDS_ordering_table), pointer :: MG_DJDS_table(:)
!
!>        Communication table structure for entire domain
        type(communication_table), pointer :: MG_comm_table(:)
!
!>        Structures for FEM marix table
        type(tables_MHD_mat_const), pointer :: MG_mat_tbls(:)
!
!>        interpolation table structure for multigrid
        type(MG_itp_table), pointer :: MG_interpolate(:)
      end type MHD_MG_matrix
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_MHD_MG_DJDS_mat(num_MG_level, matrices)
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MHD_MG_matrices), intent(inout) :: matrices
!
!
      allocate(matrices%Vmat_MG_DJDS(0:num_MG_level))
      allocate(matrices%Bmat_MG_DJDS(0:num_MG_level))
!
      allocate(matrices%Pmat_MG_DJDS(0:num_MG_level))
      allocate(matrices%Fmat_MG_DJDS(0:num_MG_level))
!
      allocate(matrices%Tmat_MG_DJDS(0:num_MG_level))
      allocate(matrices%Cmat_MG_DJDS(0:num_MG_level))
!
!
      allocate(matrices%MG_DJDS_table(0:num_MG_level))
      allocate(matrices%MG_DJDS_linear(0:num_MG_level))
      allocate(matrices%MG_comm_table(0:num_MG_level))
!
      allocate(matrices%MG_DJDS_fluid(0:num_MG_level))
      allocate(matrices%MG_DJDS_lin_fl(0:num_MG_level))
      allocate(matrices%MG_comm_fluid(0:num_MG_level))
!
      allocate(matrices%MG_DJDS_conduct(0:num_MG_level))
!
      allocate(matrices%MG_mat_tbls(0:num_MG_level))
!
      allocate(matrices%MG_interpolate(num_MG_level))
!
      end subroutine alloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_MHD_MG_DJDS_mat(matrices)
!
      type(MHD_MG_matrices), intent(inout) :: matrices
!
!
      deallocate(matrices%Vmat_MG_DJDS, matrices%Bmat_MG_DJDS)
      deallocate(matrices%Pmat_MG_DJDS, matrices%Fmat_MG_DJDS)
      deallocate(matrices%Tmat_MG_DJDS, matrices%Cmat_MG_DJDS)
!
      deallocate(matrices%MG_DJDS_table, matrices%MG_DJDS_linear)
      deallocate(matrices%MG_comm_table)
      deallocate(matrices%MG_DJDS_fluid, matrices%MG_DJDS_lin_fl)
      deallocate(matrices%MG_comm_fluid)
!
      deallocate(matrices%MG_DJDS_conduct)
!
      deallocate(matrices%MG_mat_tbls, matrices%MG_interpolate)
!
      end subroutine dealloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_aiccg_matrices                                   &
     &          (node, djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fll, &
     &           mat_velo, mat_magne, mat_temp, mat_light,              &
     &           mat_press, mat_magp)
!
      use m_control_parameter
      use t_geometry_data
      use t_geometry_data_MHD
!
      type(node_data), intent(in) :: node
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fll
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( evo_velo%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      djds_tbl_fll, mat_press)
!
        if ( evo_velo%iflag_scheme .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        djds_tbl_fl, mat_velo)
        end if
      end if
!
      if ( evo_temp%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      djds_tbl_fl, mat_temp)
      end if
!
      if ( evo_comp%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &       djds_tbl_fl, mat_light)
      end if
!
      if ( evo_magne%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &       djds_tbl_l, mat_magp)
!
        if ( evo_magne%iflag_scheme .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &       djds_tbl, mat_magne)
        end if
      end if
!
!
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      djds_tbl_l, mat_magp)
!
        if (evo_vect_p%iflag_scheme .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        djds_tbl, mat_magne)
        end if
      end if
!
      end subroutine alloc_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      subroutine alloc_MG_zero_matrices(mat_velo, mat_magne, mat_temp,  &
     &          mat_light, mat_press, mat_magp)
!
      use m_control_parameter
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( evo_velo%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_press)
!
        if ( evo_velo%iflag_scheme .ge. id_Crank_nicolson) then
          call alloc_type_zero_mat(mat_velo)
        end if
      end if
!
      if ( evo_temp%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_temp)
      end if
!
      if(evo_magne%iflag_scheme .gt. id_no_evolution                    &
     &     .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_magp)
      end if
!
      if ( evo_magne%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_magne)
      end if
!
      if ( evo_vect_p%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_magne)
      end if
!
      if ( evo_comp%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_light)
      end if
!
      end subroutine alloc_MG_zero_matrices
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_aiccg_matrices(mat_velo, mat_magne, mat_temp,  &
     &          mat_light, mat_press, mat_magp)
!
      use m_control_parameter
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        call dealloc_type_djds_mat(mat_press)
!
        if (evo_velo%iflag_scheme .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(mat_velo)
        end if
      end if
!
      if (evo_temp%iflag_scheme .ge. id_Crank_nicolson) then
         call dealloc_type_djds_mat(mat_temp)
      end if
!
      if (evo_comp%iflag_scheme .ge. id_Crank_nicolson) then
        call dealloc_type_djds_mat(mat_light)
      end if
!
      if(     evo_magne%iflag_scheme .gt. id_no_evolution               &
     &   .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call dealloc_type_djds_mat(mat_magp)
        if(    evo_magne%iflag_scheme .ge. id_Crank_nicolson            &
     &    .or. evo_vect_p%iflag_scheme .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(mat_magne)
        end if
      end if
!
      end subroutine dealloc_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      end module t_solver_djds_MHD