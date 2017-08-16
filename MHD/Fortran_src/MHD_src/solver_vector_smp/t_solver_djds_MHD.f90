!>@file   t_solver_djds_MHD.f90
!!@brief  module t_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine alloc_MHD_MGCG_matrices                              &
!!     &         (i_lev, node, MHD_prop, MHD_mat)
!!      subroutine alloc_MHD_MGCG_zero_matrices                         &
!!     &         (i_lev, MHD_prop, MHD_mat)
!!      subroutine dealloc_MHD_MGCG_matrices(i_lev, MHD_prop, MHD_mat)
!!
      module t_solver_djds_MHD
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
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
      subroutine alloc_MHD_MGCG_matrices                                &
     &         (i_lev, node, MHD_prop, MHD_mat)
!
      use allocate_solver_djds_MHD
!
      integer(kind = kint), intent(in) :: i_lev
      type(node_data), intent(in) :: node
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
!
!
      call alloc_aiccg_matrices                                         &
     &   (node, MHD_prop%fl_prop, MHD_prop%cd_prop,                     &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_mat%MG_DJDS_table(i_lev),  MHD_mat%MG_DJDS_fluid(i_lev),  &
     &    MHD_mat%MG_DJDS_linear(i_lev), MHD_mat%MG_DJDS_lin_fl(i_lev), &
     &    MHD_mat%Vmat_MG_DJDS(i_lev), MHD_mat%Bmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Tmat_MG_DJDS(i_lev), MHD_mat%Cmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Pmat_MG_DJDS(i_lev), MHD_mat%Fmat_MG_DJDS(i_lev))
!
      end subroutine alloc_MHD_MGCG_matrices
!
! ----------------------------------------------------------------------
!
      subroutine alloc_MHD_MGCG_zero_matrices                           &
     &         (i_lev, MHD_prop, MHD_mat)
!
      use allocate_solver_djds_MHD
!
      integer(kind = kint), intent(in) :: i_lev
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
!
      call alloc_MG_zero_matrices                                       &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_mat%Vmat_MG_DJDS(i_lev), MHD_mat%Bmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Tmat_MG_DJDS(i_lev), MHD_mat%Cmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Pmat_MG_DJDS(i_lev), MHD_mat%Fmat_MG_DJDS(i_lev) )
!
      end subroutine alloc_MHD_MGCG_zero_matrices
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_MHD_MGCG_matrices(i_lev, MHD_prop, MHD_mat)
!
      use allocate_solver_djds_MHD
!
      integer(kind = kint), intent(in) :: i_lev
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
!
      call dealloc_aiccg_matrices                                       &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_mat%Vmat_MG_DJDS(i_lev), MHD_mat%Bmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Tmat_MG_DJDS(i_lev), MHD_mat%Cmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Pmat_MG_DJDS(i_lev), MHD_mat%Fmat_MG_DJDS(i_lev))
!
      end subroutine dealloc_MHD_MGCG_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_MHD_MG_DJDS_mat(num_MG_level, MHD_mat)
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
!
      allocate(MHD_mat%Vmat_MG_DJDS(0:num_MG_level))
      allocate(MHD_mat%Bmat_MG_DJDS(0:num_MG_level))
!
      allocate(MHD_mat%Pmat_MG_DJDS(0:num_MG_level))
      allocate(MHD_mat%Fmat_MG_DJDS(0:num_MG_level))
!
      allocate(MHD_mat%Tmat_MG_DJDS(0:num_MG_level))
      allocate(MHD_mat%Cmat_MG_DJDS(0:num_MG_level))
!
!
      allocate(MHD_mat%MG_DJDS_table(0:num_MG_level))
      allocate(MHD_mat%MG_DJDS_linear(0:num_MG_level))
      allocate(MHD_mat%MG_comm_table(0:num_MG_level))
!
      allocate(MHD_mat%MG_DJDS_fluid(0:num_MG_level))
      allocate(MHD_mat%MG_DJDS_lin_fl(0:num_MG_level))
      allocate(MHD_mat%MG_comm_fluid(0:num_MG_level))
!
      allocate(MHD_mat%MG_DJDS_conduct(0:num_MG_level))
!
      allocate(MHD_mat%MG_mat_tbls(0:num_MG_level))
!
      allocate(MHD_mat%MG_interpolate(num_MG_level))
!
      end subroutine alloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_MHD_MG_DJDS_mat(MHD_mat)
!
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
!
      deallocate(MHD_mat%Vmat_MG_DJDS, MHD_mat%Bmat_MG_DJDS)
      deallocate(MHD_mat%Pmat_MG_DJDS, MHD_mat%Fmat_MG_DJDS)
      deallocate(MHD_mat%Tmat_MG_DJDS, MHD_mat%Cmat_MG_DJDS)
!
      deallocate(MHD_mat%MG_DJDS_table, MHD_mat%MG_DJDS_linear)
      deallocate(MHD_mat%MG_comm_table)
      deallocate(MHD_mat%MG_DJDS_fluid, MHD_mat%MG_DJDS_lin_fl)
      deallocate(MHD_mat%MG_comm_fluid)
!
      deallocate(MHD_mat%MG_DJDS_conduct)
!
      deallocate(MHD_mat%MG_mat_tbls, MHD_mat%MG_interpolate)
!
      end subroutine dealloc_MHD_MG_DJDS_mat
!
!-----------------------------------------------------------------------
!
      end module t_solver_djds_MHD