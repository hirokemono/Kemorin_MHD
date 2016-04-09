!>@file   t_MHD_matrices_pack.f90
!!@brief  module t_MHD_matrices_pack
!!
!!@author H. Matsui
!!@date Programmed in Apr.., 2006
!
!>     Matrix package by fields for MHD dynamo
!!
!!      subroutine link_MG_DJDS_MHD_structures                          &
!!     &         (num_MG_level, MHD_matrices, s_package)
!!        type(MHD_MG_matrices), intent(in) :: MHD_matrices
!!        type(MHD_matrices_pack), intent(inout) :: s_package
!!
      module t_MHD_matrices_pack
!
      use m_precision
      use t_solver_djds_MHD
!
      implicit none
!
!
!>        Structure of matrices for all fields
      type MHD_matrices_pack
!>        Structure of matrices for velocity integration
        type(MHD_MG_matrix) :: Vmatrix
!>        Structure of matrices for pressure integration
        type(MHD_MG_matrix) :: Pmatrix
!>        Structure of matrices for magnetic  integration
        type(MHD_MG_matrix) :: Bmatrix
!>        Structure of matrices for electric potantial integration
        type(MHD_MG_matrix) :: Fmatrix
!>        Structure of matrices for temperature integration
        type(MHD_MG_matrix) :: Tmatrix
!>        Structure of matrices for composition integration
        type(MHD_MG_matrix) :: Cmatrix
      end type MHD_matrices_pack
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_MG_DJDS_MHD_structures                            &
     &         (num_MG_level, MHD_matrices, s_package)
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MHD_MG_matrices), intent(in) :: MHD_matrices
      type(MHD_matrices_pack), intent(inout) :: s_package
!
!
      s_package%Vmatrix%nlevel_MG =       num_MG_level
      s_package%Vmatrix%mat_MG_DJDS =>    MHD_matrices%Vmat_MG_DJDS
      s_package%Vmatrix%MG_DJDS_table =>  MHD_matrices%MG_DJDS_fluid
      s_package%Vmatrix%MG_comm_table =>  MHD_matrices%MG_comm_fluid
      s_package%Vmatrix%MG_mat_tbls =>    MHD_matrices%MG_mat_tbls
      s_package%Vmatrix%MG_interpolate => MHD_matrices%MG_interpolate
!
!
      s_package%Pmatrix%nlevel_MG =       num_MG_level
      s_package%Pmatrix%mat_MG_DJDS =>    MHD_matrices%Pmat_MG_DJDS
      s_package%Pmatrix%MG_DJDS_table =>  MHD_matrices%MG_DJDS_lin_fl
      s_package%Pmatrix%MG_comm_table =>  MHD_matrices%MG_comm_fluid
      s_package%Pmatrix%MG_mat_tbls =>    MHD_matrices%MG_mat_tbls
      s_package%Pmatrix%MG_interpolate => MHD_matrices%MG_interpolate
!
!
      s_package%Bmatrix%nlevel_MG =       num_MG_level
      s_package%Bmatrix%mat_MG_DJDS =>    MHD_matrices%Bmat_MG_DJDS
      s_package%Bmatrix%MG_DJDS_table =>  MHD_matrices%MG_DJDS_table
      s_package%Bmatrix%MG_comm_table =>  MHD_matrices%MG_comm_table
      s_package%Bmatrix%MG_mat_tbls =>    MHD_matrices%MG_mat_tbls
      s_package%Bmatrix%MG_interpolate => MHD_matrices%MG_interpolate
!
!
      s_package%Fmatrix%nlevel_MG =       num_MG_level
      s_package%Fmatrix%mat_MG_DJDS =>    MHD_matrices%Fmat_MG_DJDS
      s_package%Fmatrix%MG_DJDS_table =>  MHD_matrices%MG_DJDS_linear
      s_package%Fmatrix%MG_comm_table =>  MHD_matrices%MG_comm_table
      s_package%Fmatrix%MG_mat_tbls =>    MHD_matrices%MG_mat_tbls
      s_package%Fmatrix%MG_interpolate => MHD_matrices%MG_interpolate
!
!
      s_package%Tmatrix%nlevel_MG =       num_MG_level
      s_package%Tmatrix%mat_MG_DJDS =>    MHD_matrices%Tmat_MG_DJDS
      s_package%Tmatrix%MG_DJDS_table =>  MHD_matrices%MG_DJDS_fluid
      s_package%Tmatrix%MG_comm_table =>  MHD_matrices%MG_comm_fluid
      s_package%Tmatrix%MG_mat_tbls =>    MHD_matrices%MG_mat_tbls
      s_package%Tmatrix%MG_interpolate => MHD_matrices%MG_interpolate
!
!
      s_package%Cmatrix%nlevel_MG =       num_MG_level
      s_package%Cmatrix%mat_MG_DJDS =>    MHD_matrices%Cmat_MG_DJDS
      s_package%Cmatrix%MG_DJDS_table =>  MHD_matrices%MG_DJDS_fluid
      s_package%Cmatrix%MG_comm_table =>  MHD_matrices%MG_comm_fluid
      s_package%Cmatrix%MG_mat_tbls =>    MHD_matrices%MG_mat_tbls
      s_package%Cmatrix%MG_interpolate => MHD_matrices%MG_interpolate
!
      end subroutine link_MG_DJDS_MHD_structures
!
!-----------------------------------------------------------------------
!
      end module t_MHD_matrices_pack
