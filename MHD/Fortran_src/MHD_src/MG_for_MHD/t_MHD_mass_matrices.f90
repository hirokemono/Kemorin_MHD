!t_MHD_mass_matrices.f90
!     module t_MHD_mass_matrices
!
!     Written by H. Matsui on Dec., 2008
!
! 
!>   @brief Structure for FEM assemble for MHD dynamo
!
!!      subroutine alloc_mass_mat_fluid(node, mk_MHD)
!!        type(node_data), intent(in) :: node
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!!      subroutine alloc_mass_mat_conduct(node, mk_MHD)
!!        type(node_data), intent(in) :: node
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!!
!!      subroutine dealloc_mass_mat_conduct(mk_MHD)
!!      subroutine dealloc_fem_mat_conduct_type(mk_MHD)
!!
!!      subroutine check_mass_martix_fluid(id_rank, node, mhd_fem_wk)
!!      subroutine check_mass_martix_conduct(id_rank, node, mhd_fem_wk)
!!      subroutine check_mass_martix_insulate(id_rank, node, mhd_fem_wk)
!!        type(node_data), intent(in) :: node
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!!
!
      module t_MHD_mass_matrices
!
      use m_precision
!
      use t_finite_element_mat
      use t_geometry_data
!
      implicit  none
!
!>      Structure of mass matrices for FEM_MHD
      type lumped_mass_mat_layerd
!>        MAss matrices for fluid
        type (lumped_mass_matrices) :: mlump_fl
!>        MAss matrices for conductor
        type (lumped_mass_matrices) :: mlump_cd
!>        MAss matrices for insulator
        type (lumped_mass_matrices) :: mlump_ins
      end type lumped_mass_mat_layerd
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_mass_mat_fluid(node, mk_MHD)
!
      type(node_data), intent(in) :: node
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call alloc_type_fem_lumped_mass(node, mk_MHD%mlump_fl)
!
      end subroutine alloc_mass_mat_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_mass_mat_conduct(node, mk_MHD)
!
      type(node_data), intent(in) :: node
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call alloc_type_fem_lumped_mass(node, mk_MHD%mlump_cd)
      call alloc_type_fem_lumped_mass(node, mk_MHD%mlump_ins)
!
      end subroutine alloc_mass_mat_conduct
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine dealloc_fem_mat_conduct_type(mk_MHD)
!
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call dealloc_type_fem_lumped_mass(mk_MHD%mlump_cd)
      call dealloc_type_fem_lumped_mass(mk_MHD%mlump_ins)
!
      end subroutine dealloc_fem_mat_conduct_type
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_mass_mat_conduct(mk_MHD)
!
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call dealloc_type_fem_lumped_mass(mk_MHD%mlump_fl)
!
      end subroutine dealloc_mass_mat_conduct
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_fluid(id_rank, node, mk_MHD)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      write(50+id_rank,*) 'Check ml_fl'
      call check_mass_martix(id_rank, node, mk_MHD%mlump_fl)
!
      end subroutine check_mass_martix_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_conduct(id_rank, node, mk_MHD)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      write(50+id_rank,*) 'Check ml_ins'
      call check_mass_martix(id_rank, node, mk_MHD%mlump_cd)
!
      end subroutine check_mass_martix_conduct
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_insulate(id_rank, node, mk_MHD)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      write(50+id_rank,*) 'Check ml_ins'
      call check_mass_martix(id_rank, node, mk_MHD%mlump_ins)
!
      end subroutine check_mass_martix_insulate
!
!   ---------------------------------------------------------------------
!
      end module t_MHD_mass_matrices
