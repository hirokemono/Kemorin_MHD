!t_MHD_mass_matrices.f90
!     module t_MHD_mass_matrices
!
!     Written by H. Matsui on Dec., 2008
!
! 
!>   @brief Structure for FEM assemble for MHD dynamo
!
!!      subroutine alloc_mass_mat_fluid(numnod, mk_MHD)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!!      subroutine alloc_mass_mat_conduct(numnod, mk_MHD)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!!
!!      subroutine dealloc_mass_mat_conduct(mk_MHD)
!!      subroutine dealloc_fem_mat_conduct_type(mk_MHD)
!!
!!      subroutine check_mass_martix_fluid                              &
!!     &         (id_rank, numnod, mhd_fem_wk)
!!      subroutine check_mass_martix_conduct                            &
!!     &         (id_rank, numnod, mhd_fem_wk)
!!      subroutine check_mass_martix_insulate                           &
!!     &         (id_rank, numnod, mhd_fem_wk)
!
      module t_MHD_mass_matrices
!
      use m_precision
!
      use t_finite_element_mat
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
      subroutine alloc_mass_mat_fluid(numnod, mk_MHD)
!
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call alloc_type_fem_lumped_mass(numnod, mk_MHD%mlump_fl)
!
      end subroutine alloc_mass_mat_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_mass_mat_conduct(numnod, mk_MHD)
!
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call alloc_type_fem_lumped_mass(numnod, mk_MHD%mlump_cd)
      call alloc_type_fem_lumped_mass(numnod, mk_MHD%mlump_ins)
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
      subroutine check_mass_martix_fluid                                &
     &         (id_rank, numnod, mk_MHD)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      write(50+id_rank,*) 'Check ml_fl'
      call check_mass_martix(id_rank, numnod, mk_MHD%mlump_fl)
!
      end subroutine check_mass_martix_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_conduct                              &
     &         (id_rank, numnod, mk_MHD)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      write(50+id_rank,*) 'Check ml_ins'
      call check_mass_martix(id_rank, numnod, mk_MHD%mlump_cd)
!
      end subroutine check_mass_martix_conduct
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_insulate                             &
     &         (id_rank, numnod, mk_MHD)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      write(50+id_rank,*) 'Check ml_ins'
      call check_mass_martix(id_rank, numnod, mk_MHD%mlump_ins)
!
      end subroutine check_mass_martix_insulate
!
!   ---------------------------------------------------------------------
!
      end module t_MHD_mass_matrices
