!t_finite_element_mat_MHD.f90
!     module t_finite_element_mat_MHD
!
!     Written by H. Matsui on Dec., 2008
!
! 
!>   @brief Structure for FEM assemble for MHD dynamo
!
!      subroutine alloc_fem_mat_conduct_type(numnod, mk_MHD)
!        integer(kind = kint), intent(in) :: numnod
!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!      subroutine alloc_fem_mat_fluid_type(numnod, mk_MHD)
!        integer(kind = kint), intent(in) :: numnod
!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!      subroutine dealloc_fem_mat_conduct_type(mk_MHD)
!      subroutine dealloc_fem_mat_fluid_type(mk_MHD)
!
      module t_finite_element_mat_MHD
!
      use m_precision
!
      use t_finite_element_mat
!
      implicit  none
!
      type lumped_mass_mat_layerd
        type (lumped_mass_matrices) :: mlump_fl
        type (lumped_mass_matrices) :: mlump_cd
        type (lumped_mass_matrices) :: mlump_ins
      end type lumped_mass_mat_layerd
!
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_mat_fluid_type(numnod, mk_MHD)
!
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call alloc_type_fem_lumped_mass(numnod, mk_MHD%mlump_fl)
!
      end subroutine alloc_fem_mat_fluid_type
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_mat_conduct_type(numnod, mk_MHD)
!
      integer(kind = kint), intent(in) :: numnod
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call alloc_type_fem_lumped_mass(numnod, mk_MHD%mlump_cd)
      call alloc_type_fem_lumped_mass(numnod, mk_MHD%mlump_ins)
!
      end subroutine alloc_fem_mat_conduct_type
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
      subroutine dealloc_fem_mat_fluid_type(mk_MHD)
!
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!
!
      call dealloc_type_fem_lumped_mass(mk_MHD%mlump_fl)
!
      end subroutine dealloc_fem_mat_fluid_type
!
!   ---------------------------------------------------------------------
!
      end module t_finite_element_mat_MHD
