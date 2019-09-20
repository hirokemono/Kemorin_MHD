!>@file   t_boundary_sph_spectr.f90
!!@brief  module t_boundary_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine alloc_sph_scalar_bc_array(jmax, bc_Sspectr)
!!        type(sph_scalar_BC_spectr), intent(inout) :: bc_Sspectr
!!      subroutine alloc_sph_vector_bc_array(jmax, sph_MHD_bc)
!!        type(sph_vector_BC_spectr), intent(inout) :: bc_Uspectr
!!
!!      subroutine dealloc_sph_scalar_bc_array(bc_Sspectr)
!!        type(sph_scalar_BC_spectr), intent(inout) :: bc_Sspectr
!!      subroutine dealloc_sph_vector_bc_array(sph_MHD_bc)
!!        type(sph_vector_BC_spectr), intent(inout) :: bc_Uspectr
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!
      module t_boundary_sph_spectr
!
      use m_precision
!
      implicit none
!
!
!>      Structure for boundary scalar spectr
      type sph_scalar_BC_spectr
!>        Number of componentts
        integer(kind = kint) :: jmax_sBC
!
!>        Fixed poloidal velocity spectrum for center
        real(kind = kreal) :: s_CTR_bc
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: s_ICB_bc(:)
!>        Angular frequency of poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: w_ICB_bc(:)
!
!>        Fixed poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: s_CMB_bc(:)
!>        Angular frequency of poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: w_CMB_bc(:)
      end type sph_scalar_BC_spectr
!
!>      Structure for boundary velocity spectr
      type sph_vector_BC_spectr
!>        Number of componentts
        integer(kind = kint) :: jmax_vBC
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: vp_ICB_bc(:)
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: dp_ICB_bc(:)
!>        Fixed toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: vt_ICB_bc(:)
!
!>        Angular frequency of poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: wp_ICB_bc(:)
!>        Angular frequency of toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: wt_ICB_bc(:)
!
!>        Fixed poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: vp_CMB_bc(:)
!>        Fixed poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: dp_CMB_bc(:)
!>        Fixed toroidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: vt_CMB_bc(:)
!
!>        Angular frequency of poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: wp_CMB_bc(:)
!>        Angular frequency of toroidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: wt_CMB_bc(:)
      end type sph_vector_BC_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_scalar_bc_array(jmax, bc_Sspectr)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_scalar_BC_spectr), intent(inout) :: bc_Sspectr
!
!
      bc_Sspectr%jmax_sBC = jmax
!
      allocate(bc_Sspectr%s_ICB_bc(bc_Sspectr%jmax_sBC))
      allocate(bc_Sspectr%w_ICB_bc(bc_Sspectr%jmax_sBC))
!
      allocate(bc_Sspectr%s_CMB_bc(bc_Sspectr%jmax_sBC))
      allocate(bc_Sspectr%w_CMB_bc(bc_Sspectr%jmax_sBC))
!
      if(bc_Sspectr%jmax_sBC .le. 0) return
      bc_Sspectr%s_ICB_bc = 0.0d0
      bc_Sspectr%w_ICB_bc = 0.0d0
!
      bc_Sspectr%s_CMB_bc = 0.0d0
      bc_Sspectr%w_CMB_bc = 0.0d0
!
      end subroutine alloc_sph_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vector_bc_array(jmax, bc_Uspectr)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_vector_BC_spectr), intent(inout) :: bc_Uspectr
!
!
      bc_Uspectr%jmax_vBC = jmax
!
      allocate(bc_Uspectr%vp_ICB_bc(bc_Uspectr%jmax_vBC))
      allocate(bc_Uspectr%dp_ICB_bc(bc_Uspectr%jmax_vBC))
      allocate(bc_Uspectr%vt_ICB_bc(bc_Uspectr%jmax_vBC))
!
      allocate(bc_Uspectr%wp_ICB_bc(bc_Uspectr%jmax_vBC))
      allocate(bc_Uspectr%wt_ICB_bc(bc_Uspectr%jmax_vBC))
!
      allocate(bc_Uspectr%vp_CMB_bc(bc_Uspectr%jmax_vBC))
      allocate(bc_Uspectr%dp_CMB_bc(bc_Uspectr%jmax_vBC))
      allocate(bc_Uspectr%vt_CMB_bc(bc_Uspectr%jmax_vBC))
!
      allocate(bc_Uspectr%wp_CMB_bc(bc_Uspectr%jmax_vBC))
      allocate(bc_Uspectr%wt_CMB_bc(bc_Uspectr%jmax_vBC))
!
      if(bc_Uspectr%jmax_vBC .le. 0) return
      bc_Uspectr%vp_ICB_bc = 0.0d0
      bc_Uspectr%dp_ICB_bc = 0.0d0
      bc_Uspectr%vt_ICB_bc = 0.0d0
!
      bc_Uspectr%wp_ICB_bc = 0.0d0
      bc_Uspectr%wt_ICB_bc = 0.0d0
!
      bc_Uspectr%vp_CMB_bc = 0.0d0
      bc_Uspectr%dp_CMB_bc = 0.0d0
      bc_Uspectr%vt_CMB_bc = 0.0d0
!
      bc_Uspectr%wp_CMB_bc = 0.0d0
      bc_Uspectr%wt_CMB_bc = 0.0d0
!
      end subroutine alloc_sph_vector_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_scalar_bc_array(bc_Sspectr)
!
      type(sph_scalar_BC_spectr), intent(inout) :: bc_Sspectr
!
      deallocate(bc_Sspectr%s_ICB_bc, bc_Sspectr%s_CMB_bc)
      deallocate(bc_Sspectr%w_ICB_bc, bc_Sspectr%w_CMB_bc)
!
      end subroutine dealloc_sph_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vector_bc_array(bc_Uspectr)
!
      type(sph_vector_BC_spectr), intent(inout) :: bc_Uspectr
!
      deallocate(bc_Uspectr%vp_ICB_bc, bc_Uspectr%vp_CMB_bc)
      deallocate(bc_Uspectr%dp_ICB_bc, bc_Uspectr%dp_CMB_bc)
      deallocate(bc_Uspectr%vt_ICB_bc, bc_Uspectr%vt_CMB_bc)
!
      deallocate(bc_Uspectr%wp_ICB_bc, bc_Uspectr%wp_CMB_bc)
      deallocate(bc_Uspectr%wt_ICB_bc, bc_Uspectr%wt_CMB_bc)
!
      end subroutine dealloc_sph_vector_bc_array
!
! -----------------------------------------------------------------------
!
      end module t_boundary_sph_spectr
