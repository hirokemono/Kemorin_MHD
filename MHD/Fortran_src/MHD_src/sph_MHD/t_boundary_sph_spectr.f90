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
!!      subroutine alloc_sph_scalar_bc_array(jmax, bc_Sspec)
!!        type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!!      subroutine alloc_sph_vector_bc_array(jmax, bc_Vspec)
!!        type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!!      subroutine alloc_sph_evo_scalar_bc_array(jmax, bc_Sevo)
!!        type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!!      subroutine alloc_sph_evo_vector_bc_array(jmax, bc_Vevo)
!!        type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!!
!!      subroutine dealloc_sph_scalar_bc_array(bc_Sspec)
!!        type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!!      subroutine dealloc_sph_vector_bc_array(bc_Vspec)
!!        type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!!      subroutine dealloc_sph_evo_scalar_bc_array(bc_Sevo)
!!        type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!!      subroutine dealloc_sph_evo_vector_bc_array(bc_Vevo)
!!        type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
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
      type sph_scalar_BC_coef
!>        Number of componentts
        integer(kind = kint) :: jmax_sBC
!
!>        Fixed poloidal velocity spectrum for center
        real(kind = kreal) :: s_CTR_bc = 0.0d0
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: s_ICB_bc(:)
!>        Fixed poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: s_CMB_bc(:)
      end type sph_scalar_BC_coef
!
!>      Structure for boundary velocity spectr
      type sph_vector_BC_coef
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
!>        Fixed poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: vp_CMB_bc(:)
!>        Fixed poloidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: dp_CMB_bc(:)
!>        Fixed toroidal velocity spectrum for CMB
        real(kind = kreal), allocatable :: vt_CMB_bc(:)
      end type sph_vector_BC_coef
!
!
!>      Structure for boundary scalar spectr
      type sph_scalar_BC_evo
!>        Number of componentts
        integer(kind = kint) :: jmax_sBC
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: S_BC_mag(:)
!>        Angular frequency scalar spectrum for ICB
        real(kind = kreal), allocatable :: S_BC_freq(:)
!>        Angular phase scalar spectrum for ICB
        real(kind = kreal), allocatable :: S_BC_phase(:)
      end type sph_scalar_BC_evo
!
!
!>      Structure for boundary velocity spectr
      type sph_vector_BC_evo
!>        Number of componentts
        integer(kind = kint) :: jmax_vBC
!
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC_mag(:)
!>        Fixed poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Dp_BC_mag(:)
!>        Fixed toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC_mag(:)
!
!>        Angular phase of poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC_phase(:)
!>        Angular phase of poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Dp_BC_phase(:)
!>        Angular phase of toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC_phase(:)
!
!>        Angular frequency poloidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vp_BC_freq(:)
!>        Angular frequency toroidal velocity spectrum for ICB
        real(kind = kreal), allocatable :: Vt_BC_freq(:)
      end type sph_vector_BC_evo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_scalar_bc_array(jmax, bc_Sspec)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
!
      bc_Sspec%jmax_sBC = jmax
!
      allocate(bc_Sspec%s_ICB_bc(bc_Sspec%jmax_sBC))
      allocate(bc_Sspec%s_CMB_bc(bc_Sspec%jmax_sBC))
!
      if(bc_Sspec%jmax_sBC .le. 0) return
      bc_Sspec%s_ICB_bc = 0.0d0
      bc_Sspec%s_CMB_bc = 0.0d0
!
      end subroutine alloc_sph_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vector_bc_array(jmax, bc_Vspec)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!
!
      bc_Vspec%jmax_vBC = jmax
!
      allocate(bc_Vspec%vp_ICB_bc(bc_Vspec%jmax_vBC))
      allocate(bc_Vspec%dp_ICB_bc(bc_Vspec%jmax_vBC))
      allocate(bc_Vspec%vt_ICB_bc(bc_Vspec%jmax_vBC))
!
      allocate(bc_Vspec%vp_CMB_bc(bc_Vspec%jmax_vBC))
      allocate(bc_Vspec%dp_CMB_bc(bc_Vspec%jmax_vBC))
      allocate(bc_Vspec%vt_CMB_bc(bc_Vspec%jmax_vBC))
!
      if(bc_Vspec%jmax_vBC .le. 0) return
      bc_Vspec%vp_ICB_bc = 0.0d0
      bc_Vspec%dp_ICB_bc = 0.0d0
      bc_Vspec%vt_ICB_bc = 0.0d0
!
      bc_Vspec%vp_CMB_bc = 0.0d0
      bc_Vspec%dp_CMB_bc = 0.0d0
      bc_Vspec%vt_CMB_bc = 0.0d0
!
      end subroutine alloc_sph_vector_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_evo_scalar_bc_array(jmax, bc_Sevo)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
!
      bc_Sevo%jmax_sBC = jmax
!
      allocate(bc_Sevo%S_BC_mag(bc_Sevo%jmax_sBC))
      allocate(bc_Sevo%S_BC_freq(bc_Sevo%jmax_sBC))
      allocate(bc_Sevo%S_BC_phase(bc_Sevo%jmax_sBC))
!
      if(bc_Sevo%jmax_sBC .le. 0) return
      bc_Sevo%S_BC_mag = 0.0d0
      bc_Sevo%S_BC_freq = 0.0d0
      bc_Sevo%S_BC_phase = 0.0d0
!
      end subroutine alloc_sph_evo_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_evo_vector_bc_array(jmax, bc_Vevo)
!
      integer(kind = kint), intent(in) :: jmax
      type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!
!
      bc_Vevo%jmax_vBC = jmax
!
      allocate(bc_Vevo%Vp_BC_mag(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Dp_BC_mag(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Vt_BC_mag(bc_Vevo%jmax_vBC))
!
      allocate(bc_Vevo%Vp_BC_phase(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Dp_BC_phase(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Vt_BC_phase(bc_Vevo%jmax_vBC))
!
      allocate(bc_Vevo%Vp_BC_freq(bc_Vevo%jmax_vBC))
      allocate(bc_Vevo%Vt_BC_freq(bc_Vevo%jmax_vBC))
!
      if(bc_Vevo%jmax_vBC .le. 0) return
      bc_Vevo%Vp_BC_mag = 0.0d0
      bc_Vevo%Dp_BC_mag = 0.0d0
      bc_Vevo%Vt_BC_mag = 0.0d0
!
      bc_Vevo%Vp_BC_freq = 0.0d0
      bc_Vevo%Vt_BC_freq = 0.0d0
!
      bc_Vevo%Vp_BC_phase = 0.0d0
      bc_Vevo%Dp_BC_phase = 0.0d0
      bc_Vevo%Vt_BC_phase = 0.0d0
!
      end subroutine alloc_sph_evo_vector_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_scalar_bc_array(bc_Sspec)
!
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
      deallocate(bc_Sspec%s_ICB_bc, bc_Sspec%s_CMB_bc)
!
      end subroutine dealloc_sph_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vector_bc_array(bc_Vspec)
!
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!
      deallocate(bc_Vspec%vp_ICB_bc, bc_Vspec%vp_CMB_bc)
      deallocate(bc_Vspec%dp_ICB_bc, bc_Vspec%dp_CMB_bc)
      deallocate(bc_Vspec%vt_ICB_bc, bc_Vspec%vt_CMB_bc)
!
      end subroutine dealloc_sph_vector_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_evo_scalar_bc_array(bc_Sevo)
!
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
!
      deallocate(bc_Sevo%S_BC_mag)
      deallocate(bc_Sevo%S_BC_phase, bc_Sevo%S_BC_freq)
!
      end subroutine dealloc_sph_evo_scalar_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_evo_vector_bc_array(bc_Vevo)
!
      type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!
!
      deallocate(bc_Vevo%Vp_BC_mag, bc_Vevo%Dp_BC_mag)
      deallocate(bc_Vevo%Vt_BC_mag)
!
      deallocate(bc_Vevo%Vp_BC_freq,  bc_Vevo%Vt_BC_freq)
!
      deallocate(bc_Vevo%Vp_BC_phase, bc_Vevo%Dp_BC_phase)
      deallocate(bc_Vevo%Vt_BC_phase)
!
      end subroutine dealloc_sph_evo_vector_bc_array
!
! -----------------------------------------------------------------------
!
      end module t_boundary_sph_spectr
