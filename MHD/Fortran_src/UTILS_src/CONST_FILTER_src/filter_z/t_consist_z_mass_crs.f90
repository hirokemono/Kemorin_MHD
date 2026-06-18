!>@file   t_consist_z_mass_crs.f90
!!        module t_consist_z_mass_crs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief CRS format consistent mass matices
!!
!!@verbatim
!!      subroutine alloc_consist_mass_crs(numnod)
!!      subroutine deallocate_consist_mass_crs
!!@endverbatim
!!
      module t_consist_z_mass_crs
!
      use m_precision
!
      implicit none
!
      type consist_z_mass_crs
        real(kind = kreal), allocatable :: d_mk_crs(:)
        real(kind = kreal), allocatable :: al_mk_crs(:)
        real(kind = kreal), allocatable :: au_mk_crs(:)
      end type consist_z_mass_crs
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_consist_mass_crs(numnod, tbl_crs, zmass)
!
      use t_crs_connect
!
      integer(kind = kint), intent(in) :: numnod
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(consist_z_mass_crs), intent(inout) :: zmass
!
      allocate(zmass%d_mk_crs(numnod))
      allocate(zmass%al_mk_crs(tbl_crs%ntot_l))
      allocate(zmass%au_mk_crs(tbl_crs%ntot_u))
!
      if(numnod .gt. 0) zmass%d_mk_crs =          0.0d0
      if(tbl_crs%ntot_l .gt. 0) zmass%al_mk_crs = 0.0d0
      if(tbl_crs%ntot_u .gt. 0) zmass%au_mk_crs = 0.0d0
!
      end subroutine alloc_consist_mass_crs
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_consist_mass_crs(zmass)
!
      type(consist_z_mass_crs), intent(inout) :: zmass
!
      deallocate(zmass%al_mk_crs, zmass%au_mk_crs, zmass%d_mk_crs)
!
      end subroutine dealloc_consist_mass_crs
!
! -----------------------------------------------------------------------
!
      end module t_consist_z_mass_crs
