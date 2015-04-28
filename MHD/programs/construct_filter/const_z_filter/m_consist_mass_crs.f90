!
!     module m_consist_mass_crs
!
!     Written by H. Matsui
!
      module m_consist_mass_crs
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), dimension(:), allocatable :: d_mk_crs
      real(kind = kreal), dimension(:), allocatable :: al_mk_crs
      real(kind = kreal), dimension(:), allocatable :: au_mk_crs
!
      real(kind = kreal), dimension(:), allocatable :: rhs_mk_crs
      real(kind = kreal), dimension(:), allocatable :: sol_mk_crs
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_consist_mass_crs
!
       use m_geometry_parameter
       use m_crs_connect
       use m_crs_matrix
!
       allocate (al_mk_crs(ntot_crs_l) )
       allocate (au_mk_crs(ntot_crs_l) )
       allocate (d_mk_crs(numnod))
       allocate (rhs_mk_crs(numnod), sol_mk_crs(numnod))
!
       al_mk_crs = 0.0d0
       au_mk_crs = 0.0d0
       d_mk_crs = 0.0d0
       rhs_mk_crs = 0.0d0
       sol_mk_crs = 0.0d0
!
       end subroutine allocate_consist_mass_crs
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_consist_mass_crs
!
       deallocate (al_mk_crs )
       deallocate (au_mk_crs )
       deallocate (d_mk_crs)
       deallocate (rhs_mk_crs, sol_mk_crs)
!
       end subroutine deallocate_consist_mass_crs
!
! -----------------------------------------------------------------------
!
      end module m_consist_mass_crs
