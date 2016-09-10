!> @file  m_node_id_spherical_IO.f90
!!      module m_node_id_spherical_IO
!!
!! @author  H. Matsui
!! @date Written in July, 2007
!
!> @brief Array for speherical harmonics indexing IO
!!
!!@verbatim
!!      subroutine set_sph_mesh_file_fmt_prefix(iflag_fmt, file_head)
!!@endverbatim
!
      module m_node_id_spherical_IO
!
      use m_precision
      use t_node_id_spherical_IO
!
      implicit none
!
!
!>      Structure for spherical harmonics table IO
      type(sph_IO_data), save :: sph_IO1
!
      integer(kind = kint), parameter :: mesh_file_id = 14
!
      character(len=kchara) :: sph_file_head =     "in_sph"
      integer(kind = kint) :: iflag_sph_file_fmt = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_mesh_file_fmt_prefix(iflag_fmt, file_head)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
!
      iflag_sph_file_fmt = iflag_fmt
      write(sph_file_head,'(a)') trim(file_head)
!
      end subroutine set_sph_mesh_file_fmt_prefix
!
! -------------------------------------------------------------------
!
      end module m_node_id_spherical_IO
