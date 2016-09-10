!> @file  m_node_id_spherical_IO.f90
!!      module m_node_id_spherical_IO
!!
!! @author  H. Matsui
!! @date Written in July, 2007
!
!> @brief Array for speherical harmonics indexing IO
!!
!!@verbatim
!!      subroutine allocate_nod_id_sph_IO
!!      subroutine allocate_idx_sph_1d1_IO
!!      subroutine allocate_idx_sph_1d2_IO
!!      subroutine allocate_idx_sph_1d3_IO
!!
!!      subroutine deallocate_nod_id_sph_IO
!!      subroutine deallocate_idx_sph_1d1_IO
!!      subroutine deallocate_idx_sph_1d2_IO
!!      subroutine deallocate_idx_sph_1d3_IO
!!
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
!sph_IO1%r_gl_1
!
!      integer(kind = kint) :: sph_rank_IO(3)
!
!      integer(kind = kint) :: nidx_gl_sph_IO(1:3)
!      integer(kind = kint) :: ltr_gl_IO
!
!      integer(kind = kint) :: ndir_sph_IO
!      integer(kind = kint) :: nnod_sph_IO
!
!      integer(kind = kint_gl), allocatable :: inod_gl_sph_IO(:)
!      integer(kind = kint), allocatable :: idx_gl_sph_IO(:,:)
!
!      integer(kind = kint) :: nidx_sph_IO(3)
!      integer(kind = kint) :: ist_sph_IO(3)
!      integer(kind = kint) :: ied_sph_IO(3)
!      integer(kind = kint) :: ncomp_itbl_1d_IO(3)
!      integer(kind = kint), allocatable :: idx_gl_1_IO(:)
!      integer(kind = kint), allocatable :: idx_gl_2_IO(:,:)
!      integer(kind = kint), allocatable :: idx_gl_3_IO(:,:)
!
!      real(kind = kreal), allocatable :: r_gl_1_IO(:)
!
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
      subroutine allocate_nod_id_sph_IO
!
!
      allocate( sph_IO1%inod_gl_sph(sph_IO1%numnod_sph) )
      allocate( sph_IO1%idx_gl_sph(sph_IO1%numnod_sph,sph_IO1%numdir_sph) )
!
      sph_IO1%inod_gl_sph = 0
      sph_IO1%idx_gl_sph =  0
!
      end subroutine allocate_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_1d1_IO
!
      allocate( sph_IO1%idx_gl_1(sph_IO1%nidx_sph(1)) )
      allocate( sph_IO1%r_gl_1(sph_IO1%nidx_sph(1)) )
      sph_IO1%idx_gl_1 =  0
      sph_IO1%r_gl_1 = 0.0d0
!
      end subroutine allocate_idx_sph_1d1_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_1d2_IO
!
      allocate( sph_IO1%idx_gl_2(sph_IO1%nidx_sph(2),sph_IO1%ncomp_table_1d(2)) )
      sph_IO1%idx_gl_2 =  0
!
      end subroutine allocate_idx_sph_1d2_IO
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_1d3_IO
!
      allocate( sph_IO1%idx_gl_3(sph_IO1%nidx_sph(3),sph_IO1%ncomp_table_1d(3)) )
      sph_IO1%idx_gl_3 =  0
!
      end subroutine allocate_idx_sph_1d3_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_id_sph_IO
!
      deallocate( sph_IO1%inod_gl_sph )
      deallocate( sph_IO1%idx_gl_sph  )
!
      end subroutine deallocate_nod_id_sph_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_1d1_IO
!
      deallocate( sph_IO1%idx_gl_1 )
      deallocate( sph_IO1%r_gl_1 )
!
      end subroutine deallocate_idx_sph_1d1_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_1d2_IO
!
      deallocate( sph_IO1%idx_gl_2 )
!
      end subroutine deallocate_idx_sph_1d2_IO
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_1d3_IO
!
      deallocate( sph_IO1%idx_gl_3 )
!
      end subroutine deallocate_idx_sph_1d3_IO
!
! -----------------------------------------------------------------------
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
