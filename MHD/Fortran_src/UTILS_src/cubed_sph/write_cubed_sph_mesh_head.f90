!write_cubed_sph_mesh_head.f90
!      module write_cubed_sph_mesh_head
!
      module write_cubed_sph_mesh_head
!
!      Written by Kemorin on Apr., 2006
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), parameter :: izero = 0
      integer(kind = kint), parameter :: itri_linear =  331
      integer(kind = kint), parameter :: isurf_liner =  221
      integer(kind = kint), parameter :: irod_linear =  111
      integer(kind = kint), parameter :: i_quad =       332
      integer(kind = kint), parameter :: isurf_quad =   222
      integer(kind = kint), parameter :: irod_quad =    112
      integer(kind = kint), parameter :: isurf_master = 411
      integer(kind = kint), parameter :: iele_master =  422
!
      private :: izero
      private :: itri_linear, isurf_liner, irod_linear, i_quad
      private :: isurf_quad, irod_quad, isurf_master, iele_master
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_header_4_mesh(id_mesh, id_conn, id_group,        &
     &          numnod, numele, nnod_4_ele)
!
!
      integer(kind = kint), intent(in) :: id_mesh, id_conn, id_group
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
!
      integer(kind = kint) :: i
!
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! 1. Parallel information'
      write(id_mesh,'(i10)') izero
      write(id_mesh,'(i10)') izero
      write(id_mesh,'(a)') ''
!
      write(id_mesh,'(a)') '!'
      write(id_mesh,'(a)') '! 2. Mesh information (nodes and elements)'
      write(id_mesh,'(a)') '! 2.1. Position of nodes'
!
      write(id_mesh,'(2i10)') numnod, numnod
!
!
      write(id_conn,'(a)') '! 2.2. Element connectivity'
!
      write(id_conn,'(i10)') numele
!
      if (nnod_4_ele.eq.20) then
        write(id_conn,'(10i6)') (i_quad,i=1,numele)
      else if (nnod_4_ele.eq.8) then
        write(id_conn,'(10i6)') (itri_linear,i=1, numele)
      end if
!
      write(id_group,'(a)') '!'
      write(id_group,'(a)') '! 3. import/export information'
      write(id_group,'(a)') '! 3.1. import'
      write(id_group,'(a)') ''
!
      write(id_group,'(a)') '! 3.2. export'
      write(id_group,'(a)') ''
!
      return
!
      end subroutine write_header_4_mesh
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      end module write_cubed_sph_mesh_head
