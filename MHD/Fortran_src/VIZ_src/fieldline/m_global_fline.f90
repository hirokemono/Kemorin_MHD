!m_global_fline.f90
!
!      module m_global_fline
!
      module m_global_fline
!
!      Written by H. Matsui on Aug., 2011
!
      use m_precision
      use m_constants
!
      implicit  none
!
      integer(kind = kint) :: ntot_nod_line_gl_buf
      integer(kind = kint) :: ntot_ele_line_gl_buf
      integer(kind = kint) :: ntot_nod_line_gl, ntot_ele_line_gl
      integer(kind = kint), allocatable :: nnod_line_gl(:)
      integer(kind = kint), allocatable :: nele_line_gl(:)
      integer(kind = kint), allocatable :: istack_nod_line_gl(:)
      integer(kind = kint), allocatable :: istack_ele_line_gl(:)
!
      integer(kind = kint), allocatable :: iedge_line_gl(:,:)
      real(kind = kreal), allocatable ::   xx_line_gl(:,:)
      real(kind = kreal), allocatable ::   col_line_gl(:)
      character(len = kchara) :: color_name_gl
!
!
      integer, save, allocatable :: sta1_fline(:,:)
!                 work array for communication (wait)
      integer, save, allocatable :: sta2_fline(:,:)
!                 work array for communication (wait)
      integer, save, allocatable :: req1_fline(:  )
!                 work array for communication (wait)
      integer, save, allocatable :: req2_fline(:  )
!                 work array for communication (wait)
!
      private :: allocate_global_fline_conn, allocate_global_fline_data
      private :: deallocate_global_fline_conn
      private :: deallocate_global_fline_data
!
!      subroutine allocate_global_fline_num
!      subroutine allocate_global_fline
!      subroutine deallocate_global_fline
!      subroutine raise_global_fline_connect
!      subroutine raise_global_fline_connect
!
!      subroutine write_global_fline(id_file)
!      subroutine write_global_fline_dx(id_file)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_global_fline_num
!
      use calypso_mpi
!
!
      allocate(istack_nod_line_gl(0:nprocs))
      allocate(istack_ele_line_gl(0:nprocs))
      allocate(nnod_line_gl(nprocs))
      allocate(nele_line_gl(nprocs))
!
      istack_nod_line_gl = 0
      istack_ele_line_gl = 0
      nnod_line_gl = 0
      nele_line_gl = 0
!
      allocate (sta1_fline(MPI_STATUS_SIZE,nprocs))
      allocate (sta2_fline(MPI_STATUS_SIZE,nprocs))
      allocate (req1_fline(nprocs))
      allocate (req2_fline(nprocs))
!
      call allocate_global_fline
!
      end subroutine allocate_global_fline_num
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_global_fline
!
      ntot_nod_line_gl_buf = 2
      ntot_ele_line_gl_buf = 1
      ntot_nod_line_gl = 0
      ntot_ele_line_gl = 0
!
      call allocate_global_fline_conn
      call allocate_global_fline_data
!
      end subroutine allocate_global_fline
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_global_fline_conn
!
      allocate(iedge_line_gl(2,ntot_ele_line_gl_buf))
      if(ntot_ele_line_gl_buf .gt. 0) iedge_line_gl = 0
!
      end subroutine allocate_global_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_global_fline_data
!
      allocate(xx_line_gl(3,ntot_nod_line_gl_buf))
      allocate(col_line_gl(ntot_nod_line_gl_buf))
      if(ntot_nod_line_gl_buf .gt. 0) xx_line_gl = 0.0d0
      if(ntot_nod_line_gl_buf .gt. 0) col_line_gl = 0.0d0
!
      end subroutine allocate_global_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_global_fline_num
!
!
      deallocate(nnod_line_gl, istack_nod_line_gl)
      deallocate(nele_line_gl, istack_ele_line_gl)
      deallocate (sta1_fline, sta2_fline)
      deallocate (req1_fline, req2_fline)
!
      end subroutine deallocate_global_fline_num
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_global_fline
!
      call deallocate_global_fline_conn
      call deallocate_global_fline_data
!
      end subroutine deallocate_global_fline
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_global_fline_conn
!
      deallocate(iedge_line_gl)
!
      end subroutine deallocate_global_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_global_fline_data
!
      deallocate(xx_line_gl, col_line_gl)
!
      end subroutine deallocate_global_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine raise_global_fline_connect
!
!
      call deallocate_global_fline_conn
!
      ntot_ele_line_gl_buf = ntot_ele_line_gl
      call allocate_global_fline_conn
!
      end subroutine raise_global_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine raise_global_fline_data
!
!
      call deallocate_global_fline_data
!
      ntot_nod_line_gl_buf = ntot_nod_line_gl
      call allocate_global_fline_data
!
      end subroutine raise_global_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_global_fline(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,*) ntot_nod_line_gl, ntot_ele_line_gl
      do i = 1, ntot_nod_line_gl
        write(id_file,'(i15,1p3e16.7)') i, xx_line_gl(1:3,i)
      end do
!
      do i = 1, ntot_ele_line_gl
        write(id_file,'(2i15,a7,2i10)') i, ione,                        &
     &               '  line ', iedge_line_gl(1:2,i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a,a1)') trim(color_name_gl), ','
      do i = 1, ntot_nod_line_gl
        write(id_file,'(i15,1pe16.7)') i, col_line_gl(i)
      end do
!
      end subroutine write_global_fline
!
!  ---------------------------------------------------------------------
!
      subroutine write_global_fline_dx(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '#  node information'
      write(id_file,'(a,i15,a)')                                        &
     &   'object 1 class array type float rank 1 shape 3 items  ',      &
     &   ntot_nod_line_gl, ' data follows'
!
      do i = 1, ntot_nod_line_gl
        write(id_file,'(1p3e16.7)') xx_line_gl(1:3,i)
      end do
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# element connectivity'
      write(id_file,'(a,i15,a)')                                        &
     &   'object 2 class array type int rank 1 shape 2 items    ',      &
     &   ntot_ele_line_gl, ' data follows'
      do i = 1, ntot_ele_line_gl
        write(id_file,'(2i15)') (iedge_line_gl(1:2,i)-1)
      end do
      write(id_file,'(a)') 'attribute "element type" string "lines"'
      write(id_file,'(a)') 'attribute "ref" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# scalar'
      write(id_file,'(a,i15,a)')                                        &
     &   'object 3 class array type float rank 1 shape 1 items  ',      &
     &   ntot_nod_line_gl, ' data follows'
      do i = 1, ntot_nod_line_gl
        write(id_file,'(1pe16.7)') col_line_gl(i)
      end do
      write(id_file,'(a)') 'attribute "dep" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') 'object "irregular positions irregular connections ascii file" class field'
      write(id_file,'(a)') 'component "positions" value 1'
      write(id_file,'(a)') 'component "connections" value 2'
      write(id_file,'(a)') 'component "data" value    3'
      write(id_file,'(a)') 'end'
!
      end subroutine write_global_fline_dx
!
!  ---------------------------------------------------------------------
!
      end module m_global_fline
