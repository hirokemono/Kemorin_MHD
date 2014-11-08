!m_local_fline.f90
!
!      module m_local_fline
!
      module m_local_fline
!
!      Written by H. Matsui on Aug., 2011
!
      use m_precision
      use m_constants
!
      implicit  none
!
      integer(kind = kint) :: nnod_line_buf, nele_line_buf
      integer(kind = kint) :: nnod_line_l, nele_line_l
      integer(kind = kint), allocatable :: iedge_line_l(:,:)
      real(kind = kreal), allocatable ::   xx_line_l(:,:)
      real(kind = kreal), allocatable ::   col_line_l(:)
!
      integer(kind = kint), allocatable :: iedge_line_tmp(:,:)
      real(kind = kreal), allocatable ::   xx_line_tmp(:,:)
      real(kind = kreal), allocatable ::   col_line_tmp(:)
!
      private :: allocate_local_fline_conn_tmp
      private :: allocate_local_fline_data_tmp
      private :: deallocate_local_fline_conn_tmp
      private :: deallocate_local_fline_data_tmp
      private :: raise_local_fline_data, raise_local_fline_connect
!
!      subroutine allocate_local_fline
!      subroutine allocate_local_fline_conn
!      subroutine allocate_local_fline_data
!      subroutine deallocate_local_fline
!      subroutine deallocate_local_fline_conn
!      subroutine deallocate_local_fline_data
!      subroutine add_fline_start(xx_add, col_add)
!      subroutine add_fline_list(xx_add, col_add)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_start(xx_add, col_add)
!
      real(kind = kreal), intent(in) :: xx_add(3), col_add
!
!
      if(nnod_line_l .ge. nnod_line_buf) call raise_local_fline_data
      nnod_line_l = nnod_line_l + 1
!
      xx_line_l(1:3,nnod_line_l) = xx_add(1:3)
      col_line_l(nnod_line_l) =    col_add
!
      end subroutine add_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_list(xx_add, col_add)
!
      real(kind = kreal), intent(in) :: xx_add(3), col_add
!
!
      if(nele_line_l .ge. nele_line_buf) call raise_local_fline_connect
      if(nnod_line_l .ge. nnod_line_buf) call raise_local_fline_data
!
      nele_line_l = nele_line_l + 1
      nnod_line_l = nnod_line_l + 1
!
      iedge_line_l(1,nele_line_l) = nnod_line_l - 1
      iedge_line_l(2,nele_line_l) = nnod_line_l
!
      xx_line_l(1:3,nnod_line_l) = xx_add(1:3)
      col_line_l(nnod_line_l) =    col_add
!
      end subroutine add_fline_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline
!
      nnod_line_buf = 2
      nele_line_buf = 1
      nnod_line_l = 0
      nele_line_l = 0
!
      call allocate_local_fline_conn
      call allocate_local_fline_data
!
      end subroutine allocate_local_fline
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_conn
!
      allocate(iedge_line_l(2,nele_line_buf))
      if(nele_line_buf .gt. 0) iedge_line_l = 0
!
      end subroutine allocate_local_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_data
!
      allocate(xx_line_l(3,nnod_line_buf))
      allocate(col_line_l(nnod_line_buf))
      if(nnod_line_buf .gt. 0) xx_line_l = 0.0d0
      if(nnod_line_buf .gt. 0) col_line_l = 0.0d0
!
      end subroutine allocate_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_conn_tmp
!
      allocate(iedge_line_tmp(2,nele_line_buf))
      if(nele_line_buf .gt. 0) iedge_line_tmp = 0
!
      end subroutine allocate_local_fline_conn_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_fline_data_tmp
!
      allocate(xx_line_tmp(3,nnod_line_l))
      allocate(col_line_tmp(nnod_line_l))
      if(nnod_line_l .gt. 0) xx_line_tmp = 0.0d0
      if(nnod_line_l .gt. 0) col_line_tmp = 0.0d0
!
      end subroutine allocate_local_fline_data_tmp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline
!
      call deallocate_local_fline_conn
      call deallocate_local_fline_data
!
      end subroutine deallocate_local_fline
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_conn
!
      deallocate(iedge_line_l)
!
      end subroutine deallocate_local_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_data
!
      deallocate(xx_line_l, col_line_l)
!
      end subroutine deallocate_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_conn_tmp
!
      deallocate(iedge_line_tmp)
!
      end subroutine deallocate_local_fline_conn_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_fline_data_tmp
!
      deallocate(xx_line_tmp, col_line_tmp)
!
      end subroutine deallocate_local_fline_data_tmp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_connect
!
!
      call allocate_local_fline_conn_tmp
      iedge_line_tmp(1,1:nele_line_l) = iedge_line_l(1,1:nele_line_l)
      iedge_line_tmp(2,1:nele_line_l) = iedge_line_l(2,1:nele_line_l)
!
      call deallocate_local_fline_conn
      nele_line_buf = 2*nele_line_l
!
      call allocate_local_fline_conn
      iedge_line_l(1,1:nele_line_l) = iedge_line_tmp(1,1:nele_line_l)
      iedge_line_l(2,1:nele_line_l) = iedge_line_tmp(2,1:nele_line_l)
!
      call deallocate_local_fline_conn_tmp
!
      end subroutine raise_local_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_data
!
!
      call allocate_local_fline_data_tmp
      xx_line_tmp(1,1:nnod_line_l) = xx_line_l(1,1:nnod_line_l)
      xx_line_tmp(2,1:nnod_line_l) = xx_line_l(2,1:nnod_line_l)
      xx_line_tmp(3,1:nnod_line_l) = xx_line_l(3,1:nnod_line_l)
      col_line_tmp(1:nnod_line_l) =  col_line_l(1:nnod_line_l)
!
      call deallocate_local_fline_data
      nnod_line_buf = 2*nnod_line_l
      call allocate_local_fline_data
!
      xx_line_l(1,1:nnod_line_l) = xx_line_tmp(1,1:nnod_line_l)
      xx_line_l(2,1:nnod_line_l) = xx_line_tmp(2,1:nnod_line_l)
      xx_line_l(3,1:nnod_line_l) = xx_line_tmp(3,1:nnod_line_l)
      col_line_l(1:nnod_line_l) =  col_line_tmp(1:nnod_line_l)
!
      call deallocate_local_fline_data_tmp
!
      end subroutine raise_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_local_fline(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,*) nnod_line_l, nele_line_l
      do i = 1, nnod_line_l
        write(id_file,'(i15,1p3e16.7)') i, xx_line_l(1:3,i)
      end do
!
      do i = 1, nele_line_l
        write(id_file,'(2i15,a7,2i10)') i, ione,                        &
     &               '  line ', iedge_line_l(1:2,i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a)') 'color_data,'
      do i = 1, nnod_line_l
        write(id_file,'(i15,1pe16.7)') i, col_line_l(i)
      end do
!
      close(id_file)
!
      end subroutine check_local_fline
!
!  ---------------------------------------------------------------------
!
      subroutine check_local_fline_dx(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '#  node information'
      write(id_file,'(a,i15,a)')                                        &
     &   'object 1 class array type float rank 1 shape 3 items  ',      &
     &   nnod_line_l, ' data follows'
!
      do i = 1, nnod_line_l
        write(id_file,'(1p3e16.7)') xx_line_l(1:3,i)
      end do
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# element connectivity'
      write(id_file,'(a,i15,a)')                                        &
     &   'object 2 class array type int rank 1 shape 2 items    ',      &
     &   nele_line_l, ' data follows'
      do i = 1, nele_line_l
        write(id_file,'(2i15)') (iedge_line_l(1:2,i)-1)
      end do
      write(id_file,'(a)') 'attribute "element type" string "lines"'
      write(id_file,'(a)') 'attribute "ref" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# scalar'
      write(id_file,'(a,i15,a)')                                        &
     &   'object 3 class array type float rank 1 shape 1 items  ',      &
     &   nnod_line_l, ' data follows'
      do i = 1, nnod_line_l
        write(id_file,'(1pe16.7)') col_line_l(i)
      end do
      write(id_file,'(a)') 'attribute "dep" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') 'object "irregular positions irregular connections ascii file" class field'
      write(id_file,'(a)') 'component "positions" value 1'
      write(id_file,'(a)') 'component "connections" value 2'
      write(id_file,'(a)') 'component "data" value    3'
      write(id_file,'(a)') 'end'


      close(id_file)
!
      end subroutine check_local_fline_dx
!
!  ---------------------------------------------------------------------
!
      end module m_local_fline
