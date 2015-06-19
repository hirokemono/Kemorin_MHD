!collect_fline_data.f90
!
!      module collect_fline_data
!
!      Written by H. Matsui on Aug., 2011
!
      module collect_fline_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_global_fline
      use m_local_fline
!
      implicit  none
!
      private :: collect_number_of_fline, collect_fline_connection
      private :: collect_fline_position, collect_fline_color
!
!      subroutine s_collect_fline_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_collect_fline_data(istep_psf, i_fln)
!
      use m_control_params_4_fline
      use m_field_file_format
      use set_ucd_file_names
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep_psf, i_fln
!
      character(len=kchara) :: ftmp_1, file_name
!
!
      color_name_gl = name_color_output(i_fln)
      call collect_number_of_fline
!
      if(ntot_nod_line_gl .gt. ntot_nod_line_gl_buf) then
        call raise_global_fline_data
      end if
!
      if(ntot_ele_line_gl .gt. ntot_ele_line_gl_buf) then
        call raise_global_fline_connect
      end if
!
      call collect_fline_connection
      call collect_fline_position
      call collect_fline_color
!
!
      if(my_rank .eq. 0) then
        if(id_fline_file_type(i_fln) .eq. 0) then
          call set_single_ucd_file_name(fline_header(i_fln), iflag_ucd, &
     &        istep_psf, file_name)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline(id_fline_data_code)
          close(id_fline_data_code)
        else
          call add_int_suffix(istep_psf, fline_header(i_fln), ftmp_1)
          call add_dx_extension(ftmp_1, file_name)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_dx(id_fline_data_code)
          close(id_fline_data_code)
        end if
      end if
!
      end subroutine s_collect_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine collect_number_of_fline
!
      integer(kind = kint) :: ip
!
!
      call MPI_AllGather(nnod_line_l, ione, CALYPSO_INTEGER,            &
     &    nnod_line_gl(1), ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      call MPI_AllGather(nele_line_l, ione, CALYPSO_INTEGER,            &
     &    nele_line_gl(1), ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_nod_line_gl(0) = 0
      istack_ele_line_gl(0) = 0
      do ip = 1, nprocs
        istack_nod_line_gl(ip) = istack_nod_line_gl(ip-1)               &
     &                            + nnod_line_gl(ip)
        istack_ele_line_gl(ip) = istack_ele_line_gl(ip-1)               &
     &                            + nele_line_gl(ip)
      end do
      ntot_nod_line_gl = istack_nod_line_gl(nprocs)
      ntot_ele_line_gl = istack_ele_line_gl(nprocs)
!
      end subroutine collect_number_of_fline
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_connection
!
      integer(kind = kint) :: ip, num, ist, ied, inum, nneib_recv
!
!
      nneib_recv = 0
      num = 2*nele_line_l
      call MPI_Isend(iedge_line_l(1,1), num, CALYPSO_INTEGER, izero,    &
     &   0, CALYPSO_COMM, req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist = istack_ele_line_gl(ip-1) + 1
          num = 2*nele_line_gl(ip)
          call MPI_Irecv(iedge_line_gl(1,ist), num, CALYPSO_INTEGER,    &
     &         (ip-1), 0, CALYPSO_COMM, req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, req2_fline, sta2_fline, ierr_MPI)
      call MPI_WAITALL(ione, req1_fline(ione), sta1_fline(ione,ione),   &
     &    ierr_MPI)
!
      if(my_rank .eq. 0) then
        do ip = 1, nprocs
          ist = istack_ele_line_gl(ip-1) + 1
          ied = istack_ele_line_gl(ip)
          do inum = ist, ied
            iedge_line_gl(1:2,inum) = iedge_line_gl(1:2,inum)           &
     &                             + istack_nod_line_gl(ip-1)
          end do
        end do
      end if
!
      end subroutine collect_fline_connection
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_position
!
      integer(kind = kint) :: ip, num, ist, nneib_recv
!
!
      nneib_recv = 0
      num = 3*nnod_line_l
      call MPI_Isend(xx_line_l(1,1), num, CALYPSO_REAL, izero,          &
     &    0, CALYPSO_COMM, req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist = istack_nod_line_gl(ip-1) + 1
          num = 3*nnod_line_gl(ip)
          call MPI_Irecv(xx_line_gl(1,ist), num, CALYPSO_REAL,          &
     &        (ip-1), 0, CALYPSO_COMM, req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, req2_fline, sta2_fline, ierr_MPI)
      call MPI_WAITALL(ione, req1_fline(ione), sta1_fline(ione,ione),   &
     &    ierr_MPI)
!
      end subroutine collect_fline_position
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_color
!
      integer(kind = kint) :: ip, num, ist, nneib_recv
!
!
      nneib_recv = 0
      num = nnod_line_l
      call MPI_Isend(col_line_l(1), num, CALYPSO_REAL, izero,           &
     &    0, CALYPSO_COMM, req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist = istack_nod_line_gl(ip-1) + 1
          num = nnod_line_gl(ip)
          call MPI_Irecv(col_line_gl(ist), num, CALYPSO_REAL,           &
     &        (ip-1), 0, CALYPSO_COMM, req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, req2_fline, sta2_fline, ierr_MPI)
      call MPI_WAITALL(ione, req1_fline(ione), sta1_fline(ione,ione),   &
     &    ierr_MPI)
!
      end subroutine collect_fline_color
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_data
