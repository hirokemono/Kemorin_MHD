!collect_fline_data.f90
!
!      module collect_fline_data
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_collect_fline_data(istep_fline, i_fln, fln_prm,    &
!!     &          fline_prm, fline_lc, fline_gl)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(fieldline_paramters), intent(in) :: fline_prm
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!
      module collect_fline_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use t_global_fieldline
      use t_local_fline
!
      implicit  none
!
      private :: collect_number_of_fline, collect_fline_connection
      private :: collect_fline_position, collect_fline_color
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_collect_fline_data(istep_fline, i_fln, fln_prm,      &
     &          fline_prm, fline_lc, fline_gl)
!
      use t_control_params_4_fline
      use m_field_file_format
      use set_ucd_file_names
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep_fline, i_fln
      type(fieldline_paramter), intent(in) :: fln_prm
      type(fieldline_paramters), intent(in) :: fline_prm
      type(local_fieldline), intent(in) :: fline_lc
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      character(len=kchara) :: ftmp_1, file_name
!
!
      fline_gl%color_name_gl = fline_prm%name_color_output(i_fln)
      call collect_number_of_fline(fline_lc, fline_gl)
!
      if(fline_gl%ntot_nod_line_gl                                      &
     &        .gt. fline_gl%ntot_nod_line_gl_buf) then
        call raise_global_fline_data(fline_gl)
      end if
!
      if(fline_gl%ntot_ele_line_gl                                      &
     &       .gt. fline_gl%ntot_ele_line_gl_buf) then
        call raise_global_fline_connect(fline_gl)
      end if
!
      call collect_fline_connection(fline_lc, fline_gl)
      call collect_fline_position(fline_lc, fline_gl)
      call collect_fline_color(fline_lc, fline_gl)
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'output format ', fline_prm%id_fline_file_type(i_fln)
        if(fline_prm%id_fline_file_type(i_fln) .eq. iflag_ucd) then
          call set_single_ucd_file_name(fln_prm%fline_prefix,           &
     &        fline_prm%id_fline_file_type(i_fln),                      &
     &        istep_fline, file_name)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_ucd(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
        else if(fline_prm%id_fline_file_type(i_fln) .eq. iflag_vtk)     &
     &        then
          call set_single_ucd_file_name(fln_prm%fline_prefix,           &
     &        fline_prm%id_fline_file_type(i_fln),                      &
     &        istep_fline, file_name)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_vtk(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
        else
          call add_int_suffix                                           &
     &       (istep_fline, fln_prm%fline_prefix, ftmp_1)
          call add_dx_extension(ftmp_1, file_name)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_dx(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
        end if
      end if
!
      end subroutine s_collect_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine collect_number_of_fline(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip
!
!
      call MPI_AllGather(fline_lc%nnod_line_l, ione, CALYPSO_INTEGER,   &
     &    fline_gl%nnod_line_gl(1), ione, CALYPSO_INTEGER,              &
     &    CALYPSO_COMM, ierr_MPI)
!
      call MPI_AllGather(fline_lc%nele_line_l, ione, CALYPSO_INTEGER,   &
     &    fline_gl%nele_line_gl(1), ione, CALYPSO_INTEGER,              &
     &    CALYPSO_COMM, ierr_MPI)
!
      fline_gl%istack_nod_line_gl(0) = 0
      fline_gl%istack_ele_line_gl(0) = 0
      do ip = 1, nprocs
        fline_gl%istack_nod_line_gl(ip)                                 &
     &                = fline_gl%istack_nod_line_gl(ip-1)               &
     &                 + fline_gl%nnod_line_gl(ip)
        fline_gl%istack_ele_line_gl(ip)                                 &
     &                = fline_gl%istack_ele_line_gl(ip-1)               &
     &                 + fline_gl%nele_line_gl(ip)
      end do
      fline_gl%ntot_nod_line_gl = fline_gl%istack_nod_line_gl(nprocs)
      fline_gl%ntot_ele_line_gl = fline_gl%istack_ele_line_gl(nprocs)
!
      end subroutine collect_number_of_fline
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_connection(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip, num, ist, ied, inum, nneib_recv
!
!
      nneib_recv = 0
      num = 2 * fline_lc%nele_line_l
      call MPI_Isend(fline_lc%iedge_line_l(1,1), num, CALYPSO_INTEGER,  &
     &   izero, 0, CALYPSO_COMM, fline_gl%req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist = fline_gl%istack_ele_line_gl(ip-1) + 1
          num = 2 * fline_gl%nele_line_gl(ip)
          call MPI_Irecv (fline_gl%iedge_line_gl(1,ist), num,           &
     &        CALYPSO_INTEGER, (ip-1), 0, CALYPSO_COMM,                 &
     &        fline_gl%req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, fline_gl%req2_fline,                 &
     &    fline_gl%sta2_fline, ierr_MPI)
      call MPI_WAITALL(ione, fline_gl%req1_fline(ione),                 &
     &    fline_gl%sta1_fline(ione,ione), ierr_MPI)
!
      if(my_rank .eq. 0) then
        do ip = 1, nprocs
          ist = fline_gl%istack_ele_line_gl(ip-1) + 1
          ied = fline_gl%istack_ele_line_gl(ip)
          do inum = ist, ied
            fline_gl%iedge_line_gl(1:2,inum)                            &
     &                     = fline_gl%iedge_line_gl(1:2,inum)           &
     &                      + fline_gl%istack_nod_line_gl(ip-1)
          end do
        end do
      end if
!
      end subroutine collect_fline_connection
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_position(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip, num, ist, nneib_recv
!
!
      nneib_recv = 0
      num = 3 * fline_lc%nnod_line_l
      call MPI_Isend(fline_lc%xx_line_l(1,1), num, CALYPSO_REAL, izero, &
     &    0, CALYPSO_COMM, fline_gl%req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist = fline_gl%istack_nod_line_gl(ip-1) + 1
          num = 3 * fline_gl%nnod_line_gl(ip)
          call MPI_Irecv                                                &
     &       (fline_gl%xx_line_gl(1,ist), num, CALYPSO_REAL, (ip-1),    &
     &        0, CALYPSO_COMM, fline_gl%req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, fline_gl%req2_fline,                 &
     &    fline_gl%sta2_fline, ierr_MPI)
      call MPI_WAITALL(ione, fline_gl%req1_fline(ione),                 &
     &    fline_gl%sta1_fline(ione,ione), ierr_MPI)
!
      end subroutine collect_fline_position
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_color(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip, num, ist, nneib_recv
!
!
      nneib_recv = 0
      num = fline_lc%nnod_line_l
      call MPI_Isend(fline_lc%col_line_l(1), num, CALYPSO_REAL, izero,  &
     &    0, CALYPSO_COMM, fline_gl%req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist = fline_gl%istack_nod_line_gl(ip-1) + 1
          num = fline_gl%nnod_line_gl(ip)
          call MPI_Irecv                                                &
     &       (fline_gl%col_line_gl(ist), num, CALYPSO_REAL, (ip-1),     &
     &        0, CALYPSO_COMM, fline_gl%req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, fline_gl%req2_fline,                 &
     &    fline_gl%sta2_fline, ierr_MPI)
      call MPI_WAITALL(ione, fline_gl%req1_fline(ione),                 &
     &    fline_gl%sta1_fline(ione,ione), ierr_MPI)
!
      end subroutine collect_fline_color
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_data
