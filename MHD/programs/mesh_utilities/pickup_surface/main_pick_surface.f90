!
!     program  pick_surface_para
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      program   pick_surface_para
!
      use m_precision
      use m_constants
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      use t_file_IO_parameter
      use para_const_kemoview_mesh
      use find_mesh_file_format
      use getarg_kemo
!
      implicit    none
!
      character(len=kchara) :: file_head
      character(len=kchara) :: cmd
      integer(kind = kint) :: icount
!
      type(field_IO_params), save ::  pick_mesh_file
      type(parallel_make_vierwer_mesh), save :: par_view1
!
!
      call calypso_MPI_init
!
      if(my_rank .eq. izero) then
        icount = command_argument_count()
        write(*,*) 'command_argument_count', icount
        call get_command(cmd)
        write (*,*) 'Whole command: ', cmd
        call get_command_argument(0, cmd)
        write (*,*) 'Command: ', trim(cmd)
        call get_command_argument(1, cmd)
        write (*,*) 'Option: ', trim(cmd)
        call get_environment_variable("OMP_NUM_THREADS", cmd)
        read(cmd,*) icount
        write (*,*) 'OMP_NUM_THREADS: ', icount

        icount = iargc_kemo()
        if(icount .eq. 0) then
          write(*,*) ' Please input header of mesh  !!'
          read (*,*) file_head
        else
          call getarg_k(1, file_head)
          write(*,*) 'file prefix from command line: ', trim(file_head)
        end if
        pick_mesh_file%file_prefix = file_head
!
        if(iflag_debug .eq. 0) write(*,*) 'find_merged_mesh_format'
        call find_merged_mesh_format(pick_mesh_file)
      end if
      call calypso_mpi_barrier
      call calypso_mpi_bcast_character                                  &
     &   (pick_mesh_file%file_prefix, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(pick_mesh_file%iflag_format, 0)
!
      call pickup_surface_mesh(pick_mesh_file, par_view1)
!
      call calypso_MPI_finalize
      stop ' //// program normally finished //// '
!
      end program pick_surface_para 
