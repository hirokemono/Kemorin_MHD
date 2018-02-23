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
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_file_IO_parameter
      use t_mesh_data_4_merge
      use parallel_const_surface_mesh
      use getarg_kemo
!
      implicit    none
!
      character(len=kchara) :: file_head
      integer(kind = kint) :: icount
!
      type(field_IO_params), save ::  pick_mesh_file
      type(element_data), save :: ele_pick
      type(surface_data), save :: surf_pick
      type(edge_data), save :: edge_pick
      type(merged_mesh), save :: mgd_mesh_ps
!
!
      call calypso_MPI_init
!
      if(my_rank .eq. izero) then
        icount = iargc_kemo()
        if(icount .eq. 0) then
          write(*,*) ' Please input header of mesh  !!'
          read (*,*) file_head
        else
          call getarg_k(1, file_head)
          write(*,*) 'file prefix from command line: ', trim(file_head)
        end if
      end if
      call calypso_mpi_barrier
      call MPI_BCAST(file_head, kchara,                                 &
     &    CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      pick_mesh_file%file_prefix = file_head
      call choose_surface_mesh_para                                     &
     &   (pick_mesh_file, ele_pick, surf_pick, edge_pick, mgd_mesh_ps)
!
      call calypso_MPI_finalize
      stop ' //// program normally finished //// '
!
      end program pick_surface_para 
