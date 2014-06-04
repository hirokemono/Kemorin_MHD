!
!      module input_MG_data
!
!        programmed by H. Matsui on Dec., 2008
!
!      subroutine input_MG_mesh
!      subroutine input_MG_itp_tables
!
      module input_MG_data
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
      use m_constants
      use m_type_AMG_data
      use m_type_AMG_mesh
!
      implicit none
!
      private ::  alloc_zero_mesh_data, sync_group_name_4_empty
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine input_MG_mesh
!
      use m_control_parameter
      use m_ctl_parameter_Multigrid
      use m_read_mesh_data
      use mesh_IO_select
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use set_mesh_types
!
      integer(kind = kint) :: i_level
!
      do i_level = 1, num_MG_level
        iflag_mesh_file_fmt = ifmt_MG_mesh_file(i_level)
        if(my_rank .lt. MG_vector(i_level)%nprocs ) then
!
          mesh_file_head = MG_mesh_file_head(i_level)
          call sel_read_mesh(my_rank)
          call set_mesh_data_types( MG_mesh(i_level) )
!
          if (iflag_MG_elem_file(i_level) .gt. 0) then
            mesh_ele_file_head = MG_elem_file_head(i_level)
            call sel_input_element_comm_table(my_rank)
            call set_ele_comm_tbl_type_data( MG_ele_mesh(i_level) )
          end if
!
          if (iflag_MG_surf_file(i_level) .gt. 0) then
            mesh_surf_file_head = MG_surf_file_head(i_level)
            call sel_input_surface_connect(my_rank)
            call set_surf_connect_type_data(MG_surf_mesh(i_level),      &
     &          MG_mesh(i_level)%mesh )
          else
            call set_nnod_surf_by_eletype(MG_surf_mesh(i_level),        &
     &          MG_mesh(i_level)%mesh )
          end if
!
          if (iflag_MG_edge_file(i_level) .gt. 0) then
            mesh_edge_file_head = MG_edge_file_head(i_level)
            call sel_input_edge_connect(my_rank)
            call set_edge_connect_type_data(MG_edge_mesh(i_level),      &
     &          MG_surf_mesh(i_level), MG_mesh(i_level)%mesh )
          else
            call set_nnod_edge_by_eletype(MG_edge_mesh(i_level),        &
     &          MG_mesh(i_level)%mesh )
          end if
!
        else
          call alloc_zero_mesh_data( MG_mesh(i_level),                  &
     &        MG_surf_mesh(i_level), MG_edge_mesh(i_level))
        end if
!
        call sync_group_name_4_empty(MG_vector(i_level)%nprocs,         &
     &      MG_mesh(i_level)%group%nod_grp,                             &
     &      MG_mesh(i_level)%group%ele_grp,                             &
     &      MG_mesh(i_level)%group%surf_grp)
      end do
!
      end subroutine input_MG_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine input_MG_itp_tables
!
      use itp_table_IO_select_4_zlib
      use copy_interpolate_type_IO
!
      integer(kind = kint) :: i_level, ierr
!
!
      do i_level = 1, num_MG_level
        if(i_level.eq.1 .or. my_rank.lt.MG_vector(i_level-1)%nprocs)    &
     &      then
          table_file_header = MG_f2c_tbl_head(i_level)
          ifmt_itp_table_file = ifmt_MG_table_file(i_level)
          write(*,*) 'MG_f2c_tbl_head format', ifmt_itp_table_file
          call sel_read_interpolate_table(my_rank, ierr)
          call copy_interpolate_types_from_IO(my_rank,                  &
     &        MG_itp(i_level)%f2c )
        else
          call alloc_zero_itp_tables(np_smp, MG_itp(i_level)%f2c)
        end if
!
      end do
!
!
!
      do i_level = 1, num_MG_level
        if(i_level.eq.1 .or. my_rank.lt.MG_vector(i_level-1)%nprocs)    &
     &      then
          table_file_header = MG_c2f_tbl_head(i_level)
          ifmt_itp_table_file = ifmt_MG_table_file(i_level)
          write(*,*) 'MG_c2f_tbl_head format', ifmt_itp_table_file
          call sel_read_interpolate_table(my_rank, ierr)
          call copy_interpolate_types_from_IO(my_rank,                  &
     &        MG_itp(i_level)%c2f )
        else
          call alloc_zero_itp_tables(np_smp, MG_itp(i_level)%c2f)
        end if
!
      end do
!
!
!
      if (iflag_MG_commute_by_ele .gt. 0) then
        do i_level = 1, num_MG_level
          if(i_level.eq.1 .or. my_rank.lt.MG_vector(i_level-1)%nprocs)  &
     &      then
            table_file_header = MG_f2c_eletbl_head(i_level)
            call sel_read_interpolate_table(my_rank, ierr)
!
            call copy_interpolate_types_from_IO(my_rank,                &
     &           MG_c2f_ele_tbl(i_level) )
          else
            call alloc_zero_itp_tables(np_smp, MG_c2f_ele_tbl(i_level))
          end if
!
        end do
      end if
!
      end subroutine input_MG_itp_tables
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_zero_mesh_data(mesh_info, surf_mesh, edge_mesh)
!
      use t_mesh_data
      use set_mesh_types
!
      type(mesh_data), intent(inout) ::        mesh_info
      type(surface_geometry), intent(inout) :: surf_mesh
      type(edge_geometry), intent(inout) ::    edge_mesh
!
!
      mesh_info%mesh%nod_comm%num_neib =    izero
      mesh_info%mesh%nod_comm%ntot_import = izero
      mesh_info%mesh%nod_comm%ntot_export = izero
      call allocate_type_comm_tbl_num(mesh_info%mesh%nod_comm)
      call allocate_type_comm_tbl_item(mesh_info%mesh%nod_comm)
!
      mesh_info%mesh%node%numnod =        izero
      mesh_info%mesh%node%internal_node = izero
      call allocate_node_geometry_type(mesh_info%mesh%node)
!
      mesh_info%mesh%ele%numele = izero
      mesh_info%mesh%ele%first_ele_type = izero
      call allocate_ele_connect_type(mesh_info%mesh%ele)
!
      call set_nnod_surf_by_eletype(surf_mesh, mesh_info%mesh )
      call set_nnod_edge_by_eletype(edge_mesh, mesh_info%mesh )
!
      end subroutine alloc_zero_mesh_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sync_group_name_4_empty(np_mpi,                        &
     &          nod_grp, ele_grp, sf_grp)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: np_mpi
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: sf_grp
!
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: nlen_comm, ntot_grp, num_grp_g(3)
      character(len=kchara), allocatable :: grp_name_g(:)
!
!
      if(my_rank .eq. 0) then
        num_grp_g(1) = nod_grp%num_grp
        num_grp_g(2) = ele_grp%num_grp
        num_grp_g(3) = sf_grp%num_grp
      end if
!
      call MPI_BCAST(num_grp_g, ithree, CALYPSO_INTEGER, 0,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ge. np_mpi) then
        nod_grp%num_grp = num_grp_g(1)
        ele_grp%num_grp = num_grp_g(2)
        sf_grp%num_grp =  num_grp_g(3)
!
        call allocate_grp_type_num(nod_grp)
        call allocate_grp_type_num(ele_grp)
        call allocate_sf_grp_type_num(sf_grp)
      end if
!
      write(*,*) 'num_grp_g', my_rank, num_grp_g
!
      ntot_grp =  num_grp_g(1) + num_grp_g(2) + num_grp_g(3)
      allocate( grp_name_g(ntot_grp) )
!
      if(my_rank .eq. 0) then
        ied = nod_grp%num_grp
        grp_name_g(1:ied) = nod_grp%grp_name(1:ied)
        ist = nod_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp
        grp_name_g(ist:ied) = ele_grp%grp_name(1:ele_grp%num_grp)
        ist = nod_grp%num_grp + ele_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp + sf_grp%num_grp
        grp_name_g(ist:ied) = sf_grp%grp_name(1:sf_grp%num_grp)
      end if
!
      nlen_comm = kchara*ntot_grp
      call MPI_BCAST(grp_name_g, nlen_comm, CALYPSO_CHARACTER, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ge. np_mpi) then
        ied = nod_grp%num_grp
        nod_grp%grp_name(1:ied) =             grp_name_g(1:ied)
        ist = nod_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp
        ele_grp%grp_name(1:ele_grp%num_grp) = grp_name_g(ist:ied)
        ist = nod_grp%num_grp + ele_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp + sf_grp%num_grp
        sf_grp%grp_name(1:sf_grp%num_grp) =   grp_name_g(ist:ied)
!
        call allocate_grp_type_item(nod_grp)
        call allocate_grp_type_item(ele_grp)
        call allocate_sf_grp_type_item(sf_grp)
      end if
!
      deallocate(grp_name_g)
!
      end subroutine sync_group_name_4_empty
!
!  ---------------------------------------------------------------------
!
      end module input_MG_data
