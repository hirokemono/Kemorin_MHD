!
!
      program  cubmesh322
! \beginPROGRAM
!    create cubic mesh data (multi-domain type)
!          programmed by k.garatani (rist) on feb. 1998 (ver 1.0)
!                            multi-domain  on mar. 1998 (ver 2.0)
!                         kdofn optionain  on mar. 1998 (ver 2.1)
!                         add control.inp  on apr. 1998 (ver 2.2)
!                         add bc dat leng. on may. 1998 (ver 2.3)
!                         bug fix (real8)  on jul. 1998 (ver 2.4)
!                         geofem format    on mar. 1999 (ver 3.0)
!                         remove step grp  on jul. 1999 (ver 3.1)
!                         control ver 1.1  on jul. 1999 (ver 3.2)z
!                         control ver 2.1  on aug. 1999 (ver 3.3)
!          programmed by H. Matsui  (rist) on Sep. 2001 (ver 4.0)
!                         periodic boundary on Sep. 2001 (ver 4.0)
! ----------------------------------------------------------------------
!  * input  files
!         ctl_cubed_sph
!
! ----------------------------------------------------------------------
!  * output files
!      'control'        :  analysis control data structure_static_linear
! ......................................................................
!
!      'in.nnn'       :  mesh data for each pe (nnn; pe number)
!
!   (GeoFEM mesh data format ver 1.0 on apr.1999)
!      '!' or '!' at the first column means comment line.
!      null line can not omit.
!      name can not include space character
!      index(0) is 0
!
!    !
!    ! 1.parallel information (pe_id start from 0, not 1)
!         (my_pe_id)
!         (total_neighbor_pe_count)
!         ((neighbor_pe_id(i)),i=1,total_neighbor_pe_count)
!    !
!    ! 2.mesh information (nodes and elements in partition)
!    ! 2.1 node
!         (total_node_count)  (total_internal_node_count)
!         do i=1,total_node_count
!           (gloval_node_id) (x-coordinate) (y-co.) (z-co.)
!         enddo
!    ! 2.2 element (connection)
!         (total_element_count)
!         (element_type(i),i=1,total_element_count)
!         do i=1,total_element_count
!           (gloval_element_id) (node-1) (node-2) (node-3) ..
!         enddo
!    !
!    ! 3.import / export information
!    ! 3.1 import
!         (index_import(i),i=1,total_neighbor_pe_count)
!         do i=1,index_import(total_neighbor_pe_count)
!           (local_import_node_id)
!         enddo
!    ! 3.2 export
!         (index_export(i),i=1,total_neighbor_pe_count)
!         do i=1,index_export(total_neighbor_pe_count)
!           (local_export_node_id)
!         enddo
!    !
!    ! 4. group information
!    ! 4.1 node group
!         (group_count)
!         (index(i),i=1,group_count)
!         do i=1,group_count
!           (group_name)
!           ((id(j)),j=1,index(i-1)+1,index(i))
!           enddo
!         enddo
!    ! 4.2 element group
!         (group_count)
!         (index(i),i=1,group_count)
!         do i=1,group_count
!           (group_name)
!           ((id(j)),j=1,index(i-1)+1,index(i))
!           enddo
!         enddo
!    ! 4.3 surface group
!         (group_count)
!         (index(i),i=1,group_count)
!         do i=1,group_count
!           (group_name)
!           (((element_id(j))(surface_id(j))),
!                               j=1,index(i-1)+1,index(i))
!           enddo
!         enddo
!
! ----------------------------------------------------------------------
!
      use m_precision
!
      use t_filter_elength
!
      use m_comm_data_cube_kemo
      use m_grp_data_cub_kemo
      use m_cube_files_data

      use t_size_of_cube
      use t_neib_range_cube
      use t_cube_position
      use t_local_node_id_cube
      use t_control_param_plane_mesh
      use t_filter_work_cubmesh
      use t_filter_data_4_plane

      use m_ctl_data_4_cub_kemo
!
      use set_cube_node_quad
      use set_cube_ele_connect
      use set_import_cube
      use write_nod_grp_cube
      use write_ele_grp_cube
      use write_surf_grp_cube
      use set_plane_geometries
      use neib_nod_cube
      use neib_edge_cube
!
      implicit  none

! ----------------------------------------------------------------------
!  * parameters
! ......................................................................
!  * parameters for variable length
!      kint       : kind parameter for integer variable (byte count)
!      kreal      : kind parameter for real    variable (byte count)
!      integer, parameter   ::  kint = 4 , kreal = 8
! ......................................................................
!  * parameters for default settings
!       elm_type  : element type id. for GeoFEM format
!      integer(kind=kint ), parameter  ::   elm_type = 331
      integer(kind=kint), parameter  ::   elm_type = 332
! ----------------------------------------------------------------------
!  * variables

      type(ctl_param_plane_mesh), save :: cube_p1
      type(size_of_cube), save :: c_size1
      type(size_of_each_cube), save :: c_each1
      type(vertical_position_cube), save :: c_vert1
      type(neib_range_cube), save :: nb_rng1
      type(local_node_id_cube), save :: loc_id1
      type(gradient_model_data_type), save :: FEM_elen_c
      type(filterings_4_cubmesh), save :: c_fils
      type(filter_data_4_plane), save :: cube_fil1

      integer(kind=kint)  ::  ipe    , jpe    , kpe    , pe_id
      integer :: id_rank

!
      character(len= 8 )   ::  date
      character(len=10 )   ::  time

! ----------------------------------------------------------------------
!
! ***** set time
!
      write(*,*) 'date_and_time'
      call  date_and_time ( date, time )
!
! ***** read nodal and subdomain division count
!
      call read_control_data_plane_mesh
      call s_set_ctl_data_plane_mesh(cube_p1, c_size1)
!
      call set_plane_range_w_sleeve(elm_type, c_size1)
      call set_plane_resolution(c_size1)
!
! ***** allocate position of node at z-component
!
      write(*,*) 'set_position_4_vartical'
      call set_position_4_vartical                                      &
     &   (elm_type, cube_p1%iflag_ztype, c_size1, c_vert1)
!
! ***** allocate nodal id table
!
      call alloc_node_informations(c_size1, loc_id1)
      call alloc_edge_informations(loc_id1)
!
      call allocate_communication_data(elm_type, c_size1)
!
      call allocate_cube_ele_group_id(c_size1)
!
!    allocate work array
!
      call alloc_work_4_filter_nod(c_size1, c_fils%c_fil_nod)
!
!      if(cube_p1%iflag_filter .ge.0) then
         FEM_elen_c%filter_conf%nf_type = cube_p1%iflag_filter
         call alloc_filter_4_plane(c_size1%ndepth, c_size1%nz_all,      &
    &        FEM_elen_c%filter_conf%nf_type, cube_fil1)
         call read_z_filter_info(cube_p1%iflag_ztype, c_size1,          &
    &        FEM_elen_c%filter_conf%nf_type, cube_fil1)
!      end if
!
! **********   domain loop for each pe   **********
!
      pe_id = 1

      do kpe = 1, c_size1%ndz
        do jpe = 1, c_size1%ndy
          do ipe = 1, c_size1%ndx
!
! ***** open output file
!
             id_rank = int(pe_id - 1)
             write(penum,'(i4   )') id_rank
             penum_left = adjustl(penum)
!
             call open_mesh_file(id_rank, c_size1)
!
! ***** set and write basic local model parameters
!                                       .. pe nod per 1 line

            write(*,*) 'set_each_cube_resolution', ipe, jpe, kpe
            call set_each_cube_resolution                               &
     &         (elm_type, kpe, c_size1, c_each1)
            call set_range_4_neighbour(ipe, jpe, kpe, c_size1, nb_rng1)

!                                       .. set neighbor pe
            call set_neigbouring_plane                                  &
     &         (c_size1, nb_rng1, pe_id, ipe, jpe)
!
!
            write(*,*) 'sort_neighboring_pes', ipe, jpe, kpe
            call sort_neighboring_pes
!
! ..... write 1.parallel information (pe_id start from 0, not 1)
!
            call write_pe_data(pe_id)
!
! ..... write 2.mesh information (nodes and elements in partition)
!
            call set_range_4_nodeloop(c_size1, kpe, nb_rng1)
            call set_edge_para_4_each_pe(kpe, c_size1%ndz, nb_rng1)

! *****  initialization to construct node information
!
            call init_node_para_4_each_pe                               &
     &         (c_size1, ipe, jpe, kpe, nb_rng1)
            call set_offset_of_domain(c_size1, ipe, jpe, kpe, nb_rng1)
            call set_node_quad(c_size1, c_each1, c_vert1, nb_rng1,      &
     &          ipe, jpe, kpe, loc_id1)
!
! ..... write 2.2 element (connection)
!
            write(*,*) 'set_ele_connect_quad', ipe, jpe, kpe
            call set_ele_connect_quad(c_size1, c_each1, nb_rng1,        &
     &          loc_id1, elm_type, ipe, jpe)

!
! ..... write 3.import / export information
!
! ***** set and write import nodes
!
            write(*,*) 'set_import_data_quad', ipe, jpe, kpe
            call set_import_data_quad                                   &
     &         (c_size1, nb_rng1, loc_id1, ipe, jpe, kpe)
!
            write(*,*) 'set_export_data_quad', ipe, jpe, kpe
            call set_export_data_quad                                   &
     &         (c_size1, nb_rng1, loc_id1, ipe, jpe, kpe)
!
            write(*,*) 'write_org_communication_data', ipe, jpe, kpe
            call write_org_communication_data(pe_id)
!
            write(*,*) 'sort_communication_table', ipe, jpe, kpe
            call sort_communication_table
!
!
            write(*,*) 'write_communication_data', ipe, jpe, kpe
            call write_communication_data
!
!
! ..... write 4.group information
!
            call write_labels_4_group(c_size1)
!
!                                       ... node    group
!
!                                        .. count node group and stack
            call count_node_group                                       &
     &       (c_size1, elm_type, c_each1%nx, c_each1%ny, ipe, jpe, kpe)
            call write_node_group_quad                                  &
     &         (c_size1, loc_id1, c_each1%nx, c_each1%ny, c_each1%nz,   &
     &          ipe, jpe, kpe)
!
!    output element group
!
!                                     .. count element group and stack
            call count_element_group(c_size1, c_each1%elm_fil1_tot)
!
            call write_cube_ele_group                                   &
     &         (c_size1, c_each1%nx, c_each1%ny, c_each1%nz,            &
     &          kpe, nb_rng1%koff)
!

!    output surface group
!
!                                     .. count element group and stack
            call count_surface_group                                    &
     &         (c_size1, c_each1%nx, c_each1%ny, kpe)
            call write_surface_group                                    &
     &         (c_size1%ndz, c_each1%nx, c_each1%ny, c_each1%nz, kpe)
!                                       ... to next pe 
!   construct filtering information
!
            call alloc_work_4_filter_ele                                &
     &         (c_size1, c_each1, c_fils%c_fil_ele)
            call alloc_work_4_filter_edge(c_size1, c_fils%c_fil_edge)
!
            write(*,*) 'filtering information', c_each1%intnodtot
            call neighboring_node                                       &
     &         (pe_id, cube_p1, c_size1, c_each1, nb_rng1, loc_id1,     &
     &          cube_fil1, FEM_elen_c, c_fils%c_fil_nod)
            call neighboring_edge(id_rank, c_size1, c_each1, nb_rng1,   &
     &          loc_id1, c_fils%c_fil_edge)
!
            call dealloc_work_4_filter_nod(c_fils%c_fil_ele)
            call dealloc_work_4_filter_edge(c_fils%c_fil_edge)
!                                       ... to next pe 
            pe_id = pe_id + 1

            close(l_out)
!
            call reset_communication_data
            call reset_node_info(loc_id1)
            call reset_edge_info(loc_id1)
            call reset_cube_ele_group_id
            call reset_work_4_filter_nod(c_fils%c_fil_nod)
!
          enddo
        enddo
      enddo
!
      call dealloc_filter_4_plane(cube_fil1)

      stop ' //// program normally finished //// '
!
      end program  cubmesh322
