!
      program  cubmesh311
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
!                         control ver 1.1  on jul. 1999 (ver 3.2)
!                         control ver 2.1  on aug. 1999 (ver 3.3)
! ----------------------------------------------------------------------
!  * input  files
!      standard input   :  nx_all, ny_all, nz_all,  ndx, ndy, ndz
!          nodal count and subdomain division for x,y,z-direction line
!
! ----------------------------------------------------------------------
!  * output files
!      'filter_node.#   : data file for filtering with respect to node
!      'filter_element.#   : data file for filtering with respect to node
! ......................................................................
!
!      'in.#    '       :  mesh data for each pe (nnn; pe number)
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
      use m_size_4_plane
      use m_size_of_cube
      use m_cube_position
      use m_comm_data_cube_kemo
      use m_grp_data_cub_kemo
      use m_cube_files_data
      use m_local_node_id_cube
      use t_neib_range_cube
      use m_offset_size_cube
      use m_neib_nod_cube
      use m_filtering_nod_4_cubmesh
      use m_filtering_ele_4_cubmesh
!
      use m_filter_data_4_plane
      use m_ctl_data_4_cub_kemo
!
      use set_vertical_position_cube
      use set_neib_pe_cube
      use set_cube_node
      use set_cube_ele_connect
      use set_import_cube
      use set_export_cube
      use read_z_filter_info
      use write_nod_grp_cube
      use write_ele_grp_cube
      use write_surf_grp_cube
      use set_plane_geometries
      use set_ctl_data_plane_mesh
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
      integer(kind=kint), parameter  ::   elm_type = 331
! ----------------------------------------------------------------------
!  * variables

      type(neib_range_cube), save :: nb_rng1
      type(gradient_model_data_type), save :: FEM_elen_c
!
      integer(kind=kint)  ::  ipe    , jpe    , kpe    , pe_id
      integer :: id_rank

      character(len= 8 )   ::  date
      character(len=10 )   ::  time
!
!
! ----------------------------------------------------------------------
!
! ***** set time
!
      call  date_and_time ( date, time )
!
! ***** read nodal and subdomain division count
!
!
!     open ( 20,file='inp.dat',form='formatted')
!     read ( 20  , * )   nx_all, ny_all, nz_all,                        &
!    &                   ndx   , ndy,    ndz
!      write(*,*) ' input range of x-direction ( xmin, xmax) '
!      read (  *  , * )   xmin, xmax
!
!      write(*,*) ' input range of y-direction ( ymin, ymax) '
!      read (  *  , * )   ymin, ymax
!
!      write(*,*) ' input range of z-direction ( zmin, zmax) '
!      read (  *  , * )   zmin, zmax
!
      call read_control_data_plane_mesh
      call s_set_ctl_data_plane_mesh
!
      call s_set_plane_geometries(elm_type)
!
! ***** allocate position of node at z-component
!
      call set_position_4_vartical(elm_type)
!
! ***** allocate nodal id table
!
      call allocate_node_informations
!
      call allocate_communication_data(elm_type)
!
      call allocate_cube_ele_group_id
!
!    allocate work array
!
      call allocate_work_4_filter_nod
!
!     set one-dimensional moments
!
!      if (iflag_filter .ge.0) then
         FEM_elen_c%filter_conf%nf_type = iflag_filter
         call allocate_filter_4_plane(FEM_elen_c%filter_conf%nf_type)
         call read_filter_info(FEM_elen_c%filter_conf%nf_type)
!      end if
!
! **********   domain loop for each pe   **********
!
      pe_id = 1

      do kpe=1,ndz
        do jpe=1,ndy
          do ipe=1,ndx
!
! ***** open output file
!
             id_rank = int(pe_id - 1)
             write(penum,'(i4   )')  id_rank
             penum_left = adjustl(penum)
!
             call open_mesh_file(id_rank)
!
! ***** set and write basic local model parameters
!                                       .. pe nod per 1 line

            call set_size_of_domain(elm_type, kpe)
            call set_range_4_neighbour(ipe, jpe, kpe, nb_rng1)
!
!                                       .. set neighbor pe
            neibpetot = 0

!      inside cube
!
            call set_neighboring_pes(nb_rng1, pe_id)
!
!      neiboring information for periodical boundaries
!
            call set_neighboring_pes_peri(nb_rng1, pe_id, ipe, jpe)
!
!
            call sort_neighboring_pes
!
! ..... write 1.parallel information (pe_id start from 0, not 1)
!
            call write_pe_data(pe_id)
!
! ..... write 2.mesh information (nodes and elements in partition)
!
            call set_range_4_nodeloop(kpe, nb_rng1)
            call set_node(nb_rng1, ipe, jpe, kpe)
!
! ..... write 2.2 element (connection)
!
            call set_ele_connect(elm_type, ipe, jpe, kpe)
!
! ..... write 3.import / export information
!
! ***** set and write import nodes
!                                     .... count nodes 
            call set_import_data(nb_rng1, ipe, jpe)
!
! ***** set and write export nodes
!                                     .... count nodes 
            call set_export_data(nb_rng1, ipe, jpe)
!
            call sort_communication_table
!
            call write_communication_data
!
!
! ..... write 4.group information
!
            call write_labels_4_group
!                                       ... node    group
!                                        .. count node group and stack
!
            call count_node_group(elm_type, ipe, jpe, kpe)
            call write_node_group(ipe, jpe, kpe)
!
!    output element group
!
!                                     .. count element group and stack
!
            call count_element_group
!
            call write_cube_ele_group(kpe)
!
!
!    output surface group
!
!                                     .. count element group and stack
            call count_surface_group(kpe)
            call write_surface_group(kpe)
!
!   construct filtering information
!
            if(iflag_filter .gt. 0) then
              call allocate_work_4_filter_ele
!
              write(*,*) 'neighboring_node'
              call neighboring_node(pe_id, nb_rng1, FEM_elen_c)
!
              write(*,*) 'deallocate_work_4_filter_ele'
              call deallocate_work_4_filter_ele
            end if
!                                       ... to next pe 
            write(*,*) 'change domain'
            pe_id = pe_id + 1

            close(l_out)
!
            write(*,*) 'reset_communication_data'
            call reset_communication_data
            call reset_node_info
            call reset_cube_ele_group_id
            call reset_work_4_filter_nod
!
          enddo
        enddo
      enddo
!

      stop ' //// program normally finished //// '
!
      end program  cubmesh311
