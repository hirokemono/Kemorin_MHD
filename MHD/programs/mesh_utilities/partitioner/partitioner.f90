!
      program patitioner
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_comm_table
      use t_surface_data
      use t_edge_data
      use t_near_mesh_id_4_node
      use t_mesh_data_4_merge
      use t_jacobians
      use t_fem_gauss_int_coefs
!
      use m_control_data_4_part
      use m_ctl_param_partitioner
!
      use m_control_data_4_merge
!      use m_original_ucd_4_merge
!      use m_geometry_data_4_merge
      use m_control_param_merge
      use ucd_IO_select
      use intelligent_partition
!
      use set_merged_geometry
      use set_2nd_geometry_4_serial
      use set_merged_udt_2_IO
!
      use init_partitioner
      use grouping_for_partition
      use generate_local_mesh
      use set_control_data_4_part
!
      use load_mesh_data
      use const_mesh_information
!
      use int_volume_of_single_domain
      use set_surf_grp_vectors
!
      use single_const_kemoview_mesh
!
      implicit none
!
      type(mesh_geometry), save :: org_mesh
      type(mesh_groups), save :: org_group
      type(element_geometry), save :: org_ele_mesh
!
      type(near_mesh), save :: included_ele
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians_T
      type(shape_finctions_at_points), save :: spfs_T
!
      integer(kind = kint), parameter :: my_rank = izero
      integer(kind = kint) :: ierr, iprint, ifield, icomp
      integer(kind = kint) :: icou
      integer(kind = kint) :: num_pe
!
      type(time_data), save :: fem_time_IO
      type(ucd_data), save :: fem_ucd
!
      character(len=kchara), parameter                                  &
      &      :: def_magnetic_field_name = 'magnetic_field'
      type(vector_field), save :: data_field_vec
      type(simulate_particle), pointer :: particles(:)
      !real(kind = kreal), pointer :: time_cost(:)
      type(time_esti), pointer:: time_cost(:)
      real(kind = kreal), pointer :: partition_tbl(:), part_num_node(:)
      !type(dimension_part_tbl) :: part_dim_tbl
      integer(kind = kint) :: num_particle
      integer(kind = kint) :: iflag_part_debug
! initial debug flag
      iflag_part_debug = 0

!
!  read control file
!
      call read_control_data_4_part
      call s_set_control_data_4_part
!
!  read global mesh
!
      call input_mesh(global_mesh_file, my_rank, org_mesh, org_group,   &
     &    org_ele_mesh%surf%nnod_4_surf, org_ele_mesh%edge%nnod_4_edge, &
     &    ierr)
      if(ierr .gt. 0) stop 'Global mesh is wrong!'
!
!      write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, org_mesh, org_group, org_ele_mesh)
!  ========= Read global field data for load balance partition =======
      write(*,*) 'read control_merge'
      call read_control_4_merge
      call set_control_4_merge(num_pe)
      !call set_control_4_newudt(sec_mesh1%num_pe2)

      call sel_read_udt_param(izero, istep_start, original_ucd_param, fem_time_IO, fem_ucd)

      !write(*,*) '-----test-----'
      !write(*,*) 'number of node: ', fem_ucd%nnod
      !write(*,*) 'number of field', fem_ucd%num_field
      !do iprint = 1, fem_ucd%num_field
      !  write(*,*) 'name:', fem_ucd%phys_name(iprint), 'dim ', fem_ucd%num_comp(iprint)
      !  write(*,*) fem_ucd%d_ucd(:, iprint)
      !end do
      !write(*,*) '-----testend-----'

!  ========= Estimate load for each subdomain based on field data ====
!  Load one field data from ucd file
      data_field_vec%nnod = fem_ucd%nnod
      icou = 0
      do ifield = 1, fem_ucd%num_field
        write(*,*) fem_ucd%phys_name(ifield)
        if(cmp_no_case(fem_ucd%phys_name(ifield), def_magnetic_field_name)) then
          !write(*,*) 'is ', def_magnetic_field_name
          data_field_vec%ncomp = fem_ucd%num_comp(ifield)
          !write(*,*) 'num of node', data_field_vec%nnod,'num of comp', data_field_vec%ncomp
          data_field_vec%phys_name = fem_ucd%phys_name(ifield)
          call alloc_vector_field(data_field_vec)
          !write(*,*) 'field:', data_field_vec%phys_name, 'component idx is ', icou + 1
          do icomp = 1, fem_ucd%num_comp(ifield)
            !write(*,*) 'read from idx:', icou+icomp
            data_field_vec%d_ucd(:,icomp) = fem_ucd%d_ucd(:,icomp + icou)
          end do
        end if
        icou = icou + fem_ucd%num_comp(ifield)
      end do

! org_mesh output test
      if(iflag_part_debug .gt. 0) then
        write(*,*) 'mesh data info test'
        write(*,*) 'numnod', org_mesh%node%numnod
        write(*,*) 'numsurf', org_ele_mesh%surf%numsurf
        write(*,*) 'numele', org_mesh%ele%numele
        write(*,*) 'nnod_4_surf',org_ele_mesh%surf%nnod_4_surf
        write(*,*) 'isf_4_ele', shape(org_ele_mesh%surf%isf_4_ele)
        write(*,*) 'iele_4_surf', shape(org_ele_mesh%surf%iele_4_surf)
        write(*,*) 'interior_surf', shape(org_ele_mesh%surf%interior_surf)
      end if
!
!  -------------------------------
!
!      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
!      call pick_surface_group_geometry(org_ele_mesh%surf,               &
!     &   org_group%surf_grp, org_group%tbls_surf_grp,                   &
!     &   org_group%surf_grp_geom)
!
!  -------------------------------
!  ========= compute element volume ===============
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (org_mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_and_single_vol                                &
     &   (org_mesh, org_group, spfs_T, jacobians_T)
write(*,*) org_mesh%ele%volume_ele(:)
!
!  ========= Routines for partitioner ==============
!
!      write(*,*) 'initialize_partitioner'
      call initialize_partitioner(org_mesh, org_group)
!      write(*,*) 'grouping_for_partitioner'
      call grouping_for_partitioner                                     &
     &   (org_mesh%node, org_mesh%ele, org_ele_mesh%edge,               &
     &    org_group%nod_grp, org_group%ele_grp, org_group%tbls_ele_grp)
!  ========= Regrouping after estimate computation load =======
!
! part_plt contain info about num of subdomains
      if(iflag_new_partition .eq. 1) then
        num_particle = num_domain * 100
        allocate(particles(num_particle))
        write(*,*) 'generatie sample particle for estimation'
        call choose_particles_from_eles(org_mesh%ele, data_field_vec, particles, num_particle)
  ! debug out put
        if(iflag_part_debug .gt. 0) then
          do iprint = 1, num_particle
            write(*,*) 'Particles for load estimation:  '
            write(*,*) 'particle: ', iprint
            write(*,*) 'pos: ', particles(iprint)%pos
            write(*,*) 'vec: ', particles(iprint)%vec
            write(*,*) 'element id:', particles(iprint)%ele_id
            write(*,*) 'group id:', particles(iprint)%group_id
          end do
        end if

        allocate(time_cost(num_domain))

        call seed_particles(org_mesh%node%numnod, org_mesh%ele%numele,    &
        &   org_ele_mesh%surf%numsurf, org_ele_mesh%surf%nnod_4_surf,     &
        &   org_ele_mesh%surf%isf_4_ele, org_ele_mesh%surf%ie_surf,       &
        &   org_ele_mesh%surf%iele_4_surf,                                &
        &   org_ele_mesh%surf%interior_surf, org_mesh%node%xx,            &
        &   data_field_vec%d_ucd,                   &
        &   particles, num_particle, time_cost)

  !cal partition table for new partition
        allocate(partition_tbl(num_domain))
        call cal_partition_tbl(time_cost, num_domain, partition_tbl)

        allocate(part_num_node(num_domain))
        part_num_node(:) = partition_tbl(:)*org_mesh%node%numnod

        if(iflag_part_debug .gt. 0) then
          write(*,*) 'time cost', time_cost(1:num_domain)%ave_time
          write(*,*) 'partition tbl', partition_tbl(1:num_domain)
          write(*,*) 'target partition num', part_num_node(:)
        end if

  !      call allocate_dim_part_tbl(part_dim_tbl, ndivide_eb)
  !      call cal_part_dim_tbl(num_domain, ndivide_eb, partition_tbl, part_dim_tbl)

        call regrouping_for_partition                                      &
        &   (org_mesh%node, org_mesh%ele, org_ele_mesh%edge,               &
        &    org_group%nod_grp, org_group%ele_grp,                         &
        &    org_group%tbls_ele_grp, partition_tbl)
      end if
!
!C===
!C-- create subdomain mesh
!      write(*,*) 'PROC_LOCAL_MESH'
      call PROC_LOCAL_MESH                                              &
     &   (org_mesh%node, org_mesh%ele, org_ele_mesh%edge,               &
     &    org_ele_mesh%surf, data_field_vec,                            &
     &    org_group, included_ele)
!C
!C-- Finalize
!      write(*,*) 'dealloc_nod_ele_infos'
      call dealloc_nod_ele_infos(org_mesh, org_group, org_ele_mesh)
!
!  ========= Construct subdomain information for viewer ==============
!
      if(iflag_viewer_output .gt. 0) then
        write(*,*) 'choose_surface_mesh_sgl'
        call choose_surface_mesh_sgl(num_domain, distribute_mesh_file)
      end if
!
      stop ' * Partitioning finished'
!
      end program patitioner
