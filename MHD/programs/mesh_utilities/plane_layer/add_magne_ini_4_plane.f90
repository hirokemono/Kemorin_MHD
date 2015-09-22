!
      program add_magne_ini_4_plane
!
!***********************************************************
!
!     program for initial value generation for plane layer
!
!***********************************************************
!
!***********************************************************
!
!     required files
!
!        mesh/in.#:          mesh data
!        mesh/domainsize.dat: size information
!        original/rst.#.#:   original restart data
!        original/adams.#.#: original restart data for Adams-Bashforth
!        restart/rst.0.#:    initial data
!        restart/adams.0.#:  initial data for Adams-Bashforth (zero)
!
!***********************************************************
!
      use m_precision
!
      use m_constants
      use m_phys_labels
      use m_geometry_data
      use m_size_4_plane
      use m_cube_position
      use m_setting_4_ini
      use m_geometry_data_4_merge
      use m_read_mesh_data
      use m_comm_data_IO
      use m_ctl_data_4_cub_kemo
      use t_field_data_IO
      use set_ctl_data_plane_mesh
      use set_parallel_file_name
      use mesh_IO_select
      use set_node_data_4_IO
      use field_IO_select
      use set_field_to_restart

      implicit none
!
      character(len=kchara), parameter                                  &
     &      :: org_rst_f_header = 'restart/rst'
      character(len=kchara), parameter                                  &
     &      :: new_rst_file_header = 'rst_new/rst'
!
      integer(kind=kint) :: i, istep, ip, id_rank, np, inod
      integer(kind=kint) :: jst
      type(field_IO) :: plane_fst_IO
!
!
      pi = four*atan(one)
      write(*,*) 'pi', pi
!
!     read outline of mesh
!
      call read_control_data_plane_mesh
      call s_set_ctl_data_plane_mesh
!
       num_pe = ndx * ndy * ndz
!
!
!
      write(*,*) 'input step number of original data'
      read(*,*) istep
!
!     open original data
!
      ip = 1
      id_rank = 0
!
      call set_field_file_fmt_prefix                                    &
     &   (izero, org_rst_f_header, plane_fst_IO)
      call sel_read_alloc_FEM_fld_head                                  &
     &   (num_pe, izero, istep, plane_fst_IO)
!
      num_rst_org = plane_fst_IO%num_field_IO
!
      call add_initial_num_comp_mhd
!
      merged_fld%num_phys = num_rst_new
      call alloc_phys_name_type(merged_fld)
!
      ntot_rst_org =  plane_fst_IO%ntot_comp_IO
      merged_fld%phys_name(1:num_rst_org)                               &
     &             =    plane_fst_IO%fld_name(1:num_rst_org)
      merged_fld%num_component(1:num_rst_org)                           &
     &             = plane_fst_IO%num_comp_IO(1:num_rst_org)
      merged_fld%istack_component(0:num_rst_org)                        &
     &             = plane_fst_IO%istack_comp_IO(0:num_rst_org)
!
      call add_initial_comp_mhd
      merged_fld%ntot_phys =  ntot_rst_org
!
!    construct new data
!
      merged%node%numnod = node1%numnod
!      
      call alloc_merged_field_stack(nprocs, plane_fst_IO)
      plane_fst_IO%istack_numnod_IO(0) = 0
      do ip = 1, num_pe
        plane_fst_IO%istack_numnod_IO(ip)                               &
     &      = plane_fst_IO%istack_numnod_IO(ip-1) + merged%node%numnod
      end do
!
      do ip = 1, num_pe
        id_rank = ip-1
!
!    read mesh file
!
        iflag_mesh_file_fmt = izero
        mesh_file_head = 'mesh/in'
        call sel_read_geometry_size(id_rank)
        call copy_node_geometry_from_IO(node1)
!
        call deallocate_neib_domain_IO
!
!     allocate added restart data
!
        merged_fld%num_phys =  num_rst_new
        merged_fld%ntot_phys = ntot_rst_new
        call alloc_phys_data_type(merged%node%numnod, merged_fld)
!
!     read original restart data
!
        plane_fst_IO%nnod_IO = merged%node%numnod
!
        plane_fst_IO%num_field_IO =  num_rst_org
        plane_fst_IO%ntot_comp_IO = ntot_rst_org
!
        call set_field_file_fmt_prefix                                  &
     &     (izero, org_rst_f_header, plane_fst_IO)
        call sel_read_step_FEM_field_file                               &
     &     (num_pe, id_rank, istep, plane_fst_IO)
!
        do np = 1, ntot_rst_org
          merged_fld%d_fld(1:merged%node%numnod,np)                     &
     &           = plane_fst_IO%d_IO(1:merged%node%numnod,np)
        end do
!
        call dealloc_phys_name_IO(plane_fst_IO)
        call dealloc_phys_data_IO(plane_fst_IO)
!
!     construct added data
!
        do np = num_rst_org+1, num_rst_new
          jst = merged_fld%istack_component(i-1)
          if (merged_fld%phys_name(np) .eq. fhd_vecp) then
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+1)                              &
     &                               = 0.01d0*sin( pi*node1%xx(inod,3)  &
     &                             / (zmax-zmin))
            end do
          else if (merged_fld%phys_name(np) .eq. fhd_magne) then
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+2) = (0.01d0*pi/two)            &
     &                       * cos( pi*node1%xx(inod,3) / (zmax-zmin))
            end do
          end if
        end do
!
!     write data
!
        plane_fst_IO%num_field_IO =  num_rst_new
        plane_fst_IO%ntot_comp_IO = ntot_rst_new
        call alloc_phys_name_IO(plane_fst_IO)
        call alloc_phys_data_IO(plane_fst_IO)
!
        call simple_copy_fld_name_to_rst(merged_fld, plane_fst_IO)
        call simple_copy_fld_data_to_rst                                &
     &     (merged%node, merged_fld, plane_fst_IO)
!
        plane_fst_IO%file_prefix = new_rst_file_header
        call sel_write_step_FEM_field_file                              &
     &     (num_pe, id_rank, izero, plane_fst_IO)
!
        call dealloc_phys_name_IO(plane_fst_IO)
        call dealloc_phys_data_IO(plane_fst_IO)
!
        call dealloc_phys_data_type(merged_fld)
        call deallocate_node_geometry_base(node1)
      end do
!
      stop
!
      end program add_magne_ini_4_plane

