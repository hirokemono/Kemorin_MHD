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
      use m_geometry_parameter
      use m_geometry_data
      use m_size_4_plane
      use m_cube_position
      use m_setting_4_ini
      use m_geometry_data_4_merge
      use m_read_mesh_data
      use m_comm_data_IO
      use m_ctl_data_4_cub_kemo
      use m_field_data_IO
      use set_ctl_data_plane_mesh
      use set_parallel_file_name
      use mesh_IO_select
      use set_node_geometry_4_IO
      use field_IO_select

      implicit none
!
      character(len=kchara), parameter                                  &
     &      :: org_rst_f_header = 'restart/rst'
      character(len=kchara), parameter                                  &
     &      :: new_rst_file_header = 'rst_new/rst'
!
      integer(kind=kint) :: i, istep, ip, my_rank, np, inod
      integer(kind=kint) :: jst
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
      my_rank = 0
!
      call set_field_file_fmt_prefix(izero, org_rst_f_header)
      call sel_read_alloc_FEM_fld_head(izero, istep)
!
      num_rst_org = num_phys_data_IO
!
      call add_initial_num_comp_mhd
!
      merged_fld%num_phys = num_rst_new
      call allocate_merged_field_name
!
      ntot_rst_org =  ntot_phys_data_IO
      merged_fld%phys_name(1:num_rst_org)                               &
     &             =    phys_data_name_IO(1:num_rst_org)
      merged_fld%num_component(1:num_rst_org)                           &
     &             = num_phys_comp_IO(1:num_rst_org)
      merged_fld%istack_component(0:num_rst_org)                        &
     &                       = istack_phys_comp_IO(0:num_rst_org)
!
      call add_initial_comp_mhd
      merged_fld%ntot_phys =  ntot_rst_org
!
!    construct new data
!
      merged%node%numnod = numnod
      do ip = 1, num_pe
        my_rank = ip-1
!
!    read mesh file
!
        iflag_mesh_file_fmt = izero
        mesh_file_head = 'mesh/in'
        call sel_read_geometry_size(my_rank)
!
        call copy_node_geometry_from_IO
        call deallocate_neib_domain_IO
!
!     allocate added restart data
!
        merged_fld%num_phys =  num_rst_new
        merged_fld%ntot_phys = ntot_rst_new
        call allocate_merged_field_data
!
!     read original restart data
!
        numgrid_phys_IO =   merged%node%numnod
!
        num_phys_data_IO =  num_rst_org
        ntot_phys_data_IO = ntot_rst_org
!
        call set_field_file_fmt_prefix(izero, org_rst_f_header)
        call sel_read_step_FEM_field_file(my_rank, istep)
!
        do np = 1, ntot_rst_org
          merged_fld%d_fld(1:merged%node%numnod,np)                     &
     &           = phys_data_IO(1:merged%node%numnod,np)
        end do
!
        call deallocate_phys_data_name_IO
        call deallocate_phys_data_IO
!
!     construct added data
!
        do np = num_rst_org+1, num_rst_new
          jst = merged_fld%istack_component(i-1)
          if (merged_fld%phys_name(np) .eq. fhd_vecp) then
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+1) = 0.01d0*sin( pi*xx(inod,3)  &
     &                             / (zmax-zmin))
            end do
          else if (merged_fld%phys_name(np) .eq. fhd_magne) then
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+2) = (0.01d0*pi/two)            &
     &                             * cos( pi*xx(inod,3) / (zmax-zmin))
            end do
          end if
        end do
!
!     write data
!
        num_phys_data_IO =  num_rst_new
        ntot_phys_data_IO = ntot_rst_new
        call allocate_phys_data_name_IO
        call allocate_phys_data_IO
!
        phys_data_name_IO(1:merged_fld%num_phys)                        &
     &             = merged_fld%phys_name(1:merged_fld%num_phys)
        num_phys_comp_IO(1:merged_fld%num_phys)                         &
     &             = merged_fld%num_component(1:merged_fld%num_phys)
        istack_phys_comp_IO(0:merged_fld%num_phys)                      &
     &             = merged_fld%istack_component(0:merged_fld%num_phys)
        phys_data_IO(1:merged%node%numnod,1:merged_fld%ntot_phys)       &
     &   =merged_fld%d_fld(1:merged%node%numnod,1:merged_fld%ntot_phys)
!
        phys_file_head = new_rst_file_header
        call sel_write_step_FEM_field_file(my_rank, izero)
!
        call deallocate_phys_data_name_IO
        call deallocate_phys_data_IO
!
        call deallocate_merged_field_data
        call deallocate_node_geometry
!
      end do
!
      stop
!
      end program add_magne_ini_4_plane

