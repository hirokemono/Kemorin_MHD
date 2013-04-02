!
      program generate_initial_4_plane
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
      use m_time_data_IO
      use m_field_data_IO
      use set_ctl_data_plane_mesh
      use set_parallel_file_name
      use mesh_IO_select
      use set_node_geometry_4_IO
      use field_IO_select
!
      implicit none
!
      integer(kind=kint) :: i, ip, my_rank, inod
      integer(kind=kint) :: np, jst, jed
!
!
      pi = four*atan(one)
!
!     read outline of mesh
!
      call read_control_data_plane_mesh
      call s_set_ctl_data_plane_mesh
!
      call set_initial_components
!
      i_time_step_IO = izero
      time_IO =    zero
      delta_t_IO = zero
      num_pe = ndx * ndy * ndz
      merged%node%numnod = numnod
!
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
        call allocate_merged_field_data
!
!   set up of physical values
!
        do np = 1, merged_fld%num_phys
          jst = merged_fld%istack_component(i-1)
          jed = merged_fld%istack_component(i)
!
          if (merged_fld%phys_name(np) .eq. fhd_temp) then
!
            do inod = 1, merged%node%numnod
              if (xx(inod,3).eq.zmin) then
                merged_fld%d_fld(inod,jst+1) = 1.0d0
              else if (xx(inod,3).eq.zmax) then
                merged_fld%d_fld(inod,jst+1) = 0.0d0
              else
                merged_fld%d_fld(inod,jst+1)                            &
     &                    = -(xx(inod,3)-zmax)/(zmax-zmin)              &
     &                     + 0.5*(xx(inod,3)-zmin)/(zmax-zmin)          &
     &                      * sin(2.0d0*pi*xx(inod,1)/(xmax-xmin))      &
     &                      * sin(2.0d0*pi*xx(inod,2)/(xmax-xmin))
              end if
            end do
!
          else if (merged_fld%phys_name(np) .eq. fhd_vecp) then
!
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+1) = 0.01d0*sin(pi*xx(inod,3)   &
     &                            / (zmax-zmin))
            end do
!
          else if (merged_fld%phys_name(np) .eq. fhd_magne) then
!
            do inod = 1, merged%node%numnod
              merged_fld%d_fld(inod,jst+2) = (0.01d0*pi/two)            &
     &                            * cos( pi*xx(inod,3) / (zmax-zmin))
            end do
          end if
        end do
!
!     write data
!
!
        numgrid_phys_IO = merged%node%numnod
!
        num_phys_data_IO = merged_fld%num_phys
        ntot_phys_data_IO = merged_fld%ntot_phys
        call allocate_phys_data_name_IO
        call allocate_phys_data_IO
!
        phys_data_name_IO(1:num_phys_data_IO)                           &
     &             = merged_fld%phys_name(1:num_phys_data_IO)
        num_phys_comp_IO(1:num_phys_data_IO)                            &
     &             = merged_fld%num_component(1:num_phys_data_IO)
        istack_phys_comp_IO(0:num_phys_data_IO)                         &
     &             = merged_fld%istack_component(0:num_phys_data_IO)
        phys_data_IO(1:merged%node%numnod,1:ntot_phys_data_IO)          &
     &    = merged_fld%d_fld(1:merged%node%numnod,1:ntot_phys_data_IO)
!
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
      end program generate_initial_4_plane

