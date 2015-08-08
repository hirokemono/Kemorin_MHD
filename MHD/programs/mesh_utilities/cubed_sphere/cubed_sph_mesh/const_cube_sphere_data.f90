!
!      module const_cube_sphere_data
!
!      Written by Kemorin on Apr., 2006
!
!      subroutine construct_sphere_mesh
!      subroutine construct_coarse_mesh
!
      module const_cube_sphere_data
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine construct_sphere_mesh
!
      use m_geometry_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
!
      use cal_shell_position
      use set_center_rect_cube_quad
      use set_center_cube_node
      use cubed_sph_center_connect
      use radial_stack_4_cubed_sph
      use write_cubed_sph_mesh_head
      use write_cubed_sph_grp_data
      use modify_colat_cube_surf
!
      integer(kind = kint) :: inod_start, iele_start, id_flag_quad
!
!
      call set_mesh_file_names
!
      write(*,*) 'output mesh information'
      call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,     &
     &          nnod_cb_sph, nele_cb_sph, num_t_linear)
!
      if (iflag_quad .gt. 0) then
        write(*,*) 'quad mesh'
        call set_quad_mesh_file_names
        call write_header_4_mesh(id_q_mesh, id_q_connect, id_q_group,   &
     &      numnod_20, numele_20, num_t_quad)
      end if
!
!  construct center cube
!
       inod_start = 0
       id_flag_quad = iflag_quad * id_q_mesh
!
       call set_center_cube(inod_start, id_l_mesh, id_flag_quad,        &
     &     num_hemi, num_hemi, x_node, x_node)
!
       if ( inod_start .ne. numnod_cube ) then
         write (*,*) 'number of node of center is wrong'
         stop
       end if
!
!  set positions of the shell
!
       call allocate_cubed_sph_posi_tmp
       call allocate_wall_latitude_ratio(num_hemi)
       call adjust_to_shell(inod_start, id_l_mesh, id_flag_quad,        &
     &     num_hemi, num_hemi)
!
       call project_to_sphere(inod_start, id_l_mesh, id_flag_quad,      &
     &     num_hemi, num_hemi)
!
       if ( inod_start .ne. nnod_cb_sph ) then
        write (*,*) 'number of node in the shell is wrong'
        stop
       end if
!
       close(id_l_mesh)
!
!  construct center cube
!
       if (iflag_quad .gt. 0) then
!
         call set_center_cube_quad(inod_start, id_q_mesh)
         write(*,*) 'set_center_cube_quad end', nnod_cb_sph, inod_start
         if ( inod_start .ne. (nnod_cb_sph+numedge_cube) ) then
           write (*,*) 'number of quadrature node in center is wrong',  &
     &             inod_start, (nnod_cb_sph+numedge_cube)
           stop
         end if
!
!  construct shell
!
         write(*,*) 'set nodes around center cube'
         call adjust_to_shell_quad(inod_start, id_q_mesh,               &
     &       num_hemi, num_hemi)
!
         write(*,*) 'set nodes in the sphere shell',                    &
     &              inod_start, numnod_20
         call projection_quad(inod_start, id_q_mesh,                    &
     &       num_hemi, num_hemi)
         if ( inod_start .ne. numnod_20 ) then
           write (*,*) 'number of quadrature node in shell is wrong',   &
     &                 inod_start, numnod_20
           stop
         end if
!
         close(id_q_mesh)
       end if
       call deallocate_wall_latitude_ratio
       call deallocate_cubed_sph_posi_tmp
!
!  set quadrature information
!   construct element connectivity
!
       iele_start = 0
       id_flag_quad = iflag_quad * id_q_connect
!
      write(*,*) 'set connectivity for center cube'
      call set_center_connect_quad(iele_start, id_l_connect,            &
     &    id_flag_quad, nnod_cb_sph, num_hemi, num_hemi)
       if ( iele_start .ne. numele_cube ) then
        write (*,*) 'number of quadrature element of center is wrong'
        stop
       end if
!
      write(*,*) 'set connectivity in the sphere shell'
      call radial_stack_quad(iele_start, id_l_connect, id_flag_quad)
       if ( iele_start .ne. nele_cb_sph ) then
        write (*,*) 'number of quadrature element in shell is wrong'
        stop
       end if
!
      close(id_l_connect)
      if (iflag_quad .gt. 0) close(id_q_connect)
!
!  output group data
!
      if (iflag_quad .gt. 0) then
        call output_group_data_quad
        close(id_q_group)
      end if
!
      call output_group_data
      close(id_l_group)
!
      write(*,*) 'finish!!'
!
      end subroutine construct_sphere_mesh
!
!   --------------------------------------------------------------------
!
      subroutine construct_coarse_mesh
!
      use m_constants
      use m_geometry_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
!
      use count_coarse_parameters
      use cal_shell_coarse_position
      use set_merged_ele_cubed_sph
      use set_center_cube_node
      use cubed_sph_center_connect
      use radial_coarse_stack_c_sph
      use write_cubed_sph_mesh_head
      use write_cubed_sph_grp_data
      use modify_colat_cube_surf
!
      integer(kind = kint) :: iele_start, inod_end, inum, ic, iele_s2
!
!
       iele_start = nele_cb_sph
       inum = iele_start
!
       call allocate_coarse_cube_sph_posi
       call allocate_wall_latitude_ratio(num_hemi)
       do ic = 1, max_coarse_level
!
         call cal_coarse_cube_params(ic)
!
         call set_coarse_mesh_names(ic)
!
         call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,  &
     &       numnod_coarse, numele_coarse, num_t_linear)
!
         call write_header_4_transfer(ic, id_transfer)
!
!     construct node
!
         call set_coarse_center_cube(inod_end, id_l_mesh, id_transfer,  &
     &       nskip_s, nl_s, num_hemi, num_hemi, x_node, x_node)
!
         call adjust_to_coarse_shell(ic, id_l_mesh, id_transfer,        &
     &       num_hemi, num_hemi)
         call projection_coarse(ic, id_l_mesh, id_transfer,             &
     &       num_hemi, num_hemi)
!
         close(id_l_mesh)
!
!    construct connectivity
!
         iele_s2 = 0
!
         write(*,*) 'set_center_connect_quad', ic
         call set_center_connect_quad(iele_s2, id_l_connect, izero,     &
     &       numnod_coarse, n_hemi_c, n_hemi_c)
!
         write(*,*) 'radial_coarse_stack'
         call radial_coarse_stack(ic, id_l_connect)
!
         write(*,*) 'count_merged_cube', ic
         call count_merged_cube(ic, id_transfer)
!
         write(*,*) 'set_merged_cube_data'
         call set_merged_cube_data(ic, id_transfer)
         write(*,*) 'set_merge_4_shell'
         call set_merge_4_shell(ic, id_transfer)
         write(*,*) 'output_domain_4_merge'
         call output_domain_4_merge(ic, id_transfer)
!
         inum = iele_start
!
!      construct groups
!
         write(*,*) 'output_coarse_group_data'
         call output_coarse_group_data(ic)
!
       end do
       call deallocate_wall_latitude_ratio
       call deallocate_coarse_cube_sph_posi
!
      end subroutine construct_coarse_mesh
!
!   --------------------------------------------------------------------
!
      end module const_cube_sphere_data
