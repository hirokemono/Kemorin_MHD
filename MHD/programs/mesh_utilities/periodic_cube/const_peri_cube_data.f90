!const_peri_cube_data.f90
!      module const_peri_cube_data
!
!      Written by Kemorin on Apr., 2006
!
!      subroutine construct_rect_sphere_mesh
!      subroutine const_coarse_rect_tri_peri
!
      module const_peri_cube_data
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
      subroutine construct_rect_sphere_mesh
!
      use m_geometry_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
!
      use peri_cube_shell_position
      use set_center_rect_cube_quad
      use set_center_cube_node
      use cubed_sph_center_connect
      use write_cubed_sph_mesh_head
      use write_cubed_sph_grp_data
      use modify_colat_cube_surf
!
      integer(kind = kint) :: inod_start, iele_start, id_flag_quad
      integer(kind = kint) :: num
!
!
      call set_linear_mesh_file_names
!
      write(*,*) 'write_header_4_mesh'
      call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,     &
     &   nnod_cb_sph, nele_cb_sph, num_t_linear)
!
      if (iflag_quad .gt. 0) then
        write(*,*) 'set_quad_mesh_file_names'
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
        write(*,*) 'set_center_cube'
       call set_center_cube(inod_start, id_l_mesh, id_flag_quad,        &
     &     num_hemi, ncube_vertical, x_node, v_node)
!
       if(inod_start .ne. c_sphere1%numnod_cube) then
        write (*,*) 'number of node of center is wrong',                &
     &              inod_start, c_sphere1%numnod_cube
        stop
       end if
!
!  set positions of the shell
!
        write(*,*) 'allocate_prei_cube_surf_tmp'
       call allocate_prei_cube_surf_tmp
        write(*,*) 'allocate_wall_latitude_ratio'
       call allocate_wall_latitude_ratio(ncube_vertical)
        write(*,*) 'cover_peri_cube'
       call cover_peri_cube(inod_start, id_l_mesh, id_flag_quad)
!
        write(*,*) 'inod_start', inod_start, nnod_cb_sph
       if ( inod_start .ne. nnod_cb_sph ) then
        write (*,*) 'number of node in the shell is wrong'
        stop
       end if
!
        write(*,*) 'id_l_mesh close'
       close(id_l_mesh)
!
!  construct center cube
!
       if (iflag_quad .gt. 0) then
         write(*,*) 'set_center_rect_quad', nnod_cb_sph, inod_start
         call set_center_rect_quad(inod_start, id_q_mesh)
         write(*,*) 'set_center_rect_quad end', nnod_cb_sph, inod_start
         num = nnod_cb_sph + c_sphere1%numedge_cube
         if(inod_start .ne. num) then
           write (*,*) 'number of quadrature node in center is wrong',  &
     &             inod_start, num
           stop
         end if
!
         close(id_q_mesh)
!
       end if
       call deallocate_wall_latitude_ratio
       call deallocate_prei_cube_surf_tmp
!
!  set quadrature information
!   construct element connectivity
!
       iele_start = 0
       id_flag_quad = iflag_quad * id_q_connect
!
      write(*,*) 'set connectivity for center cube'
      call set_center_connect_quad(iele_start, id_l_connect,            &
     &    id_flag_quad, nnod_cb_sph, num_hemi, ncube_vertical)
      if(iele_start .ne. c_sphere1%numele_cube) then
        write (*,*) 'number of quadrature element of center is wrong'
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
      end subroutine construct_rect_sphere_mesh
!
!   --------------------------------------------------------------------
!
      subroutine const_coarse_rect_tri_peri
!
      use m_constants
      use m_geometry_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
!
      use count_coarse_parameters
      use peri_cube_shell_position
      use set_merged_ele_cubed_sph
      use set_center_cube_node
      use cubed_sph_center_connect
      use write_cubed_sph_mesh_head
      use write_cubed_sph_grp_data
      use modify_colat_cube_surf
!
      integer(kind = kint) :: iele_start, inod_end, inum, ic
      integer(kind = kint) :: iele_s2
!
!
       write(*,*) 'max_coarse_level', max_coarse_level
       iele_start = nele_cb_sph
       inum = iele_start
!
       call allocate_coarse_cube_surf_tmp
       call allocate_wall_latitude_ratio(ncube_vertical)
       do ic = 1, max_coarse_level
         call cal_coarse_rect_params(ic)
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
     &       nskip_s, nl_s, num_hemi, ncube_vertical, x_node, v_node)
!
         call cover_coarse_cube(ic, id_l_mesh, id_transfer)
!
         close(id_l_mesh)
!
!    construct connectivity
!
         iele_s2 = 0
!
         write(*,*) 'set_center_connect_quad', ic
         call set_center_connect_quad(iele_s2, id_l_connect, izero,     &
     &       numnod_coarse, n_hemi_c, n_vert_c)
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
       call deallocate_prei_cube_surf_tmp
!
      end subroutine const_coarse_rect_tri_peri
!
!   --------------------------------------------------------------------
!
      end module const_peri_cube_data
