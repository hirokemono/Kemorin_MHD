!const_rect_sphere_data.f90
!      module const_rect_sphere_data
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine construct_rect_sphere_mesh(csph_mesh, c_sphere)
!!      subroutine construct_coarse_rect_mesh(csph_mesh, c_sphere)
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module const_rect_sphere_data
!
      use m_precision
!
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine construct_rect_sphere_mesh(csph_mesh, c_sphere)
!
      use m_geometry_constants
      use m_numref_cubed_sph
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
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: inod_start, iele_start, id_flag_quad
      integer(kind = kint) :: num
!
!
      call set_linear_mesh_file_names
!
      write(*,*) 'write_header_4_mesh'
      call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,     &
     &    csph_mesh%nnod_cb_sph, csph_mesh%nele_cb_sph, num_t_linear)
!
      if (iflag_quad .gt. 0) then
        write(*,*) 'set_quad_mesh_file_names'
        call set_quad_mesh_file_names
        call write_header_4_mesh(id_q_mesh, id_q_connect, id_q_group,   &
     &      csph_mesh%numnod_20, csph_mesh%numele_20, num_t_quad)
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
       if(inod_start .ne. c_sphere%numnod_cube) then
        write (*,*) 'number of node of center is wrong',                &
     &              inod_start, c_sphere%numnod_cube
        stop
       end if
!
!  set positions of the shell
!
        write(*,*) 'allocate_cubed_sph_posi_tmp'
       call allocate_cubed_sph_posi_tmp(c_sphere)
        write(*,*) 'allocate_wall_latitude_ratio'
       call allocate_wall_latitude_ratio(ncube_vertical)
        write(*,*) 'adjust_to_shell'
       call adjust_to_shell(id_l_mesh, id_flag_quad,                    &
     &     num_hemi, ncube_vertical, inod_start, c_sphere)
!
        write(*,*) 'projection'
       call project_to_sphere(id_l_mesh, id_flag_quad,                  &
     &      num_hemi, ncube_vertical, inod_start, c_sphere)
        write(*,*) 'projection end'
!
        write(*,*) 'inod_start', inod_start, csph_mesh%nnod_cb_sph
       if(inod_start .ne. csph_mesh%nnod_cb_sph) then
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
         write(*,*) 'set_center_rect_quad',                             &
     &             csph_mesh%nnod_cb_sph, inod_start
         call set_center_rect_quad(id_q_mesh, inod_start, c_sphere)
!
         num = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube
         if(inod_start .ne. num) then
           write (*,*) 'number of quadrature node in center is wrong',  &
     &             inod_start, num
           stop
         end if
!
!  construct shell
!
         write(*,*) 'set nodes around center cube'
         call adjust_to_shell_quad(id_q_mesh,                           &
     &      num_hemi, ncube_vertical, inod_start, c_sphere)
!
         write(*,*) 'set nodes in the sphere shell',                    &
     &              inod_start, csph_mesh%numnod_20
         call projection_quad(id_q_mesh,                                &
     &      num_hemi, ncube_vertical, inod_start, c_sphere)
         if ( inod_start .ne. csph_mesh%numnod_20 ) then
           write (*,*) 'number of quadrature node in shell is wrong',   &
     &                 inod_start, csph_mesh%numnod_20
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
     &    id_flag_quad, csph_mesh%nnod_cb_sph, num_hemi, ncube_vertical)
      if(iele_start .ne. c_sphere%numele_cube) then
        write (*,*) 'number of quadrature element of center is wrong'
        stop
      end if
!
      write(*,*) 'set connectivity in the sphere shell'
      call radial_stack_quad                                            &
     &   (id_l_connect, id_flag_quad, c_sphere, csph_mesh, iele_start)
      if(iele_start .ne. csph_mesh%nele_cb_sph) then
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
        call output_group_data_quad(c_sphere, csph_mesh)
        close(id_q_group)
      end if
!
      call output_group_data(c_sphere)
      close(id_l_group)
!
      write(*,*) 'finish!!'
!
      end subroutine construct_rect_sphere_mesh
!
!   --------------------------------------------------------------------
!
      subroutine construct_coarse_rect_mesh(csph_mesh, c_sphere)
!
      use m_constants
      use m_geometry_constants
      use m_numref_cubed_sph
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
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele_start, inod_end, inum, ic, iele_s2
!
!
       write(*,*) 'max_coarse_level', max_coarse_level
       iele_start = csph_mesh%nele_cb_sph
       inum = iele_start
!
       call allocate_coarse_cube_sph_posi(c_sphere)
       call allocate_wall_latitude_ratio(ncube_vertical)
       do ic = 1, max_coarse_level
         call cal_coarse_rect_params(ic, c_sphere, csph_mesh)
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
         call adjust_to_coarse_shell(ic, id_l_mesh, id_transfer,        &
     &       num_hemi, ncube_vertical, c_sphere)
         call projection_coarse(ic, id_l_mesh, id_transfer,             &
     &       num_hemi, ncube_vertical, c_sphere)
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
         write(*,*) 'radial_coarse_stack'
         call radial_coarse_stack(id_l_connect, ic, c_sphere)
!
         write(*,*) 'count_merged_cube'
         call count_merged_cube(id_transfer)
!
         write(*,*) 'set_merged_cube_data'
         call set_merged_cube_data(id_transfer, c_sphere)
         write(*,*) 'set_merge_4_shell'
         call set_merge_4_shell(ic, id_transfer, c_sphere)
         write(*,*) 'output_domain_4_merge'
         call output_domain_4_merge(id_transfer)
!
         inum = iele_start
!
!      construct groups
!
         write(*,*) 'output_coarse_group_data'
         call output_coarse_group_data
!
       end do
       call deallocate_wall_latitude_ratio
       call deallocate_coarse_cube_sph_posi
!
      end subroutine construct_coarse_rect_mesh
!
!   --------------------------------------------------------------------
!
      end module const_rect_sphere_data
