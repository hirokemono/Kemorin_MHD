!const_peri_cube_data.f90
!      module const_peri_cube_data
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine construct_rect_peri_mesh                             &
!!     &         (csph_mesh, csph_grp, csph_p, c_sphere)
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_group), intent(in) :: csph_grp
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!      subroutine const_coarse_rect_tri_peri                           &
!!     &         (csph_mesh, csph_grp, csph_p, course_p, c_sphere)
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_group), intent(in) :: csph_grp
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module const_peri_cube_data
!
      use m_precision
!
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
      use t_cubed_sph_grp_param
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine construct_rect_peri_mesh                               &
     &         (csph_mesh, csph_grp, csph_p, c_sphere)
!
      use m_geometry_constants
      use t_numref_cubed_sph
!
      use peri_cube_shell_position
      use set_center_rect_cube_quad
      use set_center_cube_node
      use cubed_sph_center_connect
      use write_cubed_sph_mesh_head
      use write_cubed_sph_grp_data
      use modify_colat_cube_surf
!
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_group), intent(in) :: csph_grp
      type(numref_cubed_sph), intent(in) :: csph_p
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
      if (csph_p%iflag_quad .gt. 0) then
        write(*,*) 'set_quad_mesh_file_names'
        call set_quad_mesh_file_names
        call write_header_4_mesh(id_q_mesh, id_q_connect, id_q_group,   &
     &      csph_mesh%numnod_20, csph_mesh%numele_20, num_t_quad)
      end if
!
!  construct center cube
!
       inod_start = 0
       id_flag_quad = csph_p%iflag_quad * id_q_mesh
!
        write(*,*) 'set_center_cube'
       call set_center_cube(inod_start, id_l_mesh, id_flag_quad,        &
     &     csph_p%num_hemi, csph_p%ncube_vertical,                      &
     &     csph_p%x_node, csph_p%v_node)
!
       if(inod_start .ne. c_sphere%numnod_cube) then
        write (*,*) 'number of node of center is wrong',                &
     &              inod_start, c_sphere%numnod_cube
        stop
       end if
!
!  set positions of the shell
!
        write(*,*) 'allocate_prei_cube_surf_tmp'
       call allocate_prei_cube_surf_tmp(c_sphere)
        write(*,*) 'allocate_wall_latitude_ratio'
       call allocate_wall_latitude_ratio(csph_p%ncube_vertical)
        write(*,*) 'cover_peri_cube'
       call cover_peri_cube                                             &
     &    (id_l_mesh, id_flag_quad, c_sphere, inod_start)
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
       if (csph_p%iflag_quad .gt. 0) then
         write(*,*) 'set_center_rect_quad',                             &
     &             csph_mesh%nnod_cb_sph, inod_start
         call set_center_rect_quad                                      &
     &     (id_q_mesh, csph_p%num_hemi, csph_p%ncube_vertical,          &
     &      csph_p%x_node, csph_p%x_edge, csph_p%v_node, csph_p%v_edge, &
     &      inod_start, c_sphere)
!
         num = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube
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
       id_flag_quad = csph_p%iflag_quad * id_q_connect
!
      write(*,*) 'set connectivity for center cube'
      call set_center_connect_quad(iele_start, id_l_connect,            &
     &    id_flag_quad, csph_mesh%nnod_cb_sph,                          &
     &    csph_p%num_hemi, csph_p%ncube_vertical)
      if(iele_start .ne. c_sphere%numele_cube) then
        write (*,*) 'number of quadrature element of center is wrong'
        stop
      end if
!
      close(id_l_connect)
      if (csph_p%iflag_quad .gt. 0) close(id_q_connect)
!
!  output group data
!
      if (csph_p%iflag_quad .gt. 0) then
        call output_group_data_quad                                     &
     &     (csph_p%num_hemi, c_sphere, csph_mesh, csph_grp)
        close(id_q_group)
      end if
!
      call output_group_data(csph_p%num_hemi, c_sphere, csph_grp)
      close(id_l_group)
!
      write(*,*) 'finish!!'
!
      end subroutine construct_rect_peri_mesh
!
!   --------------------------------------------------------------------
!
      subroutine const_coarse_rect_tri_peri                             &
     &         (csph_mesh, csph_grp, csph_p, course_p, c_sphere)
!
      use m_constants
      use m_geometry_constants
      use t_numref_cubed_sph
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
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_group), intent(in) :: csph_grp
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(inout) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele_start, inod_end, inum, ic
      integer(kind = kint) :: iele_s2
!
!
       write(*,*) 'max_coarse_level', course_p%max_coarse_level
       iele_start = csph_mesh%nele_cb_sph
       inum = iele_start
!
       call allocate_coarse_cube_surf_tmp                               &
     &    (course_p%max_coarse_level, c_sphere)
       call allocate_wall_latitude_ratio(csph_p%ncube_vertical)
       do ic = 1, course_p%max_coarse_level
         call cal_coarse_rect_params                                    &
     &      (ic, c_sphere, csph_mesh, csph_p, course_p)
!
         call set_coarse_mesh_names(ic)
!
         call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,  &
     &       course_p%numnod_coarse, course_p%numele_coarse,            &
     &       num_t_linear)
!
         call write_header_4_transfer(ic, id_transfer, course_p)
!
!     construct node
!
         call set_coarse_center_cube(inod_end, id_l_mesh, id_transfer,  &
     &       course_p%nskip_s, course_p%nl_s, csph_p%num_hemi,          &
     &       csph_p%ncube_vertical, csph_p%x_node, csph_p%v_node)
!
         call cover_coarse_cube                                         &
     &      (ic, id_l_mesh, id_transfer, course_p, c_sphere)
!
         close(id_l_mesh)
!
!    construct connectivity
!
         iele_s2 = 0
!
         write(*,*) 'set_center_connect_quad', ic
         call set_center_connect_quad(iele_s2, id_l_connect, izero,     &
     &       course_p%numnod_coarse, course_p%n_hemi_c,                 &
     &       course_p%n_vert_c)
!
         write(*,*) 'count_merged_cube'
         call count_merged_cube(id_transfer, course_p)
!
         write(*,*) 'set_merged_cube_data'
         call set_merged_cube_data                                      &
     &      (id_transfer, csph_p, course_p, c_sphere)
         write(*,*) 'set_merge_4_shell'
         call set_merge_4_shell(ic, id_transfer, course_p, c_sphere)
         write(*,*) 'output_domain_4_merge'
         call output_domain_4_merge(id_transfer, course_p)
!
         inum = iele_start
!
!      construct groups
!
         write(*,*) 'output_coarse_group_data'
         call output_coarse_group_data(course_p, csph_grp)
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
