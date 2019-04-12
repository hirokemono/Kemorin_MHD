!
!      module const_cube_sphere_data
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine construct_sphere_mesh                                &
!!     &         (rprm_csph, csph_mesh, csph_grp, csph_p, c_sphere)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_group), intent(in) :: csph_grp
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!      subroutine construct_coarse_mesh(rprm_csph, csph_mesh, csph_grp,&
!!     &          csph_p, course_p, c_sphere)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(numref_cubed_sph), intent(ino) :: csph_p
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_group), intent(in) :: csph_grp
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module const_cube_sphere_data
!
      use m_precision
!
      use t_cubed_sph_radius
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
      subroutine construct_sphere_mesh                                  &
     &         (rprm_csph, csph_mesh, csph_grp, csph_p, c_sphere)
!
      use m_geometry_constants
      use t_numref_cubed_sph
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
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_group), intent(in) :: csph_grp
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: inod_start, iele_start, id_flag_quad
      integer(kind = kint) :: num
!
!
      call set_linear_mesh_file_names
!
      write(*,*) 'output mesh information'
      call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,     &
     &    csph_mesh%nnod_cb_sph, csph_mesh%nele_cb_sph, num_t_linear)
!
      if (csph_p%iflag_quad .gt. 0) then
        write(*,*) 'quad mesh'
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
      call set_center_cube(inod_start, id_l_mesh, id_flag_quad,         &
     &    csph_p%num_hemi, csph_p%num_hemi,                             &
     &    csph_p%x_node, csph_p%x_node)
!
      if(inod_start .ne. c_sphere%numnod_cube) then
        write (*,*) 'number of node of center is wrong'
        stop
      end if
!
!  set positions of the shell
!
      call allocate_cubed_sph_posi_tmp(c_sphere)
      call allocate_wall_latitude_ratio(csph_p%num_hemi)
      call adjust_to_shell(id_l_mesh, id_flag_quad,                     &
     &    csph_p%num_hemi, csph_p%num_hemi, rprm_csph,                  &
     &    inod_start, csph_p, c_sphere)
!
      call project_to_sphere(id_l_mesh, id_flag_quad,                   &
     &    csph_p%num_hemi, csph_p%num_hemi, rprm_csph,                  &
     &    inod_start, csph_p, c_sphere)
!
      if(inod_start .ne. csph_mesh%nnod_cb_sph) then
        write (*,*) 'number of node in the shell is wrong'
        stop
      end if
!
      close(id_l_mesh)
!
!  construct center cube
!
      if (csph_p%iflag_quad .gt. 0) then
!
        write(*,*) 'set_center_cube_quad',                              &
     &            csph_mesh%nnod_cb_sph, inod_start
        call set_center_cube_quad(id_q_mesh, csph_p%num_hemi,           &
     &      csph_p%x_node, csph_p%x_edge, inod_start, c_sphere)
        num = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube
        if(inod_start .ne. num) then
          write (*,*) 'number of quadrature node in center is wrong',   &
     &             inod_start, num
          stop
        end if
!
!  construct shell
!
        write(*,*) 'set nodes around center cube'
        call adjust_to_shell_quad(id_q_mesh,                            &
     &      csph_p%num_hemi, csph_p%num_hemi, rprm_csph,                &
     &      inod_start, csph_p, c_sphere)
!
        write(*,*) 'set nodes in the sphere shell',                     &
     &             inod_start, csph_mesh%numnod_20
        call projection_quad(id_q_mesh,                                 &
     &      csph_p%num_hemi, csph_p%num_hemi, rprm_csph,                &
     &      inod_start, csph_p, c_sphere)
        if ( inod_start .ne. csph_mesh%numnod_20 ) then
          write (*,*) 'number of quadrature node in shell is wrong',    &
     &                inod_start, csph_mesh%numnod_20
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
      id_flag_quad = csph_p%iflag_quad * id_q_connect
!
      write(*,*) 'set connectivity for center cube'
      call set_center_connect_quad(iele_start, id_l_connect,            &
     &    id_flag_quad, csph_mesh%nnod_cb_sph, csph_p%num_hemi,         &
     &    csph_p%num_hemi)
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
      end subroutine construct_sphere_mesh
!
!   --------------------------------------------------------------------
!
      subroutine construct_coarse_mesh(rprm_csph, csph_mesh, csph_grp,  &
     &          csph_p, course_p, c_sphere)
!
      use m_constants
      use m_geometry_constants
      use t_numref_cubed_sph
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
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(numref_cubed_sph), intent(in) :: csph_p
      type(cubed_sph_mesh), intent(in) :: csph_mesh
      type(cubed_sph_group), intent(in) :: csph_grp
      type(coarse_cubed_sph), intent(inout) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele_start, inod_end, inum, ic, iele_s2
!
!
      iele_start = csph_mesh%nele_cb_sph
      inum = iele_start
!
      call allocate_coarse_cube_sph_posi                                &
     &   (course_p%max_coarse_level, c_sphere)
      call allocate_wall_latitude_ratio(csph_p%num_hemi)
      do ic = 1, course_p%max_coarse_level
!
        call cal_coarse_cube_params                                     &
     &     (ic, c_sphere, csph_mesh, csph_p, course_p)
!
        call set_coarse_mesh_names(ic)
!
        call write_header_4_mesh(id_l_mesh, id_l_connect, id_l_group,   &
     &      course_p%numnod_coarse, course_p%numele_coarse,             &
     &      num_t_linear)
!
        call write_header_4_transfer(ic, id_transfer, course_p)
!
!     construct node
!
        call set_coarse_center_cube(inod_end, id_l_mesh, id_transfer,   &
     &      course_p%nskip_s, course_p%nl_s, csph_p%num_hemi,           &
     &      csph_p%num_hemi, csph_p%x_node, csph_p%x_node)
!
        call adjust_to_coarse_shell(ic, id_l_mesh, id_transfer,         &
     &      course_p%nnod_cube_c, course_p%nnod_sf_c,                   &
     &      course_p%nnod_cube_fc, course_p%nnod_sf_fc,                 &
     &      course_p%nskip_r, course_p%nskip_fr, csph_p%num_hemi,       &
     &      csph_p%num_hemi, rprm_csph, c_sphere)
        call projection_coarse(ic, id_l_mesh, id_transfer,              &
     &      course_p%nnod_cube_c, course_p%nnod_sf_c,                   &
     &      course_p%nnod_cube_fc, course_p%nnod_sf_fc,                 &
     &      course_p%nskip_r, course_p%nskip_fr, csph_p%num_hemi,       &
     &      csph_p%num_hemi, rprm_csph, c_sphere)
!
        close(id_l_mesh)
!
!    construct connectivity
!
        iele_s2 = 0
!
        write(*,*) 'set_center_connect_quad', ic
        call set_center_connect_quad(iele_s2, id_l_connect, izero,      &
     &      course_p%numnod_coarse, course_p%n_hemi_c,                  &
     &      course_p%n_hemi_c)
!
        write(*,*) 'radial_coarse_stack'
        call radial_coarse_stack(id_l_connect, ic, course_p, c_sphere)
!
        write(*,*) 'count_merged_cube'
        call count_merged_cube(id_transfer, course_p)
!
        write(*,*) 'set_merged_cube_data'
        call set_merged_cube_data                                       &
     &     (id_transfer, csph_p, course_p, c_sphere)
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
      call deallocate_coarse_cube_sph_posi
!
      end subroutine construct_coarse_mesh
!
!   --------------------------------------------------------------------
!
      end module const_cube_sphere_data
