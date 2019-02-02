!construct_circle_data.f90
!      module construct_circle_data
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine construct_circle_mesh(rprm_csph, csph_mesh, c_sphere)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module construct_circle_data
!
      use m_precision
!
      use t_cubed_sph_radius
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
      subroutine construct_circle_mesh(rprm_csph, csph_mesh, c_sphere)
!
      use m_geometry_constants
      use m_numref_cubed_sph
!
      use cal_circle_position
      use set_center_rect_cube_quad
      use set_center_cube_node
      use square_circ_center_connect
      use radial_stack_4_cubed_sph
      use write_cubed_sph_mesh_head
      use write_cubed_sph_grp_data
      use modify_colat_cube_surf
!
      type(cubed_sph_radius), intent(in) :: rprm_csph
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
      call set_center_square(inod_start, id_l_mesh, id_flag_quad,       &
     &    num_hemi, x_node)
!
      if(inod_start .ne. c_sphere%numnod_cube) then
        write (*,*) 'number of node of center is wrong',                &
     &              inod_start, c_sphere%numnod_cube
        stop
      end if
!
!  set positions of the shell
!
      write(*,*) 'allocate_square_circ_posi_tmp'
      call allocate_square_circ_posi_tmp(c_sphere)
      write(*,*) 'adjust_to_circle'
      call adjust_to_circle                                             &
     &   (id_l_mesh, id_flag_quad, rprm_csph, c_sphere, inod_start)
!
      write(*,*) 'projection'
      call projection_2_circle                                          &
     &   (id_l_mesh, id_flag_quad, rprm_csph, c_sphere, inod_start)
      call back_to_square                                               &
     &   (id_l_mesh, id_flag_quad, rprm_csph, c_sphere, inod_start)
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
!  construct center cube for quadrature mesh
!
      if (iflag_quad .gt. 0) then
        write(*,*) 'set_center_square_quad',                            &
     &            csph_mesh%nnod_cb_sph, inod_start
        call set_center_square_quad(id_q_mesh, num_hemi,                &
     &      x_node, x_edge, inod_start, c_sphere)
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
      call adjust_to_circle_quad                                        &
     &   (id_q_mesh, rprm_csph, c_sphere, inod_start)
!
      write(*,*) 'set nodes in the sphere shell',                       &
     &              inod_start, csph_mesh%numnod_20
      call projection_to_circle_quad                                    &
     &   (id_q_mesh, rprm_csph, c_sphere, inod_start)
      if ( inod_start .ne. csph_mesh%numnod_20 ) then
        write (*,*) 'number of quadrature node in shell is wrong',      &
     &                 inod_start, csph_mesh%numnod_20
!       stop
      end if
!
         close(id_q_mesh)
       end if
       call deallocate_square_circ_posi_tmp
!
!  set quadrature information
!   construct element connectivity
!
       iele_start = 0
       id_flag_quad = iflag_quad * id_q_connect
!
      write(*,*) 'set connectivity for center square'
      call square_center_connect_quad(iele_start, id_l_connect,         &
     &    id_flag_quad, csph_mesh%nnod_cb_sph, num_hemi)
      if(iele_start .ne. c_sphere%numele_cube) then
        write (*,*) 'number of quadrature element of center is wrong'
        stop
      end if
!
      write(*,*) 'set connectivity in the sphere shell'
      call radial_stack_surf_q                                          &
     &   (id_l_connect, id_flag_quad, c_sphere, csph_mesh, iele_start)
      if(iele_start .ne. csph_mesh%nele_cb_sph) then
        write (*,*) 'number of quadrature element in shell is wrong'
        stop
      end if
!
      close(id_l_connect)
      if (iflag_quad .gt. 0) close(id_q_connect)
!
      return
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
      end subroutine construct_circle_mesh
!
!   --------------------------------------------------------------------
!
      end module construct_circle_data
