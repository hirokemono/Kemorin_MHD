!
!      module set_sphere_data
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine set_sphere_data_4_linear                             &
!!     &         (sphere_file_name, itheta, iphi, sphere_4_part)
!!      subroutine set_sphere_data_4_quad                               &
!!     &         (sphere_file_name, itheta, iphi, sphere_4_part)
!!        type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      module set_sphere_data
!
      use m_precision
!
      use t_shell_surface_4_part
!
      implicit none
!
!      private :: set_sphere_data_4_linear, set_sphere_data_4_quad
      private :: set_surface_layer_node_ID, set_surface_layer_edge_ID
      private :: set_center_cube_nodes, set_center_cube_edge
!
!  ---------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_sphere_data_4_linear                               &
     &         (sphere_file_name, itheta, iphi, sphere_4_part)
!
      use coordinate_converter
      use cal_minmax_and_stacks
!
      character(len = kchara), intent(in) :: sphere_file_name
      integer(kind = kint), intent(in) :: itheta, iphi
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: num1, irest1, itp
!
!
      itp = itheta * iphi
!
      call read_surface_connect_linear(sphere_file_name, sphere_4_part)
!
      call position_2_sph                                               &
     &   (sphere_4_part%num_CMB,  sphere_4_part%xx_cmb,                 &
     &    sphere_4_part%rtp_cmb(1,1), sphere_4_part%rtp_cmb(1,2),       &
     &    sphere_4_part%rtp_cmb(1,3), sphere_4_part%ar_cmb,             &
     &    sphere_4_part%s_cmb, sphere_4_part%as_cmb)
!
        write(*,*) 'num_CMB nnod_CMB', sphere_4_part%num_CMB,           &
     &                                sphere_4_part%nnod_CMB
        write(*,*) 'num_cube', sphere_4_part%num_cube
        write(*,*) 'num_layer', sphere_4_part%num_layer
!
!  conut number of surface nodes for each subdomain
!
        call cal_divide_and_rest                                        &
     &     (num1, irest1, sphere_4_part%num_CMB, itp)
        call set_number_of_segments                                     &
     &     (itp, num1, irest1, sphere_4_part%numcmb_local)
!
        call set_center_cube_nodes(sphere_4_part)
!
        call set_surface_layer_node_ID(sphere_4_part)
!
      end subroutine set_sphere_data_4_linear
!
!   --------------------------------------------------------------------
!
      subroutine set_sphere_data_4_quad                                 &
     &         (sphere_file_name, itheta, iphi, sphere_4_part)
!
      use coordinate_converter
      use cal_minmax_and_stacks
!
      character(len = kchara), intent(in) :: sphere_file_name
      integer(kind = kint), intent(in) :: itheta, iphi
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: num1, irest1, itp
!
!
      itp = itheta * iphi
!
      call read_surface_connect_quad(sphere_file_name, sphere_4_part)
!
      call position_2_sph                                               &
     &   (sphere_4_part%num_CMB,  sphere_4_part%xx_cmb,                 &
     &    sphere_4_part%rtp_cmb(1,1), sphere_4_part%rtp_cmb(1,2),       &
     &    sphere_4_part%rtp_cmb(1,3), sphere_4_part%ar_cmb,             &
     &    sphere_4_part%s_cmb, sphere_4_part%as_cmb)
!
        write(*,*) 'num_CMB nnod_CMB', sphere_4_part%num_CMB,           &
     &                                 sphere_4_part%nnod_CMB
        write(*,*) 'num_cube', sphere_4_part%num_cube
        write(*,*) 'num_layer', sphere_4_part%num_layer
!
!  conut number of surface nodes for each subdomain
!
        call cal_divide_and_rest                                        &
     &     (num1, irest1, sphere_4_part%num_CMB, itp)
        call set_number_of_segments                                     &
     &     (itp, num1, irest1, sphere_4_part%numcmb_local)
!
        call set_center_cube_nodes(sphere_4_part)
        call set_center_cube_edge(sphere_4_part)
!
        call set_surface_layer_node_ID(sphere_4_part)
        call set_surface_layer_edge_ID(sphere_4_part)
!
      end subroutine set_sphere_data_4_quad
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_center_cube_nodes(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: inod
!
      do inod = 1, sphere_4_part%nnod_cube
        sphere_4_part%inod_free(inod) = inod
      end do
!
      end subroutine set_center_cube_nodes
!
!  ---------------------------------------------------------------------
!
      subroutine set_center_cube_edge(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: inod, i, j
!
      do inod = 1, sphere_4_part%nedge_cube
        i = inod + sphere_4_part%nnod_cube
        j = inod + sphere_4_part%nnod_cube                              &
     &     + sphere_4_part%nnod_CMB * sphere_4_part%num_layer
        sphere_4_part%inod_free(i) = j
      end do
!
      end subroutine set_center_cube_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surface_layer_node_ID(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: is, inod, k, kk
!
      do k = 1, sphere_4_part%num_layer
        do is = 1, sphere_4_part%nnod_CMB
!
          inod = sphere_4_part%nnod_cube                                &
     &          + (sphere_4_part%num_layer - k)*sphere_4_part%nnod_CMB  &
     &          + is
          kk = sphere_4_part%istack_sph(is) + 1
          sphere_4_part%istack_sph(is) = kk
          sphere_4_part%item_sph(kk,is) = inod
!
        end do
      end do
!
      end subroutine set_surface_layer_node_ID
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_layer_edge_ID(sphere_4_part)
!
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
!
      integer(kind = kint) :: is, iss, inod, k, kk
!
      do k = 1, sphere_4_part%num_layer-1
        do is = 1, sphere_4_part%num_CMB
!
          inod = sphere_4_part%nnod_cube                                &
     &          + sphere_4_part%nnod_CMB * sphere_4_part%num_layer      &
     &          + sphere_4_part%nedge_cube + sphere_4_part%nedge_CMB    &
     &          + (sphere_4_part%num_layer-k-1) * sphere_4_part%num_CMB &
     &          + is
          kk = sphere_4_part%istack20_sph(is) + 1
          sphere_4_part%istack20_sph(is) = kk
          sphere_4_part%item20_sph(kk,is) = inod
!
        end do
      end do
!
      do is = 1, sphere_4_part%nedge_CMB
          inod = sphere_4_part%nnod_cube                               &
     &          + sphere_4_part%nnod_CMB*sphere_4_part%num_layer       &
     &          + sphere_4_part%nedge_cube + is
          iss = is+sphere_4_part%nnod_CMB
          kk = sphere_4_part%istack20_sph(iss) + 1
          sphere_4_part%istack20_sph(iss) = kk
          sphere_4_part%item20_sph(kk,iss) = inod
      end do
!
      end subroutine set_surface_layer_edge_ID
!
!  ---------------------------------------------------------------------
!
      end module set_sphere_data
