!
!      module set_sphere_data
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine set_sphere_data_4_linear(itheta, iphi)
!      subroutine set_sphere_data_4_quad(itheta, iphi)
!
      module set_sphere_data
!
      use m_precision
!
      use m_shell_surface
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
      subroutine set_sphere_data_4_linear(itheta, iphi)
!
      use read_sphere_surface
      use coordinate_converter
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: itheta, iphi
!
      integer(kind = kint) :: num1, irest1, itp
!
!
      itp = itheta * iphi
!
      call read_surface_connect_linear
!
      call position_2_sph(num_CMB, xx_cmb, rtp_cmb(1,1), rtp_cmb(1,2),  &
     &    rtp_cmb(1,3), ar_cmb, s_cmb, as_cmb)
!
        write(*,*) 'num_CMB nnod_CMB', num_CMB, nnod_CMB
        write(*,*) 'num_cube', num_cube
        write(*,*) 'num_layer', num_layer
!
!  conut number of surface nodes for each subdomain
!
        call cal_divide_and_rest(num1, irest1, num_CMB, itp)
        call set_number_of_segments(itp, num1, irest1, numcmb_local)
!
        call set_center_cube_nodes
!
        call set_surface_layer_node_ID
!
      end subroutine set_sphere_data_4_linear
!
!   --------------------------------------------------------------------
!
      subroutine set_sphere_data_4_quad(itheta, iphi)
!
      use read_sphere_surface
      use coordinate_converter
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: itheta, iphi
!
      integer(kind = kint) :: num1, irest1, itp
!
!
      itp = itheta * iphi
!
      call read_surface_connect_quad
!
      call position_2_sph(num_CMB, xx_cmb, rtp_cmb(1,1), rtp_cmb(1,2),  &
     &    rtp_cmb(1,3), ar_cmb, s_cmb, as_cmb)
!
        write(*,*) 'num_CMB nnod_CMB', num_CMB, nnod_CMB
        write(*,*) 'num_cube', num_cube
        write(*,*) 'num_layer', num_layer
!
!  conut number of surface nodes for each subdomain
!
        call cal_divide_and_rest(num1, irest1, num_CMB, itp)
        call set_number_of_segments(itp, num1, irest1, numcmb_local)
!
        call set_center_cube_nodes
        call set_center_cube_edge
!
        call set_surface_layer_node_ID
        call set_surface_layer_edge_ID
!
      end subroutine set_sphere_data_4_quad
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_center_cube_nodes
!
      integer(kind = kint) :: inod
!
      do inod = 1, nnod_cube
        inod_free(inod) = inod
      end do
!
      end subroutine set_center_cube_nodes
!
!  ---------------------------------------------------------------------
!
      subroutine set_center_cube_edge
!
      integer(kind = kint) :: inod, i, j
!
      do inod = 1, nedge_cube
        i = inod + nnod_cube
        j = inod + nnod_cube + nnod_CMB*num_layer
        inod_free(i) = j
      end do
!
      end subroutine set_center_cube_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surface_layer_node_ID
!
      integer(kind = kint) :: is, inod, k, kk
!
      do k = 1, num_layer
        do is = 1, nnod_CMB
!
          inod = nnod_cube + (num_layer-k)*nnod_CMB + is
          kk = istack_sph(is) + 1
          istack_sph(is) = kk
          item_sph(kk,is) = inod
!
        end do
      end do
!
      end subroutine set_surface_layer_node_ID
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_layer_edge_ID
!
      integer(kind = kint) :: is, iss, inod, k, kk
!
      do k = 1, num_layer-1
        do is = 1, num_CMB
!
          inod = nnod_cube + nnod_CMB*num_layer + nedge_cube            &
     &          + nedge_CMB + (num_layer-k-1)*num_CMB + is
          kk = istack20_sph(is) + 1
          istack20_sph(is) = kk
          item20_sph(kk,is) = inod
!
        end do
      end do
!
      do is = 1, nedge_CMB
          inod = nnod_cube + nnod_CMB*num_layer + nedge_cube + is
          iss = is+nnod_CMB
          kk = istack20_sph(iss) + 1
          istack20_sph(iss) = kk
          item20_sph(kk,iss) = inod
      end do
!
      end subroutine set_surface_layer_edge_ID
!
!  ---------------------------------------------------------------------
!
      end module set_sphere_data
